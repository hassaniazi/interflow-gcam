# Data processing for energy flows from GCAM as sankey/alluvial inputs
#
# Authors: Maggie Liu, Hassan Niazi
# Feb 2026

library("visNetwork")

# 0. Setup ----------------------------------------------------------------
source("./R/0_functions.R")

# User choices
UPDATE_DATA    <- FALSE
PLOTS_ON       <- TRUE
SCENARIO       <- "rcp45cooler_ssp3"
FILTER_YEARS   <- 2025
DISPLAY_LEVEL  <- 2   # int, 1-4 currently; lower value = less detail
COLOR_LEVEL    <- "DETAILED"


# Primary fuels
RENEWABLES     <- c("Nuclear", "Wind", "Solar", "Biomass", "Hydrogen", "Hydropower", "Geothermal")
FOSSIL_FUELS   <- c("Coal", "Oil", "Natural Gas")
FOSSIL_MAPPING <- c("coal", "crude oil", "unconventional oil", "natural gas")

# Secondary fuels
SECONDARY_EN   <- c("Electricity", "Liquid Fuels")
REFINING_TYPE  <- c("Oil Refining", "Biorefining", "Coal to Liquids", "Gas to Liquids")
ELEC_LOADTYPE  <- c("superpeak", "subpeak", "peak", "baseload", "intermediate")
TD_IO          <- 1.06237346267644    # 2021 USA I:O value for L126.IO_R_electd_F_Yh

# End use
DEMAND_SECTORS <- c("Industry", "Buildings", "Transportation")
KEEP_NAMES     <- c("scenario", "region", "year", "source", "target", "value",
                    "level", "source_color", "target_color")

# 1. Load data ------------------------------------------------------------
options(scipen = 999)
DB_NAME        <- 'database_rcp45cooler_ssp3'
DB_PATH        <- 'data'
PROJ           <- "data\\im3scen_energy.dat"
QUERIES        <- 'data\\queries_ewflows.xml'

## . get db ----
if (UPDATE_DATA) {                           # run new queries, or
  conn <- localDBConn(DB_PATH, DB_NAME)
  prj <- addScenario(conn,
                     scenario = NULL,
                     proj = PROJ,
                     clobber = TRUE,
                     queryFile = QUERIES)
} else {                                     # read in from existing .dat
  prj <- loadProject(proj = PROJ)
  listScenarios(prj)
  listQueries(prj)
}

{ ## . get mappings ----
  inputs_map       <- read_csv(paste0(data_dir, "./mappings/inputs_tech_mapping.csv"))
  fossil_prod      <- read_csv(paste0(data_dir, "/GCAMUSA_fossil_resource_production.csv"))
  seds_2024        <- read_csv(paste0(data_dir, "/seds_2024.csv"))
  uranium_2025     <- read_csv(paste0(data_dir, "/qupd_tbl1_uranium_quarterly_25.csv"), skip = 7)
  uranium_purchase <- read_csv(paste0(data_dir, "/umartableS1afigureS1.csv"), skip = 7)
}

##. remake fossil resource mapping, default has this turned off and instead
##  uses the presaved processed file
if (FALSE) {
  # resource production by tech and vintage
  fossil_vint <- read_csv(paste0(data_dir, "ref_res_prod_by_tech_vin.csv"), skip = 1)
  fossil_prod <- fossil_vint %>%
    # "PV_resource", "CSP_resource", "distributed_solar" also included by state
    # in this query. it might be helpful to save them in the future, especially
    # if more renewables added
    filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    select(-c(technology, Units)) %>%
    gather_years() %>%
    group_by(scenario, region, resource, subresource, year) %>%
    summarize(value = sum(value), .groups = "drop")
  write_csv(fossil_prod, paste0(data_dir, "/GCAMUSA_fossil_resource_production.csv"))

  # do the same pre-processing for SEDS
  seds_complete_2024 <- read_csv(paste0(data_dir, "/complete_seds_2024/complete_seds.csv"))
  seds_codes <- read_csv(paste0(data_dir, "/complete_seds_2024/seds_codes.csv"))

  seds_2024 <- seds_complete_2024 %>%
    # HYTCP: Hydroelectricity total net generation
    # ELISP: Net interstate flow of electricity (negative indicates flow out of state)
    # keeping three years to have options in case ratios are off. will default
    # to using 2021 as it aligns with the most current GCAM base year
    filter(MSN %in% c("HYTCP", "ELISP"), Year %in% c(2015, 2021, 2024)) %>%
    select(-Data_Status) %>%      # don't need to keep the data set publish year
    left_join(seds_codes, by = "MSN") %>%
    # reporting value for selected codes is million kWh, convert to EJ for GCAM
    mutate(value = Data * 3.6E-6) %>%
    select(region = StateCode, year = Year, description = Description, value)
  write_csv(seds_2024, paste0(data_dir, "/seds_2024.csv"))

}

{ ## . extract from queries ----
  in_by_tech      <- getQuery(prj, "USA inputs by tech")
  out_by_tech     <- getQuery(prj, "USA outputs by tech")
  resource_prod   <- getQuery(prj, "resource production")
}

# 2. Extract energy and water flows ---------------------------------------
{
  ## . clean up fields ----
  in_tech <- in_by_tech %>%
    # filter to US regions and selected time range
    gather_year_filt(year_filt = FILTER_YEARS) %>%
    filter_USregions() %>%
    extract_state() %>%
    mutate(# fix incorrect units to allow filtering by EJ next
      Units = case_when(
        input %in% c("regional corn", "regional oilcrop") ~ "Mt",
        input == "seawater" ~ "km^3",
        input == "industrial processes" ~ NA,
        TRUE ~ Units),
      scenario = factor(scenario)) %>%
    filter(Units %in% c("EJ") | input %in% c("uranium"), scenario %in% SCENARIO,
      # remove intl trade volumes (e.g. Africa_Eastern traded biomass)
      !grepl(paste(c("USA", nonUSregions), collapse = "|"), subsector),
      # remove irrelevant inputs
      input %!in% c("oil-credits", "renewable")) %>%
    # RFD is rainfed and IRR is irrigated crops > should be summed
    # hi/lo appliance and high/low fert efficiency for crop yields summed here
    remove_hi_lo() %>%
    # remove expected retirement dates from electricity technology names
    clean_cooling_tech_years(column = "technology") %>%

    # collapse electricity modes (e.g Apr/night/superpeak)
    remove_month_day_night_superpeak(sector) %>%
    remove_month_day_night_superpeak(subsector) %>%
    remove_month_day_night_superpeak(technology) %>%
    remove_month_day_night_superpeak(input) %>%

    # group for things like hi efficiency appliances and hi/lo fertilizer
    group_by(Units, scenario, region, year, input, sector, subsector, technology) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    replace(., . == "", NA)   # convert blanks saved as "" to proper NAs


  out_tech <- out_by_tech %>%
    # filter to US regions and selected time range
    gather_year_filt(year_filt = FILTER_YEARS) %>%
    filter_USregions() %>%
    extract_state() %>%
    filter(Units %in% c("EJ"), scenario %in% SCENARIO) %>%
    # RFD is rainfed and IRR is irrigated crops > should be summed
    # hi/lo appliance and high/low fert efficiency for crop yields summed here
    remove_hi_lo() %>%
    # remove expected retirement dates from electricity technology names
    clean_cooling_tech_years(column = "technology") %>%
    # collapse electricity modes (e.g Apr/night/superpeak)
    remove_month_day_night_superpeak(sector) %>%
    remove_month_day_night_superpeak(subsector) %>%
    remove_month_day_night_superpeak(technology) %>%
    remove_month_day_night_superpeak(output) %>%
    # group for things like hi efficiency appliances and hi/lo fertilizer
    group_by(Units, scenario, region, year, output, sector, subsector, technology) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    replace(., . == "", NA)   # convert blanks saved as "" to proper NAs


# 3. Apportion state fossil extraction by shares --------------------------
  # fossil_prod is an output from CMP429's reference scenario with fossil
  # resource production broken out by state
  state_fossil_prod <- fossil_prod %>%
    select(-scenario) %>%
    filter(year %in% FILTER_YEARS) %>%
    filter_USregions() %>%
    mutate(resource = if_else(grepl("unconventional", subresource) & resource == "crude oil",
                              "unconventional oil", resource))

  # resource_prod is the resource production from the IM3 query db. it only has
  # resource production by USA total. Use shares from CMP429 to apportion the
  # IM3 total to the states
  state_fossil_rsrc <- resource_prod %>%
    filter(Units == "EJ", year %in% FILTER_YEARS, resource %in% FOSSIL_MAPPING) %>%
    filter_USregions() %>%
    rename(us_total = value) %>%
    left_join(state_fossil_prod, by = c("year", "resource")) %>%
    group_by(resource, subresource, year) %>%
    mutate(cal_rsrc = replace_na(value / sum(value) * us_total, 0),
           resource = str_to_title(resource),
           resource = if_else(grepl("Oil", resource), "Oil", resource),
           subresource = str_to_title(subresource)) %>%
    ungroup() %>%
    select(scenario, region = region.y, source = subresource, target = resource,
           year, value = cal_rsrc)

  # downscale uranium for nuclear as it is not on the state level in IM3 GCAMUSA
  # data is from EIA Domestic Uranium Production Quarterly Report, Table 1
  # from the Q1 2026 report and represents uranium concentrate production in lbs
  state_uranium <- uranium_2025 %>%
    pivot_longer(names_to = "year", values_to = "value", cols = -c(Facility, Location, State)) %>%
    filter(!is.na(Location), grepl("2025", year)) %>%
    group_by(region = State) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(resource = "uranium", share_us_prod = value / sum(value))

  # according to EIA Uranium Marketing Annual Report Table S1a, the US purchased
  # a total of 46.9 million lbs U3O8 equivalent in 2025. the U.S.-origin value
  # doesn't match the quarterly report figures, possibly because of country of
  # origin changing mid-supply chain (e.g. mined elsewhere and enriched in the US)
  uranium_sources <- uranium_purchase %>%
    select(year = `Delivery year`, total = `Total purchased`, us_origin = `U.S.-origin uranium`) %>%
    filter(us_origin != "W", year %in% FILTER_YEARS) %>%
    mutate(resource = "uranium") %>%
    left_join(state_uranium, by = "resource") %>%
    mutate(total =  as.numeric(total),
           us_origin = as.numeric(us_origin),
           value.mmlb = us_origin * share_us_prod,
           non_us = total - us_origin) %>%
    select(region, year, value.mmlb, us_origin, non_us)


# 4. Process primary and secondary energy ---------------------------------
  # TODO: remove these two after testing
  inputs_map <- read_csv(paste0(data_dir, "./mappings/inputs_tech_mapping.csv"))
  source("./R/0_functions.R")

  simple_sources <- in_tech %>%
    filter(region %in% states) %>%
    left_join(inputs_map, by = join_by(sector, subsector, technology, input)) %>%
    drop_na(input_L1) %>%
    filter(output_L1 != input_L1)

  simple_outputs <- out_tech %>%
    filter(region %in% states) %>%
    left_join(inputs_map, by = join_by(sector, subsector, technology)) %>%
    drop_na(input_L1) %>%
    filter(output_L1 != input_L1)

  usa_only_in <- in_tech %>%
    filter(region == "USA") %>%
    left_join(inputs_map, by = join_by(sector, subsector, technology, input)) %>%
    drop_na(input_L1) %>%
    filter(output_L1 != input_L1)

  usa_only_out <- out_tech %>%
    filter(region == "USA") %>%
    left_join(inputs_map, by = join_by(sector, subsector, technology)) %>%
    drop_na(input_L1) %>%
    filter(output_L1 != input_L1)


  ## PRIMARY ENERGY PROCESSING ------------------
  {
    uranium_cons <- simple_sources %>%
      # thermal splitting for H2 production not on the state level in this GCAM
      # version so have to add back in the USA region before filtering inputs
      # TODO: add in hydrogen production here??
      rbind(usa_only_in) %>%
      filter(grepl("nuclear", input)) %>%
      group_by(scenario, region, year, source = input_L1) %>%
      summarize(value.cons = sum(value), .groups = "drop") %>%
      mutate(source = "Uranium")

    # TODO: separate out biomass and hydrogen trade; currently assume production = consumption
    non_fossil_prod <- simple_sources %>%
      filter(input_L1 %in% RENEWABLES) %>%
      select(scenario, region, year, source = input_L1, target = output_L1, value)

    # attribute nuclearFuel to state where concentrate is produced due to data
    # availability and to simplify the supply chain representation; if future
    # branches separate enrichment from mining activities will need to update
    uranium_imports <- uranium_sources %>%
      mutate(region = "USA") %>%
      select(region, year, value.mmlb = non_us) %>%
      distinct()

    uranium_prod <- uranium_sources %>%
      select(-us_origin, -non_us) %>%
      rbind(uranium_imports) %>%
      mutate(share = value.mmlb / sum(value.mmlb), year = as.integer(year)) %>%
      # don't need non-us origin line item after shares calculated
      filter(region != "USA") %>%
      left_join(uranium_cons, by = c("region", "year")) %>%
      mutate(value.prod = sum(value.cons) * share) %>%
      select(scenario, region, year, source, value.prod)

    primary_en_cons <- simple_sources %>%
      rename(source = input_L1, target = output_L1) %>%
      # PRIMARY_EN label is specific to resource query so instead exclude
      # secondary and notation sources here
      # TODO: this might be okay for fossil_mapping now
      filter(source %!in% c(SECONDARY_EN, REFINING_TYPE)) %>%
      group_by(scenario, region, year, source) %>%
      summarize(value.cons = sum(value), .groups = "drop") %>%
      rbind(uranium_cons)

    primary_en_prod <- state_fossil_rsrc %>%  # coal, NG, oil
      swap_source_target() %>%
      rbind(non_fossil_prod) %>%
      group_by(scenario, region, year, source) %>%
      summarize(value.prod = sum(value), .groups = "drop") %>%
      rbind(uranium_prod)

    # Display net trade (either imports or exports)
    primary_en_tradebal <- primary_en_prod %>%
      full_join(primary_en_cons, by = c("scenario", "region", "year", "source")) %>%
      complete(nesting(scenario, region, year), source) %>%
      replace(., is.na(.), 0) %>%
      mutate(export = pmax(value.prod - value.cons, 0),
             import = pmax(value.cons - value.prod, 0)) %>%
      select(-value.prod, -value.cons)

    primary_prod_clean <- primary_en_tradebal %>%
      filter(import > 0 | export > 0) %>%
      left_join(primary_en_prod, by = c("scenario", "region", "year", "source")) %>%
      mutate(target = if_else(source == "Uranium", "Nuclear", source),
             source = paste0("Produced ", source)) %>%
      select(scenario, region, year, source, target, value = value.prod) %>%
      na.omit()
  }

  ## SECONDARY ENERGY PROCESSING --------------
  {
    ###  A. Refining ----
    refining_input <- simple_sources %>%  # Total refining inputs
      filter(output_L1 %in% REFINING_TYPE) %>%
      group_by(scenario, region, year, sector = output_L1) %>%
      summarize(value.in = sum(value), .groups = "drop")

    refining_output <- simple_sources %>%  # Total refining outputs
      filter(output_L1 == "Liquid Fuels") %>%
      select(scenario, region, year, sector = input_L2, value.out = value)

    refining_loss <- refining_input %>%    # Subsector energy losses
      left_join_error_no_match(refining_output,
                               by = c("scenario", "region", "year", "sector")) %>%
      mutate(losses = value.in - value.out,
             target = "Losses",
             source_color = unname(COLORMAP[target]),
             target_color = unname(COLORMAP[target])) %>%
      select(scenario, region, year, source = sector, target, value = losses,
             source_color, target_color)

    refining_loss_long <- refining_loss %>%
      group_by(scenario, region, year, target, source_color, target_color) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(source = "Liquid Fuels", level = 1) %>%
      rbind(refining_loss %>% mutate(level = 2),
            refining_loss %>% mutate(level = 3))

    refined_cons <- simple_sources %>%
      filter(input_L1 == "Liquid Fuels") %>%
      group_by(scenario, region, year, input_L1) %>%
      summarize(value.cons = sum(value), .groups = "drop")

    refined_trade <- refined_cons %>%
      left_join(refining_output %>%
                  group_by(scenario, region, year) %>%
                  summarize(value.out = sum(value.out), .groups = "drop"),
                by = c("scenario", "region", "year")) %>%
      replace(., is.na(.), 0) %>%
      # if outputs from refining sector are greater than the state's consumption
      # plus refining sector losses then net the rest as exports and vice versa
      mutate(export = pmax(value.out - value.cons, 0),
             import = pmax(value.cons - value.out, 0)) %>%
      select(-value.out, -value.cons)

    ## B. Electricity ----
    #       - by default shows electricity generation technologies
    #       - technology losses kept with technology, 'electricity' losses equal
    #         to transmission losses (factor from gcamdata) and ownuse (e.g.
    #         keep the lights on at the generation station)
    # electricity domestic supply comes from grid ownuse/trade
    # proxy to net for now; electricity supplies electricity domestic supply +
    # the rest is "exported electricity"

    # this sums every input to the electricity sector
    # (e.g. regional coal, onshore wind resource)
    elec_inputs <- simple_sources %>%
      filter(grepl("Electricity", output_L1)) %>%
      select(scenario, region, year, sector, subsector, technology, val.in = value)

    # final electricity produced by each generation technology
    elec_gen_out <- simple_outputs %>%
      filter(output == "electricity") %>%
      rename(val.out = value)

    # sums produced electricity by state and calculates TD losses for the total
    elec_gen_out_tot <- elec_gen_out %>%
      group_by(scenario, region, year) %>%
      summarize(val.prod = sum(val.out), .groups = "drop") %>%
      mutate(td_loss = val.prod * TD_IO - val.prod)

    # sum all electricity demands from end uses (not just end use sectors, also
    # liquid fuel transformation)
    elec_end_use <- simple_sources %>%
      filter(grepl("Electricity", input_L1)) %>%
      group_by(scenario, region, year, input_L1) %>%
      summarize(val.cons = sum(value), .groups = "drop")

    # get technology-level losses for the electricity sector by subtracting
    # outputs by tech from inputs by tech;
    elec_tech_loss <- elec_inputs %>%
      full_join(elec_gen_out, by = c("scenario", "sector", "region", "year",
                                      "subsector", "technology")) %>%
      mutate(val.in = if_else(subsector == "hydro" | sector == "industrial energy use",
                              val.out, val.in),
             tech_loss = val.in - val.out,
             # this method gives some small negatives for wind and solar in
             # some regions. For now, zero those out.
             tech_loss = if_else(tech_loss < 0, 0, tech_loss),
             target = "Losses",
             source_color = unname(COLORMAP[target]),
             target_color = unname(COLORMAP[target]))

    # TODO: create a downstream flow next to losses saying industrial cogen for now
    #   - we can discuss how to deal with it properly when we meet
    # take another look at the elec bal node (does cogen go to the elec market?? all consumed on site??)
    # have the cogen downstream node, add to rejected energy->loss split

    elec_bal <- elec_end_use %>%
      full_join(elec_gen_out_tot, by = c("scenario", "region", "year")) %>%
      mutate(export = pmax(val.prod - (val.cons + td_loss), 0),
             import = pmax((val.cons + td_loss) - val.prod, 0)) %>%
      select(-val.prod, -val.cons) %>%
      # delaware coal is in inputs by tech but not outputs by tech
      # zero for now but need to understand why
      replace(., is.na(.), 0)

    td_losses <- elec_gen_out_tot %>%
      select(scenario, region, year, value = td_loss) %>%
      mutate(source = "Electricity",
             target = "Losses",
             source_color = unname(COLORMAP[target]),
             target_color = unname(COLORMAP[target]))

    elec_losses_l1 <- elec_tech_loss %>%
      group_by(scenario, region, year, input_L2,
               target, source_color, target_color) %>%
      summarize(value = sum(tech_loss), .groups = "drop") %>%
      mutate(level = 1) %>%
      rename(source = input_L2)

    elec_losses_long <- elec_losses_l1 %>%
      rbind(elec_losses_l1 %>% mutate(level = 2),
            elec_tech_loss %>%
              select(scenario, region, year, source = input_L3, target,
                     source_color, target_color, value = tech_loss) %>%
              mutate(level = 3),
            elec_tech_loss %>%
              select(scenario, region, year, source = input_L3, target,
                     source_color, target_color, value = tech_loss) %>%
              mutate(level = 4),
            td_losses %>% mutate(level = 1),
            td_losses %>% mutate(level = 2),
            td_losses %>% mutate(level = 3),
            td_losses %>% mutate(level = 4))

    ## C. Hydrogen ----
    hydrogen_cons <- simple_sources %>%
      filter(input_L1 == "Hydrogen") %>%
      group_by(scenario, region, year, input_L1) %>%
      summarize(val.cons = sum(value), .groups = "drop") %>%
      mutate(total = sum(val.cons),
             share = val.cons/total)

    hydrogen_prod <- usa_only_in %>%
      filter(grepl("H2.*production", sector)) %>%
      select(-region) %>%
      group_by(scenario, year, input_L1) %>%
      summarize(val.prod = sum(value), .groups = "drop") %>%
      left_join(hydrogen_cons %>% select(scenario, region, year, share, target = input_L1), by = c("scenario", "year")) %>%
      mutate(value = val.prod * share,
             source_color = unname(COLORMAP[input_L1]),
             target_color = unname(COLORMAP[target])) %>%
      select(scenario, region, year, source = input_L1, target, value, source_color, target_color)

    hydrogen_loss <- hydrogen_prod %>%
      group_by(scenario, year) %>%
      summarize(val.prod = sum(value), .groups = "drop") %>%
      left_join(hydrogen_cons %>% select(scenario, region, year, val.cons), by = c("scenario", "year")) %>%
      mutate(total_cons = sum(val.cons),
             total_loss = val.prod - total_cons,
             share = val.cons / total_cons,
             value = share * total_loss,
             source = "Hydrogen",
             target = "Losses",
             source_color = unname(COLORMAP[target]),
             target_color = unname(COLORMAP[target])) %>%
      select(scenario, region, year, source, target, value, source_color, target_color)

  }

  { ## C. Trade Balancing ----
    energy_tradebal <- primary_en_tradebal %>%
      rbind(refined_trade %>% rename(source = input_L1),
            elec_bal %>% rename(source = input_L1) %>% select(-td_loss))

    en_imports_long <- energy_tradebal %>%
      select(scenario, region, year, target = source, value = import) %>%
      filter(value > 0) %>%
      mutate(source = paste("Net Imported", target),
             target = if_else(target == "Uranium", "Nuclear", target))

    en_exports_long <- energy_tradebal %>%
      select(scenario, region, year, source, value = export) %>%
      filter(value > 0) %>%
      mutate(target = paste("Net Exported", source),
             source = if_else(source == "Uranium", "Nuclear", source))

    en_tradebal <- rbind(en_imports_long, en_exports_long) %>%
      mutate(source_color = coalesce(unname(COLORMAP[source]), unname(COLORMAP[target])),
             target_color = source_color)
  }


  # !!!!! setting up the long df ----
  # coarse: input_L1 -> output_L1
  # medium: input_L1 -> input_L2 -> output_L1 -> output_L2
  # fine: input_L0 -> input_L1 -> input_L3 (colored by input_L2) -> output_L1 -> output_L2 -> output_L3
  # finer: input_L0 -> input_L1 -> input_L3 (colored by input_L2) -> output_L1 -> output_L2 -> output_L3 -> output_L4
  {
    source("./R/0_functions.R")
    for_display <- simple_sources %>%
      mutate(source_color = COLORMAP[input_L1],
             target_color = COLORMAP[output_L1])

    # TODO: refactor as function ?
    # coarsest detail
    in1_out1 <- for_display %>%
      rename(source = input_L1, target = output_L1) %>%
      mutate(level = 1) %>%
      filter(target != "Electricity") %>%
      select(all_of(KEEP_NAMES)) # does not include trade/loss/primary extraction
    in1_in2_elec <- for_display %>%
      rename(source = input_L1, target = input_L2) %>%
      mutate(level = 1) %>%
      filter(output_L1 == "Electricity") %>%
      select(all_of(KEEP_NAMES))
    # if losses are kept on the technology level then need to remove input EJ
    # to electricity node to avoid double counting
    in2_out1_elec <- elec_gen_out %>%
      rename(source = input_L2, target = output_L1, value = val.out) %>%
      mutate(level = 1,
             source_color = COLORMAP[input_L1],
             target_color = COLORMAP[target]) %>%
      # TODO: cogeneration elec generation is categorized as Industry on the first level
      filter(target == "Electricity") %>%
      select(all_of(KEEP_NAMES))

    l1_long <- rbind(in1_out1, in1_in2_elec, in2_out1_elec) %>%
      filter(source %!in% REFINING_TYPE) %>%
      mutate(target = if_else(target %in% REFINING_TYPE, "Liquid Fuels", target))

    # medium detail beyond just electricity
    in1_in2 <- for_display %>%
      rename(source = input_L1, target = input_L2) %>%
      mutate(level = 2) %>%
      filter(output_L1 != "Electricity") %>%
      select(all_of(KEEP_NAMES))
    in2_out2 <- for_display %>%
      rename(source = input_L2, target = output_L2) %>%
      mutate(level = 2) %>%
      # feed to elec gen technologies require modified balance
      filter(output_L1 != "Electricity") %>%
      select(all_of(KEEP_NAMES))
    in2_out2_elec <- elec_gen_out %>%
      rename(source = input_L2, target = output_L2, value = val.out) %>%
      mutate(level = 1,
             source_color = COLORMAP[input_L1],
             target_color = COLORMAP[output_L1]) %>%
      # TODO: cogeneration elec generation is categorized as Industry on the first level
      filter(target == "Electricity") %>%
      select(all_of(KEEP_NAMES))

    l2_long <- rbind(in1_in2,
                     in1_in2_elec %>% mutate(level = 2),
                     in2_out2,
                     in2_out1_elec %>% mutate(level = 2))


    # filter level 3
    in1_in3 <- for_display %>%
      rename(source = input_L1, target = input_L3) %>%
      mutate(level = 3) %>%
      filter(output_L1 != "Electricity") %>%
      select(all_of(KEEP_NAMES))
    in1_in3_elec <- for_display %>%
      rename(source = input_L1, target = input_L3) %>%
      mutate(level = 3) %>%
      filter(output_L1 == "Electricity") %>%
      select(all_of(KEEP_NAMES))
    out1_out2_lvl3 <- for_display %>%
      rename(source = output_L1, target = output_L2) %>%
      mutate(level = 3) %>%
      select(all_of(KEEP_NAMES))
    in3_out1 <- for_display %>%
      rename(source = input_L3, target = output_L1) %>%
      mutate(level = 3) %>%
      # feed to elec gen technologies require modified balance
      filter(target != "Electricity") %>%
      select(all_of(KEEP_NAMES))
    in3_out1_elec <- elec_gen_out %>%
      rename(source = input_L3, target = output_L1, value = val.out) %>%
      mutate(level = 3,
             source_color = COLORMAP[input_L1],
             target_color = COLORMAP[target]) %>%
      # TODO: cogeneration elec generation is categorized as Industry on the first level
      filter(target == "Electricity") %>%
      select(all_of(KEEP_NAMES))
    out2_out3 <- for_display %>%
      rename(source = output_L2, target = output_L3) %>%
      mutate(level = 3) %>%
      select(all_of(KEEP_NAMES))

    l3_long <- rbind(in1_in3, in1_in3_elec,
                     in3_out1, in3_out1_elec,
                     out1_out2_lvl3)

    # highly detailed end use to level 4
    out3_out4 <- for_display %>%
      rename(source = output_L3, target = output_L4) %>%
      mutate(level = 4) %>%
      select(all_of(KEEP_NAMES))

    l4_long <- rbind(l3_long %>% mutate(level = 4), out3_out4)


    primary_en_prod_long <- rbind(primary_prod_clean %>% mutate(level = 1),
                                  primary_prod_clean %>% mutate(level = 2),
                                  state_fossil_rsrc %>% mutate(level = 3)) %>%
      mutate(source_color = unname(COLORMAP[target]), target_color = source_color)

    hydrogen_prod_long <- rbind(hydrogen_prod %>% mutate(level = 1),
                                hydrogen_prod %>% mutate(level = 2),
                                hydrogen_prod %>% mutate(level = 3),
                                hydrogen_prod %>% mutate(level = 4))

    hydrogen_loss_long <- rbind(hydrogen_loss %>% mutate(level = 1),
                                hydrogen_loss %>% mutate(level = 2),
                                hydrogen_loss %>% mutate(level = 3),
                                hydrogen_loss %>% mutate(level = 4))


    DISPLAY_LEVEL = 3  # TODO: remove after testing

    # if at a low resolution display level this will have repeats that need
    # summing by group
    combo_grp <- rbind(l1_long, l2_long, l3_long,
                       primary_en_prod_long,
                       hydrogen_prod_long,
                       refining_loss_long,
                       elec_losses_long,
                       hydrogen_loss_long) %>%
      filter(source != target, level == DISPLAY_LEVEL) %>%
      select(-level) %>%
      # then add net energy trade because it is independent of display level
      # due to lack of more granular data
      rbind(en_tradebal) %>%
      # fix processing sector colors (otherwise biorefining eg has elec,
      # biomass and NG colors because they are inputs and want to only color by
      # the primary input)
      mutate(target_color = if_else(target %in% REFINING_TYPE,
                                    COLORMAP[source], target_color),
             source_color = if_else(source %in% REFINING_TYPE & target != "Losses",
                                    COLORMAP[source], source_color),
             source_color = if_else(source %in% REFINING_TYPE,
                                    COLORMAP[target], source_color)) %>%
      group_by(scenario, region, year, source, target, source_color, target_color) %>%
      summarize(value = sum(value), .groups = "drop")

    plot_sankey(combo_grp, paste0("Level ", DISPLAY_LEVEL, " energy flows view"), reg = "CA", yr = 2025)


    # what does this type of visualization help convey more easily between state EWN?
    # test that other years and time progression works too
    # is there a better package for styling?
    # be able to highlight net import / export for diff energy sources?
    # for hydropower: HOW DID I BREAK THIS WHERE DID IT GO; there's EJ hydro in outputs by tech, compare with seds??

    # NOT STARTED
    # TODO: separate levels for energy sources and end uses so users can expand either side independently
    # TODO: group L3/4 by cooling tech or by energy tech?
    # TODO: should subsectors be separate nodes or have slightly different colors (how to do programmatically?)
    # TODO: give the option to group multiple states (e.g. look at new england, pnw, southeast etc)
    #        and maybe show the percent of a highlight state compared to the neighborhood? like on hover
    #        over highlight just that state's contributions?
    # TODO: merge with water sankey
    # TODO: move mappings into 0_functions as a preload?
    # TODO: save the processed csv and add reprocessing toggle (ensure last stage only needs to be filtering)
    # TODO: may need to separate location filter part of pipeline
    # TODO: should superpeak/base really be collapsed at top of part 2? or removed?

    # IN PROGRESS
    # TODO: do we want to always show the cogeneration loop, or pull it into other electricity inputs?
    # TODO: condense the labels for the electricity techs
    # TODO: keep same node for things like transport but disagg colors for road/rail/ship etc??
    # TODO: display by tech or by cooling type on higher levels? what is most compact but still communicative?
    # TODO: DOCUMENTATION, make choices clear

    # STYLING
    # TODO: color end use details by the main end use category
    # TODO: bundle exports vertically? like losses, exports, in-state energy consumption, water
    #          also want them to align with secondary prod and not end use consumption?
    # TODO: adjust labels to include value and units
    # TODO: figure out how to fix some of the weird auto y position ordering
    # TODO: add color legend
    # TODO: have a colormap toggle for different levels of detail
    #        - right now (but should liquid fuels be the same color as oil? just have a fossil color?)
    #        - fossil vs renewable + end use same nodes
    #        - same as im3 dash (orange for all non electricity, yellow elec, gray losses)
    # TODO: have some kind of on click persistent highlight + better tooltip
    # TODO: on select just highlight the path you've selected?
    # TODO: make hover over more useful
    # TODO: pin the x axis for stuff like refining and biorefining so they align;
    #         does it make sense to have like a primary, transformation, end use bar at the bottom?

  }
}


########################### Archive #################################
# 4. Source and target mapping --------------------------------------------

############## Just map connections for testing #####################
# {
#   edges <- in_tech %>%
#     # left_join(source_mapping_e, by = c("input")) %>%
#     # left_join(target_mapping_e, by = c("sector")) %>%
#     # left_join(source_mapping_w, by = c("input" = "source")) %>%
#     # left_join(target_mapping_w, by = c("sector" = "target")) %>%
#     left_join(inputs_map, by = c("sector", "subsector", "technology", "input")) %>%
#
#     rename_water_use_categories(column = sector) %>%
#     rename_water_use_categories(column = input) %>%
#     # TODO: just make a better mapping file, partitioned by levels to aggregate
#     # mutate(Source = if_else(is.na(Source), smap, Source),
#     #        Target = if_else(is.na(Target), tmap, Target),
#     #        #Source = if_else(is.na(Source), input, Source),
#     #        #Target = if_else(is.na(Target), sector, Target),
#     #        Source = if_else(Source %in% basins, "Basin", Source)) %>%
#     # rename_regional(column = Source) %>%
#     # filter(Source != Target,
#     #        !grepl("Forest|carbon-storage", Source)) %>%
#     # select(State, year, Source, Target, sector, subsector, input) %>%
#     filter(State %in% c("CA", "USA")) %>%
#     rename(Source = input1, Target = sector1) %>%
#     #select(Source, Target) %>%
#     distinct()
#
#   nodes <- edges %>%
#     select(Target) %>%
#     rbind(edges %>% select(Source) %>% rename(Target = Source)) %>%
#     distinct() %>%
#     mutate(id = row_number()) %>%
#     rename(node = Target)
#
#   edf <- edges %>%
#     left_join(nodes, by = c("Target" = "node")) %>%
#     rename(dependent = id) %>%
#     left_join(nodes, by = c("Source" = "node")) %>%
#     rename(precursor = id)
#
#   # Format nodes for visNetwork
#   vis_nodes <- nodes %>%
#     transmute(
#       id    = id,
#       label = node,
#       title = paste0("<b>", node, "</b>"),   # tooltip on hover
#       color = "lightblue",
#       font.size = 14
#     )
#
#   # Format edges for visNetwork
#   vis_edges <- edf %>%
#     transmute(
#       from   = precursor,
#       to     = dependent,
#       value  = 1,
#       width  = scales::rescale(value, to = c(0.5, 5)),
#       title  = paste0(Source, " → ", Target, "<br>Value: ", round(value, 2)),
#       color  = "rgba(100,100,100,0.5)",
#       arrows = "to"
#     )
#
#   # Render interactive network
#   layout_options <- c("layout_in_circle", "layout_nicely", "layout_as_tree",
#                       "layout_with_sugiyama")
#   visNetwork(vis_nodes, vis_edges,
#              main = "Input-Output Network",
#              width = "100%", height = "1080px") %>%
#     visIgraphLayout(layout = "layout_nicely") %>%
#     visEdges(
#       arrows = "to",
#       physics = FALSE,
#       color = list(color = 'black'),
#       smooth = FALSE
#     ) %>%
#     visNodes(physics = FALSE) %>%
#     visInteraction(
#       dragNodes        = TRUE,
#       dragView         = TRUE,
#       zoomView         = TRUE,
#       hover            = TRUE,
#       navigationButtons = TRUE,
#       keyboard         = TRUE
#     ) %>%
#     visOptions(
#       highlightNearest = list(enabled = TRUE, degree = 1, hover = F),
#       nodesIdSelection = TRUE,
#       manipulation = FALSE,
#       selectedBy       = "label"
#     ) %>%
#     visLayout(randomSeed = 789)
# }

