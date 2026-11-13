# Data processing for energy flows from GCAM as sankey/alluvial inputs
#
# Authors: Maggie Liu, Hassan Niazi
# Feb 2026

library("visNetwork")
library("arrow")

# 0. Setup ----------------------------------------------------------------
source("./R/0_functions.R")

# User choices
UPDATE_DATA    <- FALSE
FUTURE_YEARS   <- 2020:2100

# Primary fuels
RENEWABLES     <- c("Nuclear", "Wind", "Solar", "Biomass", "Hydropower", "Geothermal")
FOSSIL_FUELS   <- c("Coal", "Oil", "Natural Gas")
PRIMARY_EN     <- c(FOSSIL_FUELS, RENEWABLES)
FOSSIL_MAPPING <- c("coal", "crude oil", "unconventional oil", "natural gas")

# Secondary fuels
SECONDARY_EN   <- c("Electricity", "Liquid Fuels", "Hydrogen")
REFINING_TYPE  <- c("Oil Refining", "Biorefining", "Coal to Liquids", "Gas to Liquids")
ELEC_LOADTYPE  <- c("superpeak", "subpeak", "peak", "baseload", "intermediate")
# TODO: bring a file, left join for all regions possible in future
TD_IO          <- 1.06237346267644    # 2021 USA I:O value for L126.IO_R_electd_F_Yh

# End use
DEMAND_SECTORS <- c("Industry", "Buildings", "Transport")

# 1. Load data ------------------------------------------------------------
options(scipen = 999)
DB_NAME        <- 'database_rcp85hotter_ssp5'
DB_PATH        <- 'data'
PROJ           <- "data\\im3scen_energy1.dat"
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
  uranium_sources  <- read_csv(paste0(data_dir, "/uranium_sources.csv"))
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
    # this is taken from base year 2021; all 2020 entries are empty. cast 2021
    # to 2020 to align with IM3 scenario outputs
    filter(year != 2020) %>%
    mutate(year = if_else(year == 2021, 2020, year)) %>%
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
    filter(us_origin != "W", year %in% 2025) %>%  # data set ends at 2025
    mutate(resource = "uranium") %>%
    left_join(state_uranium, by = "resource") %>%
    mutate(total =  as.numeric(total),
           us_origin = as.numeric(us_origin),
           value.mmlb = us_origin * share_us_prod,
           non_us = total - us_origin,
           frac_state = value.mmlb / (us_origin + non_us)) %>%
    select(region, year, value.mmlb, us_origin, non_us, frac_state)
  write_csv(uranium_sources, paste0(data_dir, "/uranium_sources.csv"))

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
    gather_years() %>%
    filter_USregions() %>%
    extract_state() %>%
    mutate(# fix incorrect units to allow filtering by EJ next
      Units = case_when(
        input %in% c("regional corn", "regional oilcrop") ~ "Mt",
        input == "seawater" ~ "km^3",
        input == "industrial processes" ~ NA,
        TRUE ~ Units),
      scenario = factor(scenario)) %>%
    filter(Units %in% c("EJ") | input %in% c("uranium"), year %in% FUTURE_YEARS,
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
    gather_years() %>%
    filter_USregions() %>%
    extract_state() %>%
    filter(Units %in% c("EJ"), year %in% FUTURE_YEARS) %>%
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
    filter(year %in% FUTURE_YEARS) %>%
    filter_USregions() %>%
    mutate(resource = if_else(grepl("unconventional", subresource) & resource == "crude oil",
                              "unconventional oil", resource))

  # resource_prod is the resource production from the IM3 query db. it only has
  # resource production by USA total. Use shares from CMP429 to apportion the
  # IM3 total to the states
  state_fossil_rsrc <- resource_prod %>%
    filter(Units == "EJ", year %in% FUTURE_YEARS, resource %in% FOSSIL_MAPPING) %>%
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


  ## HYDROGEN PRODUCTION APPORTIONING ----------
  # computed before primary energy so H2 production inputs (biomass, coal,
  # natural gas, nuclear) are included in the primary energy accounting
  {
    hydrogen_cons <- simple_sources %>%
      filter(input_L1 == "Hydrogen") %>%
      group_by(scenario, region, year, input_L1) %>%
      summarize(val.cons = sum(value), .groups = "drop") %>%
      group_by(scenario, year) %>%
      mutate(total = sum(val.cons),
             share = if_else(total > 0, val.cons / total, 0)) %>%
      ungroup()

    # estimate state-level hydrogen production inputs from consumption shares.
    # in later versions of GCAM there is state-level production broken out
    hydrogen_prod <- usa_only_in %>%
      filter(grepl("H2.*production", sector)) %>%
      group_by(scenario, year, input_L1) %>%
      summarize(val.prod = sum(value), .groups = "drop") %>%
      inner_join(hydrogen_cons %>%
                   select(scenario, region, year, share, target = input_L1),
                 by = c("scenario", "year"), relationship = "many-to-many") %>%
      mutate(value = val.prod * share) %>%
      select(scenario, region, year, source = input_L1, target, value)
  }

  ## PRIMARY ENERGY PROCESSING ------------------
  {
    # thermal splitting for H2 production is not on the state level in this
    # GCAM version, so use the state-apportioned nuclear H2 inputs instead
    uranium_cons <- simple_sources %>%
      filter(grepl("nuclear", input)) %>%
      select(scenario, region, year, value) %>%
      rbind(hydrogen_prod %>%
              filter(source == "Nuclear") %>%
              select(scenario, region, year, value)) %>%
      group_by(scenario, region, year) %>%
      summarize(value.cons = sum(value), .groups = "drop") %>%
      mutate(source = "Uranium")

    # TODO: separate out biomass and hydrogen trade; currently assume production = consumption
    non_fossil_prod <- simple_sources %>%
      filter(input_L1 %in% RENEWABLES) %>%
      select(scenario, region, year, source = input_L1, target = output_L1, value) %>%
      rbind(hydrogen_prod %>%
              filter(source %in% RENEWABLES) %>%
              select(scenario, region, year, source, target, value))

    # attribute nuclearFuel to state where concentrate is produced due to data
    # availability and to simplify the supply chain representation; if future
    # branches separate enrichment from mining activities will need to update
    uranium_imports <- uranium_sources %>%
      mutate(region = "USA") %>%
      select(region, year, value.mmlb = non_us) %>%
      distinct()

    # national uranium consumption by scenario and year
    uranium_us_cons <- uranium_cons %>%
      group_by(scenario, year, source) %>%
      summarize(us_cons = sum(value.cons), .groups = "drop")

    uranium_prod <- uranium_sources %>%
      # use 2025 state production fractions for all future years; update if new
      # data become available
      distinct(region, frac_state) %>%
      cross_join(uranium_us_cons) %>%
      mutate(value.prod = us_cons * frac_state) %>%
      select(scenario, region, year, source, value.prod)

    primary_en_cons <- simple_sources %>%
      rename(source = input_L1, target = output_L1) %>%
      # PRIMARY_EN label is specific to resource query so instead exclude
      # secondary and notation sources here
      select(scenario, region, year, source, value) %>%
      # include state-apportioned hydrogen production inputs
      rbind(hydrogen_prod %>% select(scenario, region, year, source, value)) %>%
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
             source_key = sector,
             target_key = "Losses",
             source_position = "input_L2") %>%
      select(scenario, region, year, source = sector, target, value = losses,
             source_position, source_key, target_key)

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
             source_key = sector,
             target_key = "Losses") %>%
      filter(tech_loss > 0)

    # TODO: create a downstream flow next to losses saying industrial cogen for now
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
             source_key = source,
             target_key = target,
             source_position = "output_L1",
             level = 0)

    elec_losses_l1 <- elec_tech_loss %>%
      group_by(scenario, region, year, input_L2, target, source_key, target_key) %>%
      summarize(value = sum(tech_loss), .groups = "drop") %>%
      mutate(level = 1, source_position = "input_L2") %>%
      rename(source = input_L2)

    ## C. Hydrogen ----
    # hydrogen_cons and hydrogen_prod are computed before primary energy
    # processing (see HYDROGEN PRODUCTION APPORTIONING above)
    h2_inputs <- usa_only_in %>%
      filter(grepl("H2.*production", sector)) %>%
      group_by(scenario, year) %>%
      summarize(val_in = sum(value), .groups = "drop")

    h2_outputs <- usa_only_out %>%
      filter(grepl("H2.*production", sector)) %>%
      group_by(scenario, year) %>%
      summarize(val_out = sum(value), .groups = "drop")

    # national loss by technology -> share of national loss used to split
    # each state's loss across technologies
    h2_balance <- full_join(h2_inputs, h2_outputs, by = c("scenario", "year")) %>%
      replace_na(list(val_in = 0, val_out = 0)) %>%
      mutate(tech_loss = val_in - val_out) %>%
      group_by(scenario, year) %>%
      mutate(loss_share = tech_loss / sum(tech_loss)) %>%
      ungroup()

    # state loss = state inputs - state consumption, so each state balances
    h2_state_loss <- hydrogen_prod %>%
      group_by(scenario, region, year) %>%
      summarize(val.in = sum(value), .groups = "drop") %>%
      left_join(hydrogen_cons %>% select(scenario, region, year, val.cons),
                by = c("scenario", "region", "year")) %>%
      mutate(state_loss = val.in - val.cons)

    hydrogen_loss <- h2_state_loss %>%
      inner_join(h2_balance %>% select(scenario, year, loss_share),
                 by = c("scenario", "year"), relationship = "many-to-many") %>%
      transmute(
        scenario, region, year,
        source = "Hydrogen",
        target = "Losses",
        value = state_loss * loss_share,
        source_key = "Hydrogen",
        target_key = target
      )

    # per-state balance check: inputs == consumption + losses
    h2_check <- h2_state_loss %>%
      left_join(hydrogen_loss %>%
                  group_by(scenario, region, year) %>%
                  summarize(loss = sum(value), .groups = "drop"),
                by = c("scenario", "region", "year")) %>%
      mutate(resid = val.in - val.cons - coalesce(loss, 0))
    if (any(abs(h2_check$resid) > 1e-9)) {
      stop("Hydrogen not balanced in ", sum(abs(h2_check$resid) > 1e-9),
           " state-year-scenario rows")
    }
    if (any(h2_state_loss$state_loss < -1e-9)) {
      warning("Negative hydrogen losses in ", sum(h2_state_loss$state_loss < -1e-9),
              " state-year-scenario rows (USA outputs exceed inputs?)")
    }

  }

  { ## C. Trade Balancing ----
    energy_tradebal <- primary_en_tradebal %>%
      rbind(refined_trade %>% rename(source = input_L1),
            elec_bal %>% rename(source = input_L1) %>% select(-td_loss)) %>%
      mutate(source_key = source, target_key = source)

    en_imports_long <- energy_tradebal %>%
      select(scenario, region, year, target = source, value = import, source_key, target_key) %>%
      filter(value > 0) %>%
      mutate(source = paste("Net Imported", target),
             target = if_else(target == "Uranium", "Nuclear", target))

    en_exports_long <- energy_tradebal %>%
      select(scenario, region, year, source, value = export, source_key, target_key)  %>%
      filter(value > 0) %>%
      mutate(target = paste("Net Exported", source),
             source = if_else(source == "Uranium", "Nuclear", source))

    en_tradebal <- rbind(en_imports_long, en_exports_long) %>%
      mutate(source_position = if_else(grepl("Net", source), "input_L0", "input_L1"),
             target_position = if_else(grepl("Net", target), "input_L1", "output_L1"),
             level = 0,
             source_type = if_else(source %in% PRIMARY_EN, "PRIMARY", "SECONDARY"),
             target_type = if_else(target %in% c(SECONDARY_EN, REFINING_TYPE), "SECONDARY", "PRIMARY"))
  }

  { ## D. Losses ----
    refining_loss_long <- refining_loss %>%
      group_by(scenario, region, year, target, source_position, target_key) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(source = "Liquid Fuels", source_key = source, level = 1) %>%
      rbind(refining_loss %>% mutate(level = 2),
            refining_loss %>% mutate(level = 3))

    hydrogen_loss_long <- hydrogen_loss %>%
      mutate(level = 0,
             source_position = "input_L2")

    elec_losses_long <- elec_losses_l1 %>%
      rbind(elec_losses_l1 %>% mutate(level = 2),
            elec_tech_loss %>%
              select(scenario, region, year, source = input_L3, target,
                     source_key, target_key, value = tech_loss) %>%
              mutate(level = 3, source_position = "input_L3"),
            td_losses)

    losses <- rbind(refining_loss_long, hydrogen_loss_long, elec_losses_long) %>%
      mutate(target_position = "output_L4",
             source_type = "SECONDARY",
             target_type = "LOSSES")
  }

  # !!!!! setting up the long df ----
  {
    source("./R/0_functions.R")      # TODO: remove after testing
    for_display <- simple_sources %>%
      mutate(source_color = COLORMAP[input_L1],
             target_color = COLORMAP[output_L1],
             source_key = input_L1,
             target_key = output_L1,
             source_type = case_when(
               input_L1 %in% PRIMARY_EN ~ "PRIMARY",
               input_L1 %in% DEMAND_SECTORS ~ "END USE",
               TRUE ~ "SECONDARY"),
             target_type = case_when(
               output_L1 %in% DEMAND_SECTORS ~ "END USE",
               output_L1 %in% c(SECONDARY_EN, REFINING_TYPE) ~ "SECONDARY",
               TRUE ~ NA)
      )

    # Reused subsets. These retain the original filtering behavior, including
    # excluding rows with missing output_L1.
    non_electric_inputs <- for_display %>% filter(output_L1 != "Electricity")
    electric_inputs <- for_display %>% filter(output_L1 == "Electricity")

    # Electricity generation output uses val.out rather than input energy.
    # Preserve this distinction to avoid changing the electricity balance.
    electric_outputs <- elec_gen_out %>%
      filter(output_L1 == "Electricity") %>%
      mutate(source_color = COLORMAP[input_L1],
             target_color = COLORMAP[output_L1],
             source_key = input_L1,
             target_key = output_L1,
             source_type = case_when(
               input_L1 %in% PRIMARY_EN ~ "PRIMARY",
               input_L1 %in% DEMAND_SECTORS ~ "END USE",
               TRUE ~ "SECONDARY"),
             target_type = case_when(
               output_L1 %in% DEMAND_SECTORS ~ "END USE",
               output_L1 %in% c(SECONDARY_EN, REFINING_TYPE) ~ "SECONDARY",
               TRUE ~ NA))


    # --------------------------------------------------------------------------
    # Level 1: coarsest detail                             input_L1 -> output_L1
    # --------------------------------------------------------------------------
    l1_long <- bind_rows(
        make_edges(non_electric_inputs, "input_L1", "output_L1", level = 1),
        make_edges(electric_inputs, "input_L1", "input_L2", level = 1,),
        make_edges(electric_outputs, "input_L2", "output_L1", level = 1, value_col = "val.out")
      ) %>%
      filter(!(source %in% REFINING_TYPE)) %>%
      mutate(target = if_else(target %in% REFINING_TYPE,"Liquid Fuels", target))

    # --------------------------------------------------------------------------
    # Level 2: medium detail      input_L1 -> input_L2 -> output_L1 -> output_L2
    # --------------------------------------------------------------------------
    l2_long <- bind_rows(
      make_edges(non_electric_inputs,"input_L1", "input_L2", level = 2),
      make_edges(electric_inputs, "input_L1", "input_L2", level = 2),
      make_edges(non_electric_inputs, "input_L2", "output_L2", level = 2),
      make_edges(electric_outputs, "input_L2", "output_L1", level = 2,
                 value_col = "val.out")
    )

    # --------------------------------------------------------------------------
    # Level 3: detailed inputs and end uses
    #             input_L0 -> input_L1 -> input_L3 (colored by input_L2) ->
    #                output_L1 -> output_L2 -> output_L3 -> output_L4
    # --------------------------------------------------------------------------
    l3_long <- bind_rows(
      make_edges(non_electric_inputs, "input_L1", "input_L3", level = 3),
      make_edges(electric_inputs,"input_L1", "input_L3", level = 3),
      make_edges(non_electric_inputs,"input_L3", "output_L1",level = 3),
      make_edges(electric_outputs, "input_L3", "output_L1", level = 3, value_col = "val.out"),
      make_edges(for_display,"output_L1", "output_L2",level = 3),
      make_edges(for_display,"output_L2", "output_L3",level = 3),
      make_edges(for_display,"output_L3", "output_L4",level = 4),
    )

    # --------------------------------------------------------------------------
    # Level 0 and special cases
    # --------------------------------------------------------------------------
    primary_en_prod_long <- rbind(
      primary_prod_clean %>% mutate(level = 1),
      primary_prod_clean %>% mutate(level = 2),
      state_fossil_rsrc %>% mutate(level = 3)
      ) %>%
      mutate(source_key = target,
             target_key = target,
             source_position = "input_L0",
             target_position = "input_L1",
             source_type = "PRIMARY",
             target_type = source_type)

    # Level 0 is attached to all views rather than repeating in the df
    # the source for hydrogen are currently various primary energy categories;
    # at some point this may change to separate production technologies too
    hydrogen_prod_long <- hydrogen_prod %>%
      mutate(level = 0,
             source_key = source,
             target_key = target,
             source_position = "input_L1",
             target_position = "output_L1",
             source_type = "PRIMARY",
             target_type = "SECONDARY")


    # combine the display layers with primary energy, hydrogen production, and
    # losses. Add net energy trade last because it is independent of
    # display level due to lack of more granular data.
    combo_grp <- rbind(l1_long, l2_long, l3_long,
                       primary_en_prod_long, hydrogen_prod_long,
                       losses,
                       en_tradebal) %>%
      filter(source != target) %>%
      # if at a low resolution display level the df will still have repeats that
      # need summing by group
      group_by(scenario, region, year,
               source, source_key, source_position, source_type,
               target, target_key, target_position, target_type,
               level) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(
        color = if_else(
          target == "Losses",
          unname(COLORMAP["Losses"]),
          unname(COLORMAP[color_key(coalesce(source_key, source))])),
        units = "EJ",
        diagram = "Energy",
        year = as.integer(year),
        level = as.integer(level),
        ssp = str_extract(scenario, "[^_]+$"),
        rcp = str_extract(scenario,  "^.*?\\d{2}"),
        value = round(value, 6)
      )

    end_use_lookup <- end_use_sector_lookup(inputs_map)

    # Combine source and target to get unique nodes and their colors
    nodes <- bind_rows(
      combo_grp %>% distinct(node = source, position = source_position, key = source_key),
      combo_grp %>% distinct(node = target, position = target_position, key = target_key)
    ) %>%
      mutate(role   = classify_node(node, position),
             sector = classify_sector(key),
             color  = unname(COLORMAP[color_key(coalesce(key, node))]),
             # color priority: node's own color > its demand sector > a fuel key > Losses
             priority = case_when(
               key == node                    ~ 1L,
               key %in% DEMAND_SECTORS        ~ 2L,
               key %in% c(PRIMARY_EN, SECONDARY_EN, REFINING_TYPE) ~ 3L,
               key == "Losses"                ~ 9L,
               TRUE                           ~ 5L)) %>%
      arrange(node, priority) %>%
      group_by(node) %>%
      summarize(role    = first(na.omit(role)),
                sector  = first(na.omit(sector)),
                color   = first(na.omit(color)),
                n_roles = n_distinct(na.omit(role)),
                .groups = "drop") %>%
      left_join(end_use_lookup %>% select(node, eu_sector = sector), by = "node") %>%
      mutate(sector = if_else(role == "end_use", coalesce(sector, eu_sector), NA_character_),
             color  = if_else(role == "end_use", unname(COLORMAP[str_to_title(sector)]), color))

    stopifnot("nodes with conflicting roles" = all(nodes$n_roles == 1),
              "nodes missing color"          = !any(is.na(nodes$color)))
    nodes <- nodes %>%
      select(node, role, sector, color) %>%
      assign_layout(combo_grp, end_use_lookup)

    primary_ranks <- compute_primary_rank(combo_grp, PRIMARY_EN)

    validate_layout(combo_grp, nodes, primary_ranks)

    condensed <- combo_grp %>%
      # currently not displaying cooler scenarios for brevity so keep out of input
      # files to reduce load times
      filter(grepl("hotter", scenario)) %>%
      select(-c(target_key, source_type, target_type)) %>%
      select(scenario, ssp, rcp, region, year,
             source, source_position, source_key,
             target, target_position,
             level, value, color, units, diagram)


    # --------------------------------------------------------------------------
    # Write to file ----
    # --------------------------------------------------------------------------
    write_parquet(condensed, "gcam_usa_energy.parquet")

    # write level files separately to improve load times
    write_parquet(condensed %>% filter(level == 0), "gcam_usa_energy_l0.parquet")
    write_parquet(condensed %>% filter(level == 1), "gcam_usa_energy_l1.parquet")
    write_parquet(condensed %>% filter(level == 2), "gcam_usa_energy_l2.parquet")
    write_parquet(condensed %>% filter(level == 3), "gcam_usa_energy_l3.parquet")     # | level == 4

    # write the processed node label and layout info
    write_parquet(nodes, "gcam_usa_energy_nodes.parquet")
    write_parquet(primary_ranks, "gcam_usa_energy_primary_rank.parquet")

    # --------------------------------------------------------------------------
    # Test Display ----
    # --------------------------------------------------------------------------
    source("./R/0_functions.R")      # TODO: remove after testing
    # load from pre-processed file
    condensed <- read_parquet("gcam_usa_energy.parquet")
    nodes     <- read_parquet("gcam_usa_energy_nodes.parquet")

    DISPLAY_LEVEL <- 3  # TODO: remove after testing; lower value less detail
    SCENARIO      <- "rcp45hotter_ssp5"
    FILTER_YEARS  <- 2030
    STATE         <- "IL"
    plot_grp <- condensed %>%
      filter(scenario %in% SCENARIO, year %in% FILTER_YEARS, level %in% c(DISPLAY_LEVEL, 0))

    # Usage: single state plotting
    plot_sankey(plot_grp, nodes,
                paste0("Level ", DISPLAY_LEVEL, " energy flows view"),
                reg = STATE,
                yr = FILTER_YEARS,
                scen = SCENARIO,
                animate = F)


    # TODO: highlight others if i don't understand them
    # for comparison part > make some line plots, ww over time || had diff plots?

    # what does this type of visualization help convey more easily between state EWN?
    # test that other years and time progression works too
    # is there a better package for styling?
    # be able to highlight net import / export for diff energy sources?

    # NOT STARTED
    # TODO: separate levels for energy sources and end uses so users can expand either side independently
    # TODO: group L3/4 by cooling tech or by energy tech?
    # TODO: maybe show the percent of a highlight state compared to the neighborhood? like on hover
    #        over highlight just that state's contributions?
    # TODO: merge with water sankey
    # TODO: move mappings into 0_functions as a preload?
    # TODO: should superpeak/base really be collapsed at top of part 2? or removed?

    # IN PROGRESS
    # TODO: do we want to always show the cogeneration loop, or pull it into other electricity inputs?
    # TODO: condense the labels for the electricity techs
    # TODO: keep same node for things like transport but disagg colors for road/rail/ship etc??
    # TODO: display by tech or by cooling type on higher levels? what is most compact but still communicative?
    # TODO: DOCUMENTATION, make choices clear

    # STYLING
    # TODO: adjust labels to include value and units
    # TODO: add color legend
    # TODO: have some kind of on click persistent highlight + better tooltip
    # TODO: on select just highlight the path you've selected?
    # TODO: make hover over more useful
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

