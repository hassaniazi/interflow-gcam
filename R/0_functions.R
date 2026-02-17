# Functions to support the Sankey diagram scripts
#
# Hassan Niazi, May 2024

# TODO: Use roxygen2 to document the functions


# load libraries ###############################################################
{
  library(tidyverse)
  library(plotly)
  library(jgcricolors)
  library(gcamdata)
  library(rgcam)
  library(ssh)
  library(DT)
  library(igraph)
  library(ggraph)
}


# directories ##################################################################
data_dir <- "data/"
queries_dir <- "queries/"
output_dir <- "output/"
img_dir <- "img/"
shiny_dir <- "shiny/"

# filters the repeated rows
repeats <- function(df) {
  df %>% group_by(across(everything())) %>%
    filter(n() > 1) %>% ungroup()
}

# helper functions ####################################################################

'%!in%' <- function(x,y)!('%in%'(x,y))

remove_source_equals_target <- function(df, source_col = "source", target_col = "target") {
  df %>% filter(!!sym(source_col) != !!sym(target_col)) # Keep rows where source != target
}

agg_enduse <- function(df, source_col, target_col) {
  df_ag <- df %>%
    group_by(scenario, region, !!sym(source_col), !!sym(target_col), year, units) %>% # dynamically group by the column name
    summarise(value = sum(value, na.rm = TRUE)) %>% # summarize 'value' with na.rm = TRUE
    ungroup() %>%
    select(scenario, region, source = !!sym(source_col), target = !!sym(target_col), year, value, units) # dynamically map column name to 'target'
  return(df_ag)
}




# clean up functions ####################################################################

# function to gather_years and filter by one year
gather_year_filt <- function(df, year_filt = 2025) {
  df %>%
    gather_years() %>%
    filter(year == year_filt)
}
# usage: elec_td_inputs_and_outputs_2050 <- gather_year_filt(elec_td_inputs_and_outputs, 2050)


# exclude PacArctic from the data
exclude_PacArctic <- function(df, column = Basin, var = "PacArctic") {
  df %>% filter(!str_detect({{column}}, {{var}}))
}

# remove _water withdrawals part of the string, so in column resource Arkansas White Red_water withdrawals would become Arkansas White Red
remove_water_withdrawals_string <- function(df, column = resource) {
  df %>% mutate({{column}} := str_remove({{column}}, "_water withdrawals"))
}

# replace everything after _irr* in the string, so in sector column water_td_irr_ArkWhtRedR_W would become water_td_irr_W
# mutate(sector = ifelse(grepl("water_td_irr", sector), "water_td_irr_W", sector)) %>%
replace_after_irr_string <- function(df, column = sector) {
  df %>% mutate({{column}} := str_replace({{column}}, "_irr.*", "_irr_W"))
}

# map water use sectors to water use categories
map_water_use_to_categories <- function(df, column = sector) {
  df %>% mutate(watcategory := case_when(
    # Irrigation water
    grepl("Corn|FiberCrop|FodderHerb|Fruits|Legumes|FodderGrass|MiscCrop|NutsSeeds|OilCrop|OtherGrain|PalmFruit|Rice|RootTuber|Soybean|Vegetables|Wheat|biomass|SugarCrop", sector) ~ "water_td_irr_W",
    # livestock water
    str_detect({{column}}, "Beef|Dairy|Pork|Poultry|SheepGoat") ~ "water_td_an_W",
    # Municipal water
    str_detect({{column}}, "municipal") ~ "water_td_dom_W",
    # Industrial water
    str_detect({{column}}, "H2|paper|industry") ~ "water_td_ind_W",
    # Electricity water
    str_detect({{column}}, "elec") ~ "water_td_elec_W",
    # Mining water
    str_detect({{column}}, "nuclear|regional|unconventional oil production") ~ "water_td_pri_W",
    TRUE ~ "other"
  ))
}

# function to tidy up water use categories
#   # if source or target has water_td_an_W water_td_dom_W water_td_ind_W water_td_irr_W water_td_pri_W water_td_elec_W, replace them with Livestock Irrigation Domestic Industrial Mining Electricity Water Use
rename_water_use_categories <- function(df, column = c("source", "target")) {
  df %>%
    mutate_at(vars({{column}}), ~ case_when(
      . == "water_td_an_W" ~ "Livestock Water Use",
      . == "water_td_dom_W" ~ "Domestic Water Use",
      . == "water_td_ind_W" ~ "Industrial Water Use",
      . == "water_td_irr_W" ~ "Irrigation Water Use",
      . == "water_td_pri_W" ~ "Mining Water Use",
      . == "water_td_elec_W" ~ "Electricity Water Use",
      TRUE ~ .
    ))
}

# function to tidy up water sources
rename_water_source <- function(df, column = c("source", "target")) {
  df %>%
    mutate_at(vars({{column}}), ~ case_when(
      . == "runoff" ~ "Surface Water",
      . == "groundwater" ~ "Groundwater",
      . == "desalination" ~ "Desalination",
      TRUE ~ .
    ))
}

# remove GLU_names strings from a column, so biomassTree_ArkWhtRedR will become biomassTree
remove_GLUnames <- function(df, filtercol = region, regions = GLU_names){
  return(df %>% mutate({{filtercol}} := str_remove({{filtercol}}, paste(regions, collapse = "|"))))
}

# change all monthly elec dom supply (e.g., electricity domestic supply_Nov_day) to one category (electricity domestic supply)
clean_monthly_elec_dom_supply <- function(df, filtercol = input){
  return(df %>% mutate({{filtercol}} := str_remove({{filtercol}}, "_.*")))
}

# remove Month_day from a column, so comm cooling Jul_day becomes comm cooling or electricity domestic supply_Nov_day becomes electricity domestic supply
remove_month_day_night_superpeak <- function(df, column_name) {
  column <- rlang::ensym(column_name)
  df <- df %>%
    mutate(
      !!column := gsub("(\\s|_)[A-Za-z]+_(day|night)$", "", !!column), # Remove month and day/night
      !!column := gsub("_superpeak", "", !!column),                   # Remove "_superpeak"
      !!column := gsub("\\s*superpeak", "", !!column)                 # Remove "superpeak" with space
    ) %>%
    mutate(!!column := trimws(!!column))                              # Remove trailing spaces
  return(df)
}

cooling_techs <- c(" \\(once through\\)", " \\(dry cooling\\)", " \\(seawater\\)", " \\(recirculating\\)", " \\(cooling pond\\)")
cooling_techs_years <- c(" \\(pre_1970\\)", " \\(1970s\\)", " \\(1980s\\)", " \\(1990s\\)", " \\(2000s\\)", " \\(2010s\\)", " \\(retire 2020\\)")

# remove cooling tech string from a column, so Gen_III (once through) becomes Gen_III
clean_cooling_tech <- function(df, column, cooling_techs, cooling_techs_years) {
  df[[column]] <- Reduce(function(x, tech) gsub(tech, "", x), cooling_techs, df[[column]])
  df[[column]] <- Reduce(function(x, tech) gsub(tech, "", x), cooling_techs_years, df[[column]])
  return(df)
}


# swap source and target columns
swap_source_target <- function(df) {
  df <- df %>% rename("source" = "target", "target" = "source")
  return(df)
}


# query functions for IM3 gcam-usa scenarios ###################################

# write IM3 GCAM-USA scenarios data as csvs. Usage: write_im3_csvs("im3scen_water")
write_im3_csvs <- function(project, write_scen_query = F, zip_csvs = T) {
  if (exists({{project}}, envir = .GlobalEnv)) {
    prj <- get({{project}}, envir = .GlobalEnv)
  } else {
    prj <- loadProject(proj = paste0(data_dir, {{project}}, ".dat"))
  }

  dir.create(paste0(data_dir, project), recursive = TRUE)
  print(paste0("Writing ", project, " data to ", paste0(data_dir, project)))

  if (write_scen_query == T) { # write each scenario, each query in a separate file
    for (scen in listScenarios(prj)) {
      for (query in listQueries(prj)) {
        write_csv(prj[[scen]][[query]], file = paste0(data_dir, project, "/", scen, "_", query, if_else(zip_csvs == T, ".csv.gz", ".csv")))
      }
    }
  } else { # all scenarios for a query in one file
    for (query in listQueries(prj)) {
      write_csv(getQuery(prj, query) , file = paste0(data_dir, project, "/", query, if_else(zip_csvs == T, ".csv.gz", ".csv")))
    }
  }
}

# query IM3 GCAM-USA scenarios. Usage: df_im3scen_water <- query_im3_scen("water") (need to create query_water.xml first)
query_im3_scen <- function(name = "", constance_node = "constance03", user = "niaz981", write_csvs = TRUE) {
  db_loc <- "/pic/projects/im3/gcamusa/gcam-usa-im3/output"
  # ssh_exec_wait(session, command = c(paste0("cd ",  db_loc), "ls -d database_*/"))
  dbFile_names <- c("database_rcp45cooler_ssp3",
                    "database_rcp45cooler_ssp5",
                    "database_rcp45hotter_ssp3",
                    "database_rcp45hotter_ssp5",
                    "database_rcp85cooler_ssp3",
                    "database_rcp85cooler_ssp5",
                    "database_rcp85hotter_ssp3",
                    "database_rcp85hotter_ssp5")

  query.fname <- paste0(data_dir, "queries_", {{name}}, ".xml")
  prj.name <- paste0(data_dir, "im3scen_", {{name}}, ".dat")

  # connect to VPN before this
  session <- ssh_connect(paste0(user, "@", constance_node))
  # constance_node <- ssh_session_info(session)$host
  ssh_exec_wait(session, command = paste0("./basex-server-helper.sh ", db_loc))

  for (dbFile in dbFile_names) {
    scenario.names_remote <- (listScenariosInDB(remoteDBConn(dbFile, "test", "test", constance_node)))$name
    conn_remote <- remoteDBConn(dbFile, "test", "test", constance_node)
    prj <- addScenario(conn_remote, prj.name, scenario.names_remote, queryFile = query.fname)
  }

  ssh_disconnect(session)

  if (write_csvs == TRUE) {
    write_im3_csvs(paste0("im3scen_", {{name}}))
  }

  return(prj)
}
# usage: df_im3scen_water <- query_im3_scen("water")

# plotting functions ###########################################################
#
# plot sankey diagram

# df_sankey <- rbind(df_waterWithdrawalsByStateSectorBasin, df_waterWithdrawalsByWaterSource, watcategory_sector_use)

plot_sankey <- function(df_sankey, title = "GCAM-USA Sankey Diagram", tidy = T,
                        scen = "rcp45cooler_ssp3", yr = 2050,
                        animate = T, animateby = year, showFut = T, reg = states) {

  # stop if the data has multiple flows
  if (any(duplicated(df_sankey))) {
    head(repeats(df_sankey))
    stop("Data has multiple flows / repeated rows. Please check the data.")
  }

  # TODO: let animate take year or scenario or c("year", "scenario") as input,
  # and change the code accordingly. Set a default to animate over years. Not
  # sure how to handle both year and scenario, perhaps by creating two diagrams.

  if (tidy) {
    df_sankey <- df_sankey %>% rename_water_use_categories() %>% rename_water_source()
  }

  # complete the data to set the full canvas
  df_sankey <- as.data.frame(df_sankey) %>%
    complete(scenario, year, nesting(source, target), fill = list(value = 0))

  # generate node label before filtering
  node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))

  # only show future
  if (showFut) {
    df_sankey <- df_sankey %>% filter(year >= 2020)
  }

  # filter to US regions
  if ("region" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>% filter(region %in% reg)
    # df_sankey <- df_sankey %>% filter(region %in% all_regions | region %in% reg)
  }

  # # always keep USA, Mexico, and Canada if they are in the region column. Filter based on US states passed on the reg argument
  # if ("region" %in% colnames(df_sankey)) {
  #   df_sankey_reg_all <- df_sankey %>% filter(region %in% c("USA", "Mexico", "Canada") | region %in% reg)
  #
  #   df_sankey_reg_state <- df_sankey_reg_all %>% filter(region %in% c("") | region %in% reg)
  #
  #   # if the target column of df_sankey contains strings in source column of df_sankey_reg, then filter to those
  #   df_sankey_reg_f <- df_sankey_reg_all %>% filter(target %in% unique(df_sankey_reg_state$source))
  #   # df_sankey_reg_f_e <- df_sankey %>% filter(source %in% unique(df_sankey_reg_f$target))
  #   df_sankey <- rbind(df_sankey_reg_f, df_sankey_reg_state) %>% unique()
  #
  # }
  #
  #
  # # complete the data to set the full canvas
  # df_sankey <- as.data.frame(df_sankey) %>%
  #   complete(scenario, year, nesting(source, target), fill = list(value = 0))
  #
  # # generate node label after filtering
  # node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))



  # filter basins in source column that are in target column
  # if ("source" %in% colnames(df_sankey) && "target" %in% colnames(df_sankey)) {
  #   df_sankey <- df_sankey %>%
  #     filter(source %in% df_sankey$target)
  # }

  # # filter region if it is specified
  # if (length(reg) > 0) {
  #   df_sankey <- df_sankey %>% filter(region %in% reg)
  # }

  # if ("region" %in% colnames(df_sankey)) {
  #   df_sankey <- df_sankey %>% filter(region == reg)
  # }

  # filter data if scenario column exists
  if ("scenario" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>% filter(scenario == scen)
  }

  if (animate == F) {
    df_sankey <- df_sankey %>% filter(year == yr)
  }

  # # Combine source and target to get unique nodes and their colors
  # nodes <- df_sankey %>%
  #   select(source, source_color) %>%
  #   rename(node = source, node_color = source_color) %>%
  #   bind_rows(df_sankey %>%
  #       select(target, target_color) %>%
  #       rename(node = target, node_color = target_color)
  #   ) %>%
  #   distinct()
  #
  # # map node colors to node labels
  # node_colors <- nodes$node_color[match(node_labels, nodes$node)]


  # plot the sankey
  p <- plot_ly(
    data = df_sankey,
    type = "sankey",
    arrangement = "snap",
    node = list(
      # group = 2,
      # pad = 15,
      # thickness = 20,
      label = node_labels,
      # color = df_sankey$source_color,
      # color = node_colors,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = match(df_sankey$source, node_labels) - 1,
      target = match(df_sankey$target, node_labels) - 1,
      value = df_sankey$value,
      year = df_sankey$year
      # color = df_sankey$flow_color
    ),
    frame = ~df_sankey$year
  ) %>%
    layout(
      title = paste0(paste(reg, collapse = ", "), " ", title, " for ", scen, " scenario", if_else(animate == T, "", paste0(" in ", yr))),
      font = list(size = 11)
    )

  if (animate == T) {
    p <- p %>% animation_opts(2000, redraw = T) %>%
    animation_slider(currentvalue = list(prefix = "Year ", font = list(color="red")))
  }

  return(p)
}
# usage: plot_sankey(ammonia_df)

# plot_sankey(rbind(df_waterWithdrawalsByStateSectorBasin, df_waterWithdrawalsByWaterSource, watcategory_sector_use), "Water Withdrawals by Use Category and Basin")


# regional strings #############################################################
grid_regions <- c(
  "California grid",
  "Alaska grid",
  "Hawaii grid",
  "Central East grid",
  "Central Northeast grid",
  "Central Northwest grid",
  "Central Southwest grid",
  "Florida grid",
  "Mid-Atlantic grid",
  "New England grid",
  "New York grid",
  "Northwest grid",
  "Southeast grid",
  "Southwest grid",
  "Texas grid"
)

grid_regions_conus <- c(
  "California grid",
  # "Alaska grid",
  # "Hawaii grid",
  "Central East grid",
  "Central Northeast grid",
  "Central Northwest grid",
  "Central Southwest grid",
  "Florida grid",
  "Mid-Atlantic grid",
  "New England grid",
  "New York grid",
  "Northwest grid",
  "Southeast grid",
  "Southwest grid",
  "Texas grid"
)

states <- c("AK", # Alaska
            "AL", # Alabama
            "AR", # Arkansas
            "AZ", # Arizona
            "CA", # California
            "CO", # Colorado
            "CT", # Connecticut
            "DC", # District of Columbia
            "DE", # Delaware
            "FL", # Florida
            "GA", # Georgia
            "HI", # Hawaii
            "IA", # Iowa
            "ID", # Idaho
            "IL", # Illinois
            "IN", # Indiana
            "KS", # Kansas
            "KY", # Kentucky
            "LA", # Louisiana
            "MA", # Massachusetts
            "MD", # Maryland
            "ME", # Maine
            "MI", # Michigan
            "MN", # Minnesota
            "MO", # Missouri
            "MS", # Mississippi
            "MT", # Montana
            "NC", # North Carolina
            "ND", # North Dakota
            "NE", # Nebraska
            "NH", # New Hampshire
            "NJ", # New Jersey
            "NM", # New Mexico
            "NV", # Nevada
            "NY", # New York
            "OH", # Ohio
            "OK", # Oklahoma
            "OR", # Oregon
            "PA", # Pennsylvania
            "RI", # Rhode Island
            "SC", # South Carolina
            "SD", # South Dakota
            "TN", # Tennessee
            "TX", # Texas
            "UT", # Utah
            "VA", # Virginia
            "VT", # Vermont
            "WA", # Washington
            "WI", # Wisconsin
            "WV", # West Virginia
            "WY"  # Wyoming
            )

states_conus <- c(# "AK", # Alaska
                  "AL", # Alabama
                  "AR", # Arkansas
                  "AZ", # Arizona
                  "CA", # California
                  "CO", # Colorado
                  "CT", # Connecticut
                  "DC", # District of Columbia
                  "DE", # Delaware
                  "FL", # Florida
                  "GA", # Georgia
                  # "HI", # Hawaii
                  "IA", # Iowa
                  "ID", # Idaho
                  "IL", # Illinois
                  "IN", # Indiana
                  "KS", # Kansas
                  "KY", # Kentucky
                  "LA", # Louisiana
                  "MA", # Massachusetts
                  "MD", # Maryland
                  "ME", # Maine
                  "MI", # Michigan
                  "MN", # Minnesota
                  "MO", # Missouri
                  "MS", # Mississippi
                  "MT", # Montana
                  "NC", # North Carolina
                  "ND", # North Dakota
                  "NE", # Nebraska
                  "NH", # New Hampshire
                  "NJ", # New Jersey
                  "NM", # New Mexico
                  "NV", # Nevada
                  "NY", # New York
                  "OH", # Ohio
                  "OK", # Oklahoma
                  "OR", # Oregon
                  "PA", # Pennsylvania
                  "RI", # Rhode Island
                  "SC", # South Carolina
                  "SD", # South Dakota
                  "TN", # Tennessee
                  "TX", # Texas
                  "UT", # Utah
                  "VA", # Virginia
                  "VT", # Vermont
                  "WA", # Washington
                  "WI", # Wisconsin
                  "WV", # West Virginia
                  "WY"  # Wyoming
                  )


basins <- c("South Atlantic Gulf",
            "Tennessee River",
            "Arkansas White Red",
            "Lower Mississippi River",
            "Lower Colorado River",
            "Mexico-Northwest Coast",
            "Upper Colorado River",
            "California River",
            "Great",
            "Pacific and Arctic Coast",
            "Pacific Northwest",
            "Missouri River",
            "Rio Grande River",
            "Mid Atlantic",
            "New England",
            "Caribbean",
            "Hawaii",
            "Upper Mississippi",
            "Ohio River",
            "Great Lakes",
            "Texas Gulf Coast",
            "Saskatchewan-Nelson",
            "Fraser")

basins_conus <- c("South Atlantic Gulf",
                  "Tennessee River",
                  "Arkansas White Red",
                  "Lower Mississippi River",
                  "Lower Colorado River",
                  "Mexico-Northwest Coast",
                  "Upper Colorado River",
                  "California River",
                  "Great",
                  # "Pacific and Arctic Coast",
                  "Pacific Northwest",
                  "Missouri River",
                  "Rio Grande River",
                  "Mid Atlantic",
                  "New England",
                  # "Caribbean", # apparently FL is also using Carribbean basin
                  # "Hawaii",
                  "Upper Mississippi",
                  "Ohio River",
                  "Great Lakes",
                  "Texas Gulf Coast",
                  "Saskatchewan-Nelson",
                  "Fraser"
                  )

basins_conus_resource <- c("South Atlantic Gulf",
                           "Tennessee River",
                           "Arkansas White Red",
                           "Lower Mississippi River",
                           "Lower Colorado River",
                           "Mexico-Northwest Coast",
                           "Upper Colorado River",
                           "California River",
                           "Great",
                           # "Pacific and Arctic Coast",
                           "Pacific Northwest",
                           "Missouri River",
                           "Rio Grande River",
                           "Mid Atlantic",
                           "New England",
                           # "Caribbean", # apparently FL is also using Carribbean basin
                           # "Hawaii",
                           "Upper Mississippi",
                           "Ohio River",
                           "Great Lakes",
                           "Texas Gulf Coast",
                           "Saskatchewan-Nelson",
                           "Fraser"
                           )

basins_resource <- c("Arkansas White Red_water withdrawals",
                     "California River_water withdrawals",
                     "Great_water withdrawals",
                     "Great Lakes_water withdrawals",
                     # "Hawaii_water withdrawals",
                     "Mexico-Northwest Coast_water withdrawals",
                     # "Caribbean_water withdrawals",
                     "Fraser_water withdrawals",
                     "Saskatchewan-Nelson_water withdrawals",
                     "Lower Colorado River_water withdrawals",
                     "Lower Mississippi River_water withdrawals",
                     "Mid Atlantic_water withdrawals",
                     "Missouri River_water withdrawals",
                     "New England_water withdrawals",
                     "Ohio River_water withdrawals",
                     "Pacific Northwest_water withdrawals",
                     # "Pacific and Arctic Coast_water withdrawals",
                     "Rio Grande River_water withdrawals",
                     "South Atlantic Gulf_water withdrawals",
                     "Tennessee River_water withdrawals",
                     "Texas Gulf Coast_water withdrawals",
                     "Upper Colorado River_water withdrawals",
                     "Upper Mississippi_water withdrawals")



all_regions <- c("USA", states, basins, grid_regions)

all_regions_conus <- c("USA", "Canada", "Mexico", "Central America and Caribbean", states_conus, basins_conus, grid_regions_conus)

# filter for USA regions
filter_USregions <- function(df, filtercol = region, regions = all_regions){
  return(df %>% filter({{filtercol}} %in% regions))
}

filter_CONUSregions <- function(df, filtercol = region, regions = all_regions_conus){
  return(df %>% filter({{filtercol}} %in% regions))
}

filter_basin_resource <- function(df, filtercol = resource, regions = basins_resource){
  return(df %>% filter({{filtercol}} %in% regions))
}

# alternatively exclude non-US regions if unsure of the regional definitions in the US
nonUSregions <- c("Africa_Eastern",
                  "Africa_Northern",
                  "Africa_Southern",
                  "Africa_Western",
                  "Australia_NZ",
                  "Brazil",
                  "Canada",
                  "Central America and Caribbean",
                  "Central Asia",
                  "China",
                  "EU-12",
                  "EU-15",
                  "Europe_Eastern",
                  "Europe_Non_EU",
                  "European Free Trade Association",
                  "India",
                  "Indonesia",
                  "Japan",
                  "Mexico",
                  "Middle East",
                  "Pakistan",
                  "Russia",
                  "South Africa",
                  "South America_Northern",
                  "South America_Southern",
                  "South Asia",
                  "South Korea",
                  "Southeast Asia",
                  "Taiwan",
                  "Argentina",
                  "Colombia")

# exclude non-US regions
exclude_nonUSregions <- function(df, filtercol = region, regions = nonUSregions){
  return(df %>% filter(!{{filtercol}} %in% regions))
}


basin_short_name <- c("ArkWhtRedR",
                      "California",
                      "Caribbean",
                      "FraserR",
                      "GreatBasin",
                      "GreatLakes",
                      "MexCstNW",
                      "MissouriR",
                      "MissppRN",
                      "MissppRS",
                      "NelsonR",
                      "OhioR",
                      "RioGrande",
                      "TennR",
                      "TexasCst",
                      "UsaColoRN",
                      "UsaColoRS",
                      "UsaCstE",
                      "UsaCstNE",
                      "UsaCstSE",
                      "UsaPacNW")

GLU_names <- c("PacArctic",
               "NelsonR",
               "FraserR",
               "MexCstNW",
               "Caribbean",
               "California",
               "MissppRN",
               "MissppRS",
               "UsaColoRN",
               "UsaColoRS",
               "GreatBasin",
               "MissouriR",
               "ArkWhtRedR",
               "TexasCst",
               "UsaCstSE",
               "GreatLakes",
               "OhioR",
               "UsaPacNW",
               "TennR",
               "RioGrande",
               "UsaCstNE",
               "UsaCstE",
               "Hawaii")






# region filter functions ###########################################################

filter_water_data_by_region <- function(df, region_filter, basin_map) {
  # Identify if entries are from resource or use table by checking column patterns
  is_resource <- unique(df$region) %in% basin_map$region

  # Basins that touch the target region
  basins_touching_region <- basin_map %>%
    filter(subregion %in% region_filter) %>%
    pull(basin) %>%
    unique()

  # Split: resource vs use rows
  df_resource <- df %>%
    filter(region %in% basin_map$region & target %in% basins_touching_region)

  df_use <- df %>%
    filter(region %in% region_filter)

  # Combine
  bind_rows(df_resource, df_use)
}



# # Exploratory analysis for units
#
# # Load data
# {
#   gcamusa_input_by_tech <- read_csv(paste0(data_dir, "inputsbytech_gcamusa.csv"))
#   gcamusa_output_by_tech <- read_csv(paste0(data_dir, "outputsbytech_gcamusa.csv"))
# }
#
# # print all units in inputs
#
#
# print(sort(unique(gcamusa_input_by_tech$Units)))
#
# # print all units in outputs
# print(sort(unique(gcamusa_output_by_tech$Units)))
#
