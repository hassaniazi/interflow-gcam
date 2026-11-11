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

# helper functions #############################################################

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

# combine the 24 heating/cooling electricity scenarios
combine_elec_period <- function(df, col = sector) {
  months  <- paste(month.abb, collapse = "|")
  pattern <- paste0("_?(", months, ")_(day|night)")
  df %>% mutate({{ col }} := gsub(pattern, "", {{ col }}))
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

# remove _water consumption part of the string, so in column resource Arkansas
# White Red_water consumption would become Arkansas White Red
remove_water_consumption_string <- function(df, column = resource) {
  df %>% mutate({{column}} := str_remove({{column}}, "_water consumption"))
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
    grepl("Corn|FiberCrop|FodderHerb|Fruits|Legumes|FodderGrass|MiscCrop|NutsSeeds|OilCrop|OtherGrain|PalmFruit|Rice|RootTuber|root_tuber|Soybean|Vegetables|Wheat|biomass|SugarCrop", sector, ignore.case = TRUE) ~ "water_td_irr_W",
    # livestock water
    grepl("Beef|Dairy|Pork|Poultry|SheepGoat", {{column}}, ignore.case = TRUE) ~ "water_td_an_W",
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
      . == "water_td_pri_W" ~ "Mining Water Use",       # TODO: separate nuclear and refining processing?
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

# rename sectors
rename_enduse_sectors <- function(df, column = sector) {
  df %>% mutate({{column}} := case_when(
    grepl("^trn_",   {{column}}) ~ "Transport",
    grepl("^comm ",  {{column}}) ~ "Commercial",
    grepl("^resid ", {{column}}) ~ "Residential",
    {{column}} == "electricity"  ~ "Electricity",
    TRUE                         ~ {{column}}
  ))
}

# transforms strings of the form 'regional root_tuber' to 'RootTuber'
rename_regional <- function(df, column = Source) {
  df %>%
    mutate({{column}} := if_else(grepl("(?i)^regional\\s+", {{column}}, perl = TRUE),
                                 gsub("_(\\w)",
                                      "\\U\\1",
                                      sub("(?i)^regional\\s+(\\w)", "\\U\\1",
                                          {{column}}, perl = TRUE),
                                      perl = TRUE),
                                 {{column}}))
}

# remove GLU_names strings from a column, so biomassTree_ArkWhtRedR will become biomassTree
remove_GLUnames <- function(df, filtercol = region, regions = GLU_names){
  return(df %>% mutate({{filtercol}} := str_remove({{filtercol}}, paste0("_(",paste(regions, collapse = "|"), ")"))))
}

# change all monthly elec dom supply (e.g., electricity domestic supply_Nov_day)
# to one category (electricity domestic supply)
clean_monthly_elec_dom_supply <- function(df, filtercol = input){
  return(df %>% mutate({{filtercol}} := str_remove({{filtercol}}, "_.*")))
}

# remove Month_day from a column, so comm cooling Jul_day becomes comm cooling
# or electricity domestic supply_Nov_day becomes electricity domestic supply
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
# TODO: make this align with bare name strat from above, currently expects str
clean_cooling_tech <- function(df, column) {
  df[[column]] <- Reduce(function(x, tech) gsub(tech, "", x), cooling_techs, df[[column]])
  return(df)
}

# remove cooling tech vintage string from a column, so
# 'coal (conv pul) (cooling pond) (1970s)' becomes 'coal (conv pul) (cooling pond)'
clean_cooling_tech_years <- function(df, column) {
  df[[column]] <- Reduce(function(x, tech) gsub(tech, "", x), cooling_techs_years, df[[column]])
  return(df)
}

# remove hi/lo fertilizer efficiency, including RFD vs IRR, and hi-efficiency
# appliances from column
remove_hi_lo <- function(df, col = technology) {
  df %>% mutate({{col}} := gsub("(_RFD|_IRR| hi-eff).*$", "", {{col}}))
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

# Construct an edge table without renaming away the original hierarchy columns.
KEEP_NAMES     <- c("scenario", "region", "year", "source", "target", "value",
                    "level", "source_position", "target_position", "source_type",
                    "target_type", "source_key", "target_key")
make_edges <- function(data, source_col, target_col, level, value_col = "value") {
  data %>%
    mutate(
      source = .data[[source_col]],
      target = .data[[target_col]],
      value = .data[[value_col]],
      level = .env$level,
      source_position = .env$source_col,
      target_position = .env$target_col
    ) %>%
    select(all_of(KEEP_NAMES))
}

classify_node <- function(node, position) {
  case_when(
    node == "Losses"                             ~ "losses",
    grepl("^Produced |^In-State ", node)         ~ "domestic_supply",
    grepl("^Net Imported|^Out-of-State ", node)  ~ "net_import",
    grepl("^Net Exported", node)                 ~ "net_export",
    node %in% c("Electricity", "Liquid Fuels")   ~ "carrier",
    node %in% PRIMARY_EN                         ~ "primary",
    node %in% c("Hydrogen", REFINING_TYPE)       ~ "transformation",
    position == "input_L0"                       ~ "domestic_supply",
    position %in% c("input_L2", "input_L3")      ~ "transformation",
    grepl("^output_L", position)                 ~ "end_use",
    TRUE                                         ~ NA_character_
  )
}

classify_sector <- function(l1_ancestor) {
  case_when(l1_ancestor == "Buildings" ~ "buildings",
            l1_ancestor == "Industry"  ~ "industry",
            l1_ancestor == "Transport" ~ "transport",
            TRUE ~ NA_character_)
}

# Build a lookup mapping every output_L1..output_L4 node to its L1 sector
# ("buildings", "industry", "transport") and hierarchical sort path so end_use
# nodes are always colored by sector and grouped contiguously on the y-axis.
end_use_sector_lookup <- function(df) {
  sector_order <- c("buildings" = 1L, "industry" = 2L, "transport" = 3L)

  l1 <- df %>%
    filter(output_L1 %in% DEMAND_SECTORS) %>%
    distinct(node = output_L1, l1 = output_L1) %>%
    mutate(path = l1)

  l2 <- df %>%
    filter(output_L1 %in% DEMAND_SECTORS, !is.na(output_L2)) %>%
    distinct(node = output_L2, l1 = output_L1, l2 = output_L2) %>%
    mutate(path = paste(l1, l2, sep = " > "))

  l3 <- df %>%
    filter(output_L1 %in% DEMAND_SECTORS, !is.na(output_L3)) %>%
    distinct(node = output_L3, l1 = output_L1, l2 = output_L2, l3 = output_L3) %>%
    mutate(path = paste(l1, l2, l3, sep = " > "))

  l4 <- df %>%
    filter(output_L1 %in% DEMAND_SECTORS, !is.na(output_L4)) %>%
    distinct(node = output_L4, l1 = output_L1, l2 = output_L2, l3 = output_L3, l4 = output_L4) %>%
    mutate(path = paste(l1, l2, l3, l4, sep = " > "))

  bind_rows(l1, l2, l3, l4) %>%
    filter(!(node %in% c(PRIMARY_EN, SECONDARY_EN, REFINING_TYPE, "Losses"))) %>%
    mutate(sector = classify_sector(l1),
           sector_rank = unname(sector_order[sector])) %>%
    arrange(sector_rank, path, node) %>%
    group_by(node) %>%
    summarize(sector      = first(na.omit(sector)),
              sector_rank = first(na.omit(sector_rank)),
              sort_path   = first(na.omit(path)),
              .groups     = "drop")
}

# Compute per-(scenario, region, year) magnitude rank for primary energy nodes.
compute_primary_rank <- function(edges_df, primary_en = PRIMARY_EN) {
  edges_df %>%
    filter(level %in% c(0, 1), source %in% primary_en) %>%
    group_by(scenario, region, year, node = source) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    complete(nesting(scenario, region, year), node = primary_en, fill = list(value = 0)) %>%
    group_by(scenario, region, year) %>%
    arrange(scenario, region, year, desc(value), node) %>%
    mutate(y_order = as.integer(row_number())) %>%
    ungroup()
}

# Compute level-specific x_col_L1..L3 and y_order_L1..L3 for every node.
assign_layout <- function(nodes_df, edges_df, end_use_lookup) {
  # Determine which nodes appear at each display level (including level 0 base flows)
  nodes_in_level <- function(lvl_vec) {
    e <- edges_df %>% filter(level %in% c(0, lvl_vec))
    unique(c(e$source, e$target))
  }
  present_l1 <- nodes_in_level(1)
  present_l2 <- nodes_in_level(2)
  present_l3 <- nodes_in_level(c(3, 4))

  # Map each node to its end_use depth (1..4) when role == "end_use"
  eu_depth <- edges_df %>%
    transmute(node = target, pos = target_position) %>%
    bind_rows(edges_df %>% transmute(node = source, pos = source_position)) %>%
    filter(grepl("^output_L[1-4]$", pos)) %>%
    mutate(depth = as.integer(sub("^output_L", "", pos))) %>%
    group_by(node) %>%
    summarize(eu_depth = min(depth), .groups = "drop")

  # Traded energy target/source for net_import, domestic_supply, and net_export
  trade_partner <- edges_df %>%
    filter(source_position == "input_L0" | grepl("^Net Exported", target)) %>%
    transmute(
      trade_node   = if_else(grepl("^Net Exported", target), target, source),
      partner_node = if_else(grepl("^Net Exported", target), source, target)
    ) %>%
    distinct(trade_node, .keep_all = TRUE)

  nd <- nodes_df %>%
    left_join(eu_depth, by = "node") %>%
    left_join(end_use_lookup %>% select(node, sector_rank, sort_path), by = "node") %>%
    left_join(trade_partner, by = c("node" = "trade_node"))

  # Helper to compute x_col for non-trade nodes at a given level (1, 2, or 3)
  base_x_col <- function(role, node, eu_depth, lvl) {
    max_col <- if (lvl == 1) 4L else if (lvl == 2) 5L else 7L
    case_when(
      role == "primary"        ~ 1L,
      role == "transformation" ~ 2L,
      role == "carrier"        ~ 3L,
      role == "losses"         ~ max_col,
      role == "end_use" & lvl == 1 ~ 4L,
      role == "end_use" & lvl == 2 ~ if_else(eu_depth == 1L, 4L, 5L),
      role == "end_use" & lvl == 3 ~ pmin(3L + coalesce(eu_depth, 1L), 7L),
      TRUE ~ NA_integer_
    )
  }

  nd <- nd %>%
    mutate(
      x_col_L1 = base_x_col(role, node, eu_depth, 1),
      x_col_L2 = base_x_col(role, node, eu_depth, 2),
      x_col_L3 = base_x_col(role, node, eu_depth, 3)
    )

  # Resolve trade nodes (net_import / domestic_supply = x_col(partner) - 1,
  # net_export = x_col(partner) + 1)
  partner_x <- nd %>%
    select(partner_node = node,
           px1 = x_col_L1, px2 = x_col_L2, px3 = x_col_L3)

  nd <- nd %>%
    left_join(partner_x, by = "partner_node") %>%
    mutate(
      x_col_L1 = case_when(
        role %in% c("net_import", "domestic_supply") ~ px1 - 1L,
        role == "net_export"                         ~ px1 + 1L,
        TRUE                                         ~ x_col_L1
      ),
      x_col_L2 = case_when(
        role %in% c("net_import", "domestic_supply") ~ px2 - 1L,
        role == "net_export"                         ~ px2 + 1L,
        TRUE                                         ~ x_col_L2
      ),
      x_col_L3 = case_when(
        role %in% c("net_import", "domestic_supply") ~ px3 - 1L,
        role == "net_export"                         ~ px3 + 1L,
        TRUE                                         ~ x_col_L3
      )
    ) %>%
    # Mask out levels where a node does not appear
    mutate(
      x_col_L1 = if_else(node %in% present_l1, x_col_L1, NA_integer_),
      x_col_L2 = if_else(node %in% present_l2, x_col_L2, NA_integer_),
      x_col_L3 = if_else(node %in% present_l3, x_col_L3, NA_integer_)
    )

  # Global magnitude per node across dataset for default y ordering of primary/transformation
  node_mag <- bind_rows(
    edges_df %>% group_by(node = source) %>% summarize(val = sum(value, na.rm = TRUE), .groups = "drop"),
    edges_df %>% group_by(node = target) %>% summarize(val = sum(value, na.rm = TRUE), .groups = "drop")
  ) %>%
    group_by(node) %>%
    summarize(mag = max(val, na.rm = TRUE), .groups = "drop")

  nd <- nd %>%
    left_join(node_mag, by = "node") %>%
    mutate(
      mag = coalesce(mag, 0),
      # Role priority within a column so Losses is always 1 (top), then carrier,
      # primary, transformation, trade, and end_use ordered by sector (buildings -> industry -> transport)
      role_band = case_when(
        role == "losses"          ~ 1L,
        role == "primary"         ~ 2L,
        role == "net_import"      ~ 3L,
        role == "domestic_supply" ~ 4L,
        role == "transformation"  ~ 5L,
        role == "carrier"         ~ 6L,
        role == "net_export"      ~ 7L,
        role == "end_use"         ~ 10L + coalesce(sector_rank, 9L),
        TRUE                      ~ 99L
      ),
      # Refine sub-group so refining technologies stay grouped together in col 2
      sub_band = case_when(
        node %in% REFINING_TYPE ~ 1L,
        node == "Hydrogen"      ~ 2L,
        TRUE                    ~ 3L
      )
    )

  # Assign contiguous 1..N y_order within each (level, x_col)
  assign_y_for_level <- function(df_in, x_col_name, y_col_name) {
    active <- df_in %>%
      filter(!is.na(.data[[x_col_name]])) %>%
      arrange(.data[[x_col_name]], role_band, sub_band, sort_path, desc(mag), node) %>%
      group_by(.data[[x_col_name]]) %>%
      mutate(!!y_col_name := row_number()) %>%
      ungroup() %>%
      select(node, all_of(y_col_name))

    df_in %>% left_join(active, by = "node")
  }

  nd %>%
    assign_y_for_level("x_col_L1", "y_order_L1") %>%
    assign_y_for_level("x_col_L2", "y_order_L2") %>%
    assign_y_for_level("x_col_L3", "y_order_L3") %>%
    select(node, role, sector, color,
           x_col_L1, x_col_L2, x_col_L3,
           y_order_L1, y_order_L2, y_order_L3)
}

# Validate all layout & coloring requirements across L1, L2, and L3.
validate_layout <- function(combo_grp, nodes, primary_ranks) {
  # 1. Edge color check: all edges to Losses have color == COLORMAP["Losses"],
  #    and all edges have non-NA color.
  stopifnot("edges missing color" = !any(is.na(combo_grp$color)))
  losses_edges <- combo_grp %>% filter(target == "Losses")
  stopifnot("edges to Losses must use Losses color" =
              all(losses_edges$color == unname(COLORMAP["Losses"])))

  # 2. End-use node sector & color check
  eu_nodes <- nodes %>% filter(role == "end_use")
  stopifnot("end_use nodes missing sector" = !any(is.na(eu_nodes$sector)),
            "end_use nodes must be colored by sector" = all(
              (eu_nodes$sector == "buildings" & eu_nodes$color == unname(COLORMAP["Buildings"])) |
              (eu_nodes$sector == "industry"  & eu_nodes$color == unname(COLORMAP["Industry"]))  |
              (eu_nodes$sector == "transport" & eu_nodes$color == unname(COLORMAP["Transport"]))
            ))

  # 3. Refining alignment: Oil Refining, Biorefining, Coal to Liquids, Gas to Liquids
  ref_nodes <- nodes %>% filter(node %in% REFINING_TYPE)
  stopifnot("refining nodes must align in x_col_L2 == 2" = all(ref_nodes$x_col_L2 == 2L),
            "refining nodes must align in x_col_L3 == 2" = all(ref_nodes$x_col_L3 == 2L))

  # 4. Primary energy alignment: all primary energy nodes in x_col == 1 across all levels
  prim_nodes <- nodes %>% filter(role == "primary")
  stopifnot("primary nodes must be in col 1 at L1" = all(prim_nodes$x_col_L1 == 1L),
            "primary nodes must be in col 1 at L2" = all(prim_nodes$x_col_L2 == 1L),
            "primary nodes must be in col 1 at L3" = all(prim_nodes$x_col_L3 == 1L))

  # 5. Per-level checks for L1, L2, L3
  allowed_feedback_sources <- c("Electricity", "Liquid Fuels", "Hydrogen")
  level_specs <- list(
    list(lvls = c(0, 1),    x = "x_col_L1", y = "y_order_L1"),
    list(lvls = c(0, 2),    x = "x_col_L2", y = "y_order_L2"),
    list(lvls = c(0, 3, 4), x = "x_col_L3", y = "y_order_L3")
  )

  for (spec in level_specs) {
    x_col <- spec$x
    y_col <- spec$y
    e_lvl <- combo_grp %>%
      filter(level %in% spec$lvls) %>%
      distinct(source, target) %>%
      left_join(nodes %>% select(source = node, src_role = role, src_x = all_of(x_col), src_y = all_of(y_col)), by = "source") %>%
      left_join(nodes %>% select(target = node, tgt_role = role, tgt_x = all_of(x_col), tgt_y = all_of(y_col)), by = "target")

    # Losses top-right check
    max_x <- max(nodes[[x_col]], na.rm = TRUE)
    loss_nd <- nodes %>% filter(node == "Losses")
    stopifnot("Losses must be in rightmost column" = loss_nd[[x_col]] == max_x,
              "Losses must have y_order == 1"      = loss_nd[[y_col]] == 1L)

    # Trade offset check
    imp_edges <- e_lvl %>% filter(src_role %in% c("net_import", "domestic_supply"))
    exp_edges <- e_lvl %>% filter(tgt_role == "net_export")
    stopifnot("import/domestic_supply must be 1 col left of traded energy" =
                all(imp_edges$src_x == imp_edges$tgt_x - 1L),
              "net_export must be 1 col right of traded energy" =
                all(exp_edges$tgt_x == exp_edges$src_x + 1L))

    # No illegal leftward edges check: tgt_x >= src_x unless src is carrier/Hydrogen feeding transformation
    bad_edges <- e_lvl %>%
      filter(tgt_x < src_x) %>%
      filter(!(src_role %in% c("carrier", "transformation") &
                 source %in% allowed_feedback_sources &
                 tgt_role == "transformation"))
    if (nrow(bad_edges) > 0) {
      stop("Illegal leftward edges at ", x_col, ": ",
           paste0(bad_edges$source, " (", bad_edges$src_x, ") -> ",
                  bad_edges$target, " (", bad_edges$tgt_x, ")", collapse = "; "))
    }

    # End-use sector contiguity check within each column
    eu_lvl <- nodes %>%
      filter(role == "end_use", !is.na(.data[[x_col]])) %>%
      arrange(.data[[x_col]], .data[[y_col]])
    for (col_val in unique(eu_lvl[[x_col]])) {
      sub_eu <- eu_lvl %>% filter(.data[[x_col]] == col_val)
      b_y <- sub_eu %>% filter(sector == "buildings") %>% pull(all_of(y_col))
      i_y <- sub_eu %>% filter(sector == "industry")  %>% pull(all_of(y_col))
      t_y <- sub_eu %>% filter(sector == "transport") %>% pull(all_of(y_col))
      if (length(b_y) && length(i_y)) stopifnot("buildings must precede industry" = max(b_y) < min(i_y))
      if (length(i_y) && length(t_y)) stopifnot("industry must precede transport" = max(i_y) < min(t_y))
      if (length(b_y) && length(t_y)) stopifnot("buildings must precede transport" = max(b_y) < min(t_y))
    }
  }

  # 6. Primary rank table check
  stopifnot("primary_ranks non-empty" = nrow(primary_ranks) > 0,
            "primary_ranks starts at 1" = min(primary_ranks$y_order) == 1L)
  invisible(TRUE)
}

COLORMAP <- c(
  # supply colors depend on primary resource
  "Nuclear"         = "#884488",
  "Uranium"         = "#884488",
  "Wind"            = "#BDBDBD",
  "Solar"           = "#ffe28a",
  "Geothermal"      = "#FA5053",
  "Biomass"         = "#117733",
  "Hydrogen"        = "#00A7A7",
  "Hydropower"      = "#0072B2",
  "Coal"            = "#362312",  # TODO: make more separate from losses color
  "Oil"             = "#8d4e28",
  "Natural Gas"     = "#D6B588",

  # secondary processes
  "Biorefining"     = "#117733",
  "Oil Refining"    = "#8d4e28",
  "Coal to Liquids" = "#362312",
  "Gas to Liquids"  = "#D6B588",

  # secondary fuels
  "Electricity"     = "#F0E442",
  "Liquid Fuels"    = "#D55E00",
  "Losses"          = "#222021",

  # demand colors depend on L1 sectors
  "Buildings"       = "#4E79A7",
  "Industry"        = "#CC6677",
  "Transport"       = "#A6D854"#,

  # cooling technologies
 # "Dry Cooling"     = "",
#  "Recirculating"   = "",


)
COLOR_LEVELS <- names(COLORMAP)

# map a node or edge key to its COLORMAP entry
# (e.g. "Net Exported Coal" -> "Coal", "Produced Uranium" -> "Nuclear")
color_key <- function(key) {
  k <- str_remove(key, "^(Net Imported|Net Exported|Produced) ")
  if_else(k == "Uranium", "Nuclear", k)
}

plot_sankey <- function(df_sankey, nodes,
                        title = "GCAM-USA Sankey Diagram",
                        scen = "rcp45cooler_ssp3", yr = 2050, reg = states,
                        animate = FALSE, showFut = TRUE) {

  df <- df_sankey %>%
    filter(scenario == scen, region %in% reg) %>%
    { if (showFut) filter(., year >= 2020) else . } %>%
    { if (!animate) filter(., year == yr) else . } %>%
    group_by(scenario, year, source, target, source_key, color) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    filter(value > 0)

  if (nrow(df) == 0L) {
    stop("No positive flows for the selected scenario, region, and year.")
  }

  if (anyNA(df$color)) {
    stop("Some edges have no colour. Check their source_key values.")
  }

  if (animate) {
    df <- df %>%
      complete(
        scenario,
        year,
        nesting(source, target, source_key, color),
        fill = list(value = 0)
      )
  }

  # restrict node table to nodes present and fail loudly on anything unmapped
  used  <- unique(c(df$source, df$target))
  missing <- setdiff(used, nodes$node)
  if (length(missing)) stop("Nodes missing from node table: ", paste(missing, collapse = ", "))
  nd <- nodes %>% filter(node %in% used)

  src_idx <- match(df$source, nd$node) - 1
  tgt_idx <- match(df$target, nd$node) - 1

  p <- plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = nd$node,
      color = nd$color,
      pad = 15, thickness = 15,
      line = list(color = "black", width = 0.5),
      customdata = paste0(nd$role, if_else(is.na(nd$sector), "", paste0(" / ", nd$sector))),
      hovertemplate = "%{label}<br>%{customdata}<extra></extra>"
    ),
    link = list(
      source = src_idx,
      target = tgt_idx,
      value  = df$value,
      color  = toRGB(df$color, alpha = 0.6) # colour by source node
    ),
    frame = if (animate) df$year else NULL
  ) %>%
    layout(
      title = paste0(paste(reg, collapse = ", "), " ", title, " – ", scen,
                     if (!animate) paste0(" (", yr, ")") else ""),
      font = list(size = 11)
    )

  if (animate) {
    p <- p %>%
      animation_opts(frame = 2000, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Year "))
  }
  p
}


# plot_sankey <- function(df_sankey, nodes, title = "GCAM-USA Sankey Diagram",
#                         tidy = T, scen = "rcp45cooler_ssp3", yr = 2050,
#                         animate = F, animateby = year, showFut = T, reg = states) {
#
#   # stop if the data has multiple flows
#   if (any(duplicated(df_sankey))) {
#     head(repeats(df_sankey))
#     stop("Data has multiple flows / repeated rows. Please check the data.")
#   }
#
#   # TODO: let animate take year or scenario or c("year", "scenario") as input,
#   # and change the code accordingly. Set a default to animate over years. Not
#   # sure how to handle both year and scenario, perhaps by creating two diagrams.
#
#   if (tidy) {
#     df_sankey <- df_sankey %>% rename_water_use_categories() %>% rename_water_source()
#   }
#
#   # complete the data to set the full canvas
#   if ("scenario" %in% colnames(df_sankey)) {
#     df_sankey <- as.data.frame(df_sankey) %>%
#       complete(scenario, year, nesting(source, target), fill = list(value = 0))
#   } else {
#     df_sankey <- as.data.frame(df_sankey) %>%
#       complete(nesting(source, target), fill = list(value = 0)) %>%
#       mutate(year = yr)
#   }
#
#   # only show future
#   if (showFut) {
#     df_sankey <- df_sankey %>% filter(year >= 2020)
#   }
#
#   # filter to US regions
#   if ("region" %in% colnames(df_sankey)) {
#     df_sankey <- df_sankey %>% filter(region %in% reg)
#   } else {
#     reg <- "USA"   # don't want to print a title with every state abbreviation
#   }
#
#   # filter data if scenario column exists
#   if ("scenario" %in% colnames(df_sankey)) {
#     df_sankey <- df_sankey %>% filter(scenario == scen)
#   }
#
#   if (animate == F) {
#     df_sankey <- df_sankey %>% filter(year == yr)
#   }
#
#   # plot the sankey
#   p <- plot_ly(
#     data = df_sankey,
#     type = "sankey",
#     arrangement = "snap",
#     node = list(
#       # group = 2,
#       # pad = 15,
#       # thickness = 20,
#       label = nodes$node,
#       color = nodes$color,
#       line = list(color = "black", width = 0.5)
#     ),
#     link = list(
#       source = match(df_sankey$source, nodes$node) - 1,
#       target = match(df_sankey$target, nodes$node) - 1,
#       value = df_sankey$value,
#       year = df_sankey$year,
#       color = toRGB(match(df_sankey$source, nodes$node), alpha = .8)
#     ),
#     frame = ~year
#   ) %>%
#     layout(
#       title = paste0(paste(reg, collapse = ", "), " ", title, " for ", scen, " scenario"), # TODO: fix when yr is a range
#                      #if_else(animate == T, "", paste0(" in ", yr))),
#       font = list(size = 11)
#     )
#
#   # TODO: the slider needs to be created first, otherwise the animation crashes.
#   if (animate == T) {
#     p <- p %>%
#       animation_opts(frame = 2000, redraw = T) %>%
#       animation_slider(currentvalue = list(prefix = "Year ",
#                                            font = list(color = "red")))
#   }
#   return(p)
# }


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

# extract state abbreviation from region, subsector, and technology columns
extract_state <- function(df, sta = states){
  abbrv_pattern <- paste0("^(", paste(sta, collapse = "|"), ")(?=\\s|$)")
  remove_pattern <- paste0("^\\s*(?:", paste(sta, collapse = "|"), ")(?=\\s|$)\\s*")
  df_state <- df %>%
    mutate(region = coalesce(str_extract(region, abbrv_pattern),
                             str_extract(subsector, abbrv_pattern),
                             str_extract(technology, abbrv_pattern),
                             region),
           subsector = str_remove(subsector, remove_pattern),
           technology = str_remove(technology, remove_pattern)) %>%
    return(df_state)
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
