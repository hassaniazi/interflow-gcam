###############################################################################
# Exploratory Analysis: GCAM-USA Energy and Water Flows
#
# Purpose:
# Explore state-level energy, water, and energy-water results across
# scenarios and time for IM3 / IWPR Task 1.2.
###############################################################################


# 0. Setup --------------------------------------------------------------------

# 0.1 Packages ----

# Un-comment and run the following lines if you do NOT have the necessary
# packages installed:
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("arrow")
#install.packages("devtools")
#devtools::install_github("JGCRI/jgcricolors")
#devtools::install_github("JGCRI/gcamdata")
#devtools::install_github("JGCRI/rgcam")

#Load in libraries
library(arrow)    # reading parquet
library(dplyr)    # data manipulation
library(tidyr)    # reshaping data
library(ggplot2)  # plotting
library(maps)     # maps
library(readr)    # for writing out summaries (e.g. data dictionary)

# 0.2 Shared settings and helper functions ----

#source("./R/0_functions.R")

# 1. Load input data ----------------------------------------------------------

#Load in Energy Parquet Files
energy_df_L0 <- arrow::read_parquet("output/gcam_usa_energy_l0.parquet")
energy_df_L1 <- arrow::read_parquet("output/gcam_usa_energy_l1.parquet")
energy_df_L2 <- arrow::read_parquet("output/gcam_usa_energy_l2.parquet")
energy_df_L3 <- arrow::read_parquet("output/gcam_usa_energy_l3.parquet")
energy_nodes <- arrow::read_parquet("output/gcam_usa_energy_nodes.parquet")
energy_primary_rank <- arrow::read_parquet("output/gcam_usa_energy_primary_rank.parquet")
#Load in Energy-Water Parquet Files
energy_water_L1 <- arrow::read_parquet("output/gcam_usa_energy_water_l1.parquet")
energy_water_L2 <- arrow::read_parquet("output/gcam_usa_energy_water_l2.parquet")
energy_water_L3 <- arrow::read_parquet("output/gcam_usa_energy_water_l3.parquet")
energy_water_nodes <- arrow::read_parquet("output/gcam_usa_energy_water_nodes.parquet")
#Load in Water Parquet Files
water_df <- arrow::read_parquet("output/gcam_usa_water.parquet")

# 2. Energy analysis ----------------------------------------------------------

#View the Sources and Targets of each .parquet file
#Level 0
sources_L0 <- sort(unique(energy_df_L0$source))
targets_L0 <- sort(unique(energy_df_L0$target))
#Level 1
sources_L1 <- sort(unique(energy_df_L1$source))
targets_L1 <- sort(unique(energy_df_L1$target))
#Level 2
sources_L2 <- sort(unique(energy_df_L2$source))
targets_L2 <- sort(unique(energy_df_L2$target))
#Level 3
sources_L3 <- sort(unique(energy_df_L3$source))
targets_L3 <- sort(unique(energy_df_L3$target))

# 2.1 Process energy data ----

# 2.1.1. Electricity Generation ----
# 2.1.1.1 Electricity generation inputs ----
# 2.1.1.2 Electricity generation outputs ----
# 2.1.1.3 Electricity generation reliance on fossil fuel supplies ----
# 2.1.2. Industrial Energy Use ----



# 2.2 Visualize energy data ----
# 2.2.1. Electricity Generation ----
# 2.2.1.1 Electricity generation inputs ----
# 2.2.1.2 Electricity generation outputs ----
# 2.2.1.3 Electricity generation reliance on fossil fuel supplies ----
# 2.2.2. Industrial Energy Use ----

# 3. Water analysis -----------------------------------------------------------

# 3.1 Process water data ----

# 3.1.1 State-Level Water Source Archetypes
##Create state-archetypes based on water profiles for 2020, 2050, 2100
water_arch <- water_df %>%
  filter(level == 1) %>%
  select(-c("ssp", "rcp","level","color","units","diagram")) %>%
  filter(source == "Surface Water" | source == "Groundwater") %>%
  group_by(scenario, region, year, source) %>%
  summarize(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )
#Pivot to wide: columns for Surface Water and Groundwater
water_arch_wide <- water_arch %>%
  pivot_wider(
    names_from  = source,
    values_from = value,
    values_fill = 0  # if one source is missing, treat as 0
  )
#compute shares of SW vs GW
water_shares <- water_arch_wide %>%
  mutate(
    total = `Surface Water` + Groundwater,
    share_sw = if_else(total > 0, `Surface Water` / total, NA_real_),
    share_gw = if_else(total > 0, Groundwater      / total, NA_real_)
  ) %>%
  mutate(
    archetype = case_when(
      total == 0                               ~ "No water",          # optional
      dplyr::near(share_sw, 1)                ~ "100%SW",
      dplyr::near(share_gw, 1)                ~ "100%GW",
      share_sw > share_gw                     ~ "%SW>%GW",
      share_gw > share_sw                     ~ "%GW>%SW",
      TRUE                                    ~ "Tie"                 # rare edge case
    )
  )
# regions where archetype is NOT constant across all scenarios & years
regions_varying <- water_arch_types %>%
  group_by(region) %>%
  summarize(
    n_arch = n_distinct(archetype[!is.na(archetype)]),
    .groups = "drop"
  ) %>%
  filter(n_arch > 1) %>%
  pull(region)


# 3.2 Visualize water data ----

# 3.2.1. Plots for State-Level Water Source Archetypes
years_sel <- c(2020, 2050, 2100)

# all water archetypes
water_arch_types <- water_shares %>%
  filter(year %in% years_sel)
ggplot(water_arch_types,
       aes(x = factor(year),
           y = region,
           fill = archetype)) +
  geom_tile(color = "grey80") +
  facet_wrap(~ scenario) +
  scale_fill_manual(
    values = c(
      "100%SW"  = "#1f78b4",
      "%SW>%GW" = "#a6cee3",
      "%GW>%SW" = "#cab2d6",
      "100%GW"  = "#6a3d9a",
      "No water" = "grey90",
      "Tie"     = "orange"
    )
  ) +
  labs(
    title = "Water source archetypes by state, scenario, and year",
    x = "Year",
    y = "State (region)",
    fill = "Archetype"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)
  )
# varying water archetypes
# creating a refined dataset for changing regions
water_arch_types_varying <- water_arch_types %>%
  filter(region %in% regions_varying)
ggplot(water_arch_types_varying,
       aes(x = factor(year),
           y = region,
           fill = archetype)) +
  geom_tile(color = "grey80") +
  facet_wrap(~ scenario) +
  scale_fill_manual(
    values = c(
      "100%SW"  = "#1f78b4",
      "%SW>%GW" = "#a6cee3",
      "%GW>%SW" = "#cab2d6",
      "100%GW"  = "#6a3d9a",
      "No water" = "grey90",
      "Tie"     = "orange"
    )
  ) +
  labs(
    title = "Water source archetypes (only states that change) by state, scenario, and year",
    x = "Year",
    y = "State (region)",
    fill = "Archetype"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)
  )
#Custom Grouping of Changing Water Source Archetypes
preferred_order <- c(
  "FL", "OK", "TX", "SD", "ND", "NE", "KS", "MT", "MN", "IA",
  "CA", "MS", "LA", "AR", "AZ"
)
all_regions <- sort(unique(water_arch_types_varying$region))
region_levels <- c(
  preferred_order,
  setdiff(all_regions, preferred_order)  # all other states, in alpha order
)
water_arch_types_varying_ordered <- water_arch_types_varying %>%
  mutate(
    region = factor(region, levels = region_levels)
  )

ggplot(water_arch_types_varying_ordered,
       aes(x = factor(year),
           y = region,
           fill = archetype)) +
  geom_tile(color = "grey80") +
  facet_wrap(~ scenario) +
  scale_fill_manual(
    values = c(
      "100%SW"  = "#1f78b4",  # blue
      "%SW>%GW" = "#a6cee3",  # light blue
      "%GW>%SW" = "#cab2d6",  # light purple
      "100%GW"  = "#6a3d9a",  # dark purple
      "No water" = "grey90",
      "Tie"     = "orange"
    )
  ) +
  labs(
    title = "Water source archetypes (only states that change), ordered by custom groups",
    x = "Year",
    y = "State (region)",
    fill = "Archetype"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)
  )
# 4. Energy-water analysis ----------------------------------------------------

# 4.1 Process energy-water data ----

# 4.2 Visualize energy-water data ----


# 5. Previous analysis / reference code ---------------------------------------

# Figures and exploratory code from the earlier analysis.




# 2. Data Exploration  ---------------------------------------------------------

###EVERYTHING BELOW THIS LINE USED TO PREPARE FIGURES IN 11.9.26 UPDATE PPTX###
#_____________________________________________________________________________#
#energy_df_raw <- arrow::read_parquet("output/gcam_usa_energy.parquet")

# # 2.1 - gcam_usa_energy.parquet prep ----
#
# #cat("Rows:", nrow(energy_df_raw), "  Columns:", ncol(energy_df_raw), "\n")
# #(^) Can run the code to see # of rows and cols (also just check the df in
# # the environment tab on the right if using RStudio)
#
# colnames(energy_df_raw)
# # 11 Columns: scenario(IM3 Scenarios - 8 total), region (US-states),
# # year(2020-2100), source
#
# # Create a new data frame with valuable information about the original
# energy_data_summary <- energy_df_raw %>%
#   #Aggregates the raw df into single row of values
#   summarize(
#     #Specifies all columns, and lists the functions to be applied to each one)
#     across(everything(), list(
#       n_missing = function(x) sum(is.na(x)),
#       n_unique  = function(x) dplyr::n_distinct(x)
#       #Because 2 functions were specified, 2 summary values are created per col
#       #which actually results in doubling the amount of cols from the raw df
#     ),
#     .names = "{.col}X.X{.fn}"   # use "X.X" as separator
#     )
#   ) %>%
#   # Makes the data more readable
#   pivot_longer(
#     cols = everything(),
#     names_to  = c("column", ".value"),
#     names_sep = "X.X"          # split cols using "X.X" separator from earlier
#   )
#
# #Creates df for paper scenarios:
# energy_df_core <- energy_df_raw #%>%
#   #only interested in cooler scenarios, actually commented this out
#   #filter(scenario %in% c("rcp45cooler_ssp3","rcp45cooler_ssp5","rcp85cooler_ssp3","rcp85cooler_ssp5"))
#
# # Deduplicate edges across levels
# # Levels are useful for Sankey visualization, but pretty useless when working
# # with data directly, so i'm dropping the duplicated values.
#
# energy_edges <- energy_df_core %>%
#   distinct(scenario, region, year, source, target, units, diagram, value)
# # Note: X EJ of [source] flows to [target] in [scenario, region, year]
# # TODO: Why are some "losses"(target) negative and some positive?
#
# cat("Rows in core:", nrow(energy_df_core), "\n")
# cat("Rows in deduplicated edges:", nrow(energy_edges), "\n")
#
# # Trying to figure out what these negative edges are?
# neg_edges <- energy_edges %>%
#   filter(value < 0)
# # They are ALL hydrogen to losses
#
# # Exemption for negative values
# energy_edges_no_neg <- energy_edges %>%
#   filter(source != "Hydrogen" | target != "Losses")
#
# # 2.2.1 - energy data processing ----
#
# # Inspect unique targets
# targets <- sort(unique(energy_edges_no_neg$target))
# targets
#
# # What are the different targets at different levels:
#
# #Level 1 -
# energy_df_L1 <- energy_df_core %>%
#   filter(level == 1)
#
# targets_L1 <- sort(unique(energy_df_L1$target))
#
# #Level 2 -
# energy_df_L2 <- energy_df_core %>%
#   filter(level == 2)
#
# targets_L2 <- sort(unique(energy_df_L2$target))
#
# #Level 3 -
# energy_df_L3 <- energy_df_core %>%
#   filter(level == 3)
#
# targets_L3 <- sort(unique(energy_df_L3$target))
#
#
# # 2.2.1.1 - Electricity Generation ----
#
# #TODO: this actually isn't elec gen at all (it's energy inputs for elec gen)
#
# # Let's start with the most aggregate sankey data (i.e. Level 1)
# elec_gen_df_L1 <- energy_df_L1 %>%
#   filter(source == "Electricity") %>%
#   select(-c("source_color","target_color","level","diagram")) %>%
#   group_by(scenario,region,year) %>%
#   mutate(value = sum(value)) %>%
#   select(-target) %>%
#   distinct() %>%
#   ungroup()
#
# #Select Scenario for single-scenario visualizations
# scen_select_elec <- "rcp45cooler_ssp5"
# #Select year for single year visualizations:
# year_select_elec <- 2025
#
# # Plot a Single Scenario Heat Map
# ggplot(elec_gen_df_L1 %>% filter(scenario == scen_select_elec),
#        aes(x = year,
#            y = region,
#            fill = value)) +
#   geom_tile(color = "grey80") +
#   scale_fill_viridis_c(option = "C",
#                        #Use a nonlinear transformation (sqrt or log)
#                        #to spread the lower values
#                        trans = "sqrt"   # or "log10"
#   ) +
#   labs(
#     title = paste("Electricity gen by state and year -", scen_select_elec),
#     x = "Year",
#     y = "State (region)",
#     fill = "Value (EJ)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 6)
#   )
#
# # Plot a multi-scenario heat map
# ggplot(elec_gen_df_L1,
#        aes(x = year,
#            y = region,
#            fill = value)) +
#   geom_tile(color = "grey80") +
#   scale_fill_viridis_c(
#     option = "C",
#     trans = "sqrt"   # or "log10"
#   ) +
#   facet_wrap(~ scenario) +     #toggle this line to facet by scenario
#   labs(
#     title = "Electricity gen by state, year, and scenario (sqrt scale)",
#     x = "Year",
#     y = "State (region)",
#     fill = "Value (EJ)"
#   ) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 6))
#
# # Plotting USA Heatmap for scenario and year
# # prep df with earlier inputs
# elec_sel <- elec_gen_df_L1 %>%
#   filter(scenario == scen_select_elec,
#          year     == year_select_elec)
# # Map region abbreviations -> state names
# state_lookup <- data.frame(
#   region     = state.abb,
#   state_name = tolower(state.name),
#   stringsAsFactors = FALSE
# )
# state_vals_named_elec <- elec_sel %>%
#   left_join(state_lookup, by = "region")
#
# # Check any regions that didn't match (e.g., DC, PR)
# missing <- state_vals_named_elec %>%
#   filter(is.na(state_name))
# if (nrow(missing) > 0) {
#   print(missing)
# }
# # Get US map data and join
# us_map <- map_data("state") %>%
#   rename(state_name = region)
# map_df_elec <- us_map %>%
#   left_join(state_vals_named_elec, by = "state_name")
# # 5. Plot the map
# ggplot(map_df_elec, aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = value), color = "white", linewidth = 0.2) +
#   coord_fixed(1.3) +
#   scale_fill_viridis_c(option = "C",
#                        trans = "sqrt"   # or "log10"
#   ) +
#   labs(
#     title = paste("Electricity gen by state -", scen_select_elec, "year", year_select_elec),
#     fill  = "Value (EJ)"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   )
#
# # From looking at the results, I want to see how each state has changed
# # across scenarios and time in a more quantitative way
# # to do this, let's start off by creating a data set for just 2 years for all
# # states and all scenarios, then let's calculate a start year ranking and end
# # end year ranking, then let's diff them to see the change in national relevance
# # I guess I could also do this by just calculating each states share of total elec gen
# # Why not both? Rankings are good for finding interesting states, and total
# # share of elec gen is actually interesting info
#
#
# ## Data frame Comparing National Rankings
#
# # Start by selecting the years to compare
# start_year <- 2020
# end_year   <- 2050
#
# # filter L1 elec gen datafram for the 2 comparison years, and calculate ranks
# elec_rank_two_years <- elec_gen_df_L1 %>%
#   filter(year %in% c(start_year, end_year)) %>%
#   group_by(scenario, year) %>%
#   mutate(
#     rank = rank(-value, ties.method = "min")  # descending: largest value gets rank 1
#   ) %>%
#   ungroup()
#
# # calculate how the rank has changed from the start year to end year
# rank_change <- elec_rank_two_years %>%
#   group_by(scenario, region) %>%
#   summarize(
#     rank_start = rank[year == start_year],
#     rank_end   = rank[year == end_year],
#     .groups    = "drop"
#   ) %>%
#   mutate(rank_diff = rank_start - rank_end)
#
# # visualization for the change "rank" for 1 scenario
# scenario_sel <- "rcp45cooler_ssp3"
# rank_change %>%
#   filter(scenario == scenario_sel) %>%
#   ggplot(aes(x = reorder(region, rank_diff),
#              y = rank_diff)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     title = paste("Change in state electricity rank,", scenario_sel,
#                   "from", start_year, "to", end_year),
#     x = "State (region)",
#     y = "Rank change (start - end)"
#   ) +
#   theme_minimal()
#
# # Looking across scenarios at average and variability of rank diff per region:
# rank_change_summary <- rank_change %>%
#   group_by(region) %>%
#   summarize(
#     mean_rank_diff = mean(rank_diff, na.rm = TRUE),
#     sd_rank_diff   = sd(rank_diff, na.rm = TRUE),
#     min_rank_diff  = min(rank_diff, na.rm = TRUE),
#     max_rank_diff  = max(rank_diff, na.rm = TRUE),
#     n_scenarios    = n(),
#     .groups        = "drop"
#   )
#
#
# # consistency of direction (gainers vs losers)
# rank_change_direction <- rank_change %>%
#   group_by(region) %>%
#   summarize(
#     n_up   = sum(rank_diff > 0, na.rm = TRUE),  # # scenarios where rank improved (moved up)
#     n_down = sum(rank_diff < 0, na.rm = TRUE),  # # where rank worsened
#     n_zero = sum(rank_diff == 0, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Putting all the info into 1
# rank_change_all <- rank_change_summary %>%
#   left_join(rank_change_direction, by = "region")
#
# # Visualizing this info:
# ggplot(rank_change_all,
#        aes(x = mean_rank_diff,
#            y = sd_rank_diff,
#            label = region)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_text(size = 3, vjust = -0.5) +
#   labs(
#     title = paste("State rank change in electricity, across scenarios",
#                   "(from", start_year, "to", end_year, ")"),
#     x = "Mean rank change (start - end, negative = moves down)",
#     y = "SD of rank change across scenarios"
#   ) +
#   theme_minimal()
#
#
#
# # 2.2.1.2 - Industrial Energy Consumption ----
#
# # Let's start with the most aggregate sankey data (i.e. Level 1)
# industry_df_L1 <- energy_df_L1 %>%
#   filter(target == "Industry") %>%
#   select(-c("source_color","target_color","level","diagram")) %>%
#   group_by(scenario,region,year) %>%
#   mutate(value = sum(value)) %>%
#   select(-source) %>%
#   distinct() %>%
#   ungroup()
#
# #Select Scenario for single-scenario visualizations
# scen_select_ind <- "rcp45cooler_ssp3"
# #Select year for single year visualizations:
# year_select_ind <- 2050
#
# # Plot a Single Scenario Heat Map
# ggplot(industry_df_L1 %>% filter(scenario == scen_select_ind),
#        aes(x = year,
#            y = region,
#            fill = value)) +
#   geom_tile(color = "grey80") +
#   scale_fill_viridis_c(option = "C",
#                        #Use a nonlinear transformation (sqrt or log)
#                        #to spread the lower values
#                        trans = "sqrt"   # or "log10"
#   ) +
#   labs(
#     title = paste("Energy Inputs to Industry by state and year -", scen_select_ind),
#     x = "Year",
#     y = "State (region)",
#     fill = "Value (EJ)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 6)
#   )
#
# # Plot a multi-scenario heat map
# ggplot(industry_df_L1 #%>% filter_out(region == "TX")
#        ,
#        aes(x = year,
#            y = region,
#            fill = value)) +
#   geom_tile(color = "grey80") +
#   scale_fill_viridis_c(
#     option = "C",
#     trans = "sqrt"   # or "log10"
#   ) +
#   facet_wrap(~ scenario) +     #toggle this line to facet by scenario
#   labs(
#     title = "Energy Inputs to Industry by state, year, and scenario (sqrt scale)",
#     x = "Year",
#     y = "State (region)",
#     fill = "Value (EJ)"
#   ) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 6))
#
# # Plotting USA Heatmap for scenario and year
# # prep df with earlier inputs
# ind_sel <- industry_df_L1 %>%
#   filter(scenario == scen_select_ind,
#          year     == year_select_ind)
# # Map region abbreviations -> state names
# state_lookup <- data.frame(
#   region     = state.abb,
#   state_name = tolower(state.name),
#   stringsAsFactors = FALSE
# )
# state_vals_named_ind <- ind_sel %>%
#   left_join(state_lookup, by = "region")
#
# # Check any regions that didn't match (e.g., DC, PR)
# missing <- state_vals_named_ind %>%
#   filter(is.na(state_name))
# if (nrow(missing) > 0) {
#   print(missing)
# }
# # Get US map data and join
# us_map <- map_data("state") %>%
#   rename(state_name = region)
# map_df_ind <- us_map %>%
#   left_join(state_vals_named_ind, by = "state_name")
# # 5. Plot the map
# ggplot(map_df_ind, aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = value), color = "white", linewidth = 0.2) +
#   coord_fixed(1.3) +
#   scale_fill_viridis_c(option = "C",
#                        trans = "sqrt"   # or "log10"
#   ) +
#   labs(
#     title = paste("Energy Inputs to Industry by state -", scen_select_ind, "year", year_select_ind),
#     fill  = "Value (EJ)"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   )
#
# ## Data frame Comparing National Rankings
#
# # Start by selecting the years to compare
# start_year <- 2020
# end_year   <- 2050
#
# # filter L1 ind datafram for the 2 comparison years, and calculate ranks
# ind_rank_two_years <- industry_df_L1 %>%
#   filter(year %in% c(start_year, end_year)) %>%
#   group_by(scenario, year) %>%
#   mutate(
#     rank = rank(-value, ties.method = "min")  # descending: largest value gets rank 1
#   ) %>%
#   ungroup()
#
# # calculate how the rank has changed from the start year to end year
# rank_change_ind <- ind_rank_two_years %>%
#   group_by(scenario, region) %>%
#   summarize(
#     rank_start = rank[year == start_year],
#     rank_end   = rank[year == end_year],
#     .groups    = "drop"
#   ) %>%
#   mutate(rank_diff = rank_start - rank_end)
#
# # visualization for the change "rank" for 1 scenario
# scenario_sel <- "rcp45cooler_ssp3"
# rank_change_ind %>%
#   filter(scenario == scenario_sel) %>%
#   ggplot(aes(x = reorder(region, rank_diff),
#              y = rank_diff)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     title = paste("Change in state industrial energy use rank,", scenario_sel,
#                   "from", start_year, "to", end_year),
#     x = "State (region)",
#     y = "Rank change (start - end)"
#   ) +
#   theme_minimal()
#
# # Looking across scenarios at average and variability of rank diff per region:
# rank_change_ind_summary <- rank_change_ind %>%
#   group_by(region) %>%
#   summarize(
#     mean_rank_diff = mean(rank_diff, na.rm = TRUE),
#     sd_rank_diff   = sd(rank_diff, na.rm = TRUE),
#     min_rank_diff  = min(rank_diff, na.rm = TRUE),
#     max_rank_diff  = max(rank_diff, na.rm = TRUE),
#     n_scenarios    = n(),
#     .groups        = "drop"
#   )
#
#
# # consistency of direction (gainers vs losers)
# rank_change_ind_direction <- rank_change_ind %>%
#   group_by(region) %>%
#   summarize(
#     n_up   = sum(rank_diff > 0, na.rm = TRUE),  # # scenarios where rank improved (moved up)
#     n_down = sum(rank_diff < 0, na.rm = TRUE),  # # where rank worsened
#     n_zero = sum(rank_diff == 0, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Putting all the info into 1
# rank_change_ind_all <- rank_change_ind_summary %>%
#   left_join(rank_change_ind_direction, by = "region")
#
# # Visualizing this info:
# ggplot(rank_change_ind_all,
#        aes(x = mean_rank_diff,
#            y = sd_rank_diff,
#            label = region)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_text(size = 3, vjust = -0.5) +
#   labs(
#     title = paste("State rank change in industrial energy use, across scenarios",
#                   "(from", start_year, "to", end_year, ")"),
#     x = "Mean rank change (start - end, negative = moves down)",
#     y = "SD of rank change across scenarios"
#   ) +
#   theme_minimal()
#
#
# # 2.2.1.2 - Electricity generation reliance on fossil fuel supplies ----
#
# #define fossil fuels
# # All inputs to electricity = "Gen III" "Hydropower" "Natural Gas CC" "Onshore"
# #"PV" "Offshore" "Biomass IGCC" "Biomass Conv" "Coal IGCC CCS" "Gas CT"
# #"Coal Conv Pul" "Gen II LWR" "Natural Gas  CC CCS" "Coal Conv Pul CCS" "CSP"
# # "Geothermal" "Coal IGCC" "Liquid Fuels CC" "Steam"
#
# FOSSIL_FUELS <- c("Natural Gas CC",
#                   "Natural Gas CC CCS",
#                   "Gas CT",
#                   "Coal Conv Pul",
#                   "Coal Conv Pul CCS",
#                   "Coal IGCC",
#                   "Coal IGCC CCS",
#                   "Liquid Fuels CC")
#
# # Let's make a fossil fuel elec gen df(i.e. Level 1)
# FF4_elec_df_L1 <- energy_df_L1 %>%
#   filter(source %in% FOSSIL_FUELS) %>%
#   filter(target == "Electricity") %>%
#   select(-c("source_color","target_color","level","diagram")) %>%
#   group_by(scenario,region,year) %>%
#   mutate(value = sum(value)) %>%
#   select(-c("source", "target")) %>%
#   distinct() %>%
#   ungroup() %>%
#   rename(FF4_elec_value = value)
# # Then make a total energy for elec gen df (i.e. Level 1)
# NRG4_elec_gen_df_L1 <- energy_df_L1 %>%
#   filter(target == "Electricity") %>%
#   select(-c("source_color","target_color","level","diagram")) %>%
#   group_by(scenario,region,year) %>%
#   mutate(value = sum(value)) %>%
#   select(-source) %>%
#   distinct() %>%
#   ungroup() %>%
#   rename(energy4_elec_total_value = value) %>%
#   select(-target)
# # Let's calculate Elec Gen reliance on FF now (combine the files)
# elec_ff_reliance_df <- FF4_elec_df_L1 %>%
#   left_join(NRG4_elec_gen_df_L1) %>%
#   mutate(elec_ff_reliance = FF4_elec_value / energy4_elec_total_value)
# #our FF representation wasn't right, we were getting values >1 for reliance
# #this is because we were using elec gen (which doesn't include losses)
# #since we are just looking at energy inputs for reliance, we made a new df
# #for energy inputs to elec (elec as target)
#
# #Some scenarios where 100% of elec gen in DC is fossil fuel, but no higher
# ##Plotting Heatmap:
# # Plotting USA Heatmap for scenario and year
# # Start by selecting the scenario and year
# scen_select_FF <- "rcp85cooler_ssp5"
# year_select_FF   <- 2050
# # prep df with earlier inputs
# FF_reliance_sel <- elec_ff_reliance_df %>%
#   filter(scenario == scen_select_FF,
#          year     == year_select_FF) %>%
#   select(-c("FF4_elec_value","energy4_elec_total_value")) %>%
#   rename(value = "elec_ff_reliance")
# # Map region abbreviations -> state names
# state_vals_named_FF_reliance <- FF_reliance_sel %>%
#   left_join(state_lookup, by = "region") #state look up defined up above
#
# # Check any regions that didn't match (e.g., DC, PR)
# missing <- state_vals_named_FF_reliance %>%
#   filter(is.na(state_name))
# if (nrow(missing) > 0) {
#   print(missing)
# }
# # Get US map data and join
# map_df_FF_reliance <- us_map %>% #us_map defined earlier (not specific)
#   left_join(state_vals_named_FF_reliance, by = "state_name")
# # 5. Plot the map
# ggplot(map_df_FF_reliance, aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = value), color = "white", linewidth = 0.2) +
#   coord_fixed(1.3) +
#   scale_fill_viridis_c(option = "C",
#                        limits = c(0, 1)
#   ) +
#   labs(
#     title = paste("Electricity Generation Reliance on FF by state -", scen_select_FF, "year", year_select_FF),
#     fill  = "Value (EJ)"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   )
#
# ## Data frame Comparing National Rankings
# # Start by selecting the years to compare
# start_year <- 2020
# end_year   <- 2050
# # filter fossil fuel reliance for elec gen for the 2 comparison years, and calculate ranks
# elec_ff_reliance_rank_two_years <- elec_ff_reliance_df %>%
#   select(-c("FF4_elec_value","energy4_elec_total_value")) %>%
#   rename(value = "elec_ff_reliance")%>%
#   filter(year %in% c(start_year, end_year)) %>%
#   group_by(scenario, year) %>%
#   mutate(
#     rank = rank(-value, ties.method = "min")  # descending: largest value gets rank 1
#   ) %>%
#   ungroup()
#
# # calculate how the rank has changed from the start year to end year
# rank_change_elec_ff_reliance <- elec_ff_reliance_rank_two_years %>%
#   group_by(scenario, region) %>%
#   summarize(
#     rank_start = rank[year == start_year],
#     rank_end   = rank[year == end_year],
#     .groups    = "drop"
#   ) %>%
#   mutate(rank_diff = rank_start - rank_end)
#
# # visualization for the change "rank" for 1 scenario
# scenario_sel <- "rcp45cooler_ssp3"
# rank_change_elec_ff_reliance %>%
#   filter(scenario == scenario_sel) %>%
#   ggplot(aes(x = reorder(region, rank_diff),
#              y = rank_diff)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     title = paste("Change in Electricity Generation Reliance on FF rank,", scenario_sel,
#                   "from", start_year, "to", end_year),
#     x = "State (region)",
#     y = "Rank change (start - end)"
#   ) +
#   theme_minimal()
#
# # Looking across scenarios at average and variability of rank diff per region:
# rank_change_elec_ff_reliance_summary <- rank_change_elec_ff_reliance %>%
#   group_by(region) %>%
#   summarize(
#     mean_rank_diff = mean(rank_diff, na.rm = TRUE),
#     sd_rank_diff   = sd(rank_diff, na.rm = TRUE),
#     min_rank_diff  = min(rank_diff, na.rm = TRUE),
#     max_rank_diff  = max(rank_diff, na.rm = TRUE),
#     n_scenarios    = n(),
#     .groups        = "drop"
#   )
#
#
# # consistency of direction (gainers vs losers)
# rank_change_elec_ff_reliance_direction <- rank_change_elec_ff_reliance %>%
#   group_by(region) %>%
#   summarize(
#     n_up   = sum(rank_diff > 0, na.rm = TRUE),  # # scenarios where rank improved (moved up)
#     n_down = sum(rank_diff < 0, na.rm = TRUE),  # # where rank worsened
#     n_zero = sum(rank_diff == 0, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Putting all the info into 1
# rank_change_elec_ff_reliance_all <- rank_change_elec_ff_reliance_summary %>%
#   left_join(rank_change_elec_ff_reliance_direction, by = "region")
#
# # Visualizing this info:
# ggplot(rank_change_elec_ff_reliance_all,
#        aes(x = mean_rank_diff,
#            y = sd_rank_diff,
#            label = region)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
#   geom_text(size = 3, vjust = -0.5) +
#   labs(
#     title = paste("State rank change in Electricity Generation reliance on FF, across scenarios",
#                   "(from", start_year, "to", end_year, ")"),
#     x = "Mean rank change (start - end, negative = moves down)",
#     y = "SD of rank change across scenarios"
#   ) +
#   theme_minimal()
#
#
# #### Plot Sankeys ----
# source("./R/0_functions.R")
# library(plotly)
# # load from pre-processed file
# combo_grp <- read_parquet("gcam_usa_energy.parquet")
# nodes     <- read_parquet("gcam_usa_energy_nodes.parquet")
#
# DISPLAY_LEVEL <- 3  # TODO: remove after testing; lower value less detail
# SCENARIO      <- "rcp45hotter_ssp5"
# FILTER_YEARS  <- 2030
# plot_grp <- combo_grp %>%
#   filter(scenario %in% SCENARIO, year %in% FILTER_YEARS, level %in% c(DISPLAY_LEVEL, 0))
#
# # Usage: single state plotting
# plot_sankey(plot_grp, nodes,
#             paste0("Level ", DISPLAY_LEVEL, " energy flows view"),
#             reg = "WA",
#             yr = FILTER_YEARS,
#             scen = SCENARIO,
#             animate = F)
#
