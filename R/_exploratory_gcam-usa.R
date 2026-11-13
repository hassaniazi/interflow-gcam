############################################################################
# 01_exploratory_gcam_usa.R

# Ultimate Purpose: Structured exploratory data analysis of results for
# IWPR Task 1.2.

# Immediate Purpose: Exploratory analysis for the gcam_usa_energy.parquet file

############################################################################

# Un-comment and run the following lines if you do NOT have the necessary
# packages installed:

# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "readr",
#   "ggplot2",
#   "arrow"
# ))

#install.packages("devtools")   # if you don't already have it

#devtools::install_github("JGCRI/jgcricolors")
#devtools::install_github("JGCRI/gcamdata")
#devtools::install_github("JGCRI/rgcam")

library(arrow)    # reading parquet
library(dplyr)    # data manipulation
library(tidyr)    # reshaping data
library(ggplot2)  # plotting
library(maps)     # maps
library(readr)    # for writing out summaries (e.g. data dictionary)

# 0. Setup ---------------------------------------------------------------------
#source("./R/0_functions.R")

# 1. Load data -----------------------------------------------------------------

#Load in input dataframe(s):

energy_df_raw <- arrow::read_parquet("R/gcam_usa_energy.parquet")
#cat("Rows:", nrow(energy_df_raw), "  Columns:", ncol(energy_df_raw), "\n")
#(^) Can run the code to see # of rows and cols (also just check the df in
# the environment tab on the right if using RStudio)



# 2. Data Exploration  ---------------------------------------------------------

# 2.1 - gcam_usa_energy.parquet prep ----

colnames(energy_df_raw)
# 11 Columns: scenario(IM3 Scenarios - 8 total), region (US-states),
# year(2020-2100), source

# Create a new data frame with valuable information about the original
energy_data_summary <- energy_df_raw %>%
  #Aggregates the raw df into single row of values
  summarize(
    #Specifies all columns, and lists the functions to be applied to each one)
    across(everything(), list(
      n_missing = function(x) sum(is.na(x)),
      n_unique  = function(x) dplyr::n_distinct(x)
      #Because 2 functions were specified, 2 summary values are created per col
      #which actually results in doubling the amount of cols from the raw df
    ),
    .names = "{.col}X.X{.fn}"   # use "X.X" as separator
    )
  ) %>%
  # Makes the data more readable
  pivot_longer(
    cols = everything(),
    names_to  = c("column", ".value"),
    names_sep = "X.X"          # split cols using "X.X" separator from earlier
  )

#Creates df for paper scenarios:
energy_df_core <- energy_df_raw #%>%
  #only interested in cooler scenarios, actually commented this out
  #filter(scenario %in% c("rcp45cooler_ssp3","rcp45cooler_ssp5","rcp85cooler_ssp3","rcp85cooler_ssp5"))

# Deduplicate edges across levels
# Levels are useful for Sankey visualization, but pretty useless when working
# with data directly, so i'm dropping the duplicated values.

energy_edges <- energy_df_core %>%
  distinct(scenario, region, year, source, target, units, diagram, value)
# Note: X EJ of [source] flows to [target] in [scenario, region, year]
# TODO: Why are some "losses"(target) negative and some positive?

cat("Rows in core:", nrow(energy_df_core), "\n")
cat("Rows in deduplicated edges:", nrow(energy_edges), "\n")

# Trying to figure out what these negative edges are?
neg_edges <- energy_edges %>%
  filter(value < 0)
# They are ALL hydrogen to losses

# Exemption for negative values
energy_edges_no_neg <- energy_edges %>%
  filter(source != "Hydrogen" | target != "Losses")

# 2.2.1 - energy data processing ----

# Inspect unique targets
targets <- sort(unique(energy_edges_no_neg$target))
targets

# What are the different targets at different levels:

#Level 1 -
energy_df_L1 <- energy_df_core %>%
  filter(level == 1)

targets_L1 <- sort(unique(energy_df_L1$target))

#Level 2 -
energy_df_L2 <- energy_df_core %>%
  filter(level == 2)

targets_L2 <- sort(unique(energy_df_L2$target))

#Level 3 -
energy_df_L3 <- energy_df_core %>%
  filter(level == 3)

targets_L3 <- sort(unique(energy_df_L3$target))


# 2.2.1.1 - Electricity Generation ----

#TODO: this actually isn't elec gen at all (it's energy inputs for elec gen)

# Let's start with the most aggregate sankey data (i.e. Level 1)
elec_gen_df_L1 <- energy_df_L1 %>%
  filter(source == "Electricity") %>%
  select(-c("source_color","target_color","level","diagram")) %>%
  group_by(scenario,region,year) %>%
  mutate(value = sum(value)) %>%
  select(-target) %>%
  distinct() %>%
  ungroup()

#Select Scenario for single-scenario visualizations
scen_select_elec <- "rcp45cooler_ssp5"
#Select year for single year visualizations:
year_select_elec <- 2025

# Plot a Single Scenario Heat Map
ggplot(elec_gen_df_L1 %>% filter(scenario == scen_select_elec),
       aes(x = year,
           y = region,
           fill = value)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis_c(option = "C",
                       #Use a nonlinear transformation (sqrt or log)
                       #to spread the lower values
                       trans = "sqrt"   # or "log10"
  ) +
  labs(
    title = paste("Electricity gen by state and year -", scen_select_elec),
    x = "Year",
    y = "State (region)",
    fill = "Value (EJ)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)
  )

# Plot a multi-scenario heat map
ggplot(elec_gen_df_L1,
       aes(x = year,
           y = region,
           fill = value)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt"   # or "log10"
  ) +
  facet_wrap(~ scenario) +     #toggle this line to facet by scenario
  labs(
    title = "Electricity gen by state, year, and scenario (sqrt scale)",
    x = "Year",
    y = "State (region)",
    fill = "Value (EJ)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Plotting USA Heatmap for scenario and year
# prep df with earlier inputs
elec_sel <- elec_gen_df_L1 %>%
  filter(scenario == scen_select_elec,
         year     == year_select_elec)
# Map region abbreviations -> state names
state_lookup <- data.frame(
  region     = state.abb,
  state_name = tolower(state.name),
  stringsAsFactors = FALSE
)
state_vals_named_elec <- elec_sel %>%
  left_join(state_lookup, by = "region")

# Check any regions that didn't match (e.g., DC, PR)
missing <- state_vals_named_elec %>%
  filter(is.na(state_name))
if (nrow(missing) > 0) {
  print(missing)
}
# Get US map data and join
us_map <- map_data("state") %>%
  rename(state_name = region)
map_df_elec <- us_map %>%
  left_join(state_vals_named_elec, by = "state_name")
# 5. Plot the map
ggplot(map_df_elec, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = value), color = "white", linewidth = 0.2) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "C",
                       trans = "sqrt"   # or "log10"
  ) +
  labs(
    title = paste("Electricity gen by state -", scen_select_elec, "year", year_select_elec),
    fill  = "Value (EJ)"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# From looking at the results, I want to see how each state has changed
# across scenarios and time in a more quantitative way
# to do this, let's start off by creating a data set for just 2 years for all
# states and all scenarios, then let's calculate a start year ranking and end
# end year ranking, then let's diff them to see the change in national relevance
# I guess I could also do this by just calculating each states share of total elec gen
# Why not both? Rankings are good for finding interesting states, and total
# share of elec gen is actually interesting info


## Data frame Comparing National Rankings

# Start by selecting the years to compare
start_year <- 2020
end_year   <- 2050

# filter L1 elec gen datafram for the 2 comparison years, and calculate ranks
elec_rank_two_years <- elec_gen_df_L1 %>%
  filter(year %in% c(start_year, end_year)) %>%
  group_by(scenario, year) %>%
  mutate(
    rank = rank(-value, ties.method = "min")  # descending: largest value gets rank 1
  ) %>%
  ungroup()

# calculate how the rank has changed from the start year to end year
rank_change <- elec_rank_two_years %>%
  group_by(scenario, region) %>%
  summarize(
    rank_start = rank[year == start_year],
    rank_end   = rank[year == end_year],
    .groups    = "drop"
  ) %>%
  mutate(rank_diff = rank_start - rank_end)

# visualization for the change "rank" for 1 scenario
scenario_sel <- "rcp45cooler_ssp3"
rank_change %>%
  filter(scenario == scenario_sel) %>%
  ggplot(aes(x = reorder(region, rank_diff),
             y = rank_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Change in state electricity rank,", scenario_sel,
                  "from", start_year, "to", end_year),
    x = "State (region)",
    y = "Rank change (start - end)"
  ) +
  theme_minimal()

# Looking across scenarios at average and variability of rank diff per region:
rank_change_summary <- rank_change %>%
  group_by(region) %>%
  summarize(
    mean_rank_diff = mean(rank_diff, na.rm = TRUE),
    sd_rank_diff   = sd(rank_diff, na.rm = TRUE),
    min_rank_diff  = min(rank_diff, na.rm = TRUE),
    max_rank_diff  = max(rank_diff, na.rm = TRUE),
    n_scenarios    = n(),
    .groups        = "drop"
  )


# consistency of direction (gainers vs losers)
rank_change_direction <- rank_change %>%
  group_by(region) %>%
  summarize(
    n_up   = sum(rank_diff > 0, na.rm = TRUE),  # # scenarios where rank improved (moved up)
    n_down = sum(rank_diff < 0, na.rm = TRUE),  # # where rank worsened
    n_zero = sum(rank_diff == 0, na.rm = TRUE),
    .groups = "drop"
  )

# Putting all the info into 1
rank_change_all <- rank_change_summary %>%
  left_join(rank_change_direction, by = "region")

# Visualizing this info:
ggplot(rank_change_all,
       aes(x = mean_rank_diff,
           y = sd_rank_diff,
           label = region)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_text(size = 3, vjust = -0.5) +
  labs(
    title = paste("State rank change in electricity, across scenarios",
                  "(from", start_year, "to", end_year, ")"),
    x = "Mean rank change (start - end, negative = moves down)",
    y = "SD of rank change across scenarios"
  ) +
  theme_minimal()



# 2.2.1.2 - Industrial Energy Consumption ----

# Let's start with the most aggregate sankey data (i.e. Level 1)
industry_df_L1 <- energy_df_L1 %>%
  filter(target == "Industry") %>%
  select(-c("source_color","target_color","level","diagram")) %>%
  group_by(scenario,region,year) %>%
  mutate(value = sum(value)) %>%
  select(-source) %>%
  distinct() %>%
  ungroup()

#Select Scenario for single-scenario visualizations
scen_select_ind <- "rcp45cooler_ssp3"
#Select year for single year visualizations:
year_select_ind <- 2050

# Plot a Single Scenario Heat Map
ggplot(industry_df_L1 %>% filter(scenario == scen_select_ind),
       aes(x = year,
           y = region,
           fill = value)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis_c(option = "C",
                       #Use a nonlinear transformation (sqrt or log)
                       #to spread the lower values
                       trans = "sqrt"   # or "log10"
  ) +
  labs(
    title = paste("Energy Inputs to Industry by state and year -", scen_select_ind),
    x = "Year",
    y = "State (region)",
    fill = "Value (EJ)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6)
  )

# Plot a multi-scenario heat map
ggplot(industry_df_L1 #%>% filter_out(region == "TX")
       ,
       aes(x = year,
           y = region,
           fill = value)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt"   # or "log10"
  ) +
  facet_wrap(~ scenario) +     #toggle this line to facet by scenario
  labs(
    title = "Energy Inputs to Industry by state, year, and scenario (sqrt scale)",
    x = "Year",
    y = "State (region)",
    fill = "Value (EJ)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Plotting USA Heatmap for scenario and year
# prep df with earlier inputs
ind_sel <- industry_df_L1 %>%
  filter(scenario == scen_select_ind,
         year     == year_select_ind)
# Map region abbreviations -> state names
state_lookup <- data.frame(
  region     = state.abb,
  state_name = tolower(state.name),
  stringsAsFactors = FALSE
)
state_vals_named_ind <- ind_sel %>%
  left_join(state_lookup, by = "region")

# Check any regions that didn't match (e.g., DC, PR)
missing <- state_vals_named_ind %>%
  filter(is.na(state_name))
if (nrow(missing) > 0) {
  print(missing)
}
# Get US map data and join
us_map <- map_data("state") %>%
  rename(state_name = region)
map_df_ind <- us_map %>%
  left_join(state_vals_named_ind, by = "state_name")
# 5. Plot the map
ggplot(map_df_ind, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = value), color = "white", linewidth = 0.2) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "C",
                       trans = "sqrt"   # or "log10"
  ) +
  labs(
    title = paste("Energy Inputs to Industry by state -", scen_select_ind, "year", year_select_ind),
    fill  = "Value (EJ)"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

## Data frame Comparing National Rankings

# Start by selecting the years to compare
start_year <- 2020
end_year   <- 2050

# filter L1 ind datafram for the 2 comparison years, and calculate ranks
ind_rank_two_years <- industry_df_L1 %>%
  filter(year %in% c(start_year, end_year)) %>%
  group_by(scenario, year) %>%
  mutate(
    rank = rank(-value, ties.method = "min")  # descending: largest value gets rank 1
  ) %>%
  ungroup()

# calculate how the rank has changed from the start year to end year
rank_change_ind <- ind_rank_two_years %>%
  group_by(scenario, region) %>%
  summarize(
    rank_start = rank[year == start_year],
    rank_end   = rank[year == end_year],
    .groups    = "drop"
  ) %>%
  mutate(rank_diff = rank_start - rank_end)

# visualization for the change "rank" for 1 scenario
scenario_sel <- "rcp45cooler_ssp3"
rank_change_ind %>%
  filter(scenario == scenario_sel) %>%
  ggplot(aes(x = reorder(region, rank_diff),
             y = rank_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Change in state industrial energy use rank,", scenario_sel,
                  "from", start_year, "to", end_year),
    x = "State (region)",
    y = "Rank change (end - start)"
  ) +
  theme_minimal()

# Looking across scenarios at average and variability of rank diff per region:
rank_change_ind_summary <- rank_change_ind %>%
  group_by(region) %>%
  summarize(
    mean_rank_diff = mean(rank_diff, na.rm = TRUE),
    sd_rank_diff   = sd(rank_diff, na.rm = TRUE),
    min_rank_diff  = min(rank_diff, na.rm = TRUE),
    max_rank_diff  = max(rank_diff, na.rm = TRUE),
    n_scenarios    = n(),
    .groups        = "drop"
  )


# consistency of direction (gainers vs losers)
rank_change_ind_direction <- rank_change_ind %>%
  group_by(region) %>%
  summarize(
    n_up   = sum(rank_diff > 0, na.rm = TRUE),  # # scenarios where rank improved (moved up)
    n_down = sum(rank_diff < 0, na.rm = TRUE),  # # where rank worsened
    n_zero = sum(rank_diff == 0, na.rm = TRUE),
    .groups = "drop"
  )

# Putting all the info into 1
rank_change_ind_all <- rank_change_ind_summary %>%
  left_join(rank_change_ind_direction, by = "region")

# Visualizing this info:
ggplot(rank_change_ind_all,
       aes(x = mean_rank_diff,
           y = sd_rank_diff,
           label = region)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_text(size = 3, vjust = -0.5) +
  labs(
    title = paste("State rank change in industrial energy use, across scenarios",
                  "(from", start_year, "to", end_year, ")"),
    x = "Mean rank change (start - end, negative = moves down)",
    y = "SD of rank change across scenarios"
  ) +
  theme_minimal()


# 2.2.1.2 - Electricity generation reliance on fossil fuel supplies ----

# Let's start with the most aggregate sankey data (i.e. Level 1)
elec_gen_df_L1 <- energy_df_L1 %>%
  filter(source == "Electricity") %>%
  select(-c("source_color","target_color","level","diagram")) %>%
  group_by(scenario,region,year) %>%
  mutate(value = sum(value)) %>%
  select(-target) %>%
  distinct() %>%
  ungroup()

#### Plot Sankeys ----
source("./R/0_functions.R")
library(plotly)
# load from pre-processed file
combo_grp <- read_parquet("gcam_usa_energy.parquet")
nodes     <- read_parquet("gcam_usa_energy_nodes.parquet")

DISPLAY_LEVEL <- 3  # TODO: remove after testing; lower value less detail
SCENARIO      <- "rcp45hotter_ssp5"
FILTER_YEARS  <- 2030
plot_grp <- combo_grp %>%
  filter(scenario %in% SCENARIO, year %in% FILTER_YEARS, level %in% c(DISPLAY_LEVEL, 0))

# Usage: single state plotting
plot_sankey(plot_grp, nodes,
            paste0("Level ", DISPLAY_LEVEL, " energy flows view"),
            reg = "WA",
            yr = FILTER_YEARS,
            scen = SCENARIO,
            animate = F)

