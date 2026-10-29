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

library(arrow)    # reading parquet
library(dplyr)    # data manipulation
library(tidyr)    # reshaping data
library(ggplot2)  # plotting
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

# 2.1 - gcam_usa_energy.parquet

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
energy_df_core <- energy_df_raw %>%
  #only interested in cooler scenarios
  filter(scenario %in% c("rcp45cooler_ssp3","rcp45cooler_ssp5","rcp85cooler_ssp3","rcp85cooler_ssp5"))

# ---- Deduplicate edges across levels ----

#TODO: Why are some losses negative and some positive?
energy_edges <- energy_df_core %>%
  distinct(scenario, region, year, source, target, units, diagram, value)
# Note: X EJ of [source] flows to [target] in [scenario, region, year]

cat("Rows in core:", nrow(energy_df_core), "\n")
cat("Rows in deduplicated edges:", nrow(energy_edges), "\n")

# Trying to figure out what these negative edges are?
neg_edges <- energy_edges %>%
  filter(value < 0)
# They are ALL hydrogen to losses

# Exemption for negative values
energy_edges_no_neg <- energy_edges %>%
  filter(source != "Hydrogen" | target != "Losses")

state_year_scen_totals_nl <- energy_edges_no_neg %>%
  group_by(region, scenario, year) %>%
  summarize(
    total_flow = sum(value, na.rm = TRUE),
    n_edges    = n(),
    .groups    = "drop"
  )

# ---- State-level totals by scenario and year ----

state_year_scen_totals <- energy_edges %>%
  group_by(region, scenario, year) %>%
  summarize(
    total_flow = sum(value, na.rm = TRUE),
    n_edges    = n(),
    .groups    = "drop"
  )

print(head(state_year_scen_totals, 20))

# Scenario Total Energy Flows over time
scenario_year_totals <- state_year_scen_totals %>%
  group_by(scenario, year) %>%
  summarize(
    total_flow = sum(total_flow, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(scenario_year_totals,
       aes(x = year, y = total_flow, color = scenario)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Total energy flow over time - cooler scenarios",
    x = "Year",
    y = "Total energy flow (EJ)",
    color = "Scenario"
  ) +
  theme_minimal()

# Looking at State-Comparison of Total energy flow for a given sample year
# Earliest and latest years
years_vec <- sort(unique(energy_edges$year))
year_early <- years_vec[1]
year_mid <- years_vec[7]
year_late  <- tail(years_vec, 1)
year_ref <- years_vec[7]  # change this to look at diff years

# Example: use mid year
# (but you can check for any year, just add it above, and change downstream)
state_totals_mid <- state_year_scen_totals %>%
  filter(year == year_mid)

ggplot(state_totals_mid,
       aes(x = reorder(region, total_flow),
           y = total_flow)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ scenario, scales = "free_y") +
  labs(
    title = paste("Total energy flow by state - cooler scenarios, year", year_mid),
    x = "State (region)",
    y = "Total energy flow (EJ)"
  ) +
  theme_minimal()

#per-state trajectories for one scenario
example_scenario <- unique(energy_edges$scenario)[1]  # e.g. first

state_year_example <- state_year_scen_totals %>%
  filter(scenario == example_scenario)

ggplot(state_year_example,
       aes(x = year, y = total_flow, group = region)) +
  geom_line(alpha = 0.3) +
  labs(
    title = paste("State-level energy flow trajectories -", example_scenario),
    x = "Year",
    y = "Total energy flow (EJ)"
  ) +
  theme_minimal()

# State Rankings
state_totals_ref <- energy_edges %>%
  filter(year == year_ref) %>%
  group_by(region, scenario) %>%
  summarize(
    total_flow = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(scenario) %>%
  mutate(rank = rank(-total_flow, ties.method = "min")) %>%  # rank descending
  ungroup()
# Make it easier to see
ranks_wide <- state_totals_ref %>%
  select(region, scenario, rank) %>%
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = rank
  )
#Rankings compared to reference (bassline/fixed scenario, arbitrary)
rank_changes <- ranks_wide %>%
  mutate(
    d_45_ssp5  = rcp45cooler_ssp5  - rcp45cooler_ssp3,
    d_85_ssp3  = rcp85cooler_ssp3  - rcp45cooler_ssp3,
    d_85_ssp5  = rcp85cooler_ssp5  - rcp45cooler_ssp3
  ) %>%
  select(region, rcp45cooler_ssp3, d_45_ssp5, d_85_ssp3, d_85_ssp5)
