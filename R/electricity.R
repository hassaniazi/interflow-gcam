# sankey for electricity from GCAM
#
# for WPTO IWPR
#
# Team: Kendall Mongrid, Jennie Rice
# Hassan Niazi, Feb 2024

# the sankey diagram should show the flow of energy and water from the inputs to
# electricity generation all the way to the end use sectors


# load libraries
{
  library(tidyverse)
  library(plotly)
  library(jgcricolors)
  library(gcamdata)
}
# function to gather_years and filter by one year
gather_year_filt <- function(df, year_filt = 2025) {
  df %>%
    gather_years() %>%
    filter(year == year_filt)
}
# usage: elec_td_inputs_and_outputs_2050 <- gather_year_filt(elec_td_inputs_and_outputs, 2050)


# let's start with a simple example for electricity only

{ # read in the data ----------------------------------------------
  data_dir <- "data/"
  # inputs to electricity generation
  elec_energy_input_by_elec_gen_tech <- read_csv(paste0(data_dir, "elec_energy_input_by_elec_gen_tech.csv"))
  elec_water_withdrawals_by_gen_tech <- read_csv(paste0(data_dir, "elec_water_withdrawals_by_gen_tech.csv"))

  # electricity generation by technology
  elec_gen_by_gen_tech <- read_csv(paste0(data_dir, "elec_gen_by_gen_tech.csv"))

  # inputs to the end use sectors
  elec_td_inputs_and_outputs <- read_csv(paste0(data_dir, "elec_td_inputs_and_outputs.csv"))

  elec_consumption_by_demand_sector <- read_csv(paste0(data_dir, "elec_consumption_by_demand_sector.csv"))
}


# define sources and targets --------------------------------------------
# start with the energy inputs to electricity generation
elec_source_target <- rbind(
  # water withdrawals by electricity generation technology
  elec_water_withdrawals_by_gen_tech %>% gather_year_filt() %>%
    mutate(value = value * 1e-1) %>% # change to x10 km3 for better visuals
    rename(source = input, target = technology) %>%
    select(scenario, region, source, target, year, value)
  ,
  elec_energy_input_by_elec_gen_tech %>% gather_year_filt() %>%
  rename(source = input, target = technology) %>%
  select(scenario, region, source, target, year, value)
  ,
  # electricity generation by technology
  elec_gen_by_gen_tech %>% gather_year_filt() %>%
    rename(source = technology, target = subsector) %>%
    select(scenario, region, source, target, year, value)
  ,
  # bridge gen sector to electricity supply sector
  elec_gen_by_gen_tech %>% gather_year_filt() %>%
    # sum by subsector
    group_by(scenario, region, subsector, output, year) %>%
    summarise(value = sum(value)) %>%
    rename(source = subsector, target = output) %>% ungroup()
  ,
  # inputs to the end use sectors
  elec_td_inputs_and_outputs %>% gather_year_filt() %>%
    filter(grepl("elect_", sector)) %>%
    # TODO: add electricity_net_ownuse, and create a losses "end-use"
    mutate(source = "electricity") %>%
    rename(target = sector) %>%
    select(scenario, region, source, target, year, value)
  ,
  # electricity consumption by demand sector
  elec_consumption_by_demand_sector %>% gather_year_filt() %>%
    rename(source = input, target = sector)
)

df_sankey <- elec_source_target

# # just for fun, try one for inputs by tech
# inputsbytech_usa <- read_csv("inputsbytech_usa.csv") %>% gather_year_filt() %>%
#   rename(source = input, target = sector) %>% mutate(scenario = "REF") %>%
#   select(scenario, region, source, target, year, value)
# df_sankey <- inputsbytech_usa

# create a sankey diagram for electricity using plotly ----------------

{ # figure metadata
  node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))
  df_sankey$source <- factor(df_sankey$source, levels = node_labels)
  df_sankey$target <- factor(df_sankey$target, levels = node_labels)
}

# plot
sankey <- plot_ly(
  type = 'sankey',
  orientation = 'h',

  node = list(
    label = node_labels,
    pad = 15,
    thickness = 20,
    line = list(
      color = 'black',
      width = 0.5
    )
  ),

  link = list(
    source = as.integer(df_sankey$source) - 1, # subtract 1 because plotly's indexing starts at 0
    target = as.integer(df_sankey$target) - 1, # subtract 1 for the same reason
    value = df_sankey$value
  )
)

sankey %>% layout(title = "U.S. Electricity Generation and Consumption in 2025")







################## ARCHIVE ##################
# provide a seed i.e., first input in the hierarchy such as "natural gas"
# the code should be able to first determine which sectors use the seed input
# then use all the outputs of the sectors as inputs to the next level
# and so on until the last level is reached
# the last level should be the end use sectors

# the code should be able to handle multiple end use sectors
# the code should be able to handle multiple inputs at each level
# the code should be able to handle multiple outputs at each level





# create for all scenario and all years
for (scen in unique(scenarios)) {
  for (year in unique(modelyears)) {
    # CODE FROM ABOVE
  }
  }



