# This script creates a highly aggregated, all-of-economy energy-water flows
# Sankey diagram for GCAM-USA
#
# Hassan Niazi, May 2024

source("./R/functions.R")

################################################################################
# Load data
{
  gcamusa_input_by_tech <- read_csv(paste0(data_dir, "inputsbytech_gcamusa.csv"))
  gcamusa_input_by_sectors <- read_csv(paste0(data_dir, "inputsbysector_gcamusa.csv"))
}

################################################################################
# USE INPUTS BY TECH
################################################################################
{
  # Get unique categories for technologies
  gcamusa_input_by_tech %>% select(sector, subsector, technology, input) %>% unique() -> all_uniques_inputs

  # Get unique categories
  gcamusa_input_by_tech %>% select(sector) %>% unique() -> unique_sectors
  gcamusa_input_by_tech %>% select(subsector) %>% unique() -> unique_subsectors
  gcamusa_input_by_tech %>% select(technology) %>% unique() -> unique_technologies
  gcamusa_input_by_tech %>% select(input) %>% unique() -> unique_inputs

}

# filter for one year
gcamusa_input_by_tech_2050 <- gather_year_filt(gcamusa_input_by_tech, 2050)

# clean up using obvious aggregations (e.g., water withdrawals from all basins that are an input to one sector can be aggregated)
{
  gcamusa_input_by_tech_2050 %>%
    # drop the total withdrawals rows: they are identified by _W in both sector and input
    filter(!str_detect(input, "water_td_") | !str_detect(technology, "_W")) %>%
    filter(str_detect(input, "water_td_") & str_detect(input, "_W")) %>%
    group_by(sector) %>%
    summarise(value = sum(value)) %>%
    mutate(input = "water_td_W") %>%
    bind_rows(gcamusa_input_by_tech_2050 %>% filter(!str_detect(input, "water_td_"))) %>%
    mutate(across(-Units, ~ replace_na(.x, unique(na.omit(.x))[1])),
           Units = replace_na(Units, "km3")) %>%
    select(scenario, region, sector, subsector, technology, input, year, value, Units) -> gcamusa_input_by_tech_2050_wateraggreated
}





# create a sankey diagram for GCAM-USA using plotly
{
  df_sankey_data <- gcamusa_input_by_tech_2050_wateraggreated

  # define sources and targets
  gcamusa_source_target <- rbind(
    df_sankey_data %>%
      rename(source = input, target = subsector) %>%
      select(scenario, region, source, target, year, value)
  )

  df_sankey <- gcamusa_source_target

  # figure metadata
  node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))

  # create a sankey diagram for GCAM-USA using plotly
  p <- plot_ly(
    data = df_sankey,
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = node_labels
    ),
    link = list(
      source = match(df_sankey$source, node_labels) - 1,
      target = match(df_sankey$target, node_labels) - 1,
      value = df_sankey$value
    )
  ) %>%
    layout(
      title = "GCAM-USA Energy-Water Flows",
      font = list(size = 10)
    )

  p
}


################################################################################
# USE INPUTS BY SECTORS
################################################################################

# Get unique categories for sectors
{
  gcamusa_input_by_sectors %>% select(sector, input) %>% unique() -> all_unique_sectors_inputs

  gcamusa_input_by_sectors %>% select(sector) %>% unique() -> unique_sectors
  gcamusa_input_by_sectors %>% select(input) %>% unique() -> unique_inputs
  # write_csv(sector, paste0(data_dir, "sector_gcamusa.csv"))
}

# select one year using gather_year_filt
gcamusa_input_by_sectors_2050 <- gather_year_filt(gcamusa_input_by_sectors, 2050)

# clean up using obvious aggregations (e.g., water withdrawals from all basins that are an input to one sector can be aggregated)
# water withdrawals start with water_td_ end with _W in inputs column
{
  gcamusa_input_by_sectors_2050 %>%
    # drop the total withdrawals rows: they are identfied by _W in both sector and input
    filter(!str_detect(input, "water_td_") | !str_detect(sector, "_W")) %>%
    filter(str_detect(input, "water_td_") & str_detect(input, "_W")) %>%
    group_by(sector) %>%
    summarise(value = sum(value)) %>%
    mutate(input = "water_td_W") %>%
    bind_rows(gcamusa_input_by_sectors_2050 %>% filter(!str_detect(input, "water_td_"))) %>%
    mutate(across(-Units, ~ replace_na(.x, unique(na.omit(.x))[1])),
           Units = replace_na(Units, "km3")) %>%
    select(scenario, region, sector, input, year, value, Units) -> gcamusa_input_by_sectors_2050_wateraggregated
}


# drop land-input because units are too large
gcamusa_input_by_sectors_2050_wateraggregated %>% filter(input != "land-input") -> gcamusa_input_by_sectors_2050_aggregated


# create a sankey diagram for GCAM-USA using plotly
{
  df_sankey_data <- gcamusa_input_by_sectors_2050_aggregated

  # define sources and targets
  gcamusa_source_target <- rbind(
    df_sankey_data %>%
      rename(source = input, target = sector) %>%
      select(scenario, region, source, target, year, value)
  )

  df_sankey <- gcamusa_source_target

  # figure metadata
  node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))

  # create a sankey diagram for GCAM-USA using plotly
  p <- plot_ly(
    data = df_sankey,
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = node_labels
    ),
    link = list(
      source = match(df_sankey$source, node_labels) - 1,
      target = match(df_sankey$target, node_labels) - 1,
      value = df_sankey$value
    )
  ) %>%
    layout(
      title = "GCAM-USA Energy-Water Flows",
      font = list(size = 10)
    )

  p
}

# CONCLUSION: We can not create sankeys from inputs by sectors because it lacks
# the technology level detail that allows to track the inputs all the way to the
# end-uses especially for the energy system.
#
# but there is something generalize-able here! Look into it after hand-stitching a sankey together.


