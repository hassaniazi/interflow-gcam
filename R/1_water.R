# Energy water sankey diagram
#
# Hassan Niazi, June 2024

source("./R/0_functions.R")

# read these files
{
  water_usa_available <- read_csv(paste0(data_dir, "water_usa_available.csv"))
  water_usa_totwithdraw <- read_csv(paste0(data_dir, "water_usa_totwithdraw.csv"))
  water_usa_withdrawbysource <- read_csv(paste0(data_dir, "water_usa_withdrawbysource.csv"))
  water_usa_usebycategory <- read_csv(paste0(data_dir, "water_usa_usebycategory.csv"))
  water_usa_usebysector <- read_csv(paste0(data_dir, "water_usa_usebysector.csv"))
}


# filter for one year in the same order as above
year <- 2050
water_usa_available_Y <- gather_year_filt(water_usa_available, year) %>% exclude_PacArctic()
# water_usa_totwithdraw_Y <- gather_year_filt(water_usa_totwithdraw, year)
water_usa_withdrawbysource_Y <- gather_year_filt(water_usa_withdrawbysource, year) %>% exclude_PacArctic(resource)
water_usa_usebycategory_Y <- gather_year_filt(water_usa_usebycategory, year)
water_usa_usebysector_Y <- gather_year_filt(water_usa_usebysector, year)


# nesting of water sankey, indentation suggests sources and targets
# total water available (gw and sw)
# 	total water available (gw and sw) by basins
# 		total water NOT withdrawn (gw and sw)
# 		total water withdrawn (gw and sw)
# 			total water withdrawn (gw and sw) by basins
# 				water withdrawn by use category
# 					water withdrawn by end use


{ #SANKEY FOR WATER AVAILABILITY BY SOURCE AND BASINS

# add arbitrary groundwater as twice of runoff
water_usa_available_Y %>% rbind(water_usa_available_Y %>% mutate(subresource = "groundwater", value = 1.5 * value)) -> water_usa_available_Y_swgw

# # total water available in the region
# water_usa_available_Y_swgw %>% group_by(region) %>% summarise(value = sum(value)) %>% ungroup() %>%
#   # append tot_water_ to the region name
#   mutate(region = paste0("tot_water_", region)) -> total_water_available_region

# total water available by subresource
water_usa_available_Y_swgw %>% group_by(region, subresource) %>% summarise(value = sum(value)) %>% ungroup() %>%
  mutate(region = paste0("tot_water_", region),
         subresource = paste0("tot_", subresource, "_avail")) %>%
  rename(source = region, target = subresource) -> total_water_available_subresource

# total water available by basins with tot_groundwater_avail and tot_runoff_avail as sources
water_usa_available_Y_swgw %>%
  mutate(Basin = paste0(gsub("_water withdrawals", "_avail", Basin)),
         subresource = paste0("tot_", subresource, "_avail")) %>%
  select(source = subresource, target = Basin, value) -> source_to_basin

# We may not need this becasue GW and SW for each basin can already be s
# water_usa_available_Y_swgw %>%
#   mutate(Basin = paste0(gsub("_water withdrawals", "_avail", Basin)),
#          Basin_subresource = paste0(gsub("_water withdrawals", "_avail", Basin), "_", subresource)) %>%
#   select(source = Basin, target = Basin_subresource, value) -> basin_to_subresource


# prepare a dataframe for sources and targets for sankey diagram by row binding: the flow is total_water_available_region to total_water_available_subresource to total_water_available_region_basin to water_usa_available_Y_swgw. You would have to rename the columns and sources and targets to make it work. I want to see tot_avail_USA to go tot_gw_avail and tot_sw_avail and then to tot_basins_avail and then subresources (ArkWhtRedR_runoff_avail or generally basinname_subresource_avail)
rbind(total_water_available_subresource, source_to_basin) -> water_usa_Y_avail

}

# water availability sankey
plot_sankey(water_usa_Y_avail, "Water availability by source and basins")


{ #SANKEY FOR WATER WITHDRAWALS (USAGE) BY SOURCE, CATEGORY AND END-USE

# total water withdrawn by source
water_usa_withdrawbysource_Y %>%
  # sum all groundwater grades in each basin into groundwater
  mutate(subresource = ifelse(grepl("groundwater", subresource), "groundwater_W", subresource),
         subresource = ifelse(grepl("runoff", subresource), "runoff_W", subresource)) %>%
  group_by(resource, subresource) %>% summarise(value = sum(value)) %>% ungroup() %>%
  select(source = resource, target = subresource, value) -> water_usa_withdrawbysource_Y_tot_B

# add groundwater_W and runoff_W as a group after groundwater_avail and runoff_avail. Target would be groundwater and runoff
water_usa_withdrawbysource_Y_tot_B %>% group_by(target) %>%
  summarise(value = sum(value)) %>% ungroup() -> water_usa_withdrawbysource_Y_tot


# total water used by category
water_usa_usebycategory_Y %>%
  # sum all inputs with water_td_irr* into water_td_irr_W
  mutate(input = ifelse(grepl("water_td_irr", input), "water_td_irr_W", input)) %>%
  group_by(input) %>% summarise(value = sum(value)) %>% ungroup() %>%
  repeat_add_columns(tibble(source = c("runoff_W", "groundwater_W"))) %>%
  filter(!(input == "water_td_elec_W" & source == "groundwater_W")) %>%
  # split end use by source based on groundwater to runoff withdrawal share from water_usa_withdrawbysource_Y_tot except water_td_elec_W
  #TODO: THIS ASSUMES EACH BASIN HAS THE SAME SHARE OF GROUNDWATER AND RUNOFF WITHDRAWALS
  mutate(gw2totshare = water_usa_withdrawbysource_Y_tot$value[water_usa_withdrawbysource_Y_tot$target == "groundwater_W"]/sum(water_usa_withdrawbysource_Y_tot$value),
         sw2totshare = 1 - gw2totshare,
         value = case_when(
           input == "water_td_elec_W" ~ value,
           source == "groundwater_W" ~ value * gw2totshare,
           source == "runoff_W" ~ value * sw2totshare
         )) %>%
  select(source, target = input, value) -> water_usa_usebycategory_Y_tot

# print the mismatch between water withdrawn by source and water used by category
paste(sum(water_usa_withdrawbysource_Y_tot$value) - sum(water_usa_usebycategory_Y_tot$value), "km3 water withdrawn but not used")


# map water use categories water_td_ to end uses
water_usa_usebysector_Y %>%
  mutate(source = case_when(
    # Irrigation water
    grepl("Corn|FiberCrop|FodderHerb|Fruits|Legumes|FodderGrass|MiscCrop|NutsSeeds|OilCrop|OtherGrain|Rice|RootTuber|Soybean|Vegetables|Wheat|biomass|SugarCrop", sector) ~ "water_td_irr_W",
    # Animal water
    grepl("Beef|Dairy|Pork|Poultry|SheepGoat", sector) ~ "water_td_an_W",
    # Municipal water
    grepl("municipal", sector) ~ "water_td_muni_W",
    # Industrial water
    grepl("H2|paper|other industry", sector) ~ "water_td_ind_W",
    # Electricity water
    grepl("elec", sector) ~ "water_td_elec_W",
    # Mining water
    grepl("nuclear|regional", sector) ~ "water_td_pri_W",
    TRUE ~ "other"
  )) %>%
  select(source, target = sector, value) -> water_usa_usebysector_Y_mapped



# combine all data
bind_rows(water_usa_usebycategory_Y_tot, water_usa_usebysector_Y_mapped) %>%
  mutate(value = round(value, 2)) -> water_usa_Y_use


}

# water use sankey
plot_sankey(water_usa_Y_use %>% mutate(scenario = "rcp45cooler_ssp3", year = year))

# water use by basin sankey
plot_sankey(rbind(water_usa_withdrawbysource_Y_tot_B, water_usa_usebycategory_Y_tot, water_usa_usebysector_Y_mapped) %>% mutate(scenario = "rcp45cooler_ssp3", year = year), "GCAM-USA Water use by source, category and end-use in 2050")


{ #SANKEY FOR WATER AVAILABILITY, USAGE AND UNUSED BY SOURCE AND BASIN

water_usa_withdrawbysource_Y_tot_B %>% select(target = source, value) %>%  mutate(source = "Used") -> water_usa_withdrawbysource_Y_tot_B_used

water_usa_withdrawbysource_Y_tot_B_used %>% group_by(target) %>% summarise(Used = sum(value)) %>% ungroup() %>%
  mutate(target = gsub("_.*", "", target)) %>%
  right_join(source_to_basin %>% mutate(target = gsub("_.*", "", target)) %>% group_by(target) %>%
               summarise(total_avail = sum(value)) %>% ungroup(), by = "target") %>%
  mutate(Unused = total_avail - Used) %>% pivot_longer(c(Used, Unused), names_to = "type", values_to = "value") %>%
  # put _avail back to the target
  mutate(target = paste0(target, "_avail")) %>%
  select(source = target, target = type, value) -> water_avail_to_used_unused

# there is way to do this by water source if we don't do group_by(target) in the previous step

}

# sankey for combined water availability, usage and unused by source and basin
plot_sankey(rbind(water_usa_Y_avail, water_avail_to_used_unused, water_usa_withdrawbysource_Y_tot_B_used, water_usa_withdrawbysource_Y_tot_B, water_usa_usebycategory_Y_tot, water_usa_usebysector_Y_mapped) %>% exclude_PacArctic(source, "PacNW")  %>% exclude_PacArctic(target, "PacNW"), "GCAM-USA Water Supply by Source and Use by Category and End-use in 2050")
















################################################################################
# ARCHIVE
################################################################################

{# simple sankey with water sources and enduses
# total water available
water_usa_available_Y %>% group_by(subresource) %>% summarise(value = sum(value)) %>% ungroup() %>%
  mutate(subresource = ifelse(subresource == "runoff", "runoff_avail", subresource)) %>%
  # arbitrarily setting groundwater availability value
  bind_rows(data.frame(subresource = "groundwater_avail", value = 5000)) %>%
  # make subresource the source and put runoff in target for runoff_avail and groundwater for groundwater_avail
  select(source = subresource, target = subresource, value) %>%
  # remove _avail from target names
  mutate(target = gsub("_avail", "", target)) -> water_usa_available_Y_tot

# # total water available by basin
# water_usa_available_Y %>% group_by(Basin) %>% summarise(value = sum(value)) %>% ungroup() %>%
#   mutate(source = "runoff") %>%
#   select(source, target = Basin, value) -> water_usa_available_Y_tot_B

# total water withdrawn by source
water_usa_withdrawbysource_Y %>%
  # sum all groundwater grades in each basin into groundwater
  mutate(subresource = ifelse(grepl("groundwater", subresource), "groundwater", subresource)) %>%
  group_by(resource, subresource) %>% summarise(value = sum(value)) %>% ungroup() %>%
  select(source = subresource, target = resource, value) -> water_usa_withdrawbysource_Y_tot_B

# add groundwater_W and runoff_W as a group after groundwater_avail and runoff_avail. Target would be groundwater and runoff
water_usa_withdrawbysource_Y_tot_B %>% group_by(source) %>% summarise(value = sum(value)) %>% ungroup() %>%
  select(source, target = source, value) -> water_usa_withdrawbysource_Y_tot

# remove the basin level detail
water_usa_withdrawbysource_Y_tot_B %>% group_by(source) %>% summarise(value = sum(value)) %>% ungroup() -> water_usa_withdrawbysource_Y_tot

# total water used by category
water_usa_usebycategory_Y %>%
  # sum all inputs with water_td_irr* into water_td_irr_W
  mutate(input = ifelse(grepl("water_td_irr", input), "water_td_irr_W", input)) %>%
  group_by(input) %>% summarise(value = sum(value)) %>% ungroup() %>%
  repeat_add_columns(tibble(source = c("runoff", "groundwater"))) %>%
  filter(!(input == "water_td_elec_W" & source == "groundwater")) %>%
  # split end use by source based on groundwater to runoff withdrawal share from water_usa_withdrawbysource_Y_tot except water_td_elec_W
  mutate(gw2totshare = water_usa_withdrawbysource_Y_tot$value[water_usa_withdrawbysource_Y_tot$source == "groundwater"]/sum(water_usa_withdrawbysource_Y_tot$value),
         sw2totshare = 1 - gw2totshare,
         value = case_when(
           input == "water_td_elec_W" ~ value,
           source == "groundwater" ~ value * gw2totshare,
           source == "runoff" ~ value * sw2totshare
         )) %>%
  select(source, target = input, value) -> water_usa_usebycategory_Y_tot

# print the mismatch between water withdrawn by source and water used by category
paste(sum(water_usa_withdrawbysource_Y_tot$value) - sum(water_usa_usebycategory_Y_tot$value), "km3 water withdrawn but not used")


# map water use categories water_td_ to end uses
water_usa_usebysector_Y %>%
  mutate(source = case_when(
    # Irrigation water
    grepl("Corn|FiberCrop|FodderHerb|Fruits|Legumes|FodderGrass|MiscCrop|NutsSeeds|OilCrop|OtherGrain|Rice|RootTuber|Soybean|Vegetables|Wheat|biomass|SugarCrop", sector) ~ "water_td_irr_W",
    # Animal water
    grepl("Beef|Dairy|Pork|Poultry|SheepGoat", sector) ~ "water_td_an_W",
    # Municipal water
    grepl("municipal", sector) ~ "water_td_muni_W",
    # Industrial water
    grepl("H2|paper|other industry", sector) ~ "water_td_ind_W",
    # Electricity water
    grepl("elec", sector) ~ "water_td_elec_W",
    # Mining water
    grepl("nuclear|regional", sector) ~ "water_td_pri_W",
    TRUE ~ "other"
  )) %>%
  select(source, target = sector, value) -> water_usa_usebysector_Y_mapped

# combine all data
bind_rows(water_usa_available_Y_tot, water_usa_usebycategory_Y_tot, water_usa_usebysector_Y_mapped) %>%
  mutate(value = round(value, 2)) -> water_usa_Y

# plot Sankey
plot_sankey(water_usa_Y)
}


plot_sankey(elec_source_target)
plot_sankey(bind_rows(elec_source_target, water_usa_Y %>% mutate(value = value * 10e-3)))

# TODO:
# add groundwater withdrawn and runoff withdrawn as separate sources
# bring basins back in
