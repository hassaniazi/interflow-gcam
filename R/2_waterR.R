# water sankey with regions
#
# Hassan Niazi, April 2024

# load data and queries ----
source("./R/0_functions.R")

im3_epri <- loadProject(proj = paste0(data_dir, "im3scen_epri.dat"))

listScenarios(im3_epri); listQueries(im3_epri)


# extra: create a basin resource to region mapping from the results


# prepare short and long basin names
basinID <- read_csv(paste0(data_dir, "basin_ID.csv"), skip = 6)
basin_country <- read_csv(paste0(data_dir, "basin_to_country_mapping.csv"), skip = 7) %>% select(basin_id = GCAM_basin_ID, basin_short = GLU_name)# country = country_name)

# basinID %>% left_join(basin_country, by = "basin_id")

basin_names <- getQuery(im3_epri, "water withdrawals by tech") %>% select(subsector) %>% filter(grepl("_", subsector)) %>% distinct() %>%
  separate(subsector, into = c("subsector", "basin_short"), sep = "_") %>% select(basin_short) %>%
  right_join(basin_country, by = "basin_short") %>% left_join(basinID, by = "basin_id") %>% distinct() %>% arrange(basin_name)


# resources: region is where basin is located
basin_reg_map <- getQuery(im3_epri, "basin level available runoff") %>% select(region, basin = basin) %>% distinct() %>% remove_water_withdrawals_string(basin) %>%
# demand side: subregion is where the water is used
  left_join(getQuery(im3_epri, "water withdrawals by state, sector, basin (includes desal)") %>% select(subregion = region, basin = subsector) %>% distinct(),
            by = "basin") %>% arrange(basin) %>%
  # attach basin short names and IDs
  left_join(basin_names %>% select(basin_id, basin_short, basin = basin_name), by = c("basin"))
# write_csv(basin_reg_map, paste0(data_dir, "basin_reg_map.csv"))

# # create edge list and graph from region to subregion via basin
# graph <- tidygraph::as_tbl_graph(basin_reg_map %>% #filter(region %in% c("USA", "Mexico", "Canada")) %>%
#                         distinct(from = region, to = subregion), directed = F)
#
# # plot with ggraph
# ggraph::ggraph(graph, layout = "fr") +  # "fr" "kk" or "circle"
#   geom_edge_link(color = "red4") +
#   geom_node_point(size = 5, color = "dodgerblue2") +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()


# TODO: eventually remove the filtering from here. Keeping here for now for easy processing
basinLevelAvailableRunoff <- getQuery(im3_epri, "basin level available runoff") #%>% filter_CONUSregions()
totalGroundwaterAvailable <- getQuery(im3_epri, "total groundwater available") #%>% filter_CONUSregions()

waterWithdrawalsByWaterSource <- getQuery(im3_epri, "water withdrawals by water source (runoff vs. groundwater)") #%>% filter_CONUSregions()
waterWithdrawalsByStateSectorBasin <- getQuery(im3_epri, "water withdrawals by state, sector, basin (includes desal)") #%>% filter_CONUSregions()
# waterWithdrawalsByWaterMappingSource <- getQuery(im3_epri, "water withdrawals by water mapping source") #%>% filter_CONUSregions()
waterWithdrawalsByTech <- getQuery(im3_epri, "water withdrawals by tech") #%>% filter_CONUSregions()

{
# check if total USA water use is equal to all states summed up
usa_tot <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region == "USA") %>%
  filter(grepl("water_td_irr|water_td_an|water_td_pri", sector))

  usa_tot %>% filter(year == 2020) %>% replace_after_irr_string(sector) %>%
    group_by(sector) %>% summarise(value = sum(value)) %>% ungroup()

states_tot <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region %in% states) %>%
  filter(grepl("water_td_irr|water_td_an|water_td_pri", sector))

state_sums <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region %in% states, year == 2050) %>%
  replace_after_irr_string(sector) %>%
  group_by(sector) %>% summarise(value = sum(value)) %>% ungroup()

print(paste0("USA total water use ", sum(usa_tot$value), " | States total water use ", sum(states_tot$value)))
print(paste0("USA - States total water use: ", sum(usa_tot$value) - sum(states_tot$value), " |" ,ifelse(sum(usa_tot$value) - sum(states_tot$value) == 0, " (equal)", ifelse(sum(usa_tot$value) > sum(states_tot$value), " (USA is larger)", " (States are larger)"))))

# check if USA irrigation is equal to all states summed up
usa_irr <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region == "USA", year == 2020) %>%
  filter(grepl("water_td_irr", sector))

states_irr <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region %in% states, year == 2020) %>%
  filter(grepl("water_td_irr", sector))

print(paste0("USA irrigation ", sum(usa_irr$value), " | States irrigation ", sum(states_irr$value)))
print(paste0("USA - States irrigation: ", sum(usa_irr$value) - sum(states_irr$value), " |" ,ifelse(sum(usa_irr$value) - sum(states_irr$value) == 0, " (equal)", ifelse(sum(usa_irr$value) > sum(states_irr$value), " (USA is larger)", " (States are larger)"))))

sort(unique(states_irr$sector)) %in% sort(unique(usa_irr$sector))

# if states are larger does this mean trans boundary regions are using the rest
usa_neighbors <- waterWithdrawalsByStateSectorBasin %>%
  filter(scenario == "rcp45cooler_ssp3", region %in% c("Canada", "Mexico")) %>%
  filter(grepl("water_td_irr|water_td_an|water_td_pri", sector)) %>%
  filter(subsector %in% unique(states_irr$subsector))

sum(usa_neighbors$value); sum(usa_irr$value)

# add usa usa_irr and usa_neighbors and compare to states_irr
sum(rbind(usa_tot, usa_neighbors)$value) - sum(states_tot$value) # states still more
sum(rbind(usa_irr, usa_neighbors %>% filter(grepl("water_td_irr", sector)))$value) - sum(states_irr$value) # states still more
}


# PART 1: PROCESSING ----
# this will clean and process the queries to prepare the data at the region level.
# scenario; region; source; target; year; value; units
# We will filter for US regions and do necessary aggregations in the later part


# basin resources/availability ----

totalWaterAvailability_R <- rbind(
  basinLevelAvailableRunoff %>%
    # filter_basin_resource(basin) %>%
    remove_water_withdrawals_string(basin) %>%
    # we may have to switch the source and target here to start from water sources and go to basins
    select(region, scenario, target = basin, source = subresource, year, value, units = Units),
  totalGroundwaterAvailable %>%
    # filter_basin_resource() %>%
    remove_water_withdrawals_string() %>%
    filter(grade != "grade hist") %>%
    # get rid of grades; aggregate up
    group_by(scenario, region, resource, subresource, year, Units) %>%
    summarise(value = sum(value)) %>% ungroup() %>%
    select(scenario, region, target = resource, source = subresource, year, value, units = Units)
)

# plot_sankey(totalWaterAvailability_R, "Total Water Availability by Basin and Resource Type")

# desal ----
# desal by basin
desal_bybasinR <- waterWithdrawalsByStateSectorBasin %>%
  # filter(region != "USA") %>% # TODO: will need to filter only the states
  filter(technology == "desalination") %>%
  # filter_CONUSregions(subsector, basins_conus_resource) %>%
  group_by(scenario, region, subsector, technology, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = technology, target = subsector, year, value, units = Units)

# desal use by basin
desal_usebybasinR <- waterWithdrawalsByStateSectorBasin %>%
  # filter(region != "USA") %>% # TODO: will need to filter only the states
  filter(technology == "desalination") %>%
  replace_after_irr_string() %>%
  # filter_CONUSregions(subsector, basins_conus_resource) %>%
  group_by(scenario, region, sector, subsector, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = subsector, target = sector, year, value, units = Units)


# water supply type ----

df_WithdrawalsBySource_R <- rbind(desal_bybasinR,
  waterWithdrawalsByWaterSource %>%
    # filter_basin_resource() %>%
    remove_water_withdrawals_string() %>%
    select(scenario, region, source = subresource, target = resource, year, value, units = Units))

plot_sankey(df_WithdrawalsBySource_R, "Water Withdrawals by Basin and Water Source")

# water supply type by state ----

# the assumption here is that total demand in a region is the sum of the demand
# served by each basin each source. E.g., one state withdrawing from 2 basins
# will have 2 SW:GW splits (in total 4 values - B1sw B1gw B2sw B2gw) based on
# the SW:GW split of each basin but total water withdrawn will stay the same.

source_shares_R <- waterWithdrawalsByWaterSource %>%
  # filter_basin_resource() %>%
  remove_water_withdrawals_string() %>%
  group_by(scenario, region, resource, year) %>%
  mutate(share = value / sum(value)) %>% ungroup() %>% rename(units = Units)

source_byregion <- waterWithdrawalsByStateSectorBasin %>%
  filter(!(region %in% c("USA"))) %>% # exclude USA level data because it aggregated up from basin level data, but lacks the technology
  # filter(region %in% states_conus) %>%
  filter(technology != "desalination") %>% # we have desalination at the basin level

  # TODO: check if irr needs to be removed, especially when we want to do region-based sw-gw split
  replace_after_irr_string() %>% # if this is removed, each basin will supply to it's own irrigation demand
  # group_by(scenario, region, sector, technology, year, Units) %>% # not needed as the scen, reg, sector, tech, year, grouping (esp tech because it has the basin) will take care of the appropriation of water from multiple basins into one state
  # summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, subregion = region, sector, technology, year, demand_withdraw = value, units = Units) %>%
  left_join(source_shares_R, by = c("scenario", "technology" = "resource", "year", "units")) %>%
  mutate(source_withdraw = demand_withdraw * share)


# source type to basin
df_sourcetype_byregion <- source_byregion %>%
  # we need to sum the sources over demands (about 12 - 2 sources x 6 demands)
  # to 2 because source sums by region should be for all demands not for each
  # demand. This may cause an issue if we want to filter by demand type too
  # e.g., only show the flows for irrigation (the sources should also shrink in
  # that case). But at least we have the data by region, source type, basin, and
  # end use.
  group_by(scenario, subregion, subresource, technology, year, units) %>%
  summarise(source_withdraw_R = sum(source_withdraw)) %>% ungroup() %>%
  select(scenario, region = subregion, source = subresource, target = technology, year, value = source_withdraw_R, units) %>%
  bind_rows(desal_bybasinR)

plot_sankey(df_sourcetype_byregion, "Water Withdrawals by Source Type and Basin")
plot_sankey(df_sourcetype_byregion, "Water Withdrawals by Source Type and Basin", reg = "CA")

# TODO: check if queried source type is the same as the one in the source_byregion

# water use type ----

df_waterWithdrawalsByStateSectorBasin_R <- waterWithdrawalsByStateSectorBasin %>%
  filter(!(region %in% c("USA"))) %>% # exclude USA level data because it aggregated up from basin level data, but lacks the technology detail (name of the basin here)
  # filter_CONUSregions(technology, basins_conus_resource) %>% # not sure why nonCONUS basins have crept in here at the technology level, but remove them
  filter(technology != "desalination") %>% # we have desalination at the basin level in the previous plot
  replace_after_irr_string() %>% # if this is removed, each basin will supply to it's own irrigation demand
  group_by(scenario, region, sector, technology, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = technology, target = sector, year, value, units = Units)

plot_sankey(df_waterWithdrawalsByStateSectorBasin_R, "Water Withdrawals by Use Category and Basin")

plot_sankey(df_waterWithdrawalsByStateSectorBasin_R, "Water Withdrawals by Use Category and Basin", reg = c("GA"))

# total water use by basin: include desal use and direct basin use
totalwaterusebybasin <- rbind(desal_usebybasinR, df_waterWithdrawalsByStateSectorBasin_R) %>%
  # group by everythign byt value
  group_by(scenario, region, source, target, year, units) %>%
  summarise(value = sum(value)) %>% ungroup()


# let's try to piece this with waterWithdrawalsByWaterSource to the the water source type, basin, water use type flow

source_useR <- rbind(df_sourcetype_byregion, totalwaterusebybasin)

plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use")
# plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("USA"))
plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("GA"))
plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("CA", "NY", "MD"))
plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("CA", "South Africa"))



# water end use ----

{
# checks to see how demands are handled
waterWithdrawalsByStateSectorBasin %>% filter(scenario == "rcp45cooler_ssp3", year == 2020, sector == "water_td_an_W") %>%
  filter(region %in% c("USA", "AR")) # AR Akr 0.001944920 + Akr Misp 0.001762130 = 0.00370705 USA AR
# conclusion: take states and sum over tech if need a total for an and pri, which would be equal to USA-state value

waterWithdrawalsByStateSectorBasin %>% filter(scenario == "rcp45cooler_ssp3", year == 2020, sector == "water_td_irr_UsaCstSE_W")
# conclusion: take usa value but reduce by x 0.8299342 to get states values. usa withdraws, states get less due to conveyance losses 0.970467/1.16933 GA = 0.8299342 conveyance loss
}

waterWithdrawalsByTech_watCat <- waterWithdrawalsByTech %>% map_water_use_to_categories() # %>% filter_CONUSregions(region, c("USA", states_conus))

# approach # 2:
waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
  select(scenario, region, source = watcategory, target = technology, year, value, units = Units)

plot_sankey(rbind(source_useR,
                  waterWithdrawalsByTech_watCat %>%
                    filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
                    select(scenario, region, source = watcategory, target = technology, year, value, units = Units)
                  )
                  , "Water Withdrawals by Source, Basin, Use", reg = c("GA"))


## livestock and mining
USA_an_pri <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_pri|water_td_an", watcategory)) %>%
  filter(region == "USA")
  # split each row 50 ways by each state i.e., Beef Mixed FeedCrops is total US which needs to shared to all states. df_waterWithdrawalsByStateSectorBasin_R already has total state level number for water_td_an_W. by tech has tech details but states has state details, our goal is to have tech details at the state level
USA_an_pri %>% select(sector, subsector, technology) %>% distinct() %>% arrange(sector, subsector, technology) -> temp

{
# check water_td_an_W sums before disaggregation
USA_an_pri %>% filter(watcategory == "water_td_an_W" , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value)) #
df_waterWithdrawalsByStateSectorBasin_R %>% filter(target == "water_td_an_W", region %in% states , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value))
# conclusion: minor excess withdrawals by tech in future years compared to state basins query
}

# calculate share of each use category in each state (to be used to assign each tech from USA to each state)
state_cat_shares <- df_waterWithdrawalsByStateSectorBasin_R %>% filter(region %in% states) %>%
  group_by(scenario, region, target, year) %>%
  summarise(cat_total = sum(value)) %>% ungroup() %>%
  group_by(scenario, target, year) %>%
  mutate(state_cat_share = cat_total/sum(cat_total)) %>% ungroup()

# check if the shares sum to 1 e.g., sum(livestock water use over all states = 1)
sum((state_cat_shares %>% filter(scenario == "rcp45cooler_ssp3", year == 2050, target == "water_td_an_W"))$state_cat_share) == 1

# each tech use on USA level gets disaggregated to states based on the state share of category use
# e.g., if MD has 5% of livestock use, then beef will also be 5% in MD
USA_an_pri_tech <- USA_an_pri %>% select(-region) %>%
  left_join(state_cat_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
  mutate(state_tech = value * state_cat_share) %>%
  select(scenario, region, watcategory, sector, subsector, technology, year, value = state_tech, units = Units)

end_use_an_pri_sector <- agg_enduse(USA_an_pri_tech, "watcategory", "sector") # aggregate by sector
end_use_an_pri_subsector <- agg_enduse(USA_an_pri_tech, "sector", "subsector") # aggregate by subsector
end_use_an_pri_tech <- agg_enduse(USA_an_pri_tech, "sector", "technology") # aggregate by technology

end_use_an_pri <- rbind(end_use_an_pri_sector) %>% distinct() %>% remove_source_equals_target()

plot_sankey(rbind(source_useR, end_use_an_pri), "Water Withdrawals by Source, Basin, Use", reg = c("GA"))

## irrigation
USA_irr <- waterWithdrawalsByTech_watCat %>% filter(grepl("water_td_irr", watcategory)) %>%
  filter(region == "USA") %>%
  group_by(scenario, watcategory, sector, subsector, year, units = Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  separate(subsector, into = c("subsector", "basin_short"), sep = "_") #%>%
  #left_join_error_no_match(basin_reg_map %>% select(basin_short, basin) %>% distinct(), by = "basin_short")


USA_irr_state <- USA_irr %>%
  left_join(basin_reg_map %>% select(subregion, basin_short, basin) %>% distinct(), by = "basin_short")

# USA_irr %>% remove_source_equals_target("sector", "subsector")
# unique((USA_irr %>% remove_source_equals_target("sector", "subsector"))$subsector)

USA_irr_subsec <- USA_irr %>%
  left_join(state_cat_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
  mutate(state_subsec = value * state_cat_share) %>%
  select(scenario, region, watcategory, sector, subsector, year, value = state_subsec, units)

end_use_irr <- USA_irr_subsec %>%
  group_by(scenario, region, watcategory, subsector, year, units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = watcategory, target = subsector, year, value, units)

# approach # 1
# get all 6 categories: irr pri and an from USA and the rest from states
# take sector shares of water use for each state and apply to technology in the USA region
unique(df_waterWithdrawalsByStateSectorBasin_R$region) # take all regions

state_sector_use_shares <- df_waterWithdrawalsByStateSectorBasin_R %>% filter(region %in% states) %>%
  group_by(scenario, region, target, year) %>%
  summarise(sector_total = sum(value)) %>% ungroup() %>%
  group_by(scenario, region, year) %>%
  mutate(sector_share = sector_total/sum(sector_total)) %>% ungroup()

waterWithdrawalsByTech_disagg <- waterWithdrawalsByTech_watCat %>%
  filter(region == "USA") %>% select(-region) %>%
  left_join(state_sector_use_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
  mutate(tech_disagg = value * sector_share)

waterWithdrawalsByTech_disagg

watcategory_sector_use_R <- waterWithdrawalsByTech_disagg %>% #waterWithdrawalsByTech_watCat
  group_by(scenario, region, technology, watcategory, year, Units) %>%
  summarise(value = sum(tech_disagg)) %>% ungroup() %>%
  select(scenario, region, source = watcategory, target = technology, year, value, units = Units)

# plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("USA"))
plot_sankey(rbind(source_useR, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("GA"))

# expand electricity water use
watcategory_subsector_use_R <- waterWithdrawalsByTech_watCat %>%
  # filter(sector == "electricity") %>% #mutate(sector = "Electricity Water Use") %>%
  remove_GLUnames(subsector) %>%
  group_by(scenario, region, sector, subsector, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = sector, target = subsector, year, value, units = Units)


waterWithdrawalsByTech_watCat %>%
  remove_GLUnames(subsector) %>% replace_after_irr_string(subsector) %>%
  filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
  group_by(scenario, region, watcategory, subsector, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup()

plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R, watcategory_subsector_use_R), "Water Withdrawals by Source, Basin, Use")

plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("GA"))



# PART 2: AGGREGATIONS ----
# this will filter or aggregate the data at the region level
# there could be two types of aggregations: 1) by region 2) by source/target
# we could use mapping files for this



# PART 3: AESTHETICS ----
# this will clean up the names and do the plotting




# archive #####################################################################

## im3 expc presentation ----
library(rmap)
# rmap::map(data = mapGCAMBasinsUS49, save = F, labels = T)

USA_irr_map <- USA_irr %>% filter(year == 2020) %>% filter(scenario == "rcp45cooler_ssp3") %>%
  group_by(basin_short) %>% summarise(value = sum(value)) %>% ungroup() %>%
  left_join(basin_reg_map %>% select(basin_short, basin) %>% distinct(), by = "basin_short") %>%
  # replace space with _ in basin
  mutate(basin = gsub(" |-", "_", basin))

# USA irrigation water demands USAirr for 2020
rmap::map(data = USA_irr_map %>% rename(subRegion = basin),
          overLayer = rmap::mapGCAMBasinsUS49,
          crop_to_overLayer = T,
          palette = "pal_wet",
          legendType = "pretty",
          crs="+proj=longlat +datum=WGS84 +no_defs",
          # labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 1,
          # overLayerColor = "blue4", overLayerLwd = 1,
          save = F)

## gcam-usa paper ----
# create a by technology dataset by scenario, year for all US

# irrigation
USA_irr_agg <- USA_irr %>%
  group_by(scenario, watcategory, enduse = subsector, year) %>%
  summarise(value = sum(value)) %>% ungroup()

# livestock and mining
USA_an_pri_agg <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_pri|water_td_an", watcategory)) %>%
  filter(region == "USA") %>%
  group_by(scenario, watcategory, enduse = sector, year) %>%
  summarise(value = sum(value)) %>% ungroup()

# municipal and industrial
USA_mun_ind_agg <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_dom|water_td_ind", watcategory)) %>%
  filter_USregions() %>%
  group_by(scenario, watcategory, enduse = sector, year) %>%
  summarise(value = sum(value)) %>% ungroup()


# electricity
USA_elec_agg <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_elec", watcategory)) %>%
  filter_USregions() %>%
  mutate(subsector = paste0("elec_", subsector)) %>%
  group_by(scenario, watcategory, enduse = subsector, year) %>%
  summarise(value = sum(value)) %>% ungroup()

# combine all end uses
USA_water_enduse_agg <- rbind(USA_irr_agg, USA_an_pri_agg, USA_mun_ind_agg, USA_elec_agg)

summary(USA_water_enduse_agg)

# aggregate end uses into broader categories for plotting
aggregate_enduse <- function(data, enduse_col = enduse) {
  enduse_col <- enquo(enduse_col)
  data %>%
    mutate(
      enduse_agg = case_when(
        !!enduse_col %in% c("Corn", "OtherGrain", "Wheat", "Rice") ~ "Crops: Grains",
        !!enduse_col %in% c("FiberCrop", "MiscCrop", "OilCrop", "SugarCrop", "PalmFruit", "RootTuber") ~ "Crops: Specialty",
        !!enduse_col %in% c("biomassGrass", "biomassTree", "FodderHerb") ~ "Crops: Bioenergy",

        !!enduse_col %in% c("Beef", "Pork", "Poultry", "SheepGoat") ~ "Livestock: Meat",
        !!enduse_col == "Dairy" ~ "Livestock: Dairy",

        # !!enduse_col %in% c("nuclearFuelGenII", "nuclearFuelGenIII", "elec_nuclear") ~ "Energy – Nuclear",
        # !!enduse_col %in% c("nuclearFuelGenII", "nuclearFuelGenIII") ~ "Energy Mining: Nuclear",
        !!enduse_col %in% c("elec_nuclear") ~ "Electricity: Nuclear",

        # !!enduse_col %in% c("regional coal", "regional natural gas", "regional oil", "unconventional oil production") ~ "Energy Mining: Fossils",
        !!enduse_col %in% c("nuclearFuelGenII", "nuclearFuelGenIII", "regional coal", "regional natural gas", "regional oil", "unconventional oil production") ~ "Energy Mining",

        !!enduse_col %in% c("elec_biomass", "elec_geothermal", "elec_solar") ~ "Electricity: Renewables",
        !!enduse_col %in% c("elec_refined liquids", "elec_gas", "elec_coal") ~ "Electricity: Fossils",

        !!enduse_col %in% c("municipal water") ~ "Municipal",
        !!enduse_col %in% c("industry") ~ "Industrial",
        TRUE ~ "Other"
      )
    )
}

# order of end uses for plotting
enduse_levels <- c(
  "Municipal",

  "Crops: Grains",
  "Crops: Specialty",
  "Crops: Bioenergy",

  "Energy Mining",
  # "Energy Mining: Nuclear",
  # "Energy Mining: Fossils",

  "Electricity: Fossils",
  "Electricity: Nuclear",
  "Electricity: Renewables",

  "Industrial",

  "Livestock: Meat",
  "Livestock: Dairy"
)

YEARS_TO_PLOT <- c(2020, 2060, 2100)

USA_water_enduse_agg_plot <- USA_water_enduse_agg %>%
  aggregate_enduse() %>%
  # filter(year %in% c(2020, 2040, 2060, 2080, 2100)) %>%
  filter(year %in% YEARS_TO_PLOT) %>%
  group_by(scenario, watcategory, enduse_agg, year) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  mutate(
    scenario = factor(scenario, levels = unique(scenario)),
    # enduse = factor(enduse, levels = unique(enduse)),
    enduse_agg = fct_relevel(enduse_agg, enduse_levels),
    watcategory = factor(watcategory, levels = unique(watcategory))
  )

# build parallel coordinates plot
USA_water_enduse_agg_plot %>%
  plot_ly(width = 1500, height = 600) %>%
  add_trace(
    type = 'parcoords',
    # name = USA_water_enduse_agg_plot$scenario,
    line = list(color = ~year,
                # colorscale = 'Portland', # Blackbody
                colorscale = list(c(0, 'blue'), c(0.5, 'gold'), c(1, 'red')),
                # list of colorscales available at https://plotly.com/r/colorscales/
                # Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd.
                reversescale = T,
                showscale = TRUE,
                colorbar = list(title = "Year", tickvals = YEARS_TO_PLOT, ticktext = YEARS_TO_PLOT),
                size = 3,
                # cmin = 2020, cmid = 2050, cmax = 2100
                cmin = min(USA_water_enduse_agg_plot$year),
                cmax = max(USA_water_enduse_agg_plot$year)
                ),
    dimensions = c(
      # # Add year as first dimension
      # list(list(
      #   range = c(2020, 2100),
      #   label = "Year",
      #   values = USA_water_enduse_agg_plot$year,
      #   tickvals = c(2020, 2050, 2100),
      #   ticktext = c("2020", "2050", "2100")
      # )),
      # add the rest of the dimensions
      lapply(levels(USA_water_enduse_agg_plot$enduse_agg), function(dim_varplot) {
      list(# absolute range for all variables
        range = c(min(USA_water_enduse_agg_plot$value, na.rm = TRUE), max(USA_water_enduse_agg_plot$value, na.rm = TRUE)),
        # relative range for each variable
        # range = c(min(USA_water_enduse_agg_plot$value[USA_water_enduse_agg_plot$enduse_agg == dim_varplot]), max(USA_water_enduse_agg_plot$value[USA_water_enduse_agg_plot$enduse_agg == dim_varplot])),
        label = dim_varplot,
        values = USA_water_enduse_agg_plot$value[USA_water_enduse_agg_plot$enduse_agg == dim_varplot])
    })
    ),
    labelfont = list(size = 12, color = 'black'),
    rangefont = list(size = 11, color = 'black', style = "italic"),
    tickfont = list(size = 10, color = 'gray')
  )



# Reshape data properly for parallel coordinates
USA_water_enduse_agg_wide <- USA_water_enduse_agg %>%
  aggregate_enduse() %>%
  filter(year %in% YEARS_TO_PLOT) %>%
  # filter(!grepl("hotter", scenario)) %>%
  group_by(scenario, enduse_agg, year) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  pivot_wider(
    names_from = enduse_agg,
    values_from = value,
    values_fill = 0  # Fill missing combinations with 0
  ) %>%
  mutate(scenario = factor(scenario, levels = unique(scenario)))

# show scenarios
USA_water_enduse_agg_wide %>%
  plot_ly(
    type = 'parcoords',
    line = list(
      color = ~as.numeric(scenario),
      # colorscale = 'Portland', # Blackbody
      colorscale = list(c(0, 'blue'), c(0.5, 'gold'), c(1, 'red')),
      reversescale = TRUE,
      showscale = TRUE,
      colorbar = list(
        title = "Scenarios",
        tickmode = "array",
        tickvals = 1:8,
        ticktext = levels(USA_water_enduse_agg_wide$scenario)
      )
    ),
    dimensions = c(
      # Year dimension
      list(list(
        range = c(2020, 2100),
        label = "Year",
        values = USA_water_enduse_agg_wide$year,
        tickvals = YEARS_TO_PLOT
      )),
      # Enduse dimensions - now each is a column!
      lapply(enduse_levels, function(enduse_col) {
        if(enduse_col %in% names(USA_water_enduse_agg_wide)) {
          list(
            range = c(0, max(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE)),
            label = enduse_col,
            values = USA_water_enduse_agg_wide[[enduse_col]]
          )
        }
      }),

      # Scenario dimension
      list(list(
        range = c(1, 8),
        label = "Scenario",
        values = as.numeric(USA_water_enduse_agg_wide$scenario),
        tickvals = 1:8,
        ticktext = levels(USA_water_enduse_agg_wide$scenario)
      ))
    ),
    labelfont = list(size = 12, color = 'black'),
    rangefont = list(size = 11, color = 'black', style = "italic"),
    tickfont = list(size = 10, color = 'gray')
  )

calculate_adaptive_range <- function(values) {
  values <- values[!is.na(values)]
  min_val <- min(values)
  max_val <- max(values)
  q25 <- quantile(values, 0.25)
  q75 <- quantile(values, 0.75)
  q90 <- quantile(values, 0.90)
  q95 <- quantile(values, 0.95)

  # If most values are very small (90% below 5), focus on lower range
  if(q90 < 5) {
    return(c(0, q95))  # 0 to 95th percentile
  }
  # If there's a big range but most values are low, use upper range
  else if(max_val > 50 && q75 < max_val * 0.3) {
    return(c(q25, max_val))  # 25th percentile to max
  }
  # If values are well distributed, use IQR expanded
  else {
    iqr <- q75 - q25
    return(c(max(0, q25 - 0.5*iqr), q75 + 1.5*iqr))
  }
}

USA_water_enduse_agg_wide %>%
  plot_ly(
    type = 'parcoords',
    line = list(
      color = ~year,
      colorscale = list(c(0, 'blue'), c(0.5, 'gold'), c(1, 'red')),
      colorbar = list(title = "Year", tickvals = YEARS_TO_PLOT, ticktext = YEARS_TO_PLOT),
      reversescale = TRUE,
      showscale = TRUE
    ),
    dimensions = c(
      # years dimension
      list(list(
        range = c(2020, 2100),
        label = "Year",
        values = USA_water_enduse_agg_wide$year,
        tickvals = YEARS_TO_PLOT
      )),
      # enduse dimensions with adaptive ranges
      lapply(enduse_levels, function(enduse_col) {
        if(enduse_col %in% names(USA_water_enduse_agg_wide)) {
          adaptive_range <- calculate_adaptive_range(USA_water_enduse_agg_wide[[enduse_col]])
          list(
            range = adaptive_range,
            label = paste(enduse_col, "\n(", round(adaptive_range[1],1), "-", round(adaptive_range[2],1), ")"),
            values = USA_water_enduse_agg_wide[[enduse_col]]
          )
        }
      }),
      # scenario dimension
      list(list(
        range = c(1, 8),
        label = "Scenario",
        values = as.numeric(USA_water_enduse_agg_wide$scenario),
        tickvals = 1:8,
        ticktext = levels(USA_water_enduse_agg_wide$scenario)
      ))
    )
  )

# color by year, show scenario at the end
USA_water_enduse_agg_wide %>%
  plot_ly(width = 1800, height = 700) %>%
  add_trace(
    type = 'parcoords',
    line = list(
      color = ~year,
      colorscale = list(c(0, 'blue'), c(0.5, 'gold'), c(1, 'red')),
      colorbar = list(title = "Year", tickvals = YEARS_TO_PLOT, ticktext = YEARS_TO_PLOT),
      reversescale = TRUE,
      showscale = TRUE
    ),
    dimensions = c(
      # years dimension
      list(list(
        range = c(2020, 2100),
        label = "Year",
        values = USA_water_enduse_agg_plot$year,
        tickvals = YEARS_TO_PLOT
      )),
      # enduse dimensions
      lapply(enduse_levels, function(enduse_col) {
        if(enduse_col %in% names(USA_water_enduse_agg_wide)) {
          list(
            # fix lowest end at 0
            range = c(0, max(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE)),
            # absolute min and max across whole data
            # range = c(0, max(USA_water_enduse_agg_wide %>% select(all_of(enduse_levels)) %>% unlist(), na.rm = TRUE)),
            # range for each variable
            # range = c(min(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE), max(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE)),
            # fix lower to zero, add 20 of the difference from absolute max to the max of the variable
            # range = c(0, max(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE) + 0.1 * (max(USA_water_enduse_agg_wide %>% select(all_of(enduse_levels)) %>% unlist(), na.rm = TRUE) - max(USA_water_enduse_agg_wide[[enduse_col]], na.rm = TRUE))),
            label = enduse_col,
            values = USA_water_enduse_agg_wide[[enduse_col]]
          )
        }
      }),
      # scenario dimension
      list(list(
        range = c(1, 8),
        label = "Scenario",
        values = as.numeric(USA_water_enduse_agg_wide$scenario),
        tickvals = 1:8,
        ticktext = levels(USA_water_enduse_agg_wide$scenario)
      ))
    ),
    labelfont = list(size = 12, color = 'black'),
    rangefont = list(size = 11, color = 'black', style = "italic"),
    tickfont = list(size = 10, color = 'gray')
  )

