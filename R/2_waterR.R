# water sankey with regions
#
# Hassan Niazi, April 2024

# load data and queries ----
source("./R/0_functions.R")

im3_epri <- loadProject(proj = PROJ_WATER)

listScenarios(im3_epri); listQueries(im3_epri)


# extra: create a basin resource to region mapping from the results


# prepare short and long basin names
basinID <- read_csv(paste0(data_dir, "basin_ID.csv"), skip = 6)
basin_country <- read_csv(paste0(data_dir, "basin_to_country_mapping.csv"), skip = 7) %>% select(basin_id = GCAM_basin_ID, basin_short = GLU_name)# country = country_name)

# basinID %>% left_join(basin_country, by = "basin_id")

basin_names <- getQuery(im3_epri, "water withdrawals by tech") %>% select(subsector) %>% filter(grepl("_", subsector)) %>% distinct() %>%
  separate(subsector, into = c("subsector", "basin_short"), sep = "_") %>% select(basin_short) %>%
  right_join(basin_country, by = "basin_short") %>% left_join(basinID, by = "basin_id") %>% distinct() %>% arrange(basin_name)

# TODO: fix the region in this mapping. e.g., transboundary basins
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

# electricity cooling detail
elecWaterByCooling <- getQuery(im3_epri, "elec water withdrawals by gen tech and cooling tech")



if (F) {
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


# 0: basin resources/availability ----

# NOTE: this doesn't get used as it drawfs the water diagram
totalWaterAvailability_R <- rbind(
  basinLevelAvailableRunoff %>%
    # filter_basin_resource(basin) %>% # removing the filtering from here too
    remove_water_withdrawals_string(basin) %>%
    # we may have to switch the source and target here to start from water sources and go to basins
    select(region, scenario, target = basin, source = subresource, year, value, units = Units),
  # groundwater
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

# 1: desal ----
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


# 2a: water supply type ----

df_WithdrawalsBySource_R <- rbind(desal_bybasinR,
                                  waterWithdrawalsByWaterSource %>%
                                    # filter_basin_resource() %>%
                                    remove_water_withdrawals_string() %>%
                                    select(scenario, region, source = subresource, target = resource, year, value, units = Units))

# NOTE: we are not seeing runoff or groundwater because the region column with
# them is "USA" which gets filtered out, hence we have to complete the
# state_basin combination below (df_sourcetype_byregion plots all)
if (PLOTS_ON) plot_sankey(df_WithdrawalsBySource_R, "Water Withdrawals by Basin and Water Source")


# 2b: water supply type by state ----

# the assumption here is that total demand in a region is the sum of the demand
# served by each basin each source. E.g., one state withdrawing from 2 basins
# will have 2 SW:GW splits (in total 4 values - B1sw B1gw B2sw B2gw) based on
# the SW:GW split of each basin but total water withdrawn will stay the same.

# calculate source shares (sw:gw) by region_basin (to be joined late with demand share by region-basin)
source_shares_R <- waterWithdrawalsByWaterSource %>%
  # filter_basin_resource() %>%
  remove_water_withdrawals_string() %>%
  group_by(scenario, region, resource, year) %>%
  mutate(share = value / sum(value)) %>% ungroup() %>% rename(units = Units)

# demand shares by region-basin
demand_byregionbasin <- waterWithdrawalsByStateSectorBasin %>%
  filter(!(region %in% c("USA"))) %>% # exclude USA level data because it aggregated up from basin level data, but lacks the technology
  # filter(region %in% states_conus) %>%
  filter(technology != "desalination") %>% # we have desalination at the basin level
  # TODO: check if irr needs to be removed, especially when we want to do
  # region-based sw-gw split. I think it needs to be removed based on the comment below
  replace_after_irr_string() %>% # if this is removed, each basin will supply to it's own irrigation demand
  # group_by and summarize not needed as the scen, reg, sector, tech, year,
  # grouping (esp tech because it has the basin) will take care of the
  # appropriation of water from multiple basins into one state
  # group_by(scenario, region, sector, technology, year, Units) %>%
  # summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, subregion = region, sector, technology, year, demand_withdraw = value, units = Units)

# main table assigning source type (sw, gw) for each demand type (~6) for each state-basin combination
source_byregion <- demand_byregionbasin %>%
  left_join(source_shares_R, by = c("scenario", "technology" = "resource", "year", "units")) %>%
  mutate(source_withdraw = demand_withdraw * share)


# source type to basin
# NOTE: it might seem that we already have this information from
# waterWithdrawalsByWaterSource, but apparently waterWithdrawalsByWaterSource
# doesn't have states (only region e.g., USA), so we actually don't know
# state-basin level runoff-groundwater
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

if (PLOTS_ON) plot_sankey(df_sourcetype_byregion, "Water Withdrawals by Source Type and Basin")
if (PLOTS_ON) plot_sankey(df_sourcetype_byregion, "Water Withdrawals by Source Type and Basin", reg = "CA", scen = "rcp85hotter_ssp5")

# Note: this source_byregion is generated mainly from the demand side so this
# will be conveyance factor off e.g,. California river here is 47.7 but queried
# outputs are 49.7 (the difference is coming from conveyance factor for
# irrigation which will be corrected later)

# 3: water use type ----

df_waterWithdrawalsByStateSectorBasin_R <- demand_byregionbasin %>%
  select(scenario, region = subregion, source = technology, target = sector, year, value = demand_withdraw, units)

if (PLOTS_ON) plot_sankey(df_waterWithdrawalsByStateSectorBasin_R, "Water Withdrawals by Use Category and Basin")
if (PLOTS_ON) plot_sankey(df_waterWithdrawalsByStateSectorBasin_R, "Water Withdrawals by Use Category and Basin", reg = c("CA"), yr = 2070)

# total water use by basin: include desal use and direct basin use
# NOTE: desal_usebybasinR doesn't have a "desal" value in any column - source is
# basin, target is demand type. Summing up in totalwaterusebybasin loses the
# desalination distinction. this is OK because here we are trying to capture
# desal part of demand and the explicit desal volumes will be captured from
# desal to basin (the flow is source - basin - demand - end use)
totalwaterusebybasin <- rbind(desal_usebybasinR, df_waterWithdrawalsByStateSectorBasin_R) %>%
  # group by everything but value
  group_by(scenario, region, source, target, year, units) %>%
  summarise(value = sum(value)) %>% ungroup()

if (PLOTS_ON) plot_sankey(totalwaterusebybasin, "Water Withdrawals by Use Category and Basin")
if (PLOTS_ON) plot_sankey(totalwaterusebybasin, "Water Withdrawals by Use Category and Basin", reg = c("CA"), yr = 2070) # to see desal


# let's try to piece this with waterWithdrawalsByWaterSource to the the water
# source type, basin, water use type flow

source_useR <- rbind(df_sourcetype_byregion, totalwaterusebybasin)

if (PLOTS_ON) plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use")
# if (PLOTS_ON) plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("USA"))
if (PLOTS_ON) plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("GA"))
if (PLOTS_ON) plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("CA", "NY", "MD"))
if (PLOTS_ON) plot_sankey(source_useR, "Water Withdrawals by Source, Basin, Use", reg = c("CA", "South Africa"))



# 4: water end use ----

if (F) {
  # checks to see how demands are handled
  waterWithdrawalsByStateSectorBasin %>% filter(scenario == "rcp45cooler_ssp3", year == 2020, sector == "water_td_an_W") %>%
    filter(region %in% c("USA", "AR")) # AR Akr 0.001944920 + Akr Misp 0.001762130 = 0.00370705 USA AR
  # conclusion: take states and sum over tech if need a total for an and pri, which would be equal to USA-state value
  waterWithdrawalsByStateSectorBasin %>% filter(scenario == "rcp45cooler_ssp3", year == 2020, sector == "water_td_irr_UsaCstSE_W")
  # conclusion: take usa value but reduce by x 0.8299342 to get states values. usa withdraws, states get less due to conveyance losses 0.970467/1.16933 GA = 0.8299342 conveyance loss
}

waterWithdrawalsByTech_watCat <- waterWithdrawalsByTech %>% map_water_use_to_categories() # %>% filter_CONUSregions(region, c("USA", states_conus))

# # approach # 2:
waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
  select(scenario, region, source = watcategory, target = technology, year, value, units = Units)

if (PLOTS_ON) plot_sankey(rbind(source_useR,
                  waterWithdrawalsByTech_watCat %>%
                    filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
                    select(scenario, region, source = watcategory, target = technology, year, value, units = Units)
)
, "Water Withdrawals by Source, Basin, Use", reg = c("GA"))


## livestock and mining ----
USA_an_pri <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_pri|water_td_an", watcategory)) %>%
  filter(region == "USA")
# split each row 50 ways by each state i.e., Beef Mixed FeedCrops is total US
# which needs to shared to all states. df_waterWithdrawalsByStateSectorBasin_R
# already has total state level number for water_td_an_W. by tech has tech
# details but states has state details, our goal is to have tech details at
# the state level



if (F) {
  # sector subsector tech combinations
  temp <- waterWithdrawalsByStateSectorBasin |> filter(region %in% states) |>  filter(grepl("water_td_pri|water_td_an", sector)) %>% select(region, sector, subsector, technology) %>% distinct() %>% arrange(sector, subsector, technology)
  temp2 <- USA_an_pri %>% select(sector, subsector, technology) %>% distinct() %>% arrange(sector, subsector, technology)

  # check water_td_an_W sums before disaggregation
  USA_an_pri %>% filter(watcategory == "water_td_an_W" , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value)) #
  totalwaterusebybasin %>% filter(target == "water_td_an_W", region %in% states , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value))
  USA_an_pri %>% filter(watcategory == "water_td_pri_W" , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value)) #
  df_waterWithdrawalsByStateSectorBasin_R %>% filter(target == "water_td_pri_W", region %in% states , scenario == "rcp85cooler_ssp3", year > 2015) %>% summarise(value = sum(value))
  # conclusion: if checked against df_waterWithdrawalsByStateSectorBasin_R, there
  # will be a minor excess withdrawals by tech in future years compared to state
  # basins query. This is because df_waterWithdrawalsByStateSectorBasin_R is
  # screening out desal. If compared against totalwaterusebybasin, everything
  # checks out just fine. totalwaterusebybasin should be used
}

# calculate share of each use category in each state (to be used to assign each tech from USA to each state)
state_cat_shares <- totalwaterusebybasin %>%
  filter(region %in% states) %>%
  group_by(scenario, region, target, year) %>%
  summarise(cat_total = sum(value)) %>% ungroup() %>%
  group_by(scenario, target, year) %>%
  mutate(state_cat_share = cat_total / sum(cat_total)) %>% ungroup()

# check if the shares sum to 1 e.g., sum(livestock water use over all states = 1)
sum((state_cat_shares %>% filter(scenario == "rcp45cooler_ssp3", year == 2050, target == "water_td_an_W"))$state_cat_share) == 1

# each tech use on USA level gets disaggregated to states based on the state share of category use
# e.g., if MD has 5% of livestock use, then beef will also be 5% in MD
# NOTE: splitting 50 ways could be problematic if the resource isn't found in a
# state but we are associating water use for mining it.
# TODO: Could be good to check and reconcile from the energy processing and use
# resource shares (e.g., oil in each state) to fraction out water accordingly
# instead of assuming all resources exist in all states.
# TODO: We could let animal slide for now as it's tiny, but if we do tethys
# approach for irrigation, we could also do it for livestock
USA_an_pri_tech <- USA_an_pri %>% select(-region) %>%
  left_join(state_cat_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
  mutate(state_tech = value * state_cat_share) %>%
  select(scenario, region, watcategory, sector, subsector, technology, year, value = state_tech, units = Units)

# Note: these levels will form levels of complexity on the water side
end_use_an_pri_sector <- agg_enduse(USA_an_pri_tech, "watcategory", "sector") # aggregate by sector
# end_use_an_pri_subsector <- agg_enduse(USA_an_pri_tech, "sector", "subsector") # aggregate by subsector; skipping as Mixed and Pastoral doesn't add much
end_use_an_pri_tech <- agg_enduse(USA_an_pri_tech, "sector", "technology") # aggregate by technology

# rbinding to keep one layer downstream
end_use_an_pri <- rbind(end_use_an_pri_sector, end_use_an_pri_tech) %>% distinct() %>% remove_source_equals_target()

if (PLOTS_ON) plot_sankey(rbind(source_useR, end_use_an_pri), "Water Withdrawals by Source, Basin, Use", reg = c("CA"))

## irrigation ----
USA_irr <- waterWithdrawalsByTech_watCat %>% filter(grepl("water_td_irr", watcategory)) %>%
  filter(region == "USA") %>%
  group_by(scenario, watcategory, sector, subsector, year, units = Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  separate(subsector, into = c("subsector", "basin_short"), sep = "_") #%>%
#left_join_error_no_match(basin_reg_map %>% select(basin_short, basin) %>% distinct(), by = "basin_short")

# NOTE: joining all 50 states to irrigation essentially means all crops are
# grown in all states. This could be problematic. e.g., if Alaska also grown
# Corn etc. Now two reasons that this is OK for now:
# 1. the irrigation water for AK might be tiny anyway so it might not matter but
# for state level results or working with stakeholders it could be problematic.
# 2. Another reason to downplay this is that even though ag in gcam-usa is at
# the USA region level, it still have the basin level signal e.g, Corn_ArkWht
# etc, which are basin specific, so Alaska corn will be lower anyway (if it is
# meant to be lower) because of Corn_PacArctic being lower.
# TODO: Possible solution: For irrigation we could pull in tethys results,
# aggregate irrigation withdrawals by state(-basin?) and either just use those
# tables or use some sort of shares to determine crop level water use by
# state-basin and avoid uniform crop representation across all states
# USA_irr_state <- USA_irr %>%
#   left_join(basin_reg_map %>% select(subregion, basin_short, basin) %>% distinct(), by = "basin_short")

# USA_irr %>% remove_source_equals_target("sector", "subsector")
# unique((USA_irr %>% remove_source_equals_target("sector", "subsector"))$subsector)

USA_irr_state_crop <- USA_irr %>%
  left_join(state_cat_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
  mutate(state_subsec = value * state_cat_share) %>%
  select(scenario, region, watcategory, sector, subsector, year, value = state_subsec, units)

end_use_irr <- USA_irr_state_crop %>%
  group_by(scenario, region, watcategory, subsector, year, units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = watcategory, target = subsector, year, value, units)


if (F) {
  # TODO: this is faulty I think. state_sector_use_shares calculate share each
  # category in a state, which we still have to figure out how to use.

  # approach # 1
  # get all 6 categories: irr pri and an from USA and the rest from states
  # take sector shares of water use for each state and apply to technology in the USA region

  # Remember we are now in the end-use domain, so by tech queries are the baseline
  # alongside the tables used to calculate shares used to assign end-use to each
  # state-basin combination. The goal is to get each end use demand by state and basin.
  state_sector_use_shares <- totalwaterusebybasin %>%
    filter(region %in% states) %>%
    # collapse the water source e.g., all basins for irrigation in CA gets summed up
    group_by(scenario, region, target, year) %>%
    summarise(sector_total = sum(value)) %>% ungroup() %>%
    group_by(scenario, region, year) %>%
    # each state's water use category share (sum of 6 categories = 1)
    mutate(sector_share = sector_total/sum(sector_total)) %>% ungroup()

  # check: sum of all uses in a state = 1
  isTRUE(all.equal(sum((state_sector_use_shares %>% filter(scenario == "rcp45cooler_ssp3", year == 2050, region == "CA"))$sector_share), 1))

  # if irrigation in CA is X of all uses (e.g., 40%), then Corn (or whichever tech), will be 40% of the US.
  # Doesn't make sense!
  waterWithdrawalsByTech_disagg <- waterWithdrawalsByTech_watCat %>%
    filter(region == "USA") %>% select(-region) %>%
    left_join(state_sector_use_shares, by = c("scenario", "year", "watcategory" = "target")) %>%
    mutate(tech_disagg = value * sector_share)

  temp3 <- waterWithdrawalsByTech_disagg |> filter(region %in% states) %>% select(sector, subsector, technology) %>% distinct() %>% arrange(sector, subsector, technology)
  temp3

  watcategory_sector_use_R <- waterWithdrawalsByTech_disagg %>% #waterWithdrawalsByTech_watCat
    group_by(scenario, region, technology, watcategory, year, Units) %>%
    summarise(value = sum(tech_disagg)) %>% ungroup() %>%
    select(scenario, region, source = watcategory, target = technology, year, value, units = Units)

  # if (PLOTS_ON) plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("USA"))
  if (PLOTS_ON) plot_sankey(rbind(source_useR, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("CA"))

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

  if (PLOTS_ON) plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R, watcategory_subsector_use_R), "Water Withdrawals by Source, Basin, Use")

  if (PLOTS_ON) plot_sankey(rbind(df_sourcetype_byregion, totalwaterusebybasin, watcategory_sector_use_R), "Water Withdrawals by Source, Basin, Use", reg = c("CA"))
}


# PART 2: SANKEY ASSEMBLY ----
# three levels of information
#   level 1: source type to use category
#   level 2: source type to basin to use category
#   level 3: source type to basin to use category to end-use technology

# alias processing outputs to layer names for the sankey assembly
layer1_source_to_basin   <- df_sourcetype_byregion
layer2_basin_to_category <- totalwaterusebybasin

# Elec dom ind: electricity tech detail from the waterWithdrawalsByTech query (state/grid level)
layer3_elec_dom_ind <- waterWithdrawalsByTech_watCat %>%
  filter(grepl("water_td_elec|water_td_dom|water_td_ind", watcategory)) %>%
  filter(region %in% c(states)) %>%
  group_by(scenario, region, watcategory, technology, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = watcategory, target = technology, year, value, units = Units)

# An pri: livestock and mining end-use detail
layer3_an_pri <- end_use_an_pri
# Irr: irrigation crop detail
layer3_irr <- end_use_irr


# irrigation conveyance losses ----
# GCAM applies a conveyance efficiency factor between basin withdrawal and
# field delivery. The withdrawal query reports gross, but the downstream
# technology query reports net (field-level). The gap is conveyance loss.
# For the US (GCAM region 1) the factor is ~0.83.
ag_irr_eff <- read_csv(IRR_EFF_FILE)
USA_CONV_EFF <- ag_irr_eff$conveyance.eff[ag_irr_eff$GCAM_region_ID == 1]

# conveyance loss flow: attached to irrigation category node
# the withdrawal side (layer2) reports gross, the tech side (layer3) sees net
# difference = gross × (1 - conveyance_eff)
irr_gross <- layer2_basin_to_category %>%
  filter(grepl("water_td_irr", target)) %>%
  group_by(scenario, region, target, year, units) %>%
  summarise(gross = sum(value)) %>% ungroup()

irr_conveyance_loss <- irr_gross %>%
  mutate(value = gross * (1 - USA_CONV_EFF),
         source = target,
         target = "Conveyance Losses") %>%
  select(scenario, region, source, target, year, value, units)


# downstream detail for municipal and industrial ----
# the "water withdrawals by tech" query for domestic has "municipal water" as
# sector and subsector. for industrial, sector = "industry" with no further
# breakdown. we can use energy-side IO ratios to split municipal into
# residential vs commercial, and industrial into subsectors.
# for now, split municipal into residential (60%) and commercial (40%) based on
# typical US public supply distribution (USGS circular 1441).
# industrial: keep "other industry", "paper industry", "H2 production" if present

layer3_dom_detail <- waterWithdrawalsByTech_watCat %>%
  filter(watcategory == "water_td_dom_W") %>%
  filter(region %in% c(states)) %>%
  group_by(scenario, region, watcategory, sector, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  # municipal water splits roughly 60/40 residential/commercial (USGS circ 1441)
  # TODO: this split could be made based on residential vs commercial buildings energy split
  cross_join(enframe(MUNICIPAL_SPLIT, name = "target", value = "split")) %>%
  transmute(scenario, region, source = sector, target, year, value = value * split, units = Units)

# this won't do much for IM3 GCAM-USA as it doesn't have detailed industry
layer3_ind_detail <- waterWithdrawalsByTech_watCat %>%
  filter(watcategory == "water_td_ind_W") %>%
  filter(region %in% c(states)) %>%
  group_by(scenario, region, watcategory, sector, technology, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = sector, target = technology, year, value, units = Units)


# electricity cooling detail ----
# connect gen tech to its cooling variant without creating detached nodes.
# the elec_water query has subsector = gen tech name (e.g. "coal (conv pul)")
# and technology = "coal (conv pul) (once through)" etc.
# we keep the full cooling tech string and connect FROM the gen tech sector name
# (which is already a target in layer3_elec) TO the full cooling string.

layer3b_elec_cooling <- elecWaterByCooling %>%
  filter(region %in% c(states)) %>%
  group_by(scenario, region, subsector, technology, year, Units) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  select(scenario, region, source = subsector, target = technology, year, value, units = Units)


# assemble levels ----

if (F) {
  # TODO: probably remove as it has limited use (source to use without basins)
  # level 1: source type to use category (no basin detail)
  # split each category's total by source type using basin-level sw:gw shares
  water_L1_src <- layer1_source_to_basin %>%
    filter(!grepl("desalination", source)) %>%
    group_by(scenario, region, source, year) %>%
    summarise(src_total = sum(value)) %>% ungroup() %>%
    group_by(scenario, region, year) %>%
    mutate(src_share = src_total / sum(src_total)) %>% ungroup()

  water_L1_cat <- layer2_basin_to_category %>%
    group_by(scenario, region, target, year, units) %>%
    summarise(value = sum(value)) %>% ungroup()

  water_L1 <- water_L1_cat %>%
    left_join(water_L1_src %>% select(scenario, region, source, year, src_share),
              by = c("scenario", "region", "year"),
              relationship = "many-to-many") %>%
    mutate(value = value * src_share) %>%
    select(scenario, region, source, target, year, value, units) %>%
    bind_rows(
      # add desalination as a separate source
      desal_bybasinR %>%
        group_by(scenario, region, source, year, units) %>%
        summarise(value = sum(value)) %>% ungroup() %>%
        left_join(water_L1_cat %>% group_by(scenario, region, year) %>%
                    mutate(cat_share = value / sum(value)) %>% ungroup() %>%
                    select(scenario, region, target, year, cat_share),
                  by = c("scenario", "region", "year"),
                  relationship = "many-to-many") %>%
        mutate(value = value * cat_share) %>%
        select(scenario, region, source, target, year, value, units)
    ) %>%
    filter(!is.na(value), value > 0)
}

# every level has the same source type -> basin -> use category core df and
# the levels differ only in how far the end uses are resolved. An end use that
# is also a node of the energy diagram is never aggregated past that node, so
# water level k joins energy level k (4_energy_water.R):
#   level 1: power plant | fuel | municipal | industry | 2 crop groups
#   level 2: level 1 + residential/commercial, livestock type, 6 crop groups
#   level 3: power plant with cooling system | GCAM technology | every crop
water_core <- bind_rows(layer1_source_to_basin, layer2_basin_to_category)

# electricity: plant with its cooling system, and plant only
enduse_elec_cooling <- layer3_elec_dom_ind %>%
  filter(source == "water_td_elec_W") %>%
  clean_cooling_tech_years("target") %>% sum_flows()
enduse_elec_plant <- enduse_elec_cooling %>% clean_cooling_tech("target") %>% sum_flows()

# municipal and industry: category to its single sector
enduse_dom <- layer3_elec_dom_ind %>% filter(source == "water_td_dom_W")
enduse_ind <- layer3_elec_dom_ind %>% filter(source == "water_td_ind_W")

# irrigation: category to crop group (crops are net of conveyance)
enduse_irr_L1 <- layer3_irr %>%
  mutate(target = if_else(target %in% BIOENERGY_CROPS, "Bioenergy Crops", "Crops")) %>%
  sum_flows()
enduse_irr_L2 <- layer3_irr %>%
  mutate(target = coalesce(unname(CROP_GROUPS[target]), "Other Crops")) %>%
  sum_flows()

# level 1: source type, basin, use category, coarse end use
water_L1 <- bind_rows(water_core, enduse_elec_plant, enduse_dom, enduse_ind,
                      end_use_an_pri_sector %>% filter(source == "water_td_pri_W"),
                      enduse_irr_L1, irr_conveyance_loss) %>%
  filter(value > 0)

# level 2: source type, basin, use category, aggregated end use
water_L2 <- bind_rows(water_core, enduse_elec_plant, enduse_ind,
                      layer3_dom_detail %>% mutate(source = "water_td_dom_W"),
                      end_use_an_pri_sector,
                      enduse_irr_L2, irr_conveyance_loss) %>%
  filter(value > 0)

# level 3: source type, basin, use category, end-use technology
water_L3 <- bind_rows(water_core, enduse_elec_cooling, enduse_ind,
                      # municipal: sector to residential/commercial split
                      enduse_dom, layer3_dom_detail,
                      # livestock and mining: category to sector to technology
                      layer3_an_pri,
                      # irrigation: category to crop
                      layer3_irr, irr_conveyance_loss) %>%
  filter(value > 0)


# processed table for the dashboard ----
# states, future years and the scenarios kept on the energy side
water_levels <- bind_rows("1" = water_L1, "2" = water_L2, "3" = water_L3, .id = "level") %>%
  filter(region %in% states, year %in% FUTURE_YEARS, grepl("hotter", scenario)) %>%
  rename_water_use_categories() %>% rename_water_source() %>%
  mutate(level = as.integer(level),
         year = as.integer(year),
         value = round(value, 6),
         ssp = str_extract(scenario, "[^_]+$"),
         rcp = str_extract(scenario,  "^.*?\\d{2}"),
         color = node_color(source),
         diagram = "Water") %>%
  filter(value > 0) %>%
  select(scenario, ssp, rcp, region, year, source, target, level, value, color, units, diagram)

if (UPDATE_DATA) write_parquet(water_levels, out_parquet("water"))

# plotting wrapper ----

plot_water_sankey <- function(reg = "GA", scen = "rcp45cooler_ssp3", yr = 2050,
                              level = 3) {
  df <- switch(as.character(level),
               "1" = water_L1,
               "2" = water_L2,
               "3" = water_L3)
  plot_sankey(df, title = paste0("Water Sankey, Level ", level),
              scen = scen, yr = yr, reg = reg)
}

# example plots ----
if (PLOTS_ON %||% TRUE) {
  plot_water_sankey(c("GA", "CA"), level = 2)
  plot_water_sankey(c("GA", "CA"), level = 3)
  plot_water_sankey(c("AZ", "CA"), level = 3)
  plot_water_sankey(c("MD", "VA", "NJ", "DC", "DE", "PA"), level = 3)
  plot_water_sankey(states_conus, level = 1)
  plot_water_sankey(states_conus, level = 2)
  plot_water_sankey(states_conus, level = 3)
}


# save ----
if (SAVE_FILES) {
  htmlwidgets::saveWidget(
    plot_water_sankey("GA"), selfcontained = TRUE,
    file = paste0(output_dir, "sankey_water_statebasin_GA_2050.html"))
  htmlwidgets::saveWidget(
    plot_water_sankey(states_conus, level = 2), selfcontained = TRUE,
    file = paste0(output_dir, "sankey_water_statebasin_conus_L2_2050.html"))
}


# TODOS ----
# TODO:
# - levels: water level and in sync with energy levels
# - possibly have water only levels and energy-water levels. Same goes for energy.
#    Maybe L1 or L3 of water and energy could be very simple and very detailed but the L1 and L3 of combined energy-water could be slightly detailed and slightly simple than their water and energy equivalents.
# - water parquet files with levels
# - energy sync: bring in energy info to do the water breakouts e.g., commercial and residential water, or the fossil fuels by states. the script has comments on where that could be done.
#    e.g, GA has no Oil node on the energy side, but the water script splits national mining water across all states, so a small water-only Oil node appears there.
# - hydropower: handle hydropower water
# - consumption: bring in water consumption queries to inform consumption and discharge for nodes
# - revisit irrigation just in case (looks good but still e.g., bringing data from tethys to ensure crops are in their specific states)
#
# energy: some details got aggregated up - we need to bring those back
# overall: merge the .dat files into one for both energy and water

























# archive #####################################################################
if (F) {
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

}
