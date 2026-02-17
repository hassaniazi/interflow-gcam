# implement water source split by end use for each basin
#

source("./R/0_functions.R")

# im3scen_water <- query_im3_scen("water")
# read water withdrawals by water source (runoff vs. groundwater).csv from data dir
waterWithdrawBySource <- read_csv(paste0(data_dir, "im3scen_water/water withdrawals by water source (runoff vs. groundwater).csv.gz")) %>%
  filter(scenario == "rcp45hotter_ssp3") %>%
  # filter(region == "USA") %>%
  mutate(basin = gsub("_.*", "", resource)) %>%
  select(scenario, region, basin, resource, subresource, year, value, Units)



# plot: facet by basin, x= year, y = value, color = scenario, linetype = subresource
waterWithdrawBySource %>% # filter(basin != "Pacific and Arctic Coast") %>%
  filter(year > 1975) %>%
  ggplot(aes(x = year, y = value, color = scenario, linetype = subresource)) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~basin, nrow = 3, scales = "free_y") + # scales = "free_y"
  # scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = c("#A6CEE3", "#1F78B4",  "#CAB2D6", "#6A3D9A", "#FB9A99", "#E31A1C", "#FFFF99", "#B15928")) +
  # "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
  labs(#title = "Water Withdrawals by Water Source (Runoff vs. Groundwater) and IM3 Scenario",
       x = "Year", y = "Volume withdrawn (km3/year)", color = "Scenario", linetype = "Water source") +
  theme_minimal() + theme(legend.position = "bottom",
                          # strip.text = element_text(face = "bold"),
                          panel.border = element_rect(color = "grey90", fill = NA),
                          axis.text = element_text(face = "bold"), axis.title = element_text(face = "bold")
                          )

# save the plot
# ggsave(paste0(img_dir, "waterWithdrawBySource_freey.jpg"), width = 16, height = 9)
# ggsave(paste0(img_dir, "waterWithdrawBySource.jpg"), width = 16, height = 9)


# calculate groundwater to runoff share
waterWithdrawBySource_shares <- waterWithdrawBySource %>%
  group_by(scenario, region, basin, resource, year) %>% mutate(share = value / sum(value)) %>% ungroup()

# write_csv(waterWithdrawBySource_shares, paste0(data_dir, "waterWithdrawBySource_shares.csv"))

# plot shares: facet by basin, x= year, y = share, color = scenario, linetype = subresource
waterWithdrawBySource_shares %>%
  filter(year > 1975) %>%
  ggplot(aes(x = year, y = share, color = scenario, linetype = subresource)) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~basin, nrow = 3, scales = "free_y") + # scales = "free_y"
  # scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = c("#A6CEE3", "#1F78B4",  "#CAB2D6", "#6A3D9A", "#FB9A99", "#E31A1C", "#FFFF99", "#B15928")) +
  # "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
  labs(#title = "Groundwater to Runoff Share by Basin and IM3 Scenario",
       x = "Year", y = "Share of groundwater and runoff in total water withdrawals", color = "Scenario", linetype = "Water source") +
  theme_minimal() + theme(legend.position = "bottom",
                          # strip.text = element_text(face = "bold"),
                          panel.border = element_rect(color = "grey90", fill = NA),
                          axis.text = element_text(face = "bold"), axis.title = element_text(face = "bold")
                          )

# ggsave(paste0(img_dir, "waterWithdrawBySource_shares.jpg"), width = 16, height = 9)

# interpolate withdrawals over years and recalcuate shares
waterWithdrawBySource_shares_yearly <- waterWithdrawBySource %>%
  group_by(scenario, region, basin, resource, subresource, Units) %>%
  complete(year = min(year):max(year)) %>%
  mutate(value = ifelse(is.na(value), approx(year, value, xout = year)$y, value)) %>% ungroup() %>%
  group_by(scenario, region, basin, resource, year) %>% mutate(share = value / sum(value)) %>% ungroup()

# write_csv(waterWithdrawBySource_shares_yearly, paste0(data_dir, "waterWithdrawBySource_shares_yearly.csv"))
# write_csv(waterWithdrawBySource_shares_yearly, paste0(data_dir, "waterWithdrawBySource_shares_yearly.csv.gz"))

# plot shares: facet by basin, x= year, y = share, color = scenario, linetype = subresource
waterWithdrawBySource_shares_yearly %>%
  filter(year >= 1990) %>%
  ggplot(aes(x = year, y = share, color = scenario, linetype = subresource)) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~basin, nrow = 3, scales = "free_y") + # scales = "free_y"
  # scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = c("#A6CEE3", "#1F78B4",  "#CAB2D6", "#6A3D9A", "#FB9A99", "#E31A1C", "#FFFF99", "#B15928")) +
  # "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
  labs(#title = "Groundwater to Runoff Share by Basin and IM3 Scenario",
       x = "Year", y = "Share of groundwater and runoff in total water withdrawals", color = "Scenario", linetype = "Water source") +
  theme_minimal() + theme(legend.position = "bottom",
                          # strip.text = element_text(face = "bold"),
                          panel.border = element_rect(color = "grey90", fill = NA),
                          axis.text = element_text(face = "bold"), axis.title = element_text(face = "bold")
                          )

# ggsave(paste0(img_dir, "waterWithdrawBySource_shares_yearly.jpg"), width = 16, height = 9)


## processing using manually collected queries
# read these files
{
  water_usa_withdrawbysource <- read_csv(paste0(data_dir, "water_usa_withdrawbysource.csv")) %>% gather_years() %>%
    mutate(basin = gsub("_.*", "", resource)) %>% select(-scenario)

  water_usa_usebycategory <- read_csv(paste0(data_dir, "water_usa_usebycategory.csv")) %>% gather_years() %>%
    mutate(basin = ifelse(grepl("irr_", input), gsub(".*irr_", "", input), gsub("_.*", "", input)),
           basin = ifelse(grepl("_W", basin), gsub("_W$", "", basin), "all")) %>% select(-scenario)
}

# sum all groundwater grades
water_usa_withdrawbysource_GW <- water_usa_withdrawbysource %>%
  mutate(subresource = ifelse(grepl("groundwater", subresource), "groundwater", subresource)) %>%
  group_by(resource, subresource, basin, year) %>% summarise(value = sum(value)) %>% ungroup()

# calculate runoff to groundwater share for each basin and year
water_usa_withdrawbysource_GWSWshare <- water_usa_withdrawbysource_GW %>%
  group_by(resource, year) %>% mutate(share = value / sum(value)) %>% ungroup() %>% drop_na(share)

# calculate runoff to groundwater share for each basin and year
water_usa_withdrawbysource_GWSWshare_USA <- water_usa_withdrawbysource_GW %>%
  group_by(subresource, year) %>% mutate(share = value / sum(value)) %>% ungroup() %>% drop_na(share)


water_usa_withdrawbysource_GWSWshare <- water_usa_withdrawbysource %>%
  # sum all groundwater grades
  mutate(subresource = ifelse(grepl("groundwater", subresource), "groundwater", subresource)) %>%
  group_by(resource, subresource, basin, year) %>% summarise(value = sum(value)) %>% ungroup() %>%
  # calculate share of groundwater in total runoff
  group_by(resource, year) %>% mutate(share = value / sum(value)) %>% ungroup() %>% drop_na(share)


water_usa_usebycategory %>% left_join(water_usa_withdrawbysource_GWSWshare, by = c("basin", "year")) %>%
  mutate(source_use_W = value.x * share) -> a
