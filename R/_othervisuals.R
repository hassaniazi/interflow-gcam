# plotting for the "other visualizations" page in the sankey dashboard
#
# Hassan Niazi, Jan 2025


library(tidyverse)

# read C:\Users\niaz981\OneDrive - PNNL\PNNL\Projects\Interflow\im3_sankey_dashboard\sankey_dashboard\static\energy_water_data.csv
sankeystatic <- "C:/Users/niaz981/OneDrive - PNNL/PNNL/Projects/Interflow/im3_sankey_dashboard/sankey_dashboard/static/"

correct_scenario_name <- function(df, scenario) {
  mutate(df, scenario = str_replace_all(scenario, "rcp45", "RCP4.5")) %>%
    mutate(scenario = str_replace_all(scenario, "rcp85", "RCP8.5")) %>%
    mutate(scenario = str_replace_all(scenario, "cooler", " Cooler")) %>%
    mutate(scenario = str_replace_all(scenario, "hotter", " Hotter")) %>%
    mutate(scenario = str_replace_all(scenario, "ssp", "SSP")) %>%
    mutate(scenario = str_replace_all(scenario, "_", " "))

}

# mutate(scenario = str_replace_all(scenario, "rcp45", "RCP4.5")) %>%
#   mutate(scenario = str_replace_all(scenario, "rcp85", "RCP8.5")) %>%
#   mutate(scenario = str_replace_all(scenario, "cooler", " Cooler")) %>%
#   mutate(scenario = str_replace_all(scenario, "hotter", " Hotter")) %>%
#   mutate(scenario = str_replace_all(scenario, "ssp", "SSP")) %>%
#   mutate(scenario = str_replace_all(scenario, "_", " "))

water_data <- read_csv(paste0(sankeystatic, "energy_water_data.csv")) %>%
  filter(diagram == "Water") %>% correct_scenario_name()

energy_water_data <- read_csv(paste0(sankeystatic, "energy_water_data.csv")) %>%
  filter(diagram == "Energy & Water") %>% correct_scenario_name()


# > head(energy_water_data)
# # A tibble: 6 x 13
# scenario         source  target         year     value units diagram        rcp   ssp   climate_sensitivity target_color source_color link_color
# <chr>            <chr>   <chr>         <dbl>     <dbl> <chr> <chr>          <chr> <chr> <chr>               <chr>        <chr>        <chr>
#   1 rcp45cooler_ssp3 Biomass Commercial     2020 0.137     EJ    Energy & Water rcp45 ssp3  cooler              #E9D8A5      #EE7618      rgba(238, 118, 24, 0.6)
# 2 rcp45cooler_ssp3 Biomass Electricity    2020 0.0000641 EJ    Energy & Water rcp45 ssp3  cooler              #FCCF12      #EE7618      rgba(238, 118, 24, 0.6)
# 3 rcp45cooler_ssp3 Biomass Gaseous Fuels  2020 0.383     EJ    Energy & Water rcp45 ssp3  cooler              #EE7618      #EE7618      rgba(238, 118, 24, 0.6)
# 4 rcp45cooler_ssp3 Biomass Hydrogen       2020 0.00116   EJ    Energy & Water rcp45 ssp3  cooler              #EE7618      #EE7618      rgba(238, 118, 24, 0.6)
# 5 rcp45cooler_ssp3 Biomass Industry       2020 1.66      EJ    Energy & Water rcp45 ssp3  cooler              #E9D8A5      #EE7618      rgba(238, 118, 24, 0.6)
# 6 rcp45cooler_ssp3 Biomass Liquid Fuels   2020 1.83      EJ    Energy & Water rcp45 ssp3  cooler              #EE7618      #EE7618      rgba(238, 118, 24, 0.6)


# attempt for a really nice theme
othervisualstheme <- theme_minimal() + theme(
  # titles and text
  plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#333333"),
  axis.title = element_text(face = "bold", color = "#333333"),
  legend.title = element_text(face = "bold", color = "gray1"),
  legend.text = element_text(face = "bold", size = 10),
  legend.position = "top",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),  # light outline box
  # axis.line = element_line(colour = "black"),  # Black axis lines for contrast
  axis.text = element_text(color = "gray10"),
  axis.title.x = element_blank(),
  axis.ticks = element_line(colour = "gray20"),
  strip.text = element_text(face = "bold")
)


# each plot will have 8 panels, one for each scenario, x time, y value. color and line type may vary.

# 1. all basins: target == 	Surface Water Groundwater Desalination

basin_palette_dry <- c(
  "New England" = "#5E81AC",          # Light blue
  "Mid Atlantic" = "#81A1C1",         # Sky blue
  "South Atlantic Gulf" = "#A3BE8C",  # Sage green
  "Caribbean" = "#88C0D0",            # Teal
  "Tennessee River" = "#B48EAD",      # Lilac
  "Ohio River" = "#EBCB8B",           # Yellow-orange
  "Arkansas White Red" = "#D08770",   # Salmon
  "Upper Mississippi" = "#BF616A",    # Red
  "Lower Mississippi River" = "#A3A3A3", # Gray
  "Missouri River" = "#8FBCBB",       # Aqua
  "Great River" = "#E5E9F0",          # Soft white-blue
  "Lower Colorado River" = "#D08770", # Earthy orange
  "Upper Colorado River" = "#88C0D0", # Deep teal
  "Texas Gulf Coast" = "#D8DEE9",     # Soft gray
  "Rio Grande River" = "#5E81AC",     # Navy blue
  "Pacific Northwest" = "#2E3440",    # Dark gray
  "California River" = "#3B4252"      # Charcoal
)

basin_palette_bright <- c(
  "New England" = "#A6CEE3",          # Light sky blue
  "Mid Atlantic" = "#1F78B4",         # Bright blue
  "South Atlantic Gulf" = "#33A02C",  # Vibrant green
  "Caribbean" = "#B2DF8A",            # Pastel green
  "Tennessee River" = "#FB9A99",      # Light pink
  "Ohio River" = "#E31A1C",           # Bright red
  "Arkansas White Red" = "#FDBF6F",   # Light orange
  "Upper Mississippi" = "#FF7F00",    # Bright orange
  "Lower Mississippi River" = "#CAB2D6", # Lavender
  "Missouri River" = "#6A3D9A",       # Rich purple
  "Great River" = "#FFFF99",          # Soft yellow
  "Lower Colorado River" = "#B15928", # Brown
  "Upper Colorado River" = "#FFED6F", # Light gold
  "Texas Gulf Coast" = "#F4A582",     # Peach
  "Rio Grande River" = "#D73027",     # Fiery red-orange
  "Pacific Northwest" = "#4575B4",    # Steel blue
  "California River" = "#313695"      # Deep navy blue
)

basin_palette <- c(
  "New England" = "#D4E4FF",          # Soft blue
  "Mid Atlantic" = "#B0D0FF",         # Light blue
  "South Atlantic Gulf" = "#8BBCFF",  # Sky blue
  "Caribbean" = "#65A8FF",            # Bright sky blue
  "Tennessee River" = "#4B93E6",      # Light indigo
  "Ohio River" = "#347FDB",           # Medium indigo
  "Arkansas White Red" = "#2A6ABF",   # Deep indigo
  "Upper Mississippi" = "#1F579E",    # Rich navy
  "Lower Mississippi River" = "#14437E", # Dark navy
  "Missouri River" = "#0A305E",       # Deep blue-green
  "Great River" = "#EAF4D3",          # Pale green
  "Lower Colorado River" = "#C2D9A3", # Soft olive
  "Upper Colorado River" = "#9ABF73", # Light sage
  "Texas Gulf Coast" = "#73944C",     # Olive green
  "Rio Grande River" = "#4C7025",     # Forest green
  "Pacific Northwest" = "#294D0E",    # Dark green
  "California River" = "#152A06"      # Deep forest green
)

basin_palette_seq <- c(
  "New England" = "#0A305E",          # Soft blue
  "Mid Atlantic" = "#14437E",         # Light blue
  "South Atlantic Gulf" = "#1F579E",  # Sky blue
  "Caribbean" = "#2A6ABF",            # Bright sky blue
  "Tennessee River" = "#347FDB",      # Light indigo
  "Ohio River" = "#4B93E6",           # Medium indigo
  "Arkansas White Red" = "#65A8FF",   # Deep indigo
  "Upper Mississippi" = "#8BBCFF",    # Rich navy
  "Lower Mississippi River" = "#B0D0FF", # Dark navy
  "Missouri River" = "#D4E4FF",       # Deep blue-green
  "Great River" = "#EAF4D3",          # Pale green
  "Lower Colorado River" = "#C2D9A3", # Soft olive
  "Upper Colorado River" = "#9ABF73", # Light sage
  "Texas Gulf Coast" = "#73944C",     # Olive green
  "Rio Grande River" = "#4C7025",     # Forest green
  "Pacific Northwest" = "#294D0E",    # Dark green
  "California River" = "#152A06"      # Deep forest green
)

basin_palette_reversed <- setNames(rev(basin_palette), names(basin_palette))

water_data$source <- factor(water_data$source, levels = names(basin_palette))

# water_data %>%
#   filter(target %in% c("Surface Water", "Groundwater", "Desalination")) %>%
#   group_by(scenario, year, source) %>%
#   summarise(value = sum(value)) %>%
#   ggplot(aes(x = year, y = value, color = source, shape = source)) +
#   geom_line(alpha = 0.35, size = 0.75) +
#   geom_point() +
#   scale_color_manual(values = basin_palette) +
#   facet_wrap(~scenario, ncol = 4) +
#   labs(y = expression(paste("Water Availability (", km^3, ")")), color = "", shape = "") +
#   othervisualstheme

# create a stacked bar chart
water_data %>%
  filter(target %in% c("Surface Water", "Groundwater", "Desalination")) %>%
  group_by(scenario, year, source) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  ggplot(aes(x = year, y = value, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~scenario, ncol = 4) +
  labs(x = "Year", y = expression(paste("Water Availability (", km^3, ")")), fill = "") +
  othervisualstheme +
  coord_polar(theta = "y") +
  theme(axis.title = element_text(color = "gray10", face = "plain"), axis.title.x = element_text(face = "bold")) +
  scale_fill_manual(values = basin_palette_seq)

# ggsave(paste0(sankeystatic, "water_basins.jpg"), width = 16, height = 8, dpi = 300, units = "in")



# 2. water availability: source == 	Surface Water Groundwater Desalination
energy_water_data %>%
  filter(source %in% c("Surface Water", "Groundwater", "Desalination")) %>%
  group_by(scenario, year, source) %>%
  summarise(value = sum(value)) %>%
  mutate(source = factor(source, levels = c("Surface Water", "Groundwater", "Desalination"))) %>%
  ggplot(aes(x = year, y = value, color = source, shape = source)) +
  geom_line(alpha = 0.35, size = 0.75) +
  geom_point() +
  scale_color_manual(values = c("#00BFFF", "#FF8C00", "#6A3D9A")) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = expression(paste("Water Availability (", km^3, ")")), color = "", shape = "") +
  othervisualstheme

# area chart with gradient
energy_water_data %>%
  filter(source %in% c("Surface Water", "Groundwater", "Desalination")) %>%
  group_by(scenario, year, source) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  mutate(source = factor(source, levels = c("Surface Water", "Groundwater", "Desalination"))) %>%
  ggplot(aes(x = year, y = value, fill = source, color = source, shape = source)) +
  geom_area() +
  geom_line(alpha = 0.5, size = 0.75) + geom_point() +
  facet_wrap(~scenario, ncol = 4) +
  labs(x = "Year", y = expression(paste("Water Availability (", km^3, ")")), fill = "", color = "", shape = "") +
  othervisualstheme +
  scale_color_manual(values = c("black", "white", "gold")) +
  scale_fill_manual(values = c("#10BFFF", "#FF8C00", "#6A3D9A"))


ggsave(paste0(sankeystatic, "water_availability.svg"), width = 12, height = 6, dpi = 300)


# 3. water demands target == Livestock Water Use Domestic Water Use	Electricity Water Use	Industry Water Use	Irrigation Water Use	Mining Water Use
# water use colors
water_colors <- # get all unique colors of all uses
  energy_water_data %>%
  filter(target %in% c("Livestock Water Use", "Domestic Water Use", "Electricity Water Use", "Industry Water Use", "Irrigation Water Use", "Mining Water Use")) %>%
  filter(scenario == "RCP4.5 Cooler SSP3") %>%
  filter(year == 2020) %>%
  filter(source == "Surface Water") %>%
  select(target, target_color)

energy_water_data %>%
  filter(target %in% c("Livestock Water Use", "Domestic Water Use", "Electricity Water Use", "Industry Water Use", "Irrigation Water Use", "Mining Water Use")) %>%
  mutate(target = gsub(" Water Use", "", target)) %>%
  group_by(scenario, year, target) %>%
  summarise(value = sum(value)) %>%
  mutate(target = factor(target, levels = c("Irrigation", "Domestic", "Electricity", "Industry", "Mining", "Livestock"))) %>%
  ggplot(aes(x = year, y = value, color = target, shape = target)) +
  geom_line(alpha = 0.35, size = 0.75) +
  geom_point() +
  scale_color_manual(values = c("#228B22", "#FF8C00", "#6A3D9A", "#FFD700", "#FF4500", "#8B4513")) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = expression(paste("Water Use (", km^3, ")")), color = "Water Use:", shape = "Water Use:") +
  othervisualstheme

ggsave(paste0(sankeystatic, "water_demands.svg"), width = 12, height = 6, dpi = 300)

# 4 water uses by sector: source == Livestock Water Use Domestic Water Use	Electricity Water Use	Industry Water Use	Irrigation Water Use	Mining Water Use
waterusebysectorpalette <- c(
  "Biomass" = "#006400",        # Dark green
  "Coal" = "#333333",           # Charcoal gray
  "Crops" = "#8B4513",          # Saddle brown
  "Gaseous Fuels" = "#00CED1",  # Dark turquoise
  "Geothermal" = "#CD853F",     # Peru
  "Hydropower" = "#4682B4",     # Steel blue
  "Industry" = "#708090",       # Slate gray
  "Liquid Fuels" = "#B22222",   # Firebrick
  "Livestock" = "#8B0000",      # Dark red
  "Natural Gas" = "#1E90FF",    # Dodger blue
  "Nuclear" = "#FFD700",        # Gold
  "Oil" = "#A0522D",            # Sienna
  "Residential" = "#FF6347",    # Tomato
  "Solar" = "#FAFF00"           # Yellow
)

energy_water_data %>%
  filter(source %in% c("Livestock Water Use", "Domestic Water Use", "Electricity Water Use", "Industry Water Use", "Irrigation Water Use", "Mining Water Use")) %>%
  mutate(source = gsub(" Water Use", "", source)) %>%
  group_by(scenario, year, target) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value, color = target)) +
  geom_line(alpha = 0.35, size = 0.75) +
  geom_point() +
  scale_color_manual(values = waterusebysectorpalette) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = expression(paste("Water Use Sectors (", km^3, ")")), color = "Water Use:") +
  othervisualstheme

# make a stacked bar chart too
energy_water_data %>%
  filter(source %in% c("Livestock Water Use", "Domestic Water Use", "Electricity Water Use", "Industry Water Use", "Irrigation Water Use", "Mining Water Use")) %>%
  mutate(source = gsub(" Water Use", "", source)) %>%
  group_by(scenario, year, target) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value, fill = target)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = waterusebysectorpalette) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = expression(paste("Water Use Sectors (", km^3, ")")), fill = "Water Use:") +
  othervisualstheme

ggsave(paste0(sankeystatic, "water_use_sectors.svg"), width = 12, height = 6, dpi = 300)


# 5. primary energy supply: source == Nuclear Biomass Coal Geothermal Hydropower Natural Gas Nuclear Oil Solar Wind
primaryenergysupplypalette <- c(
  "Biomass" = "#006400",        # Dark green
  "Coal" = "#333333",           # Charcoal gray
  "Geothermal" = "#CD853F",     # Peru
  "Hydropower" = "#4682B4",     # Steel blue
  "Natural Gas" = "#1E90FF",    # Dodger blue
  "Nuclear" = "#FFD700",        # Gold
  "Oil" = "#A0522D",            # Sienna
  "Solar" = "#FAFF00",          # Yellow
  "Wind" = "#FF6347"            # Tomato
)


# energy_water_data %>%
#   filter(source %in% c("Nuclear", "Biomass", "Coal", "Geothermal", "Hydropower", "Natural Gas", "Nuclear", "Oil", "Solar", "Wind")) %>%
#   group_by(scenario, year, source) %>%
#   summarise(value = sum(value)) %>%
#   ggplot(aes(x = year, y = value, color = source)) +
#   geom_line(alpha = 0.35, size = 0.75) +
#   geom_point() +
#   scale_color_manual(values = primaryenergysupplypalette) +
#   facet_wrap(~scenario, ncol = 4) +
#   labs(y = expression(paste("Primary Energy Supply (", PJ, ")")), color = "Primary Energy Supply:") +
#   othervisualstheme
#

# bar chart
energy_water_data %>%
  filter(source %in% c("Nuclear", "Biomass", "Coal", "Geothermal", "Hydropower", "Natural Gas", "Nuclear", "Oil", "Solar", "Wind")) %>%
  group_by(scenario, year, source) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value, fill = source)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = primaryenergysupplypalette) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = "Primary Energy Supply (EJ)", fill = "Primary Energy Supply:") +
  othervisualstheme

ggsave(paste0(sankeystatic, "energy_supply.svg"), width = 12, height = 6, dpi = 300)

# 6. energy end-use: target ==  Commercial Residential Transportation-Vehicle Transportation-NonVehicle Industry
energy_water_data %>%
  filter(target %in% c("Commercial", "Residential", "Industry", "Transportation-Vehicle", "Transportation-NonVehicle")) %>%
  group_by(scenario, year, target) %>%
  summarise(value = sum(value)) %>%
  mutate(target = factor(target, levels = c("Commercial", "Residential", "Industry", "Transportation-Vehicle", "Transportation-NonVehicle"))) %>%
  ggplot(aes(x = year, y = value, fill = target)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Commercial" = "#FFD700", "Residential" = "#FF6347", "Transportation-Vehicle" = "#1E90FF", "Transportation-NonVehicle" = "dodgerblue4", "Industry" = "#333333")) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = "Energy End-Use (EJ)", fill = "Energy End-Use:") +
  othervisualstheme

ggsave(paste0(sankeystatic, "energy_enduse.svg"), width = 12, height = 6, dpi = 300)


# 7. transformation: target == Electricity Liquid Fuels Gaseous Fuels Hydrogen

energy_water_data %>%
  filter(target %in% c("Electricity", "Liquid Fuels", "Gaseous Fuels", "Hydrogen")) %>%
  group_by(scenario, year, target) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value, fill = target)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Electricity" = "#FFD700", "Liquid Fuels" = "#FF6347", "Gaseous Fuels" = "#4682B4", "Hydrogen" = "green4")) +
  facet_wrap(~scenario, ncol = 4) +
  labs(y = "Transformation (EJ)", fill = "Transformation:") +
  othervisualstheme

ggsave(paste0(sankeystatic, "energy_transformation.svg"), width = 12, height = 6, dpi = 300)


# 8. losses: target: Losses. color by source
energy_water_data %>%
  filter(target == "Losses") %>%
  ggplot(aes(x = year, y = value, color = source, shape = source)) +
  geom_line(size = 1, alpha = 0.35) +
  geom_point() +
  scale_color_manual(values = c("Electricity" = "#FFD700", "Liquid Fuels" = "#FF6347", "Gaseous Fuels" = "#4682B4", "Hydrogen" = "green4")) +
  facet_wrap(~scenario, nrow = 2) +
  labs(y = "Losses (EJ)", color = "", shape = "") +
  othervisualstheme

ggsave(paste0(sankeystatic, "energy_losses.svg"), width = 12, height = 6, dpi = 300)



## maps
library(rmap)
rmap::map(data = mapUS49, save = F, labels = T, subRegion = subRegionAlt)
rmap::map(data = mapGCAMBasinsUS49, save = F, labels = T)
rmap::map(data = mapGCAMBasinsUS52, save = F, labels = T)
rmap::map(data = mapGCAMReg32US49, save = F, labels = F)
rmap::map(data = mapIntersectGCAMBasinUS52, save = F, labels = F)
rmap::map(data = mapUS52Compact, save = F, labels = T)
rmap::map(data = mapUS52CountyCompact, save = F, labels = T)

rmap::map(data = rmap::mapUS52Compact,
          underLayer = rmap::mapUS52Compact,
          overLayer = rmap::mapGCAMBasinsUS49,
          crop_to_underLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 1,
          overLayerColor = "red4", overLayerLwd = 1,
          save = F)


rmap::map(data = rmap::mapGCAMBasinsUS49,
          underLayer = rmap::mapGCAMBasinsUS49,
          overLayer = rmap::mapUS52Compact,
          crop_to_overLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, #labelRepel = 1,
          overLayerColor = "red4", #overLayerLwd = 0.75,
          save = F)

# only GA and the south atlantic basin
rmap::map(data = rmap::mapUS52Compact %>% filter(subRegion == "GA"),
          underLayer = rmap::mapUS52Compact %>% filter(subRegion %in% c("GA", "AL", "TN", "SC")),
          overLayer = rmap::mapGCAMBasinsUS49 %>%
            filter(subRegion %in% c("Tennessee_River", "South_Atlantic_Gulf")),
          crop_to_overLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 1,
          overLayerColor = "red4", overLayerLwd = 1,
          save = F)

# only VA and the basins
rmap::map(data = rmap::mapUS52Compact %>% filter(subRegion == "VA"),
          underLayer = rmap::mapUS52Compact %>% filter(subRegion %in% c("VA")),
          overLayer = rmap::mapGCAMBasinsUS49 %>%
            filter(subRegion %in% c("Tennessee_River", "Mid_Atlantic",  "Ohio_River", "South_Atlantic_Gulf")),
          crop_to_overLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 0,
          overLayerColor = "red4", overLayerLwd = 1,
          save = F)

# midatlantic
rmap::map(data = rmap::mapUS52Compact %>% filter(subRegion %in% c("VA", "MD", "PA", "NJ", "NY", "DC", "WV", "DE", "VT", "CT")),
          underLayer = rmap::mapUS52Compact %>% filter(subRegion %in% c("VA")),
          overLayer = rmap::mapGCAMBasinsUS49 %>%
            filter(subRegion %in% c("Tennessee_River", "Mid_Atlantic", "New_England", "Ohio_River", "South_Atlantic_Gulf", "Great_Lakes")),
          crop_to_overLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 0,
          title = "Everything touching Mid-Atlantic (not truly everything)",
          overLayerColor = "red4", overLayerLwd = 1,
          save = F)

rmap::map(data = rmap::mapUS52Compact %>% filter(subRegion %in% c("VA", "MD", "PA", "NJ", "DC", "DE")),
          underLayer = rmap::mapUS52Compact %>% filter(subRegion %in% c("VA", "MD", "PA", "NJ", "DC", "DE")),
          overLayer = rmap::mapGCAMBasinsUS49 %>%
            filter(subRegion %in% c("Mid_Atlantic")),
          crop_to_overLayer = T,
          labels = T, underLayerLabels = T, overLayerLabels = T, labelRepel = 0,
          title = "Focused Mid-Atlantic States and Basins",
          overLayerColor = "red4", overLayerLwd = 1,
          save = F)

# kamal's hydropower paper ----
library(tidyverse)
library(treemapify)
library(plotly)
library(ggforce)
library(viridis)
library(scales)

hydrosmade <- tribble(
  ~Status, ~Type, ~Sites, ~Capacity,
  "Existing","Large Storage",887,444,
  "Existing","Local Storage",706,51,
  "Potential","Large Storage",28129,1611,
  "Potential","Local Storage",24480,200,
  "Potential","Large Diversion",13957,383,
  "Potential","Local Diversion",57767,625
) %>% mutate(cap_per_site = Capacity / Sites)

hydrosmade_det <- tribble(
  ~Status, ~Class, ~Type, ~Sites, ~Capacity,
  "Existing","Large","Storage",887,444,
  "Existing","Local","Storage",706,51,
  "Potential","Large","Storage",28129,1611,
  "Potential","Local","Storage",24480,200,
  "Potential","Large","Diversion",13957,383,
  "Potential","Local","Diversion",57767,625
) %>% mutate(cap_per_site = Capacity / Sites)

## bar charts ----
ggplot(hydrosmade, aes(x = Status, y = Capacity, fill = Type)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  labs(y = "Capacity (GW)", x = NULL)


hydrosmade %>%
  mutate(cap_per_site = Capacity / Sites) %>%
  ggplot(aes(x = reorder(Type, cap_per_site), y = cap_per_site, fill = Status)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(y = "GW per site", x = NULL)

# pretty
ggplot(hydrosmade, aes(x = reorder(Type, cap_per_site), y = cap_per_site, fill = Status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Existing" = "#440154", "Potential" = "#35B779")) +
  scale_y_continuous(labels = number_format(accuracy = 0.001, suffix = " GW")) +
  coord_flip() +
  labs(title = "Capacity per Site by Type and Status",
       x = "Type",
       y = "Capacity per Site (GW)",
       fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

{
  # by capacity
  cap <- ggplot(hydrosmade, aes(x = Status, y = Capacity, fill = Type)) +
    geom_col(width = 0.6) +
    scale_fill_viridis_d(option = "viridis") +
    geom_text(aes(label = paste0(Type, "\n", Capacity, " GW")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3, fontface = "bold") +
    labs(#title = "Total Capacity by Status and Type",
      x = "Status", y = "Capacity (GW)", fill = "Type", tag = '(c)') +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))

  # by sites
  sites <- ggplot(hydrosmade, aes(x = Status, y = Sites, fill = Type)) +
    geom_col(width = 0.6) +
    scale_fill_viridis_d(option = "viridis") +
    geom_text(aes(label = paste0(Type, "\n", Sites)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3, fontface = "bold") +
    labs(#title = "Total Sites by Status and Type",
      x = "Status", y = "Sites", fill = "Type", tag = '(d)') +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))

  # by capacity per site
  capsites <- ggplot(hydrosmade, aes(x = Status, y = cap_per_site, fill = Type)) +
    geom_col(width = 0.6) +
    scale_fill_viridis_d(option = "viridis") +
    geom_text(aes(label = paste0(round(cap_per_site, 3), "")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3, fontface = "bold") +
    labs(#title = "Total Capacity per Site by Status and Type",
      x = "Status", y = "Capacity per Site (GW/site)", fill = "Type", tag = '(e)') +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))

  cap + sites + capsites + plot_layout(guides = "collect", tag_level = 'keep') &
    # coord_flip() &
    theme(legend.position = 'bottom',
          # draw a box around each plot
          panel.border = element_rect(color = "gray80", fill = NA, size = 0.5))

}

# Reshape data for faceting
facet_data <- hydrosmade %>%
  pivot_longer(cols = c(Sites, Capacity), names_to = "Measure", values_to = "Value") %>%
  mutate(Measure = case_when(
    Measure == "Sites" ~ "Number of Sites",
    Measure == "Capacity" ~ "Capacity (GW)"
  ))

ggplot(facet_data, aes(x = reorder(Type, Value), y = Value, fill = Status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Existing" = "#440154", "Potential" = "#35B779")) +
  facet_wrap(~Measure, scales = "free_y", nrow = 2) +
  coord_flip() +
  labs(title = "Comparison of Sites and Capacity by Type and Status",
       x = "Type", y = "Value", fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"))

ggplot(facet_data, aes(x = reorder(Status, Value), y = Value, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~Measure, scales = "free_y", nrow = 2) +
  coord_flip() +
  labs(title = "Comparison of Sites and Capacity by Status and Type",
       x = "Status", y = "Value", fill = "Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"))

## sunburst ----
# Prepare data properly for sunburst
status_summary <- hydrosmade %>%
  group_by(Status) %>%
  summarise(Capacity = sum(Capacity), .groups = "drop")

# Create the hierarchical structure
plot_ly(
  type = "sunburst",
  ids = c(status_summary$Status,
          paste(hydrosmade$Status, hydrosmade$Type, sep = " - ")),
  labels = c(status_summary$Status,
             paste(hydrosmade$Type, "\n", hydrosmade$Capacity, "GW")),
  parents = c(rep("", nrow(status_summary)),
              hydrosmade$Status),
  values = c(status_summary$Capacity,
             hydrosmade$Capacity),
  branchvalues = "total"
) %>%
  layout(title = "Hydroelectric Capacity Distribution",
         font = list(size = 12))

# 3 level sunburst
# Create hierarchical structure for sunburst
# Level 1: Status
status_level <- hydrosmade_det %>%
  group_by(Status) %>%
  summarise(Capacity = sum(Capacity), .groups = "drop")

# Level 2: Status + Class
class_level <- hydrosmade_det %>%
  group_by(Status, Class) %>%
  summarise(Capacity = sum(Capacity), .groups = "drop") %>%
  mutate(id = paste(Status, Class, sep = " "),
         parent = Status)

# Level 3: Individual rows (Status + Class + Type)
type_level <- hydrosmade_det %>%
  mutate(id = paste(Status, Class, Type, sep = " "),
         parent = paste(Status, Class, sep = " "))

# Combine all levels
sunburst_data <- bind_rows(
  # Level 1: Root categories
  status_level %>% mutate(id = Status, parent = "",
                          label = paste(Status, "\n", Capacity, "GW")),
  # Level 2: Class within Status
  class_level %>% mutate(label = paste(Class, "\n", Capacity, "GW")),
  # Level 3: Type within Class and Status
  type_level %>% mutate(label = paste(Type, "\n", Capacity, "GW"))
)

# Create sunburst
plot_ly(
  type = "sunburst",
  ids = sunburst_data$id,
  labels = sunburst_data$label,
  parents = sunburst_data$parent,
  values = sunburst_data$Capacity,
  branchvalues = "total"
) %>%
  layout(
    title = "Hydroelectric Capacity: Status → Class → Type",
    font = list(size = 12)
  )



### final:   colors ----
hydrosmade_plot <- hydrosmade_det
# %>%
#   mutate(Class = ifelse(Status == "Existing",
#                         factor(Class, levels = c("Local", "Large")),
#                         factor(Class, levels = c("Large", "Local")))) %>%
#   arrange(Status, Class, Type) %>%
#   mutate(Class = case_when(
#     Class == "2" ~ "Local",
#     Class == "1" ~ "Large"
#   ))

hydrosmade_plot %>% group_by(Status, Type) %>%
  summarise(Capacity = sum(Capacity), Sites = sum(Sites), .groups = "drop") %>%
  mutate(cap_per_site = Capacity / Sites)

# Create hierarchical structure for sunburst
# Level 1: Status
status_level <- hydrosmade_plot %>%
  group_by(Status) %>%
  summarise(Capacity = sum(Capacity), .groups = "drop")

# Level 2: Status + Class
class_level <- hydrosmade_plot %>%
  group_by(Status, Class) %>%
  summarise(Capacity = sum(Capacity), .groups = "drop") %>%
  mutate(id = paste(Status, Class, sep = " "),
         parent = Status)


# Level 3: Individual rows (Status + Class + Type)
type_level <- hydrosmade_plot %>%
  mutate(id = paste(Status, Class, Type, sep = " "),
         parent = paste(Status, Class, sep = " "))

# Combine all levels with bold labels
sunburst_data <- bind_rows(
  # Level 1: Root categories
  status_level %>% mutate(id = Status, parent = "",
                          label = paste("<b>", Status, "</b>\n", Capacity, "GW")),
  # Level 2: Class within Status
  class_level %>% arrange(Class) %>%
    mutate(label = paste("<b>", Class, "</b>\n", Capacity, "GW"),
           label = if_else(id == "Existing Local" & parent == "Existing", "<b>Local</b> 51 GW", label)),
  # Level 3: Type within Class and Status
  type_level %>% mutate(label = paste("<b>", Type, "</b>\n",
                                        Capacity, "GW"))
  )




# # Define color scheme
# colors1 <- c("#FFB3BA", "#BFEFFF", "#FFDFBA", "#FFFFBA", "#BAE1FF", "#D4BAFF")
# colors2 <- c("#F4A6CD", "#A8E6CF", "#FFD3A5", "#FFF9C4", "#C7CEEA", "#E6E6FA")
# colors3 <- c("#FFCCCB", "#98FB98", "#FFDAB9", "#F0E68C", "#87CEEB", "#DDA0DD")
# colors4 <- c("#E0E0E0", "#B0E0E6", "#D3D3D3", "#F5F5DC", "#ADD8E6", "#E6E6FA")
# colors5 <- c("#FFE5CC", "#CCFFCC", "#FFCCCC", "#FFFFCC", "#CCE5FF", "#E5CCFF")
#
# colors1 <- c("#2C5F2D", "#4A90E2", "#1A365D", "#38A169", "#2B6CB0", "#059669")
# colors2 <- c("#8B4513", "#228B22", "#B8860B", "#CD853F", "#4682B4", "#32CD32")
# colors3 <- c("#FF6B6B", "#4ECDC4", "#FF8E53", "#FFE66D", "#A8E6CF", "#C7CEEA")
# colors4 <- c("#5D4E75", "#81C784", "#37474F", "#8D6E63", "#1976D2", "#388E3C")
# colors5 <- c("#D32F2F", "#388E3C", "#1976D2", "#FBC02D", "#7B1FA2", "#00796B")
#
# # shades of light ornage
# "#FFF3E0", "#FFE0B2", "#FFCC80", "#FFB74D", "#FFA726", "#FF9800", "#FB8C00", "#F57C00", "#EF6C00"
# # shades of light blue
# "#E3F2FD", "#BBDEFB", "#90CAF9", "#64B5F6", "#42A5F5", "#2196F3", "#1E88E5", "#1976D2", "#1565C0"
# # shades of blue
# "#E1F5FE", "#B3E5FC", "#81D4FA", "#4FC3F7", "#29B6F6", "#03A9F4", "#039BE5", "#0288D1", "#0277BD"

{
create_colors <- function(data) {
  colors <- character(nrow(data))

  for(i in 1:nrow(data)) {
    row <- data[i, ]

    # Level 1: Status colors
    if(is.na(row$Class) && is.na(row$Type)) {
      if(row$Status == "Existing") colors[i] <- "#2D4E75"  # Orange
      if(row$Status == "Potential") colors[i] <- "#2CAF50"  # Green
    }

    # Level 2: Class colors
    else if(is.na(row$Type)) {
      if(row$Status == "Existing" && row$Class == "Large") colors[i] <- "#FFA726"    # Red
      if(row$Status == "Existing" && row$Class == "Local") colors[i] <- "yellow"    # Yellow
      if(row$Status == "Potential" && row$Class == "Large") colors[i] <- "#FFCC80"   # Red
      if(row$Status == "Potential" && row$Class == "Local") colors[i] <- "#FFFFBA"   # Yellow
    }

    # Level 3: Type colors
    else {
      if(row$Status == "Existing" && row$Class == "Large" && row$Type == "Storage") colors[i] <- "#29B6F6"      # DodgerBlue
      if(row$Status == "Existing" && row$Class == "Local" && row$Type == "Storage") colors[i] <- "#29B6F6"      # DodgerBlue
      if(row$Status == "Potential" && row$Class == "Large" && row$Type == "Storage") colors[i] <- "#BAE1FF"     # DodgerBlue
      if(row$Status == "Potential" && row$Class == "Local" && row$Type == "Storage") colors[i] <- "#BAE1FF"     # DodgerBlue
      if(row$Status == "Potential" && row$Class == "Large" && row$Type == "Diversion") colors[i] <- "#FFC0CB"  # Pink
      if(row$Status == "Potential" && row$Class == "Local" && row$Type == "Diversion") colors[i] <- "#FFC0CB"  # Pink
    }
  }

  return(colors)
}

# Add colors to the data
sunburst_data$colors <- create_colors(sunburst_data)

# Create sunburst with custom colors
plot_ly(
  type = "sunburst",
  ids = sunburst_data$id,
  labels = sunburst_data$label,
  parents = sunburst_data$parent,
  values = sunburst_data$Capacity,
  branchvalues = "total",
  marker = list(colors = sunburst_data$colors)
) %>%
  layout(
    title = "Hydroelectric Capacity: Status → Class → Type",
    font = list(size = 12)
  )
}



## grid plot ----
ggplot(hydrosmade, aes(x = Status, y = Type)) +
  geom_tile(aes(fill = Capacity), color = "white", width = 0.9, height = 0.9) +
  scale_fill_viridis_c(option = "plasma", name = "Capacity\n(GW)") +
  geom_point(aes(size = Sites), shape = 21, fill = "white",
             color = "black", stroke = 1.5, alpha = 0.8) +
  scale_size_area(max_size = 15, name = "Sites") +
  geom_text(aes(label = paste0(Capacity, "\nGW")),
            color = "white", size = 3, fontface = "bold") +
  labs(title = "Capacity (color) and Sites (point size) by Status and Type",
       x = "Status", y = "Type") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))




ggplot(hydrosmade, aes(x = Type, y = Status)) +
  geom_point(aes(size = Capacity, color = Sites), alpha = 0.8) +
  scale_size_area(max_size = 20, name = "Capacity\n(GW)") +
  scale_color_viridis_c(option = "plasma", name = "Sites") +
  geom_text(aes(label = paste0(Capacity, " GW")),
            vjust = 0.5, fontface = "bold") +
  labs(title = "Capacity and Sites by Type and Status",
       subtitle = "Bubble size = Capacity, Color = Number of Sites",
       x = "Type", y = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))




