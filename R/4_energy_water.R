# combined energy-water sankey diagrams
#
# stitches the water tables (2_waterR.R) to the energy tables (3_energyR.R) at
# each of the three levels of detail and writes the combined table for the
# dashboard. See R/energy_water_NOTES.md for the assumptions.
#
# Hassan Niazi, Nov 2026

source("./R/0_functions.R")

# inputs -----------------------------------------------------------------------
# processed tables written by 2_waterR.R and 3_energyR.R
if (!file.exists(out_parquet("water"))) source("./R/2_waterR.R")
stopifnot("energy tables not found: run 3_energyR.R" = file.exists(out_parquet("energy")))

water        <- read_parquet(out_parquet("water"))
energy       <- read_parquet(out_parquet("energy"))
energy_nodes <- read_parquet(out_parquet("energy_nodes"))
crop_share   <- read_parquet(out_parquet("biofuel_crop_share"))


# common nodes -----------------------------------------------------------------
# a water end use that is the same activity as an energy node takes the name of
# that node, so the water flows straight into the energy diagram:
#   power plants       -> the plant (levels 1-2) or its cooling system (level 3)
#   fuel extraction    -> Coal, Natural Gas, Oil, Nuclear
#   bioenergy crops    -> Biomass
#   municipal water    -> Buildings (levels 1, 3); Residential, Commercial (2, 3)
#   industry           -> Industry (levels 1, 3); Industrial Energy Use (level 2)
# the level-specific names follow the nodes the energy diagram has at that level

elec_map <- read_csv(INPUTS_MAP_FILE, show_col_types = FALSE) %>%
  filter(sector == "electricity", !is.na(technology)) %>%
  transmute(cooling = technology, plant = technology, input_L2, input_L3) %>%
  clean_cooling_tech("plant")

# storage variants share the plant's node; a plant missing from the mapping
# takes the cooling type in its name
elec_node <- function(tech, level) {
  tech  <- str_remove(tech, "_storage")
  plant <- Reduce(function(x, p) gsub(p, "", x), cooling_techs, tech)
  l2    <- elec_map$input_L2[match(plant, elec_map$plant)]
  if (level < 3) return(coalesce(l2, tech))
  cooling <- str_to_title(str_replace(str_match(tech, "\\(([^()]*)\\)$")[, 2], "_", " "))
  coalesce(elec_map$input_L3[match(tech, elec_map$cooling)],
           if_else(plant != tech, cooling, l2), tech)
}

common_nodes <- function(water_level, level) {
  plants <- unique(water_level$target[water_level$source == "Electricity Water Use"])
  c(EW_COMMON_NODES,
    setNames(elec_node(plants, level), plants),
    "municipal water" = "Buildings",
    "industry"        = if (level == 2) "Industrial Energy Use" else "Industry")
}


# crops to biofuels ------------------------------------------------------------
# the part of a crop's irrigation water embedded in biofuel feedstock flows on
# from the crop to the biofuel: crop water x national share of the harvest used
# as feedstock. The rest of the crop's water ends at the crop (food, feed, export).
biofuel_water <- water %>%
  filter(level == 3, source == "Irrigation Water Use", target %in% names(CROP_BIOFUEL)) %>%
  inner_join(crop_share %>% select(scenario, year, target = crop, share),
             by = c("scenario", "year", "target")) %>%
  mutate(source = target,
         target = unname(CROP_BIOFUEL[source]),
         value  = value * share) %>%
  select(-share, -level)


# assemble ---------------------------------------------------------------------
# energy-water level k = energy levels 0 and k + water level k. Energy is in EJ
# and water in km^3; the two are kept as separate flows (diagram, units).
ew_water <- map_dfr(1:3, function(lvl) {
  water_level <- water %>% filter(level == lvl)
  common <- common_nodes(water_level, lvl)

  # corn and oilcrop are part of "Crops", and their biofuels part of the
  # Biomass to Liquid Fuels flow, at level 1
  biofuel <- biofuel_water %>% mutate(level = lvl)
  if (lvl == 1) biofuel <- biofuel %>% mutate(source = "Crops", target = "Biomass")

  bind_rows(water_level, biofuel) %>%
    mutate(across(c(source, target), ~ coalesce(unname(common[.x]), .x))) %>%
    filter(source != target) %>%
    group_by(scenario, ssp, rcp, region, year, source, target, level, units, diagram) %>%
    summarise(value = sum(value), .groups = "drop")
}) %>%
  mutate(source_key = "Water", color = node_color(source))

energy_water <- bind_rows(energy, ew_water) %>% select(all_of(names(energy)))

energy_water_nodes <- bind_rows(
  energy_nodes,
  tibble(node = setdiff(c(ew_water$source, ew_water$target), energy_nodes$node)) %>%
    mutate(role = "water", color = node_color(node)))

if (UPDATE_DATA) write_parquet(energy_water, out_parquet("energy_water"))
if (UPDATE_DATA) write_parquet(energy_water_nodes, out_parquet("energy_water_nodes"))

# the dashboard draws energy-water level k from the energy level files plus
# these water rows, so the energy rows are not shipped twice
if (UPDATE_DATA) for (lvl in 1:3) {
  write_parquet(energy_water %>% filter(diagram == "Water", level == lvl),
                out_parquet(paste0("energy_water_l", lvl)))
}


# plot -------------------------------------------------------------------------
plot_energy_water_sankey <- function(reg = "CA", scen = "rcp45hotter_ssp5", yr = 2050,
                                     level = 3) {
  plot_sankey(energy_water %>% filter(level %in% c(0, .env$level)), energy_water_nodes,
              title = paste0("Energy-Water Nexus, Level ", level, " (energy EJ, water km^3)"),
              scen = scen, yr = yr, reg = reg)
}

if (PLOTS_ON) {
  plot_energy_water_sankey(level = 1)
  plot_energy_water_sankey(level = 2)
  plot_energy_water_sankey(level = 3)
  plot_energy_water_sankey("CA", level = 1)
  plot_energy_water_sankey("CA", level = 2)
  plot_energy_water_sankey("CA", level = 3)
  plot_energy_water_sankey("GA", level = 3)
}


# save ----
if (SAVE_FILES) {
  for (lvl in 1:3) {
    # US level
    htmlwidgets::saveWidget(plot_energy_water_sankey(level = lvl), selfcontained = TRUE,
                            file = paste0(output_dir, "sankey_energy_water_US_2050_L", lvl, ".html"))

    # state level
    htmlwidgets::saveWidget(plot_energy_water_sankey("CA", level = lvl), selfcontained = TRUE,
                            file = paste0(output_dir, "sankey_energy_water_CA_2050_L", lvl, ".html"))
  }
  htmlwidgets::saveWidget(plot_energy_water_sankey("GA"), selfcontained = TRUE,
                          file = paste0(output_dir, "sankey_energy_water_GA_2050_L3.html"))
}
