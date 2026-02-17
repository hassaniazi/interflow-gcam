# Function to automatically create child-parent data frame
#
source("./R/0_functions.R")

# Load data
{
  gcamusa_input_by_sectors <- read_csv(paste0(data_dir, "inputsbysector_gcamusa.csv"))
  gcamusa_input_by_tech <- read_csv(paste0(data_dir, "inputsbytech_gcamusa.csv"))
  gcamusa_output_by_tech <- read_csv(paste0(data_dir, "outputsbytech_gcamusa.csv"))
}

plot_sankey_basic <- function(data) {

  data$source <- as.character(data$source)
  data$target <- as.character(data$target)

  nodes <- unique(c(data$source, data$target))
  node_ids <- seq_along(nodes) - 1
  names(node_ids) <- nodes

  plot_ly(type = "sankey",
          orientation = "h",
          node = list(label = nodes,
                      pad = 15, thickness = 20),
          link = list(source = node_ids[data$source],
                      target = node_ids[data$target],
                      value  = data$value)
  )
}

plot_network_basic <- function(data, layout = "fr") {

  data$source <- as.character(data$source)
  data$target <- as.character(data$target)

  g <- graph_from_data_frame(d = data, directed = TRUE)

  ggraph(g, layout = layout) + # fr, dh
    geom_edge_link(aes(width = value), alpha = 0.6, colour = "grey40") +
    geom_node_point(size = 4, colour = "steelblue") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width(range = c(0.3, 3)) +
    theme_void()
}


# filter for year and aggregate to sectors
aggregate_inputs_to_sectors <- function(df, var = input) {
  df_out <- df %>%
    group_by(sector, {{var}}) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  return(df_out)
}

gcamusa_input_by_tech_2050 <- gather_year_filt(gcamusa_input_by_tech, 2050) %>% aggregate_inputs_to_sectors()

plot_sankey_basic(gcamusa_input_by_tech_2050 %>% rename(source = input, target = sector))
plot_network_basic(gcamusa_input_by_tech_2050 %>% rename(source = input, target = sector), layout = "fr")
plot_network_basic(gcamusa_input_by_tech_2050 %>% rename(source = input, target = sector), layout = "dh")

# get all sectors function
get_all_sectors_of_parent_input <- function(data, parent, depth=1) {

  if (parent %in% full_list) {
    full_list <<- full_list[full_list != parent]
  } else {
    return(data.frame(source = character(), target = character(), value = double()))
  }

  df_source_target <- data.frame(source = character(), target = character(), value = double())

  # get children of the parent (input)
  data %>% filter(input == parent) %>% select(sector) %>% distinct() %>% pull() -> child_sectors

  # parent has to be an input
  if (length(child_sectors) != 0) {
    for (sector in child_sectors) {
      # get the value for the (first) child of the parent
      df_source_target <- rbind(df_source_target, data.frame(source = parent, target = sector,
                                                             value = data %>%
                                                               filter(sector == !!sector, input == parent) %>%
                                                               select(value) %>%
                                                               pull()
                                                             )
                                )

      # recursion to get all children of the child
      is_last_child <- !(sector %in% data$input)
      if(!is_last_child) {
        down_flow <- get_all_sectors_of_parent_input(data, sector, depth+1)
        df_source_target <- rbind(df_source_target, down_flow)
      }
    }

    return(df_source_target %>% dplyr::distinct(source, target, value))
  }
}

# apply the get input-to-sectors function to all the inputs in the inputs data
full_func <- function(data, parent){
  full_list <<- unique(data$input)

  return(get_all_sectors_of_parent_input(data, parent, 1))

}

plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'crude oil'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'Beef'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'natural gas'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'regional natural gas'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'traded LNG'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'regional coal'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'gas pipeline'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'Corn'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'FeedCrops'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'ammonia'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'water_td_an_W'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'elect_td_ind'))
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'irrigation water abstraction'))
get_all_sectors_of_parent_input(gcamusa_input_by_tech_2050, 'regional natural gas', 2)

get_all_sectors_of_parent_input(gcamusa_input_by_tech_2050, 'N fertilizer') -> N_fert_parent

plot_sankey_basic(get_all_sectors_of_parent_input(gcamusa_input_by_tech_2050, 'gas pipeline'))
N_fert_parent %>%  dplyr::distinct(source, target, value) -> N_fert_parent_distinct

# string that contains water_td_irr_* inputs
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'traded LNG'))
full_func_outputs


## OUTPUTS: create a similar function and workflow for outputs by tech ----



aggregate_outputs_to_sectors <- function(df, var = technology) {
  df_out <- df %>%
    group_by(output, {{var}}) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  return(df_out)
}

gcamusa_output_by_tech_2050 <- gather_year_filt(gcamusa_output_by_tech, 2050) %>%
  aggregate_outputs_to_sectors() %>%
  # aggregate technology that have _RFD or IRR or _hi or _lo in their name so CornC4_UsaCstNE_IRR_hi will just be CornC4_UsaCstNE in technology column
  mutate(technology = str_extract(technology, "^[^_]+"),
         output = str_extract(output, "^[^_]+")) %>%
  group_by(output, technology) %>%
  summarise(value = sum(value)) %>% ungroup()

plot_sankey_basic(gcamusa_output_by_tech_2050 %>% rename(source = technology, target = output))

# plot_network_basic(gcamusa_output_by_tech_2050 %>% rename(source = technology, target = output), layout = "fr")
plot_network_basic(gcamusa_output_by_tech_2050 %>% rename(source = technology, target = output), layout = "kk")


# all of gcam network and sankey by combining inputs and outputs by tech
full_gcam_io <- rbind(
  gcamusa_input_by_tech_2050 %>% rename(source = input, target = sector),
  gcamusa_output_by_tech_2050 %>% rename(source = technology, target = output)
) %>% distinct()

plot_sankey_basic(full_gcam_io)
plot_network_basic(full_gcam_io, layout = "kk")

# get all sectors function
get_all_parents_of_a_child <- function(data, child, depth=1) {

  if (child %in% full_list) {
    full_list <<- full_list[full_list != child]
  } else {
    return(data.frame(source = character(), target = character(), value = double()))
  }

  df_source_target <- data.frame(source = character(), target = character(), value = double())

  # get children of the parent (input)
  data %>% filter(output == child) %>% select(technology) %>% distinct() %>% pull() -> parent_techs

  # parent has to be an input
  if (length(parent_techs) != 0) {
    for (tech in parent_techs) {
      # get the value for the (first) child of the parent
      df_source_target <- rbind(df_source_target, data.frame(source = tech, target = child,
                                                             value = data %>%
                                                               filter(tech == !!tech, output == child) %>%
                                                               select(value) %>%
                                                               pull()
      )
      )

      # recursion to get all children of the child
      is_first_parent <- !(tech %in% data$output)
      if (!is_first_parent) {
        up_flow <- get_all_parents_of_a_child(data, tech, depth+1)
        df_source_target <- rbind(df_source_target, up_flow)
      }
    }

    return(df_source_target %>% dplyr::distinct(source, target, value))
  }
}

# apply the get input-to-sectors function to all the inputs in the inputs data
full_func_outputs <- function(data, child){
  full_list <<- unique(data$output)

  return(get_all_parents_of_a_child(data, child, 1))

}



full_func_outputs(gcamusa_output_by_tech_2050, 'ammonia') ->a
plot_sankey_basic(full_func(gcamusa_input_by_tech_2050, 'Beef'))
plot_sankey_basic(full_func_outputs(gcamusa_output_by_tech_2050, 'Beef'))
plot_sankey_basic(full_func_outputs(gcamusa_output_by_tech_2050, 'traded LNG'))
plot_sankey_basic(full_func_outputs(gcamusa_output_by_tech_2050, 'regional natural gas'))

plot_sankey_basic(rbind(full_func(gcamusa_input_by_tech_2050, 'Beef'), full_func_outputs(gcamusa_output_by_tech_2050, 'Beef')))
plot_network_basic(rbind(full_func(gcamusa_input_by_tech_2050, 'Beef'), full_func_outputs(gcamusa_output_by_tech_2050, 'Beef')))
plot_sankey_basic(rbind(full_func(gcamusa_input_by_tech_2050, 'traded LNG'), full_func_outputs(gcamusa_output_by_tech_2050, 'traded LNG')))


###############################################################################%

# INTERACTIVE----

library(dplyr)

prep_network_data <- function(
    data,
    top_n = NULL,
    min_value = NULL
) {

  df <- data %>%
    mutate(
      source = as.character(source),
      target = as.character(target)
    ) %>%
    group_by(source, target) %>%
    summarise(value = sum(value), .groups = "drop")

  if (!is.null(min_value)) {
    df <- df %>% filter(value >= min_value)
  }

  if (!is.null(top_n)) {
    df <- df %>% slice_max(value, n = top_n)
  }

  df
}



library(visNetwork)

plot_visnetwork <- function(
    data,
    layout = "layout_with_fr",
    node_color = "#1f78b4",
    edge_color = "gray",
    node_size = 25,  # Adjusted smaller node size
    font_size = 30   # Adjusted smaller font size

) {

  # Create unique nodes
  nodes <- data.frame(
    id = sort(unique(c(data$source, data$target))),
    label = sort(unique(c(data$source, data$target))),
    color = node_color
  )

  # Create edges with width scaled by value
  edges <- data.frame(
    from = data$source,
    to   = data$target,
    value = data$value,
    width = scales::rescale(data$value, c(1, 8)),
    color = edge_color
  )

  # Network visualization
  visNetwork(nodes, edges) %>%
    visEdges(smooth = FALSE) %>%
    visNodes(size = node_size, font = list(size = font_size)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(
      solver = ifelse(layout == "layout_with_fr", "forceAtlas2Based", "barnesHut"),
      stabilization = TRUE  # Stabilizes the network to stop spinning
    ) %>%
    visIgraphLayout(layout = layout)  # Apply specified layout
}


library(plotly)

plot_sankey <- function(data) {

  nodes <- unique(c(data$source, data$target))
  node_ids <- seq_along(nodes) - 1
  names(node_ids) <- nodes

  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes,
      pad = 15,
      thickness = 20
    ),
    link = list(
      source = node_ids[data$source],
      target = node_ids[data$target],
      value  = data$value
    )
  )
}


library(htmltools)

plot_hybrid <- function(
    data,
    top_n = NULL,
    min_value = NULL
) {

  df <- prep_network_data(data, top_n, min_value)

  tagList(
    div(
      style = "display:flex; gap:20px;",
      div(style = "width:50%;", plot_sankey(df)),
      div(style = "width:50%;", plot_visnetwork(df))
    )
  )
}


plot_flow <- function(
    data,
    view = c("network", "sankey", "hybrid"),
    top_n = NULL,
    min_value = NULL
) {

  view <- match.arg(view)

  df <- prep_network_data(data, top_n, min_value)

  switch(
    view,
    network = plot_visnetwork(df),
    sankey  = plot_sankey(df),
    hybrid  = plot_hybrid(df, top_n, min_value)
  )
}


df <- gcamusa_input_by_tech_2050 %>%
  rename(source = input, target = sector)

# Network only
plot_flow(df, view = "network", top_n = 500)

# Sankey only
plot_flow(df, view = "sankey", min_value = 50)

# Hybrid
plot_flow(df, view = "hybrid", top_n = 150)


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("view", "View", c("network", "sankey", "hybrid")),
      numericInput("top_n", "Top N edges", 150, min = 10),
      numericInput("min_value", "Min value", 0)
    ),
    mainPanel(
      uiOutput("plot")
    )
  )
)

server <- function(input, output) {

  output$plot <- renderUI({
    plot_flow(
      df,
      view = input$view,
      top_n = input$top_n,
      min_value = input$min_value
    )
  })
}

shinyApp(ui, server)


