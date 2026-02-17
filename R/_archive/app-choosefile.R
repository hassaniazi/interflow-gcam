library(shiny)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("slate"),

  # Jumbotron header
  tags$div(class = "jumbotron text-center", style = "margin-bottom: 5px; margin-top: 5px; padding: 12px 0;",
           tags$h1(class = 'heading', style = 'margin-bottom: 5px; margin-top: 0px; font-weight: bold; font-family: "Serif";',
                   'Energy-Water Flows in the United States'),
           p(style = 'margin-bottom: 5px; margin-top: 0px; font-family: "Serif";',
             'IWPR WPTO Sankey Diagram Project | Hassan Niazi, Kendall Mongird, and Jennie Rice | Pacific Northwest National Laboratory')
  ),

  tags$style(HTML("
    .col-sm-4 { width: 350px; }
    .col-sm-8 { width: calc(100% - 350px); }
    .shiny-output-error { visibility: hidden; }
    .shiny-output-error:before { visibility: hidden; }
    .download-btn-container { display: flex; justify-content: center; }
    .text-center { text-align: center; }
  ")),

  sidebarLayout(
    sidebarPanel(
      div(class = "text-center", h3("Build your IM3 scenario")),
      selectInput("region", "Region", choices = c("United States", "Washington", "Maryland", "California", "New York")),
      selectInput("rcp", "Representative Concentration Pathway", choices = c("RCP4.5", "RCP8.5")),
      selectInput("ssp", "Shared Socioeconomic Pathway", choices = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")),
      selectInput("climate", "Climate", choices = c("Hotter", "Cooler")),
      sliderInput("year", "Year", min = 2010, max = 2100, value = 2020, step = 5, sep = ""),

      # File input for selecting a file
      fileInput("file", "Choose HTML File",
                accept = c("text/html")),

      actionButton("reset", "Reset Diagram")
    ),

    mainPanel(
      div(class = "text-center", h3("Energy-Water Flows in the United States")),

      # Dynamic UI for Sankey diagram
      uiOutput("sankeyPlot"),

      div(class = "download-btn-container",
          downloadButton("downloadImage", "Download image (png)"),
          downloadButton("downloadData", "Download data (csv)")
      )
    )
  ),

  div(class = "text-center",
      p("This dashboard was created by IWPR WPTO Sankey Team at the Pacific Northwest National Laboratory. The data and visualizations presented in this dashboard are those of the creators and do not necessarily reflect the official policy or position of the Pacific Northwest National Laboratory or the U.S. Department of Energy.")
  )
)

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$file, {
    # Update the main panel content based on the selected file
    output$sankeyPlot <- renderUI({
      req(input$file)
      includeHTML(input$file$datapath)
    })
  })

  # Reset diagram when the reset button is clicked
  observeEvent(input$reset, {
    # Code to reset the Sankey diagram goes here
    output$sankeyPlot <- renderUI({
      includeHTML("output/sankey_elec_2025_usa.html")
    })
  })

  # Code to handle downloads (placeholder functions)
  output$downloadImage <- downloadHandler(
    filename = function() {
      paste("sankey_diagram", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Code to generate and save the image
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sankey_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Code to generate and save the data
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
