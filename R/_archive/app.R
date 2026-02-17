# This is a basic Shiny app that includes a Sankey diagram with a few options.
#
# The app is divided into two parts: the UI and the server logic. The UI defines
# the layout and components of the app, while the server logic handles the
# interactivity and functionality of the app.
#
# Hassan Niazi, July 2024

library(shiny)
library(shinythemes)

# define UI for application
ui <- fluidPage(
  theme = shinytheme("slate"),
  # shinythemes::themeSelector(),

  #Jumbotrons are pretty, they make nice headers
  tags$div(class = "jumbotron text-center", style = "margin-bottom: 5px; margin-top: 5px; padding: 12px 0;",
           # make the text bold and change the font
           tags$h1(class = 'heading', style = 'margin-bottom: 5px; margin-top: 0px; font-weight: bold; font-family: "Arial";',
                   'Energy-Water Flows in the United States'),
           p(style = 'margin-bottom: 5px; margin-top: 0px; font-family: "Arial";',
             'WPTO IWPR Sankey Diagram Project | Hassan Niazi, Kendall Mongird, Jennie Rice, and Juliet Homer | Pacific Northwest National Laboratory')
  ),


  tags$style(HTML("
    .col-sm-4 { width: 350px; }
    .col-sm-8 { width: calc(100% - 350px); }
    .shiny-output-error { visibility: hidden; }
    .shiny-output-error:before { visibility: hidden; }
    .download-btn-container { display: flex; justify-content: center; }
    .text-center { text-align: center; }
  ")),

  # div(class = "text-center", h1("Water Energy Sankey Dashboard")),

  sidebarLayout(
    sidebarPanel(
      div(class = "text-center", h3("Choose your IM3 scenario")),
      selectInput("region", "Region", choices = c("United States", "Washington", "Maryland", "California", "New York")),
      selectInput("rcp", "Representative Concentration Pathway", choices = c("RCP4.5", "RCP8.5")),
      selectInput("ssp", "Shared Socioeconomic Pathway", choices = c("SSP3", "SSP5")),
      selectInput("climate", "Climate", choices = c("Hotter", "Cooler")),
      sliderInput("year", "Year", min = 2010, max = 2100, value = 2020, step = 5, sep = ""),

      tags$hr(class = "sidebar-hr"),

      fileInput("file", "Choose Another Sankey File",
                accept = c("text/html")),

      actionButton("reset", "Reset Diagram")
    ),

    mainPanel(
      div(class = "text-center", h3("Energy-Water Flows in the United States")),

      # tags$div(id = "sankeyDiagram",
      #          # Include the Sankey diagram HTML here
      #          tags$iframe(src = "output/sankey_elec_2025_usa.html",
      #                      width = "100%", height = "600px")
      # ),

      # div(includeHTML("output/sankey_elec_2025_usa.html"),
      div(includeHTML("output/sankey_water_all_2050_usa_nopac.html"),
        style = "overflow-x: auto; overflow-y: hidden;  width: 100%; height: 600px; margin: 0 auto; border: 1px solid #ccc; border-radius: 5px; box-shadow: 0 0 10px #ccc; "

      ),


      div(class = "download-btn-container",
          downloadButton("downloadImage", "Download image (png)"),
          downloadButton("downloadData", "Download data (csv)")
      ),
    ),
  ),

  div(class = "text-center",
      p("This dashboard was created by the WPTO IWPR Sankey Team at the Pacific Northwest National Laboratory. The data and visualizations presented in this dashboard are those of the creators and do not necessarily reflect the official policy or position of the Pacific Northwest National Laboratory or the U.S. Department of Energy.")
  )
)

# define server logic
server <- function(input, output, session) {

  # Reset diagram when the reset button is clicked
  observeEvent(input$reset, {
    # code to reset the Sankey diagram goes here
  })

  # code to handle downloads (placeholder functions)
  output$downloadImage <- downloadHandler(
    filename = function() {
      paste("sankey_diagram", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # code to generate and save the image
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sankey_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # code to generate and save the data
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

