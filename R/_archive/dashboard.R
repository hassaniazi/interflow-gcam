# A dashboard for the IWPR snakey project to visualize water and energy flows in
# the United States.
#
# Hassan Niazi, December 2024

library(shiny)
library(shinydashboard)

# define UI for application
ui <- dashboardPage(
  skin = "red", # blue, black, purple, green, red, and yellow
  # dashboardHeader(title = "WPTO IWPR Water Energy Sankey Dashboard"),
  dashboardHeader(title = "WPTO IWPR Sankeys",
                  dropdownMenu(type = "messages",
                               headerText = "Messages",
                               messageItem(from = "Hassan", message = "The dashboard will be ready this month."),
                               messageItem(from = "Team", message = "How do I register?", icon = icon("question"), time = "13:45"),
                               messageItem(from = "Support", message = "The new server is ready.", icon = icon("life-ring"), time = "2024-12-01")
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Energy-Water Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Energy", tabName = "Energy", icon = icon("lightbulb")), # bolt
      menuItem("Water", tabName = "Water", icon = icon("water")),
      menuItem("Detailed", tabName = "Detailed", icon = icon("wand-magic-sparkles")),
      menuItem("Team & Documents", tabName = "team", icon = icon("users"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .sidebar { width: 300px; }
      .mainpanel { width: calc(100% - 300px); }
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before { visibility: hidden; }
      .download-btn-container { display: flex; justify-content: center; }
      .text-center { text-align: center; }
    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(12,
                       div(class = "text-center",
                           h1("Forward-looking Water Energy Flows Within United States")
                       )
                ),
                sidebarLayout(
                  sidebarPanel(
                    div(class = "text-center", h3("Choose your IM3 scenario")),
                    selectInput("region", "Region", choices = c("United States", "Washington", "Maryland", "California", "New York")),
                    selectInput("rcp", "Representative Concentration Pathway", choices = c("RCP4.5", "RCP8.5")),
                    selectInput("ssp", "Shared Socioeconomic Pathway", choices = c("SSP3", "SSP5")),
                    selectInput("climate", "Climate", choices = c("Hotter", "Cooler")),
                    sliderInput("year", "Year", min = 2020, max = 2100, value = 2020, step = 5, sep = ""),
                    # tags$hr(class = "sidebar-hr"),

                    fileInput("file", "Choose Another Sankey File",
                              accept = c("text/html")),

                    actionButton("reset", "Reset Diagram")
                  ),
                  mainPanel(
                    div(class = "text-center",
                        h3("Energy-Water Flows in the United States")
                    ),

                    # div(includeHTML("output/sankey_elec_2025_usa.html"),

                    div(includeHTML("output/sankey_elec_2025_usa.html"),
                        style = "overflow-x: auto; overflow-y: auto;  width: 100%; height: 600px; margin: 0 auto; border: 1px solid #ccc; border-radius: 5px; box-shadow: 0 0 10px #ccc; "

                    ),

                    div(class = "download-btn-container",
                        downloadButton("downloadImage", "Download image (png)"),
                        downloadButton("downloadData", "Download data (csv)")
                    )
                  )
                )
              ),
              div(class = "text-center",
                  p("This dashboard was created by the WPTO IWPR Sankey Team at the Pacific Northwest National Laboratory. The data and visualizations presented in this dashboard are those of the creators and do not necessarily reflect the official policy or position of the Pacific Northwest National Laboratory or the U.S. Department of Energy.")
              )
      ),

      # fill in the rest of the tabs
      tabItem(tabName = "Energy",
              h2("Energy Data"),
              p("Details about the energy data and links to additional documents and publications.")
      ),
      tabItem(tabName = "Water",
              h2("Water Data"),
              p("Details about the water data and links to additional documents and publications.")
      ),
      tabItem(tabName = "Detailed",
              h2("Detailed Data"),
              p("Details about the detailed data and links to additional documents and publications.")
      ),

      tabItem(tabName = "Team",
              h2("Team Members & Additional Information"),
              p("Details about the team members and links to additional documents and publications.")
      )
    )
  )
)

# define server logic
server <- function(input, output, session) {
  # reset diagram when the reset button is clicked
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

# run the application
shinyApp(ui = ui, server = server)

