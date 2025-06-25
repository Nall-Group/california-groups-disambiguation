library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
    # Custom CSS for better styling
    tags$head(
        tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        margin: 40px;
        background-color: #f8f9fa;
      }
      .container {
        max-width: 800px;
        margin: 0 auto;
        background-color: white;
        padding: 40px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      h1 {
        color: #2c3e50;
        text-align: center;
        margin-bottom: 30px;
      }
      .link-container {
        text-align: center;
        margin-top: 30px;
      }
      .csv-link {
        color: #3498db;
        text-decoration: underline;
        font-weight: bold;
        font-size: 18px;
      }
      .csv-link:hover {
        color: #2980b9;
        text-decoration: underline;
      }
    "))
    ),


    # Main content
    div(
        class = "container",
        h1("California Groups Disambiguation Tool"),
        div(
            class = "link-container",
            tags$a(
                href = "https://github.com/Nall-Group/california-groups-disambiguation/blob/main/crosswalk.standardizenames.manualedits_clean.csv",
                class = "csv-link",
                target = "_blank",
                "California Groups Spreadsheet"
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    # No server logic needed for this simple static page
}

# Run the application
shinyApp(ui = ui, server = server)
