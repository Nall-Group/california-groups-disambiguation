library(shiny)
library(tidyverse)

# Function to disambiguate organization names
disambiguate_org <- function(input_name) {
  # Read the crosswalk file
  crosswalk <- read_csv("../crosswalk.standardizenames.manualedits_clean.csv",
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )

  # Look up the input name in the original names
  result <- crosswalk %>%
    filter(originalname == input_name) %>%
    pull(editedname)

  # Return result or indicate no match found
  if (length(result) > 0) {
    return(result[1]) # Return first match if multiple exist
  } else {
    return(paste("No match found for:", input_name))
  }
}

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

    # Disambiguation summary link
    div(
      class = "link-container",
      tags$a(
        href = "https://github.com/Nall-Group/california-groups-disambiguation/blob/main/disambiguation_summary.csv",
        class = "csv-link",
        target = "_blank",
        "California Groups Disambiguation Summary"
      )
    ),
    br(), br(),

    # Single organization disambiguation section
    div(
      h3("Single Organization Disambiguation", style = "color: #2c3e50; margin-bottom: 20px;"),
      div(
        style = "max-width: 500px; margin: 0 auto;",
        textInput("org_input",
          label = "Enter organization name:",
          value = "",
          placeholder = "e.g., ACLU",
          width = "100%"
        ),
        br(),
        div(
          style = "text-align: center;",
          actionButton("submit_btn",
            "Disambiguate",
            class = "btn btn-primary",
            style = "background-color: #3498db; border-color: #3498db; padding: 10px 30px;"
          )
        ),
        br(), br(),
        div(
          id = "result_area",
          textOutput("disambiguation_result"),
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; min-height: 50px; border: 1px solid #dee2e6;"
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive function triggered by submit button
  disambiguation_result <- eventReactive(input$submit_btn, {
    # Get the input text
    org_name <- trimws(input$org_input)

    # Check if input is empty
    if (org_name == "") {
      return("Please enter an organization name.")
    }

    # Call the disambiguation function
    result <- disambiguate_org(org_name)
    return(result)
  })

  # Render the result text
  output$disambiguation_result <- renderText({
    if (input$submit_btn == 0) {
      "Enter an organization name and click 'Disambiguate' to see the standardized name."
    } else {
      disambiguation_result()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
