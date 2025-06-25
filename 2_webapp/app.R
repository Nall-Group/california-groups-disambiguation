library(shiny)
library(tidyverse)

# Increase file upload limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Load crosswalk data once at startup
crosswalk_data <- read_csv("crosswalk.standardizenames.manualedits_clean.csv",
  col_types = cols(.default = "c"),
  show_col_types = FALSE
)

# Function to disambiguate organization names
disambiguate_org <- function(input_name) {
  # Look up the input name in the original names
  result <- crosswalk_data %>%
    filter(originalname == input_name) %>%
    pull(editedname)

  # Return result or indicate no match found
  if (length(result) > 0) {
    return(result[1]) # Return first match if multiple exist
  } else {
    return(paste("No match found for:", input_name))
  }
}

# Function to process uploaded CSV and disambiguate all org names
process_uploaded_csv <- function(uploaded_file) {
  # Read the uploaded CSV
  uploaded_data <- read_csv(uploaded_file$datapath,
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )

  # Get the first column (assume it contains org names)
  first_col_name <- names(uploaded_data)[1]

  # Create disambiguation results
  results <- uploaded_data %>%
    select(original_name = all_of(first_col_name)) %>%
    mutate(
      # Look up each name in crosswalk
      disambiguated_name = map_chr(original_name, function(name) {
        match_result <- crosswalk_data %>%
          filter(originalname == name) %>%
          pull(editedname)

        if (length(match_result) > 0) {
          return(match_result[1])
        } else {
          return("No match found")
        }
      }),
      # Add match status
      match_status = ifelse(disambiguated_name == "No match found", "No Match", "Matched")
    )

  return(results)
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
    ),
    br(), br(),
    hr(style = "border-top: 2px solid #dee2e6; margin: 40px 0;"),

    # Batch CSV upload section
    div(
      h3("Batch CSV Disambiguation", style = "color: #2c3e50; margin-bottom: 20px;"),
      p("Upload a CSV file with organization names in the first column to get a disambiguated version.",
        style = "text-align: center; color: #6c757d; margin-bottom: 30px;"
      ),
      div(
        style = "max-width: 600px; margin: 0 auto;",
        fileInput("csv_upload",
          "Choose CSV File:",
          accept = c(".csv"),
          width = "100%"
        ),
        br(),
        div(
          style = "text-align: center;",
          downloadButton("download_results",
            "Download Disambiguated CSV",
            class = "btn btn-success",
            style = "background-color: #28a745; border-color: #28a745; padding: 10px 30px;"
          )
        ),
        br(), br(),
        div(
          id = "upload_status",
          textOutput("upload_status_text"),
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

  # Reactive value to store processed CSV data
  processed_data <- reactive({
    req(input$csv_upload)

    # Process the uploaded file
    tryCatch(
      {
        result <- process_uploaded_csv(input$csv_upload)
        return(result)
      },
      error = function(e) {
        return(NULL)
      }
    )
  })

  # Display upload status
  output$upload_status_text <- renderText({
    if (is.null(input$csv_upload)) {
      "Upload a CSV file to begin batch disambiguation."
    } else if (is.null(processed_data())) {
      "Error processing file. Please ensure it's a valid CSV with organization names in the first column."
    } else {
      data <- processed_data()
      total_orgs <- nrow(data)
      matched_orgs <- sum(data$match_status == "Matched")
      paste0(
        "Processed ", total_orgs, " organizations. ",
        matched_orgs, " matched (", round(matched_orgs / total_orgs * 100, 1), "%). ",
        "Click 'Download' to get results."
      )
    }
  })

  # Download handler for processed CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("disambiguated_orgs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(processed_data())
      write_csv(processed_data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
