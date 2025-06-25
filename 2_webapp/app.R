library(shiny)

# Increase file upload limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Load crosswalk data once at startup
crosswalk_data <- read.csv("crosswalk.standardizenames.manualedits_clean.csv",
  stringsAsFactors = FALSE
)

# Function to disambiguate organization names
disambiguate_org <- function(input_name) {
  # Look up the input name in the original names
  match_idx <- which(crosswalk_data$originalname == input_name)

  # Return result or indicate no match found
  if (length(match_idx) > 0) {
    return(crosswalk_data$editedname[match_idx[1]]) # Return first match if multiple exist
  } else {
    return(paste("No match found for:", input_name))
  }
}

# Function to process uploaded CSV and disambiguate all org names
process_uploaded_csv <- function(uploaded_file) {
  # Read the uploaded CSV
  uploaded_data <- read.csv(uploaded_file$datapath, stringsAsFactors = FALSE)

  # Get the first column (assume it contains org names)
  first_col <- uploaded_data[, 1]
  first_col_name <- names(uploaded_data)[1]

  # Create disambiguation results
  disambiguated_names <- sapply(first_col, function(name) {
    match_idx <- which(crosswalk_data$originalname == name)
    if (length(match_idx) > 0) {
      return(crosswalk_data$editedname[match_idx[1]])
    } else {
      return("No match found")
    }
  })

  # Create match status
  match_status <- ifelse(disambiguated_names == "No match found", "No Match", "Matched")

  # Start with original data
  results <- uploaded_data

  # Insert disambiguation columns after the first column
  if (ncol(uploaded_data) == 1) {
    # If only one column, just add the new columns
    results <- cbind(
      results,
      data.frame(
        disambiguated_name = disambiguated_names,
        match_status = match_status,
        stringsAsFactors = FALSE
      )
    )
  } else {
    # If multiple columns, insert after first column
    results <- cbind(
      uploaded_data[, 1, drop = FALSE], # First column
      data.frame(
        disambiguated_name = disambiguated_names,
        match_status = match_status,
        stringsAsFactors = FALSE
      ),
      uploaded_data[, -1, drop = FALSE] # All other columns
    )
  }

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
      .btn-disabled {
        background-color: #6c757d !important;
        border-color: #6c757d !important;
        color: white !important;
        cursor: not-allowed !important;
        opacity: 0.65;
      }
      .btn-enabled {
        background-color: #28a745 !important;
        border-color: #28a745 !important;
        color: white !important;
        cursor: pointer !important;
        opacity: 1;
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
      p("Upload a CSV file with organization names in the first column. The disambiguated name and match status will be inserted after the first column, preserving all other data.",
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
          id = "upload_status",
          textOutput("upload_status_text"),
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; min-height: 50px; border: 1px solid #dee2e6;"
        ),
        br(),
        div(
          style = "text-align: center;",
          uiOutput("download_button_ui")
        ),
        br(), br()
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

  # Render download button conditionally
  output$download_button_ui <- renderUI({
    if (is.null(input$csv_upload) || is.null(processed_data())) {
      # Disabled button - gray state
      tags$button(
        "Download Disambiguated CSV",
        class = "btn btn-disabled",
        style = "padding: 10px 30px;",
        disabled = "disabled"
      )
    } else {
      # Enabled buttons - both download and copy options
      div(
        downloadButton("download_results",
          "Download CSV",
          class = "btn btn-enabled",
          style = "padding: 10px 20px; margin-right: 10px;"
        ),
        actionButton("show_results",
          "Show Results",
          class = "btn btn-enabled",
          style = "padding: 10px 20px;"
        )
      )
    }
  })

  # Show results in modal dialog as backup for download issues
  observeEvent(input$show_results, {
    req(processed_data())

    # Convert data to CSV text
    temp_file <- tempfile(fileext = ".csv")
    write.csv(processed_data(), temp_file, row.names = FALSE, na = "")
    csv_text <- paste(readLines(temp_file), collapse = "\n")
    unlink(temp_file)

    showModal(modalDialog(
      title = "Disambiguated Results",
      div(
        p("Copy the text below and save it as a .csv file:"),
        tags$textarea(
          csv_text,
          style = "width: 100%; height: 400px; font-family: monospace; font-size: 12px;",
          readonly = "readonly"
        ),
        br(), br(),
        p("Instructions: Select all text above (Ctrl+A/Cmd+A), copy (Ctrl+C/Cmd+C), paste into a text editor, and save with .csv extension.")
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Download handler for processed CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("DISAMBIGUATED_ORGS_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      tryCatch(
        {
          data <- processed_data()
          if (!is.null(data)) {
            write.csv(data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
          }
        },
        error = function(e) {
          # Fallback: create a basic CSV
          cat("Error creating download file\n", file = file)
        }
      )
    },
    contentType = "text/csv"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
