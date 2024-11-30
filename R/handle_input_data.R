#' Handle File Upload
#'
#' @param input Shiny input object
#' @return Reactive expression containing the uploaded data
#' @importFrom shiny reactive
handle_file_upload <- function(input) {
  reactive({
    req(input$file1)
    readData(input)
  })
}

#' Handle Sample Data
#'
#' @return Reactive expression containing the sample data
#' @importFrom shiny reactive
handle_sample_data <- function() {
  reactive({
    dt_ex <- system.file("data/abridged_data.csv", package = "lifetableprojection")
    dt_read <- read.csv(dt_ex)
    dt_read$`.id` <- 1
    dt_read$`.id_label` <- "All"
    dt_read
  })
}

#' Validate Uploaded Data
#'
#' @param data Reactive expression containing the data to validate
#' @return Reactive expression containing validation results
#' @importFrom shiny reactive req
validate_data <- function(data) {
  reactive({
    req(data())
    validateData(data())
    # TODO: REMOVE
    list(pass = "Pass")
  })
}

#' Handle Column Selection
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param data Reactive expression containing the data
#' @param group_selection_passed Reactive value to flag whether the group selection stage has been passed0
#' @param selected_grouping_vars Reactive value to store the variables selected as grouping vars
#' @importFrom shiny observeEvent req
#' @importFrom shinyalert shinyalert
handle_group_selection_modal <- function(input, output, session, data, group_selection_passed, selected_grouping_vars) {
  # Reactive expression for choices with safe defaults
  choices <- reactive({
    x <- names(data())
    if (is.null(x)) {
      return(character(0))  # Return empty character vector if data is NULL
    } else {
      return(x)
    }
  })

  # Observe when data is updated
  observeEvent(data(), {
    # Render the modal UI
    output$modal_ui <- renderUI({
      modal(
        id = "column_selection_modal",
        header = "Column Selection",
        content = div(
          shiny.semantic::selectInput(
            "id_columns",
            label = "Select Identifier Columns",
            choices = choices(),
            multiple = TRUE
          ),
          br(),
          uiOutput("modal_error_message")  # Placeholder for error messages
        ),
        footer = tagList(
          actionButton("confirm_column_selection", "Confirm", class = "ui button primary"),
          actionButton("cancel_column_selection", "Cancel", class = "ui button")
        )
      )
    })

    # Use session$onFlushed to ensure the UI is updated before showing the modal
    session$onFlushed(function() {
      show_modal("column_selection_modal")
    }, once = TRUE)  # Ensure it's called only once per data update
  })

    validate_groups <- function() {
      selected_columns <- input$id_columns

      if (length(selected_columns) > 0) {
        library(dplyr)
        valid_groups <-
          ODAPbackend:::create_groupid(data(), selected_columns) %>%
          ODAPbackend:::check_groupid()

        valid_groups
      } else {
        FALSE
      }
    }

  # Observe the confirm button click
  observeEvent(input$confirm_column_selection, {
    if (length(input$id_columns) > 3) {
      output$modal_error_message <- renderUI({
        HTML('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">A maximum of 3 grouping variables are allowed.</p>')
      })
    } else if (!validate_groups()) {
      output$modal_error_message <- renderUI({
        HTML('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">The specified columns do not identify each row uniquely.</p>')
      })
    } else {
      # Success: process the data and close the modal
      # data(ODAPbackend:::create_groupid(data(), input$id_columns))  # Replace with actual processing
      data(data())  # Placeholder: no actual processing
      group_selection_passed(TRUE)
      selected_grouping_vars(input$id_columns)  # Store the selected grouping variables
      hide_modal("column_selection_modal")  # Close the modal after success
    }
  })

  # Close the modal when the "Cancel" button is clicked
  observeEvent(input$cancel_column_selection, {
    hide_modal("column_selection_modal")
  })
}

#' Show Column Selection Modal
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param data Reactive expression containing the data
#' @importFrom shiny div br
#' @importFrom shiny.semantic selectInput
#' @importFrom shinyalert shinyalert
show_column_selection_modal <- function(input, output, session, data) {
  column_selector <- selectInput(
    "id_columns",
    label = "Select Identifier Columns",
    choices = names(data()),
    multiple = TRUE
    ## selectize = TRUE
  )

  alert_content <- div(
    column_selector,
    br(), br(), br(), br(), br(), br()
  )

  shinyalert(
    title = "Column Selection",
    text = alert_content,
    html = TRUE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "Confirm",
  )

  observeEvent(input$shinyalert, {
    if (!validate_groups(input, data())) {
      print("not validated")
      show_column_selection_modal(input, output, session, data)
    } else {
      data(ODAPbackend:::create_groupid(data(), input$id_columns))
    }
  })
}

#' Validate Selected Groups
#'
#' @param input Shiny input object
#' @param data Data frame to validate
#' @return Boolean indicating if groups are valid
#' @importFrom dplyr %>%
validate_groups <- function(input, data) {
  selected_columns <- input$id_columns
  print(selected_columns)
  if (length(selected_columns) > 0) {
    library(dplyr)
    ODAPbackend:::create_groupid(data, selected_columns) %>%
      ODAPbackend:::check_groupid()
  } else {
    FALSE
  }
}



library(shiny)
library(shinyalert)  # For modal dialogs
library(shiny.semantic)  # For Semantic UI components

# Define UI
ui <- semanticPage(
  actionButton("open_modal", "Open Modal")
)

# Define Server
server <- function(input, output, session) {

  # Observe button click to open modal
  observeEvent(input$open_modal, {
    shinyalert(
      title = "Semantic Checkbox",
      html = TRUE,
      text = div(
        shiny.semantic::checkbox_input("checkbox", "Tick this box", is_marked = FALSE)  # Semantic styled checkbox
      ),
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
    )
  })
}

# Run the app
shinyApp(ui, server)
