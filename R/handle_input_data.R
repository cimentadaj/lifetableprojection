#' Handle File Upload
#'
#' @param input Shiny input object
#' @return Reactive expression containing the uploaded data
#' @importFrom shiny reactive
handle_file_upload <- function(input) {
  readData(input)
}

#' Handle Sample Data
#'
#' @return Reactive expression containing the sample data
#' @importFrom shiny reactive
handle_sample_data <- function() {
  reactive({
    dt_ex <- system.file("extdata/abridged_data.csv", package = "lifetableprojection")
    dt_read <- read.csv(dt_ex)
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
      return(character(0))
    } else {
      return(x)
    }
  })

  # Observe when data is updated AND group selection hasn't passed yet
  observeEvent(data(), {
    # Only show modal if group selection hasn't passed
    if (!group_selection_passed()) {
      # Render the modal UI
      output$modal_ui <- renderUI({
        shiny.semantic::modal(
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
            shiny.semantic::checkbox_input(
              "skip_grouping",
              "No grouping needed for this analysis",
              is_marked = FALSE
            ),
            br(),
            br(),
            br(),
            uiOutput("modal_error_message")
          ),
          footer = tagList(
            actionButton("confirm_column_selection", "Confirm", class = "ui button primary"),
            actionButton("cancel_column_selection", "Cancel", class = "ui button")
          )
        )
      })

      # Use session$onFlushed to ensure the UI is updated before showing the modal
      session$onFlushed(function() {
        shiny.semantic::show_modal("column_selection_modal")
      }, once = TRUE)
    }
  })

  validate_groups <- function() {
    selected_columns <- input$id_columns
    if (length(selected_columns) >= 0) {
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
        HTML('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">The specified columns do not identify each row uniquely or we\'ve identified there are grouping columns not specified.</p>')
      })
    } else {
      data(ODAPbackend:::create_groupid(data(), input$id_columns))
      group_selection_passed(TRUE)
      selected_grouping_vars(input$id_columns)
      shiny.semantic::hide_modal("column_selection_modal")
    }
  })

  # Close the modal when the "Cancel" button is clicked
  observeEvent(input$cancel_column_selection, {
    shiny.semantic::hide_modal("column_selection_modal")
  })
}
