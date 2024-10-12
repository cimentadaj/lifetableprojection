#' Data Input and Validation Module
#'
#' This module contains functions for reading, validating, and rendering data.

#' Read Data from Uploaded File
#'
#' Reads and returns data from the uploaded CSV file in Shiny.
#'
#' @param input Shiny input object containing the file input.
#' @return Data frame read from the uploaded CSV file.
#' @importFrom shiny req
#' @importFrom utils read.csv
#' @export
readData <- function(input) {
  req(input$file1)
  read.csv(input$file1$datapath)
}

#' Validate Uploaded Data
#'
#' Validates the uploaded data using specific criteria.
#'
#' @param data Data frame to be validated.
#' @return Data frame containing validation results.
#' @importFrom ODAPbackend check_data
#' @export
validateData <- function(data) {
  check_data(data)
}

#' Render Data Table
#'
#' Creates a read-only handsontable UI component for displaying data.
#'
#' @param data Data frame to be displayed in the table.
#' @return Shiny UI component for data table.
#' @importFrom rhandsontable rhandsontable
#' @importFrom utils head
#' @importFrom stats setNames
#' @export
renderDataTable <- function(data) {
  data <- data[!names(data) %in% c(".id", ".id_label")]
  sample_df <- head(data)
  etc_df <- setNames(replicate(ncol(sample_df), "   ...", simplify = FALSE), names(sample_df))
  rhandsontable(rbind(sample_df, as.data.frame(etc_df)), readOnly = TRUE)
}

#' Display Validation Results
#'
#' Renders UI elements to display the results of data validation.
#'
#' @param results Data frame with validation results.
#' @importFrom shiny wellPanel HTML
#' @importFrom tools toTitleCase
#' @export
displayValidationResults <- function(results) {
  if (all(results$pass == "Pass")) {
    wellPanel(class = "success", "\u2705 Everything is great, all checks passed!")
  } else {
    failed_checks <- results[results$pass != "Pass", ]
    wellPanel(
      class = "danger",
      HTML("Check Failures:<br>"),
      HTML(paste("\u274C", toTitleCase(failed_checks$message), collapse = "<br>"))
    )
  }
}


#' Validate Data After Group Selection
#'
#' Validates data and updates UI elements after group selection.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param data_in Reactive value containing input data
#' @param group_selection_passed Reactive value indicating if group selection is complete
#' @importFrom shiny observe renderUI div
#' @importFrom shiny.semantic action_button
#' @export
validate_data_after_group_selection <- function(input, output, data_in, group_selection_passed) {
  observe({
    if (group_selection_passed()) {
      check_results <- validate_data(data_in)
      output$validation_results <- renderUI(displayValidationResults(check_results()))

      output$forward_step2 <- renderUI({
        if (all(check_results()$pass == "Pass")) {
          div(
            action_button("diagnostics", "Diagnostics", class = "ui blue button"),
            action_button("forward_step", "Continue", class = "ui blue button")
          )
        }
      })
    }
  })
}
