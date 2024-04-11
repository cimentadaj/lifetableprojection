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