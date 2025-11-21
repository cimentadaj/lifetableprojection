#' Data Input and Validation Module
#'
#' This module contains functions for reading, validating, and rendering data.

#' Read Data from Uploaded File
#'
#' Reads and returns data from the uploaded CSV file in Shiny.
#'
#' @param input Shiny input object containing the file input.
#' @param i18n Translator object for internationalization
#' @return Data frame read from the uploaded CSV file.
#' @importFrom shiny req
#' @importFrom utils read.csv
#' @export
readData <- function(input, i18n) {
  req(input$file1)
  read.csv(input$file1$datapath)
}

#' Validate Uploaded Data
#'
#' Validates the uploaded data using specific criteria.
#'
#' @param data Data frame to be validated.
#' @param i18n Translator object for internationalization
#' @return Data frame containing validation results.
#' @importFrom ODAPbackend check_data
#' @export
validateData <- function(data, i18n) {
  check_data(data)
}

#' Validate ODAP Population Data
#'
#' Validates ODAP population data using ODAP-specific criteria.
#'
#' @param data Data frame to be validated (must contain Age and pop columns).
#' @param i18n Translator object for internationalization
#' @return Data frame containing validation results.
#' @export
validateData_opag <- function(data, i18n) {
  ODAPbackend::check_data_opag(data)
}

#' Render Data Table
#'
#' Creates a read-only handsontable UI component for displaying data.
#'
#' @param data Data frame to be displayed in the table.
#' @param i18n Translator object for internationalization
#' @return Shiny UI component for data table.
#' @importFrom rhandsontable rhandsontable
#' @importFrom utils head
#' @importFrom stats setNames
#' @export
renderDataTable <- function(data, i18n) {
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
#' @param i18n Translator object for internationalization
#' @importFrom shiny wellPanel HTML
#' @importFrom tools toTitleCase
#' @export
displayValidationResults <- function(results, i18n) {
  if (is.null(results) || nrow(results) == 0) {
    return(NULL)
  }

  if (all(results$pass == "Pass")) {
    shiny::wellPanel(
      class = "success",
      style = "text-align: center;",
      i18n$t("✅ Everything is great, all checks passed!")
    )
  } else {
    failed_checks <- results[results$pass != "Pass", ]
    shiny::wellPanel(
      class = "danger",
      HTML(paste0(i18n$t("Check Failures:"), "<br>")),
      HTML(paste("\u274C", i18n$t(toTitleCase(failed_checks$message)), collapse = "<br>"))
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
#' @param i18n Translator object for internationalization
#' @importFrom shiny observe renderUI div
#' @importFrom shiny.semantic action_button
#' @export
validate_data_after_group_selection <- function(input, output, data_in, group_selection_passed, i18n) {
  observe({
    if (group_selection_passed()) {
      check_results <- validate_data(data_in)

      output$validation_results <- renderUI({
        i18n <- usei18n_local()
        input$selected_language
        displayValidationResults(check_results(), i18n)
      })

      output$forward_step2 <- renderUI({
        i18n <- usei18n_local()
        input$selected_language

        if (all(check_results()$pass == "Pass")) {
          div(
            action_button("diagnostics", i18n$t("Diagnostics"), class = "ui blue button"),
            action_button("forward_step", i18n$t("Continue"), class = "ui blue button")
          )
        }
      })
    }
  })
}
