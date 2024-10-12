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
#' @importFrom shiny observeEvent req
#' @importFrom shinyalert shinyalert
handle_group_selection_modal <- function(input, output, session, data, group_selection_passed, selected_grouping_vars) {
  observeEvent(input$file1, {
    # Create the multi-select input for identifier columns
    column_selector <- selectInput(
      "id_columns",
      label = "Select Identifier Columns",
      choices = names(data()),
      multiple = TRUE,
      ## selectize = TRUE
    )

    # Create the content for shinyalert
    alert_content <- div(
      column_selector,
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    )

    column_selection_modal <- function(extra_content = "") {
      shinyalert(
        title = "Column Selection",
        text = paste0(alert_content, "<br/>", "<br/>", extra_content),
        html = TRUE,
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showConfirmButton = TRUE,
        confirmButtonText = "Confirm",
      )
    }

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

    print("valid groups")
    print(validate_groups())

    if (!validate_groups()) {
      print("passed here")
      column_selection_modal()
    }

    observeEvent(input$shinyalert, {
      if (length(input$id_columns) > 3) {
        column_selection_modal(
          extra_content = '<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;"> A maximum of 3 grouping variables are allowed </p>'
        )
      } else if (!validate_groups()) {
        column_selection_modal(
          extra_content = '<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;"> The specified columns do not identify each row uniquely </p>'
        )
      }
    })

    observeEvent(input$shinyalert, {
      if (validate_groups() & length(input$id_columns) < 4) {
        data(ODAPbackend:::create_groupid(data(), input$id_columns))
        group_selection_passed(TRUE)
        selected_grouping_vars(input$id_columns)  # Store the selected grouping variables
      }
    })

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
