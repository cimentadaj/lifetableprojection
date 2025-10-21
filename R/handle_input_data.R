#' Handle File Upload
#'
#' @param input Shiny input object
#' @param i18n Translator object for internationalization
#' @return Reactive expression containing the uploaded data
#' @importFrom shiny reactive
handle_file_upload <- function(input, i18n) {
  readData(input)
}

#' Handle Sample Data
#'
#' @param i18n Translator object for internationalization
#' @return Reactive expression containing the sample data
#' @importFrom shiny reactive
handle_sample_data <- function(i18n) {
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
  })
}

#' Handle Column Selection
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param data Reactive expression containing the data
#' @param group_selection_passed Reactive value to flag whether the group selection stage has been passed
#' @param selected_grouping_vars Reactive value to store the variables selected as grouping vars
#' @param i18n Translator object for internationalization
#' @importFrom shiny observeEvent req
#' @importFrom shinyalert shinyalert
handle_group_selection_modal <- function(input, output, session, data, group_selection_passed, selected_grouping_vars, i18n, data_raw = NULL, raw_key = NULL) {
  ns <- session$ns
  modal_id <- ns("column_selection_modal")

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
          id = modal_id,
          header = i18n$t("Column Selection"),
          content = div(
            strong(p(i18n$t("If you're analysis needs to be performed by groups (e.g Sex, Province, Region, etc..), please select the columns that group your data. If your data is not group-wise, tick the box below."))),
            br(),
            shiny.semantic::selectInput(
              ns("id_columns"),
              label = i18n$t("Select Identifier Columns"),
              choices = choices(),
              multiple = TRUE
            ),
            br(),
            shiny.semantic::checkbox_input(
              ns("skip_grouping"),
              i18n$t("No grouping needed for this analysis"),
              is_marked = FALSE
            ),
            br(),
            br(),
            br(),
            uiOutput(ns("modal_error_message"))
          ),
          footer = tagList(
            actionButton(ns("confirm_column_selection"), i18n$t("Confirm"), class = "ui button primary"),
            actionButton(ns("cancel_column_selection"), i18n$t("Cancel"), class = "ui button")
          )
        )
      })

      # Use session$onFlushed to ensure the UI is updated before showing the modal
      session$onFlushed(function() {
        shiny.semantic::show_modal(modal_id, session = session)
      }, once = TRUE)
    }
  })

  validate_groups <- function(selected_columns) {
    if (isTRUE(input$skip_grouping)) {
      return(TRUE)
    }
    if (length(selected_columns) >= 0) {
      library(dplyr)
      df <- data()
      if (is.null(df)) {
        return(FALSE)
      }
      cg <- tryCatch({
        ODAPbackend:::create_groupid(df, selected_columns)
      }, error = function(e) {
        return(e)
      })
      if (inherits(cg, "error")) {
        return(FALSE)
      }
      valid_groups <- tryCatch({
        ODAPbackend:::check_groupid(cg)
      }, error = function(e) {
        FALSE
      })
      valid_groups
    } else {
      FALSE
    }
  }

  # Observe the confirm button click
  observeEvent(input$confirm_column_selection, {
    selected_columns <- input$id_columns
    if (isTRUE(input$skip_grouping)) {
      selected_columns <- character(0)
    }

    if (isTRUE(input$skip_grouping)) {
      df <- NULL

      if (!is.null(data_raw)) {
        df <- tryCatch({
          data_raw()
        }, error = function(e) {
          NULL
        })
      }

      if (is.null(df)) {
        df <- tryCatch({
          data()
        }, error = function(e) {
          NULL
        })
      }

      if (is.null(df)) {
        env <- environment(data)
        if (!is.null(env) && exists("value", envir = env, inherits = FALSE)) {
          df <- get("value", envir = env, inherits = FALSE)
        }
      }

      if (is.null(df) && !is.null(raw_key)) {
        df <- session$userData[[raw_key]]
      }

      if (is.null(df)) {
        output$modal_error_message <- renderUI({
          HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>',
            i18n$t("We could not detect any data rows to tag. Please reload your dataset and try again.")))
        })
        return()
      }
      if (!is.data.frame(df)) {
        df <- as.data.frame(df)
      }
      df$`.id` <- 1L
      df$`.id_label` <- i18n$t("All records")
      data(df)
      group_selection_passed(TRUE)
      selected_grouping_vars(character(0))
      shiny.semantic::hide_modal(modal_id, session = session)
      return()
    }

    if (length(selected_columns) > 3) {
      output$modal_error_message <- renderUI({
        HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>', 
          i18n$t("A maximum of 3 grouping variables are allowed.")))
      })
    } else if (!validate_groups(selected_columns)) {
      output$modal_error_message <- renderUI({
        HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>', 
          i18n$t("The specified columns do not identify each row uniquely or we've identified there are grouping columns not specified.")))
      })
    } else {
      updated_data <- tryCatch({
        ODAPbackend:::create_groupid(data(), selected_columns)
      }, error = function(e) {
        e
      })
      if (inherits(updated_data, "error")) {
        output$modal_error_message <- renderUI({
          HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>', 
            i18n$t("We could not tag your groups with the selected columns. Please check your file structure and try again.")))
        })
        return()
      }
      data(updated_data)
      group_selection_passed(TRUE)
      selected_grouping_vars(selected_columns)
      shiny.semantic::hide_modal(modal_id)
    }
  })

  # Close the modal when the "Cancel" button is clicked
  observeEvent(input$cancel_column_selection, {
    shiny.semantic::hide_modal(modal_id, session = session)
  })
}
