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

  log_dataset <- function(tag, df, n_show = 3) {
    if (is.null(df)) {
      cat(sprintf("[%s] Dataset is NULL\n", tag))
      return()
    }
    classes <- paste(class(df), collapse = ", ")
    cols <- if (!is.null(colnames(df))) paste(colnames(df), collapse = ", ") else "<no colnames>"
    cat(sprintf("[%s] class: %s | rows: %s | cols: %s | names: %s\n",
      tag,
      classes,
      ifelse(is.null(nrow(df)), "NA", nrow(df)),
      ifelse(is.null(ncol(df)), "NA", ncol(df)),
      cols
    ))
    if (nrow(df) > 0) {
      preview <- capture.output(print(utils::head(df, n_show)))
      cat(sprintf("[%s] head:\n%s\n", tag, paste(preview, collapse = "\n")))
    }
  }

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
    log_dataset("[GROUP_MODAL] data() observer triggered with dataset", data())

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
        cat("[GROUP_MODAL] Triggering modal display via show_modal\n")
        shiny.semantic::show_modal(modal_id, session = session)
      }, once = TRUE)
    }
  })

  validate_groups <- function(selected_columns) {
    if (isTRUE(input$skip_grouping)) {
      cat("[GROUP_MODAL] validate_groups -> skip grouping requested\n")
      return(TRUE)
    }
    cat("[GROUP_MODAL] validate_groups -> selected columns:", paste(selected_columns, collapse = ", "), "\n")
    if (length(selected_columns) >= 0) {
      library(dplyr)
      df <- data()
      cat("[GROUP_MODAL] validate_groups -> data() class:", paste(class(df), collapse = ", "), " | type:", typeof(df), "\n")
      if (is.null(df)) {
        cat("[GROUP_MODAL] validate_groups -> data() is NULL\n")
        return(FALSE)
      }
      if (is.list(df) && !inherits(df, "data.frame")) {
        cat("[GROUP_MODAL] validate_groups -> converting list to data.frame attempt\n")
      }
      cg <- tryCatch({
        ODAPbackend:::create_groupid(df, selected_columns)
      }, error = function(e) {
        cat("[GROUP_MODAL][ERROR] create_groupid failed:", conditionMessage(e), "\n")
        return(e)
      })
      if (inherits(cg, "error")) {
        return(FALSE)
      }
      cat("[GROUP_MODAL] validate_groups -> create_groupid succeeded | cols:", paste(colnames(cg), collapse = ", "), "\n")
      valid_groups <- tryCatch({
        ODAPbackend:::check_groupid(cg)
      }, error = function(e) {
        cat("[GROUP_MODAL][ERROR] check_groupid failed:", conditionMessage(e), "\n")
        FALSE
      })
      cat("[GROUP_MODAL] validate_groups -> check_groupid result:", valid_groups, "\n")
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

    cat("[GROUP_MODAL] confirm -> selected columns after skip handling:", paste(selected_columns, collapse = ", "), "\n")

    if (isTRUE(input$skip_grouping)) {
      cat("[GROUP_MODAL] confirm -> skip mode, tagging entire dataset as one group\n")
      df <- NULL

      if (!is.null(data_raw)) {
        df <- tryCatch({
          data_raw()
        }, error = function(e) {
          cat("[GROUP_MODAL][ERROR] confirm -> data_raw() evaluation failed:", conditionMessage(e), "\n")
          NULL
        })
        log_dataset("[GROUP_MODAL] confirm -> data_raw() result", df)
      }

      if (is.null(df)) {
        df <- tryCatch({
          data()
        }, error = function(e) {
          cat("[GROUP_MODAL][ERROR] confirm -> data() evaluation failed:", conditionMessage(e), "\n")
          NULL
        })
        log_dataset("[GROUP_MODAL] confirm -> data() result after raw fallback", df)
      }

      if (is.null(df)) {
        cat("[GROUP_MODAL] confirm -> checking reactiveVal environment store\n")
        env <- environment(data)
        if (!is.null(env) && exists("value", envir = env, inherits = FALSE)) {
          df <- get("value", envir = env, inherits = FALSE)
        }
        log_dataset("[GROUP_MODAL] confirm -> environment fallback result", df)
      }

      if (is.null(df) && !is.null(raw_key)) {
        cat(sprintf("[GROUP_MODAL] confirm -> checking session$userData[%s]\n", raw_key))
        df <- session$userData[[raw_key]]
        log_dataset("[GROUP_MODAL] confirm -> session$userData fallback result", df)
      }

      if (is.null(df)) {
        cat("[GROUP_MODAL][ERROR] confirm -> dataset still NULL after all recovery attempts\n")
        output$modal_error_message <- renderUI({
          HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>',
            i18n$t("We could not detect any data rows to tag. Please reload your dataset and try again.")))
        })
        return()
      }
      if (!is.data.frame(df)) {
        cat("[GROUP_MODAL] confirm -> coercing recovered data to data.frame from class:", paste(class(df), collapse = ", "), "\n")
        df <- as.data.frame(df)
      }
      log_dataset("[GROUP_MODAL] confirm -> dataset ready for tagging", df)
      df$`.id` <- 1L
      df$`.id_label` <- i18n$t("All records")
      log_dataset("[GROUP_MODAL] confirm -> dataset after tagging", df)
      cat("[GROUP_MODAL] confirm -> skip mode tagging complete | rows:", nrow(df), "cols:", ncol(df), "\n")
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
      cat("[GROUP_MODAL] confirm -> creating group ids\n")
      updated_data <- tryCatch({
        ODAPbackend:::create_groupid(data(), selected_columns)
      }, error = function(e) {
        cat("[GROUP_MODAL][ERROR] create_groupid during confirm failed:", conditionMessage(e), "\n")
        e
      })
      if (inherits(updated_data, "error")) {
      output$modal_error_message <- renderUI({
          HTML(sprintf('<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;">%s</p>', 
            i18n$t("We could not tag your groups with the selected columns. Please check your file structure and try again.")))
        })
        return()
      }
      cat("[GROUP_MODAL] confirm -> data tagged successfully | rows:", nrow(updated_data), "cols:", ncol(updated_data), "\n")
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
