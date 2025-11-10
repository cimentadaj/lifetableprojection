#' Handle Transitions
#'
#' Manages the visibility of different sections in the Shiny app.
#'
#' @param input Shiny input object
#' @importFrom shinyjs hide show
#' @export
handle_transitions <- function(input, current_tab) {

  # Module navigation observer
  observeEvent(input$goto_lifetable, {
    hide("module_landing_page")
    show("lifetable_landing_page")
    current_tab("lifetable")
  })

  # Add back navigation if needed
  observeEvent(input$back_to_modules, {
    hide("lifetable_landing_page")
    show("module_landing_page")
    current_tab("module_landing")
  })

  observeEvent(input$lifetable_start_button, {
    hide("lifetable_landing_page")
    show("landing_page")
    current_tab("upload_page")
  })

  observeEvent(input$forward_step, {
    hide("landing_page")
    show("step_adjustment")
    current_tab("preprocessing_page")
  })

  observeEvent(input$back_to_diagnostics, {
    hide("step_adjustment")
    show("landing_page")
  })

  observeEvent(input$forward_to_lifetable, {
    hide("step_adjustment")
    show("step_input")
    current_tab("lifetable_page")
  })

  observeEvent(input$back_to_adjustment, {
    hide("step_input")
    show("step_adjustment")
  })

  # Add new transition
  observeEvent(input$back_to_lifetable_landing, {
    hide("landing_page")
    show("lifetable_landing_page")
    current_tab("lifetable")
  })

  observeEvent(input$goto_heaping, {
    hide("module_landing_page")
    hide("lifetable_landing_page")
    hide("landing_page")
    hide("step_adjustment")
    hide("step_input")
    show("heaping_module_page")
    current_tab("heaping")
  })

  observeEvent(input$heaping_back_to_modules, {
    hide("heaping_module_page")
    show("module_landing_page")
    current_tab("module_landing")
  })

  observeEvent(input$goto_smoothing, {
    hide("module_landing_page")
    hide("lifetable_landing_page")
    hide("landing_page")
    hide("step_adjustment")
    hide("step_input")
    hide("heaping_module_page")
    show("smoothing_module_page")
    current_tab("smoothing")
  })

  observeEvent(input$smoothing_back_to_modules, {
    hide("smoothing_module_page")
    show("module_landing_page")
    current_tab("module_landing")
  })

  observeEvent(input$goto_graduation, {
    hide("module_landing_page")
    hide("lifetable_landing_page")
    hide("landing_page")
    hide("step_adjustment")
    hide("step_input")
    hide("heaping_module_page")
    hide("smoothing_module_page")
    show("graduation_module_page")
    current_tab("graduation")
  })

  observeEvent(input$graduation_back_to_modules, {
    hide("graduation_module_page")
    show("module_landing_page")
    current_tab("module_landing")
  })
}


#' Setup Grouping Dropdowns
#'
#' Creates reactive dropdowns for group selection.
#'
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @param data_in Reactive value containing input data
#' @return Reactive expression containing dropdown inputs
#' @importFrom shiny reactive req
#' @importFrom shiny.semantic selectInput
#' @export
setup_grouping_dropdowns <- function(selected_grouping_vars, data_in, ns = function(x) x) {
  reactive({
    req(data_in())

    if (length(selected_grouping_vars()) == 0) return()

    cat(sprintf(
      "[GROUP_DROPDOWNS] building column dropdowns for vars: %s\n",
      paste(selected_grouping_vars(), collapse = ", ")
    ))

    lapply(selected_grouping_vars(), function(var) {
      unique_values <- unique(data_in()[[var]])
      cat(sprintf("[GROUP_DROPDOWNS] %s unique values: %s\n", var, paste(unique_values, collapse = ", ")))
      selectInput(
        inputId = ns(paste0("group_select_", var)),
        label = var,
        choices = unique_values,
        selected = unique_values[1]
      )
    })
  })
}

#' Setup Grouping Dropdown Observers
#'
#' Sets up observers for grouping dropdown changes.
#'
#' @param input Shiny input object
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @importFrom shiny observe req observeEvent
#' @export
setup_grouping_dropdown_observers <- function(input, selected_grouping_vars) {
  observe({
    req(selected_grouping_vars())
    lapply(selected_grouping_vars(), function(var) {
      observeEvent(input[[paste0("group_select_", var)]], {
        # This will trigger a re-evaluation of current_diagnostic_plots
        cat(sprintf("[GROUP_DROPDOWN] %s -> %s\n", var, input[[paste0("group_select_", var)]]))
      }, ignoreInit = TRUE)
    })
  })
}

#' Get Current Group ID
#'
#' Retrieves the current group ID based on selected dropdown values.
#'
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @param data_in Reactive value containing input data
#' @param input Shiny input object
#' @return Character string representing the current group ID
#' @importFrom dplyr %>% distinct
#' @importFrom utils type.convert
#' @export
get_current_group_id <- function(selected_grouping_vars, data_in, input) {
  df <- data_in()
  if (is.null(df) || !".id" %in% names(df)) {
    cat("[GROUP_ID] No .id column present; returning NULL\n")
    return(NULL)
  }

  vars <- selected_grouping_vars()
  if (length(vars) == 0) {
    manual <- input$group_id_select
    if (!is.null(manual) && length(manual) > 0) {
      manual_value <- utils::type.convert(manual, as.is = TRUE)
      cat(sprintf("[GROUP_ID] manual group selection -> %s\n", as.character(manual_value)))
      return(manual_value)
    }
    ids <- unique(df$.id)
    gid <- if (length(ids) > 0) ids[[1]] else NA
    cat(sprintf("[GROUP_ID] No grouping vars selected; fallback gid = %s\n", as.character(gid)))
    return(gid)
  }

  current_values <- sapply(vars, function(var) input[[paste0("group_select_", var)]], simplify = TRUE)
  cat(sprintf("[GROUP_ID] current selections: %s\n", paste(sprintf("%s=%s", vars, current_values), collapse = ", ")))

  filtered <- df
  for (var in vars) {
    val <- input[[paste0("group_select_", var)]]
    if (!is.null(val)) {
      filtered <- filtered[filtered[[var]] == val, , drop = FALSE]
    }
  }

  ids <- unique(filtered$.id)
  if (length(ids) == 0) {
    cat("[GROUP_ID] No rows matched selections; falling back to first available id\n")
    ids <- unique(df$.id)
  }
  gid <- if (length(ids) > 0) ids[[1]] else NA
  cat(sprintf("[GROUP_ID] resolved gid: %s (available ids: %s)\n", as.character(gid), paste(unique(df$.id), collapse = ", ")))
  gid
}
