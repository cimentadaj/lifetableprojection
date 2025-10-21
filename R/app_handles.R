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
setup_grouping_dropdowns <- function(selected_grouping_vars, data_in) {
  reactive({
    req(data_in())

    if (length(selected_grouping_vars()) == 0) return()

    lapply(selected_grouping_vars(), function(var) {
      unique_values <- unique(data_in()[[var]])
      selectInput(
        inputId = paste0("group_select_", var),
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
#' @export
get_current_group_id <- function(selected_grouping_vars, data_in, input) {
  current_selections <- sapply(selected_grouping_vars(), function(var) {
    input[[paste0("group_select_", var)]]
  })
  current_id_label <- paste(current_selections, collapse = " - ")
  labels_df <- data_in() %>% distinct(`.id`, `.id_label`)
  as.character(labels_df$`.id`[labels_df$`.id_label` == current_id_label])
}
