#' Generate Diagnostic Plots
#'
#' @param data_in Reactive expression containing the input data
#' @param group_selection_passed Reactive expression containing the group selection status
#' @param i18n Translator object for internationalization
#' @param lazy Boolean indicating whether to use lazy loading
#' @return Reactive expression containing diagnostic plots
#' @importFrom dplyr %>% group_split
#' @importFrom ODAPbackend plot_initial_data check_heaping_general
#' @importFrom plotly config
generate_diagnostic_plots <- function(data_in, group_selection_passed, selected_grouping_vars, input, i18n, lazy = TRUE) {
  if (lazy) {
    reactive({
      req(group_selection_passed())
      current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
      group_data <- data_in() %>% filter(.id == current_id)
      plts_original <- group_data %>% plot_initial_data(i18n = i18n)
      names(plts_original) <- to_snake(names(plts_original))

      # Convert translated plots to plotly
      plts <- lapply(names(plts_original), function(plot_name) {
        plt_obj <- plts_original[[plot_name]]
        # Convert to plotly while preserving translations
        ggplt <- ggplotly(plt_obj$figure, tooltip = c("y", "text"))
        config(ggplt, displayModeBar = FALSE)
      })
      names(plts) <- names(plts_original)

      list(
        plotly = plts,
        ggplot = plts_original
      )
    })
  } else {
    reactive({
      req(group_selection_passed())
      message("Starting parallel diagnostic plot generation...")

      # Split data by groups
      groups <- group_split(data_in(), .id)
      n_cores <- get_n_cores()  # Use the new function to determine cores

      # Stage 1: Parallel data preparation
      message("Stage 1: Parallel data preparation...")
      prepared_data <- parallel::mclapply(
        groups,
        prepare_group_data,
        mc.cores = n_cores
      )
      message("Data preparation complete")

      # Stage 2: Parallel plot generation
      message("Stage 2: Parallel plot generation...")
      plot_results <- parallel::mclapply(
        prepared_data,
        function(data) generate_group_plots(data, i18n),  # Pass i18n to generate_group_plots
        mc.cores = n_cores
      )
      message("Plot generation complete")

      # Combine results (skip plotly conversion for download)
      group_plots_ggplot <- setNames(
        lapply(plot_results, function(x) x$plots),
        sapply(plot_results, function(x) x$id)
      )

      message("Diagnostic plot generation complete")

      list(
        plotly = NULL,  # Not needed for download
        ggplot = group_plots_ggplot
      )
    })
  }
}

# Helper function for parallel data preparation
prepare_group_data <- function(group) {
  list(
    id = as.character(group$.id[1]),
    data = group
  )
}

# Helper function for parallel plot generation
generate_group_plots <- function(group_data) {
  plts_original <- group_data$data %>% plot_initial_data()
  names(plts_original) <- to_snake(names(plts_original))

  list(
    id = group_data$id,
    plots = plts_original
  )
}

#' Generate Diagnostics Text
#'
#' @param data_in Reactive expression containing the input data
#' @param i18n Translator object for internationalization
#' @return Reactive expression containing diagnostics text
generate_diagnostics_text <- function(data_in, i18n) {
  reactive({
    req(data_in())
    is_single_ages <- all(diff(sort(data_in()$Age)) == 1)

    paste0(
      i18n$t("The Roughness method measures the average absolute percentage deviation from a smoothed trend through the five-year age group data. The Sawtooth method takes the average of the ratios of the value in each five-year age group (in adult ages) to the average of the two adjacent age groups (age groups below and above). Both of these methods are trying to pick up on a phenomenon known as differential age heaping where digit preference is stronger on zeroes than on fives. This phenomenon can cause an apparent sawtooth pattern in demographic count data."),
      ifelse(is_single_ages, i18n$t("The Myers and Bachi indices both measure digit preference for single-age data. If there were no digit preference, then the distribution over terminal digits (0-9) would be roughly uniform. Both of these indices measure the departure from uniformity, with slight variations on the implementation. Higher values indicate digit distributions that are farther from uniform. If these indices are high, but the _roughness_ and _sawtooth_ indices are low, then you might adjust data using one of the _fine_ smoothing methods offered."), "")
    )
  })
}

#' Generate Diagnostics Table
#'
#' @param data_in Reactive expression containing the input data
#' @param i18n Translator object for internationalization
#' @return Reactive expression containing diagnostics table
#' @importFrom dplyr %>% group_split
generate_diagnostics_table <- function(data_in, group_selection_passed, i18n) {
  reactive({
    if (group_selection_passed()) {
      # Split data by .id into groups
      groups <- group_split(data_in(), .id)

      # Initialize an empty list to store tables for each group
      group_tables <- list()

      # Loop over each group and generate the diagnostics table
      for (i in seq_along(groups)) {
        group_data <- groups[[i]]

        heaping_exposure <- check_heaping_general(group_data, "Exposures")
        heaping_deaths <- check_heaping_general(group_data, "Deaths")

        heaping_exposure$Type <- i18n$t("Exposures")
        heaping_deaths$Type <- i18n$t("Deaths")

        heaping_res <- rbind(heaping_exposure, heaping_deaths)
        heaping_res$result <- round(heaping_res$result, 2)

        heaping_res$method <- toTitleCase(heaping_res$method)
        df <- heaping_res[c("Type", "age scale", "method", "result", "level", "color")]
        df <- df[order(df$method), ]
        names(df) <- toTitleCase(names(df))
        names(df) <- i18n$t(names(df))

        # Store table for this group in the list with the .id as the name
        group_tables[[as.character(group_data$.id[1])]] <- df
      }

      # Return the list of tables for each group with .id as names
      group_tables
    }
  })
}

#' Render Diagnostics Table
#'
#' @param diagnostics_table Reactive expression containing diagnostics table
#' @param i18n Translator object for internationalization
#' @return Rendered DataTable
#' @importFrom DT datatable formatStyle styleEqual
render_diagnostics_table <- function(diagnostics_table, i18n) {
  renderDT({
    df <- diagnostics_table()
    df <- df[, names(df) != "Color"]
    datatable(
      df,
      options = list(
        dom = "t",
        paging = FALSE,
        initComplete = DT::JS(
          "function(settings, json) {",
          "$('.dataTable th').css({'font-size': '15px'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        columns = "Result"
        ## backgroundColor = styleEqual(df$Result, df$Color)
      ) %>%
      formatStyle(columns = colnames(df), fontSize = "90%")
  })
}

#' Show Diagnostics Modal
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param diagnostic_plots Reactive expression containing diagnostic plots
#' @param diagnostics_table Reactive expression containing diagnostics table
#' @param diagnostics_text Reactive expression containing diagnostics text
#' @param grouping_dropdowns Reactive expression containing the drop down menus for the selected group keys
#' @param i18n Translator object for internationalization
#' @importFrom shiny div br
#' @importFrom DT dataTableOutput
#' @importFrom shinyalert shinyalert
show_diagnostics_modal <- function(input, output, session, diagnostic_plots, diagnostics_table, diagnostics_text, grouping_dropdowns, i18n) {
  myContent <- div(
    id = "content-wrapper",
    style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
    ## Only show grouping dropdowns if grouping vars are selected
    if (!is.null(grouping_dropdowns())) {
      div(
        class = "grouping-dropdowns-container",
        style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px; margin-bottom: 20px;",
        lapply(grouping_dropdowns(), function(dropdown) {
          div(
            style = "flex: 0 1 auto; min-width: 150px; max-width: 200px;",
            dropdown
          )
        })
      )
    },
    div(
      style = "display: flex; flex-direction: row; align-items: flex-start; width: 100%; justify-content: center;",
      div(
        class = "plot-container",
        style = "width: 55%;",
        tabset(
          list(
            list(
              menu = i18n$t("Exposures"),
              content = plotlyOutput("diag_exposures", width = "90%")
            ),
            list(
              menu = i18n$t("Deaths"),
              content = plotlyOutput("diag_deaths", width = "90%")
            ),
            list(
              menu = i18n$t("Empirical Mx"),
              content = plotlyOutput("diag_empirical_mx", width = "90%")
            )
          )
        )
      ),
      div(
        class = "table-container",
        style = "padding-left: 1%; width: 40%; max-height: 400px; overflow-y: auto;",
        dataTableOutput("diagnostics_table"),
        br(),
        div(
          style = "font-style: italic; font-size: 12px; ",
          textOutput("diagnostics_text")
        )
      )
    )
  )

  # Show the alert with the custom content
  shinyalert(title = paste0("&#x1F50D ", i18n$t("Data Diagnostics")), html = TRUE, size = "l", text = myContent)

  # Render diagnostic plots
  output$diag_exposures <- renderPlotly({
    diagnostic_plots()$exposures
  })
  output$diag_deaths <- renderPlotly({
    diagnostic_plots()$deaths
  })
  output$diag_empirical_mx <- renderPlotly({
    diagnostic_plots()$empiricalmx
  })

  # Render diagnostics table
  output$diagnostics_table <- render_diagnostics_table(diagnostics_table, i18n)

  # Render diagnostics text
  output$diagnostics_text <- renderText({
    diagnostics_text()
  })
}

#' Setup Diagnostic Data
#'
#' Prepares and manages all diagnostic data for the Shiny app.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param data_in Reactive value containing input data
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @param grouping_dropdowns Reactive expression containing grouping dropdowns
#' @param i18n Translator object for internationalization
#' @param show_modal Boolean indicating whether to show the diagnostics modal
#' @param download Boolean indicating whether to use lazy loading
#' @return List of reactive expressions for plots, table, and text
#' @importFrom shiny observeEvent
#' @export
setup_diagnostic_data <- function(input, output, session, data_in, group_selection_passed, selected_grouping_vars, grouping_dropdowns, i18n, show_modal = TRUE, download = FALSE) {
  # For downloading, use legacy behavior
  if (download) {
    diagnostic_plots <- generate_diagnostic_plots(data_in, group_selection_passed, selected_grouping_vars, input, i18n, lazy = FALSE)
    current_diagnostic_plots <- create_current_diagnostic_plots(diagnostic_plots()$plotly, selected_grouping_vars, data_in, input)
  } else {
    # For interactive viewing, use lazy loading
    diagnostic_plots <- generate_diagnostic_plots(data_in, group_selection_passed, selected_grouping_vars, input, i18n, lazy = TRUE)
    current_diagnostic_plots <- reactive({
      plots <- diagnostic_plots()$plotly
      list(
        exposures = plots$exposures,
        deaths = plots$deaths,
        empiricalmx = plots$empiricalmx
      )
    })
  }

  # Generate diagnostics text and table (these are lightweight operations)
  diagnostics_text <- generate_diagnostics_text(data_in, i18n)
  diagnostics_table <- generate_diagnostics_table(data_in, group_selection_passed, i18n)
  current_diagnostics_table <- create_current_diagnostics_table(diagnostics_table, selected_grouping_vars, data_in, input)

  if (show_modal) {
    show_diagnostics_modal(input, output, session, current_diagnostic_plots, current_diagnostics_table, diagnostics_text, grouping_dropdowns, i18n)
  }

  list(
    plots = current_diagnostic_plots,
    table = current_diagnostics_table,
    text = diagnostics_text,
    all_plots = diagnostic_plots,
    all_tables = diagnostics_table
  )
}

#' Create Current Diagnostic Plots
#'
#' Creates a reactive expression for the current diagnostic plots.
#'
#' @param diagnostic_plots Reactive expression containing all diagnostic plots
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @param data_in Reactive value containing input data
#' @param input Shiny input object
#' @return Reactive expression for current diagnostic plots
#' @importFrom shiny reactive req
#' @export
create_current_diagnostic_plots <- function(diagnostic_plots, selected_grouping_vars, data_in, input) {
  reactive({
    req(diagnostic_plots, data_in())
    # If no grouping vars selected, use default group "1"
    if (is.null(selected_grouping_vars())) {
      current_id <- "1"
    } else {
      current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    }
    diagnostic_plots[[current_id]]
  })
}

#' Create Current Diagnostics Table
#'
#' Creates a reactive expression for the current diagnostics table.
#'
#' @param diagnostics_table Reactive expression containing all diagnostics tables
#' @param selected_grouping_vars Reactive value containing selected grouping variables
#' @param data_in Reactive value containing input data
#' @param input Shiny input object
#' @return Reactive expression for current diagnostics table
#' @importFrom shiny reactive req
#' @export
create_current_diagnostics_table <- function(diagnostics_table, selected_grouping_vars, data_in, input) {
  reactive({
    req(diagnostics_table(), data_in())
    # If no grouping vars selected, use default group "1"
    if (is.null(selected_grouping_vars())) {
      current_id <- "1"
    } else {
      current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    }
    diagnostics_table()[[current_id]]
  })
}

#' Preprocess Diagnostic Data for Report
#'
#' Evaluates reactive expressions and restructures diagnostic data for report generation
#'
#' @param diagnostic_analysis The diagnostic analysis object containing reactive expressions
#' @return List containing preprocessed diagnostic data ready for report generation
#' @noRd
preprocess_diagnostic_data <- function(diagnostic_analysis) {
  # Initialize the result structure
  result <- list()
  
  # Get all plots by evaluating the reactive expression
  all_plots <- diagnostic_analysis$all_plots()
  
  # Extract and restructure ggplot objects
  if (!is.null(all_plots$ggplot)) {
    result$all_plots <- list()
    result$all_plots$ggplot <- list()
    
    # For each group
    for (group_id in names(all_plots$ggplot)) {
      plot_data <- all_plots$ggplot[[group_id]]
      
      # Initialize group in result if it has valid plot data
      if (!is.null(plot_data)) {
        # For each plot type (exposures, deaths, empiricalmx)
        for (plot_type in names(plot_data)) {
          if (!is.null(plot_data[[plot_type]]$figure) && 
              inherits(plot_data[[plot_type]]$figure, "ggplot")) {
            # Create the plot type entry if it doesn't exist
            if (!plot_type %in% names(result$all_plots$ggplot)) {
              result$all_plots$ggplot[[plot_type]] <- list()
            }
            # Store the figure under the plot type, indexed by group
            result$all_plots$ggplot[[plot_type]][[group_id]] <- list(
              figure = plot_data[[plot_type]]$figure
            )
          }
        }
      }
    }
  }
  
  # Get all tables by evaluating the reactive expression
  if (is.function(diagnostic_analysis$all_tables)) {
    result$all_tables <- diagnostic_analysis$all_tables()
  } else {
    result$all_tables <- diagnostic_analysis$all_tables
  }
  
  return(result)
} 
