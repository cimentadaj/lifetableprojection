#' Generate Diagnostic Plots
#'
#' @param data_in Reactive expression containing the input data
#' @return Reactive expression containing diagnostic plots
#' @importFrom dplyr %>%
#' @importFrom plotly config
generate_diagnostic_plots <- function(data_in) {
  reactive({
    req(data_in())
    # Split data by .id into groups
    groups <- group_split(data_in(), .id)

    # Initialize an empty list to store plots for each group
    group_plots <- list()

    # Loop over each group and apply plot_initial_data
    for (i in seq_along(groups)) {
      plts <- groups[[i]] %>% plot_initial_data()
      names(plts) <- to_snake(names(plts))

      # Convert each plot to ggplotly and configure displayModeBar
      plts <- lapply(plts, function(plt) {
        ggplt <- ggplotly(plt$figure, tooltip = c("y", "text"))
        config(ggplt, displayModeBar = FALSE)
      })

      # Store plots for this group in the list with the .id as the name
      group_plots[[as.character(groups[[i]]$.id[1])]] <- plts
    }

    # Return the list of plots for each group with .id as names
    group_plots
  })
}

#' Generate Diagnostics Text
#'
#' @param data_in Reactive expression containing the input data
#' @return Reactive expression containing diagnostics text
generate_diagnostics_text <- function(data_in) {
  reactive({
    req(data_in())
    is_single_ages <- all(diff(sort(data_in()$Age)) == 1)

    paste0(
      "The Roughness method measures the average absolute percentage deviation from a smoothed trend through the five-year age group data. The Sawtooth method takes the average of the ratios of the value in each five-year age group (in adult ages) to the average of the two adjacent age groups (age groups below and above). Both of these methods are trying to pick up on a phenomenon known as differential age heaping where digit preference is stronger on zeroes than on fives. This phenomenon can cause an apparent sawtooth pattern in demographic count data.",
      ifelse(is_single_ages, "The Myers and Bachi indices both measure digit preference for single-age data. If there were no digit preference, then the distribution over terminal digits (0-9) would be roughly uniform. Both of these indices measure the departure from uniformity, with slight variations on the implementation. Higher values indicate digit distributions that are farther from uniform. If these indices are high, but the _roughness_ and _sawtooth_ indices are low, then you might adjust data using one of the _fine_ smoothing methods offered.", "")
    )
  })
}

#' Generate Diagnostics Table
#'
#' @param data_in Reactive expression containing the input data
#' @return Reactive expression containing diagnostics table
#' @importFrom dplyr %>%
generate_diagnostics_table <- function(data_in) {
  reactive({
    req(data_in())

    # Split data by .id into groups
    groups <- group_split(data_in(), .id)

    # Initialize an empty list to store tables for each group
    group_tables <- list()

    # Loop over each group and generate the diagnostics table
    for (i in seq_along(groups)) {
      group_data <- groups[[i]]

      heaping_exposure <- check_heaping_general(group_data, "Exposures")
      heaping_deaths <- check_heaping_general(group_data, "Deaths")

      heaping_exposure$Type <- "Exposures"
      heaping_deaths$Type <- "Deaths"

      heaping_res <- rbind(heaping_exposure, heaping_deaths)
      heaping_res$result <- round(heaping_res$result, 2)

      heaping_res$method <- toTitleCase(heaping_res$method)
      df <- heaping_res[c("Type", "age scale", "method", "result", "level", "color")]
      df <- df[order(df$method), ]
      names(df) <- toTitleCase(names(df))

      # Store table for this group in the list with the .id as the name
      group_tables[[as.character(group_data$.id[1])]] <- df
    }

    # Return the list of tables for each group with .id as names
    group_tables
  })
}

#' Render Diagnostics Table
#'
#' @param diagnostics_table Reactive expression containing diagnostics table
#' @return Rendered DataTable
#' @importFrom DT datatable formatStyle styleEqual
render_diagnostics_table <- function(diagnostics_table) {
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
#' @importFrom shiny div br
#' @importFrom DT dataTableOutput
#' @importFrom shinyalert shinyalert
show_diagnostics_modal <- function(input, output, session, diagnostic_plots, diagnostics_table, diagnostics_text, grouping_dropdowns) {
  myContent <- div(
    id = "content-wrapper",
    style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
    div(
      class = "grouping-dropdowns-container",
      style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px; margin-bottom: 20px;",
      lapply(grouping_dropdowns(), function(dropdown) {
        div(
          style = "flex: 0 1 auto; min-width: 150px; max-width: 200px;",
          dropdown
        )
      })
    ),
    div(
      style = "display: flex; flex-direction: row; align-items: flex-start; width: 100%; justify-content: center;",
      div(
        class = "plot-container",
        style = "width: 55%;",
        tabset(
          list(
            list(
              menu = "Exposures",
              content = plotlyOutput("diag_exposures", width = "90%")
            ),
            list(
              menu = "Deaths",
              content = plotlyOutput("diag_deaths", width = "90%")
            ),
            list(
              menu = "Empirical Mx",
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
  shinyalert(title = "&#x1F50D Data Diagnostics", html = TRUE, size = "l", text = myContent)

  # Render diagnostic plots
  output$diag_exposures <- renderPlotly({ diagnostic_plots()$exposures })
  output$diag_deaths <- renderPlotly({ diagnostic_plots()$deaths })
  output$diag_empirical_mx <- renderPlotly({ diagnostic_plots()$empiricalmx })

  # Render diagnostics table
  output$diagnostics_table <- render_diagnostics_table(diagnostics_table)

  # Render diagnostics text
  output$diagnostics_text <- renderText({ diagnostics_text() })
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
#' @return List of reactive expressions for plots, table, and text
#' @importFrom shiny observeEvent
#' @export
setup_diagnostic_data <- function(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns) {
  # Generate diagnostic plots
  diagnostic_plots <- generate_diagnostic_plots(data_in)

  # Create reactive for current diagnostic plots
  current_diagnostic_plots <- create_current_diagnostic_plots(diagnostic_plots, selected_grouping_vars, data_in, input)

  # Generate diagnostics text
  diagnostics_text <- generate_diagnostics_text(data_in)

  # Generate diagnostics table
  diagnostics_table <- generate_diagnostics_table(data_in)

  # Create reactive for current diagnostics table
  current_diagnostics_table <- create_current_diagnostics_table(diagnostics_table, selected_grouping_vars, data_in, input)

  # Show diagnostics modal when the diagnostics button is clicked
  observeEvent(input$diagnostics, {
    show_diagnostics_modal(input, output, session, current_diagnostic_plots, current_diagnostics_table, diagnostics_text, grouping_dropdowns)
  })

  list(
    plots = current_diagnostic_plots,
    table = current_diagnostics_table,
    text = diagnostics_text
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
    req(diagnostic_plots(), selected_grouping_vars(), data_in())
    current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    diagnostic_plots()[[current_id]]
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
    req(diagnostics_table(), selected_grouping_vars(), data_in())
    current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    diagnostics_table()[[current_id]]
  })
}
