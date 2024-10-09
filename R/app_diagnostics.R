#' Generate Diagnostic Plots
#'
#' @param data_in Reactive expression containing the input data
#' @return Reactive expression containing diagnostic plots
#' @importFrom dplyr %>%
#' @importFrom plotly config
generate_diagnostic_plots <- function(data_in) {
  reactive({
    req(data_in())
    # TODO: REMOVE THIS ON THEY ADAPT THIS FUNcTION FOR several sexes
    plts <- data_in() %>% filter(Sex == "Male") %>% plot_initial_data()
    names(plts) <- to_snake(names(plts))

    lapply(plts, function(plt) {
      ggplt <- ggplotly(plt$figure, tooltip = c("y", "text"))
      config(ggplt, displayModeBar = FALSE)
    })
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
    heaping_exposure <- check_heaping_general(data_in(), "Exposures")
    heaping_deaths <- check_heaping_general(data_in(), "Deaths")

    heaping_exposure$Type <- "Exposures"
    heaping_deaths$Type <- "Deaths"

    heaping_res <- rbind(heaping_exposure, heaping_deaths)
    heaping_res$result <- round(heaping_res$result, 2)

    heaping_res$method <- toTitleCase(heaping_res$method)
    df <- heaping_res[c("Type", "age scale", "method", "result", "level", "color")]
    df <- df[order(df$method), ]
    names(df) <- toTitleCase(names(df))

    df
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
show_diagnostics_modal <- function(input, output, session, diagnostic_plots, diagnostics_table, diagnostics_text) {
  myContent <- div(
    id = "content-wrapper",
    style = "display: flex; flex-direction: row; align-items: flex-start;",
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
