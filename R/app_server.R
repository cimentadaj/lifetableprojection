to_snake <- function(x) tolower(gsub(" ", "", x))

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
      HTML(paste("\u274C", toTitleCase(failed_checks$check), collapse = "<br>"))
    )
  }
}

#' Setup Download Handlers
#'
#' Sets up download handlers for plots and data in Shiny.
#'
#' @param output Shiny output object.
#' @param plots List of reactive expressions for plots.
#' @param data data to be saved in the download button
#' @param input Shiny input object.
#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
#' @export
setupDownloadHandlers <- function(output, plots, data, input) {
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(to_snake(input$tabset), "plot.png", sep = "_")
    },
    content = function(file) {
      plot <- plots[[input$tabset]]()$gg
      ggsave(file, plot)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      "lifetable_data.csv"
    },
    content = function(file) {
      write.csv(data()$lt$lt, file, row.names = FALSE)
    }
  )
}

#' Calculate Life Table
#'
#' Performs life table calculations based on user input and the uploaded data.
#'
#' @param data_in Data frame containing mortality data.
#' @param input List of parameters from Shiny input.
#' @return List containing life table calculation results.
#' @importFrom ODAPbackend lt_flexible
#' @export
calculateLifeTable <- function(data_in, input) {
  req(input$calculate_lt)
  input_extrapfrom <- as.numeric(input$input_extrapFrom)

  begin_age <- which(data_in$Age == input$slider_ages_to_use[1])
  end_age <- which(data_in$Age == input$slider_ages_to_use[2])
  ages_to_use <- data_in$Age[begin_age:end_age]

  library(ggplot2)

  lt_res <- lt_flexible(
    data_in = data_in,
    OAnew = as.numeric(input$input_oanew),
    age_out = input$input_age_out,
    extrapFrom = input_extrapfrom,
    extrapFit = ages_to_use,
    extrapLaw = input$input_extrapLaw,
    radix = as.numeric(input$input_radix),
    SRB = as.numeric(input$input_srb),
    a0rule = input$input_a0rule,
    axmethod = input$input_axmethod,
    Sex = input$input_sex
  )

  list(lt = lt_res, extrapfrom = input_extrapfrom)
}

#' Generate Plotly Plot
#'
#' Creates a Plotly plot based on the life table calculation results.
#'
#' @param data Data frame of original data.
#' @param results Results from the life table calculation.
#' @return Plotly plot object.
#' @importFrom plotly renderPlotly
#' @export
generatePlot <- function(data, results) {
  req(results)
  plot_compare_rates_interactive(data, results)
}

#' Shiny Server Function
#'
#' Defines the server logic for the Shiny application, managing data processing, UI rendering, and user interactions.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @importFrom shiny renderUI observeEvent eventReactive actionButton reactive reactiveVal sliderInput observe
#' @importFrom shiny.semantic tabset icon updateSelectInput update_numeric_input updateSliderInput
#' @importFrom shinyjs show hide
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom ggplot2 element_blank theme geom_line
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyalert shinyalert
#' @importFrom stats reshape quantile
#' @importFrom DT datatable renderDT dataTableOutput
#' @importFrom ODAPbackend plot_initial_data check_heaping_general
#' @export
app_server <- function(input, output, session) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  dt_ex <- system.file("data/abridged_data.csv", package = "lifetableprojection")

  # Initialize a reactive value to store the data if csv if uploaded
  # or if sample data button is clciked.
  data_in <- reactiveVal()

  # Update data_in when a file is uploaded
  observe({
    data_in(readData(input))
  })

  # Update data_in when the button is clicked
  observeEvent(input$continue_no_data, {
    data_in(read.csv(dt_ex))
  })

  check_results <- reactive({
    # Since nor the cSV file has been loaded nor the sample data
    # has been clicked, we need to wait.
    req(data_in())
    validateData(data_in())
  })

  output$data_table <- renderRHandsontable(renderDataTable(read.csv(dt_ex)))
  output$validation_results <- renderUI(displayValidationResults(check_results()))

  output$forward_step2 <- renderUI({
    if (all(check_results()$pass == "Pass")) {
      div(
        actionButton("diagnostics", "Diagnostics", class = "ui blue button"),
        actionButton("forward_step", "Continue", class = "ui blue button")
      )
    }
  })

  diagnostic_plt <- reactive({
    # TODO
    library(dplyr)
    library(ggplot2)
    plts <- plot_initial_data(data_in())
    names(plts) <- to_snake(names(plts))
    plts
  })

  output$diag_exposures <- renderPlotly({
    fig <- diagnostic_plt()$exposures$figure
    dt <- diagnostic_plt()$exposures$data
    ggplotly(fig, tooltip = c("y", "text"))
  })

  output$diag_deaths <- renderPlotly({
    fig <- diagnostic_plt()$deaths$figure
    dt <- diagnostic_plt()$deaths$data
    ggplotly(fig, tooltip = c("y", "text"))
  })

  output$diag_empirical_mx <- renderPlotly({
    fig <- diagnostic_plt()$empiricalmx$figure
    dt <- diagnostic_plt()$empiricalmx$data

    ## Attempt at showing the log scale values
    ## plt <-
    ##   ggplotly(fig, tooltip = c("y", "text"))
    ## tp <- as.numeric(plt$x$layout$yaxis$ticktext)
    ## lb <- scales::label_log()(tp)
    ## expr_strings <- sapply(lb, deparse)
    ## plt$x$layout$yaxis$ticktext <- sapply(expr_strings, plotly::TeX)
    ## plt <- plt %>% plotly::config(mathjax = "cdn")

    ggplotly(fig, tooltip = c("y", "text"))
  })

  diagnostics_text <- reactive({
    req(data_in())
    is_single_ages <- all(diff(sort(data_in()$Age)) == 1)

    diagnostic_paragraph <- paste0(
      "The Roughness method measures the average absolute percentage deviation from a smoothed trend through the five-year age group data. The Sawtooth method takes the average of the ratios of the value in each five-year age group (in adult ages) to the average of the two adjacent age groups (age groups below and above). Both of these methods are trying to pick up on a phenomenon known as differential age heaping where digit preference is stronger on zeroes than on fives. This phenomenon can cause an apparent sawtooth pattern in demographic count data.",
      ifelse(is_single_ages, "", "")
    )

    diagnostic_paragraph
  })

  output$table <- renderDT(
    {
      heaping_exposure <- check_heaping_general(data_in(), "Exposures")
      heaping_deaths <- check_heaping_general(data_in(), "Deaths")

      heaping_exposure$Type <- "Exposures"
      heaping_deaths$Type <- "Deaths"

      heaping_res <- rbind(heaping_exposure, heaping_deaths)
      heaping_res$result <- round(heaping_res$result, 2)

      heaping_res$method <- toTitleCase(heaping_res$method)

      df <- heaping_res
      df <- df[c("Type", "age scale", "method", "result", "level")]

      df <- df[order(df$method), ]

      names(df) <- toTitleCase(names(df))

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
        DT::formatStyle(
          columns = "Result", # Specify the column to color
          backgroundColor = DT::styleEqual(heaping_res$result, heaping_res$color)
        ) %>%
        DT::formatStyle(columns = colnames(df), fontSize = "90%")
    },
    server = FALSE
  )

  observeEvent(input$diagnostics, {
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
        style = "padding-left: 1%; width: 40%; max-height: 400px; overflow-y: auto;",
        dataTableOutput("table"),
        br(),
        div(
          style = "font-style: italic; font-size: 12px; ",
          diagnostics_text()
        )
      )
    )

    # Show the alert with the custom content
    shinyalert(title = "&#x1F50D Data Diagnostics", html = TRUE, size = "l", text = myContent)
  })

  observeEvent(input$forward_step, {
    hide("landing_page")
    show("step_input")
  })

  observeEvent(input$back_to_landing, {
    hide("step_input")
    show("landing_page")
  })

  data_out <- eventReactive(input$calculate_lt, calculateLifeTable(data_in(), input))

  lt_nmx <- reactive({
    gg_plt <- data_out()$lt$plots$nMx$nMx_plot
    list(
      gg = gg_plt,
      plotly = ggplotly(gg_plt),
      dt = data_out()$lt$plots$nMx$nMx_plot_data
    )
  })

  lt_ndx <- reactive({
    gg_plt <- data_out()$lt$plots$ndx$ndx_plot
    list(
      gg = gg_plt,
      plotly = ggplotly(gg_plt),
      dt = data_out()$lt$plots$nDx$nDx_plot_data
    )
  })

  lt_lx <- reactive({
    gg_plt <- data_out()$lt$plots$lx$lx_plot
    list(
      gg = gg_plt,
      plotly = ggplotly(gg_plt),
      dt = data_out()$lt$plots$lx$lx_data
    )
  })

  plots <- list(
    "Mortality Rate Comparison" = lt_nmx,
    "Survival Curve" = lt_lx,
    "Death Distribution" = lt_ndx
  )

  plotRendered <- reactiveVal(FALSE)

  observeEvent(input$calculate_lt, {
    plotRendered(TRUE)
  })

  output$download_buttons <- renderUI({
    if (plotRendered()) {
      div(
        downloadButton("downloadPlot", "Download Plot"),
        downloadButton("downloadData", "Download Data")
      )
    }
  })

  ages_data <- reactive({
    all_ages <- unique(data_in()$Age)
    min_age <- if (60 %in% all_ages) 60 else round(quantile(all_ages, .60))
    step_ages <- diff(all_ages)
    step_repeat <- which.max(table(step_ages))
    step_ages <- as.numeric(names(table(step_ages))[step_repeat])
    list(all_ages = all_ages, min_age_fit = min_age, step_ages = step_ages)
  })

  output$ages_to_use <- renderUI({
    slider_widget <-
      sliderInput(
        "slider_ages_to_use",
        label = NULL,
        min = min(ages_data()$all_ages),
        max = max(ages_data()$all_ages),
        value = c(ages_data()$min_age_fit, max(ages_data()$all_ages)),
        step = ages_data()$step_ages
      )

    div(
      class = "field",
      shiny.semantic::label(
        class = "main label",
        "Ages to fit extrapolation model"
      ),
      slider_widget
    )
  })

  tabs <- reactive({
    list(
      list(
        menu = "Mortality Rate Comparison",
        id = "Mortality Rate Comparison",
        content = list(
          if (!plotRendered()) {
            uiOutput("placeholder_mortality_rate_comparison")
          } else {
            withSpinner(
              plotlyOutput("plot_mortality_rate_comparison", height = "600px")
            )
          }
        )
      ),
      list(
        menu = "Survival Curve",
        id = "Survival Curve",
        content = list(
          if (!plotRendered()) {
            uiOutput("placeholder_survival_curve")
          } else {
            withSpinner(
              plotlyOutput("plot_survival_curve", height = "600px")
            )
          }
        )
      ),
      list(
        menu = "Death Distribution",
        id = "Death Distribution",
        content = list(
          if (!plotRendered()) {
            uiOutput("placeholder_death_distribution")
          } else {
            withSpinner(
              plotlyOutput("plot_death_distribution", height = "600px")
            )
          }
        )
      )
    )
  })

  # Render the tabs in the UI
  output$tabs <- renderUI({
    tabset(
      id = "tabset",
      tabs = tabs()
    )
  })

  # Placeholder for Mortality Rate Comparison
  output$placeholder_mortality_rate_comparison <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Plot for Mortality Rate Comparison
  output$plot_mortality_rate_comparison <- renderPlotly({
    plots[["Mortality Rate Comparison"]]()$plotly
  })

  # Placeholder for Survival Curve
  output$placeholder_survival_curve <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Plot for Survival Curve
  output$plot_death_distribution <- renderPlotly({
    plots[["Death Distribution"]]()$plotly
  })

  # Placeholder for Mortality Rate Comparison
  output$placeholder_death_distribution <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Plot for Mortality Rate Comparison
  output$plot_survival_curve <- renderPlotly({
    print(input$tabset)
    plots[["Survival Curve"]]()$plotly
  })

  setupDownloadHandlers(output, plots, data_out, input)

  observeEvent(input$reset_lt, {
    # Update widgets to their default values
    updateSelectInput(session, "input_oanew", selected = 100)
    updateSelectInput(session, "input_age_out", selected = "single")
    updateSelectInput(session, "input_sex", selected = "Total")
    updateSelectInput(session, "input_extrapLaw", selected = extrap_laws[1])
    updateSelectInput(session, "input_a0rule", selected = "Andreev-Kingkade")
    updateSelectInput(session, "input_axmethod", selected = "UN (Greville)")

    updateSliderInput(
      session,
      "slider_ages_to_use",
      value = c(ages_data()$min_age_fit, max(ages_data()$all_ages))
    )

    # Update the numeric inputs
    update_numeric_input(session, "input_extrapFrom", value = 80)
    update_numeric_input(session, "input_radix", value = 100000)
    update_numeric_input(session, "input_srb", value = 1.05)
  })
}
