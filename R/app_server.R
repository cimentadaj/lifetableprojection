to_snake <- function(x) tolower(gsub(" ", "", x))

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
      paste(to_snake(input$tabSelector), "plot.png", sep = "_")
    },
    content = function(file) {
      plot <- plots[[input$tabSelector]]()$gg
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
#' @importFrom shiny renderUI observeEvent eventReactive actionButton reactive reactiveVal sliderInput observe conditionalPanel
#' @importFrom shiny.semantic tabset icon updateSelectInput update_numeric_input updateSliderInput
#' @importFrom shinyjs show hide
#' @importFrom plotly plotlyOutput renderPlotly config
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom ggplot2 element_blank theme geom_line
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyalert shinyalert
#' @importFrom stats reshape quantile
#' @importFrom DT datatable renderDT dataTableOutput
#' @importFrom untheme detect_font_size
#' @importFrom ODAPbackend plot_initial_data check_heaping_general lt_summary
#' @importFrom utils zip
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
  observeEvent(input$file1, {
    data_in(readData(input))

    # Wait for check_results to be available
    req(check_results())

    if (all(check_results()$pass == "Pass")) {
      print("File validation passed")

      # Create the multi-select input for identifier columns
      column_selector <- selectInput(
        "id_columns",
        label = "Select Identifier Columns",
        choices = names(data_in()),
        multiple = TRUE,
        selectize = TRUE
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
            ODAPbackend:::create_groupid(data_in(), selected_columns) %>%
            ODAPbackend:::check_groupid()

          valid_groups
        } else {
          FALSE
        }
      }


      if (!validate_groups()) {
        column_selection_modal()
      }

      observeEvent(input$shinyalert, {
        if (!validate_groups()) {
          column_selection_modal(
            extra_content = '<p style="color: red; font-weight: bold; font-size: 15px; text-align: center;"> The specified columns do not identify each row uniquely </p>'
          )
        }
      })

      if (validate_groups()) {
        data_in(ODAPbackend:::create_groupid(data_in(), selected_columns))
      }
    }
  })

  # Update data_in when the button is clicked
  observeEvent(input$continue_no_data, {
    dt_read <- read.csv(dt_ex)
    dt_read$`.id` <- 1
    data_in(dt_read)
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
        action_button("diagnostics", "Diagnostics", class = "ui blue button"),
        action_button("forward_step", "Continue", class = "ui blue button")
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
    ggplt <- ggplotly(fig, tooltip = c("y", "text"))

    config(
      ggplt,
      displayModeBar = FALSE
    )
  })

  output$diag_deaths <- renderPlotly({
    fig <- diagnostic_plt()$deaths$figure
    dt <- diagnostic_plt()$deaths$data
    ggplt <- ggplotly(fig, tooltip = c("y", "text"))

    config(
      ggplt,
      displayModeBar = FALSE
    )
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

    ggplt <- ggplotly(fig, tooltip = c("y", "text"))
    config(
      ggplt,
      displayModeBar = FALSE
    )
  })

  diagnostics_text <- reactive({
    req(data_in())
    is_single_ages <- all(diff(sort(data_in()$Age)) == 1)

    diagnostic_paragraph <- paste0(
      "The Roughness method measures the average absolute percentage deviation from a smoothed trend through the five-year age group data. The Sawtooth method takes the average of the ratios of the value in each five-year age group (in adult ages) to the average of the two adjacent age groups (age groups below and above). Both of these methods are trying to pick up on a phenomenon known as differential age heaping where digit preference is stronger on zeroes than on fives. This phenomenon can cause an apparent sawtooth pattern in demographic count data.",
      ifelse(is_single_ages, "The Myers and Bachi indices both measure digit preference for single-age data. If there were no digit preference, then the distribution over terminal digits (0-9) would be roughly uniform. Both of these indices measure the departure from uniformity, with slight variations on the implementation. Higher values indicate digit distributions that are farther from uniform. If these indices are high, but the _roughness_ and _sawtooth_ indices are low, then you might adjust data using one of the _fine_ smoothing methods offered.", "")
    )

    diagnostic_paragraph
  })

  diagnostics_table <- reactive({
    heaping_exposure <- check_heaping_general(data_in(), "Exposures")
    heaping_deaths <- check_heaping_general(data_in(), "Deaths")

    heaping_exposure$Type <- "Exposures"
    heaping_deaths$Type <- "Deaths"

    heaping_res <- rbind(heaping_exposure, heaping_deaths)
    heaping_res$result <- round(heaping_res$result, 2)

    heaping_res$method <- toTitleCase(heaping_res$method)
    df <- heaping_res
    df <- df[c("Type", "age scale", "method", "result", "level", "color")]
    df <- df[order(df$method), ]
    names(df) <- toTitleCase(names(df))

    df
  })

  output$table <- renderDT(
    {
      datatable(
        diagnostics_table()[, names(diagnostics_table()) != "Color"],
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
          columns = "Result",
          backgroundColor = DT::styleEqual(
            diagnostics_table()$Result,
            diagnostics_table()$Color
          )
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
        class = "table-container",
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

  # observeEvent(input$forward_step, {
  #   hide("landing_page")
  #   show("step_input")
  # })

  observeEvent(input$forward_step, {
    hide("landing_page")
    show("step_adjustment")
  })

  observeEvent(input$back_to_diagnostics, {
    hide("step_adjustment")
    show("landing_page")
  })

  observeEvent(input$forward_to_lifetable, {
    hide("step_adjustment")
    show("step_input")
  })

  observeEvent(input$back_to_adjustment, {
    hide("step_input")
    show("step_adjustment")
  })

  output$correct_abridged_inputs <- renderUI({
    div(
      numericInput("abridged_start", "Start Age", value = 0),
      numericInput("abridged_end", "End Age", value = 100)
    )
  })

  output$add_smoothness_inputs <- renderUI({
    div(
      sliderInput("smoothness_factor", "Smoothness Factor", min = 0, max = 1, value = 0.5)
    )
  })

  observeEvent(input$execute_correct_abridged, {
    # Perform the correction for abridged ages
    # Update the data_in() reactive value with the adjusted data
    # Generate and render the plot
    output$correct_abridged_plot <- renderPlot({
      # Generate plot based on the adjusted data
    })
  })

  observeEvent(input$execute_add_smoothness, {
    # Apply smoothness to the data
    # Update the data_in() reactive value with the smoothed data
    # Generate and render the plot
    output$add_smoothness_plot <- renderPlot({
      # Generate plot based on the smoothed data
    })
  })

  data_out <- eventReactive(input$calculate_lt, calculateLifeTable(data_in(), input))

  lt_nmx <- reactive({
    gg_plt <- data_out()$lt$plots$nMx$nMx_plot
    list(
      gg = gg_plt,
      plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
      dt = data_out()$lt$plots$nMx$nMx_plot_data
    )
  })

  lt_ndx <- reactive({
    gg_plt <- data_out()$lt$plots$ndx$ndx_plot
    list(
      gg = gg_plt,
      plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
      dt = data_out()$lt$plots$ndx$ndx_plot_data
    )
  })

  lt_lx <- reactive({
    gg_plt <- data_out()$lt$plots$lx$lx_plot
    list(
      gg = gg_plt,
      plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
      dt = data_out()$lt$plots$lx$lx_plot_data
    )
  })

  lt_nqx <- reactive({
    gg_plt <- data_out()$lt$plots$nqx$nqx_plot
    list(
      gg = gg_plt,
      plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
      dt = data_out()$lt$plots$lx$nqx_plot_data
    )
  })


  lt_plots <- list(
    "Mortality Rate Comparison" = lt_nmx,
    "Survival Curve" = lt_lx,
    "Death Distribution" = lt_ndx,
    "Conditional Death Probabilities" = lt_nqx,
    "Lifetable Results" = data_out
  )

  # Define tab names and IDs
  tabNames <- names(lt_plots)

  extrap_age <- reactive({
    req(data_in)
    num <- as.numeric(gsub("+", "", max(data_in()$Age)))
    num - 20
  })

  output$extrap_from_data <- renderUI({
    create_field_set(
      "",
      "Extrap. Jump-off Age",
      "input_extrapFrom",
      input_selected = extrap_age(),
      numeric_input = TRUE
    )
  })

  output$select_plots <- renderUI({
    selectInput(inputId = "tabSelector", label = NULL, choices = tabNames)
  })

  plotRendered <- reactiveVal(FALSE)

  observeEvent(input$calculate_lt, {
    plotRendered(TRUE)
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

  renderTabContent <- function(id, plotName, OutputFunction) {
    output[[id]] <- renderUI({
      if (!plotRendered()) {
        uiOutput(sprintf("placeholder_%s", plotName))
      } else {
        withSpinner(OutputFunction(sprintf("plot_%s", plotName), height = "600px"))
      }
    })
  }

  lapply(seq_along(tabNames), function(i) {
    renderTabContent(
      sprintf("tabContent%s", i),
      gsub(" ", "_", tolower(tabNames[i])),
      ifelse(grepl("Lifetable Results", tabNames[i]), DTOutput, plotlyOutput)
    )
  })

  output$render_plots <- renderUI({
    lapply(seq_along(tabNames), function(i) {
      conditionalPanel(
        condition = sprintf("input.tabSelector === '%s'", tabNames[i]),
        uiOutput(sprintf("tabContent%s", i))
      )
    })
  })

  observeEvent(input$calculate_lt, {
    output$lt_summary_indication <- renderUI({
      div(
        div(
          class = "below-main-panel fade-in-icon", # Add the class here
          shiny.semantic::icon("arrow down circle", style = "font-size: 3rem;")
        ), div(
          class = "below-main-panel",
          h1("Life Table Summary Statistics"),
        )
      )
    })
  })

  lifetable_summary_table <- reactive({
    lt_res <- lt_summary(data_out()$lt$lt)
    lt_res$message <- tools::toTitleCase(lt_res$message)
    names(lt_res) <- tools::toTitleCase(names(lt_res))
    lt_res
  })

  # Render the table
  output$lt_summary_table <- renderDT({
    datatable(
      lifetable_summary_table()[c("Label", "Message", "Value")],
      options = list(
        dom = "t",
        paging = FALSE,
        info = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(targets = c("Label"), render = JS(RENDERKATEX))
        )
      )
    ) %>%
      formatRound("Value", 8)
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
    lt_plots[["Mortality Rate Comparison"]]()$plotly
  })

  # Plot for Mortality Rate Comparison
  output$plot_survival_curve <- renderPlotly({
    lt_plots[["Survival Curve"]]()$plotly
  })

  # Placeholder for Survival Curve
  output$placeholder_survival_curve <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Plot for Mortality Rate Comparison
  output$plot_conditional_death_probabilities <- renderPlotly({
    lt_plots[["Conditional Death Probabilities"]]()$plotly
  })

  # Placeholder for Survival Curve
  output$placeholder_conditional_death_probabilities <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Plot for Survival Curve
  output$plot_death_distribution <- renderPlotly({
    lt_plots[["Death Distribution"]]()$plotly
  })

  # Placeholder for Mortality Rate Comparison
  output$placeholder_death_distribution <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Placeholder for Life table results
  output$placeholder_lifetable_results <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  output$plot_lifetable_results <- renderDT({
    dt <- lt_plots[["Lifetable Results"]]()$lt$lt
    dt$AgeInt <- NULL
    mask <- vapply(dt, is.numeric, FUN.VALUE = logical(1))
    dt[mask] <- round(dt[mask], 2)

    dt <- datatable(
      dt,
      options = list(
        paging = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "lfrtp"
      ),
      rownames = FALSE
    )

    dt
  })

  setupDownloadHandlers(output, lt_plots, data_out, input)

  output$download_button <- renderUI({
    req(input$get_screen_width)
    if (plotRendered()) {
      sizes <- detect_font_size(input$get_screen_width)

      if (sizes$type == "mobile") {
        div(
          style = "width: 100%; display: grid;",
          downloadButton("download_all", "Download All", class = "ui blue button")
        )
      } else {
        div(
          style = "margin-left: auto;",
          downloadButton("download_all", "Download All", class = "ui blue button")
        )
      }
    }
  })

  output$download_all <- downloadHandler(
    filename = function() {
      paste("lifetable_analysis_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Main directory to store the plot folders
      main_plot_path <- file.path(tempdir(), "lifetable_analysis")
      unlink(main_plot_path, recursive = TRUE, force = TRUE)
      dir.create(main_plot_path, recursive = TRUE)

      # Assuming 'lt_plots' is your list of ggplot objects
      lt_analysis <- lt_plots
      names(lt_analysis) <- to_snake(names(lt_analysis))
      lt_analysis_plots <- lapply(lt_analysis, function(x) x()$gg)
      lt_analysis_dt <- lapply(lt_analysis, function(x) x()$dt)

      # Save each plot and its corresponding DataTable in its own folder
      lapply(names(lt_analysis_plots), function(plot_name) {
        plot_folder_path <- file.path(main_plot_path, "analysis", plot_name)
        dir.create(plot_folder_path, recursive = TRUE)

        # Save the plot
        ggsave(
          filename = file.path(plot_folder_path, paste0(plot_name, ".png")),
          plot = lt_analysis_plots[[plot_name]],
          device = "png"
        )

        # Save the DataTable as a CSV
        write.csv(
          lt_analysis_dt[[plot_name]],
          file = file.path(plot_folder_path, paste0(plot_name, ".csv")),
          row.names = FALSE
        )
      })

      # Assuming 'lt_plots' is your list of ggplot objects
      diagnostic_analysis <- diagnostic_plt()
      names(diagnostic_analysis) <- to_snake(names(diagnostic_analysis))
      diagnostic_analysis_plots <- lapply(diagnostic_analysis, function(x) x$figure)
      diagnostic_analysis_dt <- lapply(diagnostic_analysis, function(x) x$data)

      lapply(names(diagnostic_analysis_plots), function(plot_name) {
        plot_folder_path <- file.path(main_plot_path, "diagnostics", plot_name)
        dir.create(plot_folder_path, recursive = TRUE)

        # Save the plot
        ggsave(
          filename = file.path(plot_folder_path, paste0(plot_name, ".png")),
          plot = diagnostic_analysis_plots[[plot_name]],
          device = "png"
        )

        # Save the DataTable as a CSV
        write.csv(
          diagnostic_analysis_dt[[plot_name]],
          file = file.path(plot_folder_path, paste0(plot_name, ".csv")),
          row.names = FALSE
        )
      })

      write.csv(
        lifetable_summary_table()[c("Measure", "Message", "Value")],
        file = file.path(main_plot_path, "analysis", "lifetable_summary.csv"),
        row.names = FALSE
      )

      write.csv(
        data_out()$lt$lt,
        file = file.path(main_plot_path, "lifetable_results.csv"),
        row.names = FALSE
      )

      write.csv(
        diagnostics_table(),
        file = file.path(main_plot_path, "diagnostics", "diagnostics_summary.csv"),
        row.names = FALSE
      )

      writeLines(
        text = diagnostics_text(),
        con = file.path(main_plot_path, "diagnostics", "diagnostics_text.txt"),
      )

      # Zip only the "lifetable_analysis" folder
      setwd(main_plot_path)
      zip(file, files = list.files(".", full.names = TRUE, recursive = TRUE))
      setwd(tempdir()) # Reset working directory to tempdir()
    }
  )


  observeEvent(input$reset_lt, {
    # Update widgets to their default values
    updateSelectInput(session, "input_oanew", selected = 100)
    updateSelectInput(session, "input_age_out", selected = "single")
    updateSelectInput(session, "input_sex", selected = "Total")
    updateSelectInput(session, "input_extrapLaw", selected = EXTRAP_LAWS[1])
    updateSelectInput(session, "input_a0rule", selected = "Andreev-Kingkade")
    updateSelectInput(session, "input_axmethod", selected = "UN (Greville)")

    updateSliderInput(
      session,
      "slider_ages_to_use",
      value = c(ages_data()$min_age_fit, max(ages_data()$all_ages))
    )

    # Update the numeric inputs
    update_numeric_input(session, "input_extrapFrom", value = extrap_age())
    update_numeric_input(session, "input_radix", value = 100000)
    update_numeric_input(session, "input_srb", value = 1.05)
  })
}
