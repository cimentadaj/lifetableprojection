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
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Define adjustment steps list (this remains inside app_server)
adjustment_steps <- list(
  smoothing = list(
    name = "Smoothing",
    input_ui = function() {
      div(
        selectInput("smoothing_variable", "Variable", choices = c("Exposures", "Deaths")),
        selectInput(
          "smoothing_rough_method",
          "Rough Method",
          choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
          selected = "auto"
        ),
        selectInput(
          "smoothing_fine_method",
          "Fine Method",
          choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
          selected = "auto"
        ),
        selectInput(
          "smoothing_age_out",
          "Age Output",
          choices = c("single", "abridged", "5-year"),
          selected = "abridged"
        ),
        numericInput("smoothing_u5m", "Under-5 Mortality (optional)", value = NULL),
        shiny.semantic::checkbox_input("smoothing_constrain_infants", "Constraint Infants", is_marked = TRUE)
      )
    },
    execute = function(data, input) {
      smooth_flexible(
        data,
        variable = input$smoothing_variable,
        rough_method = input$smoothing_rough_method,
        fine_method = input$smoothing_fine_method,
        constrain_infants = input$smoothing_constrain_infants,
        age_out = input$smoothing_age_out,
        u5m = input$smoothing_u5m
      )
    }
  ),
  smoothing_second = list(
    name = "Smoothing Second",
    input_ui = function() {
      div(
        selectInput("smoothing2_variable", "Variable", choices = c("Exposures", "Deaths")),
        selectInput(
          "smoothing2_rough_method",
          "Rough Method",
          choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
          selected = "auto"
        ),
        selectInput(
          "smoothing2_fine_method",
          "Fine Method",
          choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
          selected = "auto"
        ),
        selectInput(
          "smoothing2_age_out",
          "Age Output",
          choices = c("single", "abridged", "5-year"),
          selected = "abridged"
        ),
        numericInput("smoothing2_u5m", "Under-5 Mortality (optional)", value = NULL),
        shiny.semantic::checkbox_input("smoothing2_constrain_infants", "Constraint Infants", is_marked = TRUE)
      )
    },
    execute = function(data, input) {
      smooth_flexible(
        data,
        variable = input$smoothing2_variable,
        rough_method = input$smoothing2_rough_method,
        fine_method = input$smoothing2_fine_method,
        constrain_infants = input$smoothing2_constrain_infants,
        age_out = input$smoothing2_age_out,
        u5m = input$smoothing2_u5m
      )
    }
  )
  # Add more adjustment steps here as needed
)


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
#' @importFrom ODAPbackend lt_summary smooth_flexible
#' @importFrom utils zip
#' @export
app_server <- function(input, output, session) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  # Initialize reactive values
  data_in <- reactiveVal()

  # Handle file upload
  uploaded_data <- handle_file_upload(input)

  # Handle sample data
  sample_data <- handle_sample_data()

  # Update data_in when the sample data button is clicked
  observeEvent(input$continue_no_data, {
    data_in(sample_data())
  })

  observe({
    data_in(uploaded_data())
  })



  # Display table as example..
  output$data_table <- renderRHandsontable(renderDataTable(sample_data()))

  # General variable to indicate
  group_selection_passed <- reactiveVal(FALSE)
  selected_grouping_vars <- reactiveVal(NULL)

  # Handle column selection
  handle_group_selection_modal(input, output, session, data_in, group_selection_passed, selected_grouping_vars)

  # DF containing the id and labels
  labels_df <- reactive({
    data_in() %>%
      distinct(.id, .id_label)
  })

  # Setup group selection dropdowns
  grouping_dropdowns <- setup_grouping_dropdowns(selected_grouping_vars, data_in)

  # Validate data after group selection
  validate_data_after_group_selection(input, output, data_in, group_selection_passed)

  # Setup observers for grouping dropdown changes
  setup_grouping_dropdown_observers(input, selected_grouping_vars)

  # Generate and manage diagnostic data
  diagnostic_data <- setup_diagnostic_data(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns)

  # Handle transitions
  handle_transitions(input)

  # Create a reactive value to store the list of executed adjustments
  executed_adjustments <- reactiveVal(list())

  # Function to set up adjustment steps
  setup_adjustment_steps <- function(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns) {
    lapply(names(adjustment_steps), function(step_name) {
      step <- adjustment_steps[[step_name]]

      # Render group selection dropdown UI
      output[[paste0("adjustment_group_select_ui_", step_name)]] <- renderUI({
        div(
          class = "grouping-dropdowns-container",
          style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
          lapply(grouping_dropdowns(), function(dropdown) {
            div(style = "min-width: 150px", dropdown)
          })
        )
      })

      # Render input UI
      output[[paste0(step_name, "_inputs")]] <- renderUI(step$input_ui())

      # Set up execution observer
      observeEvent(input[[paste0("execute_", step_name)]], {
        add_adjustment_pill(step$name, executed_adjustments)

        result <- step$execute(data_in(), input)

        # Process and store results
        processed_result <- process_adjustment_result(result, labels_df(), selected_grouping_vars)
        store_adjustment_result(step_name, processed_result)
      })

      # Render plot
      output[[paste0(step_name, "_plot")]] <- renderPlotly({
        req(input[[paste0("execute_", step_name)]] > 0)
        plot_data <- get_adjustment_result(step_name)
        current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
        ggplotly(plot_data$plots[[current_id]]$plot)
      })
    })
  }

  # Helper functions
  process_adjustment_result <- function(result, labels_df, selected_grouping_vars) {
    group_plots <- lapply(labels_df()$`.id`, function(group_id) {
      group_label <- labels_df()$.id_label[labels_df()$.id == as.numeric(group_id)]
      plot <- result$figures[[group_id]]$figure
      list(id = group_id, label = group_label, plot = plot)
    })
    names(group_plots) <- labels_df()$`.id`
    list(plots = group_plots, labels = labels_df())
  }

  store_adjustment_result <- function(step_name, result) {
    current_results <- adjustment_results()
    current_results[[step_name]] <- result
    adjustment_results(current_results)
  }

  get_adjustment_result <- function(step_name) {
    adjustment_results()[[step_name]]
  }

  adjustment_results <- reactiveVal(list())

  # Set up adjustment steps
  setup_adjustment_steps(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns)

  # Render the pills UI
  output$adjustment_pills <- renderUI({
    pills <- executed_adjustments()
    render_adjustment_pills(pills)
  })

  # Observer to handle pill removal
  observeEvent(input$remove_pill, {
    remove_adjustment_pill(input$remove_pill, executed_adjustments)
  })

  ## END INTERMEDIATTE STEPS

  ## CALCULATE LIFETABLE

  # Create life table input UI and reactive values
  lt_input <- create_life_table_input_ui(data_in, output)

  # Create a reactive value to store the life table data and plots
  lt_data <- reactiveVal(NULL)

  # Function to calculate life table and generate plots
  calculate_lt_and_plots <- function(data, input) {
    reactive({
      print("Starting life table calculations")
      lt_res <- calculateLifeTable(data, input)
      plots <- lt_plot(data, lt_res, input$input_extrapFrom)
      lt_res_summary <- lt_summary(lt_res)
      print("Life table calculations complete")

      list(
        plots = plots,
        summary = lt_res_summary,
        lt = lt_res
      )
    })
  }

  plotRendered <- reactiveVal(FALSE)

  # Event to trigger life table calculation and plot generation
  observeEvent(input$calculate_lt, {
    print("Calculate LT button clicked")
    req(data_in())
    plotRendered(TRUE)
    lt_data(calculate_lt_and_plots(data_in(), input))
  })


  # Render the group selection dropdown UI
  output$lt_group_select_ui <- renderUI({
    req(lt_data())
    group_labels <- labels_df()

    selectInput(
      "lt_group_select",
      "Select Group",
      choices = setNames(group_labels$.id, group_labels$.id_label),
      selected = group_labels$.id[1]
    )
  })


  output$lt_group_select_ui <- renderUI({
    div(
      class = "grouping-dropdowns-container",
      style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
      lapply(grouping_dropdowns(), function(dropdown) {
        div(style = "min-width: 150px", dropdown)
      }),
      div(
        class = "grouping-dropdowns-container",
        style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center;",
        selectInput(inputId = "tabSelector", label = NULL, choices = tabNames)
      )
    )
  })


  # Observer for group selection change
  observeEvent(input$lt_group_select, {
    req(lt_data())
    id_col <- labels_df()$`.id`[labels_df()$`.id` == as.numeric(input$lt_group_select)]
    plot_slot <- which(names(lt_data()()$plots) == as.character(id_col))
    print("Test")
    print(id_col)
    print(labels_df())
    print(input$lt_group_select)
    selected_plots <- lt_data()()$plots[[plot_slot]]

    print("selected pltos")
    print(names(selected_plots))

    # Update all plot outputs
    output$plot_mortality_rate_comparison <- renderPlotly({
      ggplotly(selected_plots$nMx$nMx_plot)
    })

    output$plot_survival_curve <- renderPlotly({
      ggplotly(selected_plots$lx$lx_plot)
    })

    output$plot_conditional_death_probabilities <- renderPlotly({
      ggplotly(selected_plots$nqx$nqx_plot)
    })

    output$plot_death_distribution <- renderPlotly({
      ggplotly(selected_plots$ndx$ndx_plot)
    })

    lt_nmx <- reactive({
      gg_plt <- selected_plots$nMx$nMx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots$nMx$nMx_plot_data
      )
    })

    lt_ndx <- reactive({
      gg_plt <- selected_plots$ndx$ndx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots$ndx$ndx_plot_data
      )
    })

    lt_lx <- reactive({
      gg_plt <- selected_plots$lx$lx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots$lx$lx_plot_data
      )
    })

    lt_nqx <- reactive({
      gg_plt <- selected_plots$nqx$nqx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots$nqx$nqx_plot_data
      )
    })


    lt_plots <- list(
      "Mortality Rate Comparison" = lt_nmx,
      "Survival Curve" = lt_lx,
      "Death Distribution" = lt_ndx,
      "Conditional Death Probabilities" = lt_nqx,
      "Lifetable Results" = lt_data()()$lt
    )

    # Plot for Mortality Rate Comparison
    output$plot_mortality_rate_comparison <- renderPlotly({
      lt_plots[["Mortality Rate Comparison"]]()$plotly
    })

    # Plot for Mortality Rate Comparison
    output$plot_survival_curve <- renderPlotly({
      lt_plots[["Survival Curve"]]()$plotly
    })


    # Plot for Mortality Rate Comparison
    output$plot_conditional_death_probabilities <- renderPlotly({
      lt_plots[["Conditional Death Probabilities"]]()$plotly
    })

    # Plot for Survival Curve
    output$plot_death_distribution <- renderPlotly({
      lt_plots[["Death Distribution"]]()$plotly
    })
  })

  tabNames <- c(
    "Mortality Rate Comparison",
    "Survival Curve",
    "Death Distribution",
    "Conditional Death Probabilities",
    "Lifetable Results"
  )


  # Render the life table summary table
  output$lt_summary_table <- renderDT({
    req(lt_data())
    print("names lt")
    print(names(lt_data()()$summary))
    datatable(
      lt_data()()$summary,
      options = list(
        dom = "t",
        paging = FALSE,
        info = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(targets = c("label"), render = JS(RENDERKATEX))
        )
      )
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

  # Placeholder for Mortality Rate Comparison
  output$placeholder_mortality_rate_comparison <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Placeholder for Survival Curve
  output$placeholder_survival_curve <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
  })

  # Placeholder for Survival Curve
  output$placeholder_conditional_death_probabilities <- renderUI({
    tags$img(
      src = "www/placeholder_plot.png",
      height = "600px",
      width = "100%"
    )
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

  # Setup download handlers
  setupDownloadHandlers(
    output,
    function() lt_data()()$plots,
    function() lt_data()()$lt,
    input
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
      value = c(lt_input$ages_data()$min_age_fit, max(lt_input$ages_data()$all_ages))
    )

    # Update the numeric inputs
    update_numeric_input(session, "input_extrapFrom", value = lt_input$extrap_age())
    update_numeric_input(session, "input_radix", value = 100000)
    update_numeric_input(session, "input_srb", value = 1.05)
  })


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
}

