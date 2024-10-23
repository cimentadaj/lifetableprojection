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
    execute = function(input) {
      rlang::expr(
        smooth_flexible(
          .data,
          variable = !!input$smoothing_variable,
          rough_method = !!input$smoothing_rough_method,
          fine_method = !!input$smoothing_fine_method,
          constrain_infants = !!input$smoothing_constrain_infants,
          age_out = !!input$smoothing_age_out,
          u5m = !!input$smoothing_u5m
        )
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
    execute = function(input) {
      rlang::expr(
        smooth_flexible(
          .data,
          variable = !!input$smoothing2_variable,
          rough_method = !!input$smoothing2_rough_method,
          fine_method = !!input$smoothing2_fine_method,
          constrain_infants = !!input$smoothing2_constrain_infants,
          age_out = !!input$smoothing2_age_out,
          u5m = !!input$smoothing2_u5m
        )
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
    req(data_in())
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

  # Create a reactive value to store the list of executed adjustments (step names)
  executed_adjustments <- reactiveVal(character(0))

  # Create preprocessing_execution object
  preprocess_exec <- reactiveVal(NULL)

  # Update preprocess_exec whenever data_in changes
  observeEvent(data_in(), {
    req(data_in())
    preprocess_exec(preprocessing_execution(data_in()))
  })

  # List to store reactive expressions for function calls
  step_func_calls <- reactiveValues()

  force_update <- reactiveVal(FALSE)

  # Function to set up adjustment steps
  setup_adjustment_steps <- function(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns) {
    lapply(names(adjustment_steps), function(step_name) {
      step <- adjustment_steps[[step_name]]
      step_clean_name <- step$name

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

      # Create reactive expression for the function call
      step_func_calls[[step_name]] <- reactive({
        # Build the function call using the current inputs
        step$execute(input)
      })

      # Set up execution observer
      observeEvent(input[[paste0("execute_", step_name)]], {
        current_steps <- executed_adjustments()
        if (!(step_name %in% current_steps)) {
          current_steps <- c(current_steps, setNames(step_name, step_clean_name))
          executed_adjustments(current_steps)
        } else {
          executed_adjustments(current_steps)
        }

        force_update(!force_update())
      })

      # Render plot
      output[[paste0(step_name, "_plot")]] <- renderPlotly({
        plot_data <- get_adjustment_plot(step_name)
        req(plot_data)
        plot_data
      })
    })
  }

  # Helper functions
  get_adjustment_result <- function(step_name) {
    results <- preprocessing_results()
    if (!is.null(results) && step_name %in% names(results)) {
      return(results[[step_name]])
    } else {
      return(NULL)
    }
  }

  get_adjustment_plot <- function(step_name) {
    plot_data <- get_adjustment_result(step_name)
    req(plot_data)
    current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    plot_list <- plot_data$plot_result

    # Ensure plot_list is named by group IDs
    plot_ids <- names(plot_list)
    if (is.null(plot_ids)) {
      stop(paste("Plot result for step", step_name, "does not have named group IDs"))
    }

    # Find matching group ID
    group_id_str <- as.character(current_id)
    if (!group_id_str %in% plot_ids) {
      stop(paste("No plot for group ID", group_id_str, "in step", step_name))
    }
    plot_item <- plot_list[[group_id_str]]
    # Depending on the structure, extract the plot
    if ("figure" %in% names(plot_item)) {
      plot <- plot_item$figure
    } else if (inherits(plot_item, "ggplot")) {
      plot <- plot_item
    } else {
      stop(paste("Cannot find plot in plot_item for group ID", group_id_str))
    }
    req(plot)
    ggplotly(plot)
  }

  # Set up adjustment steps
  setup_adjustment_steps(input, output, session, data_in, selected_grouping_vars, grouping_dropdowns)

  # Render the pills UI
  output$adjustment_pills <- renderUI({
    pills <- executed_adjustments()
    render_adjustment_pills(pills)
  })

  # Reactive expression to execute preprocessing steps
  preprocessing_results <- reactive({
    ## req(preprocess_exec())
    force_update() # Include the force_update trigger here
    steps <- executed_adjustments()

    if (length(steps) == 0) {
      return(NULL)
    }

    # Collect function calls
    func_calls <- lapply(steps, function(step_name) step_func_calls[[step_name]]())

    # Check if any function calls are NULL
    if (any(sapply(func_calls, is.null))) {
      return(NULL)
    }

    # Add steps to preprocess_exec
    for (i in seq_along(steps)) {
      step_name <- steps[i]
      func_call <- func_calls[[i]]
      preprocess_exec()$add(step_name, func_call)
    }

    print(lapply(preprocess_exec()$get_result(), function(x) x[c("data_input", "data_output")]))

    # Execute steps
    preprocess_exec()$execute(steps)

    # Return the results
    preprocess_exec()$results
  })

  # Observer to handle pill removal
  observeEvent(input$remove_pill, {
    current_steps <- executed_adjustments()
    current_steps <- current_steps[names(current_steps) != input$remove_pill]
    executed_adjustments(current_steps)
  })


  ## END INTERMEDIATE STEPS

  ## CALCULATE LIFETABLE

  # Create life table input UI and reactive values
  lt_input <- create_life_table_input_ui(data_in, grouping_dropdowns, tabNames, input, output)

  # Create a reactive value to store the life table data and plots
  lt_data <- reactiveVal(NULL)

  # Create a reactive value to track if plots are ready to be rendered
  plots_ready <- reactiveVal(FALSE)

  # Event to trigger life table calculation and plot generation
  observeEvent(input$calculate_lt, {
    print("Calculate LT button clicked")
    # Use the final result from preprocessing or data_in if no steps
    final_data <- if (!is.null(preprocessing_results())) {
      preprocess_exec()$final_result()
    } else {
      data_in()
    }
    req(final_data)
    plots_ready(FALSE)
    lt_data(calculate_lt_and_plots(final_data, input))
    plots_ready(TRUE)
  })

  observeEvent(input$calculate_lt, {
    output$lt_summary_indication <- renderUI({
      div(
        div(
          class = "below-main-panel fade-in-icon",
          shiny.semantic::icon("arrow down circle", style = "font-size: 3rem;")
        ), div(
          class = "below-main-panel",
          h1("Life Table Summary Statistics"),
        )
      )
    })
  })

  current_id <- reactive({
    current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    current_id
  })

  # Create a reactive expression for the selected plots
  selected_plots <- reactive({
    req(lt_data())
    plot_slot <- which(names(lt_data()()$plots) == as.character(current_id()))
    lt_data()()$plots[[plot_slot]]
  })

  # Create reactive expressions for each plot type
  lt_plots <- reactive({
    req(selected_plots())
    list(
      "Mortality Rate" = reactive({
        gg_plt <- selected_plots()$nMx$nMx_plot
        list(
          gg = gg_plt,
          plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
          dt = selected_plots()$nMx$nMx_plot_data
        )
      }),
      "Survival Curve" = reactive({
        gg_plt <- selected_plots()$lx$lx_plot
        list(
          gg = gg_plt,
          plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
          dt = selected_plots()$lx$lx_plot_data
        )
      }),
      "Death Distribution" = reactive({
        gg_plt <- selected_plots()$ndx$ndx_plot
        list(
          gg = gg_plt,
          plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
          dt = selected_plots()$ndx$ndx_plot_data
        )
      }),
      "Conditional Death Probabilities" = reactive({
        gg_plt <- selected_plots()$nqx$nqx_plot
        list(
          gg = gg_plt,
          plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
          dt = selected_plots()$nqx$nqx_plot_data
        )
      }),
      "Lifetable Results" = reactive({
        lt_data()()$lt
      })
    )
  })

  # Render plots
  observe({
    req(plots_ready())

    output$plot_mortality_rate <- renderPlotly({
      lt_plots()[["Mortality Rate"]]()$plotly
    })

    output$plot_survival_curve <- renderPlotly({
      lt_plots()[["Survival Curve"]]()$plotly
    })

    output$plot_conditional_death_probabilities <- renderPlotly({
      lt_plots()[["Conditional Death Probabilities"]]()$plotly
    })

    output$plot_death_distribution <- renderPlotly({
      lt_plots()[["Death Distribution"]]()$plotly
    })

    output$plot_lifetable_results <- renderDT({
      dt <- lt_plots()[["Lifetable Results"]]()
      dt$AgeInt <- NULL
      mask <- vapply(dt, is.numeric, FUN.VALUE = logical(1))
      dt[mask] <- round(dt[mask], 2)

      plot_slot <- which(names(lt_data()()$plots) == as.character(current_id()))
      dt <-
        dt %>%
        filter(.id == plot_slot) %>%
        select(-`.id`)

      to_subset <- setdiff(names(dt), selected_grouping_vars())

      dt <- datatable(
        dt[to_subset],
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
  })

  tabNames <- c(
    "Mortality Rate",
    "Survival Curve",
    "Death Distribution",
    "Conditional Death Probabilities",
    "Lifetable Results"
  )

  # Render the life table summary table
  output$lt_summary_table <- renderDT({
    req(lt_data())

    plot_slot <- which(names(lt_data()()$plots) == as.character(current_id()))
    tbl <- lt_data()()$summary %>% dplyr::filter(.id == plot_slot)
    tbl$value <- round(tbl$value, 2)

    datatable(
      tbl,
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

  output$render_plots <- renderUI({
    lapply(seq_along(tabNames), function(i) {
      id <- sprintf("tabContent%s", i)
      plotName <- gsub(" ", "_", tolower(tabNames[i]))
      OutputFunction <- ifelse(grepl("Lifetable Results", tabNames[i]), DTOutput, plotlyOutput)

      # Define UI rendering for each tab
      output[[id]] <- renderUI({
        withSpinner(OutputFunction(sprintf("plot_%s", plotName), height = "600px"))
      })

      # Render conditional panel with the appropriate content
      conditionalPanel(
        condition = sprintf("input.tabSelector === '%s'", tabNames[i]),
        uiOutput(id)
      )
    })
  })

  # Setup download handlers
  setupDownloadHandlers(
    output,
    lt_plots,
    function() lt_data()$lt,
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
    if (plots_ready()) {
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

      # Assuming 'diagnostic_data' contains your diagnostic plots and data
      diagnostic_analysis <- diagnostic_data()
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
        lt_data()$summary[c("Measure", "Message", "Value")],
        file = file.path(main_plot_path, "analysis", "lifetable_summary.csv"),
        row.names = FALSE
      )

      write.csv(
        lt_data()$lt$lt,
        file = file.path(main_plot_path, "lifetable_results.csv"),
        row.names = FALSE
      )

      # Assuming you have a diagnostics table and text
      write.csv(
        diagnostic_data()$diagnostics_table,
        file = file.path(main_plot_path, "diagnostics", "diagnostics_summary.csv"),
        row.names = FALSE
      )

      writeLines(
        text = diagnostic_data()$diagnostics_text,
        con = file.path(main_plot_path, "diagnostics", "diagnostics_text.txt"),
      )

      # Zip only the "lifetable_analysis" folder
      setwd(main_plot_path)
      zip(file, files = list.files(".", full.names = TRUE, recursive = TRUE))
      setwd(tempdir()) # Reset working directory to tempdir()
    }
  )
}
