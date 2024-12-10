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

#' Run smoohthing for exposures/deaths together
#'
#'
#' @param output Shiny output object.
#' @param plots List of reactive expressions for plots.
#' @param data data to be saved in the download button
#' @param input Shiny input object.
#' @importFrom ggplot2 aes
#' @export
smooth_overall <- function(data_in, rough_exp, fine_exp, constraint_exp, u5m_exp, rough_deaths, fine_deaths, constraint_deaths, u5m_deaths, age_out) {
  expo <- smooth_flexible(
    data_in,
    variable = "Exposures",
    rough_method = rough_exp,
    fine_method = fine_exp,
    constrain_infants = constraint_exp,
    age_out = age_out,
    u5m = u5m_exp
  )

  deaths <- smooth_flexible(
    data_in,
    variable = "Deaths",
    rough_method = rough_deaths,
    fine_method = fine_deaths,
    constrain_infants = constraint_deaths,
    age_out = age_out,
    u5m = u5m_deaths
  )

  # Combine the data
  combined_data <-
    expo$data %>%
    left_join(deaths$data, by = c(".id", "Age")) %>%
    mutate(Rates = Deaths / Exposures) %>%
    select(-contains(".y")) %>%
    rename_with(~ gsub("\\.x", "", .x))

  # Split by id
  id_list <-
    combined_data %>%
    group_split(.id)

  # Create a list to hold the results by group ID
  rates_tmp <- list()
  rates <- list()

  # Iterate over each group
  for (group_data in id_list) {
    group_id <- unique(group_data$.id)

    # Create the plot
    figure <-
      group_data %>%
      ggplot(aes(Age, Rates)) +
      geom_line() +
      theme_minimal()

    # Store original data (without mutations) and adjusted data (e.g., `plot_y`)
    data_original <- group_data
    data_adjusted <- group_data

    # Nest figure, original, and adjusted data under the corresponding group ID
    rates_tmp[[as.character(group_id)]] <- list(
      figure = figure,
      data_original = data_original,
      data_adjusted = data_adjusted
    )
  }

  rates$figures <- rates_tmp

  lst_dt <- list()
  lst_dt$data <- combined_data
  lst_dt$figures$exposures <- expo$figures
  lst_dt$figures$deaths <- deaths$figures
  lst_dt$figures$rates <- rates$figures

  lst_dt
}


# Define adjustment steps list (this remains inside app_server)
# Define the UI and execution for smoothing exposures and deaths
adjustment_steps <- list(
  smoothing = list(
    name = "Smoothing",
    input_ui = function() {
      div(
        fluidRow(
          # Left column for Exposures
          column(
            6,
            h4("Exposures Parameters"),
            selectInput(
              "smoothing_rough_exp",
              "Rough Method (Exposures)",
              choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
              selected = "auto"
            ),
            selectInput(
              "smoothing_fine_exp",
              "Fine Method (Exposures)",
              choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
              selected = "auto"
            ),
            numericInput("smoothing_u5m_exp", "Under-5 Mortality (optional, Exposures)", value = NULL),
            shiny.semantic::checkbox_input("smoothing_constrain_infants_exp", "Constrain Infants (Exposures)", is_marked = TRUE)
          ),

          # Right column for Deaths
          column(
            6,
            h4("Deaths Parameters"),
            selectInput(
              "smoothing_rough_deaths",
              "Rough Method (Deaths)",
              choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
              selected = "auto"
            ),
            selectInput(
              "smoothing_fine_deaths",
              "Fine Method (Deaths)",
              choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
              selected = "auto"
            ),
            numericInput("smoothing_u5m_deaths", "Under-5 Mortality (optional, Deaths)", value = NULL),
            shiny.semantic::checkbox_input("smoothing_constrain_infants_deaths", "Constrain Infants (Deaths)", is_marked = TRUE)
          )
        ),

        # Shared Age Output
        h4("Shared Parameters"),
        selectInput(
          "smoothing_age_out",
          "Age Output",
          choices = c("single", "abridged", "5-year"),
          selected = "abridged"
        )
      )
    },
    execute = function(input) {
      # Call smooth_overall with the arguments from the UI
      rlang::expr(
        smooth_overall(
          .data,
          rough_exp = !!input$smoothing_rough_exp,
          fine_exp = !!input$smoothing_fine_exp,
          constraint_exp = !!input$smoothing_constrain_infants_exp,
          u5m_exp = !!input$smoothing_u5m_exp,
          rough_deaths = !!input$smoothing_rough_deaths,
          fine_deaths = !!input$smoothing_fine_deaths,
          constraint_deaths = !!input$smoothing_constrain_infants_deaths,
          u5m_deaths = !!input$smoothing_u5m_deaths,
          age_out = !!input$smoothing_age_out
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

  # Add this after setup_adjustment_steps function
  update_step_tooltips <- function(executed_steps) {
    lapply(names(adjustment_steps), function(step_name) {
      # Find the index of the current step in executed steps
      step_index <- match(step_name, executed_steps)

      if (is.na(step_index)) {
        tooltip_text <- "This step will use the initial uploaded data"
      } else {
        if (step_index == 1) {
          tooltip_text <- "This step uses the initial uploaded data"
        } else {
          prev_step <- executed_steps[step_index - 1]
          prev_step_name <- names(adjustment_steps)[match(prev_step, names(adjustment_steps))]
          tooltip_text <- sprintf(
            "This step uses the output data from the '%s' step",
            adjustment_steps[[prev_step_name]]$name
          )
        }
      }

      # Update the tooltip content using Shiny
      output[[paste0("tooltip_text_", step_name)]] <- renderText({
        tooltip_text
      })
    })
  }

  # At the beginning of app_server
  current_tab <- reactiveVal()

  # Update current_tab in transition handlers
  observeEvent(input$start_button, {
    current_tab("upload_page")
  })

  # Update current_tab in transition handlers
  observeEvent(input$forward_step, {
    current_tab("preprocessing_page")
  })

  observeEvent(input$forward_to_lifetable, {
    current_tab("lifetable_page")
  })

  # Add the instruction observers
  observeEvent(input$upload_instructions, {
    if (current_tab() == "upload_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#continue_no_data", "#file-input"),
          intro = c(
            "If you want to test out the app's functionalty, click here to use a sample dataset. A pop up window will be prompted for the grouping variables that identify your data. Simply tick the no-grouping box and you'll be able to inspect the diagnostics and perform additional preprocessing.",
            "Alternatively, upload your own data. Make sure your data is cleaned and ready for analysis (no age duplicates, unless by groups), correct column names (we expect at least the basic to be 'Sex', 'Deaths' and 'Exposures'), either single or abridged ages but nothing else."
          )
        )
      ))
    }
  })

  # Add the instruction observers
  observeEvent(input$preprocessing_instructions, {
    if (current_tab() == "preprocessing_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#adjustment_tabs", "#tooltip-preprocess", "#forward_to_lifetable"),
          intro = c(
            "These are the tabs for the preprocessing. Every time you click execute from each of the preprocessing it will use the output of the previous preprocessing step.",
            "In this icon you can see which data is used as input for each preprocessing, whether the initial data or the output of a previous preprocessing step.",
            "Once you've implemented all the preprocessing needed, you can click next to use the output of the final preprocessing step as the input to the lifetable. Equally important, if you've applied a preprocessing step, you'll see the sequential order at which they were executed as pills above this button. You can delete any step and the chain of excution will be recalculated."
          )
        )
      ))
    }
  })

  observeEvent(input$lifetable_instructions, {
    if (current_tab() == "lifetable_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#calculate_lt"),
          intro = c(
            "Here you can estimate the lifetable using the output of the final step of your preprocessing. After tweaking the parameters below, click here to see the plot and table outputs."
          )
        )
      ))
    }
  })


  # Add this observer after the existing executed_adjustments observer
  observeEvent(executed_adjustments(), {
    update_step_tooltips(executed_adjustments())
  })


  # Initialize reactive values
  data_in <- reactiveVal()

  # General variable to indicate
  group_selection_passed <- reactiveVal(FALSE)
  selected_grouping_vars <- reactiveVal(NULL)

  # Handle file upload
  uploaded_data <- reactive({
    req(input$file1) # Replace 'file_upload' with your actual file input ID

    # Reset variables after new file upload
    group_selection_passed(FALSE)
    selected_grouping_vars(NULL)
    lt_data(NULL)
    executed_adjustments(character(0))
    output$validation_results <- NULL

    # Process the uploaded file
    handle_file_upload(input)
  })

  # Handle sample data
  sample_data <- handle_sample_data()

  # Update data_in when the sample data button is clicked
  observeEvent(input$continue_no_data, {
    # Reset variables after new file upload
    group_selection_passed(FALSE)
    selected_grouping_vars(NULL)
    lt_data(NULL)
    executed_adjustments(character(0))
    output$validation_results <- NULL

    data_in(sample_data())
  })

  observe({
    data_in(uploaded_data())
  })

  # Display table as example..
  output$data_table <- renderRHandsontable(renderDataTable(sample_data()))

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

  # Initialize diagnostic_data as a reactiveVal
  diagnostic_data <- reactiveVal(NULL)

  # Trigger setup_diagnostic_data when the Diagnostics button is clicked
  observeEvent(input$diagnostics, {
    req(group_selection_passed() == TRUE)
    print("Diagnostics button clicked")

    # Update diagnostic_data reactively with lazy loading
    diagnostic_data(
      setup_diagnostic_data(
        input,
        output,
        session,
        data_in,
        group_selection_passed,
        selected_grouping_vars,
        grouping_dropdowns,
        show_modal = TRUE,
        download = FALSE # Use lazy loading for interactive viewing
      )
    )

  })

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

      if (step_name %in% c("smoothing")) {
        extra_dropdowns <- div(
          selectInput(
            inputId = "group_select_smoothing_var",
            label = "Variable",
            choices = c("Exposures" = "exposures", "Deaths" = "deaths", "Rates" = "rates"),
            selected = "exposures"
          )
        )
      } else {
        extra_dropdowns <- div()
      }

      # Render group selection dropdown UI
      output[[paste0("adjustment_group_select_ui_", step_name)]] <- renderUI({
        div(
          class = "outer-container",
          style = "width: 100%; position: relative; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
          div(
            class = "grouping-dropdowns-container",
            style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
            lapply(grouping_dropdowns(), function(dropdown) {
              div(style = "min-width: 150px", dropdown)
            })
          ),
          div(
            id = "tooltip-preprocess",
            class = "tooltip-wrapper",
            style = "position: absolute; right: 0; top: 0;",
            TooltipHost(
              content = textOutput(paste0("tooltip_text_", step_name)),
              delay = 0,
              Image(
                src = "www/info.png",
                width = "20px",
                shouldStartVisible = TRUE
              )
            )
          ),
          div(
            class = "grouping-dropdowns-container",
            style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
            extra_dropdowns
          )
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


    if (step_name %in% c("smoothing")) {
      plot_list <- plot_list[[input$group_select_smoothing_var]]
    }

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
    if (!is.null(preprocessing_results())) {
      final_data <- preprocess_exec()$final_result()
    } else {
      final_data <- data_in()
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
          actionButton("download_all", "Download", class = "ui blue button")
        )
      } else {
        div(
          style = "margin-left: auto;",
          actionButton("download_all", "Download", class = "ui blue button")
        )
      }
    }
  })

  download_choices <- c(
    "all" = "Download all (diagnostics, preprocessing steps and lifetable results)",
    "lifetable" = "Download life table results",
    "preprocessing" = "Download preprocessing results",
    "diagnostics" = "Download diagnostic results"
  )

  output$download_modal <- renderUI({
    shiny.semantic::modal(
      id = "download-modal",
      header = "Download Options",
      shiny.semantic::multiple_radio(
        "download_option",
        "Select an option:",
        choices = download_choices,
        choices_value = names(download_choices)
      ),
      footer = tagList(
        shiny.semantic::button("cancel_download", "Cancel"),
        downloadButton("confirm_download", "Download")
      )
    )
  })

  # Observe the download button click to show modal dialog
  observeEvent(input$cancel_download, {
    shiny.semantic::hide_modal("download-modal")
  })

  # Observe the download button click to show modal dialog
  observeEvent(input$download_all, {
    # Show the modal dialog
    shiny.semantic::show_modal("download-modal")
  })

  # Define the download handler based on selected option
  output$confirm_download <- downloadHandler(
    filename = function() {
      paste0("lifetable_analysis_", input$download_option, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempfile()
      dir.create(temp_dir)

      # Function to save plots and data
      save_plots_and_data <- function(plot_list, data_list, base_path, folder_name) {
        lapply(names(plot_list), function(plot_name) {
          plot_folder_path <- file.path(base_path, folder_name, plot_name)
          dir.create(plot_folder_path, recursive = TRUE)

          # Save the plot
          ggsave(
            filename = file.path(plot_folder_path, paste0(plot_name, ".png")),
            plot = plot_list[[plot_name]],
            device = "png"
          )

          # Save the DataTable as a CSV
          write.csv(
            data_list[[plot_name]],
            file = file.path(plot_folder_path, paste0(plot_name, ".csv")),
            row.names = FALSE
          )
        })
      }

      # Function to save diagnostic results
      save_diagnostic_results <- function(base_path) {
        # Get full diagnostic analysis with all plots
        diagnostic_analysis <- setup_diagnostic_data(
          input,
          output,
          session,
          data_in,
          group_selection_passed,
          selected_grouping_vars,
          grouping_dropdowns,
          show_modal = FALSE,
          download = TRUE  # Generate all plots for download
        )
        
        if (is.null(diagnostic_analysis)) {
          diagnostic_data(
            setup_diagnostic_data(
              input,
              output,
              session,
              data_in,
              group_selection_passed,
              selected_grouping_vars,
              grouping_dropdowns,
              show_modal = FALSE
            )
          )

          diagnostic_analysis <- diagnostic_data()
        }

        # Prepare diagnostics path
        diagnostics_path <- file.path(base_path, "diagnostics")
        dir.create(diagnostics_path, recursive = TRUE, showWarnings = FALSE)

        # Initialize an empty list to store tables from each group
        all_tables <- list()

        # Process diagnostic tables by group
        for (group_id in names(diagnostic_analysis$all_tables())) {
          # Add the group ID to each row in the table and store in the list
          group_table <-
            diagnostic_analysis$all_tables()[[group_id]] %>%
            mutate(.id = group_id)

          all_tables[[group_id]] <- group_table
        }

        # Combine all tables into a single data frame
        combined_table <-
          dplyr::bind_rows(all_tables) %>%
          mutate(`.id` = as.numeric(`.id`)) %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        # Save the combined diagnostic table with .id column
        write.csv(
          combined_table,
          file = file.path(diagnostics_path, "table_diagnostics.csv"),
          row.names = FALSE
        )

        # Process diagnostic plots by group and type
        for (group_id in names(diagnostic_analysis$all_plots()$ggplot)) {
          group_plots <- diagnostic_analysis$all_plots()$ggplot[[group_id]]

          # Save each plot type for the group
          for (plot_type in names(group_plots)) {
            # Define the filename based on the plot type and group ID
            plot_filename <- paste0(plot_type, "_group_", group_id, ".png")

            # Save the plot to the diagnostics folder
            ggsave(
              filename = file.path(diagnostics_path, plot_filename),
              plot = group_plots[[plot_type]]$figure,
              device = "png"
            )
          }
        }
      }

      # Function to save lifetable results
      save_lifetable_results <- function(base_path) {
        lt_analysis <- lt_data()()

        # Extract group IDs from lt_data
        group_ids <- unique(lt_analysis$lt$.id)

        path_folder <- file.path(base_path, "lifetable")

        # Define the directory for the current group
        dir.create(path_folder, recursive = TRUE, showWarnings = FALSE)

        lt_summary <-
          lt_analysis$summary %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        lt_res <-
          lt_analysis$lt %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        write.csv(
          lt_summary,
          file = file.path(path_folder, "lifetable_summary.csv"),
          row.names = FALSE
        )

        write.csv(
          lt_res,
          file = file.path(path_folder, "lifetable_results.csv"),
          row.names = FALSE
        )

        # Iterate over each group ID
        for (group_id in group_ids) {
          # Check if plots exist for the current group ID
          if (as.character(group_id) %in% names(lt_analysis$plots)) {
            group_plots <- lt_analysis$plots[[as.character(group_id)]]

            # Define which plot types to save for each group
            plot_types <- names(group_plots)

            # Iterate over each plot type and save if it exists
            for (plot_type in plot_types) {
              if (!is.null(group_plots[[plot_type]])) {
                # Save the plot as PNG
                ggsave(
                  filename = file.path(
                    path_folder,
                    paste0(plot_type, "_group_", group_id, "_plot.png")
                  ),
                  plot = group_plots[[plot_type]][[paste0(plot_type, "_plot")]],
                  device = "png"
                )
              }
            }
          }
        }
      }

      # Function to save preprocessing results
      save_preprocessing_results <- function(base_path) {
        results <- preprocess_exec()$results
        lapply(names(results), function(step_name) {
          step_result <- results[[step_name]]
          # Assuming step_result contains plots and data

          # For each variable (e.g., exposures, deaths, rates)
          plot_list <- step_result$plot_result
          data_output <- step_result$data_output
          plot_folder_path <- file.path(
            base_path,
            "preprocessing",
            step_name
          )

          dir.create(plot_folder_path, recursive = TRUE)

          if (is.list(plot_list) && step_name == "smoothing") {
            lapply(names(plot_list), function(var_name) {
              var_plots <- plot_list[[var_name]]
              lapply(names(var_plots), function(group_label) {
                # Save the plot
                plot_item <- var_plots[[group_label]]$figure
                plot_name <- paste0(var_name, "_group_", group_label, ".png")

                ggsave(
                  filename = file.path(plot_folder_path, plot_name),
                  plot = plot_item,
                  device = "png"
                )
              })
            })
          } else {
            lapply(names(plot_list), function(group_label) {
              # Save the plot
              plot_item <- var_plots[[group_label]]$figure
              plot_name <- paste0("group_", group_label, ".png")

              ggsave(
                filename = file.path(plot_folder_path, plot_name),
                plot = plot_item,
                device = "png"
              )
            })
          }

          lt_res <-
            data_output %>%
            left_join(labels_df()) %>%
            select(.id, .id_label, selected_grouping_vars(), everything())

          # Save the data
          write.csv(
            data_output,
            file = file.path(plot_folder_path, "output_data.csv"),
            row.names = FALSE
          )
        })
      }

      # Depending on the selected option, call the appropriate save functions
      if (input$download_option == "all") {
        save_diagnostic_results(temp_dir)
        save_preprocessing_results(temp_dir)
        save_lifetable_results(temp_dir)
      } else if (input$download_option == "lifetable") {
        save_lifetable_results(temp_dir)
      } else if (input$download_option == "preprocessing") {
        save_preprocessing_results(temp_dir)
      } else if (input$download_option == "diagnostics") {
        save_diagnostic_results(temp_dir)
      }

      # Zip the files
      zip::zipr(
        zipfile = file,
        files = list.dirs(temp_dir, recursive = FALSE)
      )
    }
  )
}
