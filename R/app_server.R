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
      # input$tabSelector will already be an internal name because of how we set up selectInput
      paste(to_snake(input$tabSelector), "plot.png", sep = "_")
    },
    content = function(file) {
      # Use the internal name directly from input$tabSelector
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
#' @param i18n An optional i18n object for translation.
#' @importFrom ggplot2 aes labs theme_minimal theme_light element_text
#' @importFrom dplyr rename_with left_join mutate select group_split sym
#' @export
smooth_overall <- function(data_in, rough_exp, fine_exp, constraint_exp, u5m_exp, rough_deaths, fine_deaths, constraint_deaths, u5m_deaths, age_out, i18n) {
  expo <- smooth_flexible(
    data_in,
    variable = "Exposures",
    rough_method = rough_exp,
    fine_method = fine_exp,
    constrain_infants = constraint_exp,
    age_out = age_out,
    u5m = u5m_exp,
    i18n = i18n
  )

  deaths <- smooth_flexible(
    data_in,
    variable = "Deaths",
    rough_method = rough_deaths,
    fine_method = fine_deaths,
    constrain_infants = constraint_deaths,
    age_out = age_out,
    u5m = u5m_deaths,
    i18n = i18n
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

    rates <- translate_text("Rates", i18n)
    rates_text <- translate_text("Rates (Deaths / Exposures)", i18n)
    age_text <- translate_text("Age", i18n)

    names(group_data)[names(group_data) == "Rates"] <- rates
    names(group_data)[names(group_data) == "Age"] <- age_text

    sym_rates <- sym(rates)
    sym_rates_title <- sym(rates_text)
    sym_age <- sym(age_text)

    # Create the plot
    figure <-
      group_data %>%
      ggplot(aes(!!sym_age, !!sym_rates)) +
      geom_line() +
      theme_minimal() +
      labs(title = rates_text) +
      theme_light() + 
      theme(axis.text = element_text(color = "black"), plot.title = element_text(size = 12)) + 
      labs(title = rates_text)

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
    input_ui = function(i18n) {
      div(
        # Add explanation div at the top
        div(
          style = "margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          p(i18n$t("This step simultaneously smooths age patterns in both exposures and deaths."))
        ),

        # Rest of the UI remains the same
        fluidRow(
          column(
            6,
            h4(i18n$t("Exposures Parameters")),
            selectInput(
              "smoothing_rough_exp",
              i18n$t("Rough Method (Exposures)"),
              choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
              selected = "auto"
            )
          ),
          column(
            6,
            h4(i18n$t("Deaths Parameters")),
            selectInput(
              "smoothing_rough_deaths",
              i18n$t("Rough Method (Deaths)"),
              choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
              selected = "auto"
            )
          )
        ),

        # Shared basic parameter
        h4(i18n$t("Shared Parameters")),
        selectInput(
          "smoothing_age_out",
          i18n$t("Age Output"),
          choices = c("single", "abridged", "5-year"),
          selected = "single"
        ),

        # Toggle button
        action_button("toggle_advanced_smoothing", i18n$t("Show Advanced Options"), class = "ui button"),

        # Advanced options container
        div(
          id = "advanced_smoothing_inputs",
          style = "display: none;",  # Hidden by default

          fluidRow(
            column(
              6,
              selectInput(
                "smoothing_fine_exp",
                i18n$t("Fine Method (Exposures)"),
                choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
                selected = "auto"
              ),
              numericInput("smoothing_u5m_exp", i18n$t("Under-5 Mortality (optional, Exposures)"), value = NULL),
              shiny.semantic::checkbox_input("smoothing_constrain_infants_exp", i18n$t("Constrain Infants (Exposures)"), is_marked = TRUE)
            ),
            column(
              6,
              selectInput(
                "smoothing_fine_deaths",
                i18n$t("Fine Method (Deaths)"),
                choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
                selected = "auto"
              ),
              numericInput("smoothing_u5m_deaths", i18n$t("Under-5 Mortality (optional, Deaths)"), value = NULL),
              shiny.semantic::checkbox_input("smoothing_constrain_infants_deaths", i18n$t("Constrain Infants (Deaths)"), is_marked = TRUE)
            )
          )
        ),

        # Add JavaScript for toggle functionality
        tags$script("
          $(document).ready(function() {
            let clickTimeout;
            
            // Remove any existing click handlers first
            $(document).off('click', '#toggle_advanced_smoothing');
            
            // Add debounced click handler
            $(document).on('click', '#toggle_advanced_smoothing', function() {
              // Clear any pending timeouts
              if (clickTimeout) clearTimeout(clickTimeout);
              
              // Set a new timeout
              clickTimeout = setTimeout(function() {
                console.log('Button clicked');
                console.log('Current button text:', $('#toggle_advanced_smoothing').text());
                console.log('Current visibility:', $('#advanced_smoothing_inputs').is(':visible'));
                
                $('#advanced_smoothing_inputs').slideToggle('fast', function() {
                  var isVisible = $('#advanced_smoothing_inputs').is(':visible');
                  console.log('After toggle - visibility:', isVisible);
                  console.log('Sending visibility state to Shiny');
                  Shiny.setInputValue('advanced_is_visible', isVisible);
                });
              }, 100); // 100ms debounce
            });
          });
        ")
      )
    },
    execute = function(input, i18n) {
      # Execute function remains unchanged
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
          age_out = !!input$smoothing_age_out,
          i18n = i18n
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
#' @importFrom shiny.i18n update_lang
#' @importFrom ODAPbackend lt_summary smooth_flexible
#' @importFrom utils zip
#' @export
lifetable_server <- function(input, output, session) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  # Add this after setup_adjustment_steps function
  update_step_tooltips <- function(executed_steps) {
    lapply(names(adjustment_steps), function(step_name) {
      output[[paste0("tooltip_text_", step_name)]] <- renderText({
        step_index <- match(step_name, executed_steps)
        
        if (is.na(step_index)) {
          i18n$t("This step will use the initial uploaded data")
        } else if (step_index == 1) {
          i18n$t("This step uses the initial uploaded data")
        } else {
          prev_step <- executed_steps[step_index - 1]
          prev_step_name <- names(adjustment_steps)[match(prev_step, names(adjustment_steps))]
          sprintf(
            paste0(i18n$t("This step uses the output data from the"), " '%s' ", i18n$t("step")),
            adjustment_steps[[prev_step_name]]$name
          )
        }
      })
    })
  }

  i18n <- usei18n_local()

  # Create reactive value for language changes to propagate to modules
  session$userData$language_version <- reactiveVal(0)

  # Language change observer
  observeEvent(c(input$selected_language, input$lt_advanced_is_visible), {

    # Update language if it changed
    if (!is.null(input$selected_language)) {
      update_lang(input$selected_language)
      # Increment language version to trigger module UI updates
      session$userData$language_version(session$userData$language_version() + 1)

      # Update the tabSelector dropdown with freshly translated names
      # This preserves internal names as values while showing translated display names
      if (!is.null(input$tabSelector)) {
        updateSelectInput(
          session,
          "tabSelector",
          choices = setNames(tabNames_internal, sapply(tabNames_internal, function(name) i18n$t(name))),
          selected = input$tabSelector # Maintain the current selection
        )
      }

      # The variable dropdown is now handled through renderUI with a dependency on the current language
    }

    # Update button text based on visibility
    if (!is.null(input$lt_advanced_is_visible)) {
      i18n_current <- usei18n_local()
      new_label <- if (input$lt_advanced_is_visible) i18n_current$t("Hide Advanced Options") else i18n_current$t("Show Advanced Options")
      updateActionButton(
        session,
        "toggle_advanced",
        label = new_label
      )
    }
  })

  # Reactive UI for lifetable landing page
  output$lifetable_landing_hero <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "hero-section",
      div(
        style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
        actionButton("back_to_modules", i18n$t('← Previous'), class = "ui grey button")
      ),
      h1(i18n$t("Life Table Analysis Platform"), class = "hero-title"),
      p(i18n$t("Transform mortality data into comprehensive life table analyses with just a few clicks"),
        class = "hero-subtitle")
    )
  })

  output$lifetable_landing_features <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "features-grid",
      # Upload Feature
      div(
        class = "feature-card",
        icon("upload"),
        h3(i18n$t("Upload Your Data")),
        p(i18n$t("Import your mortality data in CSV format containing Age, Deaths, and Exposures"))
      ),
      # Diagnostics Feature
      div(
        class = "feature-card",
        icon("chart line"),
        h3(i18n$t("Run Diagnostics")),
        p(i18n$t("Analyze data quality and identify potential issues with built-in diagnostic tools"))
      ),
      # Transform Feature
      div(
        class = "feature-card",
        icon("magic"),
        h3(i18n$t("Transform Data")),
        p(i18n$t("Apply sophisticated smoothing and adjustments by groups"))
      ),
      # Results Feature
      div(
        class = "feature-card",
        icon("table"),
        h3(i18n$t("Get Results")),
        p(i18n$t("Download complete life table results and visualizations"))
      )
    )
  })

  output$lifetable_landing_action <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "action-section",
      actionButton("lifetable_start_button", i18n$t("Start"), class = "ui blue button")
    )
  })

  # Set landing page outputs to render even when hidden
  outputOptions(output, "lifetable_landing_hero", suspendWhenHidden = FALSE)
  outputOptions(output, "lifetable_landing_features", suspendWhenHidden = FALSE)
  outputOptions(output, "lifetable_landing_action", suspendWhenHidden = FALSE)

  # Reactive UI for upload page
  output$upload_page_back_button <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
      actionButton("back_to_lifetable_landing", i18n$t("← Previous"), class = "ui grey button")
    )
  })

  output$upload_page_info_box <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    tags$div(
      class = "info-box",
      shiny.semantic::tabset(
        tabs = list(
          list(
            menu = i18n$t("Upload Instructions"),
            content = div(
              h1(i18n$t("Data upload and validation")),
              p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:")),
              br(),
              div(
                style = "display: flex; gap: 5px;",
                div(
                  style = "",
                  rHandsontableOutput("data_table")
                ),
                div(
                  TooltipHost(
                    content = textOutput("exposures_tooltip_text"),
                    delay = 0,
                    Image(
                      src = "www/info.png",
                      width = "20px",
                      shouldStartVisible = TRUE
                    )
                  )
                )
              ),
              br(),
              strong(h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
              br(),
              action_button("upload_instructions", i18n$t("Instructions"), class = "ui blue button")
            )
          ),
          list(
            menu = i18n$t("Method Documentation"),
            content = div(
              uiOutput("lifetable_method_docs")
            )
          )
        )
      )
    )
  })

  output$upload_page_file_buttons <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      br(),
      div(
        class = "button-container-file",
        style = "display: flex; gap: 10px;",
        div(id = "file-input", file_input("file1", "", type = "flex-override")),
        uiOutput("modal_ui"),
        action_button(
          "continue_no_data",
          i18n$t("Use sample data"),
          class = "ui blue button",
          style = "height: 4%;"
        )
      )
    )
  })

  output$lifetable_method_docs <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    tagList(
      h3(i18n$t("Life Table Analysis")),
      p(i18n$t("A life table transforms mortality data into a comprehensive picture of survival patterns. Starting from observed deaths and population exposure, it calculates the probability of surviving to each age and the expected remaining years of life. This module produces complete period life tables following standard demographic methods.")),
      h4(i18n$t("What you'll learn from your data:")),
      div(
        class = "ui relaxed divided list",
        div(class = "item", div(class = "content",
          div(class = "header", i18n$t("Mortality rates (mx)")),
          div(class = "description", i18n$t("The observed death rate at each age, calculated from your deaths and exposure data."))
        )),
        div(class = "item", div(class = "content",
          div(class = "header", i18n$t("Survival probabilities (qx, px)")),
          div(class = "description", i18n$t("The chance of dying or surviving through each age interval, derived from mortality rates."))
        )),
        div(class = "item", div(class = "content",
          div(class = "header", i18n$t("Survivors and deaths (lx, dx)")),
          div(class = "description", i18n$t("How a hypothetical cohort of 100,000 births would diminish through life based on current mortality."))
        )),
        div(class = "item", div(class = "content",
          div(class = "header", i18n$t("Person-years (nLx, Tx)")),
          div(class = "description", i18n$t("Total years lived by the cohort at each age and cumulatively, used for life expectancy calculations."))
        )),
        div(class = "item", div(class = "content",
          div(class = "header", i18n$t("Life expectancy (ex)")),
          div(class = "description", i18n$t("Average remaining years of life at each age - the key summary measure of population health."))
        ))
      )
    )
  })

  # Set upload page outputs to render even when hidden
  outputOptions(output, "upload_page_back_button", suspendWhenHidden = FALSE)
  outputOptions(output, "upload_page_info_box", suspendWhenHidden = FALSE)
  outputOptions(output, "upload_page_file_buttons", suspendWhenHidden = FALSE)

  # Reactive UI for preprocessing page buttons
  output$preprocessing_buttons <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "button-group",
      action_button("back_to_diagnostics", i18n$t("← Previous"), class = "ui grey button"),
      action_button("preprocessing_instructions", i18n$t("Instructions"), class = "ui blue button"),
      action_button("forward_to_lifetable", i18n$t("Next →"), class = "ui blue button")
    )
  })

  # Reactive UI for lifetable page buttons
  output$lifetable_buttons <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "button-container-forecast",
      style = "display: flex; gap: 10px;",
      action_button("back_to_adjustment", i18n$t("← Previous"), class = "ui grey button"),
      action_button("lifetable_instructions", i18n$t("Instructions"), class = "ui blue button"),
      action_button("calculate_lt", i18n$t("Calculate"), class = "ui blue button"),
      action_button("reset_lt", i18n$t("Reset Options"), class = "ui blue button"),
      uiOutput("download_button")
    )
  })

  # Set preprocessing and lifetable button outputs to render even when hidden
  outputOptions(output, "preprocessing_buttons", suspendWhenHidden = FALSE)
  outputOptions(output, "lifetable_buttons", suspendWhenHidden = FALSE)

  # Reactive UI for lifetable input page
  output$lifetable_input_page <- renderUI({
    i18n <- usei18n_local()
    input$selected_language

    div(
      class = "ui form",
      # Basic Inputs as in initial setup
      create_field_set("", i18n$t("Desired Open Age Group"), "input_oanew", seq(70, 100, by = 5), 100),
      create_field_set("", i18n$t("Output Age Classes"), "input_age_out", c("single", "abridged"), "single"),
      uiOutput("sex_to_use"),
      uiOutput("ages_to_use"),
      uiOutput("extrap_from_data"),
      br(),
      # Advanced Inputs - Initially Hidden
      div(
        id = "advanced_inputs",
        style = "display: none;",
        div(
          class = "ui two column grid",
          div(
            class = "column",
            create_field_set("", i18n$t("Extrapolation Law"), "input_extrapLaw", EXTRAP_LAWS, EXTRAP_LAWS[1]),
            create_field_set("", i18n$t("Lifetable Radix"), "input_radix", input_selected = 100000, numeric_input = TRUE)
          ),
          div(
            class = "column",
            create_field_set("", i18n$t("Sex Ratio at Birth"), "input_srb", input_selected = 1.05, numeric_input = TRUE),
            create_field_set("", i18n$t("a(0) Rule"), "input_a0rule", c("Andreev-Kingkade", "Coale-Demeny"), "Andreev-Kingkade"),
            create_field_set("", i18n$t("a(x) Method"), "input_axmethod", c("UN (Greville)", "PASEX"), "UN (Greville)")
          )
        )
      ),
      # Dropdown to toggle Advanced Inputs
      action_button("toggle_advanced", i18n$t("Show Advanced Options"), class = "ui button"),
      br(),
      br(),
      uiOutput("download_buttons")
    )
  })

  # Set lifetable input page to render even when hidden
  outputOptions(output, "lifetable_input_page", suspendWhenHidden = FALSE)

  # At the beginning of app_server
  current_tab <- reactiveVal()

  # Add the instruction observers
  observeEvent(input$upload_instructions, {
    if (current_tab() == "upload_page") {
      rintrojs::introjs(session, options = list(
        steps = data.frame(
          element = c("#continue_no_data", "#file-input"),
          intro = c(
            i18n$t("If you want to test out the app's functionalty, click here to use a sample dataset. A pop up window will be prompted for the grouping variables that identify your data. Simply tick the no-grouping box and you'll be able to inspect the diagnostics and perform additional preprocessing."),
            i18n$t("Alternatively, upload your own data. Make sure your data is cleaned and ready for analysis (no age duplicates, unless by groups), correct column names (we expect at least the basic to be 'Sex', 'Deaths' and 'Exposures'), either single or abridged ages but nothing else.")
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
            i18n$t("These are the tabs for the preprocessing. Every time you click execute from each of the preprocessing it will use the output of the previous preprocessing step."),
            i18n$t("In this icon you can see which data is used as input for each preprocessing, whether the initial data or the output of a previous preprocessing step."),
            i18n$t("Once you've implemented all the preprocessing needed, you can click next to use the output of the final preprocessing step as the input to the lifetable. Equally important, if you've applied a preprocessing step, you'll see the sequential order at which they were executed as pills above this button. You can delete any step and the chain of excution will be recalculated.")
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
            i18n$t("Here you can estimate the lifetable using the output of the final step of your preprocessing. After tweaking the parameters below, click here to see the plot and table outputs.")
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
    plots_ready(FALSE)
    executed_adjustments(character(0))
    output$validation_results <- NULL

    # Process the uploaded file
    handle_file_upload(input, i18n)
  })

  # Handle sample data
  sample_data <- handle_sample_data(i18n)

  # Update data_in when the sample data button is clicked
  observeEvent(input$continue_no_data, {
    # Reset variables after new file upload
    group_selection_passed(FALSE)
    selected_grouping_vars(NULL)
    lt_data(NULL)
    plots_ready(FALSE)
    executed_adjustments(character(0))
    output$validation_results <- NULL

    data_in(sample_data())
  })

  observe({
    data_in(uploaded_data())
  })

  # Display table as example..
  output$data_table <- renderRHandsontable(renderDataTable(sample_data(), i18n))

  # Handle column selection
  handle_group_selection_modal(input, output, session, data_in, group_selection_passed, selected_grouping_vars, i18n, data_raw = data_in)

  # DF containing the id and labels
  labels_df <- reactive({
    req(data_in())
    data_in() %>%
      distinct(.id, .id_label)
  })

  # Setup group selection dropdowns
  grouping_dropdowns <- setup_grouping_dropdowns(selected_grouping_vars, data_in)

  # Validate data after group selection
  validate_data_after_group_selection(input, output, data_in, group_selection_passed, i18n)

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
        i18n,
        show_modal = TRUE,
        download = FALSE # Use lazy loading for interactive viewing
      )
    )
  })

  # Handle transitions
  handle_transitions(input, current_tab)

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
        # Create the variable dropdown as a dynamic renderUI to ensure it gets updated with language changes
        extra_dropdowns <- div(
          uiOutput("variable_selector_ui")
        )

        # This renderUI will get re-rendered when i18n$translator$get_translation_language() changes
        output$variable_selector_ui <- renderUI({

          # Define internal variable names for smoothing
          smoothing_var_internal <- c("exposures", "deaths", "rates")

          selectInput(
            inputId = "group_select_smoothing_var",
            label = i18n$t("Variable"),
            choices = setNames(
              smoothing_var_internal,
              sapply(c("Exposures", "Deaths", "Rates"), function(name) i18n$t(name))
            ),
            selected = if(!is.null(input$group_select_smoothing_var)) input$group_select_smoothing_var else "exposures"
          )
        })
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
      output[[paste0(step_name, "_inputs")]] <- renderUI(step$input_ui(i18n))

      # Render execute button with fresh i18n
      output[[paste0("execute_button_", step_name)]] <- renderUI({
        i18n <- usei18n_local()
        input$selected_language
        action_button(paste0("execute_", step_name), i18n$t("Execute"), class = "ui blue button")
      })

      # Create reactive expression for the function call
      step_func_calls[[step_name]] <- reactive({
        # Build the function call using the current inputs
        step$execute(input, i18n)
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
        # Helper function to create error plot
        create_error_plot <- function(error_msg) {
          plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                          marker = list(size = 0, opacity = 0)) |>
            plotly::add_annotations(
              text = paste0("<b>", i18n$t("Error"), ":</b><br><br>", gsub("\n", "<br>", error_msg)),
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "red"),
              xanchor = "center", yanchor = "middle"
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
              plot_bgcolor = "white", paper_bgcolor = "white",
              showlegend = FALSE
            ) |>
            plotly::config(displayModeBar = FALSE)
        }

        # Helper function to extract meaningful error message
        extract_error_msg <- function(e) {
          error_msg <- conditionMessage(e)
          # Strip ANSI escape codes (added by cli/rlang)
          error_msg <- gsub("\033\\[[0-9;]*m", "", error_msg)
          # Try to extract just the meaningful part after the last "Caused by error"
          if (grepl("Caused by error.*?:", error_msg)) {
            parts <- strsplit(error_msg, "Caused by error[^:]*:")[[1]]
            if (length(parts) > 1) {
              error_msg <- trimws(parts[length(parts)])
            }
          }
          # Remove leading "! " if present
          error_msg <- sub("^!\\s*", "", error_msg)
          error_msg
        }

        # Wrap everything in tryCatch to catch errors from preprocessing_results()
        tryCatch({
          results <- preprocessing_results()
          if (is.null(results) || !(step_name %in% names(results))) {
            return(NULL)
          }

          plot_data <- get_adjustment_plot(step_name)
          req(plot_data)
          plot_data
        }, error = function(e) {
          error_msg <- extract_error_msg(e)

          # Skip if error message is empty or just whitespace
          if (nchar(trimws(error_msg)) == 0) {
            return(NULL)
          }

          create_error_plot(error_msg)
        })
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
    translated_pills <- setNames(
      sapply(unname(pills), function(name) i18n$t(name)),
      sapply(names(pills), function(name) i18n$t(name))
    )

    print("translated_pills")
    print(translated_pills)
    print("pills")
    print(pills)
    render_adjustment_pills(translated_pills)
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

  observe({
    print("input$remove_pill")
    print(input$remove_pill)
  })

  # Observer to handle pill removal
  observeEvent(input$remove_pill, {
    current_steps <- executed_adjustments()
    tmp_current_steps <- current_steps
    names(tmp_current_steps) <- sapply(names(tmp_current_steps), function(name) i18n$t(name))
    current_steps <- current_steps[names(tmp_current_steps) != input$remove_pill]
    executed_adjustments(current_steps)
  })


  ## END INTERMEDIATE STEPS

  ## CALCULATE LIFETABLE

  # Create life table input UI and reactive values
  lt_input <- create_life_table_input_ui(data_in, grouping_dropdowns, tabNames_internal, input, output, i18n)

  # Create a reactive value to store the life table data and plots
  lt_data <- reactiveVal(NULL)

  # Create a reactive value to track if plots are ready to be rendered
  plots_ready <- reactiveVal(FALSE)

  # Create a reactive expression for the selected plots
  # A reactiveVal to track the last calculate_lt count for which we computed results
  last_calc_count <- reactiveVal(0)

  observeEvent(input$calculate_lt, {
    output$lt_summary_indication <- renderUI({
      div(
        div(
          class = "below-main-panel fade-in-icon",
          shiny.semantic::icon("arrow down circle", style = "font-size: 3rem;")
        ), 
        div(
          class = "below-main-panel",
          h3(i18n$t("Life Table Summary Statistics"))
        )
      )
    })
  })

  current_id <- reactive({
    current_id <- get_current_group_id(selected_grouping_vars, data_in, input)
    current_id
  })


  selected_plots <- reactive({
    # Force a dependency on the button click
    req(input$calculate_lt)

    # Check if the button has been clicked more times than the last time we computed
    if (input$calculate_lt > last_calc_count()) {
      print("ho")
      # Button was clicked again, re-run heavy computations

      if (!is.null(preprocessing_results())) {
        print("preprocess")
        final_data <- preprocess_exec()$final_result()
      } else {
        print("original")
        final_data <- data_in()
      }

      plots_ready(FALSE)
      # Re-run the heavy computation and store the results
      lt_data(calculate_lt_and_plots(final_data, input, i18n))
      plots_ready(TRUE)

      # Update the counter to reflect we've computed at this button click count
      last_calc_count(input$calculate_lt)
    }

    req(lt_data())
    plot_slot <- which(names(lt_data()()$plots) == as.character(current_id()))
    lt_data()()$plots[[plot_slot]]
  })


  # Create reactive expressions for each plot type using internal tab names
  lt_plots <- reactive({
    req(selected_plots())
    result_list <- list()

    # Map internal tab names to plots
    result_list[[tabNames_internal[1]]] <- reactive({  # Mortality Rate
      gg_plt <- selected_plots()$nMx$nMx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots()$nMx$nMx_plot_data
      )
    })

    result_list[[tabNames_internal[2]]] <- reactive({  # Survival Curve
      gg_plt <- selected_plots()$lx$lx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots()$lx$lx_plot_data
      )
    })

    result_list[[tabNames_internal[3]]] <- reactive({  # Death Distribution
      gg_plt <- selected_plots()$ndx$ndx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots()$ndx$ndx_plot_data
      )
    })

    result_list[[tabNames_internal[4]]] <- reactive({  # Conditional Death Probabilities
      gg_plt <- selected_plots()$nqx$nqx_plot
      list(
        gg = gg_plt,
        plotly = config(ggplotly(gg_plt), displayModeBar = FALSE),
        dt = selected_plots()$nqx$nqx_plot_data
      )
    })

    result_list[[tabNames_internal[5]]] <- reactive({  # Lifetable Results
      lt_data()()$lt
    })

    result_list
  })

  # Render plots
  observe({
    req(input$calculate_lt)

    output$plot_mortality_rate <- renderPlotly({
      lt_plots()[[tabNames_internal[1]]]()$plotly
    })

    output$plot_survival_curve <- renderPlotly({
      lt_plots()[[tabNames_internal[2]]]()$plotly
    })

    output$plot_conditional_death_probabilities <- renderPlotly({
      lt_plots()[[tabNames_internal[4]]]()$plotly
    })

    output$plot_death_distribution <- renderPlotly({
      lt_plots()[[tabNames_internal[3]]]()$plotly
    })

    output$plot_lifetable_results <- renderDT({
      dt <- lt_plots()[[tabNames_internal[5]]]()
      # browser()
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

  # Fixed internal tab names (English only, for internal references)
  tabNames_internal <- c(
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

    value_text <- translate_text("Value", i18n)
    label_text <- translate_text("Label", i18n)

    tbl[[value_text]] <- round(tbl[[value_text]], 2)

    datatable(
      tbl,
      options = list(
        dom = "t",
        paging = FALSE,
        info = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(targets = c(label_text), render = JS(RENDERKATEX))
        )
      )
    )
  })

  output$render_plots <- renderUI({
    lapply(seq_along(tabNames_internal), function(i) {
      id <- sprintf("tabContent%s", i)
      # Use internal tab names for plot naming consistency
      plotName <- gsub(" ", "_", tolower(tabNames_internal[i]))
      # Use internal names for logic
      OutputFunction <- ifelse(grepl("Lifetable Results", tabNames_internal[i]), DTOutput, plotlyOutput)

      # Define UI rendering for each tab
      output[[id]] <- renderUI({
        withSpinner(OutputFunction(sprintf("plot_%s", plotName), height = "600px"))
      })

      # Render conditional panel with the internal name as the value to match
      conditionalPanel(
        condition = sprintf("input.tabSelector === '%s'", tabNames_internal[i]),
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
          style = "width: 100%; display: grid; gap: 10px;",
          actionButton("download_all", i18n$t("Export"), class = "ui blue button")
        )
      } else {
        div(
          style = "margin-left: auto; display: flex; gap: 10px;",
          actionButton("download_all", i18n$t("Export"), class = "ui blue button")
        )
      }
    }
  })



  output$download_modal <- renderUI({
    download_choices <- c(
      "lifetable" = i18n$t("Download life table results"),
      "preprocessing" = i18n$t("Download preprocessing results"),
      "diagnostics" = i18n$t("Download diagnostic results"),
      "report" = i18n$t("Download PDF Report")
    )
    shiny.semantic::modal(
      id = "download-modal",
      header = i18n$t("Download Options"),
      shiny.semantic::multiple_radio(
        "download_option",
        i18n$t("Select an option:"),
        choices = unname(download_choices),
        choices_value = names(download_choices)
      ),
      footer = tagList(
        shiny.semantic::button("cancel_download", i18n$t("Cancel")),
        downloadButton("confirm_download", i18n$t("Download"))
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

      N_CORES <- get_n_cores()  # Use the new function to determine cores

      # Function to save diagnostic results
      save_diagnostic_results <- function(base_path) {
        message("Starting diagnostic results save...")

        # Get diagnostic analysis with parallel plot generation
        message("Generating diagnostic analysis...")
        diagnostic_analysis <- setup_diagnostic_data(
          input, output, session, data_in,
          group_selection_passed, selected_grouping_vars,
          grouping_dropdowns, i18n,
          show_modal = FALSE,
          download = TRUE
        )

        if (is.null(diagnostic_analysis)) {
          diagnostic_analysis <- diagnostic_data()
        }

        diagnostics_path <- file.path(base_path, "diagnostics")
        dir.create(diagnostics_path, recursive = TRUE, showWarnings = FALSE)

        # Save tables in parallel
        message("Saving diagnostic tables...")
        all_tables <- list()
        n_cores <- N_CORES

        # Prepare table data in parallel
        group_ids <- names(diagnostic_analysis$all_tables())
        table_results <- parallel::mclapply(group_ids, function(group_id) {
          group_table <- diagnostic_analysis$all_tables()[[group_id]] %>%
            mutate(.id = group_id)
          list(id = group_id, table = group_table)
        }, mc.cores = n_cores)

        # Combine tables
        all_tables <- lapply(table_results, function(x) x$table)
        names(all_tables) <- sapply(table_results, function(x) x$id)

        combined_table <- dplyr::bind_rows(all_tables) %>%
          mutate(`.id` = as.numeric(`.id`)) %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        write.csv(combined_table, file = file.path(diagnostics_path, "table_diagnostics.csv"), row.names = FALSE)
        message("Finished saving diagnostic tables")

        # Prepare plots for PDF
        message("Starting diagnostic plots PDF creation...")

        # Collect all plots (already in ggplot format)
        all_plots <- list()
        translated_group_text <- i18n$t("Group")

        for(group_id in names(diagnostic_analysis$all_plots()$ggplot)) {
          group_plots <- diagnostic_analysis$all_plots()$ggplot[[group_id]]
          for(plot_type in names(group_plots)) {
            original_plot <- group_plots[[plot_type]]$figure
            existing_title <- original_plot$labels$title
            
            if (is.null(existing_title) || !nzchar(trimws(existing_title))) {
              existing_title <- "" # Default to empty string if no title
            } else {
              # Remove semicolon and .id = digit pattern from existing title
              existing_title <- gsub(";?\\s*\\.\\s*id\\s*=\\s*\\d+", "", existing_title, perl = TRUE)
              existing_title <- trimws(existing_title) # Clean up any leading/trailing whitespace after gsub
            }

            new_title_prefix <- paste(translated_group_text, group_id)
            new_title <- if (nzchar(existing_title)) paste(new_title_prefix, "-", existing_title) else new_title_prefix
            
            all_plots[[length(all_plots) + 1]] <- original_plot +
              labs(title = new_title)
          }
        }

        # Save plots in a grid layout with batching
        if (length(all_plots) > 0) {
          message("Creating PDF with grid layout...")
          pdf(file = file.path(diagnostics_path, "diagnostic_plots.pdf"), width = 14, height = 10)

          # Process plots in batches of 8 (2 pages of 2x2 grid)
          batch_size <- 8
          n_batches <- ceiling(length(all_plots) / batch_size)

          for(i in seq_len(n_batches)) {
            start_idx <- (i-1) * batch_size + 1
            end_idx <- min(i * batch_size, length(all_plots))
            batch_plots <- all_plots[start_idx:end_idx]

            # Create and print grid for this batch
            plot_matrix <- gridExtra::marrangeGrob(
              batch_plots,
              nrow = 2,
              ncol = 2,
              top = paste("Page", i, "of", n_batches)
            )
            print(plot_matrix)

            # Force garbage collection after each batch
            gc()
          }
          dev.off()
        }
        message("Finished diagnostic plots PDF creation")
      }

      # Function to save lifetable results
      save_lifetable_results <- function(base_path) {
        lt_analysis <- lt_data()()
        group_ids <- unique(lt_analysis$lt$.id)
        path_folder <- file.path(base_path, "lifetable")
        dir.create(path_folder, recursive = TRUE, showWarnings = FALSE)

        lt_summary <- lt_analysis$summary %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        lt_res <- lt_analysis$lt %>%
          left_join(labels_df()) %>%
          select(.id, .id_label, everything())

        write.csv(lt_summary, file = file.path(path_folder, "lifetable_summary.csv"), row.names = FALSE)
        write.csv(lt_res, file = file.path(path_folder, "lifetable_results.csv"), row.names = FALSE)

        # Prepare plot data for parallel processing
        message("Starting lifetable plots PDF creation...")
        message("Collecting plots in parallel...")

        # Create plot collection function
        collect_group_plots <- function(group_id) {
          plots <- list()
          translated_group_text <- i18n$t("Group")

          if (as.character(group_id) %in% names(lt_analysis$plots)) {
            group_plots_data <- lt_analysis$plots[[as.character(group_id)]]
            plot_types <- names(group_plots_data)
            for(plot_type in plot_types) {
              if (!is.null(group_plots_data[[plot_type]])) {
                original_plot <- group_plots_data[[plot_type]][[paste0(plot_type, "_plot")]]
                existing_title <- original_plot$labels$title
                
                if (is.null(existing_title) || !nzchar(trimws(existing_title))) {
                  existing_title <- "" # Default to plot_type if no title or only whitespace
                } else {
                  # Remove semicolon and .id = digit pattern from existing title
                  existing_title <- gsub(";?\\s*\\.\\s*id\\s*=\\s*\\d+", "", existing_title, perl = TRUE)
                  existing_title <- trimws(existing_title) # Clean up any leading/trailing whitespace after gsub
                }

                new_title_prefix <- paste(translated_group_text, group_id)
                
                # Use the plot_type for the specific plot if existing_title is empty, otherwise prepend.
                new_title <- if (nzchar(existing_title)) {
                  paste(new_title_prefix, "-", existing_title)
                } else {
                  paste(new_title_prefix, "-", plot_type) # Fallback to plot_type if original title is missing/empty after cleaning
                }
                
                plots[[length(plots) + 1]] <- original_plot +
                  labs(title = new_title)
              }
            }
          }
          plots
        }

        # Parallel collection of plots
        n_cores <- N_CORES
        plot_lists <- parallel::mclapply(group_ids, collect_group_plots, mc.cores = n_cores)

        # Combine all plot lists
        all_plots <- do.call(c, plot_lists)
        message("Finished collecting plots")

        # Save plots in a grid layout with batching
        if (length(all_plots) > 0) {
          message("Creating PDF with grid layout...")
          pdf(file = file.path(path_folder, "lifetable_plots.pdf"), width = 14, height = 10)

          # Process plots in batches of 8 (2 pages of 2x2 grid)
          batch_size <- 8
          n_batches <- ceiling(length(all_plots) / batch_size)

          for(i in seq_len(n_batches)) {
            start_idx <- (i-1) * batch_size + 1
            end_idx <- min(i * batch_size, length(all_plots))
            batch_plots <- all_plots[start_idx:end_idx]

            # Create and print grid for this batch
            plot_matrix <- gridExtra::marrangeGrob(
              batch_plots,
              nrow = 2,
              ncol = 2,
              top = paste("Page", i, "of", n_batches)
            )
            print(plot_matrix)

            # Force garbage collection after each batch
            gc()
          }
          dev.off()
        }
        message("Finished lifetable plots PDF creation")
      }

      # Function to save preprocessing results
      save_preprocessing_results <- function(base_path) {
        results <- preprocess_exec()$results
        for(step_name in names(results)) {
          step_result <- results[[step_name]]
          plot_list <- step_result$plot_result
          data_output <- step_result$data_output
          plot_folder_path <- file.path(base_path, "preprocessing", step_name)
          dir.create(plot_folder_path, recursive = TRUE, showWarnings = FALSE)

          # Save data
          message(paste("Saving preprocessing data for step:", step_name))
          lt_res <- 
            data_output %>%
            left_join(labels_df()) %>%
            select(.id, .id_label, selected_grouping_vars(), everything())

          write.csv(data_output, file = file.path(plot_folder_path, "output_data.csv"), row.names = FALSE)
          message(paste("Finished saving preprocessing data for step:", step_name))

          # Prepare plot collection function based on step type
          message(paste("Starting preprocessing plots PDF creation for step:", step_name))
          message(paste("Collecting plots in parallel for step:", step_name))

          # Collect all plots
          all_plots <- list()
          translated_group_text <- i18n$t("Group")

          if (is.list(plot_list) && step_name == "smoothing") {
            for(var_name in names(plot_list)) {
              var_plots <- plot_list[[var_name]]
              for(group_label in names(var_plots)) {
                original_plot <- var_plots[[group_label]]$figure
                existing_title <- original_plot$labels$title
                
                if (is.null(existing_title) || !nzchar(trimws(existing_title))) {
                  existing_title <- "" # Default to empty string
                } else {
                  # Remove semicolon and .id = digit pattern from existing title
                  existing_title <- gsub(";?\\s*\\.\\s*id\\s*=\\s*\\d+", "", existing_title, perl = TRUE)
                  existing_title <- trimws(existing_title)
                }
                
                # The user previously decided to remove var_name from the prefix here.
                new_title_prefix <- paste(translated_group_text, group_label)
                new_title <- if (nzchar(existing_title)) paste(new_title_prefix, "-", existing_title) else new_title_prefix
                
                all_plots[[length(all_plots) + 1]] <- original_plot +
                  labs(title = new_title)
              }
            }
          } else { # For non-smoothing steps
            for(group_label in names(plot_list)) {
              original_plot <- plot_list[[group_label]]$figure
              existing_title <- original_plot$labels$title

              if (is.null(existing_title) || !nzchar(trimws(existing_title))) {
                existing_title <- "" # Default to empty string
              } else {
                # Remove semicolon and .id = digit pattern from existing title
                existing_title <- gsub(";?\\s*\\.\\s*id\\s*=\\s*\\d+", "", existing_title, perl = TRUE)
                existing_title <- trimws(existing_title)
              }

              new_title_prefix <- paste(translated_group_text, group_label)
              new_title <- if (nzchar(existing_title)) paste(new_title_prefix, "-", existing_title) else new_title_prefix
              
              all_plots[[length(all_plots) + 1]] <- original_plot +
                labs(title = new_title)
            }
          }

          # Save plots in a grid layout with batching
          if (length(all_plots) > 0) {
            message(paste("Creating PDF with grid layout for step:", step_name))
            pdf(file = file.path(plot_folder_path, "plots.pdf"), width = 14, height = 10)

            # Process plots in batches of 8 (2 pages of 2x2 grid)
            batch_size <- 8
            n_batches <- ceiling(length(all_plots) / batch_size)

            for(i in seq_len(n_batches)) {
              start_idx <- (i-1) * batch_size + 1
              end_idx <- min(i * batch_size, length(all_plots))
              batch_plots <- all_plots[start_idx:end_idx]

              # Create and print grid for this batch
              plot_matrix <- gridExtra::marrangeGrob(
                batch_plots,
                nrow = 2,
                ncol = 2,
                top = paste("Page", i, "of", n_batches)
              )
              print(plot_matrix)

              # Force garbage collection after each batch
              gc()
            }
            dev.off()
          }
          message(paste("Finished preprocessing plots PDF creation for step:", step_name))
        }
      }

      withProgress(message = i18n$t("Preparing download:"), value = 0, {
        # Calculate number of groups and estimated time
        n_groups <- length(unique(data_in()$.id))

        # Calculate estimated times for each component
        time_estimates <- list(
          diagnostics = n_groups * 1.4,
          preprocessing = n_groups * 1,
          lifetable = n_groups * 1.4,
          report = n_groups * 2
        )

        # Calculate total time based on download option
        total_time <- if(input$download_option == "all") {
          sum(unlist(time_estimates))
        } else {
          time_estimates[[input$download_option]]
        }

        # Format time estimate message
        format_time <- function(seconds) {
          if (seconds < 60) {
            return(paste(round(seconds), i18n$t("seconds")))
          } else if (seconds < 3600) {
            minutes <- floor(seconds / 60)
            return(paste(minutes, i18n$t("minutes")))
          } else {
            hours <- floor(seconds / 3600)
            minutes <- floor((seconds %% 3600) / 60)
            return(paste(hours, i18n$t("hours"), minutes, i18n$t("minutes")))
          }
        }

        # Create progress message with time estimate
        progress_msg <- paste(i18n$t("Preparing download... Estimated time:"), format_time(total_time))

        total_steps <- ifelse(input$download_option == "all", 3, 1)

        # Depending on the selected option, call the appropriate save functions
        if (input$download_option == "all") {
          message(paste("Starting parallel download of all components. Estimated time:", format_time(total_time)))

          # Create all necessary directories upfront
          dir.create(file.path(temp_dir, "diagnostics"), recursive = TRUE, showWarnings = FALSE)
          dir.create(file.path(temp_dir, "preprocessing"), recursive = TRUE, showWarnings = FALSE)
          dir.create(file.path(temp_dir, "lifetable"), recursive = TRUE, showWarnings = FALSE)

          # Pre-compute diagnostic analysis to avoid multiple computations
          message("Pre-computing diagnostic analysis...")
          diagnostic_analysis <- setup_diagnostic_data(
            input, output, session, data_in,
            group_selection_passed, selected_grouping_vars,
            grouping_dropdowns, i18n,
            show_modal = FALSE,
            download = TRUE
          )
          if (is.null(diagnostic_analysis)) {
            diagnostic_analysis <- diagnostic_data()
          }

          # Pre-compute lifetable analysis
          message("Pre-computing lifetable analysis...")
          lt_analysis <- lt_data()()

          # Pre-compute preprocessing results
          message("Pre-computing preprocessing results...")
          preprocess_results <- preprocess_exec()$results

          # Define tasks for parallel execution with pre-computed data
          tasks <- list(
            diagnostics = function() {
              message(paste("Starting diagnostics save. Estimated time:", format_time(time_estimates$diagnostics)))
              tryCatch({
                save_diagnostic_results(temp_dir)
                message("Finished diagnostics save")
              }, error = function(e) {
                message("Error in diagnostics save: ", e$message)
              })
            },
            preprocessing = function() {
              message(paste("Starting preprocessing save. Estimated time:", format_time(time_estimates$preprocessing)))
              tryCatch({
                save_preprocessing_results(temp_dir)
                message("Finished preprocessing save")
              }, error = function(e) {
                message("Error in preprocessing save: ", e$message)
              })
            },
            lifetable = function() {
              message(paste("Starting lifetable save. Estimated time:", format_time(time_estimates$lifetable)))
              tryCatch({
                save_lifetable_results(temp_dir)
                message("Finished lifetable save")
              }, error = function(e) {
                message("Error in lifetable save: ", e$message)
              })
            }
          )

          incProgress(0.2, detail = progress_msg)

          # Execute tasks in parallel with error handling
          message("Executing parallel saves...")
          n_cores <- N_CORES
          results <- parallel::mclapply(
            tasks,
            function(task) {
              # Force garbage collection before each task
              gc()
              task()
            },
            mc.cores = n_cores
          )

          # Check for any errors
          errors <- sapply(results, inherits, "error")
          if (any(errors)) {
            error_messages <- sapply(results[errors], function(e) e$message)
            warning("Some components failed to save: ", paste(error_messages, collapse = "; "))
          }

          message("Finished parallel execution of all components")
          incProgress(1, detail = paste(i18n$t("All components saved. Total time:"), format_time(total_time)))

        } else if (input$download_option == "lifetable") {
          est_time <- format_time(time_estimates$lifetable)
          message(paste("Starting lifetable save. Estimated time:", est_time))
          incProgress(0.5, detail = paste(i18n$t("Saving lifetable results... Estimated time:"), est_time))
          save_lifetable_results(temp_dir)
        } else if (input$download_option == "preprocessing") {
          est_time <- format_time(time_estimates$preprocessing)
          message(paste("Starting preprocessing save. Estimated time:", est_time))
          incProgress(0.5, detail = paste(i18n$t("Saving preprocessing results... Estimated time:"), est_time))
          save_preprocessing_results(temp_dir)
        } else if (input$download_option == "diagnostics") {
          est_time <- format_time(time_estimates$diagnostics)
          message(paste("Starting diagnostics save. Estimated time:", est_time))
          incProgress(0.5, detail = paste(i18n$t("Saving diagnostic results... Estimated time:"), est_time))
          save_diagnostic_results(temp_dir)
        } else if (input$download_option == "report") {
          est_time <- format_time(time_estimates$report)
          message(paste("Generating PDF report. Estimated time:", est_time))
          incProgress(0.3, detail = paste(i18n$t("Preparing diagnostic data... Estimated time:"), est_time))
          
          # Ensure we have diagnostic data
          if (is.null(diagnostic_data())) {
            diagnostic_analysis <- setup_diagnostic_data(
              input, output, session, data_in,
              group_selection_passed, selected_grouping_vars,
              grouping_dropdowns, i18n,
              show_modal = FALSE,
              download = TRUE
            )
            diagnostic_data(diagnostic_analysis)  # Save the diagnostic analysis for future use
          } else {
            diagnostic_analysis <- diagnostic_data()
          }
          
          # Preprocess diagnostic data to resolve reactive expressions
          preprocessed_diagnostics <- preprocess_diagnostic_data(diagnostic_analysis)
          
          incProgress(0.5, detail = paste(i18n$t("Generating PDF report... Estimated time:"), est_time))
          # Generate report and copy to temp directory
          labels_names <- setNames(labels_df()$.id_label, labels_df()$.id)
          
          # Get preprocessing results if they exist and evaluate them
          preprocessing_data <- NULL
          if (!is.null(preprocess_exec()) && length(executed_adjustments()) > 0) {
            preprocessing_data <- preprocess_exec()$results
          }
          
          report_file <- generate_analysis_report(
            lt_results = lt_data()(),
            diagnostic_results = preprocessed_diagnostics,
            preprocessing_results = preprocessing_data,
            group_labels = labels_names,
            i18n = i18n
          )
          dir.create(file.path(temp_dir, "report"), recursive = TRUE, showWarnings = FALSE)
          file.copy(report_file, file.path(temp_dir, "report", "report.pdf"))
        }

        # Final step - creating zip file (only for non-report options)
        incProgress(1, detail = i18n$t("Creating zip file..."))
        message("Creating zip file...")
        print(temp_dir)
        print(list.dirs(temp_dir, recursive = FALSE))
        zip::zipr(
          zipfile = file,
          files = list.dirs(temp_dir, recursive = FALSE)
        )
        message("Download preparation complete")
      })
    }
  )

  # Add tooltip text handler
  output$exposures_tooltip_text <- renderText({
    i18n <- usei18n_local()
    input$selected_language
    i18n$t("Exposures refer to the person-years lived over the same period where Deaths were registered. If Deaths refer to a single year, then sometimes mid-year population can be used to approximate Exposures.")
  })
}

# Helper function for translation with fallback
translate_text <- function(text, i18n = NULL) {
  if (!is.null(i18n) && !is.null(text)) {
    tryCatch({
      return(i18n$t(text))
    }, error = function(e) {
      return(text)  # Fallback to original if translation fails
    })
  }
  return(text)
}

app_server <- function(input, output, session) {
  # Create reactive value for language changes to propagate to modules
  # Initialize to 1 so initial render works
  session$userData$language_version <- reactiveVal(1)

  # Observe language changes to update version
  observeEvent(input$selected_language, {
    if (!is.null(input$selected_language)) {
      session$userData$language_version(session$userData$language_version() + 1)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Render lifetable module card dynamically for language switching
  output$lifetable_module_card <- renderUI({
    i18n <- usei18n_local()
    # Force reactivity on language change
    input$selected_language

    # Get the lifetable module definition with current language
    modules <- get_app_modules(i18n)
    mod <- modules$lifetable

    div(
      class = "module-card",
      div(
        class = "module-icon",
        icon(mod$icon)
      ),
      h3(mod$name, class = "module-name"),
      p(mod$description, class = "module-description"),
      actionButton("goto_lifetable", mod$button_label, class = "ui blue button")
    )
  })

  # Render heaping module card dynamically for language switching
  output$heaping_module_card <- renderUI({
    i18n <- usei18n_local()
    # Force reactivity on language change
    input$selected_language

    # Get the heaping module definition with current language
    modules <- get_app_modules(i18n)
    mod <- modules$heaping

    div(
      class = "module-card",
      div(
        class = "module-icon",
        icon(mod$icon)
      ),
      h3(mod$name, class = "module-name"),
      p(mod$description, class = "module-description"),
      actionButton("goto_heaping", mod$button_label, class = "ui blue button")
    )
  })

  output$smoothing_module_card <- renderUI({
    i18n <- usei18n_local()
    # Force reactivity on language change
    input$selected_language

    # Get the smoothing module definition with current language
    modules <- get_app_modules(i18n)
    mod <- modules$smoothing

    div(
      class = "module-card",
      div(
        class = "module-icon",
        icon(mod$icon)
      ),
      h3(mod$name, class = "module-name"),
      p(mod$description, class = "module-description"),
      actionButton("goto_smoothing", mod$button_label, class = "ui blue button")
    )
  })

  output$graduation_module_card <- renderUI({
    i18n <- usei18n_local()
    # Force reactivity on language change
    input$selected_language

    # Get the graduation module definition with current language
    modules <- get_app_modules(i18n)
    mod <- modules$graduation

    div(
      class = "module-card",
      div(
        class = "module-icon",
        icon(mod$icon)
      ),
      h3(mod$name, class = "module-name"),
      p(mod$description, class = "module-description"),
      actionButton("goto_graduation", mod$button_label, class = "ui blue button")
    )
  })

  output$odap_module_card <- renderUI({
    i18n <- usei18n_local()
    # Force reactivity on language change
    input$selected_language

    # Get the odap module definition with current language
    modules <- get_app_modules(i18n)
    mod <- modules$odap

    div(
      class = "module-card",
      div(
        class = "module-icon",
        icon(mod$icon)
      ),
      h3(mod$name, class = "module-name"),
      p(mod$description, class = "module-description"),
      actionButton("goto_odap", mod$button_label, class = "ui blue button")
    )
  })

  modules <- get_app_modules(usei18n_local())
  lapply(modules, function(mod) {
    if (!is.null(mod$server_fun)) {
      mod$server_fun(input, output, session)
    }
    invisible(NULL)
  })
}
