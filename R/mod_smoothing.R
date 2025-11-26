#' Smoothing Preview Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
smoothing_module_ui <- function(i18n) {
  ns <- shiny::NS("smoothing")

  shinyjs::hidden(
    shiny::div(
      id = "smoothing_module_page",
      # EXACT CSS from heaping (DRY)
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .smoothing-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }

          .smoothing-step {
            width: 100%;
          }
          .smoothing-step.main-content {
            display: block;
          }
          .smoothing-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .smoothing-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .smoothing-hero p {
            margin: 0;
            color: #425466;
            max-width: 100%;
          }
          .info-box {
            border: 1px solid #ddd;
            border-radius: 12px;
            padding: 24px;
            margin: 0 0 24px;
            text-align: left;
            color: #1b1c1d;
            background: #ffffff;
            box-shadow: 0 14px 32px rgba(15, 23, 42, 0.08);
          }
          .info-box h1 {
            margin-top: 0;
            color: #1b6ec2;
            font-size: 1.65rem;
          }
          .info-box h3 {
            margin-top: 1rem;
          }
          .info-box p {
            font-size: 1rem;
            color: #4a5568;
          }
          .validation-results {
            text-align: center;
            font-size: 15px;
            color: #555;
            font-family: 'Arial', sans-serif;
            font-weight: bold;
            margin-top: 1rem;
          }
          .button-container,
          .button-container-file {
            display: flex;
            justify-content: center; /* Aligns children (buttons) in the center horizontally */
            gap: 10px; /* Space between buttons */
          }
          .button-container {
            margin-top: 1.5rem;
          }
          .button-container-file .ui.button,
          .button-container .ui.button {
            min-width: 180px;
            font-weight: 600;
          }
          .button-container-file {
            display: flex;
            justify-content: center; /* Aligns children (buttons) in the center horizontally */
            gap: 10px; /* Space between buttons */
          }
          .button-container-file .ui-ss-input {
            display: flex;
            align-items: center;
            min-width: 260px;
          }
          .button-container-file .ui.button {
            display: flex;
            align-items: center;
            justify-content: center; /* Aligns children (buttons) in the center horizontally */
            height: 38px;
          }
          .smoothing-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
        "))
      ),
      shiny::tags$script(shiny::HTML("
        $(document).ready(function() {
          // Initialize Semantic UI tooltips
          $('[data-tooltip]').popup({
            hoverable: true,
            delay: { show: 300, hide: 100 }
          });
        });
      ")),
      shiny::div(
        class = "smoothing-main",

        # STEP 1: Data Upload (EXACT copy from heaping)
        shiny::div(
          id = ns("data_step"),
          class = "smoothing-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "smoothing-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("smoothing_back_button"))
            ),
            shiny::uiOutput(ns("smoothing_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = i18n$t("Upload Instructions"),
                  content = shiny::div(
                    shiny::uiOutput(ns("smoothing_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("smoothing_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("smoothing_info_box_instructions"))
                  )
                ),
                list(
                  menu = i18n$t("Method Documentation"),
                  content = shiny::div(
                    shiny::uiOutput(ns("smoothing_method_docs"))
                  )
                )
              )
            )
          ),
          shiny::tags$script(shiny::HTML("
            $(document).ready(function() {
              $('div.ui.left.action.input.ui-ss-input').css('display', 'flex');
            });
          ")),
          shiny::div(
            class = "button-container-file",
            style = "display: flex; gap: 10px; padding: 0 1rem;",
            shiny::div(
              id = ns("file_input"),
              shiny.semantic::file_input(
                ns("file1"),
                label = "",
                type = "flex-override"
              )
            ),
            shiny::uiOutput(ns("modal_ui")),
            shiny::uiOutput(ns("smoothing_sample_button"))
          ),
          shiny::div(
            class = "smoothing-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("smoothing_continue_ui"))
          )
        ),

        # STEP 2: Analysis (lifetable sidebar layout style)
        # Start hidden via CSS, will be shown by JavaScript
        shiny::div(
          id = ns("analysis_step"),
          class = "smoothing-step",
          style = "display: none;",  # Initially hidden
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("smoothing_back_to_upload_button"))
            ),
            untheme::sidebar_layout_responsive(
              # LEFT SIDEBAR: Controls
              list(
                children = shiny::div(
                  class = "ui form",
                  shiny::uiOutput(ns("smoothing_controls"))
                )
              ),
              # RIGHT MAIN: Plot area
              shiny::div(
                style = "flex: 1; min-width: 0;",  # Allow flex grow and prevent overflow
                shiny::uiOutput(ns("smoothing_group_selectors")),
                shiny::div(
                  id = ns("plot_container"),
                  style = "display: none;",  # Hidden until results available
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns("smoothing_plot"), height = "600px", width = "100%")
                  )
                ),
                shiny::br(),
                shinyjs::hidden(
                  shiny::div(
                    id = ns("download_container"),
                    class = "button-container",
                    style = "justify-content: flex-end; margin-top: 1rem;",
                    shiny::uiOutput(ns("smoothing_download_button"))
                  )
                ),
                shiny::uiOutput(ns("download_modal_ui"))
              )
            )
          )  # Close analysis_step div
        )  # Close smoothing-main div
      )  # Close smoothing_module_page div
    )  # Close hidden() wrapper
}


#' Smoothing Preview Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
smoothing_module_server <- function(input, output, session) {
  message("[SMOOTHING_MODULE] server initialised")

  mod_simple_module_server("smoothing", list(
    setup = function(input, output, session, i18n) {
      # Sample data loader - minimum data: Age + Deaths (or Exposures)
      smoothing_sample_loader <- function() {
        path <- system.file("extdata", "dat_heap_smooth.csv.gz", package = "ODAPbackend")
        df <- readr::read_csv(path, show_col_types = FALSE)
        # Show both Deaths and Exposures as examples of variables that can be smoothed
        df <- df |>
          dplyr::select(Age, Deaths, Exposures)
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      # Create shared data context (reuse from heaping)
      shared <- create_shared_data_context(
        "smoothing",
        input, output, session, i18n,
        sample_loader = smoothing_sample_loader
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      # Language-reactive UI outputs (Step 1)
      output$smoothing_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("smoothing_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$smoothing_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Smoothing Preview")),
          shiny::p(i18n$t("Preview mortality smoothing methods before integrating them into your workflow."))
        )
      })

      output$smoothing_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$smoothing_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::p(shiny::strong(i18n$t("Data requirements:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Required: Age column (must be single-year ages: 0, 1, 2, 3, ... not 5-year or abridged groups)")),
            shiny::tags$li(i18n$t("Required: At least one numeric column (Deaths, Exposures, or any other metric)")),
            shiny::tags$li(i18n$t("Data can be raw counts or proportions/rates"))
          ),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("The smoothing sample contains Age, Deaths, and Exposures observed at single ages."))
        )
      })

      output$smoothing_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h3(i18n$t("Smoothing Methods")),
          shiny::h4(i18n$t("Fine Methods (splitting to single ages):")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "sprague"),
              shiny::div(class = "description", i18n$t("Sprague 4th difference formula - standard demographic method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "beers(ord)"),
              shiny::div(class = "description", i18n$t("Beers ordinary interpolation - less constrained"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "beers(mod)"),
              shiny::div(class = "description", i18n$t("Beers modified interpolation - additional smoothing constraints"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "grabill"),
              shiny::div(class = "description", i18n$t("Grabill method - designed for population data"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "pclm"),
              shiny::div(class = "description", i18n$t("Penalized Composite Link Model - modern spline-based approach"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "mono"),
              shiny::div(class = "description", i18n$t("Monotonic graduation - prevents oscillations/negative values"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "uniform"),
              shiny::div(class = "description", i18n$t("Uniform distribution within age groups - simplest method"))
            ))
          ),
          shiny::h4(i18n$t("Rough Methods (smoothing 5-year groups):")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Carrier-Farrag"),
              shiny::div(class = "description", i18n$t("Ratio-based smoothing method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "KKN"),
              shiny::div(class = "description", i18n$t("Karup-King-Newton method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Arriaga"),
              shiny::div(class = "description", i18n$t("Arriaga redistribution method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "United Nations"),
              shiny::div(class = "description", i18n$t("UN standard smoothing"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Strong"),
              shiny::div(class = "description", i18n$t("Aggressive smoothing method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Zigzag"),
              shiny::div(class = "description", i18n$t("Corrects oscillation patterns"))
            ))
          )
        )
      })

      output$smoothing_sample_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny.semantic::action_button(
          ns("use_sample_data"),
          i18n$t("Use sample data"),
          class = "ui blue button"
        )
      })

      # Sample data preview table
      sample_preview <- smoothing_sample_loader()
      output$smoothing_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$smoothing_continue_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        # Require group selection to be complete
        req(shared$group_selection_passed())

        # Try to get validation details
        details <- tryCatch(shared$validation_details(), error = function(e) NULL)

        # If we have details, check if they pass
        if (!is.null(details) && is.data.frame(details)) {
          if (!all(details$pass == "Pass")) {
            return(NULL)  # Validation failed, don't show button
          }
        }

        # Show button if: validation passed OR no validation details available (assume OK)
        shiny::actionButton(
          ns("go_to_analysis"),
          i18n$t("Continue"),
          class = "ui blue button"
        )
      })

      # Language-reactive UI outputs (Step 2)
      output$smoothing_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "smoothing_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$smoothing_controls <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        # Get data from shared
        df <- shared$data()
        req(df)

        # Get numeric columns that aren't Age or grouping columns
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        exclude_cols <- c("Age", ".id", ".id_label")
        choices <- setdiff(numeric_cols, exclude_cols)

        # If no numeric columns available, show error
        if (length(choices) == 0) {
          return(shiny::div(
            class = "ui warning message",
            i18n$t("No numeric columns found in uploaded data")
          ))
        }

        shiny::tagList(
          shiny.semantic::selectInput(
            ns("smoothing_variable"),
            i18n$t("Variable to smooth"),
            choices = choices,
            selected = if (!is.null(input$smoothing_variable) && input$smoothing_variable %in% choices)
              input$smoothing_variable else choices[1]
          ),
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(
                `for` = ns("fine_method"),
                i18n$t("Fine Method")
              ),
              shiny::tags$span(
                class = "ui circular label",
                style = "cursor: help; font-size: 0.8em; padding: 0.3em 0.5em;",
                `data-tooltip` = i18n$t("Method for splitting grouped ages to single ages. 'sprague' - standard formula, 'beers(ord)' - ordinary Beers, 'beers(mod)' - modified Beers, 'pclm' - modern spline method, 'mono' - monotonic (no negatives), 'uniform' - equal distribution."),
                `data-position` = "right center",
                `data-variation` = "wide",
                "?"
              )
            ),
            shiny.semantic::selectInput(
              ns("fine_method"),
              NULL,  # No label here since we have it above
              choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
              selected = if (!is.null(input$fine_method)) input$fine_method else "sprague"
            )
          ),
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(
                `for` = ns("rough_method"),
                i18n$t("Rough Method")
              ),
              shiny::tags$span(
                class = "ui circular label",
                style = "cursor: help; font-size: 0.8em; padding: 0.3em 0.5em;",
                `data-tooltip` = i18n$t("Method for smoothing 5-year age groups. 'Carrier-Farrag' - ratio method, 'KKN' - Karup-King-Newton, 'Arriaga' - redistribution, 'United Nations' - UN method, 'Strong' - aggressive smoothing, 'Zigzag' - oscillation correction."),
                `data-position` = "right center",
                `data-variation` = "wide",
                "?"
              )
            ),
            shiny.semantic::selectInput(
              ns("rough_method"),
              NULL,  # No label here since we have it above
              choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
              selected = if (!is.null(input$rough_method)) input$rough_method else "none"
            )
          ),
          shiny.semantic::checkbox_input(
            ns("constrain_infants"),
            i18n$t("Constrain Infants"),
            is_marked = TRUE
          ),
          shiny::numericInput(
            ns("u5m"),
            i18n$t("Under-5 Mortality (optional)"),
            value = NULL
          ),
          shiny.semantic::selectInput(
            ns("age_out"),
            i18n$t("Output Age Classes"),
            choices = c("single", "abridged", "5-year"),
            selected = if (!is.null(input$age_out)) input$age_out else "single"
          ),
          shiny::br(),
          shiny::actionButton(
            ns("run_analysis"),  # Use ns() - heaping does this
            i18n$t("Run Smoothing"),
            class = "ui primary button"
          )
        )
      })
      outputOptions(output, "smoothing_controls", suspendWhenHidden = FALSE)

      # Group selectors (matching lifetable pattern)
      output$smoothing_group_selectors <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!isTRUE(shared$group_selection_passed())) {
          return(NULL)
        }
        dropdowns <- shared$grouping_dropdowns()
        if (!is.null(dropdowns) && length(dropdowns) > 0) {
          shiny::div(
            class = "ui form",
            style = "display: flex; flex-wrap: wrap; gap: 1rem; margin-bottom: 1rem;",
            lapply(dropdowns, function(dropdown) {
              shiny::div(style = "min-width: 150px", dropdown)
            })
          )
        } else {
          NULL
        }
      })
      outputOptions(output, "smoothing_group_selectors", suspendWhenHidden = FALSE)

      # Download button - triggers modal if grouped data
      output$smoothing_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        df <- shared$data()
        is_grouped <- !is.null(df) && ".id" %in% names(df) && length(unique(df$.id)) > 1

        if (is_grouped) {
          # Show button that opens modal
          shiny::actionButton(
            ns("open_download_modal"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        } else {
          # Direct download button for single group
          shiny::downloadButton(
            ns("download_smoothing_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "smoothing_download_button", suspendWhenHidden = FALSE)

      # Download options modal
      output$download_modal_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        modal_id <- ns("download_modal")

        shiny.semantic::modal(
          id = modal_id,
          header = i18n$t("Download Options"),
          content = shiny::div(
            style = "padding: 1rem;",
            shiny::p(i18n$t("Your data contains multiple groups. How would you like to download?")),
            shiny::br(),
            shiny::div(
              class = "ui form",
              shiny::div(
                class = "grouped fields",
                shiny::div(
                  class = "field",
                  shiny::div(
                    class = "ui radio checkbox",
                    id = "radio_single",
                    shiny::tags$input(
                      type = "radio",
                      name = "download_scope_radio",
                      value = "single",
                      checked = "checked"
                    ),
                    shiny::tags$label(i18n$t("Current group only"))
                  )
                ),
                shiny::div(
                  class = "field",
                  shiny::div(
                    class = "ui radio checkbox",
                    id = "radio_all",
                    shiny::tags$input(
                      type = "radio",
                      name = "download_scope_radio",
                      value = "all"
                    ),
                    shiny::tags$label(i18n$t("All groups (parallel processing)"))
                  )
                )
              ),
              # Hidden input that Shiny can read
              shiny::tags$input(
                id = ns("download_scope"),
                type = "hidden",
                value = "single"
              )
            ),
            shiny::br(),
            shiny::p(
              style = "font-size: 0.9em; color: #666;",
              i18n$t("Note: Downloading all groups will process each group in parallel and create a combined ZIP file with all results.")
            ),
            shiny::tags$script(shiny::HTML(sprintf("
              $(document).ready(function() {
                $('.ui.radio.checkbox').checkbox();

                // Update hidden input and send to Shiny when radio changes
                $('input[name=\"download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"download_scope_radio\"]:checked').val();
                  $('#%s').val(selectedValue).trigger('change');
                  console.log('[MODAL] Radio changed to:', selectedValue);

                  // Send custom message to Shiny
                  Shiny.setInputValue('%s', selectedValue, {priority: 'event'});
                });
              });
            ", ns("download_scope"), ns("download_scope_js"))))
          ),
          footer = shiny::tagList(
            shiny::actionButton(ns("cancel_download"), i18n$t("Cancel"), class = "ui button"),
            shiny::downloadButton(ns("confirm_download"), i18n$t("Download"), class = "ui primary button")
          )
        )
      })
      outputOptions(output, "download_modal_ui", suspendWhenHidden = FALSE)

      # Open modal observer
      shiny::observeEvent(input$open_download_modal, {
        modal_id <- ns("download_modal")
        shiny.semantic::show_modal(modal_id, session = session)
      })

      # Cancel modal observer
      shiny::observeEvent(input$cancel_download, {
        modal_id <- ns("download_modal")
        shiny.semantic::hide_modal(modal_id, session = session)
      })

      # Observer to capture radio button selection via JavaScript
      shiny::observeEvent(input$download_scope_js, {
        choice <- input$download_scope_js
        message(sprintf("[SMOOTHING_MODAL] download_scope_js changed to: %s", choice))
        download_scope_choice(choice)
      })

      # Debug observer to monitor download_scope changes
      shiny::observe({
        scope <- input$download_scope
        message(sprintf("[SMOOTHING_MODAL] download_scope input changed to: %s", scope))
      })

      # Helper function to download single group
      download_single_group <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest)

        df <- shared$data()
        shiny::req(df)
        variable <- latest$variable %||% "Deaths"

        # Create temporary directory
        temp_dir <- tempdir()
        temp_files <- c()

        # 1. Create CSV with original + smoothed data
        csv_file <- file.path(temp_dir, "smoothing_results.csv")

        # Get smoothed data from the result
        smoothed_data <- latest$result$data_out
        shiny::req(smoothed_data)

        # Rename smoothed variable to distinguish from original
        smoothed_col_name <- paste0(variable, "_smoothed")
        names(smoothed_data)[names(smoothed_data) == variable] <- smoothed_col_name

        # Prepare original data for merging
        original_data <- df
        if (".id" %in% names(original_data)) {
          # Keep original columns including grouping info
          merge_cols <- c(".id", "Age", variable)
          if ("Exposures" %in% names(original_data) && variable != "Exposures") {
            merge_cols <- c(merge_cols, "Exposures")
          }
          if ("Deaths" %in% names(original_data) && variable != "Deaths") {
            merge_cols <- c(merge_cols, "Deaths")
          }
          original_subset <- original_data[, intersect(merge_cols, names(original_data)), drop = FALSE]

          # Filter to current group only
          gid <- shared$active_group_id()
          original_subset <- original_subset[original_subset$.id == gid, , drop = FALSE]

          # Merge original and smoothed
          out_data <- merge(original_subset, smoothed_data, by = c(".id", "Age"), all = TRUE)

          # Add labels
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }
        } else {
          # No grouping - simple merge
          original_subset <- df[, c("Age", variable), drop = FALSE]
          out_data <- merge(original_subset, smoothed_data, by = "Age", all = TRUE)
        }

        # Reorder columns
        col_order <- c()
        if (".id" %in% names(out_data)) col_order <- c(col_order, ".id")
        if (".id_label" %in% names(out_data)) col_order <- c(col_order, ".id_label")
        col_order <- c(col_order, "Age")
        if (variable %in% names(out_data)) col_order <- c(col_order, variable)
        if (smoothed_col_name %in% names(out_data)) col_order <- c(col_order, smoothed_col_name)
        remaining_cols <- setdiff(names(out_data), col_order)
        out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

        utils::write.csv(out_data, csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # 2. Save plot as PNG
        plot_file <- file.path(temp_dir, "smoothing_plot.png")
        group_id_str <- as.character(latest$group_id)
        plot_data <- latest$result$figures[[group_id_str]]

        if (!is.null(plot_data) && !is.null(plot_data$figure)) {
          ggplot2::ggsave(
            plot_file,
            plot = plot_data$figure,
            width = 10,
            height = 6,
            dpi = 300
          )
          temp_files <- c(temp_files, plot_file)
        }

        # 3. Create analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        method_type <- if (latest$age_out == "single") "Fine" else "Rough"
        method_used <- if (latest$age_out == "single") latest$fine_method else latest$rough_method

        info_text <- paste(
          "Module: Smoothing Analysis",
          "Description: This module performs age smoothing and graduation on demographic data.",
          "",
          sprintf("Method Type: %s", method_type),
          sprintf("Method Used: %s", method_used),
          "",
          "Method Descriptions:",
          if (method_type == "Fine") {
            paste(
              "- sprague: Sprague 4th difference formula - standard demographic method",
              "- beers(ord): Beers ordinary interpolation - less constrained",
              "- beers(mod): Beers modified interpolation - additional smoothing constraints",
              "- grabill: Grabill method - designed for population data",
              "- pclm: Penalized Composite Link Model - modern spline-based approach",
              "- mono: Monotonic graduation - prevents oscillations/negative values",
              "- uniform: Uniform distribution within age groups - simplest method",
              sep = "\n"
            )
          } else {
            paste(
              "- Carrier-Farrag: Ratio-based smoothing method",
              "- KKN: Karup-King-Newton method",
              "- Arriaga: Arriaga redistribution method",
              "- United Nations: UN standard smoothing",
              "- Strong: Aggressive smoothing method",
              "- Zigzag: Corrects oscillation patterns",
              sep = "\n"
            )
          },
          "",
          sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("Variable Analyzed: %s", variable),
          sprintf("Age Output Type: %s", latest$age_out),
          if (!is.null(latest$u5m)) sprintf("Under-5 Mortality: %s", latest$u5m) else NULL,
          sprintf("Constrain Infants: %s", latest$constrain_infants),
          sep = "\n"
        )
        writeLines(info_text, info_file)
        temp_files <- c(temp_files, info_file)

        # 4. Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
      }

      # Helper function to download all groups
      download_all_groups <- function(file) {
        df <- shared$data()
        shiny::req(df)

        latest <- shared$last_result()
        shiny::req(latest)

        variable <- latest$variable %||% "Deaths"
        all_ids <- unique(df$.id)

        # Get current parameters from latest result
        params <- list(
          variable = latest$variable,
          fine_method = latest$fine_method,
          rough_method = latest$rough_method,
          age_out = latest$age_out,
          u5m = latest$u5m,
          constrain_infants = latest$constrain_infants
        )

        # Run smoothing in parallel for all groups
        message(sprintf("[SMOOTHING_DOWNLOAD] Processing %d groups in parallel...", length(all_ids)))

        # Use future.apply if available, otherwise lapply
        if (requireNamespace("future.apply", quietly = TRUE)) {
          results <- future.apply::future_lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            tryCatch({
              result <- ODAPbackend::smooth_flexible(
                data_subset,
                variable = params$variable,
                age_out = params$age_out,
                fine_method = params$fine_method,
                rough_method = params$rough_method,
                u5m = params$u5m,
                constrain_infants = params$constrain_infants,
                Sex = "t",
                i18n = i18n
              )
              list(group_id = gid, result = result, error = NULL)
            }, error = function(e) {
              list(group_id = gid, result = NULL, error = as.character(e))
            })
          }, future.seed = TRUE)
        } else {
          results <- lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            tryCatch({
              result <- ODAPbackend::smooth_flexible(
                data_subset,
                variable = params$variable,
                age_out = params$age_out,
                fine_method = params$fine_method,
                rough_method = params$rough_method,
                u5m = params$u5m,
                constrain_infants = params$constrain_infants,
                Sex = "t",
                i18n = i18n
              )
              list(group_id = gid, result = result, error = NULL)
            }, error = function(e) {
              list(group_id = gid, result = NULL, error = as.character(e))
            })
          })
        }

        # Create temporary directory
        temp_dir <- tempfile()
        dir.create(temp_dir)
        temp_files <- c()

        # Combine all data_out into one CSV
        all_smoothed_data <- list()
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$data_out)) {
            smoothed <- res$result$data_out
            smoothed$.id <- res$group_id
            all_smoothed_data[[length(all_smoothed_data) + 1]] <- smoothed
          }
        }

        if (length(all_smoothed_data) > 0) {
          combined_smoothed <- dplyr::bind_rows(all_smoothed_data)

          # Rename smoothed variable
          smoothed_col_name <- paste0(variable, "_smoothed")
          names(combined_smoothed)[names(combined_smoothed) == variable] <- smoothed_col_name

          # Merge with original data
          merge_cols <- c(".id", "Age", variable)
          if ("Exposures" %in% names(df) && variable != "Exposures") {
            merge_cols <- c(merge_cols, "Exposures")
          }
          if ("Deaths" %in% names(df) && variable != "Deaths") {
            merge_cols <- c(merge_cols, "Deaths")
          }
          original_subset <- df[, intersect(merge_cols, names(df)), drop = FALSE]

          out_data <- merge(original_subset, combined_smoothed, by = c(".id", "Age"), all = TRUE)

          # Add labels
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }

          # Reorder columns
          col_order <- c(".id")
          if (".id_label" %in% names(out_data)) col_order <- c(col_order, ".id_label")
          col_order <- c(col_order, "Age")
          if (variable %in% names(out_data)) col_order <- c(col_order, variable)
          if (smoothed_col_name %in% names(out_data)) col_order <- c(col_order, smoothed_col_name)
          remaining_cols <- setdiff(names(out_data), col_order)
          out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

          # Write CSV
          csv_file <- file.path(temp_dir, "smoothing_results_all_groups.csv")
          utils::write.csv(out_data, csv_file, row.names = FALSE)
          temp_files <- c(temp_files, csv_file)
        }

        # Save all plots with group labels
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$figures)) {
            gid_str <- as.character(res$group_id)
            plot_data <- res$result$figures[[gid_str]]

            if (!is.null(plot_data) && !is.null(plot_data$figure)) {
              # Get label for filename
              label <- gid_str
              if (!is.null(lbl_df)) {
                matching_label <- lbl_df[lbl_df$.id == res$group_id, ".id_label", drop = TRUE]
                if (length(matching_label) > 0) {
                  label <- matching_label[1]
                  # Sanitize label for filename
                  label <- gsub("[^a-zA-Z0-9_-]", "_", label)
                }
              }

              plot_file <- file.path(temp_dir, sprintf("smoothing_plot_%s.png", label))
              ggplot2::ggsave(
                plot_file,
                plot = plot_data$figure,
                width = 10,
                height = 6,
                dpi = 300
              )
              temp_files <- c(temp_files, plot_file)
            }
          }
        }

        # Create analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        info_text <- paste(
          "Module: Smoothing Analysis (All Groups)",
          "Description: This module performs age smoothing and graduation on demographic data.",
          "",
          sprintf("Method Type: %s", if (params$age_out == "single") "Fine" else "Rough"),
          sprintf("Method Used: %s", if (params$age_out == "single") params$fine_method else params$rough_method),
          "",
          "Method Descriptions:",
          if (params$age_out == "single") {
            paste(
              "- sprague: Sprague 4th difference formula - standard demographic method",
              "- beers(ord): Beers ordinary interpolation - less constrained",
              "- beers(mod): Beers modified interpolation - additional smoothing constraints",
              "- grabill: Grabill method - designed for population data",
              "- pclm: Penalized Composite Link Model - modern spline-based approach",
              "- mono: Monotonic graduation - prevents oscillations/negative values",
              "- uniform: Uniform distribution within age groups - simplest method",
              sep = "\n"
            )
          } else {
            paste(
              "- Carrier-Farrag: Ratio-based smoothing method",
              "- KKN: Karup-King-Newton method",
              "- Arriaga: Arriaga redistribution method",
              "- United Nations: UN standard smoothing",
              "- Strong: Aggressive smoothing method",
              "- Zigzag: Corrects oscillation patterns",
              sep = "\n"
            )
          },
          "",
          sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("Variable Analyzed: %s", params$variable),
          sprintf("Age Output Type: %s", params$age_out),
          sprintf("Number of Groups: %d", length(results)),
          if (!is.null(params$u5m)) sprintf("Under-5 Mortality: %s", params$u5m) else NULL,
          sprintf("Constrain Infants: %s", params$constrain_infants),
          sep = "\n"
        )
        writeLines(info_text, info_file)
        temp_files <- c(temp_files, info_file)

        # Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)

        # Cleanup
        unlink(temp_dir, recursive = TRUE)
      }

      # Download handler for single group (non-grouped data or direct button)
      output$download_smoothing_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("smoothing_results_%s.zip", timestamp)
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      # Download handler for modal confirmation (handles both single and all)
      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          scope <- download_scope_choice()
          message(sprintf("[SMOOTHING_DOWNLOAD] filename: scope = %s", scope))
          if (scope == "all") {
            sprintf("smoothing_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("smoothing_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          message(sprintf("[SMOOTHING_DOWNLOAD] content: scope = %s", scope))
          modal_id <- ns("download_modal")
          shiny.semantic::hide_modal(modal_id, session = session)

          if (scope == "all") {
            message("[SMOOTHING_DOWNLOAD] Executing download_all_groups")
            download_all_groups(file)
          } else {
            message("[SMOOTHING_DOWNLOAD] Executing download_single_group")
            download_single_group(file)
          }
        },
        contentType = "application/zip"
      )

      # State management: hide analysis on init
      session$onFlushed(function() {
        shinyjs::hide(id = analysis_step_id)
        shinyjs::hide(id = ns("download_container"))
      }, once = TRUE)

      # Data reset observers (from heaping)
      shiny::observeEvent(shared$data(), {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shared$last_result(NULL)
        output$smoothing_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = ns("download_container"))
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        if (origin %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shared$last_result(NULL)
          output$smoothing_plot <- plotly::renderPlotly(NULL)
          shinyjs::hide(id = ns("download_container"))
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Navigation: Step 1 → Step 2
      shiny::observeEvent(input$go_to_analysis, {
        shinyjs::hide(id = data_step_id)
        shinyjs::show(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Navigation: Step 2 → Step 1
      shiny::observeEvent(input$back_to_upload, {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))
        shinyjs::hide(id = ns("plot_container"))
        shinyjs::hide(id = ns("download_container"))
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').hide();", ns("plot_container"), ns("download_container")))
      })

      shared
    },

    prepare = function(input, shared, i18n) {
      list(
        variable = input$smoothing_variable %||% "Deaths",
        fine_method = input$fine_method %||% "sprague",
        rough_method = input$rough_method %||% "none",
        constrain_infants = input$constrain_infants %||% TRUE,
        u5m = input$u5m,
        age_out = input$age_out %||% "single"
      )
    },

    run = function(shared, params, input, i18n) {
      data_subset <- shared$filtered_data()
      shiny::req(data_subset)

      if (nrow(data_subset) == 0) {
        return(list(error = i18n$t("The selected group returned no rows to analyse.")))
      }

      if (!params$variable %in% names(data_subset)) {
        return(list(error = sprintf(i18n$t("Column '%s' is missing from the dataset."), params$variable)))
      }

      result <- tryCatch({
        ODAPbackend::smooth_flexible(
          data_subset,
          variable = params$variable,
          age_out = params$age_out,
          fine_method = params$fine_method,
          rough_method = params$rough_method,
          u5m = params$u5m,
          constrain_infants = params$constrain_infants,
          Sex = "t",
          i18n = i18n
        )
      }, error = function(e) {
        list(error = i18n$t("Smoothing failed. Check input data validity."))
      })

      if (!is.null(result$error)) {
        return(result)
      }

      gid <- shared$active_group_id()
      list(
        result = result,
        group_id = gid,
        variable = params$variable,
        fine_method = params$fine_method,
        rough_method = params$rough_method,
        age_out = params$age_out,
        u5m = params$u5m,
        constrain_infants = params$constrain_infants
      )
    },

    render = function(result, output, shared, input, i18n) {
      if (is.null(result) || !is.null(result$error)) {
        msg <- if (!is.null(result$error)) result$error else i18n$t("No smoothing results yet.")
        output$smoothing_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = session$ns("plot_container"))
        shinyjs::hide(id = session$ns("download_container"))
        return()
      }

      shared$last_result(result)

      # Extract plot for current group
      group_id_str <- as.character(result$group_id)
      plot_data <- result$result$figures[[group_id_str]]

      if (is.null(plot_data) || is.null(plot_data$figure)) {
        output$smoothing_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = session$ns("plot_container"))
        return()
      }

      # Render plot (matching lifetable pattern)
      output$smoothing_plot <- plotly::renderPlotly({
        plotly::ggplotly(plot_data$figure) |>
          plotly::config(displayModeBar = FALSE)
      })

      # Show plot and download button
      shinyjs::show(id = session$ns("plot_container"))
      shinyjs::show(id = session$ns("download_container"))
    },

    register_downloads = NULL
  ))
}
