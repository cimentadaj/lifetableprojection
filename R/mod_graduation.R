#' Graduation Preview Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
graduation_module_ui <- function(i18n) {
  ns <- shiny::NS("graduation")

  shinyjs::hidden(
    shiny::div(
      id = "graduation_module_page",
      # EXACT CSS from heaping (DRY)
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .graduation-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }

          .graduation-step {
            width: 100%;
          }
          .graduation-step.main-content {
            display: block;
          }
          .graduation-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .graduation-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .graduation-hero p {
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
          .graduation-upload-log {
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
        class = "graduation-main",

        # STEP 1: Data Upload (EXACT copy from heaping)
        shiny::div(
          id = ns("data_step"),
          class = "graduation-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "graduation-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("graduation_back_button"))
            ),
            shiny::uiOutput(ns("graduation_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = i18n$t("Upload Instructions"),
                  content = shiny::div(
                    shiny::uiOutput(ns("graduation_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("graduation_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("graduation_info_box_instructions"))
                  )
                ),
                list(
                  menu = i18n$t("Method Documentation"),
                  content = shiny::div(
                    shiny::uiOutput(ns("graduation_method_docs"))
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
            shiny::uiOutput(ns("graduation_sample_button"))
          ),
          shiny::div(
            class = "graduation-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("graduation_continue_ui"))
          )
        ),

        # STEP 2: Analysis (lifetable sidebar layout style)
        # Start hidden via CSS, will be shown by JavaScript
        shiny::div(
          id = ns("analysis_step"),
          class = "graduation-step",
          style = "display: none;",  # Initially hidden
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("graduation_back_to_upload_button"))
            ),
            untheme::sidebar_layout_responsive(
              # LEFT SIDEBAR: Controls
              list(
                children = shiny::div(
                  class = "ui form",
                  shiny::uiOutput(ns("graduation_controls"))
                )
              ),
              # RIGHT MAIN: Plot area
              shiny::div(
                style = "flex: 1; min-width: 0;",  # Allow flex grow and prevent overflow
                shiny::uiOutput(ns("graduation_group_selectors")),
                shiny::div(
                  id = ns("plot_container"),
                  style = "display: none;",  # Hidden until results available
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns("graduation_plot"), height = "600px", width = "100%")
                  )
                ),
                shiny::br(),
                shinyjs::hidden(
                  shiny::div(
                    id = ns("download_container"),
                    class = "button-container",
                    style = "justify-content: flex-end; margin-top: 1rem;",
                    shiny::uiOutput(ns("graduation_download_button"))
                  )
                ),
                shiny::uiOutput(ns("download_modal_ui"))
              )
            )
          )  # Close analysis_step div
        )  # Close graduation-main div
      )  # Close graduation_module_page div
    )  # Close hidden() wrapper
}


#' Graduation Preview Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
graduation_module_server <- function(input, output, session) {
  message("[GRADUATION_MODULE] server initialised")

  mod_simple_module_server("graduation", list(
    setup = function(input, output, session, i18n) {
      # Sample data loader - minimum data: Age + Deaths (or Exposures)
      graduation_sample_loader <- function() {
        path <- system.file("extdata", "dat_heap_smooth.csv.gz", package = "ODAPbackend")
        df <- readr::read_csv(path, show_col_types = FALSE)
        # Show both Deaths and Exposures as examples of variables that can be graduated
        df <- df |>
          dplyr::select(Age, Deaths, Exposures)
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      # Create shared data context (reuse from heaping)
      shared <- create_shared_data_context(
        "graduation",
        input, output, session, i18n,
        sample_loader = graduation_sample_loader
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      # Language-reactive UI outputs (Step 1)
      output$graduation_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("graduation_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$graduation_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Graduation Tool")),
          shiny::p(i18n$t("Fine-tune mortality graduation with configurable infant constraints."))
        )
      })

      output$graduation_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$graduation_info_box_instructions <- shiny::renderUI({
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
          shiny::p(i18n$t("The graduation sample contains Age, Deaths, and Exposures."))
        )
      })

      output$graduation_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h3(i18n$t("Graduation Methods")),
          shiny::p(i18n$t("The graduation module uses the same methods as the smoothing module to transform age data between different grouping structures.")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "sprague"),
              shiny::div(class = "description", i18n$t("Sprague 4th difference formula - standard demographic method"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "beers(ord)"),
              shiny::div(class = "description", i18n$t("Beers ordinary interpolation"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "beers(mod)"),
              shiny::div(class = "description", i18n$t("Beers modified interpolation with smoothing"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "grabill"),
              shiny::div(class = "description", i18n$t("Grabill method for population data"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "pclm"),
              shiny::div(class = "description", i18n$t("Penalized Composite Link Model - spline approach"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "mono"),
              shiny::div(class = "description", i18n$t("Monotonic graduation - prevents negative values"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "uniform"),
              shiny::div(class = "description", i18n$t("Uniform distribution within age groups"))
            ))
          ),
          shiny::p(i18n$t("Additional graduation parameters:")),
          shiny::div(
            class = "ui bulleted list",
            shiny::div(class = "item", i18n$t("Age Output: Choose between 5-year, abridged, or single age formats")),
            shiny::div(class = "item", i18n$t("Constraints: Apply specific constraints to infant age groups")),
            shiny::div(class = "item", i18n$t("Pivoting: Re-aggregate data to desired age structure"))
          )
        )
      })

      output$graduation_sample_button <- shiny::renderUI({
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
      sample_preview <- graduation_sample_loader()
      output$graduation_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$graduation_continue_ui <- shiny::renderUI({
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
      output$graduation_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "graduation_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$graduation_controls <- shiny::renderUI({
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
            ns("graduation_variable"),
            i18n$t("Variable to graduate"),
            choices = choices,
            selected = if (!is.null(input$graduation_variable) && input$graduation_variable %in% choices)
              input$graduation_variable else choices[1]
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
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(
                `for` = ns("age_out"),
                i18n$t("Output Age Classes")
              ),
              shiny::tags$span(
                class = "ui circular label",
                style = "cursor: help; font-size: 0.8em; padding: 0.3em 0.5em;",
                `data-tooltip` = i18n$t("Target age grouping for graduation. 'single' - individual ages (0,1,2...), 'abridged' - standard life table ages (0,1-4,5-9...), '5-year' - five-year groups (0-4,5-9...)."),
                `data-position` = "right center",
                `data-variation` = "wide",
                "?"
              )
            ),
            shiny.semantic::selectInput(
              ns("age_out"),
              NULL,  # No label here since we have it above
              choices = c("single", "abridged", "5-year"),
              selected = if (!is.null(input$age_out)) input$age_out else "single"
            )
          ),
          shiny::br(),
          shiny::actionButton(
            ns("run_analysis"),
            i18n$t("Run Graduation"),
            class = "ui primary button"
          )
        )
      })
      outputOptions(output, "graduation_controls", suspendWhenHidden = FALSE)

      # Group selectors (matching lifetable pattern)
      output$graduation_group_selectors <- shiny::renderUI({
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
      outputOptions(output, "graduation_group_selectors", suspendWhenHidden = FALSE)

      # Download button - triggers modal if grouped data
      output$graduation_download_button <- shiny::renderUI({
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
            ns("download_graduation_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "graduation_download_button", suspendWhenHidden = FALSE)

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
        message(sprintf("[GRADUATION_MODAL] download_scope_js changed to: %s", choice))
        download_scope_choice(choice)
      })

      # Debug observer to monitor download_scope changes
      shiny::observe({
        scope <- input$download_scope
        message(sprintf("[GRADUATION_MODAL] download_scope input changed to: %s", scope))
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

        # 1. Create CSV with original + graduated data
        csv_file <- file.path(temp_dir, "graduation_results.csv")

        # Get graduated data from the result
        graduated_data <- latest$result$data_out
        shiny::req(graduated_data)

        # Rename graduated variable to distinguish from original
        graduated_col_name <- paste0(variable, "_graduated")
        names(graduated_data)[names(graduated_data) == variable] <- graduated_col_name

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

          # Merge original and graduated
          out_data <- merge(original_subset, graduated_data, by = c(".id", "Age"), all = TRUE)

          # Add labels
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }
        } else {
          # No grouping - simple merge
          original_subset <- df[, c("Age", variable), drop = FALSE]
          out_data <- merge(original_subset, graduated_data, by = "Age", all = TRUE)
        }

        # Reorder columns
        col_order <- c()
        if (".id" %in% names(out_data)) col_order <- c(col_order, ".id")
        if (".id_label" %in% names(out_data)) col_order <- c(col_order, ".id_label")
        col_order <- c(col_order, "Age")
        if (variable %in% names(out_data)) col_order <- c(col_order, variable)
        if (graduated_col_name %in% names(out_data)) col_order <- c(col_order, graduated_col_name)
        remaining_cols <- setdiff(names(out_data), col_order)
        out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

        utils::write.csv(out_data, csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # 2. Save plot as PNG (use manually created plot from run callback)
        plot_file <- file.path(temp_dir, "graduation_plot.png")
        plot_obj <- latest$plot

        if (!is.null(plot_obj)) {
          ggplot2::ggsave(
            plot_file,
            plot = plot_obj,
            width = 10,
            height = 6,
            dpi = 300
          )
          temp_files <- c(temp_files, plot_file)
        }

        # 3. Create ZIP file
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
          age_out = latest$age_out,
          u5m = latest$u5m,
          constrain_infants = latest$constrain_infants
        )

        # Run graduation in parallel for all groups
        message(sprintf("[GRADUATION_DOWNLOAD] Processing %d groups in parallel...", length(all_ids)))

        # Use future.apply if available, otherwise lapply
        if (requireNamespace("future.apply", quietly = TRUE)) {
          results <- future.apply::future_lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            tryCatch({
              result <- ODAPbackend::graduate_auto(
                data_subset,
                variable = params$variable,
                age_out = params$age_out,
                u5m = params$u5m,
                constrain_infants = params$constrain_infants,
                Sex = "t"
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
              result <- ODAPbackend::graduate_auto(
                data_subset,
                variable = params$variable,
                age_out = params$age_out,
                u5m = params$u5m,
                constrain_infants = params$constrain_infants,
                Sex = "t"
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
        all_graduated_data <- list()
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$data_out)) {
            graduated <- res$result$data_out
            graduated$.id <- res$group_id
            all_graduated_data[[length(all_graduated_data) + 1]] <- graduated
          }
        }

        if (length(all_graduated_data) > 0) {
          combined_graduated <- dplyr::bind_rows(all_graduated_data)

          # Rename graduated variable
          graduated_col_name <- paste0(variable, "_graduated")
          names(combined_graduated)[names(combined_graduated) == variable] <- graduated_col_name

          # Merge with original data
          merge_cols <- c(".id", "Age", variable)
          if ("Exposures" %in% names(df) && variable != "Exposures") {
            merge_cols <- c(merge_cols, "Exposures")
          }
          if ("Deaths" %in% names(df) && variable != "Deaths") {
            merge_cols <- c(merge_cols, "Deaths")
          }
          original_subset <- df[, intersect(merge_cols, names(df)), drop = FALSE]

          out_data <- merge(original_subset, combined_graduated, by = c(".id", "Age"), all = TRUE)

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
          if (graduated_col_name %in% names(out_data)) col_order <- c(col_order, graduated_col_name)
          remaining_cols <- setdiff(names(out_data), col_order)
          out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

          # Write CSV
          csv_file <- file.path(temp_dir, "graduation_results_all_groups.csv")
          utils::write.csv(out_data, csv_file, row.names = FALSE)
          temp_files <- c(temp_files, csv_file)
        }

        # Create plots manually for all groups
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$data_out)) {
            gid <- res$group_id
            graduated_data <- res$result$data_out
            original_data <- df[df$.id == gid, , drop = FALSE]

            # Create plot
            # For plotting, apply same normalization as in graduation.R for visual comparison
            original_values <- original_data[[variable]]
            graduated_values <- graduated_data[[variable]]

            # Check if converting from single ages to grouped ages for plot normalization
            input_is_single <- DemoTools::is_single(original_data$Age)
            output_is_grouped <- !DemoTools::is_single(graduated_data$Age)

            if (input_is_single && output_is_grouped) {
              # For visual comparison, divide grouped values by typical interval width
              # This shows average per single age rather than sum
              age_intervals <- diff(c(graduated_data$Age, max(graduated_data$Age) + 5))
              graduated_values <- graduated_values / age_intervals
            }

            plot_data <- data.frame(
              Age = c(original_data$Age, graduated_data$Age),
              Value = c(original_values, graduated_values),
              Type = c(rep("Original", nrow(original_data)), rep("Graduated", nrow(graduated_data)))
            )

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Age, y = Value, color = Type, linetype = Type)) +
              ggplot2::geom_line(linewidth = 1) +
              ggplot2::geom_point(data = subset(plot_data, Type == "Original"), size = 2) +
              ggplot2::scale_color_manual(values = c("Original" = "black", "Graduated" = "blue")) +
              ggplot2::scale_linetype_manual(values = c("Original" = "solid", "Graduated" = "solid")) +
              ggplot2::theme_minimal() +
              ggplot2::labs(
                title = paste("Graduation:", variable),
                x = "Age",
                y = variable,
                color = "Data Type",
                linetype = "Data Type"
              )

            # Get label for filename
            label <- as.character(gid)
            if (!is.null(lbl_df)) {
              matching_label <- lbl_df[lbl_df$.id == gid, ".id_label", drop = TRUE]
              if (length(matching_label) > 0) {
                label <- matching_label[1]
                label <- gsub("[^a-zA-Z0-9_-]", "_", label)
              }
            }

            plot_file <- file.path(temp_dir, sprintf("graduation_plot_%s.png", label))
            ggplot2::ggsave(plot_file, plot = p, width = 10, height = 6, dpi = 300)
            temp_files <- c(temp_files, plot_file)
          }
        }

        # Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)

        # Cleanup
        unlink(temp_dir, recursive = TRUE)
      }

      # Download handler for single group (non-grouped data or direct button)
      output$download_graduation_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("graduation_results_%s.zip", timestamp)
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      # Download handler for modal confirmation (handles both single and all)
      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          scope <- download_scope_choice()
          message(sprintf("[GRADUATION_DOWNLOAD] filename: scope = %s", scope))
          if (scope == "all") {
            sprintf("graduation_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("graduation_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          message(sprintf("[GRADUATION_DOWNLOAD] content: scope = %s", scope))
          modal_id <- ns("download_modal")
          shiny.semantic::hide_modal(modal_id, session = session)

          if (scope == "all") {
            message("[GRADUATION_DOWNLOAD] Executing download_all_groups")
            download_all_groups(file)
          } else {
            message("[GRADUATION_DOWNLOAD] Executing download_single_group")
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
        output$graduation_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = ns("download_container"))
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        if (origin %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shared$last_result(NULL)
          output$graduation_plot <- plotly::renderPlotly(NULL)
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
        variable = input$graduation_variable %||% "Deaths",
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

      gid <- shared$active_group_id()

      # Return parameters and data for render callback to use
      # Heavy computation will happen in renderPlotly for spinner to show
      list(
        data_subset = data_subset,
        group_id = gid,
        variable = params$variable,
        age_out = params$age_out,
        u5m = params$u5m,
        constrain_infants = params$constrain_infants
      )
    },

    render = function(result, output, shared, input, i18n) {
      if (is.null(result) || !is.null(result$error)) {
        msg <- if (!is.null(result$error)) result$error else i18n$t("No graduation results yet.")
        output$graduation_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = session$ns("plot_container"))
        shinyjs::hide(id = session$ns("download_container"))
        return()
      }

      # Extract parameters from result (no computation happened yet)
      data_subset <- result$data_subset
      group_id <- result$group_id
      variable <- result$variable
      age_out <- result$age_out
      u5m <- result$u5m
      constrain_infants <- result$constrain_infants

      # THE KEY: Heavy computation happens INSIDE renderPlotly
      # This makes withSpinner show during the full computation!
      output$graduation_plot <- plotly::renderPlotly({
        # Heavy computation happens HERE during rendering
        grad_result <- tryCatch({
          ODAPbackend::graduate_auto(
            data_subset,
            variable = variable,
            age_out = age_out,
            u5m = u5m,
            constrain_infants = constrain_infants,
            Sex = "t",
            i18n = i18n
          )
        }, error = function(e) {
          cat(sprintf("[GRADUATION_RENDER] Backend error: %s\n", e$message))
          list(error = paste0(i18n$t("Graduation failed: "), e$message))
        })

        if (!is.null(grad_result$error)) {
          # Create error visualization plot
          error_msg <- grad_result$error
          error_plot <- plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                                         marker = list(size = 0, opacity = 0)) |>
            plotly::add_annotations(
              text = paste0("<b>", i18n$t("Error"), ":</b><br><br>", error_msg),
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 16, color = "red"),
              xanchor = "center",
              yanchor = "middle"
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
              plot_bgcolor = "#f8f9fa",
              paper_bgcolor = "#f8f9fa",
              showlegend = FALSE
            ) |>
            plotly::config(displayModeBar = FALSE)
          return(error_plot)
        }

        # Add .id back to graduated data (graduate_auto doesn't preserve it)
        if (!is.null(grad_result$data_out)) {
          grad_result$data_out$.id <- group_id
        }

        # Save result for download handlers
        result_with_computation <- list(
          result = grad_result,
          group_id = group_id,
          variable = variable,
          age_out = age_out,
          u5m = u5m,
          constrain_infants = constrain_infants
        )
        shared$last_result(result_with_computation)

        # Extract and render plot
        plot_obj <- grad_result$plot
        if (is.null(plot_obj)) {
          return(NULL)
        }

        plotly::ggplotly(plot_obj) |>
          plotly::config(displayModeBar = FALSE)
      })

      # Show plot and download button
      shinyjs::show(id = session$ns("plot_container"))
      shinyjs::show(id = session$ns("download_container"))
    },

    register_downloads = NULL
  ))
}
