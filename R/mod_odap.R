#' Old-Age Population Redistribution Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
odap_module_ui <- function(i18n) {
  ns <- shiny::NS("odap")

  shinyjs::hidden(
    shiny::div(
      id = "odap_module_page",
      # CSS styling
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .odap-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }

          .odap-step {
            width: 100%;
          }
          .odap-step.main-content {
            display: block;
          }
          .odap-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .odap-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .odap-hero p {
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
            justify-content: center;
            gap: 10px;
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
            justify-content: center;
            gap: 10px;
          }
          .button-container-file .ui-ss-input {
            display: flex;
            align-items: center;
            min-width: 260px;
          }
          .button-container-file .ui.button {
            display: flex;
            align-items: center;
            justify-content: center;
            height: 38px;
          }
          .odap-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
        "))
      ),
      shiny::div(
        class = "odap-main",

        # STEP 1: Data Upload
        shiny::div(
          id = ns("data_step"),
          class = "odap-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "odap-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("odap_back_button"))
            ),
            shiny::uiOutput(ns("odap_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny::uiOutput(ns("odap_info_box_content")),
            rhandsontable::rHandsontableOutput(ns("odap_sample_table"), width = "100%", height = 210),
            shiny::uiOutput(ns("odap_info_box_instructions"))
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
            shiny::uiOutput(ns("odap_sample_button"))
          ),
          shiny::div(
            class = "odap-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("odap_continue_ui"))
          )
        ),

        # STEP 2: Analysis
        shiny::div(
          id = ns("analysis_step"),
          class = "odap-step",
          style = "display: none;",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("odap_back_to_upload_button"))
            ),
            untheme::sidebar_layout_responsive(
              # LEFT SIDEBAR: Controls
              list(
                children = shiny::div(
                  class = "ui form",
                  shiny::uiOutput(ns("odap_controls"))
                )
              ),
              # RIGHT MAIN: Plot area
              shiny::div(
                style = "flex: 1; min-width: 0;",
                shiny::uiOutput(ns("odap_group_selectors")),
                shiny::div(
                  id = ns("plot_container"),
                  style = "display: none;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns("odap_plot"), height = "600px", width = "100%")
                  )
                ),
                shiny::br(),
                shinyjs::hidden(
                  shiny::div(
                    id = ns("download_container"),
                    class = "button-container",
                    style = "justify-content: flex-end; margin-top: 1rem;",
                    shiny::uiOutput(ns("odap_download_button"))
                  )
                ),
                shiny::uiOutput(ns("download_modal_ui"))
              )
            )
          )
        )
      )
    )
}


#' Old-Age Population Redistribution Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
odap_module_server <- function(input, output, session) {
  message("[ODAP_MODULE] server initialised")

  mod_simple_module_server("odap", list(
    setup = function(input, output, session, i18n) {
      # Sample data loader
      odap_sample_loader <- function() {
        path <- system.file("extdata", "odap_sample.csv", package = "lifetableprojection")
        df <- readr::read_csv(path, show_col_types = FALSE)
        df <- df |>
          dplyr::select(Age, pop, name, country_code, sex, year)
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      # Create shared data context
      shared <- create_shared_data_context(
        "odap",
        input, output, session, i18n,
        sample_loader = odap_sample_loader,
        validation_function = validateData_opag
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      # Language-reactive UI outputs (Step 1)
      output$odap_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("odap_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$odap_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Old-Age Population Redistribution")),
          shiny::p(i18n$t("Apply old-age population redistribution based on stable population patterns fitted to observed data."))
        )
      })

      output$odap_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$odap_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::p(i18n$t("Minimum required: Age and pop (population counts). Optional: name, sex, year, country_code, nLx (mortality data).")),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("The ODAP sample contains Age and population counts for India (1971, Males)."))
        )
      })

      output$odap_sample_button <- shiny::renderUI({
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
      sample_preview <- odap_sample_loader()
      output$odap_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$odap_continue_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        req(shared$group_selection_passed())

        details <- tryCatch(shared$validation_details(), error = function(e) NULL)

        if (!is.null(details) && is.data.frame(details)) {
          if (!all(details$pass == "Pass")) {
            return(NULL)
          }
        }

        shiny::actionButton(
          ns("go_to_analysis"),
          i18n$t("Continue"),
          class = "ui blue button"
        )
      })

      # Language-reactive UI outputs (Step 2)
      output$odap_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "odap_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$odap_controls <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny.semantic::selectInput(
            ns("method"),
            i18n$t("Redistribution method"),
            choices = c("mono", "pclm", "uniform"),
            selected = if (!is.null(input$method)) input$method else "mono"
          ),
          shiny::numericInput(
            ns("redistribute_from"),
            i18n$t("Redistribute from age"),
            value = 80,
            min = 50,
            max = 100,
            step = 5
          ),
          shiny::numericInput(
            ns("oanew"),
            i18n$t("New open age group"),
            value = 100,
            min = 80,
            max = 110,
            step = 5
          ),
          shiny::numericInput(
            ns("age_fit_start"),
            i18n$t("Fitting age range start"),
            value = 60,
            min = 40,
            max = 80,
            step = 5
          ),
          shiny::numericInput(
            ns("age_fit_end"),
            i18n$t("Fitting age range end"),
            value = 70,
            min = 50,
            max = 90,
            step = 5
          ),
          shiny::br(),
          shiny::actionButton(
            ns("run_analysis"),
            i18n$t("Run Redistribution"),
            class = "ui primary button"
          )
        )
      })
      outputOptions(output, "odap_controls", suspendWhenHidden = FALSE)

      # Group selectors
      output$odap_group_selectors <- shiny::renderUI({
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
      outputOptions(output, "odap_group_selectors", suspendWhenHidden = FALSE)

      # Download button
      output$odap_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        df <- shared$data()
        is_grouped <- !is.null(df) && ".id" %in% names(df) && length(unique(df$.id)) > 1

        if (is_grouped) {
          shiny::actionButton(
            ns("open_download_modal"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        } else {
          shiny::downloadButton(
            ns("download_odap_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "odap_download_button", suspendWhenHidden = FALSE)

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

                $('input[name=\"download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"download_scope_radio\"]:checked').val();
                  $('#%s').val(selectedValue).trigger('change');
                  console.log('[MODAL] Radio changed to:', selectedValue);

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

      # Observer to capture radio button selection
      shiny::observeEvent(input$download_scope_js, {
        choice <- input$download_scope_js
        message(sprintf("[ODAP_MODAL] download_scope_js changed to: %s", choice))
        download_scope_choice(choice)
      })

      # Debug observer
      shiny::observe({
        scope <- input$download_scope
        message(sprintf("[ODAP_MODAL] download_scope input changed to: %s", scope))
      })

      # Helper function to download single group
      download_single_group <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest)

        df <- shared$data()
        shiny::req(df)

        # Create temporary directory
        temp_dir <- tempdir()
        temp_files <- c()

        # 1. Create CSV with redistributed data
        csv_file <- file.path(temp_dir, "odap_results.csv")

        # Get the .id_label for this group_id (ODAPbackend uses .id_label as key)
        group_id <- latest$group_id
        labels_df <- shared$labels_df()
        shiny::req(labels_df)

        group_label <- labels_df$.id_label[labels_df$.id == group_id]
        if (length(group_label) == 0) {
          stop("Could not find label for group_id: ", group_id)
        }

        # Get redistributed data using .id_label
        redist_data <- latest$result$data_out[[group_label]]
        shiny::req(redist_data)

        # Create output dataframe
        out_data <- data.frame(
          Age = redist_data$Age_out,
          Pop_original = df$pop[match(redist_data$Age_out, df$Age)],
          Pop_redistributed = redist_data$Pop_out,
          stringsAsFactors = FALSE
        )

        # Add grouping info if present
        if (".id" %in% names(df)) {
          out_data$.id <- latest$group_id
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }
        }

        utils::write.csv(out_data, csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # 2. Save plot as PNG using .id_label
        plot_file <- file.path(temp_dir, "odap_plot.png")
        plot_obj <- latest$result$figures[[group_label]]

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

        all_ids <- unique(df$.id)

        # Get current parameters from latest result
        params <- list(
          method = latest$method,
          redistribute_from = latest$redistribute_from,
          oanew = latest$oanew,
          age_fit = latest$age_fit
        )

        # Run ODAP in parallel for all groups
        message(sprintf("[ODAP_DOWNLOAD] Processing %d groups in parallel...", length(all_ids)))

        if (requireNamespace("future.apply", quietly = TRUE)) {
          results <- future.apply::future_lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            tryCatch({
              result <- ODAPbackend::odap_opag(
                data_in = data_subset,
                Age_fit = params$age_fit,
                AgeInt_fit = c(params$age_fit[2] - params$age_fit[1], params$age_fit[2] - params$age_fit[1]),
                Redistribute_from = params$redistribute_from,
                OAnew = params$oanew,
                method = params$method,
                nLx = NULL,
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
              result <- ODAPbackend::odap_opag(
                data_in = data_subset,
                Age_fit = params$age_fit,
                AgeInt_fit = c(params$age_fit[2] - params$age_fit[1], params$age_fit[2] - params$age_fit[1]),
                Redistribute_from = params$redistribute_from,
                OAnew = params$oanew,
                method = params$method,
                nLx = NULL,
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

        # Get labels for all groups
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)

        # Combine all data_out into one CSV
        all_redist_data <- list()
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$data_out)) {
            # Get the .id_label for this group_id
            group_label <- NULL
            if (!is.null(lbl_df)) {
              matching_label <- lbl_df[lbl_df$.id == res$group_id, ".id_label", drop = TRUE]
              if (length(matching_label) > 0) {
                group_label <- matching_label[1]
              }
            }

            if (is.null(group_label)) {
              group_label <- as.character(res$group_id)
            }

            # Access data_out using .id_label
            redist <- res$result$data_out[[group_label]]
            if (!is.null(redist)) {
              redist_df <- data.frame(
                .id = res$group_id,
                Age = redist$Age_out,
                Pop_redistributed = redist$Pop_out,
                stringsAsFactors = FALSE
              )
              all_redist_data[[length(all_redist_data) + 1]] <- redist_df
            }
          }
        }

        if (length(all_redist_data) > 0) {
          combined_redist <- dplyr::bind_rows(all_redist_data)

          # Merge with original data
          original_subset <- df[, c(".id", "Age", "pop"), drop = FALSE]
          names(original_subset)[names(original_subset) == "pop"] <- "Pop_original"

          out_data <- merge(original_subset, combined_redist, by = c(".id", "Age"), all = TRUE)

          # Add labels
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }

          # Write CSV
          csv_file <- file.path(temp_dir, "odap_results_all_groups.csv")
          utils::write.csv(out_data, csv_file, row.names = FALSE)
          temp_files <- c(temp_files, csv_file)
        }

        # Save all plots with group labels (using .id_label to access figures)
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$figures)) {
            # Get the .id_label for this group_id
            group_label <- NULL
            if (!is.null(lbl_df)) {
              matching_label <- lbl_df[lbl_df$.id == res$group_id, ".id_label", drop = TRUE]
              if (length(matching_label) > 0) {
                group_label <- matching_label[1]
              }
            }

            if (is.null(group_label)) {
              group_label <- as.character(res$group_id)
            }

            # Access figures using .id_label
            plot_obj <- res$result$figures[[group_label]]

            if (!is.null(plot_obj)) {
              # Sanitize label for filename
              safe_label <- gsub("[^a-zA-Z0-9_-]", "_", group_label)

              plot_file <- file.path(temp_dir, sprintf("odap_plot_%s.png", safe_label))
              ggplot2::ggsave(
                plot_file,
                plot = plot_obj,
                width = 10,
                height = 6,
                dpi = 300
              )
              temp_files <- c(temp_files, plot_file)
            }
          }
        }

        # Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)

        # Cleanup
        unlink(temp_dir, recursive = TRUE)
      }

      # Download handler for single group
      output$download_odap_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("odap_results_%s.zip", timestamp)
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      # Download handler for modal confirmation
      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          scope <- download_scope_choice()
          message(sprintf("[ODAP_DOWNLOAD] filename: scope = %s", scope))
          if (scope == "all") {
            sprintf("odap_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("odap_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          message(sprintf("[ODAP_DOWNLOAD] content: scope = %s", scope))
          modal_id <- ns("download_modal")
          shiny.semantic::hide_modal(modal_id, session = session)

          if (scope == "all") {
            message("[ODAP_DOWNLOAD] Executing download_all_groups")
            download_all_groups(file)
          } else {
            message("[ODAP_DOWNLOAD] Executing download_single_group")
            download_single_group(file)
          }
        },
        contentType = "application/zip"
      )

      # State management
      session$onFlushed(function() {
        shinyjs::hide(id = analysis_step_id)
        shinyjs::hide(id = ns("download_container"))
      }, once = TRUE)

      # Data reset observers
      shiny::observeEvent(shared$data(), {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shared$last_result(NULL)
        output$odap_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = ns("download_container"))
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        if (origin %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shared$last_result(NULL)
          output$odap_plot <- plotly::renderPlotly(NULL)
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
        method = input$method %||% "mono",
        redistribute_from = input$redistribute_from %||% 80,
        oanew = input$oanew %||% 100,
        age_fit_start = input$age_fit_start %||% 60,
        age_fit_end = input$age_fit_end %||% 70
      )
    },

    run = function(shared, params, input, i18n) {
      data_subset <- shared$filtered_data()
      shiny::req(data_subset)

      if (nrow(data_subset) == 0) {
        return(list(error = i18n$t("The selected group returned no rows to analyse.")))
      }

      if (!"pop" %in% names(data_subset)) {
        return(list(error = i18n$t("Column 'pop' is missing from the dataset.")))
      }

      if (!"Age" %in% names(data_subset)) {
        return(list(error = i18n$t("Column 'Age' is missing from the dataset.")))
      }

      # Build Age_fit parameter
      age_fit <- c(params$age_fit_start, params$age_fit_end)
      ageint_fit <- c(
        params$age_fit_end - params$age_fit_start,
        params$age_fit_end - params$age_fit_start
      )

      gid <- shared$active_group_id()

      # Return parameters and data for render callback to use
      # Heavy computation will happen in renderPlotly for spinner to show
      list(
        data_subset = data_subset,
        group_id = gid,
        method = params$method,
        redistribute_from = params$redistribute_from,
        oanew = params$oanew,
        age_fit = age_fit,
        ageint_fit = ageint_fit
      )
    },

    render = function(result, output, shared, input, i18n) {
      if (is.null(result) || !is.null(result$error)) {
        msg <- if (!is.null(result$error)) result$error else i18n$t("No redistribution results yet.")
        output$odap_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = session$ns("plot_container"))
        shinyjs::hide(id = session$ns("download_container"))
        return()
      }

      # Extract parameters from result (no computation happened yet)
      data_subset <- result$data_subset
      group_id <- result$group_id
      method <- result$method
      redistribute_from <- result$redistribute_from
      oanew <- result$oanew
      age_fit <- result$age_fit
      ageint_fit <- result$ageint_fit

      # THE KEY: Heavy computation happens INSIDE renderPlotly
      # This makes withSpinner show during the full computation!
      output$odap_plot <- plotly::renderPlotly({
        # Heavy computation happens HERE during rendering
        odap_result <- tryCatch({
          ODAPbackend::odap_opag(
            data_in = data_subset,
            Age_fit = age_fit,
            AgeInt_fit = ageint_fit,
            Redistribute_from = redistribute_from,
            OAnew = oanew,
            method = method,
            nLx = NULL,
            i18n = i18n
          )
        }, error = function(e) {
          cat(sprintf("[ODAP_RENDER] Backend error: %s\n", e$message))
          list(error = paste0(i18n$t("Redistribution failed: "), e$message))
        })

        if (!is.null(odap_result$error)) {
          return(NULL)
        }

        # Save result for download handlers
        result_with_computation <- list(
          result = odap_result,
          group_id = group_id,
          method = method,
          redistribute_from = redistribute_from,
          oanew = oanew,
          age_fit = age_fit
        )
        shared$last_result(result_with_computation)

        # Extract plot using .id_label
        labels_df <- shared$labels_df()
        if (is.null(labels_df) || nrow(labels_df) == 0) {
          return(NULL)
        }

        group_label <- labels_df$.id_label[labels_df$.id == group_id]
        if (length(group_label) == 0 || is.na(group_label)) {
          return(NULL)
        }

        plot_obj <- odap_result$figures[[group_label]]
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
