#' Heaping Diagnostics Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
heaping_module_ui <- function(i18n) {
  ns <- shiny::NS("heaping")

  shinyjs::hidden(
    shiny::div(
      id = "heaping_module_page",
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .heaping-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }
          
          .heaping-step {
            width: 100%;
          }
          .heaping-step.main-content {
            display: block;
          }
          .heaping-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .heaping-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .heaping-hero p {
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
          .heaping-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
          .heaping-controls-block {
            margin-bottom: 1.5rem;
          }
          .heaping-controls-header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 0.75rem;
          }
          .heaping-controls-header h3 {
            margin: 0;
            font-size: 1.25rem;
            color: #1b1c1d;
          }
          .heaping-controls-row {
            display: flex;
            flex-wrap: wrap;
            gap: 1.5rem;
            align-items: flex-end;
          }
          .heaping-controls-row .heaping-control-form {
            flex: 0 0 220px;
            min-width: 200px;
          }
          .heaping-controls-row .grouping-control-panel {
            flex: 1 1 calc(100% - 240px);
            min-width: 240px;
          }
          .heaping-controls-row .grouping-control-panel .simple-module-grouping {
            width: 100%;
          }
          .heaping-control-form .field {
            margin-bottom: 1rem;
          }
          .heaping-run-panel {
            display: flex;
            flex-wrap: wrap;
            align-items: center;
            gap: 1rem;
            margin-bottom: 1.5rem;
            padding: 1.25rem 1.5rem;
            border-radius: 12px;
            background: #f8fbff;
            border: 1px solid rgba(27,110,194,0.2);
          }
          .heaping-run-panel .ui.button {
            min-width: 220px;
            font-size: 1.05rem;
            padding: 0.85rem 1.75rem;
          }
          .heaping-run-log {
            flex: 1 1 auto;
            color: #425466;
          }
          .heaping-run-log.error {
            color: #c0392b;
            font-weight: 600;
          }
          .heaping-results {
            margin-bottom: 1.5rem;
          }
          .heaping-download {
            justify-content: flex-end;
          }
          @media (max-width: 768px) {
            .heaping-run-panel {
              flex-direction: column;
              align-items: stretch;
            }
            .heaping-run-panel .ui.button {
              width: 100%;
            }
            .heaping-controls-row {
              flex-direction: column;
            }
          }
        "))
      ),
      shiny::div(
        class = "heaping-main",
        shiny::div(
          id = ns("data_step"),
          class = "heaping-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "heaping-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("heaping_back_button"))
            ),
            shiny::uiOutput(ns("heaping_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = i18n$t("Upload Instructions"),
                  content = shiny::div(
                    shiny::uiOutput(ns("heaping_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("heaping_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("heaping_info_box_instructions"))
                  )
                ),
                list(
                  menu = i18n$t("Method Documentation"),
                  content = shiny::div(
                    shiny::uiOutput(ns("heaping_method_docs"))
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
            shiny::uiOutput(ns("heaping_sample_button"))
          ),
          shiny::div(
            class = "heaping-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("heaping_continue_ui"))
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns("analysis_step"),
            class = "heaping-step",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("heaping_back_to_upload_button"))
            ),
            shiny::uiOutput(ns("heaping_analysis_info_box")),
            shiny::div(
              class = "heaping-controls-block",
              shiny::uiOutput(ns("heaping_controls_header")),
              shiny::div(
                class = "heaping-controls-row",
                shiny::div(
                  class = "ui form heaping-control-form",
                  shiny::uiOutput(ns("heaping_variable_selector"))
                ),
                shiny::div(
                  class = "grouping-control-panel",
                  shiny::uiOutput(ns("grouping_controls"))
                )
              )
            ),
            shiny::div(
              class = "heaping-run-panel",
              shiny::uiOutput(ns("heaping_run_button")),
              shiny::uiOutput(ns("run_log"))
            ),
            shiny::div(
              class = "heaping-results",
              shiny::uiOutput(ns("heaping_table_container"))
            ),
            shinyjs::hidden(
              shiny::div(
                id = ns("download_container"),
                class = "button-container heaping-download",
                shiny::uiOutput(ns("heaping_download_button"))
              )
            )
          )
        )
      )
    )
  )
}


#' Heaping Diagnostics Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
heaping_module_server <- function(input, output, session) {
  message("[HEAPING_MODULE] server initialised")
  mod_simple_module_server("heaping", list(
    setup = function(input, output, session, i18n) {
      heaping_sample_loader <- function() {
        path <- system.file("extdata", "dat_heap_smooth.csv.gz", package = "ODAPbackend")
        df <- readr::read_csv(path, show_col_types = FALSE)
        df <- df |>
          dplyr::select(Age, Deaths, Exposures)
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      shared <- create_shared_data_context(
        "heaping",
        input, output, session, i18n,
        sample_loader = heaping_sample_loader
      )
      shared$last_result <- shiny::reactiveVal(NULL)

      # Reactive UI elements for language switching
      output$heaping_back_button <- shiny::renderUI({
        tryCatch({
          if (!is.null(session$userData$language_version)) {
            lang_ver <- session$userData$language_version()
            cat(sprintf("[HEAPING_UI] heaping_back_button render | lang_ver=%s\n", lang_ver))
          } else {
            cat("[HEAPING_UI] heaping_back_button render | language_version is NULL\n")
          }
        }, error = function(e) {
          cat(sprintf("[HEAPING_UI] heaping_back_button render ERROR: %s\n", e$message))
        })
        shiny::actionButton("heaping_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$heaping_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Heaping Diagnostics")),
          shiny::p(i18n$t("Upload mortality detail, confirm group columns, and review age heaping checks before continuing your workflow."))
        )
      })

      output$heaping_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$heaping_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("The heaping sample contains Age, Deaths, and Exposures observed at single ages."))
        )
      })

      output$heaping_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h3(i18n$t("Heaping Diagnostic Methods")),
          shiny::p(i18n$t("The heaping module provides several methods to detect age preference patterns in demographic data:")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(
              class = "item",
              shiny::div(class = "content",
                shiny::div(class = "header", i18n$t("Bachi Index")),
                shiny::div(class = "description", i18n$t("Measures digit preference by comparing observed vs expected frequencies across all terminal digits"))
              )
            ),
            shiny::div(
              class = "item",
              shiny::div(class = "content",
                shiny::div(class = "header", i18n$t("Myers' Blended Index")),
                shiny::div(class = "description", i18n$t("Detects preference for terminal digits 0-9 using a blending technique to reduce age-specific effects"))
              )
            ),
            shiny::div(
              class = "item",
              shiny::div(class = "content",
                shiny::div(class = "header", i18n$t("Whipple Index")),
                shiny::div(class = "description", i18n$t("Measures attraction to ages ending in 0 or 5, commonly used for data quality assessment"))
              )
            ),
            shiny::div(
              class = "item",
              shiny::div(class = "content",
                shiny::div(class = "header", i18n$t("Noumbissi Index")),
                shiny::div(class = "description", i18n$t("Digit-specific heaping measure that evaluates preference for each terminal digit separately"))
              )
            ),
            shiny::div(
              class = "item",
              shiny::div(class = "content",
                shiny::div(class = "header", i18n$t("Sawtooth Pattern")),
                shiny::div(class = "description", i18n$t("Detects alternating high-low pattern in adjacent ages, indicating systematic data collection issues"))
              )
            )
          )
        )
      })

      output$heaping_sample_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny.semantic::action_button(
          ns("use_sample_data"),
          i18n$t("Use sample data"),
          class = "ui blue button"
        )
      })

      output$heaping_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })

      output$heaping_analysis_info_box <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::div(
          class = "info-box",
          shiny::h1(i18n$t("Heaping Diagnostics")),
          shiny::p(i18n$t("Analyze data quality and identify potential issues with built-in diagnostic tools"))
        )
      })

      output$heaping_controls_header <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::div(
          class = "heaping-controls-header",
          shiny::h3(i18n$t("Controls"))
        )
      })

      output$heaping_variable_selector <- shiny::renderUI({
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

        shiny::div(
          class = "field",
          shiny.semantic::selectInput(
            ns("heaping_variable"),
            i18n$t("Variable to evaluate"),
            choices = choices,
            selected = if (!is.null(input$heaping_variable) && input$heaping_variable %in% choices)
              input$heaping_variable else choices[1]
          )
        )
      })

      output$heaping_run_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(
          ns("run_analysis"),
          i18n$t("Run heaping analysis"),
          class = "ui primary button"
        )
      })

      output$heaping_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::downloadButton(
          ns("download_heaping_csv"),
          i18n$t("Download results"),
          class = "ui primary button"
        )
      })

      output$download_heaping_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("heaping_results_%s.csv", timestamp)
        },
        content = function(file) {
          latest <- shared$last_result()
          cat(sprintf("[HEAPING_MODULE] download handler invoked | last_result_null=%s\n", is.null(latest)))
          shiny::req(latest)

          df <- shared$data()
          shiny::req(df)
          variable <- latest$variable %||% "Deaths"
          cat(sprintf("[HEAPING_MODULE] download context | variable=%s | rows=%s | cols=%s | has_.id=%s\n",
            variable, nrow(df), ncol(df), ".id" %in% names(df)))

          get_label <- function(gid) {
            lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
            if (!is.null(lbl_df) && ".id" %in% names(lbl_df) && ".id_label" %in% names(lbl_df)) {
              matches <- lbl_df[lbl_df$.id == gid, , drop = FALSE]
              if (nrow(matches) > 0) return(matches$.id_label[1])
            }
            return(as.character(gid))
          }

          build_results <- function(data_subset, gid_val) {
            cat(sprintf("[HEAPING_MODULE] building result | gid=%s | subset_rows=%s\n",
              as.character(gid_val), nrow(data_subset)))
            out <- tryCatch(
              ODAPbackend::check_heaping_general(data_subset, variable),
              error = function(e) NULL
            )
            if (is.null(out)) return(NULL)
            out$.id <- gid_val
            out$.id_label <- if (is.na(gid_val)) i18n$t("All records") else get_label(gid_val)
            out
          }

          if (!".id" %in% names(df)) {
            cat("[HEAPING_MODULE] download -> no .id column; running single analysis\n")
            results <- build_results(df, NA)
          } else {
            gid_values <- unique(df$.id)
            cat(sprintf("[HEAPING_MODULE] download -> aggregating %s groups\n", length(gid_values)))
            pieces <- lapply(gid_values, function(gid) {
              subset <- df[df$.id == gid, , drop = FALSE]
              build_results(subset, gid)
            })
            pieces <- Filter(Negate(is.null), pieces)
            results <- if (length(pieces) > 0) do.call(rbind, pieces) else NULL
          }

          shiny::req(results)
          results$variable <- variable
          if (!".id_label" %in% names(results)) {
            results$.id_label <- ifelse(is.na(results$.id), i18n$t("All records"), vapply(results$.id, get_label, character(1L)))
          }
          ordered_cols <- c(".id", ".id_label", "variable", setdiff(names(results), c(".id", ".id_label", "variable")))
          results <- results[, ordered_cols, drop = FALSE]
          cat(sprintf("[HEAPING_MODULE] download final dataset | rows=%s | cols=%s\n", nrow(results), ncol(results)))
          utils::write.csv(results, file, row.names = FALSE)
        },
        contentType = "text/csv"
      )

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      session$onFlushed(function() {
        cat("[HEAPING_MODULE] session onFlushed (initial setup) triggered; hiding analysis step and download button\n")
        shinyjs::hide(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').hide();", analysis_step_id))
        shiny::outputOptions(output, "run_log", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "grouping_controls", suspendWhenHidden = FALSE)
        # Make analysis page UI elements render even when hidden
        shiny::outputOptions(output, "heaping_back_to_upload_button", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "heaping_analysis_info_box", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "heaping_controls_header", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "heaping_variable_selector", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "heaping_run_button", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "heaping_download_button", suspendWhenHidden = FALSE)
        cat("[HEAPING_MODULE] initial setup: hiding download_container\n")
        shinyjs::hide(id = ns("download_container"))
        cat("[HEAPING_MODULE] initial setup: disabling download_heaping_csv button\n")
        shinyjs::disable(ns("download_heaping_csv"))
        cat("[HEAPING_MODULE] initial setup complete\n")
      }, once = TRUE)

      cat("[HEAPING_MODULE] setup: pre-hiding download_container\n")
      shinyjs::hide(id = ns("download_container"))
      cat("[HEAPING_MODULE] setup: pre-disabling download_heaping_csv button\n")
      shinyjs::disable(ns("download_heaping_csv"))

      output$run_log <- shiny::renderUI({ NULL })

      sample_preview <- heaping_sample_loader()

      output$heaping_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      output$heaping_continue_ui <- shiny::renderUI({
        # Force reactivity to language changes
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        if (!isTRUE(shared$group_selection_passed())) {
          return(NULL)
        }
        details <- shared$validation_details()
        if (is.null(details) || !is.data.frame(details)) {
          return(NULL)
        }
        if (all(details$pass == "Pass")) {
          shiny::actionButton(
            ns("go_to_analysis"),
            i18n$t("Continue"),
            class = "ui blue button"
          )
        } else {
          NULL
        }
      })

      shiny::observeEvent(shared$group_selection_passed(), {
        cat(sprintf("[HEAPING_MODULE] shared$group_selection_passed observer | status=%s | data_null=%s | origin=%s\n",
          shared$group_selection_passed(), is.null(shared$data()), shared$data_origin()))
        if (!isTRUE(shared$group_selection_passed())) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))
          shared$last_result(NULL)
          output$heaping_table <- DT::renderDT(NULL)
          output$run_log <- shiny::renderUI({ NULL })
          shinyjs::hide(id = ns("download_container"))
          shinyjs::disable(ns("download_heaping_csv"))
          return()
        }

        # no-op when grouping is confirmed; UI controls update via outputs
      }, ignoreNULL = FALSE)

      shiny::observeEvent(shared$data(), {
        dims <- if (is.null(shared$data())) "NA" else sprintf("%s x %s", nrow(shared$data()), ncol(shared$data()))
        cat(sprintf("[HEAPING_MODULE] ========== shared$data() OBSERVER FIRED ==========\n"))
        cat(sprintf("[HEAPING_MODULE] shared$data() observer fired | data_null=%s | dims=%s | group_passed=%s | origin=%s\n",
          is.null(shared$data()), dims, shared$group_selection_passed(), shared$data_origin()))
        cat(sprintf("[HEAPING_MODULE] RESETTING ANALYSIS STATE: clearing last_result, table, run_log\n"))

        cat(sprintf("[HEAPING_MODULE] showing data_step, hiding analysis_step\n"))
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))

        cat(sprintf("[HEAPING_MODULE] clearing shared$last_result()\n"))
        shared$last_result(NULL)

        cat(sprintf("[HEAPING_MODULE] clearing output$heaping_table_container (removing table from UI)\n"))
        output$heaping_table_container <- shiny::renderUI({ NULL })

        cat(sprintf("[HEAPING_MODULE] clearing output$run_log\n"))
        output$run_log <- shiny::renderUI({ NULL })

        cat(sprintf("[HEAPING_MODULE] hiding download button container\n"))
        shinyjs::hide(id = ns("download_container"))
        shinyjs::runjs(sprintf("$('#%s').hide();", ns("download_container")))

        cat(sprintf("[HEAPING_MODULE] disabling download button\n"))
        shinyjs::disable(ns("download_heaping_csv"))

        cat(sprintf("[HEAPING_MODULE] ========== DATA RESET COMPLETE ==========\n"))
      }, ignoreNULL = TRUE)

      # Additional safety net: observe data_origin() changes to catch data source switches
      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        cat(sprintf("[HEAPING_MODULE] ========== data_origin() OBSERVER FIRED ==========\n"))
        cat(sprintf("[HEAPING_MODULE] data_origin changed to: %s\n", origin))

        if (origin %in% c("upload", "sample")) {
          cat(sprintf("[HEAPING_MODULE] NEW DATA SOURCE DETECTED (%s) - forcing reset\n", origin))
          cat(sprintf("[HEAPING_MODULE] showing data_step, hiding analysis_step\n"))
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))

          cat(sprintf("[HEAPING_MODULE] clearing shared$last_result()\n"))
          shared$last_result(NULL)

          cat(sprintf("[HEAPING_MODULE] clearing output$heaping_table_container (removing table from UI)\n"))
          output$heaping_table_container <- shiny::renderUI({ NULL })

          cat(sprintf("[HEAPING_MODULE] clearing output$run_log\n"))
          output$run_log <- shiny::renderUI({ NULL })

          cat(sprintf("[HEAPING_MODULE] hiding download button container\n"))
          shinyjs::hide(id = ns("download_container"))
          shinyjs::runjs(sprintf("$('#%s').hide();", ns("download_container")))

          cat(sprintf("[HEAPING_MODULE] disabling download button\n"))
          shinyjs::disable(ns("download_heaping_csv"))

          cat(sprintf("[HEAPING_MODULE] ========== DATA ORIGIN RESET COMPLETE ==========\n"))
        } else {
          cat(sprintf("[HEAPING_MODULE] data_origin is '%s' - no reset needed\n", origin))
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      shiny::observeEvent(input$go_to_analysis, {
        cat(sprintf("[HEAPING_MODULE] ========== GO TO ANALYSIS CLICKED ==========\n"))
        message("[HEAPING_MODULE] Continuing to analysis view")
        cat(sprintf("[HEAPING_MODULE] go_to_analysis clicked | data_available=%s | last_result_null=%s | group_passed=%s\n",
          !is.null(shared$data()), is.null(shared$last_result()), shared$group_selection_passed()))
        cat(sprintf("[HEAPING_MODULE] current data_origin: %s\n", shared$data_origin()))
        cat(sprintf("[HEAPING_MODULE] data dimensions: %s\n",
          if (is.null(shared$data())) "NULL" else sprintf("%s x %s", nrow(shared$data()), ncol(shared$data()))))

        cat(sprintf("[HEAPING_MODULE] hiding data_step_id: %s\n", data_step_id))
        shinyjs::hide(id = data_step_id)
        cat(sprintf("[HEAPING_MODULE] showing analysis_step_id: %s\n", analysis_step_id))
        shinyjs::show(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
        cat(sprintf("[HEAPING_MODULE] ========== TRANSITION TO ANALYSIS COMPLETE ==========\n"))
      })

      shiny::observeEvent(input$back_to_upload, {
        cat(sprintf("[HEAPING_MODULE] ========== BACK TO UPLOAD CLICKED ==========\n"))
        message("[HEAPING_MODULE] Returning to upload view")
        cat(sprintf("[HEAPING_MODULE] back_to_upload clicked | data_origin=%s | group_passed=%s\n",
          shared$data_origin(), shared$group_selection_passed()))
        cat(sprintf("[HEAPING_MODULE] last_result_null=%s\n", is.null(shared$last_result())))

        cat(sprintf("[HEAPING_MODULE] showing data_step_id: %s\n", data_step_id))
        shinyjs::show(id = data_step_id)
        cat(sprintf("[HEAPING_MODULE] hiding analysis_step_id: %s\n", analysis_step_id))
        shinyjs::hide(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))

        cat(sprintf("[HEAPING_MODULE] hiding download button container\n"))
        shinyjs::hide(id = ns("download_container"))
        shinyjs::runjs(sprintf("$('#%s').hide();", ns("download_container")))
        cat(sprintf("[HEAPING_MODULE] disabling download button\n"))
        shinyjs::disable(ns("download_heaping_csv"))
        cat(sprintf("[HEAPING_MODULE] ========== BACK TO UPLOAD COMPLETE ==========\n"))
      })

      observeEvent(input$download_heaping_csv, {
        cat(sprintf("[HEAPING_MODULE] download button clicked | value=%s | has_result=%s\n",
          input$download_heaping_csv, !is.null(shared$last_result())))
      }, ignoreNULL = TRUE)

      shared
    },
    prepare = function(input, shared, i18n) {
      cat(sprintf("[HEAPING_MODULE] ========== PREPARE CALLBACK ==========\n"))
      variable <- input$heaping_variable
      cat(sprintf("[HEAPING_MODULE] heaping_variable input: %s\n", if (is.null(variable)) "NULL" else variable))
      if (is.null(variable) || !nzchar(variable)) {
        cat(sprintf("[HEAPING_MODULE] variable is null/empty, defaulting to 'Deaths'\n"))
        variable <- "Deaths"
      }
      cat(sprintf("[HEAPING_MODULE] prepare returning | variable=%s\n", variable))
      list(variable = variable)
    },
    run = function(shared, params, input, i18n) {
      cat(sprintf("[HEAPING_MODULE] ========== RUN CALLBACK ==========\n"))
      cat(sprintf("[HEAPING_MODULE] checking data availability\n"))

      if (is.null(shared$data()) || is.null(shared$data())) {
        cat(sprintf("[HEAPING_MODULE] ERROR: No data available\n"))
        return(list(error = i18n$t("No data available. Please upload a dataset first.")))
      }
      cat(sprintf("[HEAPING_MODULE] data is available | dims=%s x %s\n", nrow(shared$data()), ncol(shared$data())))

      cat(sprintf("[HEAPING_MODULE] checking group_selection_passed status\n"))
      if (!isTRUE(shared$group_selection_passed())) {
        cat(sprintf("[HEAPING_MODULE] ERROR: grouping not confirmed | status=%s\n", shared$group_selection_passed()))
        return(list(error = i18n$t("Please confirm the grouping modal before running the analysis.")))
      }
      cat(sprintf("[HEAPING_MODULE] group selection passed\n"))

      cat(sprintf("[HEAPING_MODULE] retrieving filtered_data()\n"))
      data_subset <- shared$filtered_data()
      cat(sprintf("[HEAPING_MODULE] filtered subset -> rows: %s | cols: %s\n", nrow(data_subset), ncol(data_subset)))
      if (nrow(data_subset) > 0) {
        cat(sprintf("[HEAPING_MODULE] preview of filtered data (first 3 rows):\n"))
        capture.output(print(head(data_subset, 3))) |> cat(sep = "\n")
      }

      if (is.null(data_subset) || nrow(data_subset) == 0) {
        cat(sprintf("[HEAPING_MODULE] ERROR: filtered data is empty\n"))
        return(list(error = i18n$t("The selected group returned no rows to analyse.")))
      }

      cat(sprintf("[HEAPING_MODULE] checking if variable '%s' exists in data\n", params$variable))
      if (!params$variable %in% names(data_subset)) {
        cat(sprintf("[HEAPING_MODULE] ERROR: variable '%s' not found | available columns: %s\n",
          params$variable, paste(names(data_subset), collapse = ", ")))
        return(list(error = sprintf(i18n$t("Column '%s' is missing from the dataset."), params$variable)))
      }

      cat(sprintf("[HEAPING_MODULE] running heaping analysis | variable=%s\n", params$variable))
      analysis <- tryCatch({
        ODAPbackend::check_heaping_general(data_subset, params$variable)
      }, error = function(e) {
        cat(sprintf("[HEAPING_MODULE] ERROR during check_heaping_general: %s\n", e$message))
        list(error = i18n$t("Heaping diagnostics failed. Check input data validity."))
      })

      if (!is.data.frame(analysis)) {
        cat(sprintf("[HEAPING_MODULE] analysis result is not a data.frame | has_error=%s\n", !is.null(analysis$error)))
        if (!is.null(analysis$error)) {
          return(analysis)
        }
        return(list(error = i18n$t("Heaping diagnostics failed. Check input data validity.")))
      }

      cat(sprintf("[HEAPING_MODULE] analysis successful | result_rows=%s | result_cols=%s\n",
        nrow(analysis), ncol(analysis)))

      gid <- shared$active_group_id()
      cat(sprintf("[HEAPING_MODULE] active_group_id: %s\n", as.character(gid)))

      label_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
      group_label <- gid
      if (!is.null(label_df) && ".id" %in% names(label_df) && ".id_label" %in% names(label_df)) {
        match_idx <- which(label_df$.id == gid)
        if (length(match_idx) > 0) {
          group_label <- label_df$.id_label[match_idx][1]
          cat(sprintf("[HEAPING_MODULE] found label for group %s: %s\n", as.character(gid), group_label))
        }
      }

      cat(sprintf("[HEAPING_MODULE] preparing result object\n"))
      result <- list(
        table = analysis,
        group_label = group_label,
        variable = params$variable
      )
      cat(sprintf("[HEAPING_MODULE] ========== RUN CALLBACK COMPLETE ==========\n"))
      result
    },
    render = function(result, output, shared, input, i18n) {
      cat(sprintf("[HEAPING_MODULE] ========== RENDER CALLBACK ==========\n"))
      cat(sprintf("[HEAPING_MODULE] checking result status | result_null=%s | has_error=%s\n",
        is.null(result), !is.null(result$error)))

      if (is.null(result) || !is.null(result$error)) {
        msg <- if (!is.null(result$error)) result$error else i18n$t("No diagnostics computed yet.")
        cat(sprintf("[HEAPING_MODULE] ERROR in result, displaying error message: %s\n", msg))

        cat(sprintf("[HEAPING_MODULE] rendering error in run_log\n"))
        output$run_log <- shiny::renderUI({
          shiny::span(class = "heaping-run-log error", msg)
        })

        cat(sprintf("[HEAPING_MODULE] clearing heaping_table_container\n"))
        output$heaping_table_container <- shiny::renderUI({ NULL })

        cat(sprintf("[HEAPING_MODULE] hiding download button container\n"))
        shinyjs::hide(id = session$ns("download_container"))
        shinyjs::runjs(sprintf("$('#%s').hide();", session$ns("download_container")))

        cat(sprintf("[HEAPING_MODULE] disabling download button\n"))
        shinyjs::disable(session$ns("download_heaping_csv"))

        cat(sprintf("[HEAPING_MODULE] ========== RENDER ERROR COMPLETE ==========\n"))
        return()
      }

      cat(sprintf("[HEAPING_MODULE] result is valid, saving to shared$last_result()\n"))
      shared$last_result(result)

      cat(sprintf("[HEAPING_MODULE] clearing run_log (success)\n"))
      output$run_log <- shiny::renderUI({ NULL })

      cat(sprintf("[HEAPING_MODULE] showing download button container\n"))
      shinyjs::show(id = session$ns("download_container"))
      shinyjs::runjs(sprintf("$('#%s').show();", session$ns("download_container")))

      cat(sprintf("[HEAPING_MODULE] enabling download button\n"))
      shinyjs::enable(session$ns("download_heaping_csv"))

      cat(sprintf("[HEAPING_MODULE] rendering heaping_table | table_rows=%s | table_cols=%s\n",
        nrow(result$table), ncol(result$table)))

      # Render the table container with the table inside it
      output$heaping_table_container <- shiny::renderUI({
        cat(sprintf("[HEAPING_MODULE] [inside heaping_table_container renderUI] checking for result\n"))

        # Get the current result
        current_result <- shared$last_result()
        if (is.null(current_result)) {
          cat(sprintf("[HEAPING_MODULE] [inside heaping_table_container renderUI] no result, returning NULL\n"))
          return(NULL)
        }

        cat(sprintf("[HEAPING_MODULE] [inside heaping_table_container renderUI] result exists, preparing table\n"))
        tbl <- current_result$table
        display_tbl <- tbl
        color_map <- NULL

        if ("color" %in% names(tbl)) {
          cat(sprintf("[HEAPING_MODULE] color column found, extracting color map\n"))
          color_map <- stats::setNames(unique(tbl$color), unique(tbl$level))
          display_tbl$color <- NULL
        }

        cat(sprintf("[HEAPING_MODULE] creating DT::datatable widget\n"))
        widget <- DT::datatable(
          display_tbl,
          rownames = FALSE,
          options = list(
            dom = "tip",
            pageLength = nrow(display_tbl),
            searching = FALSE,
            ordering = FALSE
          )
        )

        if (!is.null(color_map)) {
          cat(sprintf("[HEAPING_MODULE] applying color formatting | color_levels=%s\n", length(color_map)))
          widget <- DT::formatStyle(
            widget,
            "level",
            target = "row",
            backgroundColor = DT::styleEqual(names(color_map), unname(color_map)),
            color = DT::styleEqual(names(color_map), rep("#000000", length(color_map)))
          )
        }

        cat(sprintf("[HEAPING_MODULE] table widget created successfully, returning to UI\n"))
        widget
      })

      cat(sprintf("[HEAPING_MODULE] ========== RENDER SUCCESS COMPLETE ==========\n"))
    },
    register_downloads = NULL
  ))
}
