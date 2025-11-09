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
            max-width: 900px;
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
            shiny::uiOutput(ns("smoothing_info_box_content")),
            rhandsontable::rHandsontableOutput(ns("smoothing_sample_table"), width = "100%", height = 210),
            shiny::uiOutput(ns("smoothing_info_box_instructions"))
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
                shiny::uiOutput(ns("smoothing_group_selectors")),
                shiny::div(
                  id = ns("plot_container"),
                  style = "display: none;",  # Hidden until results available
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns("smoothing_plot"), height = "500px")
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
                )
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
          shiny::p(i18n$t("Minimum required: Age and one numeric variable to smooth (Deaths, Exposures, or similar counts).")),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("The smoothing sample contains Age, Deaths, and Exposures observed at single ages."))
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

        message("[SMOOTHING] smoothing_continue_ui rendering...")

        # Require group selection to be complete
        group_passed <- shared$group_selection_passed()
        message("[SMOOTHING] group_selection_passed = ", group_passed)
        req(group_passed)

        # Try to get validation details
        details <- tryCatch(shared$validation_details(), error = function(e) NULL)
        message("[SMOOTHING] validation_details: ", !is.null(details))

        # If we have details, check if they pass
        if (!is.null(details) && is.data.frame(details)) {
          if (!all(details$pass == "Pass")) {
            message("[SMOOTHING] Validation failed - not showing button")
            return(NULL)  # Validation failed, don't show button
          }
        }

        # Show button if: validation passed OR no validation details available (assume OK)
        message("[SMOOTHING] Creating continue button with ID: ", ns("go_to_analysis"))
        shiny::actionButton(
          ns("go_to_analysis"),  # YES use ns() - heaping does this too!
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
        message("[SMOOTHING] Rendering smoothing_controls...")
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        message("[SMOOTHING] Creating control inputs...")
        shiny::tagList(
          shiny.semantic::selectInput(
            ns("smoothing_variable"),
            i18n$t("Variable to smooth"),
            choices = c("Deaths", "Exposures"),
            selected = if (!is.null(input$smoothing_variable)) input$smoothing_variable else "Deaths"
          ),
          shiny.semantic::selectInput(
            ns("fine_method"),
            i18n$t("Fine Method"),
            choices = c("auto", "none", "sprague", "beers(ord)", "beers(mod)", "grabill", "pclm", "mono", "uniform"),
            selected = if (!is.null(input$fine_method)) input$fine_method else "sprague"
          ),
          shiny.semantic::selectInput(
            ns("rough_method"),
            i18n$t("Rough Method"),
            choices = c("auto", "none", "Carrier-Farrag", "KKN", "Arriaga", "United Nations", "Strong", "Zigzag"),
            selected = if (!is.null(input$rough_method)) input$rough_method else "none"
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

      # Download button
      output$smoothing_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::downloadButton(
          ns("download_smoothing_csv"),
          i18n$t("Download results"),
          class = "ui primary button"
        )
      })
      outputOptions(output, "smoothing_download_button", suspendWhenHidden = FALSE)

      # Download handler
      output$download_smoothing_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("smoothing_results_%s.csv", timestamp)
        },
        content = function(file) {
          latest <- shared$last_result()
          shiny::req(latest)

          df <- shared$data()
          shiny::req(df)
          variable <- latest$variable %||% "Deaths"

          # Run smoothing for all groups
          if (!".id" %in% names(df)) {
            result_smooth <- ODAPbackend::smooth_flexible(
              df, variable, latest$age_out, latest$fine_method,
              latest$rough_method, latest$u5m, latest$constrain_infants,
              Sex = "t", i18n = i18n
            )
            out_data <- result_smooth$data_out
          } else {
            # Process all groups
            group_ids <- unique(df$.id)
            pieces <- lapply(group_ids, function(gid) {
              subset <- df[df$.id == gid, , drop = FALSE]
              result_smooth <- ODAPbackend::smooth_flexible(
                subset, variable, latest$age_out, latest$fine_method,
                latest$rough_method, latest$u5m, latest$constrain_infants,
                Sex = "t", i18n = i18n
              )
              result_smooth$data_out
            })
            out_data <- do.call(rbind, pieces)
          }

          # Add labels
          if (".id" %in% names(out_data)) {
            lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
            if (!is.null(lbl_df)) {
              out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
            }
          }

          utils::write.csv(out_data, file, row.names = FALSE)
        },
        contentType = "text/csv"
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
      message("[SMOOTHING] Setting up go_to_analysis observer...")
      shiny::observeEvent(input$go_to_analysis, {
        message("[SMOOTHING] ========== GO_TO_ANALYSIS CLICKED ==========")
        message("[SMOOTHING] input$go_to_analysis value: ", input$go_to_analysis)
        message("[SMOOTHING] data_step_id: ", data_step_id)
        message("[SMOOTHING] analysis_step_id: ", analysis_step_id)
        message("[SMOOTHING] Calling hide on data_step...")
        shinyjs::hide(id = data_step_id)
        message("[SMOOTHING] Calling show on analysis_step...")
        shinyjs::show(id = analysis_step_id)
        # Also use jQuery like heaping does
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
        message("[SMOOTHING] ========== NAVIGATION COMPLETE ==========")
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      message("[SMOOTHING] go_to_analysis observer registered")

      # Navigation: Step 2 → Step 1
      shiny::observeEvent(input$back_to_upload, {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))
        shinyjs::hide(id = ns("download_container"))
        shinyjs::runjs(sprintf("$('#%s').hide();", ns("download_container")))
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
