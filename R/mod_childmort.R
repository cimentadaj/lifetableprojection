#' Child Mortality (Brass) Module UI
#'
#' Two-step module (data upload, then analysis) estimating child mortality
#' from children ever born and children dead with the Brass indirect method.
#'
#' @param i18n Translator helper used for labels.
#' @noRd
childmort_module_ui <- function(i18n) {
  ns <- shiny::NS("childmort")

  shinyjs::hidden(
    shiny::div(
      id = "childmort_module_page",
      # CSS styling
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .childmort-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }
          .childmort-step {
            width: 100%;
          }
          .childmort-step.main-content {
            display: block;
          }
          .childmort-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .childmort-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .childmort-hero p {
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
          .childmort-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
          .childmort-results {
            margin-top: 1.5rem;
          }
          .childmort-diagnostics {
            margin-top: 1.5rem;
          }
          .childmort-diagnostics ul {
            padding-left: 1.2rem;
            margin: 0.5rem 0 0;
          }
          .childmort-diagnostics li {
            margin-bottom: 0.4rem;
            color: #425466;
          }
          /* Fix tooltip horizontal overflow - force text wrapping */
          [data-tooltip]:before {
            white-space: normal !important;
            max-width: 300px !important;
            line-height: 1.4 !important;
          }
        "))
      ),
      shiny::div(
        class = "childmort-main",

        # STEP 1: Data Upload
        shiny::div(
          id = ns("data_step"),
          class = "childmort-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "childmort-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("childmort_back_button"))
            ),
            shiny::uiOutput(ns("childmort_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = create_info_tab_label(ns("info_tab_upload_label"), i18n$t("Upload Instructions")),
                  content = shiny::div(
                    shiny::uiOutput(ns("childmort_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("childmort_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("childmort_info_box_instructions"))
                  )
                ),
                list(
                  menu = create_info_tab_label(ns("info_tab_docs_label"), i18n$t("Method Documentation")),
                  content = shiny::div(
                    shiny::uiOutput(ns("childmort_method_docs"))
                  )
                )
              )
            )
          ),
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
            shiny::uiOutput(ns("childmort_sample_button"))
          ),
          shiny::div(
            class = "childmort-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("childmort_continue_ui"))
          )
        ),

        # STEP 2: Analysis
        shiny::div(
          id = ns("analysis_step"),
          class = "childmort-step",
          style = "display: none;",
          shiny::div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
            shiny::uiOutput(ns("childmort_back_to_upload_button"))
          ),
          untheme::sidebar_layout_responsive(
            # LEFT SIDEBAR: Controls
            list(
              children = shiny::div(
                class = "ui form",
                shiny::uiOutput(ns("childmort_controls")),
                shiny::br(),
                shiny::uiOutput(ns("childmort_run_button"))
              )
            ),
            # RIGHT MAIN: Plot, table and diagnostics
            shiny::div(
              style = "flex: 1; min-width: 0;",
              shiny::uiOutput(ns("childmort_group_selectors")),
              shiny::div(
                id = ns("plot_container"),
                style = "display: none;",
                shiny::uiOutput(ns("childmort_run_settings")),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("childmort_plot"), height = "520px", width = "100%")
                ),
                shiny::div(
                  class = "childmort-results",
                  DT::DTOutput(ns("childmort_table"))
                ),
                shiny::uiOutput(ns("childmort_diagnostics"))
              ),
              shiny::br(),
              shinyjs::hidden(
                shiny::div(
                  id = ns("download_container"),
                  class = "button-container",
                  style = "justify-content: flex-end; margin-top: 1rem;",
                  shiny::uiOutput(ns("childmort_download_button"))
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


#' Build an error plot with a centred message
#'
#' @param message Error text.
#' @param i18n Translator helper.
#' @noRd
childmort_error_plot <- function(message, i18n) {
  plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(size = 0, opacity = 0)) |>
    plotly::add_annotations(
      text = paste0("<b>", i18n$t("Error"), ":</b><br><br>", message),
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 16, color = "red"),
      xanchor = "center", yanchor = "middle"
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
      plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa",
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)
}


#' Analysis info text written into child mortality downloads
#'
#' @param params Parameters used in the run (`run_childmort()$params`).
#' @param n_groups Number of groups included, or `NULL` for a single group.
#' @noRd
childmort_info_text <- function(params, n_groups = NULL) {
  variant_label <- if (identical(params$variant, "palloni_helligman")) {
    "Palloni-Heligman (UN model life tables)"
  } else {
    "Trussell (Coale-Demeny model life tables)"
  }
  date_label <- if (is.null(params$date_obs) || length(params$date_obs) == 0) {
    "taken from the Date column"
  } else {
    as.character(params$date_obs)
  }
  paste(
    c(
      if (is.null(n_groups)) {
        "Module: Child Mortality (Brass) Analysis"
      } else {
        "Module: Child Mortality (Brass) Analysis (All Groups)"
      },
      "Description: Indirect estimation of child mortality from children ever born and children dead reported by women aged 15-49 (UN Manual X, 1983), using MortIndEst::childmort_brass().",
      "",
      sprintf("Multiplier variant: %s", variant_label),
      sprintf("Model life table family: %s", params$mlt_model),
      sprintf("Model life table sex: %s", params$sex),
      sprintf("Date of observation: %s", date_label),
      if (identical(params$variant, "palloni_helligman")) {
        sprintf("Mean age at maternity: %s", params$mam)
      },
      "",
      "Output columns:",
      "- Age / age_label: women's five-year age group",
      "- x / q_label: childhood age of the estimate, q(x) = probability of dying before age x",
      "- qx: estimated probability of dying; lx = 1 - qx",
      "- t_years: years before the observation date the estimate refers to",
      "- date_t: reference date of the estimate (decimal year)",
      "- e0_implied: life expectancy at birth of the matching model life table level",
      "",
      sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      if (!is.null(n_groups)) sprintf("Number of Groups: %d", n_groups),
      "",
      "Note: estimates from women aged 15-19 and 20-24 are usually biased upwards; estimates from women aged 45-49 refer to distant periods and are affected by recall errors."
    ),
    collapse = "\n"
  )
}


#' Child Mortality (Brass) Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
childmort_module_server <- function(input, output, session) {

  mod_simple_module_server("childmort", list(
    setup = function(input, output, session, i18n) {
      # Sample data loader
      childmort_sample_loader <- function() {
        path <- system.file("extdata", "childmort_sample.csv", package = "lifetableprojection")
        df <- readr::read_csv(path, show_col_types = FALSE)
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      # Create shared data context
      shared <- create_shared_data_context(
        "childmort",
        input, output, session, i18n,
        sample_loader = childmort_sample_loader,
        validation_function = validateData_childmort
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")
      # Validation messages in the selected language
      render_translated_validation(output, session, i18n, shared, CHILDMORT_MESSAGE_TEMPLATES)

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      # Language-reactive UI outputs (Step 1)
      output$childmort_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("childmort_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$childmort_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Child Mortality (Brass Method)")),
          shiny::p(i18n$t("Estimate infant and child mortality from the number of children ever born and children dead reported by women aged 15-49."))
        )
      })

      output$childmort_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$childmort_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::p(shiny::strong(i18n$t("Required columns:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Age (lower bound of women's five-year age groups: 15, 20, 25, 30, 35, 40, 45)")),
            shiny::tags$li(i18n$t("Women (number of women in each age group)")),
            shiny::tags$li(i18n$t("CEB (children ever born to those women)")),
            shiny::tags$li(i18n$t("CD (children dead) or CS (children surviving)"))
          ),
          shiny::p(shiny::strong(i18n$t("Optional columns:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Date (date of the census or survey as a decimal year, e.g. 1976.5). If absent, set it in the analysis step."))
          ),
          shiny::p(shiny::tags$em(i18n$t("Additional grouping columns are supported for batch analysis."))),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("Sample data: Panama 1976 survey (UN Manual X, Table 49)."))
        )
      })

      output$childmort_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h3(i18n$t("Brass Child Mortality Method")),
          shiny::p(i18n$t("The Brass method converts the proportion of children dead among children ever born, reported by women in each five-year age group, into probabilities of dying in childhood. Women aged 15-19 give q(1), 20-24 give q(2), 25-29 give q(3), 30-34 give q(5), 35-39 give q(10), 40-44 give q(15) and 45-49 give q(20). Each estimate is located in time a number of years before the census or survey, and matched to a model life table to obtain an implied life expectancy at birth.")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Trussell"),
              shiny::div(class = "description", i18n$t("Trussell multipliers use the Coale-Demeny model life table families (North, South, East, West). This is the variant recommended in UN Manual X."))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "Palloni-Heligman"),
              shiny::div(class = "description", i18n$t("Palloni-Heligman multipliers use the United Nations model life tables for developing countries (Latin American, Chilean, South Asian, Far East Asian, General) and also require the mean age at maternity."))
            ))
          ),
          shiny::h4(i18n$t("Reading the results")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Each point on the chart is an estimate of q(x), the probability of dying before age x, placed at the date it refers to. Estimates from younger women refer to more recent dates.")),
            shiny::tags$li(i18n$t("The proportion of children dead drives the estimates; the multiplier variant and model life table family only adjust them slightly. The model life table sex changes only the implied life expectancy at birth.")),
            shiny::tags$li(i18n$t("Implied life expectancy at birth is that of the model life table with the same q(x). It puts the estimates for different ages x on one scale, so you can read the trend over time.")),
            shiny::tags$li(i18n$t("Give less weight to estimates from women aged 15-19 and 20-24, which are often too high, and to those from women aged 45-49, which refer to distant dates and suffer from recall errors."))
          ),
          shiny::h4(i18n$t("References")),
          # Citations are kept in their original language.
          shiny::tags$ul(
            class = "childmort-references",
            shiny::tags$li("United Nations (1983). Manual X: Indirect Techniques for Demographic Estimation. Population Studies No. 81. New York: United Nations."),
            shiny::tags$li("Brass, W. et al. (1968). The Demography of Tropical Africa. Princeton: Princeton University Press."),
            shiny::tags$li("Trussell, T. J. (1975). A re-estimation of the multiplying factors for the Brass technique for determining childhood survivorship rates. Population Studies 29(1): 97-107."),
            shiny::tags$li("Palloni, A. and Heligman, L. (1985). Re-estimation of structural parameters to obtain estimates of mortality in developing countries. Population Bulletin of the United Nations 18: 10-33.")
          )
        )
      })

      output$childmort_sample_button <- shiny::renderUI({
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
      sample_preview <- childmort_sample_loader()
      output$childmort_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$childmort_continue_ui <- shiny::renderUI({
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
      output$childmort_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "childmort_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$childmort_controls <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        df <- shared$data()
        has_date_col <- !is.null(df) && "date" %in% tolower(names(df))

        shiny::tagList(
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(
                `for` = ns("variant"),
                i18n$t("Multiplier variant")
              ),
              shiny.fluent::TooltipHost(
                content = shiny::uiOutput(ns("variant_tooltip")),
                delay = 0,
                shiny.fluent::Image(
                  src = "www/info.png",
                  width = "20px",
                  shouldStartVisible = TRUE
                )
              )
            ),
            shiny.semantic::selectInput(
              ns("variant"),
              NULL,
              choices = c("Trussell" = "trussell", "Palloni-Heligman" = "palloni_helligman"),
              selected = shiny::isolate(input$variant) %||% "trussell"
            )
          ),
          # Family and mean age at maternity depend on the variant
          shiny::uiOutput(ns("childmort_family_ui")),
          shiny::uiOutput(ns("childmort_mam_ui")),
          shiny.semantic::selectInput(
            ns("mlt_sex"),
            i18n$t("Model life table sex"),
            choices = stats::setNames(c("male", "female"), c(i18n$t("Male"), i18n$t("Female"))),
            selected = shiny::isolate(input$mlt_sex) %||% "male"
          ),
          shiny::p(
            class = "childmort-control-hint",
            style = "font-size: 0.9em; color: #666; margin-top: 0.4em;",
            i18n$t("Only changes the implied life expectancy at birth; the q(x) estimates stay the same.")
          ),
          if (has_date_col) {
            shiny::p(i18n$t("Date values will be taken from your data"))
          } else {
            shiny::numericInput(
              ns("date_obs"),
              i18n$t("Date of observation (decimal year)"),
              value = shiny::isolate(input$date_obs) %||% 2020,
              min = 1900,
              max = 2100,
              step = 0.1
            )
          }
        )
      })
      outputOptions(output, "childmort_controls", suspendWhenHidden = FALSE)

      # Model family depends on the multiplier variant
      output$childmort_family_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        variant <- input$variant %||% "trussell"
        families <- childmort_families(variant)
        family_labels <- CHILDMORT_FAMILY_LABELS
        current <- shiny::isolate(input$mlt_model)
        default <- if (identical(variant, "trussell")) "West" else "General"
        shiny.semantic::selectInput(
          ns("mlt_model"),
          i18n$t("Model life table family"),
          choices = stats::setNames(families, i18n$t(unname(family_labels[families]))),
          selected = if (!is.null(current) && current %in% families) current else default
        )
      })
      outputOptions(output, "childmort_family_ui", suspendWhenHidden = FALSE)

      # Mean age at maternity only applies to Palloni-Heligman
      output$childmort_mam_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!identical(input$variant, "palloni_helligman")) {
          return(NULL)
        }
        shiny::numericInput(
          ns("mam"),
          i18n$t("Mean age at maternity"),
          value = shiny::isolate(input$mam) %||% 27,
          min = 20,
          max = 40,
          step = 0.1
        )
      })
      outputOptions(output, "childmort_mam_ui", suspendWhenHidden = FALSE)

      output$childmort_run_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(
          ns("run_analysis"),
          i18n$t("Estimate Child Mortality"),
          class = "ui primary button"
        )
      })
      outputOptions(output, "childmort_run_button", suspendWhenHidden = FALSE)

      # Settings behind the shown results, and a note when the controls differ
      output$childmort_run_settings <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        latest <- shared$last_result()
        if (is.null(latest) || is.null(latest$params)) {
          return(NULL)
        }
        items <- childmort_settings_items(latest$params)
        parts <- vapply(seq_len(nrow(items)), function(i) {
          value <- if (items$translate_value[i]) i18n$t(items$value[i]) else items$value[i]
          paste0(i18n$t(items$label[i]), ": ", value)
        }, character(1))
        changed <- childmort_settings_changed(latest$params, list(
          variant = input$variant,
          mlt_model = input$mlt_model,
          sex = input$mlt_sex,
          date_obs = input$date_obs,
          mam = input$mam
        ))
        shiny::tagList(
          shiny::div(
            class = "ui small message childmort-run-settings",
            shiny::tags$b(paste0(i18n$t("Results for"), ": ")),
            paste(parts, collapse = " \u00b7 ")
          ),
          if (changed) {
            shiny::div(
              class = "ui warning message childmort-settings-changed",
              i18n$t("The settings have changed. Click 'Estimate Child Mortality' to update the results.")
            )
          }
        )
      })

      # Tooltip text output for info icon
      output$variant_tooltip <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::HTML(i18n$t("Set of multipliers converting proportions of children dead into probabilities of dying:<br><br>'Trussell' - Coale-Demeny model life tables (North, South, East, West)<br>'Palloni-Heligman' - UN model life tables for developing countries; also needs the mean age at maternity"))
      })

      # Group selectors
      output$childmort_group_selectors <- shiny::renderUI({
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
      outputOptions(output, "childmort_group_selectors", suspendWhenHidden = FALSE)

      # Download button
      output$childmort_download_button <- shiny::renderUI({
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
            ns("download_childmort_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "childmort_download_button", suspendWhenHidden = FALSE)

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
                    id = "childmort_radio_single",
                    shiny::tags$input(
                      type = "radio",
                      name = "childmort_download_scope_radio",
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
                    id = "childmort_radio_all",
                    shiny::tags$input(
                      type = "radio",
                      name = "childmort_download_scope_radio",
                      value = "all"
                    ),
                    shiny::tags$label(i18n$t("All groups"))
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
              i18n$t("Note: Downloading all groups creates a combined ZIP file with the results of every group.")
            ),
            shiny::tags$script(shiny::HTML(sprintf("
              $(document).ready(function() {
                $('.ui.radio.checkbox').checkbox();

                $('input[name=\"childmort_download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"childmort_download_scope_radio\"]:checked').val();
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
        download_scope_choice(input$download_scope_js)
      })

      # Label for a group id
      group_label_for <- function(gid) {
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        label <- NULL
        if (!is.null(lbl_df)) {
          label <- lbl_df$.id_label[lbl_df$.id == gid]
        }
        if (length(label) == 0 || is.na(label[1])) as.character(gid) else as.character(label[1])
      }

      # Add group labels to an output table and sort by group / age
      add_labels <- function(out_data) {
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        if (!is.null(lbl_df) && ".id" %in% names(out_data)) {
          out_data$.id_label <- NULL
          out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
        }
        out_data$Age <- as.numeric(out_data$Age)
        if (".id" %in% names(out_data)) {
          out_data <- out_data[order(out_data$.id, out_data$Age), ]
        } else {
          out_data <- out_data[order(out_data$Age), ]
        }
        out_data
      }

      # Save the plot of one group to a PNG file
      save_group_plot <- function(estimates, plot_file, gid) {
        plot_obj <- childmort_plot(
          estimates,
          title = paste0(i18n$t("Child mortality estimates"), " - ", group_label_for(gid)),
          x_label = i18n$t("Reference date"),
          y_label = i18n$t("Probability of dying, q(x)"),
          hover_labels = list(women = i18n$t("Women"), date = i18n$t("Date"))
        )
        ggplot2::ggsave(plot_file, plot = plot_obj, width = 10, height = 6, dpi = 300)
      }

      # Helper function to download single group
      download_single_group <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest)
        gid <- shared$active_group_id()

        estimates <- latest$estimates
        shiny::req(estimates)
        estimates <- estimates[estimates$.id == gid, , drop = FALSE]
        shiny::req(nrow(estimates) > 0)
        mlt <- latest$mlt[latest$mlt$.id == gid, , drop = FALSE]

        temp_dir <- tempfile()
        dir.create(temp_dir)
        temp_files <- c()

        # 1. Estimates CSV
        csv_file <- file.path(temp_dir, "childmort_results.csv")
        utils::write.csv(add_labels(estimates), csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # 2. Interpolated model life tables CSV
        mlt_file <- file.path(temp_dir, "childmort_model_life_tables.csv")
        mlt$Age <- mlt$age
        mlt <- add_labels(mlt)
        mlt$Age <- NULL
        utils::write.csv(mlt, mlt_file, row.names = FALSE)
        temp_files <- c(temp_files, mlt_file)

        # 3. Plot PNG
        plot_file <- file.path(temp_dir, "childmort_plot.png")
        save_group_plot(estimates, plot_file, gid)
        temp_files <- c(temp_files, plot_file)

        # 4. Analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        writeLines(childmort_info_text(latest$params), info_file)
        temp_files <- c(temp_files, info_file)

        # 5. Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
        unlink(temp_dir, recursive = TRUE)
      }

      # Helper function to download all groups
      download_all_groups <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest)
        estimates <- latest$estimates
        shiny::req(estimates)

        temp_dir <- tempfile()
        dir.create(temp_dir)
        temp_files <- c()

        # Combined estimates for all groups
        csv_file <- file.path(temp_dir, "childmort_results_all_groups.csv")
        utils::write.csv(add_labels(estimates), csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # Combined interpolated model life tables
        mlt <- latest$mlt
        mlt_file <- file.path(temp_dir, "childmort_model_life_tables_all_groups.csv")
        mlt$Age <- mlt$age
        mlt <- add_labels(mlt)
        mlt$Age <- NULL
        utils::write.csv(mlt, mlt_file, row.names = FALSE)
        temp_files <- c(temp_files, mlt_file)

        # Groups that could not be estimated
        if (!is.null(latest$group_errors) && nrow(latest$group_errors) > 0) {
          err_file <- file.path(temp_dir, "childmort_errors.csv")
          utils::write.csv(latest$group_errors, err_file, row.names = FALSE)
          temp_files <- c(temp_files, err_file)
        }

        # One plot per group, named after the group label
        for (gid in unique(estimates$.id)) {
          safe_label <- gsub("[^a-zA-Z0-9_-]", "_", group_label_for(gid))
          plot_file <- file.path(temp_dir, sprintf("childmort_plot_%s.png", safe_label))
          save_group_plot(estimates[estimates$.id == gid, , drop = FALSE], plot_file, gid)
          temp_files <- c(temp_files, plot_file)
        }

        # Analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        n_groups <- length(unique(estimates$.id)) + nrow(latest$group_errors %||% data.frame())
        writeLines(childmort_info_text(latest$params, n_groups = n_groups), info_file)
        temp_files <- c(temp_files, info_file)

        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
        unlink(temp_dir, recursive = TRUE)
      }

      # Download handler for single group
      output$download_childmort_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("childmort_results_%s.zip", timestamp)
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      # Download handler for modal confirmation
      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          scope <- download_scope_choice()
          if (scope == "all") {
            sprintf("childmort_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("childmort_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          modal_id <- ns("download_modal")
          shiny.semantic::hide_modal(modal_id, session = session)

          if (scope == "all") {
            download_all_groups(file)
          } else {
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

      reset_results <- function() {
        shared$last_result(NULL)
        output$childmort_plot <- plotly::renderPlotly(NULL)
        output$childmort_table <- DT::renderDT(NULL)
        output$childmort_diagnostics <- shiny::renderUI(NULL)
        shinyjs::hide(id = ns("plot_container"))
        shinyjs::hide(id = ns("download_container"))
      }

      # Data reset observers
      shiny::observeEvent(shared$data(), {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        reset_results()
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        if (origin %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          reset_results()
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
      df <- shiny::isolate(shared$data())
      has_date_col <- !is.null(df) && "date" %in% tolower(names(df))
      variant <- input$variant %||% "trussell"
      mlt_model <- input$mlt_model %||% if (identical(variant, "trussell")) "West" else "General"
      list(
        variant = variant,
        mlt_model = mlt_model,
        sex = input$mlt_sex %||% "male",
        date_obs = if (has_date_col) NULL else input$date_obs,
        mam = input$mam %||% 27
      )
    },

    run = function(shared, params, input, i18n) {
      data_full <- shiny::isolate(shared$data())
      if (is.null(data_full) || nrow(data_full) == 0) {
        return(list(error = i18n$t("No data available")))
      }
      result <- tryCatch(
        run_childmort(
          data_full,
          variant = params$variant,
          mlt_model = params$mlt_model,
          sex = params$sex,
          date_obs = params$date_obs,
          mam = params$mam
        ),
        error = function(e) {
          list(error = paste0(
            i18n$t("Estimation failed: "),
            translate_messages(conditionMessage(e), i18n, CHILDMORT_MESSAGE_TEMPLATES)
          ))
        }
      )
      shared$last_result(if (is.null(result$error)) result else NULL)
      result
    },

    render = function(result, output, shared, input, i18n) {
      output$childmort_plot <- plotly::renderPlotly({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!is.null(result$error)) {
          return(childmort_error_plot(result$error, i18n))
        }
        current_gid <- shared$active_group_id()

        group_error <- result$group_errors$message[result$group_errors$.id == as.character(current_gid)]
        if (length(group_error) > 0) {
          return(childmort_error_plot(translate_messages(group_error[1], i18n, CHILDMORT_MESSAGE_TEMPLATES), i18n))
        }

        estimates <- result$estimates
        if (is.null(estimates)) {
          return(NULL)
        }
        estimates <- estimates[estimates$.id == current_gid, , drop = FALSE]
        if (nrow(estimates) == 0) {
          return(NULL)
        }

        plot_obj <- childmort_plot(
          estimates,
          title = i18n$t("Child mortality estimates"),
          x_label = i18n$t("Reference date"),
          y_label = i18n$t("Probability of dying, q(x)"),
          hover_labels = list(women = i18n$t("Women"), date = i18n$t("Date"))
        )
        plotly::ggplotly(plot_obj, tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })

      output$childmort_table <- DT::renderDT({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!is.null(result$error) || is.null(result$estimates)) {
          return(NULL)
        }
        current_gid <- shared$active_group_id()
        est <- result$estimates[result$estimates$.id == current_gid, , drop = FALSE]
        if (nrow(est) == 0) {
          return(NULL)
        }
        tbl <- data.frame(
          est$age_label,
          est$q_label,
          round(est$qx, 4),
          round(est$t_years, 2),
          round(est$date_t, 1),
          round(est$e0_implied, 1),
          stringsAsFactors = FALSE
        )
        names(tbl) <- c(
          i18n$t("Age group of women"),
          i18n$t("Estimate"),
          i18n$t("Probability of dying"),
          i18n$t("Years before observation"),
          i18n$t("Reference date"),
          i18n$t("Implied life expectancy at birth")
        )
        DT::datatable(
          tbl,
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE, pageLength = 7)
        )
      })

      output$childmort_diagnostics <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!is.null(result$error) || is.null(result$estimates)) {
          return(NULL)
        }
        current_gid <- shared$active_group_id()
        est <- result$estimates[result$estimates$.id == current_gid, , drop = FALSE]
        notes <- childmort_diagnostics(est)
        failed_groups <- if (!is.null(result$group_errors)) nrow(result$group_errors) else 0
        if (length(notes) == 0 && failed_groups == 0) {
          return(NULL)
        }
        shiny::div(
          class = "ui info message childmort-diagnostics",
          shiny::div(class = "header", i18n$t("Diagnostics")),
          shiny::tags$ul(
            lapply(notes, function(note) shiny::tags$li(i18n$t(note))),
            if (failed_groups > 0) {
              shiny::tags$li(sprintf(
                i18n$t("%d group(s) could not be estimated; see the errors file in the all-groups download."),
                failed_groups
              ))
            }
          )
        )
      })

      # Show plot and download button
      shinyjs::show(id = session$ns("plot_container"))
      if (is.null(result$error)) {
        shinyjs::show(id = session$ns("download_container"))
      }
    },

    register_downloads = NULL
  ))
}
