#' Intercensal Survival Module UI
#'
#' Two-step module (data upload, then analysis) estimating adult mortality
#' from the survival of cohorts between two censuses.
#'
#' @param i18n Translator helper used for labels.
#' @noRd
intercensal_module_ui <- function(i18n) {
  ns <- shiny::NS("intercensal")

  shinyjs::hidden(
    shiny::div(
      id = "intercensal_module_page",
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .intercensal-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }
          .intercensal-step {
            width: 100%;
          }
          .intercensal-step.main-content {
            display: block;
          }
          .intercensal-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .intercensal-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .intercensal-hero p {
            margin: 0;
            color: #425466;
            max-width: 100%;
          }
          .intercensal-sample-picker {
            display: flex;
            align-items: center;
            gap: 10px;
          }
          .intercensal-sample-picker .ui.form {
            min-width: 300px;
          }
          .intercensal-sample-picker .field {
            margin: 0 !important;
          }
          .intercensal-inline {
            display: flex;
            gap: 10px;
            align-items: flex-end;
          }
          .intercensal-inline > div {
            flex: 1;
            min-width: 0;
          }
          .intercensal-date-title {
            font-weight: 700;
            margin: 0.6rem 0 0.2rem;
          }
          .intercensal-hint {
            font-size: 0.9em;
            color: #666;
            margin-top: 0.3em;
          }
          .intercensal-results {
            margin-top: 1.5rem;
          }
          .intercensal-summary {
            margin-bottom: 1rem;
          }
          .intercensal-diagnostics {
            margin-top: 1.5rem;
          }
          .intercensal-diagnostics ul {
            padding-left: 1.2rem;
            margin: 0.5rem 0 0;
          }
          .intercensal-diagnostics li {
            margin-bottom: 0.4rem;
            color: #425466;
          }
          .intercensal-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
        "))
      ),
      shiny::div(
        class = "intercensal-main",

        # STEP 1: Data Upload
        shiny::div(
          id = ns("data_step"),
          class = "intercensal-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "intercensal-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("intercensal_back_button"))
            ),
            shiny::uiOutput(ns("intercensal_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = create_info_tab_label(ns("info_tab_upload_label"), i18n$t("Upload Instructions")),
                  content = shiny::div(
                    shiny::uiOutput(ns("intercensal_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("intercensal_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("intercensal_info_box_instructions"))
                  )
                ),
                list(
                  menu = create_info_tab_label(ns("info_tab_docs_label"), i18n$t("Method Documentation")),
                  content = shiny::div(
                    shiny::uiOutput(ns("intercensal_method_docs"))
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
            shiny::uiOutput(ns("intercensal_sample_button"))
          ),
          shiny::div(
            class = "intercensal-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("intercensal_continue_ui"))
          )
        ),

        # STEP 2: Analysis
        shiny::div(
          id = ns("analysis_step"),
          class = "intercensal-step",
          style = "display: none;",
          shiny::div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
            shiny::uiOutput(ns("intercensal_back_to_upload_button"))
          ),
          untheme::sidebar_layout_responsive(
            list(
              children = shiny::div(
                class = "ui form",
                shiny::uiOutput(ns("intercensal_controls")),
                shiny::br(),
                shiny::uiOutput(ns("intercensal_run_button"))
              )
            ),
            shiny::div(
              style = "flex: 1; min-width: 0;",
              shiny::uiOutput(ns("intercensal_group_selectors")),
              shiny::div(
                id = ns("plot_container"),
                style = "display: none;",
                shiny::uiOutput(ns("intercensal_run_settings")),
                shiny::div(
                  class = "intercensal-summary",
                  DT::DTOutput(ns("intercensal_summary_table"))
                ),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("intercensal_plot"), height = "480px", width = "100%")
                ),
                shiny::div(
                  class = "intercensal-results",
                  DT::DTOutput(ns("intercensal_table"))
                ),
                shiny::uiOutput(ns("intercensal_diagnostics"))
              ),
              shiny::br(),
              shinyjs::hidden(
                shiny::div(
                  id = ns("download_container"),
                  class = "button-container",
                  style = "justify-content: flex-end; margin-top: 1rem;",
                  shiny::uiOutput(ns("intercensal_download_button"))
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
#' @noRd
intercensal_error_plot <- function(message, i18n) {
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


#' Analysis info text written into intercensal survival downloads
#'
#' @param params Parameters used in the run (`run_intercensal()$params`).
#' @param n_groups Number of groups included, or `NULL` for a single group.
#' @noRd
intercensal_info_text <- function(params, n_groups = NULL) {
  items <- intercensal_settings_items(params)
  paste(
    c(
      if (is.null(n_groups)) {
        "Module: Intercensal Survival Analysis"
      } else {
        "Module: Intercensal Survival Analysis (All Groups)"
      },
      "Description: Indirect estimation of adult mortality from the survival of cohorts between two censuses (UN Manual X, 1983, chapter VIII), using MortIndEst::intercensal_survival(). Each method finds the model life table level implied at each age; the median level over the ages used for the fit gives the fitted life table, its life expectancy at birth, 45q15 and 35q15.",
      "",
      paste0(items$label, ": ", items$value),
      if (!is.null(params$hiv)) "HIV prevalence and ART coverage are proportions; they are passed to the Spectrum (SVD-comp) models as percentages.",
      "",
      "Files:",
      "- intercensal_summary.csv: one row per model life table family. median_e0 is the selected level; mean_e0, p25_e0, p75_e0 and coef_var describe the levels implied by the ages used (ages_used). e0, q15_45 (45q15) and q15_35 (35q15) come from the fitted life table. reference_e0 is the starting level of the Feeney and Preston logit methods. clamped counts implied levels set to the limits of the accepted e0 range.",
      "- intercensal_by_age.csv: populations (Pop1_moved is the first census moved by the overall growth rate so that the interval is a multiple of five years), the survival ratio and the implied e0 by age, and whether the age was used for the fit.",
      "- intercensal_life_tables.csv: the fitted abridged life table of each family.",
      "",
      sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      if (!is.null(n_groups)) sprintf("Number of Groups: %d", n_groups)
    ),
    collapse = "\n"
  )
}


#' Optional numeric control value: `NULL` when blank
#' @noRd
intercensal_optional <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || is.na(x)) NULL else x
}


#' Census date set in the controls, as a decimal year
#'
#' `NULL` while the controls are not rendered, `NA` for an impossible date
#' (e.g. 30 February), so the run can say the date is not valid.
#' @noRd
intercensal_control_date <- function(year, month, day) {
  if (is.null(year) || is.null(month) || is.null(day)) {
    return(NULL)
  }
  # Not rounded here, so the settings show the date entered; the estimation
  # rounds it to two decimals, as intercensal_survival() does with text dates
  tryCatch(intercensal_decimal_date(year, month, day), error = function(e) NA_real_)
}


#' Control values that apply to an intercensal survival run on the given data
#'
#' Sex and dates are `NULL` when the data carry them; the reference e0 only
#' applies to the Feeney and Preston logit methods.
#'
#' @param input Module input.
#' @param data Uploaded data (any column names).
#' @return Named list of control values, in the form of `run_intercensal()`
#'   parameters (plus `hiv_on`, `q_given`, `hiv_prev` and `hiv_art`).
#' @noRd
intercensal_control_values <- function(input, data) {
  prepared <- if (is.null(data)) data.frame() else prepare_intercensal_input(data)
  has_dates <- all(c("Date1", "Date2") %in% names(prepared))
  has_sex <- "Sex" %in% names(prepared)
  method <- input$method
  e0_min <- intercensal_optional(input$e0_min)
  e0_max <- intercensal_optional(input$e0_max)
  q0 <- intercensal_optional(input$q0)
  q5 <- intercensal_optional(input$q5)
  list(
    method = method,
    family = input$mlt_family,
    sex = if (has_sex) NULL else input$sex,
    date1 = if (has_dates) NULL else intercensal_control_date(input$date1_year, input$date1_month, input$date1_day),
    date2 = if (has_dates) NULL else intercensal_control_date(input$date2_year, input$date2_month, input$date2_day),
    ages_fit = if (is.null(input$ages_fit)) NULL else sort(as.numeric(input$ages_fit)),
    e0_accept = if (is.null(e0_min) || is.null(e0_max)) NULL else c(e0_min, e0_max),
    ref_e0 = if (!is.null(method) && method %in% INTERCENSAL_REFERENCE_METHODS) intercensal_optional(input$ref_e0) else NULL,
    q0 = q0,
    q5 = q5,
    # An empty numeric field is NULL too, so whether the controls exist is
    # judged from the method, which is always set once they are rendered
    q_given = if (is.null(method)) NULL else !is.null(q0) && !is.null(q5),
    hiv_on = input$hiv,
    hiv_prev = intercensal_optional(input$hiv_prev),
    hiv_art = intercensal_optional(input$hiv_art)
  )
}


#' Intercensal Survival Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
intercensal_module_server <- function(input, output, session) {

  mod_simple_module_server("intercensal", list(
    setup = function(input, output, session, i18n) {
      sample_files <- c(
        panama = "intercensal_sample_panama.csv",
        japan = "intercensal_sample_japan.csv"
      )
      read_sample <- function(choice) {
        file <- sample_files[[choice %||% "panama"]] %||% sample_files[["panama"]]
        path <- system.file("extdata", file, package = "lifetableprojection")
        df <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
        df <- as.data.frame(df)
        for (col in c("Age", "Pop1", "Pop2")) df[[col]] <- as.numeric(df[[col]])
        df[order(df$Age), , drop = FALSE]
      }
      intercensal_sample_loader <- function() {
        read_sample(input$sample_choice)
      }

      shared <- create_shared_data_context(
        "intercensal",
        input, output, session, i18n,
        sample_loader = intercensal_sample_loader,
        validation_function = validateData_intercensal
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")
      render_translated_validation(output, session, i18n, shared, INTERCENSAL_MESSAGE_TEMPLATES)

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")
      family_labels <- function() {
        stats::setNames(i18n$t(unname(INTERCENSAL_FAMILY_LABELS)), names(INTERCENSAL_FAMILY_LABELS))
      }

      output$intercensal_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("intercensal_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$intercensal_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Adult Mortality from Intercensal Survival")),
          shiny::p(i18n$t("Estimate adult mortality by comparing the population of each age group in one census with the population ten (or n) years older in the next."))
        )
      })

      output$intercensal_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$intercensal_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::p(shiny::strong(i18n$t("Required columns:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Age (lower bound of five-year age groups starting at 0: 0, 5, 10, ...; the last group is open, e.g. 75 for 75+)")),
            shiny::tags$li(i18n$t("Pop1 and Pop2 (population of each age group in the first and second census)"))
          ),
          shiny::p(shiny::strong(i18n$t("Optional columns:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Date1 and Date2 (census dates, e.g. 1960-12-11, or decimal years). If absent, set them in the analysis step.")),
            shiny::tags$li(i18n$t("Sex (female or male). With both sexes in one file, group the data by Sex."))
          ),
          shiny::p(shiny::tags$em(i18n$t("Additional grouping columns are supported for batch analysis."))),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("Sample data: Panama females 1960-1970 (UN Manual X, Table 174) and Japan females 1960-1970 (Feeney 2002)."))
        )
      })

      output$intercensal_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        method_item <- function(name, text) {
          shiny::div(class = "item", shiny::div(class = "content",
            shiny::div(class = "header", i18n$t(name)),
            shiny::div(class = "description", i18n$t(text))
          ))
        }
        shiny::tagList(
          shiny::h3(i18n$t("Intercensal Survival Methods")),
          shiny::p(i18n$t("People aged x in the first census are aged x + n in a second census taken n years later, so the ratio of the two counts measures survival between the censuses. Each method converts these ratios into the level of a model life table at each age; the median of those levels over the ages used for the fit gives a complete life table, its life expectancy at birth and the adult mortality indicators 45q15 and 35q15.")),
          shiny::div(
            class = "ui relaxed divided list",
            method_item("Survival ratios", "Matches each survival ratio to the model life table level with the same ratio (UN Manual X, chapter VIII)."),
            method_item("Back projection", "Projects the second census back to the date of the first with each model life table level, and finds the level that reproduces the population above each age."),
            method_item("Forward projection", "Projects the first census forward to the date of the second, in the same way."),
            method_item("Variable-r (Preston and Bennett)", "Builds a life table from the age-specific growth rates between the censuses and matches its life expectancy at each age (UN Manual X, Table 186)."),
            method_item("Feeney", "Turns the survival ratios into a survival function and matches the life expectancy at each age; it needs a reference level for the oldest ages (Feeney 2002)."),
            method_item("Preston logit", "Fits a one-parameter logit relation between the data and a reference model life table, using the growth rates and the 5q0 of that table (Preston, Heuveline and Guillot 2001).")
          ),
          shiny::h4(i18n$t("Reading the results")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("The chart shows the level (life expectancy at birth) implied by each age group; filled points are the ages used for the fit and the dashed line is the selected level, their median.")),
            shiny::tags$li(i18n$t("Levels that change steadily with age, or survival ratios above 1, point to age misreporting, changes in census coverage or migration. Leave the affected ages out of the fit, as UN Manual X does for Panama.")),
            shiny::tags$li(i18n$t("The method assumes a population closed to migration and the same census coverage in both censuses. The first census is moved with the overall growth rate so that the interval is a multiple of five years.")),
            shiny::tags$li(i18n$t("With the Spectrum HIV model life tables, the age pattern of mortality comes from the HIV prevalence, ART coverage and 5q0 you give, instead of a model life table family."))
          ),
          shiny::h4(i18n$t("References")),
          shiny::tags$ul(
            shiny::tags$li("United Nations (1983). Manual X: Indirect Techniques for Demographic Estimation. Population Studies No. 81. New York: United Nations."),
            shiny::tags$li("Preston, S. H. and Bennett, N. G. (1983). A census-based method for estimating adult mortality. Population Studies 37(1): 91-104."),
            shiny::tags$li("Preston, S. H., Heuveline, P. and Guillot, M. (2001). Demography: Measuring and Modeling Population Processes. Oxford: Blackwell."),
            shiny::tags$li("Feeney, G. (2002). Estimating mortality from intercensal survival. United Nations, ESA/P/WP.175."),
            shiny::tags$li("Stover, J. et al. (2012). The Spectrum projection package: improvements in estimating mortality, ART needs, PMTCT impact and uncertainty bounds. Sexually Transmitted Infections 88: i11-i16.")
          )
        )
      })

      output$intercensal_sample_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        choices <- stats::setNames(
          c("panama", "japan"),
          c(i18n$t("Panama females 1960-1970 (UN Manual X)"), i18n$t("Japan females 1960-1970 (Feeney)"))
        )
        shiny::div(
          class = "intercensal-sample-picker",
          shiny.semantic::selectInput(
            ns("sample_choice"),
            NULL,
            choices = choices,
            selected = shiny::isolate(input$sample_choice) %||% "panama"
          ),
          shiny.semantic::action_button(
            ns("use_sample_data"),
            i18n$t("Use sample data"),
            class = "ui blue button"
          )
        )
      })

      sample_preview <- read_sample("panama")
      output$intercensal_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      output$intercensal_continue_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        req(shared$group_selection_passed())
        details <- tryCatch(shared$validation_details(), error = function(e) NULL)
        if (!is.null(details) && is.data.frame(details) && !all(details$pass == "Pass")) {
          return(NULL)
        }
        shiny::actionButton(ns("go_to_analysis"), i18n$t("Continue"), class = "ui blue button")
      })

      output$intercensal_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "intercensal_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$intercensal_controls <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        df <- shared$data()
        prepared <- if (is.null(df)) data.frame() else prepare_intercensal_input(df)
        has_dates <- all(c("Date1", "Date2") %in% names(prepared))
        has_sex <- "Sex" %in% names(prepared)
        ages <- sort(unique(intercensal_num(prepared$Age)))
        fit_choices <- if (length(ages) > 1) ages[-length(ages)] else ages
        fit_selected <- shiny::isolate(input$ages_fit)
        if (is.null(fit_selected) || !all(as.numeric(fit_selected) %in% fit_choices)) {
          fit_selected <- intersect(INTERCENSAL_DEFAULT_AGES_FIT, fit_choices)
        }
        months <- c("January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December")
        date_inputs <- function(which, title, default) {
          shiny::tagList(
            shiny::div(class = "intercensal-date-title", i18n$t(title)),
            shiny::div(
              class = "intercensal-inline",
              shiny::numericInput(
                ns(paste0(which, "_year")), i18n$t("Year"),
                value = shiny::isolate(input[[paste0(which, "_year")]]) %||% default[1],
                min = 1800, max = 2100, step = 1
              ),
              shiny.semantic::selectInput(
                ns(paste0(which, "_month")), i18n$t("Month"),
                choices = stats::setNames(as.character(1:12), i18n$t(months)),
                selected = shiny::isolate(input[[paste0(which, "_month")]]) %||% as.character(default[2])
              ),
              shiny::numericInput(
                ns(paste0(which, "_day")), i18n$t("Day"),
                value = shiny::isolate(input[[paste0(which, "_day")]]) %||% default[3],
                min = 1, max = 31, step = 1
              )
            )
          )
        }

        shiny::tagList(
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(`for` = ns("method"), i18n$t("Method")),
              shiny.fluent::TooltipHost(
                content = shiny::uiOutput(ns("method_tooltip")),
                delay = 0,
                shiny.fluent::Image(src = "www/info.png", width = "20px", shouldStartVisible = TRUE)
              )
            ),
            shiny.semantic::selectInput(
              ns("method"),
              NULL,
              choices = stats::setNames(names(INTERCENSAL_METHODS), i18n$t(unname(INTERCENSAL_METHODS))),
              selected = shiny::isolate(input$method) %||% "match"
            )
          ),
          shiny::uiOutput(ns("intercensal_family_ui")),
          if (has_sex) {
            shiny::p(i18n$t("Sex will be taken from your data"))
          } else {
            shiny.semantic::selectInput(
              ns("sex"),
              i18n$t("Sex"),
              choices = stats::setNames(names(INTERCENSAL_SEX_LABELS), i18n$t(unname(INTERCENSAL_SEX_LABELS))),
              selected = shiny::isolate(input$sex) %||% "female"
            )
          },
          if (has_dates) {
            shiny::p(i18n$t("Census dates will be taken from your data"))
          } else {
            shiny::tagList(
              date_inputs("date1", "Date of the first census", c(2000, 7, 1)),
              date_inputs("date2", "Date of the second census", c(2010, 7, 1))
            )
          },
          shiny.semantic::selectInput(
            ns("ages_fit"),
            i18n$t("Ages used for the fit"),
            choices = stats::setNames(as.character(fit_choices), paste0(fit_choices, "-", fit_choices + 4)),
            selected = as.character(fit_selected),
            multiple = TRUE
          ),
          shiny::p(class = "intercensal-hint", i18n$t("The selected level is the median of the levels implied by these age groups.")),
          shiny::div(
            class = "intercensal-inline",
            shiny::numericInput(
              ns("e0_min"), i18n$t("Lowest accepted e0"),
              value = shiny::isolate(input$e0_min) %||% INTERCENSAL_DEFAULT_E0_ACCEPT[1], min = 15, max = 100, step = 1
            ),
            shiny::numericInput(
              ns("e0_max"), i18n$t("Highest accepted e0"),
              value = shiny::isolate(input$e0_max) %||% INTERCENSAL_DEFAULT_E0_ACCEPT[2], min = 20, max = 115, step = 1
            )
          ),
          shiny::uiOutput(ns("intercensal_reference_ui")),
          shiny::div(class = "intercensal-date-title", i18n$t("Child mortality (optional)")),
          shiny::div(
            class = "intercensal-inline",
            shiny::numericInput(ns("q0"), "1q0", value = shiny::isolate(input$q0) %||% NA, min = 0, max = 1, step = 0.001),
            shiny::numericInput(ns("q5"), "5q0", value = shiny::isolate(input$q5) %||% NA, min = 0, max = 1, step = 0.001)
          ),
          shiny::p(class = "intercensal-hint", i18n$t("Replace the first ages of the fitted life table. Required for the Spectrum HIV life tables.")),
          shiny.semantic::checkbox_input(
            ns("hiv"),
            i18n$t("Use the Spectrum HIV model life tables"),
            is_marked = isTRUE(shiny::isolate(input$hiv))
          ),
          shiny::uiOutput(ns("intercensal_hiv_ui"))
        )
      })
      outputOptions(output, "intercensal_controls", suspendWhenHidden = FALSE)

      # The family is replaced by the Spectrum tables when HIV is used
      output$intercensal_family_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (isTRUE(input$hiv)) {
          return(shiny::p(i18n$t("Model life tables: Spectrum HIV model life tables")))
        }
        labels <- family_labels()
        shiny.semantic::selectInput(
          ns("mlt_family"),
          i18n$t("Model life table family"),
          choices = stats::setNames(
            c(INTERCENSAL_FAMILIES, "all"),
            c(unname(labels[INTERCENSAL_FAMILIES]), i18n$t("All families"))
          ),
          selected = shiny::isolate(input$mlt_family) %||% "CD_West"
        )
      })
      outputOptions(output, "intercensal_family_ui", suspendWhenHidden = FALSE)

      # Reference level for the Feeney and Preston logit methods
      output$intercensal_reference_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!isTRUE(input$method %in% INTERCENSAL_REFERENCE_METHODS)) {
          return(NULL)
        }
        shiny::tagList(
          shiny::numericInput(
            ns("ref_e0"), i18n$t("Reference e0 (optional)"),
            value = shiny::isolate(input$ref_e0) %||% NA, min = 20, max = 100, step = 2.5
          ),
          shiny::p(class = "intercensal-hint", i18n$t("Level of the model life table the method starts from, rounded to 2.5 years. Leave blank to use the level found by the survival-ratio method."))
        )
      })
      outputOptions(output, "intercensal_reference_ui", suspendWhenHidden = FALSE)

      output$intercensal_hiv_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!isTRUE(input$hiv)) {
          return(NULL)
        }
        shiny::tagList(
          shiny::div(
            class = "intercensal-inline",
            shiny::numericInput(
              ns("hiv_prev"), i18n$t("HIV prevalence (proportion)"),
              value = shiny::isolate(input$hiv_prev) %||% 0.05, min = 0, max = 1, step = 0.01
            ),
            shiny::numericInput(
              ns("hiv_art"), i18n$t("ART coverage (proportion)"),
              value = shiny::isolate(input$hiv_art) %||% 0, min = 0, max = 1, step = 0.01
            )
          ),
          shiny::p(class = "intercensal-hint", i18n$t("Adult HIV prevalence and the share of people living with HIV on treatment, between 0 and 1 (e.g. 0.05 for 5%)."))
        )
      })
      outputOptions(output, "intercensal_hiv_ui", suspendWhenHidden = FALSE)

      output$intercensal_run_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("run_analysis"), i18n$t("Estimate Adult Mortality"), class = "ui primary button")
      })
      outputOptions(output, "intercensal_run_button", suspendWhenHidden = FALSE)

      output$method_tooltip <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::HTML(i18n$t("How the populations of the two censuses are turned into a mortality level:<br><br>'Survival ratios' - matches each survival ratio to a model life table<br>'Back / Forward projection' - projects one census to the date of the other<br>'Variable-r' - uses the age-specific growth rates<br>'Feeney' and 'Preston logit' - start from a reference model life table level"))
      })

      # Settings behind the shown results, and a note when the controls differ
      output$intercensal_run_settings <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        latest <- shared$last_result()
        if (is.null(latest) || is.null(latest$params)) {
          return(NULL)
        }
        items <- intercensal_settings_items(latest$params)
        parts <- vapply(seq_len(nrow(items)), function(i) {
          value <- if (items$translate_value[i]) i18n$t(items$value[i]) else items$value[i]
          paste0(i18n$t(items$label[i]), ": ", value)
        }, character(1))
        changed <- intercensal_settings_changed(
          latest$params,
          intercensal_control_values(input, shared$data())
        )
        shiny::tagList(
          shiny::div(
            class = "ui small message intercensal-run-settings",
            shiny::tags$b(paste0(i18n$t("Results for"), ": ")),
            paste(parts, collapse = " · ")
          ),
          if (changed) {
            shiny::div(
              class = "ui warning message intercensal-settings-changed",
              i18n$t("The settings have changed. Click 'Estimate Adult Mortality' to update the results.")
            )
          }
        )
      })

      output$intercensal_group_selectors <- shiny::renderUI({
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
            lapply(dropdowns, function(dropdown) shiny::div(style = "min-width: 150px", dropdown))
          )
        }
      })
      outputOptions(output, "intercensal_group_selectors", suspendWhenHidden = FALSE)

      output$intercensal_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        df <- shared$data()
        is_grouped <- !is.null(df) && ".id" %in% names(df) && length(unique(df$.id)) > 1
        if (is_grouped) {
          shiny::actionButton(ns("open_download_modal"), i18n$t("Download results"), class = "ui primary button")
        } else {
          shiny::downloadButton(ns("download_intercensal_csv"), i18n$t("Download results"), class = "ui primary button")
        }
      })
      outputOptions(output, "intercensal_download_button", suspendWhenHidden = FALSE)

      output$download_modal_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny.semantic::modal(
          id = ns("download_modal"),
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
                    id = "intercensal_radio_single",
                    shiny::tags$input(type = "radio", name = "intercensal_download_scope_radio", value = "single", checked = "checked"),
                    shiny::tags$label(i18n$t("Current group only"))
                  )
                ),
                shiny::div(
                  class = "field",
                  shiny::div(
                    class = "ui radio checkbox",
                    id = "intercensal_radio_all",
                    shiny::tags$input(type = "radio", name = "intercensal_download_scope_radio", value = "all"),
                    shiny::tags$label(i18n$t("All groups"))
                  )
                )
              ),
              shiny::tags$input(id = ns("download_scope"), type = "hidden", value = "single")
            ),
            shiny::br(),
            shiny::p(
              style = "font-size: 0.9em; color: #666;",
              i18n$t("Note: Downloading all groups creates a combined ZIP file with the results of every group.")
            ),
            shiny::tags$script(shiny::HTML(sprintf("
              $(document).ready(function() {
                $('.ui.radio.checkbox').checkbox();
                $('input[name=\"intercensal_download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"intercensal_download_scope_radio\"]:checked').val();
                  $('#%s').val(selectedValue).trigger('change');
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

      shiny::observeEvent(input$open_download_modal, {
        shiny.semantic::show_modal(ns("download_modal"), session = session)
      })
      shiny::observeEvent(input$cancel_download, {
        shiny.semantic::hide_modal(ns("download_modal"), session = session)
      })
      shiny::observeEvent(input$download_scope_js, {
        download_scope_choice(input$download_scope_js)
      })

      group_label_for <- function(gid) {
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        label <- NULL
        if (!is.null(lbl_df)) {
          label <- lbl_df$.id_label[lbl_df$.id == gid]
        }
        if (length(label) == 0 || is.na(label[1])) as.character(gid) else as.character(label[1])
      }

      # Add group labels to an output table
      add_labels <- function(out_data) {
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        if (!is.null(lbl_df) && ".id" %in% names(out_data)) {
          out_data$.id_label <- NULL
          out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE, sort = FALSE)
        }
        out_data
      }

      plot_for <- function(by_age, summary, title) {
        intercensal_plot(
          by_age, summary,
          title = title,
          x_label = i18n$t("Age"),
          y_label = i18n$t("Implied life expectancy at birth"),
          hover_labels = list(
            ratio = i18n$t("Survival ratio"), implied = i18n$t("Implied e0"),
            used = i18n$t("Used for the fit"), not_used = i18n$t("Not used for the fit")
          ),
          legend_labels = list(
            used = i18n$t("Used for the fit"), not_used = i18n$t("Not used for the fit"),
            selected = i18n$t("Selected level (median)")
          ),
          family_labels = family_labels()
        )
      }

      write_group_files <- function(latest, ids, temp_dir, suffix) {
        pick <- function(tbl) if (is.null(tbl)) NULL else tbl[tbl$.id %in% ids, , drop = FALSE]
        files <- c()
        tables <- list(
          intercensal_summary = pick(latest$summary),
          intercensal_by_age = pick(latest$by_age),
          intercensal_life_tables = pick(latest$lt)
        )
        for (name in names(tables)) {
          f <- file.path(temp_dir, sprintf("%s%s.csv", name, suffix))
          utils::write.csv(add_labels(tables[[name]]), f, row.names = FALSE)
          files <- c(files, f)
        }
        for (gid in ids) {
          by_age <- latest$by_age[latest$by_age$.id == gid, , drop = FALSE]
          if (nrow(by_age) == 0) next
          summary <- latest$summary[latest$summary$.id == gid, , drop = FALSE]
          name <- if (length(ids) > 1) {
            sprintf("intercensal_plot_%s.png", gsub("[^a-zA-Z0-9_-]", "_", group_label_for(gid)))
          } else {
            "intercensal_plot.png"
          }
          f <- file.path(temp_dir, name)
          title <- i18n$t("Levels implied by each age group")
          if (length(ids) > 1) title <- paste0(title, " - ", group_label_for(gid))
          ggplot2::ggsave(f, plot = plot_for(by_age, summary, title), width = 10, height = 6, dpi = 300)
          files <- c(files, f)
        }
        files
      }

      download_single_group <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest, latest$summary)
        gid <- shared$active_group_id()
        shiny::req(any(latest$summary$.id == gid))
        temp_dir <- tempfile()
        dir.create(temp_dir)
        files <- write_group_files(latest, gid, temp_dir, "")
        info_file <- file.path(temp_dir, "analysis_info.txt")
        writeLines(intercensal_info_text(latest$params), info_file)
        files <- c(files, info_file)
        zip::zip(zipfile = file, files = basename(files), root = temp_dir)
        unlink(temp_dir, recursive = TRUE)
      }

      download_all_groups <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest, latest$summary)
        temp_dir <- tempfile()
        dir.create(temp_dir)
        files <- write_group_files(latest, unique(latest$summary$.id), temp_dir, "_all_groups")
        if (!is.null(latest$group_errors) && nrow(latest$group_errors) > 0) {
          err_file <- file.path(temp_dir, "intercensal_errors.csv")
          utils::write.csv(latest$group_errors, err_file, row.names = FALSE)
          files <- c(files, err_file)
        }
        info_file <- file.path(temp_dir, "analysis_info.txt")
        n_groups <- length(unique(latest$summary$.id)) + nrow(latest$group_errors %||% data.frame())
        writeLines(intercensal_info_text(latest$params, n_groups = n_groups), info_file)
        files <- c(files, info_file)
        zip::zip(zipfile = file, files = basename(files), root = temp_dir)
        unlink(temp_dir, recursive = TRUE)
      }

      output$download_intercensal_csv <- shiny::downloadHandler(
        filename = function() sprintf("intercensal_results_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
        content = download_single_group,
        contentType = "application/zip"
      )

      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          if (download_scope_choice() == "all") {
            sprintf("intercensal_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("intercensal_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          shiny.semantic::hide_modal(ns("download_modal"), session = session)
          if (download_scope_choice() == "all") download_all_groups(file) else download_single_group(file)
        },
        contentType = "application/zip"
      )

      session$onFlushed(function() {
        shinyjs::hide(id = analysis_step_id)
        shinyjs::hide(id = ns("download_container"))
      }, once = TRUE)

      reset_results <- function() {
        shared$last_result(NULL)
        output$intercensal_plot <- plotly::renderPlotly(NULL)
        output$intercensal_table <- DT::renderDT(NULL)
        output$intercensal_summary_table <- DT::renderDT(NULL)
        output$intercensal_diagnostics <- shiny::renderUI(NULL)
        shinyjs::hide(id = ns("plot_container"))
        shinyjs::hide(id = ns("download_container"))
      }

      shiny::observeEvent(shared$data(), {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        reset_results()
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        if (shared$data_origin() %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          reset_results()
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      shiny::observeEvent(input$go_to_analysis, {
        shinyjs::hide(id = data_step_id)
        shinyjs::show(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
      values <- intercensal_control_values(input, shiny::isolate(shared$data()))
      list(
        method = values$method %||% "match",
        family = values$family %||% "CD_West",
        sex = values$sex %||% "female",
        date1 = values$date1,
        date2 = values$date2,
        ages_fit = values$ages_fit %||% INTERCENSAL_DEFAULT_AGES_FIT,
        e0_accept = values$e0_accept %||% INTERCENSAL_DEFAULT_E0_ACCEPT,
        ref_e0 = values$ref_e0,
        q0 = values$q0,
        q5 = values$q5,
        hiv = if (isTRUE(values$hiv_on)) list(prev = values$hiv_prev, art = values$hiv_art) else NULL
      )
    },

    run = function(shared, params, input, i18n) {
      data_full <- shiny::isolate(shared$data())
      if (is.null(data_full) || nrow(data_full) == 0) {
        return(list(error = i18n$t("No data available")))
      }
      if (any(is.na(c(params$date1, params$date2)))) {
        return(list(error = i18n$t("The census dates are not valid dates")))
      }
      if (!is.null(params$hiv) && (is.null(params$hiv$prev) || is.null(params$hiv$art) ||
                                     params$hiv$prev < 0 || params$hiv$prev > 1 ||
                                     params$hiv$art < 0 || params$hiv$art > 1)) {
        return(list(error = i18n$t("HIV prevalence and ART coverage must be proportions between 0 and 1")))
      }
      result <- tryCatch(
        run_intercensal(
          data_full,
          method = params$method,
          family = params$family,
          sex = params$sex,
          date1 = params$date1,
          date2 = params$date2,
          ages_fit = params$ages_fit,
          e0_accept = params$e0_accept,
          ref_e0 = params$ref_e0,
          q0 = params$q0,
          q5 = params$q5,
          hiv = params$hiv
        ),
        error = function(e) {
          list(error = paste0(
            i18n$t("Estimation failed: "),
            translate_messages(conditionMessage(e), i18n, INTERCENSAL_MESSAGE_TEMPLATES)
          ))
        }
      )
      shared$last_result(if (is.null(result$error)) result else NULL)
      result
    },

    render = function(result, output, shared, input, i18n) {
      # Called inside each output so the labels follow the selected language
      family_label_set <- function() {
        stats::setNames(i18n$t(unname(INTERCENSAL_FAMILY_LABELS)), names(INTERCENSAL_FAMILY_LABELS))
      }
      group_rows <- function(tbl) {
        if (is.null(tbl)) return(NULL)
        tbl[tbl$.id == shared$active_group_id(), , drop = FALSE]
      }

      output$intercensal_plot <- plotly::renderPlotly({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!is.null(result$error)) {
          return(intercensal_error_plot(result$error, i18n))
        }
        current_gid <- shared$active_group_id()
        group_error <- result$group_errors$message[result$group_errors$.id == as.character(current_gid)]
        if (length(group_error) > 0) {
          return(intercensal_error_plot(translate_messages(group_error[1], i18n, INTERCENSAL_MESSAGE_TEMPLATES), i18n))
        }
        by_age <- group_rows(result$by_age)
        summary <- group_rows(result$summary)
        if (is.null(by_age) || nrow(by_age) == 0) {
          return(NULL)
        }
        plot_obj <- intercensal_plot(
          by_age, summary,
          title = i18n$t("Levels implied by each age group"),
          x_label = i18n$t("Age"),
          y_label = i18n$t("Implied life expectancy at birth"),
          hover_labels = list(
            ratio = i18n$t("Survival ratio"), implied = i18n$t("Implied e0"),
            used = i18n$t("Used for the fit"), not_used = i18n$t("Not used for the fit")
          ),
          legend_labels = list(
            used = i18n$t("Used for the fit"), not_used = i18n$t("Not used for the fit"),
            selected = i18n$t("Selected level (median)")
          ),
          family_labels = family_label_set()
        )
        intercensal_plotly(plot_obj)
      })

      output$intercensal_summary_table <- DT::renderDT({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        summary <- group_rows(result$summary)
        if (!is.null(result$error) || is.null(summary) || nrow(summary) == 0) {
          return(NULL)
        }
        tbl <- data.frame(
          unname(family_label_set()[summary$family]),
          round(summary$e0, 1),
          round(summary$q15_45, 4),
          round(summary$q15_35, 4),
          round(summary$median_e0, 1),
          round(summary$mean_e0, 1),
          paste0(round(summary$p25_e0, 1), " - ", round(summary$p75_e0, 1)),
          round(summary$coef_var, 3),
          summary$ages_used,
          stringsAsFactors = FALSE
        )
        names(tbl) <- c(
          i18n$t("Model life table family"),
          i18n$t("Life expectancy at birth"),
          "45q15", "35q15",
          i18n$t("Selected level (median)"),
          i18n$t("Mean level"),
          i18n$t("Interquartile range"),
          i18n$t("Coefficient of variation"),
          i18n$t("Ages used")
        )
        if (any(!is.na(summary$reference_e0))) {
          tbl[[i18n$t("Reference e0")]] <- summary$reference_e0
        }
        DT::datatable(
          tbl,
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE, pageLength = nrow(tbl), scrollX = TRUE)
        )
      })

      output$intercensal_table <- DT::renderDT({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        by_age <- group_rows(result$by_age)
        if (!is.null(result$error) || is.null(by_age) || nrow(by_age) == 0) {
          return(NULL)
        }
        tbl <- data.frame(
          by_age$age_label,
          round(by_age$Pop1),
          round(by_age$Pop2),
          round(by_age$survival_ratio, 4),
          round(by_age$implied_e0, 1),
          ifelse(by_age$used, i18n$t("Yes"), i18n$t("No")),
          stringsAsFactors = FALSE
        )
        names(tbl) <- c(
          i18n$t("Age group"),
          i18n$t("First census"),
          i18n$t("Second census"),
          i18n$t("Survival ratio"),
          i18n$t("Implied e0"),
          i18n$t("Used for the fit")
        )
        several <- length(unique(by_age$family)) > 1
        if (several) {
          tbl <- cbind(stats::setNames(data.frame(unname(family_label_set()[by_age$family])), i18n$t("Model life table family")), tbl)
        }
        n_ages <- length(unique(by_age$Age))
        DT::datatable(
          tbl,
          rownames = FALSE,
          options = list(
            dom = if (several) "tp" else "t", ordering = FALSE,
            pageLength = n_ages, scrollX = TRUE,
            language = list(paginate = list(previous = i18n$t("Previous"), `next` = i18n$t("Next")))
          )
        )
      })

      output$intercensal_diagnostics <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        if (!is.null(result$error)) {
          return(NULL)
        }
        notes <- group_rows(result$notes)
        notes <- if (is.null(notes)) character(0) else notes$note
        failed_groups <- if (!is.null(result$group_errors)) nrow(result$group_errors) else 0
        if (length(notes) == 0 && failed_groups == 0) {
          return(NULL)
        }
        shiny::div(
          class = "ui info message intercensal-diagnostics",
          shiny::div(class = "header", i18n$t("Diagnostics")),
          shiny::tags$ul(
            lapply(notes, function(note) {
              shiny::tags$li(intercensal_translate_note(note, i18n))
            }),
            if (failed_groups > 0) {
              shiny::tags$li(sprintf(
                i18n$t("%d group(s) could not be estimated; see the errors file in the all-groups download."),
                failed_groups
              ))
            }
          )
        )
      })

      shinyjs::show(id = session$ns("plot_container"))
      if (is.null(result$error)) {
        shinyjs::show(id = session$ns("download_container"))
      }
    },

    register_downloads = NULL
  ))
}
