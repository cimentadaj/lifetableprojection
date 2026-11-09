#' Adult Mortality from Orphanhood Module UI
#'
#' Two-step module (data upload, then analysis) estimating adult mortality
#' (45q15) from the proportion of respondents whose mother or father is
#' alive, in one census or survey or in two.
#'
#' @param i18n Translator helper used for labels.
#' @noRd
orphanhood_module_ui <- function(i18n) {
  ns <- shiny::NS("orphanhood")

  shinyjs::hidden(
    shiny::div(
      id = "orphanhood_module_page",
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .orphanhood-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }
          .orphanhood-step {
            width: 100%;
          }
          .orphanhood-step.main-content {
            display: block;
          }
          .orphanhood-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .orphanhood-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .orphanhood-hero p {
            margin: 0;
            color: #425466;
            max-width: 100%;
          }
          .orphanhood-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
          .orphanhood-sample-picker {
            display: flex;
            align-items: center;
            gap: 10px;
          }
          .orphanhood-sample-picker .ui.form {
            min-width: 280px;
          }
          .orphanhood-sample-picker .field {
            margin: 0 !important;
          }
          .orphanhood-results {
            margin-top: 1.5rem;
          }
          .orphanhood-diagnostics {
            margin-top: 1.5rem;
          }
          .orphanhood-diagnostics ul {
            padding-left: 1.2rem;
            margin: 0.5rem 0 0;
          }
          .orphanhood-diagnostics li {
            margin-bottom: 0.4rem;
            color: #425466;
          }
          .orphanhood-control-hint {
            font-size: 0.9em;
            color: #666;
            margin-top: 0.4em;
          }
          .orphanhood-data-note {
            color: #425466;
            margin: 0.5em 0 1em;
          }
          .orphanhood-hiv-panel {
            border-top: 1px solid #e2e8f0;
            margin-top: 1rem;
            padding-top: 0.75rem;
          }
          .orphanhood-standard-table {
            margin: 0.5em 0 1em;
          }
        "))
      ),
      shiny::div(
        class = "orphanhood-main",

        # STEP 1: Data Upload
        shiny::div(
          id = ns("data_step"),
          class = "orphanhood-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "orphanhood-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("orphanhood_back_button"))
            ),
            shiny::uiOutput(ns("orphanhood_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = create_info_tab_label(ns("info_tab_upload_label"), i18n$t("Upload Instructions")),
                  content = shiny::div(
                    shiny::uiOutput(ns("orphanhood_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("orphanhood_sample_table"), width = "100%", height = 230),
                    shiny::uiOutput(ns("orphanhood_info_box_instructions"))
                  )
                ),
                list(
                  menu = create_info_tab_label(ns("info_tab_docs_label"), i18n$t("Method Documentation")),
                  content = shiny::div(
                    shiny::uiOutput(ns("orphanhood_method_docs"))
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "button-container-file",
            style = "display: flex; gap: 10px; padding: 0 1rem; flex-wrap: wrap;",
            shiny::div(
              id = ns("file_input"),
              shiny.semantic::file_input(
                ns("file1"),
                label = "",
                type = "flex-override"
              )
            ),
            shiny::uiOutput(ns("modal_ui")),
            shiny::uiOutput(ns("orphanhood_sample_button"))
          ),
          shiny::div(
            class = "orphanhood-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("orphanhood_continue_ui"))
          )
        ),

        # STEP 2: Analysis
        shiny::div(
          id = ns("analysis_step"),
          class = "orphanhood-step",
          style = "display: none;",
          shiny::div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
            shiny::uiOutput(ns("orphanhood_back_to_upload_button"))
          ),
          untheme::sidebar_layout_responsive(
            # LEFT SIDEBAR: Controls
            list(
              children = shiny::div(
                class = "ui form",
                shiny::uiOutput(ns("orphanhood_controls")),
                shiny::uiOutput(ns("orphanhood_model_standard_ui")),
                # The custom standard table is not re-rendered with the
                # controls, so the values typed in it are kept.
                shiny::conditionalPanel(
                  condition = "input.standard_type == 'custom'",
                  ns = ns,
                  shiny::uiOutput(ns("orphanhood_custom_standard_hint")),
                  shiny::div(
                    class = "orphanhood-standard-table",
                    rhandsontable::rHandsontableOutput(ns("custom_standard"), width = "100%", height = 380)
                  )
                ),
                shiny::uiOutput(ns("orphanhood_mac_ui")),
                shiny::uiOutput(ns("orphanhood_dates_ui")),
                shiny::uiOutput(ns("orphanhood_hiv_ui")),
                # HIV series of the Masquelier-Timaeus method; static for the
                # same reason as the custom standard table
                shiny::conditionalPanel(
                  condition = "output.two_observations && input.method == 'masquelier_timaeus'",
                  ns = ns,
                  shiny::div(
                    class = "orphanhood-hiv-panel",
                    shiny::uiOutput(ns("orphanhood_hiv_series_header")),
                    shiny::div(
                      class = "orphanhood-standard-table",
                      rhandsontable::rHandsontableOutput(ns("hiv_series"), width = "100%", height = 420)
                    ),
                    shiny::uiOutput(ns("orphanhood_hiv_period_ui"))
                  )
                ),
                shiny::br(),
                shiny::uiOutput(ns("orphanhood_run_button"))
              )
            ),
            # RIGHT MAIN: Plot, table and diagnostics
            shiny::div(
              style = "flex: 1; min-width: 0;",
              shiny::uiOutput(ns("orphanhood_group_selectors")),
              shiny::div(
                id = ns("plot_container"),
                style = "display: none;",
                shiny::uiOutput(ns("orphanhood_run_settings")),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("orphanhood_plot"), height = "520px", width = "100%")
                ),
                shiny::div(
                  class = "orphanhood-results",
                  DT::DTOutput(ns("orphanhood_table"))
                ),
                shiny::uiOutput(ns("orphanhood_diagnostics"))
              ),
              shiny::br(),
              shinyjs::hidden(
                shiny::div(
                  id = ns("download_container"),
                  class = "button-container",
                  style = "justify-content: flex-end; margin-top: 1rem;",
                  shiny::uiOutput(ns("orphanhood_download_button"))
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


#' Analysis info text written into orphanhood downloads
#'
#' @param params Parameters used in the run (`run_orphanhood()$params`).
#' @param n_groups Number of groups included, or `NULL` for a single group.
#' @noRd
orphanhood_info_text <- function(params, n_groups = NULL) {
  items <- orphanhood_settings_items(params)
  two <- identical(as.integer(params$observations), 2L)
  paste(
    c(
      if (is.null(n_groups)) {
        "Module: Adult Mortality from Orphanhood"
      } else {
        "Module: Adult Mortality from Orphanhood (All Groups)"
      },
      if (two) {
        "Description: Indirect estimation of adult mortality from the proportions of respondents with a living parent in two censuses or surveys, using MortIndEst::orphanhood_two()."
      } else {
        "Description: Indirect estimation of adult mortality from the proportion of respondents with a living parent in one census or survey (Timaeus 1992, IUSSP Tools for Demographic Estimation), using MortIndEst::orphanhood_one()."
      },
      "Fitting: one-parameter Brass logit against the standard life table (orphanhood_standard.csv).",
      "",
      sprintf("%s: %s", items$label, items$value),
      if (isTRUE(params$masquelier)) {
        c(
          "HIV/AIDS adjustment: Masquelier and Timaeus, with the HIV prevalence, ART and PMTCT series in orphanhood_hiv_series.csv (the same series for every group).",
          "orphanhood_hiv_adjustment.csv: proportions not orphaned before and after the adjustment, HIV prevalence at the respondents' birth, and p25_n (conditional maternal survival from 25 to 25 + n)."
        )
      },
      if (isTRUE(params$hiv)) {
        c(
          sprintf("Proportion of children infected by their mothers: %s", format(params$hiv_vert_transm)),
          sprintf("Fertility of HIV-positive relative to HIV-negative women: %s", format(params$hiv_fert_ratio)),
          if (!isTRUE(params$maternal)) {
            sprintf("Proportion of HIV-positive men whose partner is infected: %s", format(params$hiv_prop_partner))
          }
        )
      },
      "",
      "Output columns:",
      "- Age / age_label: five-year age group of respondents",
      "- q15_45: probability of dying between exact ages 15 and 60 (45q15)",
      "- q15_35: probability of dying between exact ages 15 and 50 (35q15)",
      "- ref_date: date the estimate refers to (decimal year)",
      if (!two) "- years_before: years before the observation date the estimate refers to",
      if (two) "- years_between: years between the two observations",
      "- plausible: FALSE when the estimate falls outside 0-1 (left out of the chart)",
      "Life table file: age_mlt and lx, the fitted survivorship of the parents for each age group of respondents.",
      "",
      sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      if (!is.null(n_groups)) sprintf("Number of Groups: %d", n_groups),
      "",
      "Note: estimates from respondents aged 5-9 and 10-14 often understate mortality (adoption effect)."
    ),
    collapse = "\n"
  )
}


#' Control values that apply to a run on the given data
#'
#' Controls that the data override (a MAC or date column) or that do not apply
#' (the two-census method for one observation) are returned as `NULL`, so the
#' settings summary shows what the run really used.
#'
#' @param input Module input.
#' @param data Uploaded data (any column names).
#' @param hiv_series Current HIV series table (Masquelier-Timaeus).
#' @return Named list of control values.
#' @noRd
orphanhood_control_values <- function(input, data, hiv_series = NULL) {
  prepared <- if (is.null(data)) data.frame() else prepare_orphanhood_input(data)
  has <- names(prepared)
  obs <- orphanhood_observations(prepared)
  mac_from_data <- "MAC" %in% has || (obs == 2 && all(c("MAC1", "MAC2") %in% has))
  hiv <- obs == 1 && "HIVPrev" %in% has
  model <- !identical(input$standard_type, "custom")
  by_q5 <- identical(input$std_level, "q5")
  masquelier <- obs == 2 && identical(input$method, "masquelier_timaeus")
  list(
    parent = input$parent,
    method = if (obs == 2) input$method else NULL,
    standard_type = input$standard_type,
    family = if (model) input$family else NULL,
    std_level = if (model) input$std_level else NULL,
    std_e0 = if (model && !by_q5) input$std_e0 else NULL,
    std_q5 = if (model && by_q5) input$std_q5 else NULL,
    hiv_series = if (masquelier) hiv_series else NULL,
    hiv_period = if (masquelier) input$hiv_period else NULL,
    mac = if (mac_from_data) NULL else input$mac,
    date_obs = if (obs == 1 && !"Date" %in% has) input$date_obs else NULL,
    date1 = if (obs == 2 && !"Date1" %in% has) input$date1 else NULL,
    date2 = if (obs == 2 && !"Date2" %in% has) input$date2 else NULL,
    hiv_vert_transm = if (hiv) input$hiv_vert_transm else NULL,
    hiv_fert_ratio = if (hiv) input$hiv_fert_ratio else NULL,
    hiv_prop_partner = if (hiv && identical(input$parent, "paternal")) input$hiv_prop_partner else NULL
  )
}


#' Adult Mortality from Orphanhood Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
orphanhood_module_server <- function(input, output, session) {

  mod_simple_module_server("orphanhood", list(
    setup = function(input, output, session, i18n) {
      sample_files <- c(
        one = "orphanhood_sample.csv",
        two = "orphanhood_sample_two.csv",
        hiv = "orphanhood_sample_hiv.csv",
        sa = "orphanhood_sample_sa.csv"
      )
      read_sample <- function(choice) {
        file <- sample_files[[choice %||% "one"]] %||% sample_files[["one"]]
        path <- system.file("extdata", file, package = "lifetableprojection")
        df <- readr::read_csv(path, show_col_types = FALSE)
        as.data.frame(df[order(df$Age), , drop = FALSE])
      }
      # The sample picked in the dropdown next to the sample button
      orphanhood_sample_loader <- function() {
        read_sample(input$sample_choice)
      }

      shared <- create_shared_data_context(
        "orphanhood",
        input, output, session, i18n,
        sample_loader = orphanhood_sample_loader,
        validation_function = validateData_orphanhood
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      language_dep <- function() {
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
      }

      # What the uploaded data provide
      data_info <- shiny::reactive({
        df <- shared$data()
        if (is.null(df)) {
          return(list(obs = 1L, has = character(0)))
        }
        df <- prepare_orphanhood_input(df)
        list(obs = orphanhood_observations(df), has = names(df))
      })
      data_has <- function(cols) all(cols %in% data_info()$has)

      # Language-reactive UI outputs (Step 1)
      output$orphanhood_back_button <- shiny::renderUI({
        language_dep()
        shiny::actionButton("orphanhood_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$orphanhood_hero_content <- shiny::renderUI({
        language_dep()
        shiny::tagList(
          shiny::h1(i18n$t("Adult Mortality from Orphanhood")),
          shiny::p(i18n$t("Estimate adult mortality (45q15) from the proportion of respondents whose mother or father is still alive, reported in one census or survey or in two."))
        )
      })

      output$orphanhood_info_box_content <- shiny::renderUI({
        language_dep()
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$orphanhood_info_box_instructions <- shiny::renderUI({
        language_dep()
        shiny::tagList(
          shiny::p(shiny::strong(i18n$t("Required columns (one census or survey):"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Age (lower bound of the respondents' five-year age groups, e.g. 5, 10, ..., 45)")),
            shiny::tags$li(i18n$t("NotOrphaned (proportion of respondents whose mother, or father, is alive, between 0 and 1)"))
          ),
          shiny::p(shiny::strong(i18n$t("Two censuses or surveys:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("NotOrphaned1 and NotOrphaned2 instead of NotOrphaned, one per observation"))
          ),
          shiny::p(shiny::strong(i18n$t("Optional columns:"))),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Date, or Date1 and Date2 (dates of the observations as decimal years, e.g. 1997.8). If absent, set them in the analysis step.")),
            shiny::tags$li(i18n$t("MAC (mean age of mothers, or fathers, at the birth of their children): one value, or one per age group. If absent, set it in the analysis step.")),
            shiny::tags$li(i18n$t("HIVPrev (one census or survey only): HIV prevalence at the time of the respondents' birth, by age group, among pregnant women (maternal) or men (paternal). Turns on the HIV/AIDS adjustment."))
          ),
          shiny::p(i18n$t("For the Masquelier-Timaeus method (two observations with HIV), the HIV prevalence, ART and PMTCT series by calendar year are entered in the analysis step.")),
          shiny::p(shiny::tags$em(i18n$t("Additional grouping columns are supported for batch analysis."))),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("Sample data: Iraq 1997 survey, Panama 1977 and 1980 censuses, Kenya 1999 census with HIV prevalence (IUSSP Tools for Demographic Estimation), and South Africa 2007 and 2016 surveys with HIV series (Masquelier and Timaeus)."))
        )
      })

      output$orphanhood_method_docs <- shiny::renderUI({
        language_dep()
        shiny::tagList(
          shiny::h3(i18n$t("Orphanhood Method")),
          shiny::p(i18n$t("The proportion of respondents whose mother (or father) is still alive measures the survival of parents from the birth of their children to the date of the census or survey. The orphanhood method converts these proportions, together with the mean age of parents at the birth of their children, into conditional probabilities of survival in adulthood, and then into 45q15, the probability of dying between exact ages 15 and 60.")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", i18n$t("One census or survey")),
              shiny::div(class = "description", i18n$t("Timaeus (1992) regression coefficients, as in the IUSSP Tools for Demographic Estimation. Each age group of respondents gives an estimate that refers to a date a number of years before the census or survey."))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", i18n$t("Two censuses or surveys")),
              shiny::div(class = "description", i18n$t("Synthetic-cohort methods that use the change in the proportions not orphaned between the two observations: Timaeus (default), Preston-Chen, or Masquelier-Timaeus for populations affected by HIV/AIDS. The estimates describe mortality between the two observations."))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", i18n$t("Standard life table")),
              shiny::div(class = "description", i18n$t("The survival probabilities are turned into a life table with a one-parameter Brass logit fit against a standard: a model life table of the chosen family and level (female tables for mothers, male tables for fathers), or a standard you enter."))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", i18n$t("HIV/AIDS adjustment")),
              shiny::div(class = "description", i18n$t("Where HIV is common, orphans are more likely to be infected and to die, so surviving respondents overstate parental survival. With one census or survey and an HIVPrev column, the proportions are adjusted following Timaeus and Nunn (1997). With two, the Masquelier-Timaeus method adjusts them using the HIV prevalence, ART and PMTCT series by calendar year, which also account for prevention of mother-to-child transmission and treatment."))
            ))
          ),
          shiny::h4(i18n$t("Reading the results")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Each point is an estimate of 45q15 from one age group of respondents. With one census or survey, it is placed at the date it refers to: estimates from older respondents refer to earlier dates.")),
            shiny::tags$li(i18n$t("The mean age at childbearing locates the estimates in time and adjusts their level; use the mean age of mothers (or fathers) at the births of the respondents' cohorts.")),
            shiny::tags$li(i18n$t("Give less weight to estimates from respondents aged 5-9 and 10-14: orphans are often adopted and reported as having their parent alive, which makes mortality look lower.")),
            shiny::tags$li(i18n$t("Paternal orphanhood gives estimates for men; they are less reliable than the maternal ones."))
          ),
          shiny::h4(i18n$t("References")),
          # Citations are kept in their original language.
          shiny::tags$ul(
            class = "orphanhood-references",
            shiny::tags$li("Moultrie, T. A. et al. (eds.) (2013). Tools for Demographic Estimation. Paris: International Union for the Scientific Study of Population."),
            shiny::tags$li("Timaeus, I. M. (1992). Estimation of adult mortality from paternal orphanhood: a reassessment and a new approach. Population Bulletin of the United Nations 33: 47-63."),
            shiny::tags$li("Timaeus, I. M. (1991). Estimation of mortality from orphanhood in adulthood. Demography 28(2): 213-227."),
            shiny::tags$li("Preston, S. H. and Chen, N. (1984). Two-census orphanhood methods for estimating adult mortality, with applications to Latin America. Population Studies 38(1): 1-23."),
            shiny::tags$li("Timaeus, I. M. and Nunn, A. J. (1997). Measurement of adult mortality in populations affected by AIDS: an assessment of the orphanhood method. Health Transition Review 7(Suppl. 2): 23-43."),
            # As cited in MortIndEst::orphanhood_two()
            shiny::tags$li("Masquelier, B. and Timaeus, I. M. HIV/AIDS correction method for maternal orphanhood in two inquiries.")
          )
        )
      })

      output$orphanhood_sample_button <- shiny::renderUI({
        language_dep()
        choices <- stats::setNames(
          c("one", "two", "hiv", "sa"),
          c(
            i18n$t("One survey: Iraq 1997"),
            i18n$t("Two censuses: Panama 1977-1980"),
            i18n$t("One census with HIV: Kenya 1999"),
            i18n$t("Two surveys with HIV: South Africa 2007-2016")
          )
        )
        shiny::div(
          class = "orphanhood-sample-picker",
          shiny.semantic::selectInput(
            ns("sample_choice"),
            NULL,
            choices = choices,
            selected = shiny::isolate(input$sample_choice) %||% "one"
          ),
          shiny.semantic::action_button(
            ns("use_sample_data"),
            i18n$t("Use sample data"),
            class = "ui blue button"
          )
        )
      })

      # Sample data preview table (one census or survey)
      sample_preview <- read_sample("one")
      output$orphanhood_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$orphanhood_continue_ui <- shiny::renderUI({
        language_dep()
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
      output$orphanhood_back_to_upload_button <- shiny::renderUI({
        language_dep()
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "orphanhood_back_to_upload_button", suspendWhenHidden = FALSE)

      # Parent, two-census method and kind of standard
      output$orphanhood_controls <- shiny::renderUI({
        language_dep()
        two <- data_info()$obs == 2
        shiny::tagList(
          shiny.semantic::selectInput(
            ns("parent"),
            i18n$t("Parent"),
            choices = stats::setNames(
              c("maternal", "paternal"),
              c(i18n$t("Mother (maternal orphanhood)"), i18n$t("Father (paternal orphanhood)"))
            ),
            selected = shiny::isolate(input$parent) %||% "maternal"
          ),
          if (two) {
            shiny::div(
              shiny::div(
                style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
                shiny::tags$label(`for` = ns("method"), i18n$t("Two-census method")),
                shiny.fluent::TooltipHost(
                  content = shiny::uiOutput(ns("method_tooltip")),
                  delay = 0,
                  shiny.fluent::Image(src = "www/info.png", width = "20px", shouldStartVisible = TRUE)
                )
              ),
              shiny.semantic::selectInput(
                ns("method"),
                NULL,
                choices = stats::setNames(
                  ORPHANHOOD_METHODS,
                  c("Timaeus", "Preston-Chen", i18n$t("Masquelier-Timaeus (HIV/AIDS)"))
                ),
                selected = shiny::isolate(input$method) %||% "timaeus"
              )
            )
          },
          shiny.semantic::selectInput(
            ns("standard_type"),
            i18n$t("Standard life table"),
            choices = stats::setNames(
              c("model", "custom"),
              c(i18n$t("Model life table"), i18n$t("Custom standard (enter lx)"))
            ),
            selected = shiny::isolate(input$standard_type) %||% "model"
          )
        )
      })
      outputOptions(output, "orphanhood_controls", suspendWhenHidden = FALSE)

      output$method_tooltip <- shiny::renderUI({
        language_dep()
        shiny::HTML(i18n$t("How the two observations are combined:<br><br>'Timaeus' - converts the change in the proportions not orphaned with Timaeus coefficients; needs respondents aged 15-19 and 20-24<br>'Preston-Chen' - adjusts the average proportions with the growth rates between the observations, then applies the one-census method<br>'Masquelier-Timaeus' - adjusts both proportions for HIV/AIDS, PMTCT and ART with the HIV series you enter; maternal orphanhood only"))
      })

      # Family and level of a model life table standard
      output$orphanhood_model_standard_ui <- shiny::renderUI({
        language_dep()
        if (identical(input$standard_type, "custom")) {
          return(NULL)
        }
        shiny::tagList(
          shiny.semantic::selectInput(
            ns("family"),
            i18n$t("Model life table family"),
            choices = stats::setNames(ORPHANHOOD_FAMILIES, unname(ORPHANHOOD_FAMILY_LABELS[ORPHANHOOD_FAMILIES])),
            selected = shiny::isolate(input$family) %||% "CD_West"
          ),
          shiny.semantic::selectInput(
            ns("std_level"),
            i18n$t("Level of the standard"),
            choices = stats::setNames(
              c("e0", "q5"),
              c(i18n$t("Life expectancy at birth (e0)"), i18n$t("Child mortality (5q0)"))
            ),
            selected = shiny::isolate(input$std_level) %||% "e0"
          ),
          shiny::conditionalPanel(
            condition = "input.std_level != 'q5'",
            ns = ns,
            shiny::numericInput(
              ns("std_e0"),
              i18n$t("Life expectancy at birth of the standard"),
              value = shiny::isolate(input$std_e0) %||% 60,
              min = 20, max = 100, step = 2.5
            )
          ),
          shiny::conditionalPanel(
            condition = "input.std_level == 'q5'",
            ns = ns,
            shiny::numericInput(
              ns("std_q5"),
              i18n$t("5q0 of the standard"),
              value = shiny::isolate(input$std_q5) %||% 0.1,
              min = 0.001, max = 0.5, step = 0.005
            ),
            shiny::p(
              class = "orphanhood-control-hint",
              i18n$t("The model life table level with the closest 5q0 is used.")
            )
          ),
          shiny::p(
            class = "orphanhood-control-hint",
            i18n$t("Female tables are used for mothers and male tables for fathers. The level is rounded to the nearest 2.5 years.")
          )
        )
      })
      outputOptions(output, "orphanhood_model_standard_ui", suspendWhenHidden = FALSE)

      # Custom standard: survivors out of 1 at ages 15 to 85
      output$orphanhood_custom_standard_hint <- shiny::renderUI({
        language_dep()
        shiny::p(
          class = "orphanhood-control-hint",
          i18n$t("Type or paste lx (survivors out of 1, e.g. 0.8868) for ages 15 to 85 of the parents.")
        )
      })
      outputOptions(output, "orphanhood_custom_standard_hint", suspendWhenHidden = FALSE)

      output$custom_standard <- rhandsontable::renderRHandsontable({
        blank <- data.frame(Age = ORPHANHOOD_STANDARD_AGES, lx = rep(NA_real_, length(ORPHANHOOD_STANDARD_AGES)))
        rhandsontable::rhandsontable(blank, rowHeaders = NULL, stretchH = "all") |>
          rhandsontable::hot_col("Age", readOnly = TRUE, format = "0") |>
          rhandsontable::hot_col("lx", type = "numeric", format = "0.0000")
      })
      outputOptions(output, "custom_standard", suspendWhenHidden = FALSE)

      # Masquelier-Timaeus HIV series: filled from the South Africa sample,
      # emptied when other data are loaded
      hiv_series_start <- shiny::reactiveVal(orphanhood_blank_hiv_series())
      read_sa_series <- function() {
        path <- system.file("extdata", "orphanhood_sample_sa_hiv_series.csv", package = "lifetableprojection")
        sa <- utils::read.csv(path)
        rbind(sa, orphanhood_blank_hiv_series(5))
      }
      shiny::observeEvent(shared$data(), {
        sa_sample <- identical(shiny::isolate(shared$data_origin()), "sample") &&
          identical(shiny::isolate(input$sample_choice), "sa")
        hiv_series_start(if (sa_sample) read_sa_series() else orphanhood_blank_hiv_series())
      }, ignoreNULL = TRUE)

      output$two_observations <- shiny::reactive(data_info()$obs == 2)
      outputOptions(output, "two_observations", suspendWhenHidden = FALSE)

      output$orphanhood_hiv_series_header <- shiny::renderUI({
        language_dep()
        shiny::tagList(
          shiny::h4(i18n$t("HIV series (Masquelier-Timaeus)")),
          shiny::p(
            class = "orphanhood-control-hint",
            i18n$t("By calendar year: HIV prevalence among women aged 15-49, and the ART and PMTCT coverage, as proportions. Leave a cell blank when there is no value; values between years are interpolated. The same series is used for every group.")
          )
        )
      })
      outputOptions(output, "orphanhood_hiv_series_header", suspendWhenHidden = FALSE)

      output$hiv_series <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(hiv_series_start(), rowHeaders = NULL, stretchH = "all") |>
          rhandsontable::hot_col("Year", type = "numeric", format = "0.000") |>
          rhandsontable::hot_col(ORPHANHOOD_HIV_SERIES, type = "numeric", format = "0.000")
      })
      outputOptions(output, "hiv_series", suspendWhenHidden = FALSE)

      hiv_series_value <- function() {
        tbl <- tryCatch(rhandsontable::hot_to_r(input$hiv_series), error = function(e) NULL)
        if (is.null(tbl)) {
          tbl <- shiny::isolate(hiv_series_start())
        }
        tbl <- as.data.frame(tbl)
        tbl[] <- lapply(tbl, function(x) suppressWarnings(as.numeric(x)))
        tbl
      }
      shared$hiv_series_value <- hiv_series_value

      output$orphanhood_hiv_period_ui <- shiny::renderUI({
        language_dep()
        shiny.semantic::selectInput(
          ns("hiv_period"),
          i18n$t("Period HIV and ART covariates"),
          choices = stats::setNames(
            c("midpoint", "average"),
            c(i18n$t("Midpoint"), i18n$t("Average of the two dates"))
          ),
          selected = shiny::isolate(input$hiv_period) %||% "midpoint"
        )
      })
      outputOptions(output, "orphanhood_hiv_period_ui", suspendWhenHidden = FALSE)

      custom_standard_value <- function() {
        tbl <- tryCatch(rhandsontable::hot_to_r(input$custom_standard), error = function(e) NULL)
        if (is.null(tbl)) {
          tbl <- data.frame(Age = ORPHANHOOD_STANDARD_AGES, lx = NA_real_)
        }
        data.frame(age = tbl$Age, lx = suppressWarnings(as.numeric(tbl$lx)))
      }

      # Mean age at childbearing: the default depends on the parent
      output$orphanhood_mac_ui <- shiny::renderUI({
        language_dep()
        from_data <- data_has("MAC") || (data_info()$obs == 2 && data_has(c("MAC1", "MAC2")))
        if (from_data) {
          return(shiny::p(class = "orphanhood-data-note", i18n$t("Mean age at childbearing will be taken from your data")))
        }
        default <- if (identical(input$parent, "paternal")) 32 else 27
        shiny::numericInput(
          ns("mac"),
          if (identical(input$parent, "paternal")) i18n$t("Mean age of fathers at childbearing") else i18n$t("Mean age of mothers at childbearing"),
          value = default,
          min = 15, max = 50, step = 0.1
        )
      })
      outputOptions(output, "orphanhood_mac_ui", suspendWhenHidden = FALSE)

      # Dates of observation, unless the data carry them
      output$orphanhood_dates_ui <- shiny::renderUI({
        language_dep()
        if (data_info()$obs == 2) {
          if (data_has(c("Date1", "Date2"))) {
            return(shiny::p(class = "orphanhood-data-note", i18n$t("Date values will be taken from your data")))
          }
          return(shiny::tagList(
            if (!data_has("Date1")) {
              shiny::numericInput(
                ns("date1"), i18n$t("Date of the first observation (decimal year)"),
                value = shiny::isolate(input$date1) %||% 2010, min = 1900, max = 2100, step = 0.1
              )
            },
            if (!data_has("Date2")) {
              shiny::numericInput(
                ns("date2"), i18n$t("Date of the second observation (decimal year)"),
                value = shiny::isolate(input$date2) %||% 2020, min = 1900, max = 2100, step = 0.1
              )
            }
          ))
        }
        if (data_has("Date")) {
          return(shiny::p(class = "orphanhood-data-note", i18n$t("Date values will be taken from your data")))
        }
        shiny::numericInput(
          ns("date_obs"),
          i18n$t("Date of observation (decimal year)"),
          value = shiny::isolate(input$date_obs) %||% 2020,
          min = 1900, max = 2100, step = 0.1
        )
      })
      outputOptions(output, "orphanhood_dates_ui", suspendWhenHidden = FALSE)

      # HIV/AIDS adjustment: one census or survey with an HIVPrev column
      output$orphanhood_hiv_ui <- shiny::renderUI({
        language_dep()
        if (data_info()$obs != 1 || !data_has("HIVPrev")) {
          return(NULL)
        }
        paternal <- identical(input$parent, "paternal")
        shiny::div(
          class = "orphanhood-hiv-panel",
          shiny::h4(i18n$t("HIV/AIDS adjustment")),
          shiny::p(
            class = "orphanhood-control-hint",
            i18n$t("Your data include HIVPrev, so the proportions not orphaned are adjusted for HIV/AIDS (Timaeus and Nunn 1997).")
          ),
          shiny::numericInput(
            ns("hiv_vert_transm"),
            i18n$t("Proportion of children infected by their mothers"),
            value = shiny::isolate(input$hiv_vert_transm) %||% 0.33, min = 0, max = 1, step = 0.01
          ),
          shiny::numericInput(
            ns("hiv_fert_ratio"),
            i18n$t("Fertility of HIV-positive relative to HIV-negative women"),
            value = shiny::isolate(input$hiv_fert_ratio) %||% 0.75, min = 0.01, max = 2, step = 0.01
          ),
          if (paternal) {
            shiny::numericInput(
              ns("hiv_prop_partner"),
              i18n$t("Proportion of HIV-positive men whose partner is infected"),
              value = shiny::isolate(input$hiv_prop_partner) %||% 0.5, min = 0, max = 1, step = 0.01
            )
          }
        )
      })
      outputOptions(output, "orphanhood_hiv_ui", suspendWhenHidden = FALSE)

      output$orphanhood_run_button <- shiny::renderUI({
        language_dep()
        shiny::actionButton(
          ns("run_analysis"),
          i18n$t("Estimate Adult Mortality"),
          class = "ui primary button"
        )
      })
      outputOptions(output, "orphanhood_run_button", suspendWhenHidden = FALSE)

      # Settings behind the shown results, and a note when the controls differ
      output$orphanhood_run_settings <- shiny::renderUI({
        language_dep()
        latest <- shared$last_result()
        if (is.null(latest) || is.null(latest$params)) {
          return(NULL)
        }
        items <- orphanhood_settings_items(latest$params)
        parts <- vapply(seq_len(nrow(items)), function(i) {
          value <- if (items$translate_value[i]) i18n$t(items$value[i]) else items$value[i]
          paste0(i18n$t(items$label[i]), ": ", value)
        }, character(1))
        changed <- orphanhood_settings_changed(
          latest$params,
          orphanhood_control_values(input, shared$data(), hiv_series_value())
        )
        shiny::tagList(
          shiny::div(
            class = "ui small message orphanhood-run-settings",
            shiny::tags$b(paste0(i18n$t("Results for"), ": ")),
            paste(parts, collapse = " · ")
          ),
          if (changed) {
            shiny::div(
              class = "ui warning message orphanhood-settings-changed",
              i18n$t("The settings have changed. Click 'Estimate Adult Mortality' to update the results.")
            )
          }
        )
      })

      # Group selectors
      output$orphanhood_group_selectors <- shiny::renderUI({
        language_dep()
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
      outputOptions(output, "orphanhood_group_selectors", suspendWhenHidden = FALSE)

      # Download button
      output$orphanhood_download_button <- shiny::renderUI({
        language_dep()
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
            ns("download_orphanhood_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "orphanhood_download_button", suspendWhenHidden = FALSE)

      # Download options modal
      output$download_modal_ui <- shiny::renderUI({
        language_dep()
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
                    id = "orphanhood_radio_single",
                    shiny::tags$input(
                      type = "radio",
                      name = "orphanhood_download_scope_radio",
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
                    id = "orphanhood_radio_all",
                    shiny::tags$input(
                      type = "radio",
                      name = "orphanhood_download_scope_radio",
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

                $('input[name=\"orphanhood_download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"orphanhood_download_scope_radio\"]:checked').val();
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
        sort_cols <- intersect(c(".id", "Age", "age_mlt"), names(out_data))
        out_data[do.call(order, unname(as.list(out_data[sort_cols]))), , drop = FALSE]
      }

      plot_labels <- function(observations) {
        list(
          title = i18n$t("Adult mortality estimates"),
          x_label = if (identical(as.integer(observations), 2L)) {
            i18n$t("Age group of respondents")
          } else {
            i18n$t("Reference date")
          },
          y_label = i18n$t("Probability of dying between ages 15 and 60, 45q15")
        )
      }

      # Save the plot of one group to a PNG file
      save_group_plot <- function(estimates, plot_file, gid, observations) {
        labels <- plot_labels(observations)
        plot_obj <- orphanhood_plot(
          estimates,
          observations = observations,
          title = paste0(labels$title, " - ", group_label_for(gid)),
          x_label = labels$x_label,
          y_label = labels$y_label
        )
        ggplot2::ggsave(plot_file, plot = plot_obj, width = 10, height = 6, dpi = 300)
      }

      write_common_files <- function(latest, temp_dir, estimates, lx, suffix, gid = NULL) {
        files <- c()
        if (!is.null(latest$hiv_adjustment)) {
          adj <- latest$hiv_adjustment
          if (!is.null(gid)) {
            adj <- adj[adj$.id == gid, , drop = FALSE]
          }
          adj_file <- file.path(temp_dir, sprintf("orphanhood_hiv_adjustment%s.csv", suffix))
          utils::write.csv(add_labels(adj), adj_file, row.names = FALSE)
          series_file <- file.path(temp_dir, "orphanhood_hiv_series.csv")
          utils::write.csv(latest$params$hiv_series, series_file, row.names = FALSE, na = "")
          files <- c(files, adj_file, series_file)
        }
        csv_file <- file.path(temp_dir, sprintf("orphanhood_results%s.csv", suffix))
        utils::write.csv(add_labels(estimates), csv_file, row.names = FALSE)
        files <- c(files, csv_file)
        lx_file <- file.path(temp_dir, sprintf("orphanhood_life_tables%s.csv", suffix))
        utils::write.csv(add_labels(lx), lx_file, row.names = FALSE)
        files <- c(files, lx_file)
        std_file <- file.path(temp_dir, "orphanhood_standard.csv")
        utils::write.csv(latest$standard, std_file, row.names = FALSE)
        c(files, std_file)
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
        lx <- latest$lx[latest$lx$.id == gid, , drop = FALSE]

        temp_dir <- tempfile()
        dir.create(temp_dir)
        temp_files <- write_common_files(latest, temp_dir, estimates, lx, "", gid = gid)

        plot_file <- file.path(temp_dir, "orphanhood_plot.png")
        save_group_plot(estimates, plot_file, gid, latest$params$observations)
        temp_files <- c(temp_files, plot_file)

        info_file <- file.path(temp_dir, "analysis_info.txt")
        writeLines(orphanhood_info_text(latest$params), info_file)
        temp_files <- c(temp_files, info_file)

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
        temp_files <- write_common_files(latest, temp_dir, estimates, latest$lx, "_all_groups")

        if (!is.null(latest$group_errors) && nrow(latest$group_errors) > 0) {
          err_file <- file.path(temp_dir, "orphanhood_errors.csv")
          utils::write.csv(latest$group_errors, err_file, row.names = FALSE)
          temp_files <- c(temp_files, err_file)
        }

        for (gid in unique(estimates$.id)) {
          safe_label <- gsub("[^a-zA-Z0-9_-]", "_", group_label_for(gid))
          plot_file <- file.path(temp_dir, sprintf("orphanhood_plot_%s.png", safe_label))
          save_group_plot(estimates[estimates$.id == gid, , drop = FALSE], plot_file, gid,
                          latest$params$observations)
          temp_files <- c(temp_files, plot_file)
        }

        info_file <- file.path(temp_dir, "analysis_info.txt")
        n_groups <- length(unique(estimates$.id)) + nrow(latest$group_errors %||% data.frame())
        writeLines(orphanhood_info_text(latest$params, n_groups = n_groups), info_file)
        temp_files <- c(temp_files, info_file)

        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
        unlink(temp_dir, recursive = TRUE)
      }

      output$download_orphanhood_csv <- shiny::downloadHandler(
        filename = function() {
          sprintf("orphanhood_results_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S"))
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          if (download_scope_choice() == "all") {
            sprintf("orphanhood_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("orphanhood_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          shiny.semantic::hide_modal(ns("download_modal"), session = session)
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
        output$orphanhood_plot <- plotly::renderPlotly(NULL)
        output$orphanhood_table <- DT::renderDT(NULL)
        output$orphanhood_diagnostics <- shiny::renderUI(NULL)
        shinyjs::hide(id = ns("plot_container"))
        shinyjs::hide(id = ns("download_container"))
      }

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

      shared$custom_standard_value <- custom_standard_value
      shared
    },

    prepare = function(input, shared, i18n) {
      values <- orphanhood_control_values(
        input, shiny::isolate(shared$data()), shiny::isolate(shared$hiv_series_value())
      )
      list(
        maternal = !identical(values$parent, "paternal"),
        method = values$method %||% "timaeus",
        family = values$family %||% "CD_West",
        std_e0 = values$std_e0 %||% 60,
        standard = if (identical(values$standard_type, "custom")) shared$custom_standard_value() else NULL,
        mac = values$mac,
        date_obs = values$date_obs,
        date1 = values$date1,
        date2 = values$date2,
        hiv_vert_transm = values$hiv_vert_transm %||% (1 / 3),
        hiv_fert_ratio = values$hiv_fert_ratio %||% 0.75,
        hiv_prop_partner = values$hiv_prop_partner %||% 0.5,
        std_q5 = values$std_q5,
        hiv_series = values$hiv_series,
        hiv_period = values$hiv_period %||% "midpoint"
      )
    },

    run = function(shared, params, input, i18n) {
      data_full <- shiny::isolate(shared$data())
      if (is.null(data_full) || nrow(data_full) == 0) {
        return(list(error = i18n$t("No data available")))
      }
      # Problems with a custom standard or the HIV series are known messages:
      # show them translated
      problems <- character(0)
      if (!is.null(params$standard)) {
        problems <- check_orphanhood_standard(params$standard)
      }
      if (!is.null(params$hiv_series)) {
        if (!isTRUE(params$maternal)) {
          problems <- c(problems, "The Masquelier-Timaeus method is for maternal orphanhood only")
        }
        problems <- c(problems, check_orphanhood_hiv_series(params$hiv_series))
      }
      if (length(problems) > 0) {
        shared$last_result(NULL)
        return(list(error = paste(vapply(problems, i18n$t, character(1)), collapse = "; ")))
      }
      result <- tryCatch(
        run_orphanhood(
          data_full,
          maternal = params$maternal,
          method = params$method,
          family = params$family,
          std_e0 = params$std_e0,
          standard = params$standard,
          mac = params$mac,
          date_obs = params$date_obs,
          date1 = params$date1,
          date2 = params$date2,
          hiv_vert_transm = params$hiv_vert_transm,
          hiv_fert_ratio = params$hiv_fert_ratio,
          hiv_prop_partner = params$hiv_prop_partner,
          std_q5 = params$std_q5,
          hiv_series = params$hiv_series,
          hiv_period = params$hiv_period
        ),
        error = function(e) {
          list(error = paste0(i18n$t("Estimation failed: "), conditionMessage(e)))
        }
      )
      shared$last_result(if (is.null(result$error)) result else NULL)
      result
    },

    render = function(result, output, shared, input, i18n) {
      language_dep <- function() {
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
      }
      group_rows <- function(tbl) {
        if (is.null(tbl)) return(NULL)
        tbl[tbl$.id == shared$active_group_id(), , drop = FALSE]
      }
      observations <- result$params$observations %||% 1L

      output$orphanhood_plot <- plotly::renderPlotly({
        language_dep()
        if (!is.null(result$error)) {
          return(childmort_error_plot(result$error, i18n))
        }
        current_gid <- shared$active_group_id()
        group_error <- result$group_errors$message[result$group_errors$.id == as.character(current_gid)]
        if (length(group_error) > 0) {
          return(childmort_error_plot(group_error[1], i18n))
        }
        estimates <- group_rows(result$estimates)
        if (is.null(estimates) || nrow(estimates) == 0) {
          return(NULL)
        }
        labels <- if (identical(as.integer(observations), 2L)) {
          i18n$t("Age group of respondents")
        } else {
          i18n$t("Reference date")
        }
        plot_obj <- orphanhood_plot(
          estimates,
          observations = observations,
          title = i18n$t("Adult mortality estimates"),
          x_label = labels,
          y_label = i18n$t("Probability of dying between ages 15 and 60, 45q15")
        )
        plotly::ggplotly(plot_obj, tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })

      output$orphanhood_table <- DT::renderDT({
        language_dep()
        if (!is.null(result$error)) {
          return(NULL)
        }
        est <- group_rows(result$estimates)
        if (is.null(est) || nrow(est) == 0) {
          return(NULL)
        }
        shown <- function(x, digits) ifelse(est$plausible, round(x, digits), NA)
        tbl <- data.frame(
          est$age_label,
          shown(est$q15_45, 4),
          shown(est$q15_35, 4),
          round(est$ref_date, 1),
          stringsAsFactors = FALSE
        )
        headers <- c(
          i18n$t("Age group of respondents"),
          "45q15",
          "35q15",
          i18n$t("Reference date")
        )
        if ("years_before" %in% names(est)) {
          tbl$years_before <- round(est$years_before, 1)
          headers <- c(headers, i18n$t("Years before observation"))
        }
        names(tbl) <- headers
        DT::datatable(
          tbl,
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE, pageLength = 20)
        )
      })

      output$orphanhood_diagnostics <- shiny::renderUI({
        language_dep()
        if (!is.null(result$error)) {
          return(NULL)
        }
        est <- group_rows(result$estimates)
        input_df <- shiny::isolate(shared$data())
        input_group <- NULL
        if (!is.null(input_df)) {
          input_df <- prepare_orphanhood_input(input_df)
          if (".id" %in% names(input_df)) {
            input_df <- input_df[input_df$.id == shared$active_group_id(), , drop = FALSE]
          }
          input_df <- input_df[order(orphanhood_num(input_df$Age)), , drop = FALSE]
          prop_cols <- intersect(c("NotOrphaned", "NotOrphaned1", "NotOrphaned2"), names(input_df))
          input_group <- list(ages = input_df$Age, props = as.list(input_df[prop_cols]))
        }
        notes <- if (is.null(est) || nrow(est) == 0) {
          character(0)
        } else {
          orphanhood_diagnostics(est, result$params, input_group$ages, input_group$props)
        }
        failed_groups <- if (!is.null(result$group_errors)) nrow(result$group_errors) else 0
        if (length(notes) == 0 && failed_groups == 0) {
          return(NULL)
        }
        shiny::div(
          class = "ui info message orphanhood-diagnostics",
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

      shinyjs::show(id = session$ns("plot_container"))
      if (is.null(result$error)) {
        shinyjs::show(id = session$ns("download_container"))
      } else {
        # The previous results were cleared, so there is nothing to download
        shinyjs::hide(id = session$ns("download_container"))
      }
    },

    register_downloads = NULL
  ))
}
