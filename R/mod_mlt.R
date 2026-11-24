#' Model Life Tables Module UI
#'
#' One-step module: the life tables to build are typed (or pasted) into an
#' editable table, as in demographydash, so no file upload is needed.
#'
#' @param i18n Translator helper used for labels.
#' @noRd
mlt_module_ui <- function(i18n) {
  ns <- shiny::NS("mlt")

  shinyjs::hidden(
    shiny::div(
      id = "mlt_module_page",
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .mlt-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }
          .mlt-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .mlt-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .mlt-hero p {
            margin: 0;
            color: #425466;
          }
          .mlt-hint {
            font-size: 0.9em;
            color: #666;
            margin-top: 0.4em;
          }
          .mlt-input-title {
            font-weight: 700;
            margin-bottom: 0.4rem;
          }
          .mlt-input {
            margin-bottom: 1.2rem;
          }
          .mlt-results {
            margin-top: 1rem;
          }
          .mlt-inline {
            display: flex;
            gap: 10px;
            align-items: flex-end;
            flex-wrap: wrap;
          }
          .mlt-inline > div {
            min-width: 220px;
          }
          .mlt-section {
            margin-top: 1.5rem;
          }
          .mlt-diagnostics ul {
            padding-left: 1.2rem;
            margin: 0.5rem 0 0;
          }
        "))
      ),
      shiny::div(
        class = "mlt-main",
        shiny::div(
          class = "mlt-hero",
          shiny::div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
            shiny::uiOutput(ns("mlt_back_button"))
          ),
          shiny::uiOutput(ns("mlt_hero_content"))
        ),
        shiny::div(
          class = "info-box",
          shiny.semantic::tabset(
            tabs = list(
              list(
                menu = create_info_tab_label(ns("info_tab_upload_label"), i18n$t("Instructions")),
                content = shiny::uiOutput(ns("mlt_instructions"))
              ),
              list(
                menu = create_info_tab_label(ns("info_tab_docs_label"), i18n$t("Method Documentation")),
                content = shiny::uiOutput(ns("mlt_method_docs"))
              )
            )
          )
        ),
        untheme::sidebar_layout_responsive(
          list(
            children = shiny::div(
              class = "ui form",
              shiny::uiOutput(ns("mlt_controls")),
              shiny::br(),
              shiny::uiOutput(ns("mlt_run_button"))
            )
          ),
          shiny::div(
            style = "flex: 1; min-width: 0;",
            shiny::div(
              class = "mlt-input",
              shiny::uiOutput(ns("mlt_input_title")),
              rhandsontable::rHandsontableOutput(ns("mlt_input_table"), width = "100%"),
              shiny::uiOutput(ns("mlt_input_hint"))
            ),
            shiny::div(
              id = ns("results_container"),
              class = "mlt-results",
              style = "display: none;",
              shiny::uiOutput(ns("mlt_run_settings")),
              DT::DTOutput(ns("mlt_summary_table")),
              shiny::div(
                class = "mlt-section",
                shiny::uiOutput(ns("mlt_measure_ui")),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("mlt_plot"), height = "460px", width = "100%")
                )
              ),
              shiny::div(
                class = "mlt-section",
                shiny::uiOutput(ns("mlt_table_controls")),
                DT::DTOutput(ns("mlt_life_table"))
              ),
              shiny::uiOutput(ns("mlt_diagnostics")),
              shiny::div(
                class = "button-container",
                style = "justify-content: flex-end; margin-top: 1rem;",
                shiny::uiOutput(ns("mlt_download_button"))
              )
            )
          )
        )
      )
    )
  )
}


#' Build an error plot with a centred message
#' @noRd
mlt_error_plot <- function(message, i18n) {
  plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                  marker = list(size = 0, opacity = 0)) |>
    plotly::add_annotations(
      text = paste0("<b>", i18n$t("Error"), ":</b><br><br>", message),
      x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
      font = list(size = 16, color = "red"), xanchor = "center", yanchor = "middle"
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, range = c(-1, 1)),
      plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa", showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)
}


#' Columns of the input table for a method
#'
#' @return Character vector of internal column names (after `Name` and `Sex`).
#' @noRd
mlt_value_columns <- function(method) {
  if (identical(method, "combin")) c("q1", "q5", "adult") else "value"
}


#' Starting rows of the input table
#'
#' @param method `"match"` or `"combin"`.
#' @param indicator,adult_indicator Indicators of the method.
#' @param previous The current table (`Name`, `Sex`, ...), whose names and
#'   sexes are kept; values are reset because a change of indicator changes
#'   their scale.
#' @return Data frame with `Name`, `Sex` (codes) and the value columns.
#' @noRd
mlt_default_requests <- function(method, indicator = "e0", adult_indicator = "45q15", previous = NULL) {
  base <- if (!is.null(previous) && nrow(previous) > 0) {
    previous[, c("Name", "Sex"), drop = FALSE]
  } else {
    data.frame(Name = c("Example 1", "Example 2"), Sex = c("female", "male"), stringsAsFactors = FALSE)
  }
  n <- nrow(base)
  if (identical(method, "combin")) {
    base$q1 <- rep(MLT_DEFAULT_VALUES[["1q0"]], n)
    base$q5 <- rep(MLT_DEFAULT_VALUES[["5q0"]], n)
    base$adult <- rep(MLT_DEFAULT_VALUES[[adult_indicator]], n)
  } else {
    base$value <- rep(MLT_DEFAULT_VALUES[[indicator]], n)
  }
  base
}


#' Read the input table back from the browser
#'
#' @param df Data frame from `rhandsontable::hot_to_r()` (translated headers;
#'   Sex as shown).
#' @param method `"match"` or `"combin"`.
#' @param sex_labels Translated sex labels, named `female` and `male`.
#' @return Data frame with `Name`, `Sex` (codes, `NA` when not recognised) and
#'   the value columns.
#' @noRd
mlt_requests_from_table <- function(df, method, sex_labels = MLT_SEX_LABELS) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cols <- c("Name", "Sex", mlt_value_columns(method))
  if (ncol(df) < length(cols)) {
    for (k in seq(ncol(df) + 1, length(cols))) df[[k]] <- NA
  }
  df <- df[, seq_along(cols), drop = FALSE]
  names(df) <- cols
  df$Name <- as.character(df$Name)
  df$Sex <- mlt_sex_from_input(df$Sex, sex_labels)
  for (col in mlt_value_columns(method)) {
    df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  rownames(df) <- NULL
  df
}


#' Analysis info text written into model life table downloads
#' @noRd
mlt_info_text <- function(params, n_tables) {
  items <- mlt_settings_items(params)
  paste(
    c(
      "Module: Model Life Tables",
      "Description: Coale-Demeny and United Nations model life tables by single year of age, built as MortPak's MATCH (one indicator) and COMBIN (child and adult mortality) do, with Sara Hertog's R implementation (github.com/sarahertog/MortpakAbacusLegacyMLT) on the single-age tables of MortCast::MLT1Ylookup. The abridged tables are derived from the single-age ones with DemoTools::lt_single2abridged().",
      "",
      paste0(items$label, ": ", items$value),
      sprintf("Number of life tables: %d", n_tables),
      "",
      "Files:",
      "- mlt_summary.csv: the values asked for and the indicators of each life table built (e0, 1q0, 5q0, 45q15, 35q15). They can differ slightly from the values asked for because levels are interpolated.",
      "- mlt_single_age.csv: the life tables by single year of age.",
      "- mlt_abridged.csv: the abridged life tables (0, 1-4, 5-9, ...).",
      "- mlt_not_built.csv (when some rows failed): the rows that could not be built and why.",
      "",
      sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    ),
    collapse = "\n"
  )
}


#' Control values that define a model life table run
#' @noRd
mlt_control_values <- function(input) {
  list(
    method = input$method,
    family = input$mlt_family,
    indicator = input$indicator,
    adult_indicator = input$adult_indicator,
    open_age = suppressWarnings(as.numeric(input$open_age))
  )
}


#' Model Life Tables Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
mlt_module_server <- function(input, output, session) {

  mod_simple_module_server("mlt", list(
    setup = function(input, output, session, i18n) {
      ns <- session$ns
      shared <- list(
        last_result = shiny::reactiveVal(NULL),
        # Input table, stored with internal column names and sex codes
        requests = shiny::reactiveVal(mlt_default_requests("match")),
        # Bumped when the table must be redrawn (method or indicator changed)
        table_version = shiny::reactiveVal(0),
        # Method and indicator the table was drawn for
        table_key = shiny::reactiveVal("match e0")
      )
      sex_labels <- function() {
        stats::setNames(i18n$t(unname(MLT_SEX_LABELS)), names(MLT_SEX_LABELS))
      }
      lang <- function() {
        if (!is.null(session$userData$language_version)) session$userData$language_version()
      }

      # Info box tab labels (see create_info_tab_label()); the upload modules
      # get them from create_shared_data_context(), which this one does not use
      output$info_tab_upload_label <- shiny::renderUI({
        lang()
        i18n$t("Instructions")
      })
      output$info_tab_docs_label <- shiny::renderUI({
        lang()
        i18n$t("Method Documentation")
      })

      output$mlt_back_button <- shiny::renderUI({
        lang()
        shiny::actionButton("mlt_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$mlt_hero_content <- shiny::renderUI({
        lang()
        shiny::tagList(
          shiny::h1(i18n$t("Model Life Tables")),
          shiny::p(i18n$t("Build Coale-Demeny and United Nations model life tables from one mortality indicator, or from child and adult mortality, as MortPak's MATCH and COMBIN do."))
        )
      })

      output$mlt_instructions <- shiny::renderUI({
        lang()
        shiny::tagList(
          shiny::h1(i18n$t("How it works")),
          shiny::tags$ol(
            shiny::tags$li(i18n$t("Choose the method, the model life table family and the indicator on the left.")),
            shiny::tags$li(i18n$t("Type the life tables to build in the table: one row per life table, with a name, the sex and the value(s). You can paste rows from a spreadsheet, and right-click the table to add or remove rows.")),
            shiny::tags$li(i18n$t("Click 'Build life tables'. Each table is built by single year of age, with its abridged version, and can be downloaded."))
          ),
          shiny::p(class = "mlt-hint", i18n$t("Probabilities of dying (1q0, 5q0, 45q15, 35q15) are proportions, e.g. 0.05. The tables are by sex: there is no both-sexes table."))
        )
      })

      output$mlt_method_docs <- shiny::renderUI({
        lang()
        method_item <- function(name, text) {
          shiny::div(class = "item", shiny::div(class = "content",
            shiny::div(class = "header", i18n$t(name)),
            shiny::div(class = "description", i18n$t(text))
          ))
        }
        shiny::tagList(
          shiny::h3(i18n$t("Model life tables")),
          shiny::p(i18n$t("Model life tables describe typical age patterns of mortality at each level of mortality. The Coale-Demeny families (West, North, South, East) and the United Nations families for developing countries (General, Latin American, Chilean, South Asian, Far East Asian) are those of MortPak, extended to high life expectancies and graduated to single years of age (MortCast).")),
          shiny::div(
            class = "ui relaxed divided list",
            method_item("One indicator (MATCH)", "Finds the two levels of the family whose indicator (e0, 1q0, 5q0, 45q15 or 35q15) brackets the value given and interpolates their death rates on a log scale."),
            method_item("Child and adult mortality (COMBIN)", "Combines child mortality (1q0, 5q0 or both) with adult mortality (45q15 or 35q15): the adult ages come from the table matching the adult value, and ages 0-14 are fitted to the child values with MortPak's BESTFT procedure.")
          ),
          shiny::h4(i18n$t("Reading the results")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("The summary shows each table's e0, 1q0, 5q0, 45q15 and 35q15. They can differ slightly from the values given, because levels are interpolated and the tables are graduated to single years of age.")),
            shiny::tags$li(i18n$t("The abridged table (0, 1-4, 5-9, ...) is computed from the single-age table, so both describe the same mortality."))
          ),
          shiny::h4(i18n$t("References")),
          shiny::tags$ul(
            shiny::tags$li("United Nations (1982). Model Life Tables for Developing Countries. Population Studies No. 77. New York: United Nations."),
            shiny::tags$li("Coale, A. J. and Demeny, P. (1983). Regional Model Life Tables and Stable Populations. 2nd ed. New York: Academic Press."),
            shiny::tags$li("United Nations (2013). MortPak for Windows, version 4.3. New York: United Nations Population Division."),
            shiny::tags$li("Hertog, S. MortpakAbacusLegacyMLT: R implementation of the MortPak and Abacus model life tables. github.com/sarahertog/MortpakAbacusLegacyMLT.")
          )
        )
      })

      # Controls
      output$mlt_controls <- shiny::renderUI({
        lang()
        family_labels <- stats::setNames(i18n$t(unname(MLT_FAMILY_LABELS)), names(MLT_FAMILY_LABELS))
        shiny::tagList(
          shiny.semantic::selectInput(
            ns("method"), i18n$t("Method"),
            choices = stats::setNames(names(MLT_METHODS), i18n$t(unname(MLT_METHODS))),
            selected = shiny::isolate(input$method) %||% "match"
          ),
          shiny.semantic::selectInput(
            ns("mlt_family"), i18n$t("Model life table family"),
            choices = stats::setNames(MLT_FAMILIES, unname(family_labels[MLT_FAMILIES])),
            selected = shiny::isolate(input$mlt_family) %||% "CD_West"
          ),
          shiny::uiOutput(ns("mlt_indicator_ui")),
          shiny::numericInput(
            ns("open_age"), i18n$t("Open age group"),
            value = shiny::isolate(input$open_age) %||% MLT_DEFAULT_OPEN_AGE, min = 85, max = 130, step = 5
          )
        )
      })
      outputOptions(output, "mlt_controls", suspendWhenHidden = FALSE)

      output$mlt_indicator_ui <- shiny::renderUI({
        lang()
        if (identical(input$method, "combin")) {
          shiny.semantic::selectInput(
            ns("adult_indicator"), i18n$t("Adult mortality indicator"),
            choices = stats::setNames(MLT_ADULT_INDICATORS, i18n$t(unname(MLT_INDICATORS[MLT_ADULT_INDICATORS]))),
            selected = shiny::isolate(input$adult_indicator) %||% "45q15"
          )
        } else {
          shiny.semantic::selectInput(
            ns("indicator"), i18n$t("Indicator"),
            choices = stats::setNames(names(MLT_INDICATORS), i18n$t(unname(MLT_INDICATORS))),
            selected = shiny::isolate(input$indicator) %||% "e0"
          )
        }
      })
      outputOptions(output, "mlt_indicator_ui", suspendWhenHidden = FALSE)

      output$mlt_run_button <- shiny::renderUI({
        lang()
        shiny::actionButton(ns("run_analysis"), i18n$t("Build life tables"), class = "ui primary button")
      })
      outputOptions(output, "mlt_run_button", suspendWhenHidden = FALSE)

      # Input table: rebuilt when the method or indicator changes (values reset,
      # names and sexes kept); edits are read back on every change
      shiny::observeEvent(list(input$method, input$indicator, input$adult_indicator), {
        method <- input$method %||% "match"
        indicator <- if (identical(method, "combin")) input$adult_indicator %||% "45q15" else input$indicator %||% "e0"
        key <- paste(method, indicator)
        # Only a change of method or of the indicator in use resets the values
        if (identical(key, shiny::isolate(shared$table_key()))) return()
        shared$table_key(key)
        shared$requests(mlt_default_requests(
          method, input$indicator %||% "e0", input$adult_indicator %||% "45q15",
          previous = shiny::isolate(shared$requests())
        ))
        shared$table_version(shiny::isolate(shared$table_version()) + 1)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$mlt_input_table, {
        df <- tryCatch(rhandsontable::hot_to_r(input$mlt_input_table), error = function(e) NULL)
        method <- input$method %||% "match"
        # A table drawn for the other method (sent before the redraw) is ignored
        if (is.null(df) || ncol(df) != 2 + length(mlt_value_columns(method))) return()
        shared$requests(mlt_requests_from_table(df, method, sex_labels()))
      })

      output$mlt_input_title <- shiny::renderUI({
        lang()
        shiny::div(class = "mlt-input-title", i18n$t("Life tables to build"))
      })

      output$mlt_input_table <- rhandsontable::renderRHandsontable({
        lang()
        shared$table_version()
        method <- shiny::isolate(input$method) %||% "match"
        req <- shiny::isolate(shared$requests())
        labels <- sex_labels()
        shown <- req
        shown$Sex <- ifelse(is.na(req$Sex), NA_character_, unname(labels[req$Sex]))
        value_headers <- if (identical(method, "combin")) {
          c("1q0", "5q0", shiny::isolate(input$adult_indicator) %||% "45q15")
        } else {
          shiny::isolate(input$indicator) %||% "e0"
        }
        names(shown) <- c(i18n$t("Name"), i18n$t("Sex"), value_headers)
        # Fixed column widths: a stretched table is first drawn at a width that
        # changes on the first click, so cells move under the pointer
        rhandsontable::rhandsontable(shown, rowHeaders = TRUE, stretchH = "none", height = 230,
                                     colWidths = c(220, 110, rep(110, length(value_headers)))) |>
          rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
          rhandsontable::hot_col(i18n$t("Sex"), type = "dropdown", source = unname(labels), strict = TRUE) |>
          # e0 with one decimal, probabilities with five (the default two
          # would show 0.04651 as 0.05)
          rhandsontable::hot_col(value_headers, type = "numeric",
                                 format = if (identical(value_headers, "e0")) "0.0" else "0.00000")
      })
      # Drawn only once visible: a handsontable drawn while the page is hidden
      # gets no width and shows squashed

      output$mlt_input_hint <- shiny::renderUI({
        lang()
        shiny::p(class = "mlt-hint", i18n$t("One row per life table. Paste from a spreadsheet or right-click to add and remove rows. Probabilities are proportions (e.g. 0.05)."))
      })

      output$mlt_download_button <- shiny::renderUI({
        lang()
        shiny::downloadButton(ns("download_mlt"), i18n$t("Download results"), class = "ui primary button")
      })

      output$mlt_run_settings <- shiny::renderUI({
        lang()
        latest <- shared$last_result()
        if (is.null(latest) || is.null(latest$params)) return(NULL)
        items <- mlt_settings_items(latest$params)
        parts <- vapply(seq_len(nrow(items)), function(i) {
          value <- if (items$translate_value[i]) i18n$t(items$value[i]) else items$value[i]
          paste0(i18n$t(items$label[i]), ": ", value)
        }, character(1))
        current <- mlt_control_values(input)
        p <- latest$params
        differs <- function(old, new) !is.null(new) && length(new) == 1 && !is.na(new) &&
          !identical(as.character(old), as.character(new))
        changed <- differs(p$method, current$method) || differs(p$family, current$family) ||
          (identical(p$method, "match") && differs(p$indicator, current$indicator)) ||
          (identical(p$method, "combin") && differs(p$adult_indicator, current$adult_indicator)) ||
          differs(p$open_age, current$open_age) ||
          !identical(latest$requests, shared$requests())
        shiny::tagList(
          shiny::div(
            class = "ui small message mlt-run-settings",
            shiny::tags$b(paste0(i18n$t("Results for"), ": ")),
            paste(parts, collapse = " · ")
          ),
          if (changed) {
            shiny::div(
              class = "ui warning message mlt-settings-changed",
              i18n$t("The settings or the table have changed. Click 'Build life tables' to update the results.")
            )
          }
        )
      })

      output$download_mlt <- shiny::downloadHandler(
        filename = function() sprintf("model_life_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
        content = function(file) {
          latest <- shared$last_result()
          shiny::req(latest, latest$summary)
          temp_dir <- tempfile()
          dir.create(temp_dir)
          files <- c()
          write <- function(tbl, name) {
            f <- file.path(temp_dir, name)
            utils::write.csv(tbl, f, row.names = FALSE)
            f
          }
          files <- c(
            write(latest$summary, "mlt_summary.csv"),
            write(latest$single, "mlt_single_age.csv"),
            write(latest$abridged, "mlt_abridged.csv")
          )
          if (nrow(latest$failed) > 0) files <- c(files, write(latest$failed, "mlt_not_built.csv"))
          plot_file <- file.path(temp_dir, "mlt_plot.png")
          measure <- shiny::isolate(input$measure) %||% "nMx"
          ggplot2::ggsave(
            plot_file,
            plot = mlt_plot(latest$single, measure, title = i18n$t("Model life tables"),
                            x_label = i18n$t("Age"), y_label = i18n$t(MLT_PLOT_MEASURES[[measure]]),
                            age_label = i18n$t("Age")),
            width = 10, height = 6, dpi = 300
          )
          info_file <- file.path(temp_dir, "analysis_info.txt")
          writeLines(mlt_info_text(latest$params, nrow(latest$summary)), info_file)
          files <- c(files, plot_file, info_file)
          zip::zip(zipfile = file, files = basename(files), root = temp_dir)
          unlink(temp_dir, recursive = TRUE)
        },
        contentType = "application/zip"
      )

      shared
    },

    prepare = function(input, shared, i18n) {
      values <- mlt_control_values(input)
      list(
        method = values$method %||% "match",
        family = values$family %||% "CD_West",
        indicator = values$indicator %||% "e0",
        adult_indicator = values$adult_indicator %||% "45q15",
        open_age = if (is.null(values$open_age) || is.na(values$open_age)) MLT_DEFAULT_OPEN_AGE else values$open_age,
        requests = shiny::isolate(shared$requests())
      )
    },

    run = function(shared, params, input, i18n) {
      if (params$open_age < 85 || params$open_age > 130) {
        result <- list(error = i18n$t("The open age group must be between 85 and 130"))
        shared$last_result(NULL)
        return(result)
      }
      result <- tryCatch(
        run_mlt(params$requests, method = params$method, family = params$family,
                indicator = params$indicator, adult_indicator = params$adult_indicator,
                open_age = params$open_age),
        error = function(e) list(error = translate_messages(conditionMessage(e), i18n, MLT_MESSAGE_TEMPLATES))
      )
      if (is.null(result$error)) {
        result$requests <- params$requests
        if (is.null(result$summary)) {
          result$error <- i18n$t("None of the life tables could be built; see the messages below.")
        }
      }
      shared$last_result(if (is.null(result$error)) result else NULL)
      result
    },

    render = function(result, output, shared, input, i18n) {
      # Inputs created here need the module namespace explicitly
      ns <- shiny::NS("mlt")
      lang <- function() {
        if (!is.null(session$userData$language_version)) session$userData$language_version()
      }
      ok <- is.null(result$error)

      output$mlt_summary_table <- DT::renderDT({
        lang()
        if (!ok) return(NULL)
        s <- result$summary
        sex <- i18n$t(unname(MLT_SEX_LABELS[s$Sex]))
        tbl <- data.frame(s$Name, sex, s$asked, round(s$e0, 2), round(s$q1, 4), round(s$q5, 4),
                          round(s$q45_15, 4), round(s$q35_15, 4), stringsAsFactors = FALSE)
        names(tbl) <- c(i18n$t("Name"), i18n$t("Sex"), i18n$t("Values given"), "e0", "1q0", "5q0", "45q15", "35q15")
        DT::datatable(tbl, rownames = FALSE,
                      options = list(dom = "t", ordering = FALSE, pageLength = nrow(tbl), scrollX = TRUE))
      })

      output$mlt_measure_ui <- shiny::renderUI({
        lang()
        if (!ok) return(NULL)
        shiny::div(
          style = "max-width: 360px;",
          shiny.semantic::selectInput(
            ns("measure"), i18n$t("Show on the chart"),
            choices = stats::setNames(names(MLT_PLOT_MEASURES), i18n$t(unname(MLT_PLOT_MEASURES))),
            selected = shiny::isolate(input$measure) %||% "nMx"
          )
        )
      })

      output$mlt_plot <- plotly::renderPlotly({
        lang()
        if (!ok) return(mlt_error_plot(result$error, i18n))
        measure <- input$measure %||% "nMx"
        p <- mlt_plot(result$single, measure, title = i18n$t("Model life tables"),
                      x_label = i18n$t("Age"), y_label = i18n$t(MLT_PLOT_MEASURES[[measure]]),
                      age_label = i18n$t("Age"))
        plotly::ggplotly(p, tooltip = "text") |> plotly::config(displayModeBar = FALSE)
      })

      output$mlt_table_controls <- shiny::renderUI({
        lang()
        if (!ok) return(NULL)
        names_built <- result$summary$Name
        shiny::div(
          class = "mlt-inline",
          shiny.semantic::selectInput(
            ns("table_name"), i18n$t("Life table"),
            choices = names_built,
            selected = if (isTRUE(shiny::isolate(input$table_name) %in% names_built)) shiny::isolate(input$table_name) else names_built[1]
          ),
          shiny.semantic::selectInput(
            ns("table_ages"), i18n$t("Ages"),
            choices = stats::setNames(c("single", "abridged"), c(i18n$t("Single years"), i18n$t("Abridged (0, 1-4, 5-9, ...)"))),
            selected = shiny::isolate(input$table_ages) %||% "single"
          )
        )
      })

      output$mlt_life_table <- DT::renderDT({
        lang()
        if (!ok) return(NULL)
        name <- input$table_name %||% result$summary$Name[1]
        lt <- if (identical(input$table_ages, "abridged")) result$abridged else result$single
        lt <- lt[lt$Name == name, , drop = FALSE]
        if (nrow(lt) == 0) return(NULL)
        tbl <- data.frame(
          Age = ifelse(lt$Age == max(lt$Age), paste0(lt$Age, "+"), as.character(lt$Age)),
          nMx = signif(lt$nMx, 5), nqx = signif(lt$nqx, 5), lx = round(lt$lx), ndx = round(lt$ndx),
          nLx = round(lt$nLx), Tx = round(lt$Tx), ex = round(lt$ex, 2),
          stringsAsFactors = FALSE
        )
        names(tbl)[1] <- i18n$t("Age")
        DT::datatable(tbl, rownames = FALSE, options = list(
          dom = "tp", ordering = FALSE, pageLength = 20, scrollX = TRUE,
          language = list(paginate = list(previous = i18n$t("Previous"), `next` = i18n$t("Next")))
        ))
      })

      output$mlt_diagnostics <- shiny::renderUI({
        lang()
        failed <- result$failed
        if (is.null(failed) || nrow(failed) == 0) return(NULL)
        shiny::div(
          class = "ui warning message mlt-diagnostics",
          shiny::div(class = "header", i18n$t("Some life tables could not be built")),
          shiny::tags$ul(lapply(seq_len(nrow(failed)), function(i) {
            shiny::tags$li(paste0(failed$Name[i], ": ", translate_messages(failed$message[i], i18n, MLT_MESSAGE_TEMPLATES)))
          }))
        )
      })

      shinyjs::show(id = "results_container")
      if (ok) {
        shinyjs::show(id = "mlt_download_button")
      } else {
        shinyjs::hide(id = "mlt_download_button")
      }
    },

    register_downloads = NULL
  ))
}
