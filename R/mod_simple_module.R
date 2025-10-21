#' Simple Module UI Wrapper
#'
#' Provides a standardized layout for analytic modules that share the same
#' workflow: upload/ingest data, configure parameters, run diagnostics, and
#' review/download results. Module-specific UIs can inject additional controls
#' or visualisations via the `content_ui` list.
#'
#' @param id Module namespace identifier.
#' @param metadata Named list containing UI labels such as `title`, `subtitle`,
#'   `description`, `run_button`, and `instructions`. Strings should already be
#'   translated by the caller.
#' @param content_ui Optional named list of functions used to inject module
#'   specific UI. Recognised names are `before_data`, `controls`, `results`, and
#'   `downloads`; each function receives the module namespace.
#'
#' @return A UI definition that can be included in the app layout.
#' @importFrom shiny NS tagList div h2 p strong span actionButton uiOutput br
#' @importFrom shiny.semantic icon
#' @noRd
mod_simple_module_ui <- function(id, metadata, content_ui = list()) {
  ns <- NS(id)

  styles <- shiny::tags$head(shiny::singleton(shiny::tags$style(shiny::HTML("
    .simple-module-container {
      max-width: 1180px;
      margin: 0 auto 3rem;
    }
    .simple-module-inner {
      background: #ffffff;
      border-radius: 16px;
      box-shadow: 0 10px 35px rgba(17, 24, 39, 0.08);
      padding: 2.5rem 3rem;
    }
    .simple-module-header {
      display: flex;
      align-items: center;
      gap: 1.5rem;
      margin-bottom: 1.5rem;
    }
    .simple-module-header .icon {
      font-size: 2.8rem;
      color: #1b6ec2;
    }
    .simple-module-title {
      margin: 0;
      font-size: 2.1rem;
      font-weight: 600;
      color: #1b1c1d;
    }
    .simple-module-subtitle {
      margin: 0.2rem 0 0;
      font-size: 1.15rem;
      color: #4a5568;
    }
    .simple-module-description {
      margin: 0.35rem 0 0;
      color: #4a5568;
    }
    .simple-module-callout {
      background: #f1f5ff;
      border-left: 4px solid #1b6ec2;
      padding: 1rem 1.25rem;
      border-radius: 10px;
      margin-bottom: 1.75rem;
      color: #1b1c1d;
    }
    .simple-module-hero {
      display: flex;
      flex-wrap: wrap;
      gap: 1.5rem;
      align-items: stretch;
      margin-bottom: 2rem;
    }
    .simple-module-hero .hero-card {
      flex: 1 1 320px;
      background: linear-gradient(135deg, rgba(27,110,194,0.12), rgba(27,110,194,0.04));
      border-radius: 16px;
      padding: 1.8rem;
      display: flex;
      flex-direction: column;
      justify-content: center;
    }
    .simple-module-hero .hero-card h3 {
      margin-top: 0;
      font-weight: 600;
      color: #1b1c1d;
    }
    .simple-module-hero .hero-card p {
      margin-bottom: 0.6rem;
      color: #425466;
      line-height: 1.55;
    }
    .simple-module-hero .hero-actions {
      display: flex;
      flex-wrap: wrap;
      gap: 0.75rem;
      margin-top: 1.25rem;
    }
    .simple-module-info {
      margin-bottom: 2rem;
    }
    .simple-module-info .ui.segment {
      border-radius: 12px !important;
      box-shadow: none !important;
      border: 1px solid rgba(27,110,194,0.18) !important;
    }
    .simple-module-info ul {
      padding-left: 1.1rem;
      margin-bottom: 0;
    }
    .simple-module-section {
      margin-bottom: 2rem;
    }
    .simple-module-upload {
      gap: 1.5rem !important;
    }
    .simple-module-upload-box {
      padding: 1.25rem;
      border: 1px dashed rgba(27, 110, 194, 0.35);
      border-radius: 12px;
      background: #f8fbff;
      display: flex;
      flex-direction: column;
      gap: 1rem;
      min-height: 220px;
      justify-content: center;
    }
    .simple-module-upload-box .ui.button {
      width: 100%;
    }
    .simple-module-validation {
      padding: 1.25rem;
      border-radius: 12px;
      background: #f9fafb;
      min-height: 220px;
      border: 1px solid rgba(148, 163, 184, 0.35);
    }
    .simple-module-grouping {
      display: flex;
      gap: 1rem;
      flex-wrap: wrap;
    }
    .simple-module-run {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 1rem;
      padding: 1.25rem 1.75rem;
      border-radius: 12px;
      background: #f8fbff;
      border: 1px solid rgba(27,110,194,0.18);
    }
    .simple-module-run .ui.button {
      min-width: 220px;
      font-size: 1.05rem;
      padding: 0.85rem 1.75rem;
    }
    .simple-module-log {
      color: #425466;
    }
    .simple-module-log.success {
      color: #227447;
      font-weight: 600;
    }
    .simple-module-log.error {
      color: #c0392b;
      font-weight: 600;
    }
    .simple-module-downloads {
      display: flex;
      justify-content: flex-end;
    }
    @media (max-width: 992px) {
      .simple-module-inner {
        padding: 2rem 1.5rem;
      }
      .simple-module-run {
        flex-direction: column;
        align-items: stretch;
      }
      .simple-module-run .ui.button {
        width: 100%;
      }
    }
    @media (max-width: 640px) {
      .simple-module-header {
        flex-direction: column;
        align-items: flex-start;
      }
      .simple-module-title {
        font-size: 1.75rem;
      }
    }
  "))))

  get_section <- function(name) {
    fn <- content_ui[[name]]
    if (is.null(fn) || !is.function(fn)) {
      return(shiny::tagList())
    }
    fn(ns)
  }

  before_data_ui <- get_section("before_data")
  info_ui <- get_section("info")
  controls_ui <- get_section("controls")
  results_ui <- get_section("results")
  downloads_ui <- get_section("downloads")

  metadata <- utils::modifyList(list(
    title = "",
    subtitle = NULL,
    description = NULL,
    instructions = NULL,
    run_button = "Run analysis",
    icon = "chart line",
    file_label = "",
    sample_button = "Use sample data"
  ), metadata)

  tagList(
    styles,
    div(
      id = ns("simple_module"),
      class = "simple-module-container",
      div(
        class = "simple-module-inner ui segment",
        div(
          class = "simple-module-header",
          shiny.semantic::icon(metadata$icon),
          div(
            class = "simple-module-heading",
            h2(metadata$title, class = "simple-module-title"),
            if (!is.null(metadata$subtitle)) {
              p(metadata$subtitle, class = "simple-module-subtitle")
            },
            if (!is.null(metadata$description)) {
              p(metadata$description, class = "simple-module-description")
            }
          )
        ),
        if (!is.null(metadata$instructions)) {
          div(
            class = "simple-module-callout",
            strong(span(metadata$instructions, class = "simple-module-instructions"))
          )
        },
        before_data_ui,
        if (length(info_ui) > 0) {
          div(class = "simple-module-info", info_ui)
        },
        div(
          class = "simple-module-section",
          uiOutput(ns("modal_ui")),
          div(
            class = "simple-module-upload ui two column stackable grid",
            div(
              class = "column",
              div(
                class = "simple-module-upload-box",
                shiny.semantic::file_input(
                  ns("file1"),
                  label = metadata$file_label %||% "",
                  type = "flex-override"
                ),
                actionButton(ns("use_sample_data"), metadata$sample_button %||% "Use sample data",
                  class = "ui blue button"
                ),
                uiOutput(ns("upload_log"))
              )
            ),
            div(
              class = "column",
              div(
                class = "simple-module-validation",
                uiOutput(ns("validation_summary")),
                uiOutput(ns("validation_table_ui"))
              )
            )
          )
        ),
        div(
          class = "simple-module-section",
          uiOutput(ns("grouping_controls"))
        ),
        if (length(controls_ui) > 0) {
          div(
            class = "simple-module-section",
            controls_ui
          )
        },
        div(
          class = "simple-module-section simple-module-run",
          actionButton(ns("run_analysis"), metadata$run_button,
            class = "ui primary button"
          ),
          uiOutput(ns("run_log"))
        ),
        div(
          class = "simple-module-section simple-module-results",
          results_ui
        ),
        if (length(downloads_ui) > 0) {
          div(
            class = "simple-module-section simple-module-downloads",
            downloads_ui
          )
        }
      )
    )
  )
}


#' Simple Module Server Wrapper
#'
#' Centralises common orchestration for analytic modules. The wrapper expects
#' callbacks that hook into the shared lifecycle while the wrapper manages
#' logging, data ingestion and reactive guards.
#'
#' @param id Module namespace identifier.
#' @param callbacks Named list with optional entries:
#'   - `setup`: function `(input, output, session, i18n)` returning a list with
#'     shared objects (e.g. `data`, `guards`).
#'   - `prepare`: function `(input, shared, i18n)` returning parameters passed to
#'     `run`.
#'   - `run`: function `(shared, params, input, i18n)` returning analysis result.
#'   - `render`: function `(result, output, shared, input, i18n)` to update UI.
#'   - `register_downloads`: optional function `(result, output, session, shared, i18n)`.
#'
#' Missing callbacks fall back to safe defaults.
#'
#' @importFrom shiny moduleServer observeEvent req isolate
#' @noRd
mod_simple_module_server <- function(id, callbacks) {
  callbacks <- utils::modifyList(list(
    setup = function(input, output, session, i18n) {
      list()
    },
    prepare = function(input, shared, i18n) {
      list()
    },
    run = function(shared, params, input, i18n) {
      NULL
    },
    render = function(result, output, shared, input, i18n) {
      output$run_log <- shiny::renderUI({
        shiny::span(class = "simple-module-log", sprintf("No renderer defined for module '%s'.", id))
      })
    },
    register_downloads = NULL
  ), callbacks, keep.null = TRUE)

  moduleServer(id, function(input, output, session) {
    i18n <- usei18n_local()

    shared <- callbacks$setup(input, output, session, i18n)
    if (is.null(shared) || !is.list(shared)) {
      shared <- list()
    }

    observeEvent(input$run_analysis, {
      params <- callbacks$prepare(input, shared, i18n)

      result <- callbacks$run(shared, params, input, i18n)

      callbacks$render(result, output, shared, input, i18n)

      if (is.function(callbacks$register_downloads)) {
        callbacks$register_downloads(result, output, session, shared, i18n)
      }
    }, ignoreNULL = TRUE)
  })
}


# Helper: Provide default value with fallback
# Avoid importing rlang just for `%||%`.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
