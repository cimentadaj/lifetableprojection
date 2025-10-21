#' Heaping Diagnostics Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
heaping_module_ui <- function(i18n) {
  metadata <- list(
    title = i18n$t("Heaping Diagnostics"),
    subtitle = i18n$t("Evaluate age heaping patterns for uploaded mortality data."),
    description = i18n$t("Compute Bachi, Myers, Roughness, and Sawtooth indices to assess digit preference across ages."),
    instructions = i18n$t("Upload your CSV, confirm grouping columns when prompted, then choose the variable you wish to evaluate."),
    run_button = i18n$t("Run heaping analysis"),
    file_label = i18n$t("Upload CSV file"),
    sample_button = i18n$t("Use sample data"),
    icon = "chart bar"
  )

  content_ui <- list(
    before_data = function(ns) {
      shiny::div(
        class = "simple-module-hero",
        shiny::div(
          class = "hero-card",
          shiny::tags$button(
            id = "heaping_back_to_modules",
            class = "ui button",
            type = "button",
            onclick = "Shiny.setInputValue('heaping_back_to_modules', Date.now());",
            i18n$t("← Back to modules")
          ),
          shiny::h3(i18n$t("Understand reporting accuracy in seconds")),
          shiny::p(
            i18n$t("Select exposures or deaths to reveal digit-preference warnings before you progress into smoothing or graduation.")
          ),
          shiny::div(
            class = "hero-actions",
            shiny::tags$div(
              class = "ui basic label",
              shiny::icon("check"),
              i18n$t("Single-age and abridged support")
            ),
            shiny::tags$div(
              class = "ui basic label",
              shiny::icon("clock"),
              i18n$t("Instant classifications by severity")
            )
          )
        ),
        shiny::div(
          class = "hero-card",
          shiny::h3(i18n$t("Quick start checklist")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("CSV columns must include Age, Deaths, and Exposures.")),
            shiny::tags$li(i18n$t("When the modal opens, tick \"No grouping needed\" unless your file contains grouping identifiers.")),
            shiny::tags$li(i18n$t("Use the toggle below to switch between Deaths and Exposures diagnostics."))
          )
        )
      )
    },
    info = function(ns) {
      shiny::div(
        class = "ui stackable two column grid",
        shiny::div(
          class = "column",
          shiny.semantic::segment(
            shiny::h4(i18n$t("How to prepare your data")),
            shiny::p(
              i18n$t("Ensure all ages are unique within each group, provide numeric counts, and reserve optional columns like Sex or Region for grouping selections.")
            ),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong(i18n$t("Required:")), " Age, Deaths, Exposures"),
              shiny::tags$li(shiny::strong(i18n$t("Optional:")), " ", i18n$t("Grouping columns (e.g., Sex, Province); leave blank and tick the checkbox if none apply.")),
              shiny::tags$li(shiny::strong(i18n$t("Recommended:")), " ", i18n$t("Provide exposures in the same scale as deaths for comparable indices."))
            )
          )
        ),
        shiny::div(
          class = "column",
          shiny.semantic::segment(
            shiny::h4(i18n$t("Sample heaping CSV preview")),
            shiny::p(i18n$t("Source: ODAPbackend::dat_heap_smooth.csv.gz (single-year age counts).")),
            DT::dataTableOutput(ns("heaping_sample_preview"))
          )
        )
      )
    },
    controls = function(ns) {
      shiny::div(
        class = "ui form heaping-controls",
        shiny.semantic::segment(
          shiny::div(
            class = "two fields",
            shiny::div(
              class = "field",
              shiny.semantic::selectInput(
                ns("heaping_variable"),
                i18n$t("Variable to evaluate"),
                choices = c("Deaths", "Exposures"),
                selected = "Deaths"
              )
            ),
            shiny::div(
              class = "field",
              shiny::uiOutput(ns("active_group_label"))
            )
          ),
          shiny::div(
            class = "field",
            shiny::div(
              class = "ui small message",
              shiny::icon("info-circle"),
              shiny::span(i18n$t("Tip: choose \"Deaths\" for Bachi/Myers and \"Exposures\" to inspect denominator quality."))
            )
          )
        )
      )
    },
    results = function(ns) {
      shiny::div(
        class = "simple-module-results-table",
        DT::dataTableOutput(ns("heaping_table"))
      )
    },
    downloads = function(ns) {
      shiny::div(
        class = "simple-module-download",
        shiny::downloadButton(ns("download_heaping_csv"), i18n$t("Download results"), class = "ui button")
      )
    }
  )

  shinyjs::hidden(
    shiny::div(
      id = "heaping_module_page",
      mod_simple_module_ui("heaping", metadata, content_ui)
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
  mod_simple_module_server("heaping", list(
    setup = function(input, output, session, i18n) {
      heaping_sample_loader <- function() {
        cat("[HEAPING_MODULE] Loading ODAPbackend heaping sample dataset\n")
        path <- system.file("extdata", "dat_heap_smooth.csv.gz", package = "ODAPbackend")
        df <- readr::read_csv(path, show_col_types = FALSE)
        df <- df |>
          dplyr::select(Age, Deaths, Exposures)
        df <- df[order(df$Age), , drop = FALSE]
        cat("[HEAPING_MODULE] Sample loader output | class:", paste(class(df), collapse = ", "),
            "| rows:", nrow(df), "| cols:", ncol(df), "\n")
        df
      }

      shared <- create_shared_data_context(
        "heaping",
        input, output, session, i18n,
        sample_loader = heaping_sample_loader
      )
      shared$last_result <- shiny::reactiveVal(NULL)

      observeEvent(shared$group_selection_passed(), {
        if (!isTRUE(shared$group_selection_passed())) return()
        cat("[HEAPING_MODULE] Group selection confirmed\n")

        gid <- shared$active_group_id()
        label <- gid
        labels_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
        if (!is.null(labels_df)) {
          if (!is.null(labels_df) && ".id" %in% names(labels_df) && ".id_label" %in% names(labels_df)) {
            match_idx <- which(labels_df$.id == gid)
            if (length(match_idx) > 0) {
              label <- labels_df$.id_label[match_idx][1]
            }
          }
        }
        if (is.null(label) || identical(label, "")) {
          label <- i18n$t("All records")
        }
        output$active_group_label <- shiny::renderUI({
          shiny::span(
            class = "simple-module-log",
            sprintf(i18n$t("Current group: %s"), label)
          )
        })
      })

      sample_preview <- head(heaping_sample_loader(), 6)

      output$heaping_sample_preview <- DT::renderDT({
        DT::datatable(
          sample_preview,
          rownames = FALSE,
          options = list(
            dom = "t",
            paging = FALSE
          )
        )
      })

      output$active_group_label <- shiny::renderUI({
        shiny::span(
          class = "simple-module-log",
          i18n$t("Current group: awaiting selection")
        )
      })

      shared
    },
    prepare = function(input, shared, i18n) {
      variable <- input$heaping_variable
      if (is.null(variable) || !nzchar(variable)) {
        variable <- "Deaths"
      }
      list(variable = variable)
    },
    run = function(shared, params, input, i18n) {
      if (is.null(shared$data()) || is.null(shared$data())) {
        return(list(error = i18n$t("No data available. Please upload a dataset first.")))
      }

      if (!isTRUE(shared$group_selection_passed())) {
        return(list(error = i18n$t("Please confirm the grouping modal before running the analysis.")))
      }

      data_subset <- shared$filtered_data()
      if (is.null(data_subset) || nrow(data_subset) == 0) {
        return(list(error = i18n$t("The selected group returned no rows to analyse.")))
      }

      if (!params$variable %in% names(data_subset)) {
        return(list(error = sprintf(i18n$t("Column '%s' is missing from the dataset."), params$variable)))
      }

      cat(sprintf(
        "[HEAPING_MODULE] Running diagnostics for variable '%s' on %d rows\n",
        params$variable,
        nrow(data_subset)
      ))

      analysis <- tryCatch({
        ODAPbackend::check_heaping_general(data_subset, params$variable)
      }, error = function(e) {
        cat(sprintf("[HEAPING_MODULE][ERROR] %s\n", e$message))
        list(error = i18n$t("Heaping diagnostics failed. Check input data validity."))
      })

      if (!is.null(analysis$error)) {
        return(analysis)
      }

      gid <- shared$active_group_id()
      label_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
      group_label <- gid
      if (!is.null(label_df) && ".id" %in% names(label_df) && ".id_label" %in% names(label_df)) {
        match_idx <- which(label_df$.id == gid)
        if (length(match_idx) > 0) {
          group_label <- label_df$.id_label[match_idx][1]
        }
      }

      list(
        table = analysis,
        group_label = group_label,
        variable = params$variable
      )
    },
    render = function(result, output, shared, input, i18n) {
      if (is.null(result) || !is.null(result$error)) {
        msg <- if (!is.null(result$error)) result$error else i18n$t("No diagnostics computed yet.")
        output$run_log <- shiny::renderUI({
          shiny::span(class = "simple-module-log error", msg)
        })
        output$heaping_table <- DT::renderDT(NULL)
        return()
      }

      shared$last_result(result)

      output$run_log <- shiny::renderUI({
        shiny::span(
          class = "simple-module-log success",
          sprintf(i18n$t("Heaping diagnostics completed for %s (%s)."), result$group_label, result$variable)
        )
      })

      output$active_group_label <- shiny::renderUI({
        shiny::span(
          class = "simple-module-log",
          sprintf(i18n$t("Current group: %s"), result$group_label)
        )
      })

      output$heaping_table <- DT::renderDT({
        tbl <- result$table
        display_tbl <- tbl
        color_map <- NULL

        if ("color" %in% names(tbl)) {
          color_map <- stats::setNames(unique(tbl$color), unique(tbl$level))
          display_tbl$color <- NULL
        }

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
          widget <- DT::formatStyle(
            widget,
            "level",
            target = "row",
            backgroundColor = DT::styleEqual(names(color_map), unname(color_map)),
            color = DT::styleEqual(names(color_map), rep("#000000", length(color_map)))
          )
        }

        widget
      })
    },
    register_downloads = function(result, output, session, shared, i18n) {
      output$download_heaping_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("heaping_results_%s.csv", timestamp)
        },
        content = function(file) {
          latest <- shared$last_result()
          shiny::req(latest)
          utils::write.csv(latest$table, file, row.names = FALSE)
        }
      )
    }
  ))
}
