#' Create Shared Data Context
#'
#' Sets up the upload, validation, and grouping workflow reused across the new
#' analytic modules. Returns a list of reactive helpers that modules can use to
#' access the prepared data.
#'
#' @param module_id Character identifier used only for logging context.
#' @param input Shiny input object within the module namespace.
#' @param output Shiny output object within the module namespace.
#' @param session Shiny session object within the module namespace.
#' @param i18n Translator helper.
#' @param sample_loader Optional function returning a data.frame used when the
#'   user clicks the sample data button. Defaults to the lifetable sample data.
#' @return A list containing shared reactive values (data, grouping metadata,
#'   validation details, etc.).
#' @importFrom shiny reactiveVal reactive observeEvent observe isolate renderUI span
#' @importFrom dplyr distinct filter
#' @importFrom DT renderDT datatable dataTableOutput
#' @noRd
create_shared_data_context <- function(module_id, input, output, session, i18n, sample_loader = NULL) {
  ns <- session$ns

  data_in <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  raw_storage_key <- paste0("raw__", module_id)
  data_origin <- reactiveVal("none")
  group_selection_passed <- reactiveVal(FALSE)
  selected_grouping_vars <- reactiveVal(character(0))

  sample_data <- if (is.null(sample_loader)) {
    handle_sample_data(i18n)
  } else {
    reactive(sample_loader())
  }

  uploaded_data <- reactive({
    req(input$file1)
    handle_file_upload(input, i18n)
  })

  observeEvent(uploaded_data(), {
    df <- uploaded_data()
    raw_data(df)
    session$userData[[raw_storage_key]] <- df
    data_in(df)
    data_origin("upload")
    group_selection_passed(FALSE)
    selected_grouping_vars(character(0))
  })

  observeEvent(input$use_sample_data, {
    sample_df <- sample_data()
    if (is.null(sample_df)) {
      cat(sprintf("[DATA_CONTEXT][%s] sample data requested but loader returned NULL\n", module_id))
      return()
    }

    sample_df <- as.data.frame(sample_df)
    if (!".id" %in% names(sample_df)) {
      sample_df$.id <- 1L
    }
    if (!".id_label" %in% names(sample_df)) {
      sample_df$.id_label <- i18n$t("Sample dataset")
    }

    cat(sprintf(
      "[DATA_CONTEXT][%s] sample data loaded | rows=%s | cols=%s | unique_ids=%s\n",
      module_id, nrow(sample_df), ncol(sample_df), paste(unique(sample_df$.id), collapse = ", ")
    ))

    group_selection_passed(TRUE)
    selected_grouping_vars(character(0))

    raw_data(sample_df)
    session$userData[[raw_storage_key]] <- sample_df
    data_in(sample_df)
    data_origin("sample")

    output$modal_ui <- renderUI(NULL)
  })

  output$upload_log <- renderUI({
    origin <- data_origin()
    if (identical(origin, "none")) return(NULL)
    file_label <- if (!is.null(input$file1) && !is.null(input$file1$name)) {
      input$file1$name
    } else {
      i18n$t("Uploaded file")
    }
    shiny::span(
      class = "simple-module-log",
      sprintf(
        i18n$t("Data source: %s"),
        if (identical(origin, "sample")) i18n$t("Sample dataset") else file_label
      )
    )
  })

  # Hook group selection modal
  handle_group_selection_modal(
    input, output, session,
    data = data_in,
    group_selection_passed = group_selection_passed,
    selected_grouping_vars = selected_grouping_vars,
    i18n = i18n,
    data_raw = raw_data,
    raw_key = raw_storage_key
  )

  # Validation
  validation_details <- reactive({
    req(data_in())
    validateData(data_in(), i18n)
  })

  output$validation_summary <- renderUI({
    req(group_selection_passed())
    results <- validation_details()
    displayValidationResults(results, i18n)
  })

  output$validation_table_ui <- renderUI({
    req(group_selection_passed())
    results <- validation_details()
    if (is.null(results) || all(results$pass == "Pass")) {
      return(NULL)
    }
    DT::dataTableOutput(ns("validation_table"))
  })

  output$validation_table <- DT::renderDT({
    req(group_selection_passed())
    results <- validation_details()
    if (is.null(results) || all(results$pass == "Pass")) {
      return(NULL)
    }
    DT::datatable(
      results,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "tip"
      )
    )
  })

  grouping_dropdowns <- setup_grouping_dropdowns(selected_grouping_vars, data_in, ns = session$ns)
  setup_grouping_dropdown_observers(input, selected_grouping_vars)

  output$grouping_controls <- renderUI({
    status <- group_selection_passed()
    cat(sprintf("[GROUPING_UI][%s] render request | group_selection_passed=%s\n", module_id, status))
    req(status)
    dropdowns <- grouping_dropdowns()
    drop_count <- if (is.null(dropdowns)) 0L else length(dropdowns)
    cat(sprintf("[GROUPING_UI][%s] render | dropdown_count=%s\n", module_id, drop_count))
    if (!is.null(dropdowns) && length(dropdowns) > 0) {
      cat(sprintf("[GROUPING_UI][%s] rendering %s column dropdowns\n", module_id, length(dropdowns)))
      return(shiny::div(
        class = "simple-module-grouping ui form",
        lapply(dropdowns, shiny::div, class = "field")
      ))
    }

    df <- data_in()
    if (!is.null(df) && ".id" %in% names(df)) {
      id_values <- unique(df$.id)
      if (length(id_values) > 0) {
        id_values_chr <- as.character(id_values)
        if (".id_label" %in% names(df)) {
          label_df <- dplyr::distinct(df, .id, .id_label)
          label_df <- label_df[match(id_values, label_df$.id), , drop = FALSE]
          labels <- if (!is.null(label_df$.id_label)) label_df$.id_label else id_values_chr
        } else {
          labels <- id_values_chr
        }
        choices <- stats::setNames(id_values_chr, labels)
        current <- input[[ns("group_id_select")]]
        if (is.null(current) || !current %in% id_values_chr) {
          current <- id_values_chr[1]
        }
        cat(sprintf(
          "[GROUPING_UI][%s] rendering fallback group selector | choices=%s | selected=%s\n",
          module_id, paste(labels, collapse = ", "), as.character(current)
        ))
        return(shiny::div(
          class = "simple-module-grouping ui form",
          shiny::div(
            class = "field",
            shiny.semantic::selectInput(
              ns("group_id_select"),
              i18n$t("Group to view"),
              choices = choices,
              selected = current
            )
          )
        ))
      }
    }

    cat(sprintf("[GROUPING_UI][%s] no grouping controls available; showing info message\n", module_id))
    shiny::span(
      class = "simple-module-log",
      i18n$t("No grouping variables selected. Analysis will run on the entire dataset.")
    )
  })

  labels_df <- reactive({
    req(data_in())
    suppressWarnings({
      dplyr::distinct(data_in(), .data$.id, .data$.id_label)
    })
  })

  available_groups <- reactive({
    req(labels_df())
    as.list(stats::setNames(labels_df()$.id, labels_df()$.id_label))
  })

  active_group_id <- reactive({
    req(data_in())
    df <- data_in()
    if (!group_selection_passed()) {
      ids <- if (".id" %in% names(df)) stats::na.omit(unique(df$.id)) else numeric(0)
      gid <- if (length(ids) > 0) ids[[1]] else NA
      cat(sprintf("[DATA_CONTEXT][%s] active_group_id (no grouping) -> %s\n", module_id, as.character(gid)))
      return(gid)
    }

    if (length(selected_grouping_vars()) == 0) {
      ids <- if (".id" %in% names(df)) stats::na.omit(unique(df$.id)) else numeric(0)
      gid <- if (length(ids) > 0) ids[[1]] else NA
      cat(sprintf("[DATA_CONTEXT][%s] active_group_id (no vars) -> %s\n", module_id, as.character(gid)))
      return(gid)
    }

    gid <- get_current_group_id(selected_grouping_vars, data_in, input)
    cat(sprintf("[DATA_CONTEXT][%s] active_group_id (selection) -> %s\n", module_id, as.character(gid)))
    gid
  })

  filtered_data <- reactive({
    req(data_in())
    df <- data_in()
    gid <- active_group_id()
    cat(sprintf("[DATA_CONTEXT][%s] filtered_data | gid: %s | rows: %s | cols: %s\n", module_id, as.character(gid), nrow(df), ncol(df)))
    if (is.null(gid) || length(gid) == 0 || all(is.na(gid)) || !".id" %in% names(df)) {
      cat(sprintf("[DATA_CONTEXT][%s] filtered_data -> returning full dataset (missing gid/.id)\n", module_id))
      return(df)
    }
    out <- dplyr::filter(df, .data$.id == gid[[1]])
    cat(sprintf("[DATA_CONTEXT][%s] filtered_data -> rows after filter: %s\n", module_id, nrow(out)))
    if (nrow(out) == 0) {
      cat(sprintf("[DATA_CONTEXT][%s] filtered_data -> fallback to full dataset\n", module_id))
      return(df)
    }
    out
  })

  list(
    data = data_in,
    data_origin = data_origin,
    group_selection_passed = group_selection_passed,
    selected_grouping_vars = selected_grouping_vars,
    grouping_dropdowns = grouping_dropdowns,
    validation_details = validation_details,
    labels_df = labels_df,
    available_groups = available_groups,
    active_group_id = active_group_id,
    filtered_data = filtered_data,
    sample_data = sample_data,
    raw_data = raw_data
  )
}
