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
  cat(sprintf("[DATA_CONTEXT][%s] Initialising shared data helpers\n", module_id))

  ns <- session$ns

  log_dataset <- function(tag, df, n_show = 3) {
    if (is.null(df)) {
      cat(sprintf("[%s] Dataset is NULL\n", tag))
      return()
    }
    classes <- paste(class(df), collapse = ", ")
    cols <- if (!is.null(colnames(df))) paste(colnames(df), collapse = ", ") else "<no colnames>"
    cat(sprintf("[%s] class: %s | rows: %s | cols: %s | names: %s\n",
      tag,
      classes,
      ifelse(is.null(nrow(df)), "NA", nrow(df)),
      ifelse(is.null(ncol(df)), "NA", ncol(df)),
      cols
    ))
    if (nrow(df) > 0) {
      preview <- capture.output(print(utils::head(df, n_show)))
      cat(sprintf("[%s] head:\n%s\n", tag, paste(preview, collapse = "\n")))
    }
  }

  data_in <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  raw_storage_key <- paste0("raw__", module_id)
  data_origin <- reactiveVal("none")
  group_selection_passed <- reactiveVal(FALSE)
  selected_grouping_vars <- reactiveVal(character(0))

  sample_data <- if (is.null(sample_loader)) {
    handle_sample_data(i18n)
  } else {
    reactive({
      cat(sprintf("[DATA_CONTEXT][%s] Loading custom sample data\n", module_id))
      sample <- sample_loader()
      cat(sprintf("[DATA_CONTEXT][%s] Sample data loaded | class: %s | rows: %s | cols: %s\n",
        module_id,
        paste(class(sample), collapse = ", "),
        ifelse(is.null(sample), "NULL", nrow(sample)),
        ifelse(is.null(sample), "NULL", ncol(sample))
      ))
      sample
    })
  }

  uploaded_data <- reactive({
    req(input$file1)
    cat(sprintf("[DATA_CONTEXT][%s] Upload triggered: %s\n", module_id, input$file1$name))
    handle_file_upload(input, i18n)
  })

  observeEvent(uploaded_data(), {
    df <- uploaded_data()
    cat(sprintf(
      "[DATA_CONTEXT][%s] Loaded uploaded data -> rows: %d, cols: %d\n",
      module_id, nrow(df), ncol(df)
    ))
    raw_data(df)
    session$userData[[raw_storage_key]] <- df
    data_in(df)
    data_origin("upload")
    group_selection_passed(FALSE)
    selected_grouping_vars(character(0))
  })

  observeEvent(input$use_sample_data, {
    cat(sprintf("[DATA_CONTEXT][%s] Sample data requested\n", module_id))
    sample_df <- sample_data()
    cat(sprintf("[DATA_CONTEXT][%s] Assigning sample data | class: %s | rows: %s | cols: %s\n",
      module_id,
      paste(class(sample_df), collapse = ", "),
      ifelse(is.null(sample_df), "NULL", nrow(sample_df)),
      ifelse(is.null(sample_df), "NULL", ncol(sample_df))
    ))
    raw_data(sample_df)
    session$userData[[raw_storage_key]] <- sample_df
    data_in(sample_df)
    data_origin("sample")
    group_selection_passed(FALSE)
    selected_grouping_vars(character(0))
  })

  observe({
    current <- data_in()
    log_dataset(sprintf("[DATA_CONTEXT][%s] data_in reactiveVal updated", module_id), current)
  })

  observe({
    raw <- raw_data()
    log_dataset(sprintf("[DATA_CONTEXT][%s] raw_data reactiveVal updated", module_id), raw)
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
    cat(sprintf("[DATA_CONTEXT][%s] Running validation checks\n", module_id))
    validateData(data_in(), i18n)
  })

  output$validation_summary <- renderUI({
    req(group_selection_passed())
    cat(sprintf("[DATA_CONTEXT][%s] Rendering validation summary\n", module_id))
    displayValidationResults(validation_details(), i18n)
  })

  output$validation_table_ui <- renderUI({
    req(group_selection_passed())
    DT::dataTableOutput(ns("validation_table"))
  })

  output$validation_table <- DT::renderDT({
    req(group_selection_passed())
    DT::datatable(
      validation_details(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "tip"
      )
    )
  })

  grouping_dropdowns <- setup_grouping_dropdowns(selected_grouping_vars, data_in)

  output$grouping_controls <- renderUI({
    req(group_selection_passed())
    dropdowns <- grouping_dropdowns()
    if (is.null(dropdowns) || length(dropdowns) == 0) {
      return(shiny::span(
        class = "simple-module-log",
        i18n$t("No grouping variables selected. Analysis will run on the entire dataset.")
      ))
    }

    shiny::div(
      class = "simple-module-grouping ui form",
      lapply(dropdowns, shiny::div, class = "field")
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
    if (!group_selection_passed()) {
      return(stats::na.omit(unique(data_in()$.id))[1])
    }

    if (length(selected_grouping_vars()) == 0) {
      return(stats::na.omit(unique(data_in()$.id))[1])
    }

    get_current_group_id(selected_grouping_vars, data_in, input)
  })

  filtered_data <- reactive({
    req(data_in())
    df <- data_in()
    gid <- active_group_id()
    if (is.null(gid) || !".id" %in% names(df)) {
      return(df)
    }
    dplyr::filter(df, .data$.id == gid)
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
