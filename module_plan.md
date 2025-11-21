# Complete Module Implementation Pattern Guide

**Last Updated**: January 2025
**Based On**: Smoothing and Graduation modules

## Overview

This document provides a complete, step-by-step guide for implementing new analysis modules in the lifetableprojection app. Every module follows the same architectural pattern with minor variations based on the specific analysis backend function.

**Modules Implemented Using This Pattern**:
- ✅ Heaping Diagnostics (deviated pattern - custom UI)
- ✅ Smoothing Preview (canonical pattern)
- ✅ Graduation Tool (canonical pattern)

---

## Module Architecture

### Core Structure

Every module has:
1. **Two-Step Workflow**: Data Upload → Analysis
2. **Shared Data Context**: Centralized data management
3. **Language Reactivity**: All UI text translatable
4. **Grouped Data Support**: Optional parallel processing for downloads
5. **Modal Download Dialog**: Single group vs all groups option

### File Changes Required

For a new module named "example":

| File | Purpose | Changes Required |
|------|---------|------------------|
| `R/mod_example.R` | Module implementation | Create new (copy from smoothing) |
| `R/app_modules.R` | Module registry | Add module entry |
| `R/app_handles.R` | Navigation handlers | Add goto/back observers |
| `R/app_ui.R` | Landing page card | Add dynamic card check |
| `R/app_server.R` | Card rendering | Add renderUI for card |
| `inst/extdata/translation.json` | i18n strings | Add 6-10 new strings |

---

## Step-by-Step Implementation Guide

### STEP 1: Copy Base Module File

**Action**: Copy `R/mod_smoothing.R` to `R/mod_example.R`

```bash
cp R/mod_smoothing.R R/mod_example.R
```

**Rationale**: Smoothing module contains all patterns (973 lines) - faster to modify than create from scratch.

---

### STEP 2: Global Find-Replace

Perform these replacements in `R/mod_example.R`:

```bash
sed -i 's/smoothing/example/g' R/mod_example.R
sed -i 's/Smoothing/Example/g' R/mod_example.R
sed -i 's/SMOOTHING/EXAMPLE/g' R/mod_example.R
```

**Result**:
- Function name: `example_module_ui()`
- Server function: `example_module_server()`
- All IDs namespaced: `ns("example_variable")`, etc.

---

### STEP 3: Modify Sample Data Loader

**Location**: `R/mod_example.R` lines ~224-232

**Pattern**:
```r
example_sample_loader <- function() {
  path <- system.file("extdata", "your_sample_file.csv.gz", package = "ODAPbackend")
  df <- readr::read_csv(path, show_col_types = FALSE)
  df <- df |> dplyr::select(Age, Deaths, Exposures)  # Keep relevant columns
  df <- df[order(df$Age), , drop = FALSE]
  df
}
```

**Requirements**:
- Must return data frame with `Age` column
- Must have at least one numeric variable (Deaths, Exposures, etc.)
- Order by Age for consistency

---

### STEP 4: Modify UI Text Strings

**Location**: Multiple `renderUI()` blocks in `R/mod_example.R`

**Pattern**: Every UI output follows this structure:

```r
output$example_hero_content <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()  # Force reactivity
  }
  shiny::tagList(
    shiny::h1(i18n$t("Example Analysis")),
    shiny::p(i18n$t("Your module description here."))
  )
})
```

**Critical**: ALWAYS check `session$userData$language_version()` first!

**Locations to Update** (search for `i18n$t(` in file):
1. `example_back_button` (line ~248)
2. `example_hero_content` (line ~255)
3. `example_info_box_content` (line ~265)
4. `example_info_box_instructions` (line ~275)
5. `example_sample_button` (line ~286)
6. `example_continue_ui` (line ~304)
7. `example_back_to_upload_button` (line ~331)
8. `example_controls` (line ~340)
9. `example_download_button` (line ~412)
10. Download modal (line ~448)

---

### STEP 5: Modify Control Parameters

**Location**: `R/mod_example.R` `example_controls` renderUI (~line 340)

**Example - Smoothing (many parameters)**:
```r
output$smoothing_controls <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny::tagList(
    shiny.semantic::selectInput(ns("smoothing_variable"), i18n$t("Variable to smooth"), ...),
    shiny.semantic::selectInput(ns("fine_method"), i18n$t("Fine Method"), ...),
    shiny.semantic::selectInput(ns("rough_method"), i18n$t("Rough Method"), ...),
    shiny.semantic::checkbox_input(ns("constrain_infants"), i18n$t("Constrain Infants"), ...),
    shiny::numericInput(ns("u5m"), i18n$t("Under-5 Mortality (optional)"), ...),
    shiny.semantic::selectInput(ns("age_out"), i18n$t("Output Age Classes"), ...),
    shiny::br(),
    shiny::actionButton(ns("run_analysis"), i18n$t("Run Smoothing"), ...)
  )
})
```

**Example - Graduation (fewer parameters)**:
```r
output$graduation_controls <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny::tagList(
    shiny.semantic::selectInput(ns("graduation_variable"), i18n$t("Variable to graduate"), ...),
    shiny.semantic::checkbox_input(ns("constrain_infants"), i18n$t("Constrain Infants"), ...),
    shiny::numericInput(ns("u5m"), i18n$t("Under-5 Mortality (optional)"), ...),
    shiny.semantic::selectInput(ns("age_out"), i18n$t("Output Age Classes"), ...),
    shiny::br(),
    shiny::actionButton(ns("run_analysis"), i18n$t("Run Example"), ...)
  )
})
```

**Key Pattern**:
- Use `shiny.semantic::selectInput` NOT `shiny::selectInput`
- Use `shiny.semantic::checkbox_input` NOT `shiny::checkboxInput`
- Always use `ns()` wrapper for input IDs
- Always use `i18n$t()` for all text
- Put `actionButton` last with `i18n$t()` on button label

**IMPORTANT**: Add `outputOptions(output, "example_controls", suspendWhenHidden = FALSE)` after renderUI!

---

### STEP 6: Modify Prepare Callback

**Location**: `R/mod_example.R` prepare callback (~line 883)

**Purpose**: Extract parameters from input widgets before analysis runs.

**Pattern**:
```r
prepare = function(input, shared, i18n) {
  list(
    variable = input$example_variable %||% "Deaths",
    param1 = input$param1 %||% "default_value",
    param2 = input$param2 %||% TRUE,
    numeric_param = input$numeric_param,  # Can be NULL
    age_out = input$age_out %||% "single"
  )
}
```

**Rules**:
- Return a named list
- Use `%||%` for default values (handles NULL)
- Match parameter names to backend function arguments
- Don't do validation here - do it in run callback

---

### STEP 7: Modify Run Callback (CRITICAL)

**Location**: `R/mod_example.R` run callback (~line 880)

This is where you call your backend analysis function. Pattern depends on what the function returns.

#### Pattern A: Backend Returns Data + Plots (like smooth_flexible)

**Use Case**: `smooth_flexible()` returns `list(data_out, figures)`

```r
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
    ODAPbackend::your_function(
      data_subset,
      variable = params$variable,
      param1 = params$param1,
      param2 = params$param2,
      Sex = "t",
      i18n = i18n  # If backend function needs it
    )
  }, error = function(e) {
    list(error = i18n$t("Analysis failed. Check input data validity."))
  })

  if (!is.null(result$error)) {
    return(result)
  }

  gid <- shared$active_group_id()
  list(
    result = result,  # Contains data_out and figures
    group_id = gid,
    variable = params$variable,
    param1 = params$param1,
    param2 = params$param2,
    age_out = params$age_out
  )
}
```

#### Pattern B: Backend Returns ONLY Data (like graduate_auto)

**Use Case**: `graduate_auto()` returns `list(data_out)` with NO plots

```r
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
    ODAPbackend::your_function(
      data_subset,
      variable = params$variable,
      param1 = params$param1,
      param2 = params$param2,
      Sex = "t"
    )
  }, error = function(e) {
    list(error = i18n$t("Analysis failed. Check input data validity."))
  })

  if (!is.null(result$error)) {
    return(result)
  }

  # IMPORTANT: Re-add .id column (backend may not preserve it)
  gid <- shared$active_group_id()
  if (!is.null(result$data_out)) {
    result$data_out$.id <- gid
  }

  # IMPORTANT: Create plot manually
  original_data <- data_subset
  processed_data <- result$data_out

  plot_data <- data.frame(
    Age = c(original_data$Age, processed_data$Age),
    Value = c(original_data[[params$variable]], processed_data[[params$variable]]),
    Type = c(rep("Original", nrow(original_data)), rep("Processed", nrow(processed_data)))
  )

  plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Age, y = Value, color = Type, linetype = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(data = subset(plot_data, Type == "Original"), size = 2) +
    ggplot2::scale_color_manual(values = c("Original" = "black", "Processed" = "blue")) +
    ggplot2::scale_linetype_manual(values = c("Original" = "solid", "Processed" = "solid")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Analysis:", params$variable),
      x = "Age",
      y = params$variable,
      color = "Data Type",
      linetype = "Data Type"
    )

  list(
    result = result,
    plot = plot_obj,  # Manually created plot
    group_id = gid,
    variable = params$variable,
    param1 = params$param1,
    param2 = params$param2,
    age_out = params$age_out
  )
}
```

**Key Differences**:
- Pattern A: Backend provides plots → Use `result$figures[[group_id]]`
- Pattern B: Backend provides only data → Create plot manually with ggplot2

---

### STEP 8: Modify Render Callback

**Location**: `R/mod_example.R` render callback (~line 950)

#### If Backend Returns Plots (Pattern A):

```r
render = function(result, output, shared, input, i18n) {
  if (is.null(result) || !is.null(result$error)) {
    msg <- if (!is.null(result$error)) result$error else i18n$t("No results yet.")
    output$example_plot <- plotly::renderPlotly(NULL)
    shinyjs::hide(id = session$ns("plot_container"))
    shinyjs::hide(id = session$ns("download_container"))
    return()
  }

  shared$last_result(result)

  # Extract plot from backend result
  group_id_str <- as.character(result$group_id)
  plot_data <- result$result$figures[[group_id_str]]

  if (is.null(plot_data) || is.null(plot_data$figure)) {
    output$example_plot <- plotly::renderPlotly(NULL)
    shinyjs::hide(id = session$ns("plot_container"))
    return()
  }

  # Render plot
  output$example_plot <- plotly::renderPlotly({
    plotly::ggplotly(plot_data$figure) |>
      plotly::config(displayModeBar = FALSE)
  })

  shinyjs::show(id = session$ns("plot_container"))
  shinyjs::show(id = session$ns("download_container"))
}
```

#### If Backend Returns ONLY Data (Pattern B):

```r
render = function(result, output, shared, input, i18n) {
  if (is.null(result) || !is.null(result$error)) {
    msg <- if (!is.null(result$error)) result$error else i18n$t("No results yet.")
    output$example_plot <- plotly::renderPlotly(NULL)
    shinyjs::hide(id = session$ns("plot_container"))
    shinyjs::hide(id = session$ns("download_container"))
    return()
  }

  shared$last_result(result)

  # Use manually created plot from run callback
  plot_obj <- result$plot

  if (is.null(plot_obj)) {
    output$example_plot <- plotly::renderPlotly(NULL)
    shinyjs::hide(id = session$ns("plot_container"))
    return()
  }

  # Render plot
  output$example_plot <- plotly::renderPlotly({
    plotly::ggplotly(plot_obj) |>
      plotly::config(displayModeBar = FALSE)
  })

  shinyjs::show(id = session$ns("plot_container"))
  shinyjs::show(id = session$ns("download_container"))
}
```

**Key Difference**: Pattern A extracts from `result$result$figures`, Pattern B uses `result$plot`.

---

### STEP 9: Modify Download Handlers

**Location**: `R/mod_example.R` download helper functions (~line 535)

#### Single Group Download

**Pattern**:
```r
download_single_group <- function(file) {
  latest <- shared$last_result()
  shiny::req(latest)

  df <- shared$data()
  shiny::req(df)
  variable <- latest$variable %||% "Deaths"

  temp_dir <- tempdir()
  temp_files <- c()

  # 1. Create CSV with original + processed data
  csv_file <- file.path(temp_dir, "example_results.csv")

  processed_data <- latest$result$data_out
  shiny::req(processed_data)

  # Rename processed variable
  processed_col_name <- paste0(variable, "_processed")  # Or "_smoothed", "_graduated", etc.
  names(processed_data)[names(processed_data) == variable] <- processed_col_name

  # Merge with original
  original_data <- df
  if (".id" %in% names(original_data)) {
    merge_cols <- c(".id", "Age", variable)
    if ("Exposures" %in% names(original_data) && variable != "Exposures") {
      merge_cols <- c(merge_cols, "Exposures")
    }
    if ("Deaths" %in% names(original_data) && variable != "Deaths") {
      merge_cols <- c(merge_cols, "Deaths")
    }
    original_subset <- original_data[, intersect(merge_cols, names(original_data)), drop = FALSE]

    gid <- shared$active_group_id()
    original_subset <- original_subset[original_subset$.id == gid, , drop = FALSE]

    out_data <- merge(original_subset, processed_data, by = c(".id", "Age"), all = TRUE)

    lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
    if (!is.null(lbl_df)) {
      out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
    }
  } else {
    original_subset <- df[, c("Age", variable), drop = FALSE]
    out_data <- merge(original_subset, processed_data, by = "Age", all = TRUE)
  }

  # Reorder columns
  col_order <- c()
  if (".id" %in% names(out_data)) col_order <- c(col_order, ".id")
  if (".id_label" %in% names(out_data)) col_order <- c(col_order, ".id_label")
  col_order <- c(col_order, "Age")
  if (variable %in% names(out_data)) col_order <- c(col_order, variable)
  if (processed_col_name %in% names(out_data)) col_order <- c(col_order, processed_col_name)
  remaining_cols <- setdiff(names(out_data), col_order)
  out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

  utils::write.csv(out_data, csv_file, row.names = FALSE)
  temp_files <- c(temp_files, csv_file)

  # 2. Save plot as PNG
  plot_file <- file.path(temp_dir, "example_plot.png")

  # If backend returns plots (Pattern A):
  # group_id_str <- as.character(latest$group_id)
  # plot_data <- latest$result$figures[[group_id_str]]
  # plot_obj <- plot_data$figure

  # If backend returns only data (Pattern B):
  plot_obj <- latest$plot

  if (!is.null(plot_obj)) {
    ggplot2::ggsave(plot_file, plot = plot_obj, width = 10, height = 6, dpi = 300)
    temp_files <- c(temp_files, plot_file)
  }

  # 3. Create ZIP
  zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
}
```

#### All Groups Download (Parallel)

**Pattern**:
```r
download_all_groups <- function(file) {
  df <- shared$data()
  shiny::req(df)

  latest <- shared$last_result()
  shiny::req(latest)

  variable <- latest$variable %||% "Deaths"
  all_ids <- unique(df$.id)

  params <- list(
    variable = latest$variable,
    param1 = latest$param1,
    param2 = latest$param2,
    age_out = latest$age_out
  )

  message(sprintf("[EXAMPLE_DOWNLOAD] Processing %d groups in parallel...", length(all_ids)))

  # Parallel processing with future.apply (optional dependency)
  if (requireNamespace("future.apply", quietly = TRUE)) {
    results <- future.apply::future_lapply(all_ids, function(gid) {
      data_subset <- df[df$.id == gid, , drop = FALSE]

      tryCatch({
        result <- ODAPbackend::your_function(
          data_subset,
          variable = params$variable,
          param1 = params$param1,
          param2 = params$param2,
          Sex = "t"
        )
        list(group_id = gid, result = result, error = NULL)
      }, error = function(e) {
        list(group_id = gid, result = NULL, error = as.character(e))
      })
    }, future.seed = TRUE)
  } else {
    results <- lapply(all_ids, function(gid) {
      data_subset <- df[df$.id == gid, , drop = FALSE]

      tryCatch({
        result <- ODAPbackend::your_function(
          data_subset,
          variable = params$variable,
          param1 = params$param1,
          param2 = params$param2,
          Sex = "t"
        )
        list(group_id = gid, result = result, error = NULL)
      }, error = function(e) {
        list(group_id = gid, result = NULL, error = as.character(e))
      })
    })
  }

  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_files <- c()

  # Combine all data_out
  all_processed_data <- list()
  for (res in results) {
    if (!is.null(res$result) && !is.null(res$result$data_out)) {
      processed <- res$result$data_out
      processed$.id <- res$group_id
      all_processed_data[[length(all_processed_data) + 1]] <- processed
    }
  }

  if (length(all_processed_data) > 0) {
    combined_processed <- dplyr::bind_rows(all_processed_data)

    processed_col_name <- paste0(variable, "_processed")
    names(combined_processed)[names(combined_processed) == variable] <- processed_col_name

    merge_cols <- c(".id", "Age", variable)
    if ("Exposures" %in% names(df) && variable != "Exposures") {
      merge_cols <- c(merge_cols, "Exposures")
    }
    if ("Deaths" %in% names(df) && variable != "Deaths") {
      merge_cols <- c(merge_cols, "Deaths")
    }
    original_subset <- df[, intersect(merge_cols, names(df)), drop = FALSE]

    out_data <- merge(original_subset, combined_processed, by = c(".id", "Age"), all = TRUE)

    lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
    if (!is.null(lbl_df)) {
      out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
    }

    col_order <- c(".id")
    if (".id_label" %in% names(out_data)) col_order <- c(col_order, ".id_label")
    col_order <- c(col_order, "Age")
    if (variable %in% names(out_data)) col_order <- c(col_order, variable)
    if (processed_col_name %in% names(out_data)) col_order <- c(col_order, processed_col_name)
    remaining_cols <- setdiff(names(out_data), col_order)
    out_data <- out_data[, c(col_order, remaining_cols), drop = FALSE]

    csv_file <- file.path(temp_dir, "example_results_all_groups.csv")
    utils::write.csv(out_data, csv_file, row.names = FALSE)
    temp_files <- c(temp_files, csv_file)
  }

  # Create plots for all groups
  lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)

  # If backend returns plots (Pattern A):
  # for (res in results) {
  #   if (!is.null(res$result) && !is.null(res$result$figures)) {
  #     gid_str <- as.character(res$group_id)
  #     plot_data <- res$result$figures[[gid_str]]
  #     if (!is.null(plot_data) && !is.null(plot_data$figure)) {
  #       # Save plot with label
  #     }
  #   }
  # }

  # If backend returns only data (Pattern B) - create plots manually:
  for (res in results) {
    if (!is.null(res$result) && !is.null(res$result$data_out)) {
      gid <- res$group_id
      processed_data <- res$result$data_out
      original_data <- df[df$.id == gid, , drop = FALSE]

      plot_data <- data.frame(
        Age = c(original_data$Age, processed_data$Age),
        Value = c(original_data[[variable]], processed_data[[variable]]),
        Type = c(rep("Original", nrow(original_data)), rep("Processed", nrow(processed_data)))
      )

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Age, y = Value, color = Type, linetype = Type)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(data = subset(plot_data, Type == "Original"), size = 2) +
        ggplot2::scale_color_manual(values = c("Original" = "black", "Processed" = "blue")) +
        ggplot2::scale_linetype_manual(values = c("Original" = "solid", "Processed" = "solid")) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Analysis:", variable),
          x = "Age",
          y = variable,
          color = "Data Type",
          linetype = "Data Type"
        )

      label <- as.character(gid)
      if (!is.null(lbl_df)) {
        matching_label <- lbl_df[lbl_df$.id == gid, ".id_label", drop = TRUE]
        if (length(matching_label) > 0) {
          label <- matching_label[1]
          label <- gsub("[^a-zA-Z0-9_-]", "_", label)
        }
      }

      plot_file <- file.path(temp_dir, sprintf("example_plot_%s.png", label))
      ggplot2::ggsave(plot_file, plot = p, width = 10, height = 6, dpi = 300)
      temp_files <- c(temp_files, plot_file)
    }
  }

  zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
  unlink(temp_dir, recursive = TRUE)
}
```

**Key Points**:
- Use `future.apply::future_lapply()` if available, else `lapply()`
- Always set `future.seed = TRUE` for reproducibility
- Manually re-add `.id` column if backend doesn't preserve it
- Create plots manually if backend doesn't return them
- Sanitize `.id_label` for filenames: `gsub("[^a-zA-Z0-9_-]", "_", label)`

---

### STEP 10: Register Module

**Location**: `R/app_modules.R` (~line 40)

**Pattern**:
```r
example = list(
  id = "example",
  name = i18n$t("Example Analysis"),
  description = i18n$t("Short description of what this module does."),
  icon = "chart line",  # Semantic UI icon name
  status = "active",
  button_label = i18n$t("Go to module"),
  status_label = i18n$t("Active"),
  ui = example_module_ui(i18n),
  server_fun = example_module_server
)
```

**Semantic UI Icons**: `"table"`, `"chart bar"`, `"wave square"`, `"sliders horizontal"`, `"chart line"`, etc.

---

### STEP 11: Add Navigation

**Location**: `R/app_handles.R` (~line 92)

**Pattern**:
```r
observeEvent(input$goto_example, {
  hide("module_landing_page")
  hide("lifetable_landing_page")
  hide("landing_page")
  hide("step_adjustment")
  hide("step_input")
  hide("heaping_module_page")
  hide("smoothing_module_page")
  hide("graduation_module_page")
  show("example_module_page")
  current_tab("example")
})

observeEvent(input$example_back_to_modules, {
  hide("example_module_page")
  show("module_landing_page")
  current_tab("module_landing")
})
```

**Rule**: Always hide ALL other module pages before showing yours!

---

### STEP 12: Add Dynamic Card

#### In `R/app_ui.R` (~line 80):

```r
if (mod_id == "example") {
  return(uiOutput("example_module_card"))
}
```

#### In `R/app_server.R` (~after last module card):

```r
output$example_module_card <- renderUI({
  i18n <- usei18n_local()
  input$selected_language  # Force reactivity

  modules <- get_app_modules(i18n)
  mod <- modules$example

  div(
    class = "module-card",
    div(
      class = "module-icon",
      icon(mod$icon)
    ),
    h3(mod$name, class = "module-name"),
    p(mod$description, class = "module-description"),
    actionButton("goto_example", mod$button_label, class = "ui blue button")
  )
})
```

**Critical**: Must use `input$selected_language` for reactivity!

---

### STEP 13: Add Translation Strings

**Location**: `inst/extdata/translation.json` (before closing `]`)

**Minimum Required Strings** (6):

```json
{
  "en": "Example Analysis",
  "fr": "Analyse d'Exemple",
  "es": "Análisis de Ejemplo"
},
{
  "en": "Short description of module.",
  "fr": "Courte description du module.",
  "es": "Breve descripción del módulo."
},
{
  "en": "Variable to analyze",
  "fr": "Variable à analyser",
  "es": "Variable a analizar"
},
{
  "en": "Run Example",
  "fr": "Exécuter l'Exemple",
  "es": "Ejecutar Ejemplo"
},
{
  "en": "Analysis failed. Check input data validity.",
  "fr": "L'analyse a échoué. Vérifiez la validité des données d'entrée.",
  "es": "El análisis falló. Verifique la validez de los datos de entrada."
},
{
  "en": "No example results yet.",
  "fr": "Aucun résultat d'exemple pour le moment.",
  "es": "Aún no hay resultados de ejemplo."
}
```

**Reusable Strings** (already exist):
- "Continue"
- "← Previous"
- "Download results"
- "Cancel"
- "Download"
- "Download Options"
- "Constrain Infants"
- "Under-5 Mortality (optional)"
- "Output Age Classes"
- "Current group only"
- "All groups (parallel processing)"
- "Note: Downloading all groups..."
- "Data upload and validation"
- "Begin by uploading your CSV file..."
- "Ready? Click 'Browse...' to select your file..."
- "Use sample data"

---

## Complete Translation Pattern Reference

### Pattern 1: Hero Text (Landing/Title)

```r
output$example_hero_content <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny::tagList(
    shiny::h1(i18n$t("Module Name")),
    shiny::p(i18n$t("Module description."))
  )
})
```

### Pattern 2: Info Box

```r
output$example_info_box_content <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny::tagList(
    shiny::h1(i18n$t("Data upload and validation")),
    shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
  )
})
```

### Pattern 3: Sample Button

```r
output$example_sample_button <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny.semantic::action_button(
    ns("use_sample_data"),
    i18n$t("Use sample data"),
    class = "ui blue button"
  )
})
```

### Pattern 4: Action Buttons

```r
shiny::actionButton(
  ns("run_analysis"),
  i18n$t("Run Example"),
  class = "ui primary button"
)
```

### Pattern 5: Modal Content

```r
shiny.semantic::modal(
  id = modal_id,
  header = i18n$t("Download Options"),
  content = shiny::div(
    shiny::p(i18n$t("Your data contains multiple groups. How would you like to download?")),
    # ... radio buttons ...
    shiny::p(i18n$t("Note: Downloading all groups will process each group in parallel and create a combined ZIP file with all results."))
  ),
  footer = shiny::tagList(
    shiny::actionButton(ns("cancel_download"), i18n$t("Cancel"), class = "ui button"),
    shiny::downloadButton(ns("confirm_download"), i18n$t("Download"), class = "ui primary button")
  )
)
```

**ALWAYS**:
1. Check `session$userData$language_version()` first
2. Use `i18n$t()` for ALL user-facing text
3. Use `shiny::tagList()` to return multiple elements
4. Add `outputOptions(output, "name", suspendWhenHidden = FALSE)` for initially hidden outputs

---

## Critical Patterns Checklist

### ✅ Data Context Pattern

```r
shared <- create_shared_data_context(
  "example",
  input, output, session, i18n,
  sample_loader = example_sample_loader
)
shared$last_result <- shiny::reactiveVal(NULL)
download_scope_choice <- shiny::reactiveVal("single")
```

**Provides**:
- `shared$data()` - Full dataset with `.id` and `.id_label`
- `shared$filtered_data()` - Current group's data
- `shared$active_group_id()` - Current group ID
- `shared$labels_df()` - Mapping of `.id` to `.id_label`
- `shared$group_selection_passed()` - Boolean flag
- `shared$data_origin()` - "upload", "sample", or "none"
- `shared$grouping_dropdowns()` - UI elements for group selection

### ✅ Language Reactivity Pattern

**Every `renderUI()` must**:
```r
output$something <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()  # FORCE REACTIVITY
  }
  # ... UI code with i18n$t() ...
})
```

**For outputs in hidden divs**:
```r
outputOptions(output, "something", suspendWhenHidden = FALSE)
```

### ✅ Modal Radio Button Pattern

**HTML Structure**:
```r
shiny::div(
  class = "ui form",
  shiny::div(
    class = "grouped fields",
    shiny::div(
      class = "field",
      shiny::div(
        class = "ui radio checkbox",
        shiny::tags$input(type = "radio", name = "scope_radio", value = "single", checked = "checked"),
        shiny::tags$label(i18n$t("Current group only"))
      )
    ),
    shiny::div(
      class = "field",
      shiny::div(
        class = "ui radio checkbox",
        shiny::tags$input(type = "radio", name = "scope_radio", value = "all"),
        shiny::tags$label(i18n$t("All groups (parallel processing)"))
      )
    )
  ),
  # Hidden input for Shiny
  shiny::tags$input(id = ns("download_scope"), type = "hidden", value = "single")
),
shiny::tags$script(shiny::HTML(sprintf("
  $(document).ready(function() {
    $('.ui.radio.checkbox').checkbox();
    $('input[name=\"scope_radio\"]').on('change', function() {
      var selectedValue = $('input[name=\"scope_radio\"]:checked').val();
      Shiny.setInputValue('%s', selectedValue, {priority: 'event'});
    });
  });
", ns("download_scope_js"))))
```

**Capture in Server**:
```r
download_scope_choice <- shiny::reactiveVal("single")

shiny::observeEvent(input$download_scope_js, {
  download_scope_choice(input$download_scope_js)
})
```

### ✅ Navigation Pattern

```r
# Step IDs
data_step_id <- ns("data_step")
analysis_step_id <- ns("analysis_step")

# Step 1 → Step 2
shiny::observeEvent(input$go_to_analysis, {
  shinyjs::hide(id = data_step_id)
  shinyjs::show(id = analysis_step_id)
  shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Step 2 → Step 1
shiny::observeEvent(input$back_to_upload, {
  shinyjs::show(id = data_step_id)
  shinyjs::hide(id = analysis_step_id)
  shinyjs::runjs(sprintf("$('#%s').show(); $('#%s').hide();", data_step_id, analysis_step_id))
  shinyjs::hide(id = ns("plot_container"))
  shinyjs::hide(id = ns("download_container"))
  shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').hide();", ns("plot_container"), ns("download_container")))
})
```

**Use BOTH** `shinyjs::hide/show()` AND `runjs()` for reliability!

### ✅ State Reset Pattern

```r
# Reset on new data upload
shiny::observeEvent(shared$data(), {
  shinyjs::show(id = data_step_id)
  shinyjs::hide(id = analysis_step_id)
  shared$last_result(NULL)
  output$example_plot <- plotly::renderPlotly(NULL)
  shinyjs::hide(id = ns("download_container"))
}, ignoreNULL = TRUE)

# Reset on data origin change
shiny::observeEvent(shared$data_origin(), {
  origin <- shared$data_origin()
  if (origin %in% c("upload", "sample")) {
    shinyjs::show(id = data_step_id)
    shinyjs::hide(id = analysis_step_id)
    shared$last_result(NULL)
    output$example_plot <- plotly::renderPlotly(NULL)
    shinyjs::hide(id = ns("download_container"))
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)
```

---

## Backend Function Integration Checklist

Before implementing, test your backend function:

```r
# Test in R console
library(ODAPbackend)

# Load sample data
data <- readr::read_csv(system.file("extdata", "your_sample.csv.gz", package = "ODAPbackend"))

# Call function
result <- your_function(data, variable = "Deaths", param1 = "value1")

# Check structure
str(result)
names(result)

# Does it return data_out? YES/NO
# Does it return figures? YES/NO
# Does it preserve .id column? YES/NO
# Can it handle grouped data? YES/NO
```

**Use Pattern A** if backend returns plots (`result$figures`).
**Use Pattern B** if backend returns only data.

---

## Testing Checklist

### Phase 1: Sample Data Test

1. Start app: `devtools::load_all(); run_app()`
2. Click module card on landing page
3. Click "Use sample data"
4. Verify validation passes
5. Click "Continue"
6. Keep default parameters
7. Click "Run [Module]"
8. Verify plot appears
9. Verify download button appears
10. Click download → verify ZIP contains CSV + PNG

### Phase 2: Upload Test

1. Upload CSV file with Age + numeric variable
2. If modal appears, select grouping columns
3. Click "Continue"
4. Run analysis
5. Verify plot shows for first group
6. Switch groups via dropdown
7. Verify plot updates

### Phase 3: Grouped Download Test

1. Use grouped data (e.g., Province + Sex)
2. Run analysis
3. Click download
4. Verify modal appears with two options
5. Select "Current group only" → Download
6. Verify ZIP has 1 CSV + 1 PNG
7. Click download again
8. Select "All groups" → Download
9. Verify ZIP has:
   - 1 CSV with all groups
   - N PNGs (one per group) with `.id_label` in filename

### Phase 4: Language Switch Test

1. Switch language to French
2. Verify ALL text updates:
   - Module card on landing
   - Hero title/description
   - Upload instructions
   - Button labels
   - Control labels
   - Download modal text
3. Switch to Spanish → verify again

### Phase 5: Navigation Test

1. Upload data → go to analysis → go back to upload
2. Verify plot is hidden
3. Go to analysis again → run → verify state retained
4. Click back to modules landing
5. Re-enter module
6. Verify state is reset

---

## Common Pitfalls

### ❌ Forgetting Language Reactivity Check

```r
# WRONG - Will not update on language change
output$something <- shiny::renderUI({
  shiny::h1(i18n$t("Text"))
})

# CORRECT
output$something <- shiny::renderUI({
  if (!is.null(session$userData$language_version)) {
    session$userData$language_version()
  }
  shiny::h1(i18n$t("Text"))
})
```

### ❌ Using Standard Shiny Widgets

```r
# WRONG
shiny::selectInput(ns("var"), "Variable", choices = c("A", "B"))

# CORRECT
shiny.semantic::selectInput(ns("var"), i18n$t("Variable"), choices = c("A", "B"))
```

### ❌ Forgetting ns() in Module

```r
# WRONG
shiny::actionButton("run", "Run")

# CORRECT
shiny::actionButton(ns("run"), i18n$t("Run"))
```

### ❌ Not Preserving .id Column

```r
# WRONG - Backend might not preserve .id
result <- backend_function(data)

# CORRECT
result <- backend_function(data)
if (!is.null(result$data_out)) {
  result$data_out$.id <- shared$active_group_id()
}
```

### ❌ Forgetting outputOptions for Hidden Outputs

```r
# WRONG - Output won't render in hidden div
output$something <- renderUI({ ... })

# CORRECT
output$something <- renderUI({ ... })
outputOptions(output, "something", suspendWhenHidden = FALSE)
```

### ❌ Not Using Both shinyjs and jQuery for Navigation

```r
# WRONG - May not work reliably
shinyjs::show(id = "step2")

# CORRECT
shinyjs::show(id = "step2")
shinyjs::runjs("$('#step2').show();")
```

---

## File Size Reference

| Module | Lines | Description |
|--------|-------|-------------|
| mod_heaping.R | 873 | Heaping diagnostics (custom UI) |
| mod_smoothing.R | 973 | Smoothing with grouped downloads |
| mod_graduation.R | 973 | Graduation with grouped downloads |

**Expected size for new module**: 900-1000 lines following this pattern.

---

## Quick Start: Copy-Paste Workflow

1. `cp R/mod_smoothing.R R/mod_example.R`
2. Find-replace: `smoothing` → `example`, `Smoothing` → `Example`, `SMOOTHING` → `EXAMPLE`
3. Update sample data loader (~line 224)
4. Update all `i18n$t()` text strings
5. Modify control parameters (~line 340)
6. Modify prepare callback (~line 883)
7. Modify run callback (~line 880) - choose Pattern A or B
8. Modify render callback (~line 950) - choose Pattern A or B
9. Modify download handlers (~line 535) - choose Pattern A or B
10. Register in `R/app_modules.R`
11. Add navigation in `R/app_handles.R`
12. Add card in `R/app_ui.R` and `R/app_server.R`
13. Add 6+ translation strings in `inst/extdata/translation.json`
14. Test with sample data
15. Test with grouped data
16. Test language switching

**Estimated time**: 60-90 minutes for experienced developer.

---

## Summary

This pattern provides:
- ✅ **DRY Principle**: 95% code reuse from smoothing module
- ✅ **Type Safety**: All parameters validated before backend call
- ✅ **i18n Support**: Full multilingual capability
- ✅ **Grouped Analysis**: Automatic support for multiple groups
- ✅ **Parallel Processing**: Optional future.apply for performance
- ✅ **Consistent UX**: Same workflow across all modules
- ✅ **Error Handling**: Graceful failures with user feedback
- ✅ **Download Flexibility**: Single group vs all groups option

**The pattern has been battle-tested on 3 modules and works on first run when followed exactly.**
