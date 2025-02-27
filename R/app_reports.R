#' Generate Comprehensive Analysis Report
#'
#' Creates a PDF report combining diagnostic analysis and life table results,
#' showing data quality checks, mortality rates, survival curves, and more.
#'
#' @param lt_results List containing life table results with plots and summary statistics
#' @param diagnostic_results List containing diagnostic analysis results and plots
#' @param preprocessing_results List containing preprocessing steps results and plots
#' @param group_labels Named vector mapping group IDs to their full labels
#' @return Path to the generated PDF file
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom dplyr group_by summarize arrange desc
#' @export
generate_analysis_report <- function(lt_results = NULL, diagnostic_results = NULL, preprocessing_results = NULL, group_labels = NULL) {
  # Create a temporary directory for report generation
  report_dir <- tempfile("report")
  dir.create(report_dir)
  
  # Create the R Markdown template
  rmd_file <- file.path(report_dir, "report.Rmd")
  
  # Generate the R Markdown content
  rmd_content <- create_rmd_template(lt_results, diagnostic_results, preprocessing_results, group_labels)
  writeLines(rmd_content, rmd_file)
  
  # Render the report
  output_file <- file.path(report_dir, "analysis_report.pdf")
  rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    output_format = "pdf_document",
    quiet = TRUE
  )
  
  return(output_file)
}

#' Create R Markdown Template for Combined Report
#'
#' @param lt_results Life table results containing plots and summary statistics
#' @param diagnostic_results Diagnostic analysis results and plots
#' @param preprocessing_results List containing preprocessing steps results and plots
#' @param group_labels Named vector mapping group IDs to their full labels
#' @return Character string containing the R Markdown template
#' @importFrom glue glue
#' @noRd
create_rmd_template <- function(lt_results, diagnostic_results, preprocessing_results, group_labels = NULL) {
  glue::glue('---
title: Mortality Analysis Report
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    latex_engine: xelatex
header-includes: |
    \\usepackage{{booktabs}}
    \\usepackage{{longtable}}
    \\usepackage{{float}}
    \\usepackage{{array}}
    \\usepackage{{placeins}}
    \\setlength{{\\aboverulesep}}{{0.2em}}
    \\setlength{{\\belowrulesep}}{{0.2em}}
    \\setlength{{\\extrarowheight}}{{.5em}}
---

```{{r setup, include=FALSE}}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 6
)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
```

# Executive Summary

This report presents a comprehensive mortality analysis including data quality diagnostics and life table calculations.


# Data Quality Diagnostics

```{{r diagnostics-analysis, results="asis"}}
if (!is.null(diagnostic_results)) {{
  # Process tables first
  group_ids <- names(diagnostic_results$all_tables)
  for (group_id in group_ids) {{
    if (is.na(as.numeric(group_id))) next
    
    # Get group label
    group_label <- if (!is.null(group_labels) && group_id %in% names(group_labels)) {{
      group_labels[group_id]
    }} else {{
      sprintf("Group %s", group_id)
    }}
    
    cat(sprintf("## %s\\n\\n", group_label))
    
    # Get diagnostic table
    diag_table <- diagnostic_results$all_tables[[group_id]]
    if ("color" %in% names(diag_table)) {{
      diag_table <- select(diag_table, -Color)
    }}
    
    cat("### Diagnostic Metrics\\n\\n")
    
    print(
      kable(diag_table,
            format = "latex",
            booktabs = TRUE,
            caption = sprintf("Diagnostic Metrics for %s", group_label),
            align = "c") %>%
        kable_styling(
          latex_options = c("hold_position"),
          position = "center",
          full_width = FALSE
        )
    )
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat("### Diagnostic Plots\\n\\n")
    # Print plots by type
    plot_types <- names(diagnostic_results$all_plots$ggplot)
    for (plot_type in plot_types) {{
        print(diagnostic_results$all_plots$ggplot[[plot_type]][[group_id]]$figure)
        cat("\\n\\n\\\\FloatBarrier\\n\\n")
    }}
  }}
}}
```

```{{r preprocessing-analysis, results="asis"}}
if (!is.null(preprocessing_results)) {{
  cat("# Preprocessing Analysis\\n\\n")
  # Process each preprocessing step
  for (step_name in names(preprocessing_results)) {{
    # Get step results
    step_result <- preprocessing_results[[step_name]]
    step_clean_name <- tools::toTitleCase(step_name)
    
    cat(sprintf("## %s\\n", step_clean_name))
    
    # Get all groups
    group_ids <- unique(step_result$data_output$.id)
    
    for (group_id in group_ids) {{
      if (is.na(as.numeric(group_id))) next
      
      # Get group label
      group_label <- if (!is.null(group_labels) && group_id %in% names(group_labels)) {{
        group_labels[group_id]
      }} else {{
        sprintf("Group %s", group_id)
      }}
      
      cat(sprintf("### %s\\n\\n", group_label))
      
      # For smoothing, we have different variable types
      if (step_name == "smoothing") {{
        var_names <- names(step_result$plot_result)
        for (var_name in var_names) {{
            print(step_result$plot_result[[var_name]][[group_id]]$figure)
            cat("\\n\\n\\\\FloatBarrier\\n\\n")
        }}
      }} else {{
        # For other preprocessing steps
        if (!is.null(step_result$plot_result[[group_id]]$figure)) {{
          print(step_result$plot_result[[group_id]]$figure)
          cat("\\n\\n\\\\FloatBarrier\\n\\n")
        }}
      }}
      cat("\\n\\n\\\\FloatBarrier\\n\\n")
    }}
  }}
}}
```

# Life Table Analysis

```{{r lifetable-analysis, results="asis"}}
if (!is.null(lt_results)) {{
  for (group_id in names(lt_results$plots)) {{
    if (is.na(as.numeric(group_id))) next
    
    # Get group label
    group_label <- if (!is.null(group_labels) && group_id %in% names(group_labels)) {{
      group_labels[group_id]
    }} else {{
      sprintf("Group %s", group_id)
    }}
    
    cat(sprintf("## %s\\n\\n", group_label))
    
    # Summary statistics
    stats <- lt_results$summary %>%
      filter(.id == group_id) %>%
      select(label, message, value) %>%
      mutate(
        message = tools::toTitleCase(paste0(message, "\\\\\\\\"))
      ) %>%
      select(message, value)
    
    cat("### Summary Statistics\\n\\n")
    
    print(
      kable(stats,
            format = "latex",
            booktabs = TRUE,
            col.names = c("Description", "Value"),
            digits = 2,
            escape = FALSE,
            caption = sprintf("Summary Statistics for %s", group_label),
            align = c("l", "c")) %>%
        kable_styling(
          latex_options = c("hold_position"),
          position = "center",
          full_width = FALSE
        )
    )
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    # Plots
    plots <- lt_results$plots[[group_id]]
    
    cat("### Mortality Rates\\n\\n")
    print(plots$nMx$nMx_plot + ggtitle(sprintf("Mortality Rates (nMx) - %s", group_label)))
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat("### Survival Curve\\n\\n")
    print(plots$lx$lx_plot + ggtitle(sprintf("Survival Curve (lx) - %s", group_label)))
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat("### Death Distribution\\n\\n")
    print(plots$ndx$ndx_plot + ggtitle(sprintf("Death Distribution (ndx) - %s", group_label)))
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat("### Death Probabilities\\n\\n")
    print(plots$nqx$nqx_plot + ggtitle(sprintf("Death Probabilities (nqx) - %s", group_label)))
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
  }}
}}
```
')
} 
