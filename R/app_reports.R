#' Generate Comprehensive Analysis Report
#'
#' Creates a PDF report combining diagnostic analysis and life table results,
#' showing data quality checks, mortality rates, survival curves, and more.
#'
#' @param lt_results List containing life table results with plots and summary statistics
#' @param diagnostic_results List containing diagnostic analysis results and plots
#' @param preprocessing_results List containing preprocessing steps results and plots
#' @param group_labels Named vector mapping group IDs to their full labels
#' @param i18n An i18n object for translations
#' @return Path to the generated PDF file
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom dplyr group_by summarize arrange desc
#' @export
generate_analysis_report <- function(lt_results = NULL, diagnostic_results = NULL, preprocessing_results = NULL, group_labels = NULL, i18n = NULL) {
  # Create a temporary directory for report generation
  report_dir <- tempfile("report")
  dir.create(report_dir)
  
  # Create the R Markdown template
  rmd_file <- file.path(report_dir, "report.Rmd")
  
  # Generate the R Markdown content
  rmd_content <- create_rmd_template(lt_results, diagnostic_results, preprocessing_results, group_labels, i18n)
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
#' @param i18n An i18n object for translations
#' @return Character string containing the R Markdown template
#' @importFrom glue glue
#' @noRd
create_rmd_template <- function(lt_results, diagnostic_results, preprocessing_results, group_labels = NULL, i18n = NULL) {

  # Helper function to translate with fallback to original text
  translate <- function(text, i18n) {
    if (!is.null(i18n)) {
      tryCatch({
        return(i18n$t(text))
      }, error = function(e) {
        return(text)  # Fallback to original text if translation fails
      })
    }
    return(text)
  }


  glue::glue('---
title: {translate("Mortality Analysis Report", i18n)}
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

```{{r}}
# Helper function to translate with fallback to original text
translate <- function(text, i18n) {{
  if (!is.null(i18n)) {{
    tryCatch({{
      return(i18n$t(text))
    }}, error = function(e) {{
      return(text)  # Fallback to original text if translation fails
    }})
  }
  return(text)
}}
```

# {translate("Executive Summary", i18n)}

{translate("This report presents a comprehensive mortality analysis including data quality diagnostics and life table calculations.", i18n)}


# {translate("Data Quality Diagnostics", i18n)}

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
      sprintf("%s %s", translate("Group", i18n), group_id)
    }}
    
    cat(sprintf("## %s\\n\\n", group_label))
    
    # Get diagnostic table
    diag_table <- diagnostic_results$all_tables[[group_id]]
    
    cat(sprintf("### %s\\n\\n", translate("Diagnostic Metrics", i18n)))
    
    print(
      kable(diag_table,
            format = "latex",
            booktabs = TRUE,
            caption = sprintf("%s %s", translate("Diagnostic Metrics for", i18n), group_label),
            align = "c") %>%
        kable_styling(
          latex_options = c("hold_position"),
          position = "center",
          full_width = FALSE
        )
    )
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat(sprintf("### %s\\n\\n", translate("Diagnostic Plots", i18n)))
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
  cat(sprintf("# %s\\n\\n", translate("Preprocessing Analysis", i18n)))
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
        sprintf("%s %s", translate("Group", i18n), group_id)
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

# {translate("Life Table Analysis", i18n)}

```{{r lifetable-analysis, results="asis"}}
if (!is.null(lt_results)) {{
  for (group_id in names(lt_results$plots)) {{
    if (is.na(as.numeric(group_id))) next
    
    # Get group label
    group_label <- if (!is.null(group_labels) && group_id %in% names(group_labels)) {{
      group_labels[group_id]
    }} else {{
      sprintf("%s %s", translate("Group", i18n), group_id)
    }}
    
    cat(sprintf("## %s\\n\\n", group_label))
    
    # Summary statistics
    stats <- lt_results$summary %>%
      filter(.id == group_id) %>%
      # This is hardcoded to be message and value. Since this table
      # is coming translated per language, we do this dirty by picking
      # the 3rd and 4th columns. We also left a note in the ODAPbackend
      # saying that these column names should be kept.
      select(4, 3)
    
    cat(sprintf("### %s\\n\\n", translate("Summary Statistics", i18n)))
    
    print(
      kable(stats,
            format = "latex",
            booktabs = TRUE,
            digits = 2,
            escape = FALSE,
            caption = sprintf("%s %s", translate("Summary Statistics for", i18n), group_label),
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
    
    cat(sprintf("### %s\\n\\n", translate("Mortality Rates", i18n)))
    print(plots$nMx$nMx_plot)
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat(sprintf("### %s\\n\\n", translate("Survival Curve", i18n)))
    print(plots$lx$lx_plot)
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat(sprintf("### %s\\n\\n", translate("Death Distribution", i18n)))
    print(plots$ndx$ndx_plot)
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
    
    cat(sprintf("### %s\\n\\n", translate("Death Probabilities", i18n)))
    print(plots$nqx$nqx_plot)
    cat("\\n\\n\\\\FloatBarrier\\n\\n")
  }}
}}
```')
} 