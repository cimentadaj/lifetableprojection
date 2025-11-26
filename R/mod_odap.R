#' Get WPP Country Code from Country Name
#'
#' Dynamically retrieves country code from WPP package UNlocations dataset.
#'
#' @param country_name Character. Name of the country.
#' @return Numeric country code or NULL if not found.
#' @noRd
get_wpp_country_code <- function(country_name) {
  if (is.null(country_name) || country_name == "") {
    return(NULL)
  }

  tryCatch({
    # Load UNlocations from wpp2024 package
    data("UNlocations", package = "wpp2024", envir = environment())

    # Lookup country code
    code <- UNlocations$country_code[UNlocations$name == country_name]

    if (length(code) > 0) {
      return(code[1])
    } else {
      warning("Country '", country_name, "' not found in WPP UNlocations.")
      return(NULL)
    }
  }, error = function(e) {
    warning("Could not load WPP UNlocations: ", e$message)
    return(NULL)
  })
}


#' Old-Age Population Redistribution Module UI
#'
#' @param i18n Translator helper used for labels.
#' @noRd
odap_module_ui <- function(i18n) {
  ns <- shiny::NS("odap")

  shinyjs::hidden(
    shiny::div(
      id = "odap_module_page",
      # CSS styling
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .odap-main {
            max-width: 1400px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 3rem;
          }

          .odap-step {
            width: 100%;
          }
          .odap-step.main-content {
            display: block;
          }
          .odap-hero {
            margin-bottom: 1.5rem;
            padding: 0 1rem;
          }
          .odap-hero h1 {
            margin: 0 0 0.35rem 0;
            font-size: 2rem;
            color: #1b1c1d;
          }
          .odap-hero p {
            margin: 0;
            color: #425466;
            max-width: 100%;
          }
          .info-box {
            border: 1px solid #ddd;
            border-radius: 12px;
            padding: 24px;
            margin: 0 0 24px;
            text-align: left;
            color: #1b1c1d;
            background: #ffffff;
            box-shadow: 0 14px 32px rgba(15, 23, 42, 0.08);
          }
          .info-box h1 {
            margin-top: 0;
            color: #1b6ec2;
            font-size: 1.65rem;
          }
          .info-box h3 {
            margin-top: 1rem;
          }
          .info-box p {
            font-size: 1rem;
            color: #4a5568;
          }
          .validation-results {
            text-align: center;
            font-size: 15px;
            color: #555;
            font-family: 'Arial', sans-serif;
            font-weight: bold;
            margin-top: 1rem;
          }
          .button-container,
          .button-container-file {
            display: flex;
            justify-content: center;
            gap: 10px;
          }
          .button-container {
            margin-top: 1.5rem;
          }
          .button-container-file .ui.button,
          .button-container .ui.button {
            min-width: 180px;
            font-weight: 600;
          }
          .button-container-file {
            display: flex;
            justify-content: center;
            gap: 10px;
          }
          .button-container-file .ui-ss-input {
            display: flex;
            align-items: center;
            min-width: 260px;
          }
          .button-container-file .ui.button {
            display: flex;
            align-items: center;
            justify-content: center;
            height: 38px;
          }
          .odap-upload-log {
            margin-top: 0.75rem;
            text-align: center;
            color: #4a5568;
            font-weight: 500;
          }
        "))
      ),
      shiny::tags$script(shiny::HTML("
        $(document).ready(function() {
          // Initialize Semantic UI tooltips
          $('[data-tooltip]').popup({
            hoverable: true,
            delay: { show: 300, hide: 100 }
          });
        });
      ")),
      shiny::div(
        class = "odap-main",

        # STEP 1: Data Upload
        shiny::div(
          id = ns("data_step"),
          class = "odap-step main-content",
          style = "width: 74%; margin: 0 auto;",
          shiny::div(
            class = "odap-hero",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("odap_back_button"))
            ),
            shiny::uiOutput(ns("odap_hero_content"))
          ),
          shiny::div(
            class = "info-box",
            shiny.semantic::tabset(
              tabs = list(
                list(
                  menu = i18n$t("Upload Instructions"),
                  content = shiny::div(
                    shiny::uiOutput(ns("odap_info_box_content")),
                    rhandsontable::rHandsontableOutput(ns("odap_sample_table"), width = "100%", height = 210),
                    shiny::uiOutput(ns("odap_info_box_instructions"))
                  )
                ),
                list(
                  menu = i18n$t("Method Documentation"),
                  content = shiny::div(
                    shiny::uiOutput(ns("odap_method_docs"))
                  )
                )
              )
            )
          ),
          shiny::tags$script(shiny::HTML("
            $(document).ready(function() {
              $('div.ui.left.action.input.ui-ss-input').css('display', 'flex');
            });
          ")),
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
            shiny::uiOutput(ns("odap_sample_button"))
          ),
          shiny::div(
            class = "odap-upload-log",
            shiny::uiOutput(ns("upload_log"))
          ),
          shiny::div(
            class = "validation-results",
            shiny::uiOutput(ns("validation_summary")),
            shiny::uiOutput(ns("validation_table_ui"))
          ),
          shiny::div(
            class = "button-container",
            shiny::uiOutput(ns("odap_continue_ui"))
          )
        ),

        # STEP 2: Analysis
        shiny::div(
          id = ns("analysis_step"),
          class = "odap-step",
          style = "display: none;",
            shiny::div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              shiny::uiOutput(ns("odap_back_to_upload_button"))
            ),
            untheme::sidebar_layout_responsive(
              # LEFT SIDEBAR: Controls
              list(
                children = shiny::div(
                  class = "ui form",
                  shiny::uiOutput(ns("odap_controls"))
                )
              ),
              # RIGHT MAIN: Plot area
              shiny::div(
                style = "flex: 1; min-width: 0;",
                shiny::uiOutput(ns("odap_group_selectors")),
                shiny::div(
                  id = ns("plot_container"),
                  style = "display: none;",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns("odap_plot"), height = "600px", width = "100%")
                  )
                ),
                shiny::br(),
                shinyjs::hidden(
                  shiny::div(
                    id = ns("download_container"),
                    class = "button-container",
                    style = "justify-content: flex-end; margin-top: 1rem;",
                    shiny::uiOutput(ns("odap_download_button"))
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


#' Old-Age Population Redistribution Module Server
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @noRd
odap_module_server <- function(input, output, session) {

  mod_simple_module_server("odap", list(
    setup = function(input, output, session, i18n) {
      # Sample data loader
      odap_sample_loader <- function() {
        path <- system.file("extdata", "odap_sample.csv", package = "lifetableprojection")
        df <- readr::read_csv(path, show_col_types = FALSE)
        # New sample has Age, pop, nLx, name
        df <- df[order(df$Age), , drop = FALSE]
        df
      }

      # Create shared data context
      shared <- create_shared_data_context(
        "odap",
        input, output, session, i18n,
        sample_loader = odap_sample_loader,
        validation_function = validateData_opag
      )
      shared$last_result <- shiny::reactiveVal(NULL)
      download_scope_choice <- shiny::reactiveVal("single")

      # Store parameters when button is clicked to prevent auto-trigger on input changes
      shared$button_click_count <- shiny::reactiveVal(0)

      ns <- session$ns
      data_step_id <- ns("data_step")
      analysis_step_id <- ns("analysis_step")

      # Language-reactive UI outputs (Step 1)
      output$odap_back_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton("odap_back_to_modules", i18n$t("← Previous"), class = "ui grey button")
      })

      output$odap_hero_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Old-Age Population Redistribution")),
          shiny::p(i18n$t("Apply old-age population redistribution based on stable population patterns fitted to observed data."))
        )
      })

      output$odap_info_box_content <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h1(i18n$t("Data upload and validation")),
          shiny::p(i18n$t("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"))
        )
      })

      output$odap_info_box_instructions <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::p(i18n$t("Required columns: Age and pop (population counts). Age can be single-year (0-100) or abridged (5 or 10-year intervals).")),
          shiny::p(i18n$t("How to provide mortality data:")),
          shiny::tags$ul(
            shiny::tags$li(i18n$t("Include 'nLx' column (custom mortality life table)")),
            shiny::tags$li(i18n$t("Provide only Age + pop (select WPP mortality using dropdowns in the analysis step)"))
          ),
          shiny::p(shiny::tags$strong(i18n$t("Important: If you want to do the analysis by groups, you must provide nLx. Without nLx, WPP mortality selection applies to ALL groups."))),
          shiny::strong(shiny::h3(i18n$t("Ready? Click 'Browse...' to select your file or start with our sample data."))),
          shiny::p(i18n$t("Sample data includes Age, pop, and nLx (custom mortality)."))
        )
      })

      output$odap_method_docs <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::tagList(
          shiny::h3(i18n$t("ODAP Redistribution Methods")),
          shiny::p(i18n$t("ODAP (Old Age Population Adjustment) redistributes population from an open-age group to extended single ages:")),
          shiny::div(
            class = "ui relaxed divided list",
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "mono"),
              shiny::div(class = "description", i18n$t("Monotonic redistribution - preserves mortality monotonicity patterns"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "pclm"),
              shiny::div(class = "description", i18n$t("PCLM-based redistribution - uses penalized composite link model with spline smoothing"))
            )),
            shiny::div(class = "item", shiny::div(class = "content",
              shiny::div(class = "header", "uniform"),
              shiny::div(class = "description", i18n$t("Uniform redistribution - simple proportional distribution across ages"))
            ))
          ),
          shiny::h4(i18n$t("Key Parameters:")),
          shiny::div(
            class = "ui bulleted list",
            shiny::div(class = "item", shiny::tags$strong("Age_fit:"), " ", i18n$t("Ages used for fitting the graduation model")),
            shiny::div(class = "item", shiny::tags$strong("AgeInt_fit:"), " ", i18n$t("Age intervals for the fitting data")),
            shiny::div(class = "item", shiny::tags$strong("Redistribute_from:"), " ", i18n$t("Starting age for redistribution (e.g., 80+)")),
            shiny::div(class = "item", shiny::tags$strong("OAnew:"), " ", i18n$t("New open age group (e.g., extend to 100+)"))
          ),
          shiny::p(i18n$t("The module extends population data to higher ages while maintaining demographic consistency using mortality patterns from WPP or custom life tables."))
        )
      })

      output$odap_sample_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny.semantic::action_button(
          ns("use_sample_data"),
          i18n$t("Use sample data"),
          class = "ui blue button"
        )
      })

      # Sample data preview table
      sample_preview <- odap_sample_loader()
      output$odap_sample_table <- rhandsontable::renderRHandsontable({
        renderDataTable(sample_preview, i18n)
      })

      # Continue button
      output$odap_continue_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

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
      output$odap_back_to_upload_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }
        shiny::actionButton(ns("back_to_upload"), i18n$t("← Previous"), class = "ui grey button")
      })
      outputOptions(output, "odap_back_to_upload_button", suspendWhenHidden = FALSE)

      # Controls panel
      output$odap_controls <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

        # Check if we need WPP selection inputs
        df <- shared$data()
        needs_wpp <- FALSE
        needs_sex <- FALSE
        needs_year <- FALSE
        if (!is.null(df)) {
          # Check case-insensitively since backend will lowercase column names
          col_names_lower <- tolower(names(df))
          has_nlx <- "nlx" %in% col_names_lower
          has_sex_col <- "sex" %in% col_names_lower
          has_year_col <- "year" %in% col_names_lower

          # We need WPP data if we don't have nLx
          needs_wpp <- !has_nlx
          # Country is always from dropdown (never from data)
          # Only hide sex/year if columns are provided
          needs_sex <- !has_nlx && !has_sex_col
          needs_year <- !has_nlx && !has_year_col
        }

        shiny::tagList(
          shiny::div(
            shiny::div(
              style = "display: inline-flex; align-items: center; gap: 8px; margin-bottom: 4px;",
              shiny::tags$label(
                `for` = ns("method"),
                i18n$t("Redistribution method")
              ),
              shiny::tags$span(
                class = "ui circular label",
                style = "cursor: help; font-size: 0.8em; padding: 0.3em 0.5em;",
                `data-tooltip` = i18n$t("Method for redistributing old-age population. 'mono' - monotonic (preserves mortality pattern), 'pclm' - spline-based smoothing, 'uniform' - simple proportional distribution."),
                `data-position` = "right center",
                `data-variation` = "wide",
                "?"
              )
            ),
            shiny.semantic::selectInput(
              ns("method"),
              NULL,  # No label here since we have it above
              choices = c("mono", "pclm", "uniform"),
              selected = if (!is.null(input$method)) input$method else "mono"
            )
          ),
          shiny::numericInput(
            ns("redistribute_from"),
            i18n$t("Redistribute from age"),
            value = if (!is.null(input$redistribute_from)) input$redistribute_from else 80,
            min = 50,
            max = 100,
            step = 5
          ),
          shiny::numericInput(
            ns("oanew"),
            i18n$t("New open age group"),
            value = if (!is.null(input$oanew)) input$oanew else 100,
            min = 80,
            max = 110,
            step = 5
          ),
          shiny::numericInput(
            ns("age_fit_start"),
            i18n$t("Fitting age range start"),
            value = if (!is.null(input$age_fit_start)) input$age_fit_start else 60,
            min = 40,
            max = 80,
            step = 5
          ),
          shiny::numericInput(
            ns("age_fit_end"),
            i18n$t("Fitting age range end"),
            value = if (!is.null(input$age_fit_end)) input$age_fit_end else 70,
            min = 50,
            max = 90,
            step = 5
          ),

          # CONDITIONAL WPP INPUTS
          if (needs_wpp) {
            shiny::tagList(
              shiny::hr(),
              shiny::h4(i18n$t("WPP Mortality Data Selection")),
              shiny::p(i18n$t("Your data doesn't include nLx. Select which WPP mortality data to use:")),
              # Country is always shown when WPP is needed
              shiny.semantic::selectInput(
                ns("wpp_country"),
                i18n$t("Country"),
                choices = OPPPserver::get_wpp_countries(),
                selected = if (!is.null(input$wpp_country)) input$wpp_country else "India"
              ),
              # Only show sex dropdown if sex column not in data
              if (needs_sex) {
                shiny.semantic::selectInput(
                  ns("wpp_sex"),
                  i18n$t("Sex"),
                  choices = c("Male" = "M", "Female" = "F"),
                  selected = if (!is.null(input$wpp_sex)) input$wpp_sex else "M"
                )
              } else {
                shiny::p(i18n$t("Sex values will be taken from your data"))
              },
              # Only show year dropdown if year column not in data
              if (needs_year) {
                shiny::numericInput(
                  ns("wpp_year"),
                  i18n$t("Year"),
                  value = if (!is.null(input$wpp_year)) input$wpp_year else 2020,
                  min = 1950,
                  max = 2024,
                  step = 1
                )
              } else {
                shiny::p(i18n$t("Year values will be taken from your data"))
              }
            )
          },

          shiny::br(),
          shiny::actionButton(
            ns("run_analysis"),
            i18n$t("Run Redistribution"),
            class = "ui primary button"
          )
        )
      })
      outputOptions(output, "odap_controls", suspendWhenHidden = FALSE)

      # Group selectors
      output$odap_group_selectors <- shiny::renderUI({
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
            lapply(dropdowns, function(dropdown) {
              shiny::div(style = "min-width: 150px", dropdown)
            })
          )
        } else {
          NULL
        }
      })
      outputOptions(output, "odap_group_selectors", suspendWhenHidden = FALSE)

      # Download button
      output$odap_download_button <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

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
            ns("download_odap_csv"),
            i18n$t("Download results"),
            class = "ui primary button"
          )
        }
      })
      outputOptions(output, "odap_download_button", suspendWhenHidden = FALSE)

      # Download options modal
      output$download_modal_ui <- shiny::renderUI({
        if (!is.null(session$userData$language_version)) {
          session$userData$language_version()
        }

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
                    id = "radio_single",
                    shiny::tags$input(
                      type = "radio",
                      name = "download_scope_radio",
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
                    id = "radio_all",
                    shiny::tags$input(
                      type = "radio",
                      name = "download_scope_radio",
                      value = "all"
                    ),
                    shiny::tags$label(i18n$t("All groups (parallel processing)"))
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
              i18n$t("Note: Downloading all groups will process each group in parallel and create a combined ZIP file with all results.")
            ),
            shiny::tags$script(shiny::HTML(sprintf("
              $(document).ready(function() {
                $('.ui.radio.checkbox').checkbox();

                $('input[name=\"download_scope_radio\"]').on('change', function() {
                  var selectedValue = $('input[name=\"download_scope_radio\"]:checked').val();
                  $('#%s').val(selectedValue).trigger('change');
                  console.log('[MODAL] Radio changed to:', selectedValue);

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

      # Open modal observer
      shiny::observeEvent(input$open_download_modal, {
        modal_id <- ns("download_modal")
        shiny.semantic::show_modal(modal_id, session = session)
      })

      # Cancel modal observer
      shiny::observeEvent(input$cancel_download, {
        modal_id <- ns("download_modal")
        shiny.semantic::hide_modal(modal_id, session = session)
      })

      # Observer to capture radio button selection
      shiny::observeEvent(input$download_scope_js, {
        choice <- input$download_scope_js
        download_scope_choice(choice)
      })

      # Debug observer
      shiny::observe({
        scope <- input$download_scope
      })

      # Helper function to download single group
      download_single_group <- function(file) {
        latest <- shared$last_result()
        shiny::req(latest)

        df <- shared$data()
        shiny::req(df)

        # Create temporary directory
        temp_dir <- tempdir()
        temp_files <- c()

        # 1. Create CSV with redistributed data
        csv_file <- file.path(temp_dir, "odap_results.csv")

        # Get the .id_label for this group_id (ODAPbackend uses .id_label as key)
        group_id <- latest$group_id
        labels_df <- shared$labels_df()
        shiny::req(labels_df)

        group_label <- labels_df$.id_label[labels_df$.id == group_id]
        if (length(group_label) == 0) {
          stop("Could not find label for group_id: ", group_id)
        }

        # Get redistributed data using .id_label
        redist_data <- latest$result$data_out[[group_label]]
        shiny::req(redist_data)

        # Create output dataframe
        out_data <- data.frame(
          Age = redist_data$Age_out,
          Pop_original = df$pop[match(redist_data$Age_out, df$Age)],
          Pop_redistributed = redist_data$Pop_out,
          stringsAsFactors = FALSE
        )

        # Add grouping info if present
        if (".id" %in% names(df)) {
          out_data$.id <- latest$group_id
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }
        }

        # Ensure Age is numeric and sort by Age
        out_data$Age <- as.numeric(out_data$Age)
        out_data <- out_data[order(out_data$Age), ]

        utils::write.csv(out_data, csv_file, row.names = FALSE)
        temp_files <- c(temp_files, csv_file)

        # 2. Save plot as PNG using .id_label
        plot_file <- file.path(temp_dir, "odap_plot.png")
        plot_obj <- latest$result$figures[[group_label]]

        if (!is.null(plot_obj)) {
          ggplot2::ggsave(
            plot_file,
            plot = plot_obj,
            width = 10,
            height = 6,
            dpi = 300
          )
          temp_files <- c(temp_files, plot_file)
        }

        # 3. Create analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        info_text <- paste(
          "Module: ODAP (Old-Age Population) Analysis",
          "Description: This module redistributes old-age population data using mortality patterns.",
          "",
          sprintf("Method Used: %s", latest$method %||% "mono"),
          "",
          "Method Descriptions:",
          "- mono: Monotonic redistribution - preserves mortality monotonicity patterns",
          "- pclm: Penalized Composite Link Model - spline-based smoothing approach",
          "- uniform: Uniform redistribution - simple proportional distribution",
          "",
          "Parameters:",
          sprintf("- OAnew (new open age group): %s", latest$OAnew %||% "100"),
          sprintf("- Age_fit (ages to fit model): %s", paste(latest$Age_fit %||% "60:89", collapse = ", ")),
          sprintf("- AgeInt_fit (age intervals): %s", paste(latest$AgeInt_fit %||% "1", collapse = ", ")),
          sprintf("- Redistribute_from (starting age): %s", latest$Redistribute_from %||% "80"),
          "",
          sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("Variable Analyzed: pop"),
          "",
          "Note: ODAP redistributes population in the open age group to create more detailed age breakdowns.",
          sep = "\n"
        )
        writeLines(info_text, info_file)
        temp_files <- c(temp_files, info_file)

        # 4. Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)
      }

      # Helper function to download all groups
      download_all_groups <- function(file) {
        df <- shared$data()
        shiny::req(df)

        latest <- shared$last_result()
        shiny::req(latest)

        all_ids <- unique(df$.id)

        # Get current parameters from latest result
        params <- list(
          method = latest$method,
          redistribute_from = latest$redistribute_from,
          oanew = latest$oanew,
          age_fit = latest$age_fit
        )

        # Run ODAP in parallel for all groups

        if (requireNamespace("future.apply", quietly = TRUE)) {
          results <- future.apply::future_lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            # Check for sex/year columns in the data
            col_names_lower <- tolower(names(data_subset))
            has_sex_col <- "sex" %in% col_names_lower
            has_year_col <- "year" %in% col_names_lower
            has_nlx <- "nlx" %in% col_names_lower

            # Get sex/year from data if available, otherwise use UI inputs
            row_sex <- NULL
            row_year <- NULL
            wpp_name <- NULL
            wpp_country_code <- NULL

            if (!has_nlx) {
              # Get sex from data column or UI
              if (has_sex_col) {
                row_sex <- data_subset[["sex"]][1]
                if (is.na(row_sex)) row_sex <- data_subset[["Sex"]][1]
              } else {
                row_sex <- isolate(input$wpp_sex)
              }

              # Get year from data column or UI
              if (has_year_col) {
                row_year <- as.numeric(data_subset[["year"]][1])
                if (is.na(row_year)) row_year <- as.numeric(data_subset[["Year"]][1])
              } else {
                row_year <- isolate(input$wpp_year)
              }

              # Country is always from dropdown
              wpp_name <- isolate(input$wpp_country)
              wpp_country_code <- if (!is.null(wpp_name)) get_wpp_country_code(wpp_name) else NULL
            }

            tryCatch({
              result <- ODAPbackend::odap_opag(
                data_in = data_subset,
                Age_fit = params$age_fit,
                AgeInt_fit = c(params$age_fit[2] - params$age_fit[1], params$age_fit[2] - params$age_fit[1]),
                Redistribute_from = params$redistribute_from,
                OAnew = params$oanew,
                method = params$method,
                nLx = NULL,
                name = wpp_name,
                sex = row_sex,
                year = row_year,
                country_code = wpp_country_code,
                i18n = i18n
              )
              list(group_id = gid, result = result, error = NULL)
            }, error = function(e) {
              list(group_id = gid, result = NULL, error = as.character(e))
            })
          }, future.seed = TRUE)
        } else {
          results <- lapply(all_ids, function(gid) {
            data_subset <- df[df$.id == gid, , drop = FALSE]

            # Check for sex/year columns in the data
            col_names_lower <- tolower(names(data_subset))
            has_sex_col <- "sex" %in% col_names_lower
            has_year_col <- "year" %in% col_names_lower
            has_nlx <- "nlx" %in% col_names_lower

            # Get sex/year from data if available, otherwise use UI inputs
            row_sex <- NULL
            row_year <- NULL
            wpp_name <- NULL
            wpp_country_code <- NULL

            if (!has_nlx) {
              # Get sex from data column or UI
              if (has_sex_col) {
                row_sex <- data_subset[["sex"]][1]
                if (is.na(row_sex)) row_sex <- data_subset[["Sex"]][1]
              } else {
                row_sex <- isolate(input$wpp_sex)
              }

              # Get year from data column or UI
              if (has_year_col) {
                row_year <- as.numeric(data_subset[["year"]][1])
                if (is.na(row_year)) row_year <- as.numeric(data_subset[["Year"]][1])
              } else {
                row_year <- isolate(input$wpp_year)
              }

              # Country is always from dropdown
              wpp_name <- isolate(input$wpp_country)
              wpp_country_code <- if (!is.null(wpp_name)) get_wpp_country_code(wpp_name) else NULL
            }

            tryCatch({
              result <- ODAPbackend::odap_opag(
                data_in = data_subset,
                Age_fit = params$age_fit,
                AgeInt_fit = c(params$age_fit[2] - params$age_fit[1], params$age_fit[2] - params$age_fit[1]),
                Redistribute_from = params$redistribute_from,
                OAnew = params$oanew,
                method = params$method,
                nLx = NULL,
                name = wpp_name,
                sex = row_sex,
                year = row_year,
                country_code = wpp_country_code,
                i18n = i18n
              )
              list(group_id = gid, result = result, error = NULL)
            }, error = function(e) {
              list(group_id = gid, result = NULL, error = as.character(e))
            })
          })
        }

        # Create temporary directory
        temp_dir <- tempfile()
        dir.create(temp_dir)
        temp_files <- c()

        # Get labels for all groups
        lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)

        # Combine all data_out into one CSV
        all_redist_data <- list()
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$data_out)) {
            # Get the .id_label for this group_id
            group_label <- NULL
            if (!is.null(lbl_df)) {
              matching_label <- lbl_df[lbl_df$.id == res$group_id, ".id_label", drop = TRUE]
              if (length(matching_label) > 0) {
                group_label <- matching_label[1]
              }
            }

            if (is.null(group_label)) {
              group_label <- as.character(res$group_id)
            }

            # Access data_out using .id_label
            redist <- res$result$data_out[[group_label]]
            if (!is.null(redist)) {
              redist_df <- data.frame(
                .id = res$group_id,
                Age = redist$Age_out,
                Pop_redistributed = redist$Pop_out,
                stringsAsFactors = FALSE
              )
              all_redist_data[[length(all_redist_data) + 1]] <- redist_df
            }
          }
        }

        if (length(all_redist_data) > 0) {
          combined_redist <- dplyr::bind_rows(all_redist_data)

          # Merge with original data
          original_subset <- df[, c(".id", "Age", "pop"), drop = FALSE]
          names(original_subset)[names(original_subset) == "pop"] <- "Pop_original"

          out_data <- merge(original_subset, combined_redist, by = c(".id", "Age"), all = TRUE)

          # Add labels
          lbl_df <- tryCatch(shared$labels_df(), error = function(e) NULL)
          if (!is.null(lbl_df)) {
            out_data <- merge(out_data, lbl_df, by = ".id", all.x = TRUE)
          }

          # Ensure Age is numeric and sort by Age (within groups)
          out_data$Age <- as.numeric(out_data$Age)
          out_data <- out_data[order(out_data$.id, out_data$Age), ]

          # Write CSV
          csv_file <- file.path(temp_dir, "odap_results_all_groups.csv")
          utils::write.csv(out_data, csv_file, row.names = FALSE)
          temp_files <- c(temp_files, csv_file)
        }

        # Save all plots with group labels (using .id_label to access figures)
        for (res in results) {
          if (!is.null(res$result) && !is.null(res$result$figures)) {
            # Get the .id_label for this group_id
            group_label <- NULL
            if (!is.null(lbl_df)) {
              matching_label <- lbl_df[lbl_df$.id == res$group_id, ".id_label", drop = TRUE]
              if (length(matching_label) > 0) {
                group_label <- matching_label[1]
              }
            }

            if (is.null(group_label)) {
              group_label <- as.character(res$group_id)
            }

            # Access figures using .id_label
            plot_obj <- res$result$figures[[group_label]]

            if (!is.null(plot_obj)) {
              # Sanitize label for filename
              safe_label <- gsub("[^a-zA-Z0-9_-]", "_", group_label)

              plot_file <- file.path(temp_dir, sprintf("odap_plot_%s.png", safe_label))
              ggplot2::ggsave(
                plot_file,
                plot = plot_obj,
                width = 10,
                height = 6,
                dpi = 300
              )
              temp_files <- c(temp_files, plot_file)
            }
          }
        }

        # Create analysis info text file
        info_file <- file.path(temp_dir, "analysis_info.txt")
        info_text <- paste(
          "Module: ODAP (Old-Age Population) Analysis (All Groups)",
          "Description: This module redistributes old-age population data using mortality patterns.",
          "",
          sprintf("Method Used: %s", params$method %||% "mono"),
          "",
          "Method Descriptions:",
          "- mono: Monotonic redistribution - preserves mortality monotonicity patterns",
          "- pclm: Penalized Composite Link Model - spline-based smoothing approach",
          "- uniform: Uniform redistribution - simple proportional distribution",
          "",
          "Parameters:",
          sprintf("- OAnew (new open age group): %s", params$oanew %||% "100"),
          sprintf("- Age_fit (ages to fit model): %s", paste(params$age_fit %||% "60:89", collapse = ", ")),
          sprintf("- Redistribute_from (starting age): %s", params$redistribute_from %||% "80"),
          "",
          sprintf("Analysis Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("Variable Analyzed: pop"),
          sprintf("Number of Groups: %d", length(results)),
          "",
          "Note: ODAP redistributes population in the open age group to create more detailed age breakdowns.",
          sep = "\n"
        )
        writeLines(info_text, info_file)
        temp_files <- c(temp_files, info_file)

        # Create ZIP file
        zip::zip(zipfile = file, files = basename(temp_files), root = temp_dir)

        # Cleanup
        unlink(temp_dir, recursive = TRUE)
      }

      # Download handler for single group
      output$download_odap_csv <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("odap_results_%s.zip", timestamp)
        },
        content = download_single_group,
        contentType = "application/zip"
      )

      # Download handler for modal confirmation
      output$confirm_download <- shiny::downloadHandler(
        filename = function() {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          scope <- download_scope_choice()
          if (scope == "all") {
            sprintf("odap_results_all_groups_%s.zip", timestamp)
          } else {
            sprintf("odap_results_%s.zip", timestamp)
          }
        },
        content = function(file) {
          scope <- download_scope_choice()
          modal_id <- ns("download_modal")
          shiny.semantic::hide_modal(modal_id, session = session)

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

      # Data reset observers
      shiny::observeEvent(shared$data(), {
        shinyjs::show(id = data_step_id)
        shinyjs::hide(id = analysis_step_id)
        shared$last_result(NULL)
        output$odap_plot <- plotly::renderPlotly(NULL)
        shinyjs::hide(id = ns("download_container"))
      }, ignoreNULL = TRUE)

      shiny::observeEvent(shared$data_origin(), {
        origin <- shared$data_origin()
        if (origin %in% c("upload", "sample")) {
          shinyjs::show(id = data_step_id)
          shinyjs::hide(id = analysis_step_id)
          shared$last_result(NULL)
          output$odap_plot <- plotly::renderPlotly(NULL)
          shinyjs::hide(id = ns("download_container"))
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Navigation: Step 1 → Step 2
      shiny::observeEvent(input$go_to_analysis, {
        shinyjs::hide(id = data_step_id)
        shinyjs::show(id = analysis_step_id)
        shinyjs::runjs(sprintf("$('#%s').hide(); $('#%s').show();", data_step_id, analysis_step_id))
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Navigation: Step 2 → Step 1
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
      list(
        method = input$method %||% "mono",
        redistribute_from = input$redistribute_from %||% 80,
        oanew = input$oanew %||% 100,
        age_fit_start = input$age_fit_start %||% 60,
        age_fit_end = input$age_fit_end %||% 70,
        # Include WPP inputs as reactive dependencies
        wpp_country = input$wpp_country,
        wpp_sex = input$wpp_sex,
        wpp_year = input$wpp_year
      )
    },

    run = function(shared, params, input, i18n) {
      # Increment button click count to trigger renderPlotly
      shared$button_click_count(shared$button_click_count() + 1)

      # Just return params - computation will happen in renderPlotly for spinner
      # But renderPlotly will use isolate() to read inputs, preventing auto-trigger
      NULL
    },

    render = function(result, output, shared, input, i18n) {
      # THE KEY: Computation happens inside renderPlotly for spinner
      # But we use button_click_count + isolate() to prevent auto-trigger on inputs
      output$odap_plot <- plotly::renderPlotly({
        # Make reactive to button clicks AND group selection
        shared$button_click_count()  # Triggers on button click
        current_gid <- shared$active_group_id()  # Triggers on group change

        # Use isolate() to read ALL inputs without creating dependencies
        data_full <- isolate(shared$data())
        method <- isolate(input$method %||% "mono")
        redistribute_from <- isolate(input$redistribute_from %||% 80)
        oanew <- isolate(input$oanew %||% 100)
        age_fit_start <- isolate(input$age_fit_start %||% 60)
        age_fit_end <- isolate(input$age_fit_end %||% 70)

        # Validate data
        if (is.null(data_full) || nrow(data_full) == 0) {
          return(NULL)
        }

        if (!"pop" %in% names(data_full) || !"Age" %in% names(data_full)) {
          return(NULL)
        }

        # Build Age_fit parameter
        age_fit <- c(age_fit_start, age_fit_end)
        ageint_fit <- c(age_fit_end - age_fit_start, age_fit_end - age_fit_start)

        # Determine WPP parameters from data structure
        col_names_lower <- tolower(names(data_full))
        has_nlx <- "nlx" %in% col_names_lower
        has_grouping <- all(c("name", "sex", "year") %in% col_names_lower)

        wpp_name <- NULL
        wpp_sex <- NULL
        wpp_year <- NULL
        wpp_country_code <- NULL

        # Only use WPP inputs if data doesn't have nLx or grouping columns
        # ISOLATE these too!
        if (!has_nlx && !has_grouping) {
          # Country is always from dropdown
          wpp_name <- isolate(input$wpp_country)

          # Check for sex column in data
          if ("sex" %in% col_names_lower) {
            wpp_sex <- data_full[["sex"]][1]
            if (is.na(wpp_sex)) wpp_sex <- data_full[["Sex"]][1]
          } else {
            wpp_sex <- isolate(input$wpp_sex)
          }

          # Check for year column in data
          if ("year" %in% col_names_lower) {
            wpp_year <- as.numeric(data_full[["year"]][1])
            if (is.na(wpp_year)) wpp_year <- as.numeric(data_full[["Year"]][1])
          } else {
            wpp_year <- isolate(input$wpp_year)
          }

          wpp_country_code <- if (!is.null(wpp_name)) get_wpp_country_code(wpp_name) else NULL
        }

        # Run the HEAVY computation HERE (inside renderPlotly for spinner)
        odap_result <- tryCatch({
          ODAPbackend::odap_opag(
            data_in = data_full,
            Age_fit = age_fit,
            AgeInt_fit = ageint_fit,
            Redistribute_from = redistribute_from,
            OAnew = oanew,
            method = method,
            nLx = NULL,
            name = wpp_name,
            sex = wpp_sex,
            year = wpp_year,
            country_code = wpp_country_code,
            i18n = i18n
          )
        }, error = function(e) {
          list(error = paste0(i18n$t("Redistribution failed: "), e$message))
        })

        # Handle errors
        if (!is.null(odap_result$error)) {
          error_msg <- odap_result$error
          error_plot <- plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "markers",
                                         marker = list(size = 0, opacity = 0)) |>
            plotly::add_annotations(
              text = paste0("<b>", i18n$t("Error"), ":</b><br><br>", error_msg),
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
          return(error_plot)
        }

        # Save result for download handlers
        result_with_computation <- list(
          result = odap_result,
          group_id = current_gid,
          method = method,
          redistribute_from = redistribute_from,
          oanew = oanew,
          age_fit = age_fit
        )
        shared$last_result(result_with_computation)

        # Extract plot for current selected group using .id_label
        labels_df <- shared$labels_df()
        if (is.null(labels_df) || nrow(labels_df) == 0) {
          return(NULL)
        }

        group_label <- labels_df$.id_label[labels_df$.id == current_gid]
        if (length(group_label) == 0 || is.na(group_label)) {
          return(NULL)
        }

        plot_obj <- odap_result$figures[[group_label]]
        if (is.null(plot_obj)) {
          return(NULL)
        }

        plotly::ggplotly(plot_obj) |>
          plotly::config(displayModeBar = FALSE)
      })

      # Show plot and download button
      shinyjs::show(id = session$ns("plot_container"))
      shinyjs::show(id = session$ns("download_container"))
    },

    register_downloads = NULL
  ))
}
