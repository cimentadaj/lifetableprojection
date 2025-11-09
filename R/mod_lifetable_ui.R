#' Lifetable module UI
#'
#' @param i18n Translator object
#' @noRd
lifetable_module_ui <- function(i18n) {
  tagList(
    tags$head(
      tags$style(HTML("
          /* Base styles with Flexbox */
          .main-content {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
          }

          .info-box {
              border: 1px solid #ddd;
              border-radius: 5px;
              padding: 20px;
              margin: 20px 0; /* Add margin to the top and bottom */
              text-align: center;
              color: #555;
          }

          .info-box h1 {
              color: #337ab7;
          }

          .info-box p {
              font-size: 16px;
          }

          .validation-results {
              text-align: center;
              font-size: 15px;
              color: #555;
              font-family: 'Arial', sans-serif;
              font-weight: bold;
          }
          
          .content {
            max-width: 100% !important;
          }

          .well {
              background-color: #f8d7da;
              border-color: #f5c6cb;
              border-radius: 5px;
              padding: 15px;
              box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          }

          .well.success {
              background-color: #d4edda;
              border-color: #c3e6cb;
          }

          .below-main-panel {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
              margin-top: 20px;
          }

          /* Keyframe animation for fading in */
          @keyframes fadeIn {
              from { opacity: 0; }
              to { opacity: 1; }
          }

          /* Centering container for buttons */
          .button-container {
              display: flex;
              justify-content: center; /* Aligns children (buttons) in the center horizontally */
              align-items: center; /* Aligns children (buttons) in the center vertically */
              gap: 10px; /* Space between buttons */
          }

          /* Centering container for buttons */
          .button-container-file {
              display: flex;
              justify-content: center; /* Aligns children (buttons) in the center horizontally */
              gap: 10px; /* Space between buttons */
          }

          /* Apply the fadeIn animation to the icon */
          .fade-in-icon {
              animation: fadeIn 2s ease-in-out;
          }

          @media (max-width: 1024px) {
              .info-box {
                  margin-top: 40px; /* Increase margin-top to avoid header overlap */
              }

              .content {
                max-width: 90% !important;
              }
          }

          @media (max-width: 768px) {
              .info-box p, .validation-results {
                  font-size: 14px; /* Smaller font size for smaller screens */
              }

              /* Stack elements vertically on small screens */
              .main-content {
                  flex-direction: column;
                  align-items: center;
              }
          }
          @media (max-width: 480px) {
              .info-box p, .validation-results {
                  font-size: 14px; /* Even smaller font size for very small screens */
              }

              .main-content {
                  display: block;
              }

              .info-box h1, h3 {
                  font-size: 18px;
              }

              p div#content-wrapper {
                flex-direction: column !important;
              }

              .plot-container {
                width: 100% !important;
              }

              .table-container {
                width: 100% !important;
              }

              .button-container-file {
                flex-direction: column;
                flex-wrap: wrap;
                gap: 1px;
                width: 100%;
              }

              .button-container-forecast {
                flex-direction: column;
                flex-wrap: wrap;
                align-items: center;
              }

              .button-container-forecast > * {
                width: 100%;
                margin: auto;
                flex-wrap: wrap;
                align-items: center;
              }
          }

          #adjustment_pills {
            margin-bottom: 5px;
          }

          #adjustment_pills .ui.label {
            margin-right: 5px;
            margin-bottom: 5px;
            cursor: default;
          }

          #adjustment_pills .ui.label i.delete.icon {
            cursor: pointer;
          }

      .hero-section {
        text-align: center;
        padding: 3rem 1rem;
        background: linear-gradient(135deg, #f5f7fa 0%, #e4e9f2 100%);
        border-radius: 10px;
        margin-bottom: 2rem;
      }

      .hero-title {
        font-size: 2.5rem;
        color: #2c3e50;
        margin-bottom: 1rem;
      }

      .hero-subtitle {
        font-size: 1.2rem;
        color: #34495e;
        max-width: 600px;
        margin: 0 auto;
      }

      .features-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 2rem;
        padding: 2rem 0;
      }

      .feature-card {
        background: white;
        padding: 1.5rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
        transition: transform 0.2s;
      }

      .feature-card:hover {
        transform: translateY(-5px);
      }

      .feature-card i {
        font-size: 2rem;
        color: #3498db;
        margin-bottom: 1rem;
      }

      .feature-card h3 {
        color: #2c3e50;
        margin-bottom: 0.5rem;
      }

      .feature-card p {
        color: #7f8c8d;
        font-size: 0.9rem;
      }

      .data-format-section {
        background: white;
        padding: 2rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin: 2rem 0;
      }

      .action-section {
        text-align: center;
        padding: 2rem 0;
      }

      @media (max-width: 768px) {
        .hero-title {
          font-size: 2rem;
        }

        .hero-subtitle {
          font-size: 1rem;
        }

        .features-grid {
          grid-template-columns: 1fr;
        }
      }


        .module-landing {
          min-height: 100vh;
          background: white;
          padding: 2rem;
        }

        .module-header {
          text-align: center;
          margin-bottom: 4rem;
          padding: 2rem 0;
        }

        .module-title {
          font-size: 3rem;
          color: #1a237e;
          font-weight: 700;
          margin-bottom: 1rem;
          letter-spacing: -0.5px;
        }

        .module-subtitle {
          font-size: 1.25rem;
          color: #546e7a;
          max-width: 600px;
          margin: 0 auto;
          line-height: 1.6;
        }

        .modules-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
          gap: 2rem;
          max-width: 1200px;
          margin: 0 auto;
        }

        .module-card {
          background: white;
          border-radius: 16px;
          padding: 2rem;
          box-shadow: 0 4px 20px rgba(0,0,0,0.05);
          transition: all 0.3s ease;
          border: 1px solid rgba(0,0,0,0.05);
          cursor: pointer;
        }

        .module-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.1);
        }

        .module-icon {
          width: 60px;
          height: 60px;
          background: #e3f2fd;
          border-radius: 12px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 1.5rem;
        }

        .module-icon i {
          font-size: 24px;
          color: #1976d2;
        }

        .module-name {
          font-size: 1.5rem;
          color: #1a237e;
          margin-bottom: 1rem;
          font-weight: 600;
        }

        .module-description {
          color: #546e7a;
          line-height: 1.6;
          margin-bottom: 1.5rem;
          font-size: 1rem;
        }

        .module-status {
          display: inline-flex;
          align-items: center;
          padding: 0.5rem 1rem;
          border-radius: 20px;
          font-size: 0.875rem;
          font-weight: 500;
        }

        .status-active {
          background: #e8f5e9;
          color: #2e7d32;
        }

        .status-coming {
          background: #fff3e0;
          color: #ef6c00;
        }

        @media (max-width: 768px) {
          .module-title {
            font-size: 2.5rem;
          }
          .modules-grid {
            grid-template-columns: 1fr;
            padding: 0 1rem;
          }
        }

        .button-container-adjustment {
          margin-bottom: 20px;
        }

        .button-group {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          align-items: center;
        }

        .button-group .ui.button,
        #adjustment_pills .ui.label {
          height: 36px;
          line-height: 34px;
          padding: 0 15px;
          font-size: 14px;
          border-radius: 4px;
          margin: 0;
          box-sizing: border-box;
        }

        #adjustment_pills {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
        }

        #adjustment_pills .ui.label {
          background-color: #e0e1e2;
          color: rgba(0,0,0,.6);
          font-weight: 700;
          border: 1px solid #d4d4d5;
          align-items: center;
        }

        #adjustment_pills .ui.label i.delete.icon {
          cursor: pointer;
          margin-left: 8px;
          font-size: 12px;
        }

        /* Ensure consistent sizing for buttons and labels */
        .ui.button,
        .ui.label {
          box-sizing: border-box;
        }
      "))
    ),
    tags$head(
      tags$script(HTML("
          $(document).ready(function() {
            let clickTimeout;
            
            // Remove any existing click handlers first
            $(document).off('click', '#toggle_advanced');
            
            // Add debounced click handler
            $(document).on('click', '#toggle_advanced', function() {
              // Clear any pending timeouts
              if (clickTimeout) clearTimeout(clickTimeout);
              
              // Set a new timeout
              clickTimeout = setTimeout(function() {
                console.log('Button clicked');
                console.log('Current button text:', $('#toggle_advanced').text());
                console.log('Current visibility:', $('#advanced_inputs').is(':visible'));
                
                $('#advanced_inputs').slideToggle('fast', function() {
                  var isVisible = $('#advanced_inputs').is(':visible');
                  console.log('After toggle - visibility:', isVisible);
                  console.log('Sending visibility state to Shiny');
                  Shiny.setInputValue('lt_advanced_is_visible', isVisible);
                });
              }, 100); // 100ms debounce
            });
          });
        "))
    ),
    div(
      class = "main-content",
      hidden(
        div(
          id = "lifetable_landing_page",
          # Dynamic UI elements that update with language changes
          uiOutput("lifetable_landing_hero"),
          uiOutput("lifetable_landing_features"),
          uiOutput("lifetable_landing_action")
        )
      )
    ),
    div(
      class = "main-content",
      hidden(
        div(
          id = "landing_page",
          # Dynamic UI elements that update with language changes
          uiOutput("upload_page_back_button"),
          uiOutput("upload_page_info_box"),
          tags$script(HTML("
            $(document).ready(function() {
              $('div.ui.left.action.input.ui-ss-input').css('display', 'flex');
            });
          ")),
          uiOutput("upload_page_file_buttons"),
          div(
            br(),
            div(
              class = "validation-results",
              uiOutput("validation_results")
            ),
            br(),
            div(
              class = "button-container",
              style = "display: flex; gap: 10px;",
              uiOutput("forward_step2")
            )
          )
        )
      )
    ),
    hidden(
      div(
        id = "step_adjustment",
        div(
          class = "button-container-adjustment",
          div(id = "pill-container", class = "pill-container", create_pills_ui()),
          div(
            class = "button-group",
            action_button("back_to_diagnostics", i18n$t("← Previous"), class = "ui grey button"),
            action_button("preprocessing_instructions", i18n$t("Instructions"), class = "ui blue button"),
            action_button("forward_to_lifetable", i18n$t("Next →"), class = "ui blue button")
          )
        ),
        tabset(
          id = "adjustment_tabs",
          list(
            list(
              menu = i18n$t("Smoothing"),
              content = create_adjustment_tab("smoothing", "smoothing_inputs", "smoothing_plot", i18n)
            )
          )
        )
      )
    ),
    hidden(
      div(
        id = "step_input",
        div(
          class = "button-container-forecast",
          style = "display: flex; gap: 10px;",
          action_button("back_to_adjustment", i18n$t("← Previous"), class = "ui grey button"),
          action_button("lifetable_instructions", i18n$t("Instructions"), class = "ui blue button"),
          action_button("calculate_lt", i18n$t("Calculate"), class = "ui blue button"),
          action_button("reset_lt", i18n$t("Reset Options"), class = "ui blue button"),
          uiOutput("download_button")
        ),
        br(),
        uiOutput("download_modal"),
        div(
          sidebar_layout_responsive(
            list(children = div(input_page(i18n))),
            div(
              id = "tabContent",
              uiOutput("lt_group_select_ui"),
              uiOutput("select_plots"),
              br(),
              uiOutput("render_plots")
            )
          )
        ),
        uiOutput("lt_summary_indication"),
        div(
          class = "ui container",
          style = "padding-top: 20px;",
          withSpinner(DTOutput("lt_summary_table"))
        )
      )
    )
  )
}

create_adjustment_tab <- function(tab_name, input_id, plot_id, i18n) {
  div(
    br(),
    sidebar_layout_responsive(
      list(
        children = div(
          uiOutput(input_id),
          br(),
          action_button(paste0("execute_", tab_name), i18n$t("Execute"), class = "ui blue button")
        )
      ),
      div(
        uiOutput(paste0("adjustment_group_select_ui_", tab_name)),
        withSpinner(plotlyOutput(plot_id, height = "400px"))
      )
    ),
    div(style = "padding: 10px 0;")
  )
}

create_pills_ui <- function(i18n) {
  uiOutput("adjustment_pills")
}
