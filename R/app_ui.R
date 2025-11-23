#' The application User-Interface for input page
#'
#' @param i18n Translator object for internationalization
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @importFrom shiny downloadButton
#' @noRd
input_page <- function(i18n) {
  div(
    class = "ui form",
    # Basic Inputs as in initial setup
    create_field_set("", i18n$t("Desired Open Age Group"), "input_oanew", seq(70, 100, by = 5), 100),
    create_field_set("", i18n$t("Output Age Classes"), "input_age_out", c("single", "abridged"), "single"),
    uiOutput("sex_to_use"),
    uiOutput("ages_to_use"),
    uiOutput("extrap_from_data"),
    br(),
    # Advanced Inputs - Initially Hidden
    div(
      id = "advanced_inputs",
      style = "display: none;",
      div(
        class = "ui two column grid",
        div(
          class = "column",
          create_field_set("", i18n$t("Extrapolation Law"), "input_extrapLaw", EXTRAP_LAWS, EXTRAP_LAWS[1]),
          create_field_set("", i18n$t("Lifetable Radix"), "input_radix", input_selected = 100000, numeric_input = TRUE)
        ),
        div(
          class = "column",
          create_field_set("", i18n$t("Sex Ratio at Birth"), "input_srb", input_selected = 1.05, numeric_input = TRUE),
          create_field_set("", i18n$t("a(0) Rule"), "input_a0rule", c("Andreev-Kingkade", "Coale-Demeny"), "Andreev-Kingkade"),
          create_field_set("", i18n$t("a(x) Method"), "input_axmethod", c("UN (Greville)", "PASEX"), "UN (Greville)")
        )
      )
    ),
    # Dropdown to toggle Advanced Inputs
    action_button("toggle_advanced", i18n$t("Show Advanced Options"), class = "ui button"),
    br(),
    br(),
    uiOutput("download_buttons")
  )
}


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div HTML h1 p uiOutput br plotOutput strong h3 tagList actionButton
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom plotly plotlyOutput
#' @importFrom shiny.semantic main_panel action_button selectInput file_input sidebar_panel tabset
#' @importFrom untheme fluidUnTheme sidebar_layout_responsive
#' @importFrom DT DTOutput formatRound JS
#' @importFrom shinycssloaders withSpinner
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny.fluent TooltipHost Image
#' @importFrom shiny.i18n Translator
#' @importFrom rintrojs introjsUI
#' @noRd
app_ui <- function(request) {
  options(shiny.suppressCompleteHTMLWarning = TRUE)
  # Create translator object
  i18n <- usei18n_local()

  modules <- get_app_modules(i18n)
  module_cards <- lapply(names(modules), function(mod_id) {
    mod <- modules[[mod_id]]
    # Make active module cards dynamic for language switching
    if (mod_id == "lifetable") {
      return(uiOutput("lifetable_module_card"))
    }
    if (mod_id == "heaping") {
      return(uiOutput("heaping_module_card"))
    }
    if (mod_id == "smoothing") {
      return(uiOutput("smoothing_module_card"))
    }
    if (mod_id == "graduation") {
      return(uiOutput("graduation_module_card"))
    }
    if (mod_id == "odap") {
      return(uiOutput("odap_module_card"))
    }

    # Keep other cards static
    if (identical(mod$status, "active")) {
      div(
        class = "module-card",
        div(
          class = "module-icon",
          icon(mod$icon)
        ),
        h3(mod$name, class = "module-name"),
        p(mod$description, class = "module-description"),
        actionButton(paste0("goto_", mod$id), mod$button_label, class = "ui blue button")
      )
    } else {
      div(
        class = "module-card",
        style = "opacity: 0.7; pointer-events: none;",
        div(
          class = "module-icon",
          icon(mod$icon)
        ),
        h3(mod$name, class = "module-name"),
        p(mod$description, class = "module-description"),
        div(
          class = sprintf("module-status %s", ifelse(identical(mod$status, "coming_soon"), "status-coming", "status-active")),
          span(mod$status_label)
        )
      )
    }
  })
  module_cards <- if (length(module_cards) > 0) do.call(tagList, module_cards) else tagList()
  module_sections <- lapply(modules, `[[`, "ui")
  module_sections <- Filter(Negate(is.null), module_sections)
  module_sections <- if (length(module_sections) > 0) do.call(tagList, module_sections) else tagList()

  fluidUnTheme(
    useShinyjs(),
    introjsUI(),
    shiny.i18n::usei18n(i18n),
    LATEX_PRE_TAGS,
    tags$script(JS_CODE_SCREEN_SIZE),
    tags$style(HTML("
      /* Documentation Container */
      .documentation-container {
        max-width: 900px;
        margin: 40px auto;
        padding: 40px;
        background: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        position: relative;
      }

      /* Documentation Header */
      .documentation-header {
        position: relative;
        margin-bottom: 32px;
        padding-bottom: 24px;
        border-bottom: 3px solid #2185d0;
      }

      .documentation-title {
        font-size: 2.5rem;
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
        display: flex;
        align-items: center;
      }

      .documentation-title i {
        color: #2185d0;
      }

      /* Documentation Sections */
      .documentation-section {
        margin-bottom: 32px;
      }

      .documentation-intro {
        font-size: 1.15rem;
        color: #555;
        line-height: 1.8;
        margin: 0;
      }

      /* Method Sections */
      .method-section {
        background: #f8f9fa;
        border-left: 5px solid #2185d0;
        padding: 28px;
        border-radius: 8px;
        margin-bottom: 28px;
      }

      .method-header {
        display: flex;
        align-items: center;
        margin-bottom: 16px;
        gap: 16px;
      }

      .method-number {
        width: 48px;
        height: 48px;
        background: #2185d0;
        color: white;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 1.8rem;
        font-weight: bold;
        flex-shrink: 0;
      }

      .method-title {
        font-size: 1.8rem;
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
      }

      .method-description {
        font-size: 1.1rem;
        color: #666;
        margin: 0 0 24px 0;
        line-height: 1.6;
      }

      /* Subsections */
      .documentation-subsection {
        margin: 24px 0;
      }

      .documentation-subsection h3 {
        font-size: 1.3rem;
        font-weight: 600;
        color: #34495e;
        margin: 20px 0 12px 0;
        display: flex;
        align-items: center;
      }

      .documentation-subsection h3 i {
        color: #27ae60;
      }

      /* Lists */
      .requirements-list,
      .steps-list {
        margin: 16px 0;
        padding-left: 24px;
        line-height: 1.8;
      }

      .requirements-list li,
      .steps-list li {
        margin: 12px 0;
        font-size: 1.05rem;
        color: #444;
      }

      .steps-list li {
        margin: 20px 0;
        padding-left: 8px;
      }

      .steps-list li strong {
        color: #2c3e50;
        display: block;
        margin-bottom: 8px;
        font-size: 1.1rem;
      }

      /* Code Blocks */
      .code-block {
        background: #2c3e50;
        border-radius: 6px;
        padding: 16px;
        margin: 12px 0;
        overflow-x: auto;
      }

      .code-block code {
        color: #e8f5e9;
        font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', 'Consolas', monospace;
        font-size: 0.95rem;
        background: none;
        padding: 0;
      }

      /* Inline code */
      code {
        background: #f1f3f5;
        padding: 3px 8px;
        border-radius: 4px;
        font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', 'Consolas', monospace;
        font-size: 0.9rem;
        color: #e74c3c;
      }

      /* Docker Links */
      .docker-links {
        display: flex;
        gap: 16px;
        margin: 16px 0;
        flex-wrap: wrap;
      }

      .docker-links a {
        flex: 1;
        min-width: 200px;
        text-decoration: none;
      }

      /* Documentation Footer */
      .documentation-footer {
        margin-top: 40px;
        padding-top: 24px;
        border-top: 2px solid #e8e8e8;
        text-align: center;
      }

      /* Responsive Design */
      @media (max-width: 768px) {
        .documentation-container {
          margin: 20px;
          padding: 24px;
        }

        .documentation-title {
          font-size: 2rem;
        }

        .method-title {
          font-size: 1.5rem;
        }

        .method-number {
          width: 40px;
          height: 40px;
          font-size: 1.5rem;
        }

        .docker-links {
          flex-direction: column;
        }

        .docker-links a {
          width: 100%;
        }
      }

      /* Module header responsive fix */
      @media (max-width: 768px) {
        .module-header {
          padding-top: 50px;
        }

        .module-header .ui.button {
          position: absolute !important;
          top: 0 !important;
          right: 0 !important;
        }

        .module-title {
          font-size: 1.8rem !important;
        }

        .module-subtitle {
          font-size: 1rem !important;
        }
      }
    ")),
    main_panel(
      div(
        id = "module_landing_page",
        class = "module-landing",
        div(
          class = "module-header",
          style = "position: relative; text-align: center;",
          action_button(
            "show_documentation",
            tagList(
              icon("book"),
              span(i18n$t("Documentation"), style = "margin-left: 8px;")
            ),
            class = "ui button",
            style = "position: absolute; top: 0; right: 0;"
          ),
          div(
            class = "module-header-content",
            h1(i18n$t("Demographic Analysis Suite"), class = "module-title"),
            p(i18n$t("Advanced tools for demographic research and analysis"),
              class = "module-subtitle"
            )
          )
        ),
        div(
          class = "modules-grid",
          module_cards
        )
      ),
      # Documentation section (initially hidden)
      hidden(
        div(
          id = "documentation_page",
          class = "documentation-container",
          # Header
          div(
            class = "documentation-header",
            h1(
              icon("book"),
              span(i18n$t("Installation Guide"), style = "margin-left: 12px;"),
              class = "documentation-title"
            ),
            action_button(
              "close_documentation",
              icon("close"),
              class = "ui icon button",
              style = "position: absolute; top: 20px; right: 20px;"
            )
          ),

          # Introduction
          div(
            class = "documentation-section",
            p(
              i18n$t("This guide will help you install and run the Life Table Projection application on your local machine. Choose the method that works best for you."),
              class = "documentation-intro"
            )
          ),

          # Method 1: R Installation
          div(
            class = "documentation-section method-section",
            div(
              class = "method-header",
              div(
                class = "method-number",
                "1"
              ),
              h2(i18n$t("Install via R"), class = "method-title")
            ),
            p(i18n$t("Recommended for R users. This is the simplest way to get started."), class = "method-description"),

            # Requirements
            div(
              class = "documentation-subsection",
              h3(
                icon("check circle"),
                span(i18n$t("Requirements"), style = "margin-left: 8px;")
              ),
              tags$ul(
                class = "requirements-list",
                tags$li(
                  tags$strong(i18n$t("R version:")),
                  " ",
                  i18n$t("R >= 4.0.0 (check your version with"),
                  " ",
                  tags$code("R.version.string"),
                  ")"
                )
              )
            ),

            # Steps
            div(
              class = "documentation-subsection",
              h3(
                icon("list ol"),
                span(i18n$t("Installation Steps"), style = "margin-left: 8px;")
              ),
              tags$ol(
                class = "steps-list",
                tags$li(
                  tags$strong(i18n$t("Install the pak package")),
                  div(
                    class = "code-block",
                    tags$code('install.packages("pak")')
                  )
                ),
                tags$li(
                  tags$strong(i18n$t("Install the application")),
                  div(
                    class = "code-block",
                    tags$code('pak::pak("cimentadaj/lifetableprojection")')
                  )
                ),
                tags$li(
                  tags$strong(i18n$t("Run the application")),
                  div(
                    class = "code-block",
                    tags$code("lifetableprojection::run_app()")
                  )
                ),
                tags$li(
                  tags$strong(i18n$t("Open your browser")),
                  br(),
                  i18n$t("The app will automatically open in your default browser. If not, navigate to the URL shown in your R console (typically"),
                  " ",
                  tags$code("http://localhost:XXXX"),
                  ")."
                )
              )
            )
          ),

          # Method 2: Docker Installation
          div(
            class = "documentation-section method-section",
            div(
              class = "method-header",
              div(
                class = "method-number",
                "2"
              ),
              h2(i18n$t("Install via Docker"), class = "method-title")
            ),
            p(i18n$t("Alternative method using Docker containers. Use this if the R installation doesn't work."), class = "method-description"),

            # Prerequisites
            div(
              class = "documentation-subsection",
              h3(
                icon("check circle"),
                span(i18n$t("Prerequisites"), style = "margin-left: 8px;")
              ),
              p(i18n$t("First, install Docker Desktop for your operating system:")),
              div(
                class = "docker-links",
                tags$a(
                  href = "https://docs.docker.com/desktop/setup/install/mac-install/",
                  target = "_blank",
                  class = "ui button",
                  icon("apple"),
                  span(i18n$t("Download for Mac"), style = "margin-left: 8px;")
                ),
                tags$a(
                  href = "https://docs.docker.com/desktop/setup/install/windows-install/",
                  target = "_blank",
                  class = "ui button",
                  icon("windows"),
                  span(i18n$t("Download for Windows"), style = "margin-left: 8px;")
                )
              )
            ),

            # Steps
            div(
              class = "documentation-subsection",
              h3(
                icon("list ol"),
                span(i18n$t("Installation Steps"), style = "margin-left: 8px;")
              ),
              tags$ol(
                class = "steps-list",
                tags$li(
                  tags$strong(i18n$t("Download the Dockerfile")),
                  br(),
                  i18n$t("Get the Dockerfile from GitHub:"),
                  " ",
                  tags$a(
                    href = "https://github.com/cimentadaj/lifetableprojection/blob/main/Dockerfile",
                    target = "_blank",
                    "github.com/cimentadaj/lifetableprojection"
                  ),
                  br(),
                  i18n$t("Save it to a folder on your computer.")
                ),
                tags$li(
                  tags$strong(i18n$t("Build the Docker image")),
                  br(),
                  i18n$t("Open a terminal/command prompt in the folder containing the Dockerfile and run:"),
                  div(
                    class = "code-block",
                    tags$code("docker build -t lifetable --build-arg PORT=3838 .")
                  )
                ),
                tags$li(
                  tags$strong(i18n$t("Run the container")),
                  div(
                    class = "code-block",
                    tags$code("docker run -p 3838:3838 lifetable")
                  )
                ),
                tags$li(
                  tags$strong(i18n$t("Open your browser")),
                  br(),
                  i18n$t("Navigate to"),
                  " ",
                  tags$a(href = "http://localhost:3838", target = "_blank", tags$code("http://localhost:3838"))
                )
              )
            )
          ),

          # Back button
          div(
            class = "documentation-footer",
            action_button(
              "back_to_landing_from_docs",
              tagList(
                icon("arrow left"),
                span(i18n$t("Back to modules"), style = "margin-left: 8px;")
              ),
              class = "ui button"
            )
          )
        )
      ),
      module_sections,
      width = NULL
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lifetableprojection"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' Create a translator object for internationalization
#' 
#' @return A Translator object initialized with the package's translation file
#' @importFrom shiny.i18n Translator
#' @export
usei18n_local <- function() {
  translation_path <- system.file("extdata", "translation.json", package = "lifetableprojection")
  if (translation_path == "") {
    stop("Could not find translation.json in package. Please ensure the package is installed correctly.")
  }
  i18n <- Translator$new(translation_json_path = translation_path)
  i18n$set_translation_language("en")
  i18n
}
