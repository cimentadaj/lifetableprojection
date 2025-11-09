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
    main_panel(
      div(
        id = "module_landing_page",
        class = "module-landing",
        div(
          class = "module-header",
          h1(i18n$t("Demographic Analysis Suite"), class = "module-title"),
          p(i18n$t("Advanced tools for demographic research and analysis"),
            class = "module-subtitle"
          )
        ),
        div(
          class = "modules-grid",
          module_cards
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
