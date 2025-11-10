#' Application module registry
#'
#' @param i18n Translator object used for labels
#' @return A named list describing available modules
#' @noRd
get_app_modules <- function(i18n) {
  list(
    lifetable = list(
      id = "lifetable",
      name = i18n$t("Life Table Analysis"),
      description = i18n$t("Create comprehensive life tables with advanced smoothing capabilities and mortality analysis tools."),
      icon = "table",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = lifetable_module_ui(i18n),
      server_fun = lifetable_server
    ),
    heaping = list(
      id = "heaping",
      name = i18n$t("Heaping Diagnostics"),
      description = i18n$t("Identify age heaping patterns using Myers, Bachi, Roughness, and Sawtooth indices."),
      icon = "chart bar",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = heaping_module_ui(i18n),
      server_fun = heaping_module_server
    ),
    smoothing = list(
      id = "smoothing",
      name = i18n$t("Smoothing Preview"),
      description = i18n$t("Preview mortality smoothing options before integrating them into your workflow."),
      icon = "wave square",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = smoothing_module_ui(i18n),
      server_fun = smoothing_module_server
    ),
    graduation = list(
      id = "graduation",
      name = i18n$t("Graduation Tool"),
      description = i18n$t("Fine-tune mortality graduation with configurable infant constraints."),
      icon = "sliders horizontal",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = graduation_module_ui(i18n),
      server_fun = graduation_module_server
    )
  )
}
