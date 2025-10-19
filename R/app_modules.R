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
    interpolation = list(
      id = "interpolation",
      name = i18n$t("Demographic Interpolation"),
      description = i18n$t("Fill data gaps and estimate demographic indicators between time points using advanced interpolation methods."),
      icon = "chart-line",
      status = "coming_soon",
      button_label = NULL,
      status_label = i18n$t("Coming Soon"),
      ui = NULL,
      server_fun = NULL
    )
  )
}
