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
    ),
    odap = list(
      id = "odap",
      name = i18n$t("Old-Age Population Redistribution"),
      description = i18n$t("Redistribute population in older ages using stable population methods and mortality data."),
      icon = "users",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = odap_module_ui(i18n),
      server_fun = odap_module_server
    ),
    childmort = list(
      id = "childmort",
      name = i18n$t("Child Mortality (Brass Method)"),
      description = i18n$t("Estimate infant and child mortality from children ever born and children dead reported by women."),
      icon = "child",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = childmort_module_ui(i18n),
      server_fun = childmort_module_server
    ),
    orphanhood = list(
      id = "orphanhood",
      name = i18n$t("Adult Mortality from Orphanhood"),
      description = i18n$t("Estimate adult mortality from the proportion of respondents whose mother or father is alive, in one or two censuses or surveys."),
      icon = "hand holding heart",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = orphanhood_module_ui(i18n),
      server_fun = orphanhood_module_server
    ),
    intercensal = list(
      id = "intercensal",
      name = i18n$t("Adult Mortality from Intercensal Survival"),
      description = i18n$t("Estimate adult mortality from the survival of each age group between two censuses."),
      icon = "exchange",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = intercensal_module_ui(i18n),
      server_fun = intercensal_module_server
    ),
    mlt = list(
      id = "mlt",
      name = i18n$t("Model Life Tables"),
      description = i18n$t("Build Coale-Demeny and UN model life tables from e0, child or adult mortality."),
      icon = "table",
      status = "active",
      button_label = i18n$t("Go to module"),
      status_label = i18n$t("Active"),
      ui = mlt_module_ui(i18n),
      server_fun = mlt_module_server
    )
  )
}

#' Families of modules shown on the landing page
#'
#' The landing page lists these families; opening one shows its modules.
#' Every module of `get_app_modules()` belongs to exactly one family.
#' @noRd
MODULE_FAMILIES <- list(
  lifetables = list(
    name = "Life tables",
    description = "Build life tables from your data, or model life tables from mortality indicators.",
    icon = "table",
    modules = c("lifetable", "mlt")
  ),
  data_by_age = list(
    name = "Evaluate and adjust data by age",
    description = "Check age heaping, smooth and graduate mortality rates, and redistribute the population at older ages.",
    icon = "sliders horizontal",
    modules = c("heaping", "smoothing", "graduation", "odap")
  ),
  indirect = list(
    name = "Indirect estimation of mortality",
    description = "Estimate child and adult mortality from census and survey questions.",
    icon = "search",
    modules = c("childmort", "orphanhood", "intercensal")
  )
)

#' Display names of the modules (English; translated where shown)
#'
#' Kept apart from `get_app_modules()`, which also builds each module's UI,
#' so the family cards can list module names cheaply.
#' @noRd
MODULE_NAMES <- c(
  lifetable = "Life Table Analysis",
  heaping = "Heaping Diagnostics",
  smoothing = "Smoothing Preview",
  graduation = "Graduation Tool",
  odap = "Old-Age Population Redistribution",
  childmort = "Child Mortality (Brass Method)",
  orphanhood = "Adult Mortality from Orphanhood",
  intercensal = "Adult Mortality from Intercensal Survival",
  mlt = "Model Life Tables"
)

#' Card of one family of modules on the landing page
#'
#' @param family_id Name of the family in `MODULE_FAMILIES`.
#' @param i18n Translator object.
#' @return A module-card div with the family's modules and an open button.
#' @noRd
create_family_card <- function(family_id, i18n) {
  fam <- MODULE_FAMILIES[[family_id]]
  shiny::div(
    class = "module-card family-card",
    # Semantic UI icon names, as the module cards use
    shiny::div(class = "module-icon", shiny.semantic::icon(fam$icon)),
    shiny::h3(i18n$t(fam$name), class = "module-name"),
    shiny::p(i18n$t(fam$description), class = "module-description"),
    shiny::tags$ul(
      class = "family-module-list",
      lapply(fam$modules, function(mod_id) shiny::tags$li(i18n$t(MODULE_NAMES[[mod_id]])))
    ),
    shiny::actionButton(
      paste0("goto_family_", family_id),
      sprintf(i18n$t("Open (%d modules)"), length(fam$modules)),
      class = "ui blue button"
    )
  )
}

#' Header of an opened family: back button and the family name
#' @noRd
create_family_header <- function(family_id, i18n) {
  fam <- MODULE_FAMILIES[[family_id]]
  shiny::div(
    class = "family-header",
    shiny::actionButton(paste0("back_to_families_", family_id), i18n$t("← All families"), class = "ui grey button"),
    shiny::h2(i18n$t(fam$name), class = "family-title")
  )
}
