options(scipen = 99999999)

EXTRAP_LAWS <- c(
  "Kannisto",
  "Kannisto_Makeham",
  "Makeham",
  "Gompertz",
  "GGompertz",
  "Beard",
  "Beard_Makeham",
  "Quadratic"
)

RENDERKATEX <- c(
  "function(data, type, row, meta){",
  "  if(type === 'display'){",
  "    data = katex.renderToString(data);",
  "  }",
  "  return data;",
  "}"
)

LATEX_PRE_TAGS <- tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css",
    integrity = "sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ",
    crossorigin = "anonymous"
  ),
  tags$script(
    defer = "",
    src = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.js",
    integrity = "sha384-VQ8d8WVFw0yHhCk5E8I86oOhv48xLpnDZx5T9GogA/Y84DcCKWXDmSDfn13bzFZY",
    crossorigin = "anonymous"
  )
)

JS_CODE_SCREEN_SIZE <- '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("get_screen_width",jsWidth);
});
'

#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "lifetableprojection")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' @importFrom shiny numericInput textOutput renderText
#' @importFrom dplyr mutate group_split
utils::globalVariables(c(".id", ".id_label"))
