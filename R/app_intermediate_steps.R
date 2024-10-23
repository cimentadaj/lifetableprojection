#' Render adjustment pills
#'
#' This function creates HTML tags for adjustment pills based on the input list.
#'
#' @param pills A named list of adjustment pills
#' @return A div containing span elements for each adjustment pill
#' @export
render_adjustment_pills <- function(pills) {
  pill_tags <- lapply(names(pills), function(adjustment_name) {
    tags$span(
      class = "ui label",
      adjustment_name,
      tags$i(class = "delete icon", onclick = sprintf("
        Shiny.setInputValue('remove_pill', '%s');
        setTimeout(function() { Shiny.setInputValue('remove_pill', null); }, 0);
      ", adjustment_name))
    )
  })
  do.call(tags$div, c(list(id = "adjustment_pills", class = "ui labels"), pill_tags))
}

#' Add adjustment pill
#'
#' This function adds a new adjustment pill to the list of executed adjustments.
#'
#' @param adjustment_name The name of the adjustment to add
#' @param executed_adjustments A reactive value containing the current adjustments
#' @export
add_adjustment_pill <- function(adjustment_name, executed_adjustments) {
  current_adjustments <- executed_adjustments()
  current_adjustments[[adjustment_name]] <- TRUE
  executed_adjustments(current_adjustments)
}

#' Remove adjustment pill
#'
#' This function removes an adjustment pill from the list of executed adjustments.
#'
#' @param adjustment_name The name of the adjustment to remove
#' @param executed_adjustments A reactive value containing the current adjustments
#' @export
remove_adjustment_pill <- function(adjustment_name, executed_adjustments) {
  current_adjustments <- executed_adjustments()
  current_adjustments[[adjustment_name]] <- NULL
  executed_adjustments(current_adjustments)
}
