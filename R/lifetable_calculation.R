#' Calculate Life Table
#'
#' Performs life table calculations based on user input and the uploaded data.
#'
#' @param data_in Data frame containing mortality data.
#' @param input List of parameters from Shiny input.
#' @return List containing life table calculation results.
#' @importFrom ODAPbackend lt_flexible lt_plot
#' @export
calculateLifeTable <- function(data_in, input) {
  req(input$calculate_lt)
  input_extrapfrom <- as.numeric(input$input_extrapFrom)

  begin_age <- which(data_in$Age == input$slider_ages_to_use[1])
  end_age <- which(data_in$Age == input$slider_ages_to_use[2])
  ages_to_use <- data_in$Age[begin_age:end_age]

  library(ggplot2)

  lt_res <- lt_flexible(
    data_in = data_in,
    OAnew = as.numeric(input$input_oanew),
    age_out = input$input_age_out,
    extrapFrom = input_extrapfrom,
    extrapFit = ages_to_use,
    extrapLaw = input$input_extrapLaw,
    radix = as.numeric(input$input_radix),
    SRB = as.numeric(input$input_srb),
    a0rule = input$input_a0rule,
    axmethod = input$input_axmethod,
    Sex = input$input_sex
  )

  final_lt_res <- list(lt = lt_res, plots = lt_plot(data_in, lt_res, input_extrapfrom))

  list(lt = final_lt_res, extrapfrom = input_extrapfrom)
}
