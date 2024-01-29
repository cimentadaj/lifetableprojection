#' Compare and Plot Rates from Input Data
#'
#' This function generates a comparison plot of rates from input data sets. It utilizes ggplot2 for plotting and then converts the plot to a Plotly interactive graph.
#' The function primarily serves to visually compare and analyze rate data derived from 'data_in' and 'data_out'.
#'
#' @param data_in A reactive expression or data frame containing the initial data set.
#' @param data_out A reactive expression or data frame containing the calculated data set, typically the result of some transformation or analysis of 'data_in'.
#' @return A list containing two elements: 'gg', the ggplot object, and 'plotly', the corresponding interactive Plotly object.
#' @importFrom ggplot2 ggplot theme_minimal ggtitle
#' @importFrom plotly ggplotly
#' @importFrom tools toTitleCase
#' @importFrom ODAPbackend plot_compare_rates
#' @export
plot_compare_rates_interactive <- function(data_in, data_out) {
  # TODO: remove ggplot2. Something is happening on their side not loading ggplot2.
  library(ggplot2)
  plt <-
    plot_compare_rates(
      data_in,
      data_out$lt,
      extrapFrom = data_out$extrapfrom
    ) +
    ggtitle(
      label = "Difference Between Empirical Mx and Extrapolated Values on Log10 Scale"
    ) +
    theme_minimal(base_size = 13)

  list(
    gg = plt,
    plotly = config(
      ggplotly(plt),
      displayModeBar = FALSE
    )
  )
}

