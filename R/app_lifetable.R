#' Calculate Life Table
#'
#' Performs life table calculations based on user input and the uploaded data.
#'
#' @param data_in Data frame containing mortality data.
#' @param input List of parameters from Shiny input.
#' @return List containing life table calculation results.
#' @importFrom ODAPbackend lt_flexible lt_plot
#' @importFrom dplyr mutate
#' @export
calculateLifeTable <- function(data_in, input) {
  `.id` <- NULL
  print("Went into calculate lifetable")
  req(input$calculate_lt)
  input_extrapfrom <- isolate(as.numeric(input$input_extrapFrom))

  begin_age <- isolate(which(data_in$Age == input$slider_ages_to_use[1]))
  end_age <- isolate(which(data_in$Age == input$slider_ages_to_use[2]))
  ages_to_use <- data_in$Age[begin_age:end_age]

  library(ggplot2)
  print(begin_age)
  print(end_age)
  print(ages_to_use)

  lt_res <- lt_flexible(
    data_in = data_in,
    OAnew = isolate(as.numeric(input$input_oanew)),
    age_out = isolate(input$input_age_out),
    extrapFrom = input_extrapfrom,
    extrapFit = ages_to_use,
    extrapLaw = isolate(input$input_extrapLaw),
    radix = isolate(as.numeric(input$input_radix)),
    SRB = isolate(as.numeric(input$input_srb)),
    a0rule = isolate(input$input_a0rule),
    axmethod = isolate(input$input_axmethod),
    Sex = isolate(input$input_sex)
  ) %>%
    mutate(.id = as.numeric(.id))

  return(lt_res)
}



#' Create Life Table Input UI
#'
#' This function generates all the UI components and reactive values needed for life table calculation.
#'
#' @param data_in Reactive value containing the input data
#' @param grouping_dropdowns Reactive with widgets for all grouping variabels
#' @param tabNames Vector with the tab names
#' @param input Internal shiny input list
#' @param output Output shiny input list
#' @return A list containing UI elements and reactive expressions for life table calculation
#' @importFrom shiny reactive renderUI sliderInput div req
#' @importFrom shiny.semantic label
#' @importFrom stats quantile
#' @export
create_life_table_input_ui <- function(data_in, grouping_dropdowns, tabNames, input, output) {
  extrap_age <- reactive({
    req(data_in())
    num <- as.numeric(gsub("+", "", max(data_in()$Age)))
    num - 20
  })


  ages_data <- reactive({
    req(data_in())
    all_ages <- unique(data_in()$Age)
    min_age <- if (60 %in% all_ages) 60 else round(quantile(all_ages, .60))
    step_ages <- diff(all_ages)
    step_repeat <- which.max(table(step_ages))
    step_ages <- as.numeric(names(table(step_ages))[step_repeat])
    list(all_ages = all_ages, min_age_fit = min_age, step_ages = step_ages)
  })


  output$lt_group_select_ui <- renderUI({
    req(input$calculate_lt)
    div(
      class = "grouping-dropdowns-container",
      style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;",
      lapply(grouping_dropdowns(), function(dropdown) {
        div(style = "min-width: 150px", dropdown)
      }),
      div(
        class = "grouping-dropdowns-container",
        style = "width: 100%; display: flex; flex-wrap: wrap; justify-content: center;",
        selectInput(inputId = "tabSelector", label = NULL, choices = tabNames)
      )
    )
  })


  lt_input <- list(
    extrap_age = extrap_age,
    ages_data = ages_data,
    extrap_from = renderUI({
      create_field_set(
        "",
        "Extrap. Jump-off Age",
        "input_extrapFrom",
        input_selected = extrap_age(),
        numeric_input = TRUE
      )
    }),
    ages_to_use = renderUI({
      req(ages_data())
      slider_widget <- sliderInput(
        "slider_ages_to_use",
        label = NULL,
        min = min(ages_data()$all_ages),
        max = max(ages_data()$all_ages),
        value = c(ages_data()$min_age_fit, max(ages_data()$all_ages)),
        step = ages_data()$step_ages
      )

      div(
        class = "field",
        shiny.semantic::label(
          class = "main label",
          "Ages to fit extrapolation model"
        ),
        slider_widget
      )
    })
  )

  output$extrap_from_data <- lt_input$extrap_from
  output$ages_to_use <- lt_input$ages_to_use

  lt_input
}


# Function to calculate life table and generate plots
calculate_lt_and_plots <- function(data, input) {
  reactive({
    print("Starting life table calculations")
    lt_res <- calculateLifeTable(data, input)
    plots <- lt_plot(data, lt_res, input$input_extrapFrom)
    lt_res_summary <- lt_summary(lt_res)
    print("Life table calculations complete")

    list(
      plots = plots,
      summary = lt_res_summary,
      lt = lt_res
    )
  })
}
