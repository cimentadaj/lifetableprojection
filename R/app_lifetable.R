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

  input_extrapfrom <- as.numeric(isolate(input$input_extrapFrom))

  unique_ages <- unique(data_in$Age)

  begin_age <- which(unique_ages == isolate(input$slider_ages_to_use[1]))
  end_age <- which(unique_ages == isolate(input$slider_ages_to_use[2]))
  ages_to_use <- unique_ages[begin_age:end_age]

  library(ggplot2)

  lt_res <- isolate(
    lt_flexible(
    data_in = data_in,
    OAnew = as.numeric(input$input_oanew),
    age_out = input$input_age_out,
    extrapFrom = input_extrapfrom,
    extrapFit = ages_to_use,
    extrapLaw = input$input_extrapLaw,
    radix = input$input_radix,
    SRB = input$input_srb,
    a0rule = input$input_a0rule,
    axmethod = input$input_axmethod,
    Sex = input$input_sex
    )$data_out
  )

  lt_res <-
    lt_res %>%
    mutate(.id = as.numeric(.id))

  return(lt_res)
}



#' Create Life Table Input UI
#'
#' This function generates all the UI components and reactive values needed for life table calculation.
#'
#' @param data_in Reactive value containing the input data
#' @param grouping_dropdowns Reactive with widgets for all grouping variabels
#' @param tabNames_internal Vector with internal fixed tab names (English)
#' @param input Internal shiny input list
#' @param output Output shiny input list
#' @return A list containing UI elements and reactive expressions for life table calculation
#' @importFrom shiny reactive renderUI sliderInput div req
#' @importFrom shiny.semantic label
#' @importFrom stats quantile
#' @export
create_life_table_input_ui <- function(data_in, grouping_dropdowns, tabNames_internal, input, output, i18n) {
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
    i18n <- usei18n_local()
    input$selected_language
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
        selectInput(
          inputId = "tabSelector",
          label = NULL,
          choices = setNames(tabNames_internal, sapply(tabNames_internal, function(name) i18n$t(name)))
        )
      )
    )
  })

  lt_input <- list(
    extrap_age = extrap_age,
    ages_data = ages_data,
    extrap_from = renderUI({
      i18n <- usei18n_local()
      input$selected_language
      create_field_set(
        "",
        i18n$t("Extrap. Jump-off Age"),
        "input_extrapFrom",
        input_selected = extrap_age(),
        numeric_input = TRUE
      )
    }),
    ages_to_use = renderUI({
      i18n <- usei18n_local()
      input$selected_language
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
          i18n$t("Ages to fit extrapolation model")
        ),
        slider_widget
      )
    }),
    sex_to_use = renderUI({
      i18n <- usei18n_local()
      input$selected_language
      sex_widget <- create_field_set("", "Sex", "input_sex", c("Total", "Female", "Male"), "Total")
      if ("sex" %in% tolower(names(data_in()))) {
        sex_widget <- div()
      }

      sex_widget
    })
  )

  output$extrap_from_data <- lt_input$extrap_from
  output$ages_to_use <- lt_input$ages_to_use
  output$sex_to_use <- lt_input$sex_to_use

  # Set outputs to render even when hidden
  outputOptions(output, "lt_group_select_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "extrap_from_data", suspendWhenHidden = FALSE)
  outputOptions(output, "ages_to_use", suspendWhenHidden = FALSE)
  outputOptions(output, "sex_to_use", suspendWhenHidden = FALSE)

  lt_input
}


# Function to calculate life table and generate plots
calculate_lt_and_plots <- function(data, input, i18n) {
  reactive({
    print("Starting life table calculations")
    lt_res <- calculateLifeTable(data, input)
    plots <- lt_plot(data, lt_res, isolate(input$input_extrapFrom), i18n)
    plt_names <- unname(unlist(lapply(plots, function(x) unique(x$nMx$nMx_plot_data$.id))))
    names(plots) <- plt_names
    lt_res_summary <- lt_summary(lt_res, i18n)
    print("Life table calculations complete")

    list(
      plots = plots,
      summary = lt_res_summary,
      lt = lt_res
    )
  })
}
