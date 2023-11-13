#' Read Data from Uploaded File
#'
#' Reads and returns data from the uploaded CSV file in Shiny.
#'
#' @param input Shiny input object containing the file input.
#' @return Data frame read from the uploaded CSV file.
#' @importFrom shiny req
#' @importFrom utils read.csv
#' @export
readData <- function(input) {
  req(input$file1)
  read.csv(input$file1$datapath)
}

#' Validate Uploaded Data
#'
#' Validates the uploaded data using specific criteria.
#'
#' @param data Data frame to be validated.
#' @return Data frame containing validation results.
#' @importFrom ODAPbackend check_data
#' @export
validateData <- function(data) {
  check_data(data)
}

#' Render Data Table
#'
#' Creates a read-only handsontable UI component for displaying data.
#'
#' @param data Data frame to be displayed in the table.
#' @return Shiny UI component for data table.
#' @importFrom rhandsontable rhandsontable
#' @importFrom utils head
#' @importFrom stats setNames
#' @export
renderDataTable <- function(data) {
  sample_df <- head(data)
  etc_df <- setNames(replicate(ncol(sample_df), "   ...", simplify = FALSE), names(sample_df))
  rhandsontable(rbind(sample_df, as.data.frame(etc_df)), readOnly = TRUE)
}

#' Display Validation Results
#'
#' Renders UI elements to display the results of data validation.
#'
#' @param results Data frame with validation results.
#' @importFrom shiny wellPanel HTML
#' @importFrom tools toTitleCase
#' @export
displayValidationResults <- function(results) {
  if (all(results$pass == "Pass")) {
    wellPanel(class = "success", "\u2705 Everything is great, all checks passed!")
  } else {
    failed_checks <- results[results$pass != "Pass", ]
    wellPanel(
      class = "danger",
      HTML("Check Failures:<br>"),
      HTML(paste("\u274C", toTitleCase(failed_checks$check), collapse = "<br>"))
    )
  }
}

#' Setup Download Handlers
#'
#' Sets up download handlers for plots and data in Shiny.
#'
#' @param output Shiny output object.
#' @param plots List of reactive expressions for plots.
#' @param input Shiny input object.
#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.csv
#' @export
setupDownloadHandlers <- function(output, plots, input) {
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$tabset, "plot.png", sep = "_")
    },
    content = function(file) {
      plot <- plots[[input$tabset]]()$gg
      ggsave(file, plot)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$tabset, "data.csv", sep = "_")
    },
    content = function(file) {
      data <- plots[[input$tabset]]()$gg$data
      write.csv(data, file, row.names = FALSE)
    }
  )
}

#' Calculate Life Table
#'
#' Performs life table calculations based on user input and the uploaded data.
#'
#' @param data_in Data frame containing mortality data.
#' @param input List of parameters from Shiny input.
#' @return List containing life table calculation results.
#' @importFrom ODAPbackend lt_flexible
#' @export
calculateLifeTable <- function(data_in, input) {
    req(input$calculate_lt) # Trigger recalculation when button is clicked
    input_extrapfrom <- as.numeric(input$input_extrapFrom)

    lt_res <- lt_flexible(
      Deaths = data_in$Deaths,
      Exposures = data_in$Exposures,
      Age = data_in$Age,
      OAnew = as.numeric(input$input_oanew),
      age_out = input$input_age_out,
      extrapFrom = input_extrapfrom,
      extrapFit = data_in$Age[data_in$Age >= 60],
      extrapLaw = input$input_extrapLaw,
      radix = as.numeric(input$input_radix),
      SRB = as.numeric(input$input_srb),
      a0rule = input$input_a0rule,
      axmethod = input$input_axmethod,
      Sex = input$input_sex
    )

    list(lt = lt_res, extrapfrom = input_extrapfrom)
}

#' Generate Plotly Plot
#'
#' Creates a Plotly plot based on the life table calculation results.
#'
#' @param data Data frame of original data.
#' @param results Results from the life table calculation.
#' @return Plotly plot object.
#' @importFrom plotly renderPlotly
#' @export
generatePlot <- function(data, results) {
  req(results)
  plot_compare_rates_interactive(data, results)
}

#' Shiny Server Function
#'
#' Defines the server logic for the Shiny application, managing data processing, UI rendering, and user interactions.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @importFrom shiny renderUI observeEvent eventReactive actionButton reactive
#' @importFrom shiny.semantic tabset
#' @importFrom shinyjs show hide
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom shinycssloaders withSpinner
#' @export
app_server <- function(input, output, session) {
  data_in <- reactive(readData(input))
  check_results <- reactive(validateData(data_in()))

  output$data_table <- renderRHandsontable(renderDataTable(data_in()))
  output$validation_results <- renderUI(displayValidationResults(check_results()))

  output$forward_step2 <- renderUI({
    req(input$file1)
    if (all(check_results()$pass == "Pass")) {
      actionButton("forward_step", "Continue", class = "ui blue button")
    }
  })

  observeEvent(input$forward_step, {
    hide("landing_page")
    show("step_input")
  })

  observeEvent(input$back_to_landing, {
    hide("step_input")
    show("landing_page")
  })

  data_out <- eventReactive(input$calculate_lt, calculateLifeTable(data_in(), input))

  lt_plt <- reactive({
    generatePlot(data_in(), data_out())
  })

  plots <- list("Mx" = lt_plt, "Yx" = lt_plt)

  output$tabs <- renderUI({
    tabset(
      id = "tabset",
      lapply(names(plots), function(p) {
        list(menu = p, id = p, content = withSpinner(plotlyOutput(p, height = "600px")))
      })
    )
  })

  lapply(names(plots), function(p) {
    output[[p]] <- renderPlotly(plots[[p]]()$plotly)
  })

  setupDownloadHandlers(output, plots, input)
}
