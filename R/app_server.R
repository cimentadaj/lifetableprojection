#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny renderUI req wellPanel HTML observeEvent reactive renderPlot 
#' @importFrom shinyjs show hide
#' @importFrom tools toTitleCase
#' @importFrom utils read.csv head
#' @importFrom stats setNames
#' @importFrom ODAPbackend check_data lt_flexible plot_compare_rates
#' @importFrom rhandsontable renderRHandsontable rhandsontable
#' @export
app_server <- function(input, output, session) {
  data_in <- reactive({
    read.csv(input$file1$datapath)
  })

  check_results <- reactive({
    check_data(data_in())
  })

  output$data_table <- renderRHandsontable({
    req(input$file1) # Ensure a file is uploaded
    sample_df <- head(data_in())
    etc_df <- as.data.frame(
      setNames(replicate(ncol(sample_df), "   ...", simplify = FALSE), names(sample_df))
    )

    rhandsontable(rbind(sample_df, etc_df), readOnly = TRUE)
  })

  output$validation_results <- renderUI({
    req(input$file1)
    # Assuming 'data_in' is the processed data from the uploaded file

    if (all(check_results()$pass == "Pass")) {
      wellPanel(class = "success", "\u2705 Everything is great, all checks passed!")
    } else {
      failed_checks <- check_results()[check_results()$pass != "Pass", ]
      wellPanel(
        class = "danger",
        HTML("Check Failures:<br>"),
        HTML(paste("\u274C", toTitleCase(failed_checks$check), collapse = "<br>"))
      )
    }
  })

  output$forward_step2 <- renderUI({
    req(input$file1)
    res <- check_results()
    if (all(res$pass == "Pass")) {
      action_button("forward_step", "Continue", class = "ui blue button")
    }
  })

  observeEvent(
    input$forward_step, {
      hide("landing_page")
      show("step_input")
    }
  )

  observeEvent(
    input$back_to_landing,
    {
      hide("step_input")
      show("landing_page")
    }
  )

  # Assuming 'data_in' is a reactive expression that provides the data
  # Define reactive expression for lt_flexible results
  data_out <- reactive({
    req(input$calculate_lt)  # Trigger recalculation when button is clicked

    print(data_in())
    print(tail(data_in()))

    print(input$input_extrapFrom)

    lt_flexible(
      Deaths = data_in()$Deaths,
      Exposures = data_in()$Exposures,
      Age = data_in()$Age,
      OAnew = as.numeric(input$input_oanew),
      age_out = input$input_age_out,
      extrapFrom = as.numeric(input$input_extrapFrom),
      extrapFit = data_in()$Age[data_in()$Age >= 60],
      extrapLaw = input$input_extrapLaw,
      radix = as.numeric(input$input_radix),
      SRB = as.numeric(input$input_srb),
      a0rule = input$input_a0rule,
      axmethod = input$input_axmethod,
      Sex = input$input_sex
    )
  })

  # Observe event for the 'Calculate LT' button
  observeEvent(input$calculate_lt, {
    print("clicked")
    # Accessing data_out() to trigger the reactive
    data_out()
  })

  # Render the plot output
  output$lt_plt <- renderPlot({
    req(data_out())  # Ensure data_out is available before plotting
    print("in plot")
    plot_compare_rates(
      data_in(),
      data_out(),
      extrapFrom = input$input_extrapFrom
    )
  })
}
