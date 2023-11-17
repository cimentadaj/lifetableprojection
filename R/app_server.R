to_snake <- function(x) tolower(gsub(" ", "", x))

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
      data <- mtcars ## plots[[input$tabset]]()$gg$data
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
  req(input$calculate_lt)
  input_extrapfrom <- as.numeric(input$input_extrapFrom)

  begin_age <- which(data_in$Age == input$slider_ages_to_use[1])
  end_age <- which(data_in$Age == input$slider_ages_to_use[2])
  ages_to_use <- data_in$Age[begin_age:end_age]

  lt_res <- lt_flexible(
    Deaths = data_in$Deaths,
    Exposures = data_in$Exposures,
    Age = data_in$Age,
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
#' @importFrom shiny renderUI observeEvent eventReactive actionButton reactive reactiveVal sliderInput
#' @importFrom shiny.semantic tabset icon
#' @importFrom shinyjs show hide
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom ggplot2 element_blank theme
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyalert shinyalert
#' @importFrom stats reshape quantile
#' @importFrom DT datatable renderDT dataTableOutput
#' @importFrom ODAPbackend plot_initial_data check_heaping_general
#' @export
app_server <- function(input, output, session) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  data_in <- reactive(readData(input))
  check_results <- reactive(validateData(data_in()))

  dt_ex <- system.file("data/abridged_data.csv", package = "lifetableprojection")
  output$data_table <- renderRHandsontable(renderDataTable(read.csv(dt_ex)))
  output$validation_results <- renderUI(displayValidationResults(check_results()))

  output$forward_step2 <- renderUI({
    req(input$file1)
    if (all(check_results()$pass == "Pass")) {
      div(
        actionButton("diagnostics", "Diagnostics", class = "ui blue button"),
        actionButton("forward_step", "Continue", class = "ui blue button")
      )
    }
  })

  diagnostic_plt <- reactive({
    # TODO
    library(dplyr)
    library(ggplot2)
    plts <- plot_initial_data(data_in())
    names(plts) <- to_snake(names(plts))
    plts
  })

  output$diag_exposures <- renderPlotly({
    plt <-
      diagnostic_plt()$exposures +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ggplotly(plt)
  })

  output$diag_deaths <- renderPlotly({
    plt <-
      diagnostic_plt()$deaths +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ggplotly(plt)
  })

  output$diag_empirical_mx <- renderPlotly({
    plt <-
      diagnostic_plt()$`empiricalmx` +
      theme_minimal(base_size = 16)

    ggplotly(plt)
  })

  output$table <- renderDT({
      heaping_exposure <- check_heaping_general(data_in(), "Exposures")
      heaping_deaths <- check_heaping_general(data_in(), "Deaths")

      heaping_exposure$Type <- "Exposures"
      heaping_deaths$Type <- "Deaths"

      heaping_res <- rbind(heaping_exposure, heaping_deaths)
      heaping_res$result <- round(heaping_res$result, 2)

      wide_data <- reshape(
        heaping_res,
        timevar = "Type",
        idvar = "method",
        direction = "wide"
      )

      wide_data$method <- toTitleCase(wide_data$method)

      names(wide_data) <- toTitleCase(gsub("result.", "", names(wide_data)))

      datatable(
        wide_data,
        rownames = FALSE,
        options = list(paging = FALSE, searching = FALSE, info = FALSE)
      )
    },
    server = FALSE
  )

  observeEvent(input$diagnostics, {
    myContent <- div(
      id = "content-wrapper",
      style = "display: flex; flex-direction: row; align-items: flex-start;",
      div(
        class = "plot-container",
        style = "width: 55%;",
        tabset(
          list(
            list(
              menu = "Exposures",
              content = plotlyOutput("diag_exposures", width = "90%")
            ),
            list(
              menu = "Deaths",
              content = plotlyOutput("diag_deaths", width = "90%")
            ),
            list(
              menu = "Empirical Mx",
              content = plotlyOutput("diag_empirical_mx", width = "90%")
            )
          )
        )
      ),
      div(
        style = "width: 45%; padding-left: 20px;",
        dataTableOutput("table")
      )
    )

    # Show the alert with the custom content
    shinyalert(title = "&#x1F50D Data Diagnostics", html = TRUE, size = "l", text = myContent)
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

  plots <- list("Mortality Rate Comparison" = lt_plt)

  plotRendered <- reactiveVal(FALSE)

  observeEvent(input$calculate_lt, {
    plotRendered(TRUE)
  })

  output$download_buttons <- renderUI({
    if (plotRendered()) {
      div(
        downloadButton("downloadPlot", "Download Plot"),
        downloadButton("downloadData", "Download Data")
      )
    }
  })

  ages_data <- reactive({
    all_ages <- unique(data_in()$Age)
    min_age <- if (60 %in% all_ages) 60 else round(quantile(all_ages, .60))
    step_ages <- diff(all_ages)
    step_repeat <- which.max(table(step_ages))
    step_ages <- as.numeric(names(table(step_ages))[step_repeat])
    list(all_ages = all_ages, min_age_fit = min_age, step_ages = step_ages)
  })

  output$ages_to_use <- renderUI({

    slider_widget <-
      sliderInput(
        "slider_ages_to_use",
        label = NULL,
        min = min(ages_data()$all_ages),
        max = max(ages_data()$all_ages),
        value = c(ages_data()$min_age_fit, max(ages_data()$all_ages)),
        step = ages_data()$step_ages
      )

    print("slider working")

    div(
      class = "field",
      icon("hashtag"),
      shiny.semantic::label(
        class = "main label",
        "Ages to include in model fit"
      ),
      slider_widget
    )
  })


  output$tabs <- renderUI({
    tabset(
      id = "tabset",
      tabs = lapply(
        names(plots),
        function(p) {
          list(
            menu = p,
            id = p,
            content = list(
              if (!plotRendered()) {
                uiOutput(paste0("placeholder_", to_snake(p)))
              } else {
                withSpinner(
                  plotlyOutput(
                    paste0("plot_", to_snake(p)),
                    height = "600px"
                  )
                )
              }
            )
          )
        }
      )
    )
  })

  for (p in names(plots)) {
    p_clean <- to_snake(p)
    output[[paste0("placeholder_", p_clean)]] <- renderUI({
      tags$img(
        src = "www/placeholder_plot.png",
        height = "600px",
        width = "100%"
      )
    })

    output[[paste0("plot_", p_clean)]] <- renderPlotly(plots[[p]]()$plotly)
  }

  setupDownloadHandlers(output, plots, input)
}
