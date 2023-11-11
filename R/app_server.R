#' The Application Server-Side Logic
#'
#' This function defines the server logic for the Shiny application,
#' managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny renderUI req wellPanel HTML
#' @importFrom tools toTitlese
#' @importFrom utils read.csv
#' @importFrom ODAP.backend check_data
#' @export
app_server <- function(input, output, session) {
    output$validation_results <- renderUI({
        req(input$file1)
        # Assuming 'data_in' is the processed data from the uploaded file
        data_in <- read.csv(input$file1$datapath)
        check_results <- check_data(data_in)

        if (all(check_results$pass == "Pass")) {
            wellPanel(class = "success", "\u2705 Everything is great, all checks passed!")
        } else {
            failed_checks <- check_results[check_results$pass != "Pass", ]
            wellPanel(
                class = "danger",
                HTML("Check Failures:<br>"),
                HTML(paste("\u274C", toTitleCase(failed_checks$check), collapse = "<br>"))
            )
        }
    })
}
