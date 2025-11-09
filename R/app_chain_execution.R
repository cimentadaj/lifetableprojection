#' Preprocessing Execution Pipeline
#'
#' This function initializes and manages a preprocessing execution pipeline using the R6 class system.
#' The pipeline allows for adding multiple processing steps, executing them in a specified order,
#' and retrieving intermediate and final results, including input/output data and any plot results generated.
#'
#' @param data_in The input data for the preprocessing pipeline.
#'
#' @return An instance of the `preprocessing_execution` R6 class, which allows for adding steps,
#' executing them, and retrieving results.
#'
#' @details
#' The `preprocessing_execution` function creates an R6 class to handle a series of preprocessing steps.
#' Each step is defined by a name and a function call. When executed, the steps are applied sequentially,
#' with each step receiving the output from the previous step. The results (both data and figures)
#' of each step can be retrieved later.
#'
#' The pipeline supports the following methods:
#'
#' - `add(name, func_call)`: Adds a new step to the pipeline. `name` is a string identifier for the step,
#' and `func_call` is the function call to be executed.
#'
#' - `execute(step_names)`: Executes the pipeline steps in the order provided by `step_names`. Each step is executed
#' with the data from the previous step as input, and its results (input data, output data, and plot results)
#' are stored.
#'
#' - `get_result(step_name = NULL)`: Retrieves the results for a specific step or all steps if no `step_name`
#' is provided. Each result contains the input data, output data, and any plot results from the step.
#'
#' - `final_result()`: Retrieves the output data from the final executed step. If no steps have been executed,
#' `NULL` is returned.
#'
#' @importFrom dplyr left_join select rename_with
#' @export
preprocessing_execution <- function(data_in) {
  preprocessing_execution_internal <- R6::R6Class(
    "preprocessing_execution",
    public = list(
      data_in = NULL,
      steps = list(),
      results = list(),

      initialize = function(data_in) {
        self$data_in <- data_in
      },

      add = function(name, func_call) {
        self$steps[[name]] <- func_call
      },

      execute = function(step_names) {
        self$results <- list()
        previous_data <- self$data_in
        for (name in step_names) {
          func_call <- self$steps[[name]]
          # Create an environment where .data is assigned to previous_data
          eval_env <- new.env(parent = parent.frame())
          eval_env$.data <- previous_data
          # Evaluate the function call in this environment
          result <- eval(func_call, envir = eval_env)

          result$data$.id <- as.numeric(result$data$.id)
          self$data_in$.id <- as.numeric(self$data_in$.id)

          res <- dplyr::left_join(
            result$data,
            self$data_in,
            by = c(".id", "Age"),
            keep = FALSE
          ) %>%
            select(-contains(".y")) %>%
            rename_with(~ gsub("\\.x", "", .x))

          # Store the result with data_in, data_out, plot_results
          self$results[[name]] <- list(
            data_input = previous_data,
            data_output = res,
            plot_result = result$figures
          )
          # Update previous_data for next step
          previous_data <- res
        }
      },

      get_result = function(step_name = NULL) {
        if (is.null(step_name)) {
          return(self$results)
        } else if (step_name %in% names(self$results)) {
          res <- self$results[[step_name]]
          return(res)
        } else {
          stop(paste("No results found for step:", step_name))
        }
      },

      final_result = function() {
        if (length(self$results) > 0) {
          last_step <- tail(names(self$results), 1)
          return(self$results[[last_step]]$data_output)
        } else {
          return(NULL)
        }
      }
    )
  )

  preprocessing_execution_internal$new(data_in)
}



## data_in <- read_csv("~/Downloads/abridged_data.csv")

## data_in <-
##   data_in %>%
##   ODAPbackend:::create_groupid(
##     c("Sex", "Province")
##   )

## ## expo <- smooth_flexible(data_in, "Exposures", age_out = "single")$data
## ## deaths <- smooth_flexible(data_in, "Deaths", age_out = "single")$data

## ## combined <- left_join(expo, deaths)

## ## combined %>%
## ##   left_join()

## ## data_in %>%
## ##   lt_flexible(
## ##     OAnew = 100,
## ##     age_out = "single",
## ##     extrapFrom = 80,
## ##     extrapFit = seq(60, 100, by = 5),
## ##     extrapLaw = "Kannisto",
## ##     radix = 100000,
## ##     SRB = 1.05,
## ##     a0rule = "Andreev-Kingkade",
## ##     axmethod = "UN (Greville)",
## ##     Sex = "Total"
## ##   )

## ## data_in <- readr::read_csv("~/Downloads/data_in.csv")

## smooth_call <- expr(
##   smooth_overall(
##     .data,
##     rough_exp = "auto",
##     fine_exp = "auto",
##     constraint_exp = TRUE,
##     u5m_exp = NULL,
##     rough_deaths = "auto",
##     fine_deaths = "auto",
##     constraint_deaths = TRUE,
##     u5m_deaths = NULL,
##     age_out = "single"
##   )
## )

## ## data_in <- data.frame(x = 1:5, y = 6:10)
## init <- preprocessing_execution(data_in)

## ## # Example function (replace this with your actual function)
## ## smooth_test <- function(data) {
## ##   # For demonstration, simply returns the data multiplied by 2
## ##   data_output <- data * 2
## ##   list(
## ##     data = data_output,
## ##     figures = list(
## ##       figure_two = list(figure = NULL) # Replace with actual plotting code
## ##     )
## ##   )
## ## }

## ## # Usage:
## ## # Create function calls with .data as a placeholder
## ## smooth_call <- expr(smooth_test(.data))

## # Add steps
## init$add("smoothing", smooth_call)
## ## init$add("smoothing_two", smooth_call)

## # Execute steps
## init$execute(c("smoothing"))

## # Access results for specific steps
## init$get_result("smoothing")$data_output
## ## init$get_result("smoothing_two")$data_output

## # Get final result (data_output of the last step)

## init$final_result() %>%
## ##   tidyr::fill(.id_label, Sex, Province) %>%
## ##   mutate(
## ##     Exposures = as.numeric(Exposures),
## ##     Deaths = as.numeric(Deaths),
## ##     Rates = as.numeric(Rates),
## ##     AgeInt = 1
## ##   ) %>%
## ##   write_csv("~/Downloads/single_ages_error.csv")


## ## read_csv("~/Downloads/single_ages_error.csv") %>%
## ##   lt_flexible(
## ##     OAnew = 100,
## ##     age_out = "single",
## ##     extrapFrom = 80,
## ##     extrapFit = seq(60, 100, by = 5),
## ##     extrapLaw = "Kannisto",
## ##     radix = 100000,
## ##     SRB = 1.05,
## ##     a0rule = "Andreev-Kingkade",
## ##     axmethod = "UN (Greville)",
## ##     Sex = "Total"
## ##   )
