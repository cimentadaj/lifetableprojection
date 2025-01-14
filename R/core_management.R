#' Get number of cores for parallel processing
#'
#' Determines the number of cores to use based on environment variables.
#' If LOCAL_ENV is set to TRUE in .env, uses 1 core.
#' Otherwise defaults to 12 cores for server environment.
#'
#' @return Integer number of cores to use
#' @importFrom utils readRenviron
#' @export
get_n_cores <- function() {
  # Try to load .env file if it exists
  if (file.exists(".env")) {
    readRenviron(".env")
  }

  # Check if LOCAL_ENV is set
  is_local <- Sys.getenv("LOCAL_ENV", unset = "FALSE")

  # Convert string to logical
  is_local <- tolower(is_local) == "true"

  # Return appropriate number of cores
  if (is_local) {
    message("Running in local environment with 1 core")
    return(1)
  } else {
    message("Running in server environment with 12 cores")
    return(12)
  }
}
