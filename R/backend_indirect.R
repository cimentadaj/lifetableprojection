#' Shared backend layer for the indirect estimation modules
#'
#' Helpers reused by the modules that wrap `MortIndEst` (child mortality,
#' orphanhood, intercensal survival). They run an estimation function once per
#' group so that one bad group does not stop the whole batch.
#'
#' @name indirect_backend
#' @noRd
NULL

#' Evaluate a MortIndEst call with the packages it expects on the search path
#'
#' Several MortIndEst functions (the orphanhood ones among them) call dplyr,
#' purrr and tidyr functions without importing them, so they only work when
#' those packages are attached. Without dplyr, `lag()` silently resolves to
#' `stats::lag()` and gives wrong results instead of an error. This attaches
#' the packages that are not attached yet, evaluates `expr` and then detaches
#' everything that was added to the search path meanwhile.
#'
#' @param expr Expression to evaluate.
#' @param extra Further packages to attach, before dplyr so that dplyr's
#'   functions come first (see `ORPHANHOOD_SPECTRUM_PACKAGES`).
#' @return The value of `expr`.
#' @noRd
with_mortindest_deps <- function(expr, extra = character(0)) {
  before <- search()
  on.exit({
    added <- setdiff(search(), before)
    for (entry in added[startsWith(added, "package:")]) {
      try(detach(entry, character.only = TRUE), silent = TRUE)
    }
  }, add = TRUE)
  for (pkg in c(extra, "tidyr", "purrr", "dplyr")) {
    if (!paste0("package:", pkg) %in% search()) {
      suppressPackageStartupMessages(attachNamespace(pkg))
    }
  }
  force(expr)
}

#' Run an estimation function separately for each group
#'
#' Splits `data` on `.id`, checks each group, calls `fun` on it and binds the
#' per-group results back together. Groups that fail the check, or whose
#' estimation throws an error, are reported in `group_errors` and the other
#' groups still compute.
#'
#' @param data Data frame. When there is no `.id` column all rows are treated
#'   as a single group with `.id = 1`.
#' @param fun Function called as `fun(df, ...)` for each group. It must return
#'   a named list of data frames (for example `estimates` and `mlt`).
#' @param ... Further arguments passed to `fun`.
#' @param check Optional function called as `check(df)` before `fun`. It must
#'   return the app's standard checks table (`check`, `message`, `pass`); a
#'   group with any `"Fail"` row is skipped and its messages are reported.
#' @return A list with one element per table returned by `fun` (rows bound
#'   across successful groups, with `.id` as the first column and `.id_label`
#'   as the last column when `data` has one; `NULL` when no group succeeded),
#'   plus `group_errors`: a data frame with columns `.id` (character) and
#'   `message`, one row per failed group.
#' @noRd
run_by_group <- function(data, fun, ..., check = NULL) {
  if (!".id" %in% names(data)) {
    data$.id <- 1L
  }
  ids <- unique(data$.id)

  # Each element: list(.id, tables, failure). `failure` is NULL on success.
  per_group <- lapply(ids, function(gid) {
    df <- data[data$.id == gid, , drop = FALSE]
    if (!is.null(check)) {
      checks <- check(df)
      if (any(checks$pass == "Fail")) {
        msg <- paste(stats::na.omit(checks$message), collapse = "; ")
        return(list(.id = gid, tables = NULL, failure = msg))
      }
    }
    tryCatch({
      tables <- fun(df, ...)
      tables <- lapply(tables, function(tbl) {
        if (is.null(tbl)) return(NULL)
        cbind(.id = rep(gid, nrow(tbl)), tbl, stringsAsFactors = FALSE)
      })
      list(.id = gid, tables = tables, failure = NULL)
    }, error = function(e) {
      list(.id = gid, tables = NULL, failure = conditionMessage(e))
    })
  })

  ok <- Filter(function(r) is.null(r$failure), per_group)
  failed <- Filter(function(r) !is.null(r$failure), per_group)

  table_names <- unique(unlist(lapply(ok, function(r) names(r$tables))))
  labels <- if (".id_label" %in% names(data)) unique(data[, c(".id", ".id_label")]) else NULL

  results <- lapply(stats::setNames(table_names, table_names), function(name) {
    parts <- lapply(ok, function(r) r$tables[[name]])
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) return(NULL)
    out <- do.call(rbind, parts)
    if (!is.null(labels)) {
      out$.id_label <- labels$.id_label[match(out$.id, labels$.id)]
    }
    out
  })

  results$group_errors <- data.frame(
    .id = vapply(failed, function(r) as.character(r$.id), character(1)),
    message = vapply(failed, function(r) r$failure, character(1)),
    stringsAsFactors = FALSE
  )
  results
}

#' Translate messages that may carry values
#'
#' Validation and estimation messages are built in English and some carry
#' values (column names, numbers). Each message is looked up as a whole; when
#' it is not a translation key it is matched against `templates`, English
#' strings with `%s` placeholders: the template is translated and the values
#' are put back. Messages joined with "; " are translated part by part, and
#' unknown messages are returned unchanged.
#'
#' @param messages Character vector of English messages.
#' @param i18n Translator object.
#' @param templates Character vector of English templates with `%s`.
#' @return Character vector of translated messages.
#' @noRd
translate_messages <- function(messages, i18n, templates = character(0)) {
  escape <- function(x) gsub("([][.|()\\^{}+$*?\\\\])", "\\\\\\1", x, perl = TRUE)
  patterns <- paste0("^", gsub("%s", "(.+)", escape(templates), fixed = TRUE), "$")
  translate_one <- function(msg) {
    for (i in seq_along(templates)) {
      hit <- regmatches(msg, regexec(patterns[i], msg, perl = TRUE))[[1]]
      if (length(hit) > 1) {
        translated <- suppressWarnings(i18n$t(templates[i]))
        return(do.call(sprintf, c(list(translated), as.list(hit[-1]))))
      }
    }
    suppressWarnings(i18n$t(msg))
  }
  vapply(messages, function(msg) {
    if (is.na(msg) || !nzchar(msg)) return(msg)
    parts <- strsplit(msg, "; ", fixed = TRUE)[[1]]
    paste(vapply(parts, translate_one, character(1)), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
