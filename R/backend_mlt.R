#' Model Life Tables Backend
#'
#' Server-side helpers for the model life tables module (T8). They build
#' Coale-Demeny and United Nations model life tables by single year of age,
#' as MortPak's MATCH (one indicator) and COMBIN (child and adult mortality)
#' do, using Sara Hertog's R implementation (R/vendor_mortpak_mlt.R) on the
#' single-age tables of `MortCast::MLT1Ylookup`. The abridged table is derived
#' from the single-age one with `DemoTools::lt_single2abridged()`, so both
#' share the same life table.
#'
#' @name mlt_backend
#' @noRd
NULL

#' Methods
#' @noRd
MLT_METHODS <- c(
  match = "One indicator (MATCH)",
  combin = "Child and adult mortality (COMBIN)"
)

#' Model life table families of `MortCast::MLT1Ylookup`
#' @noRd
MLT_FAMILIES <- c(
  "CD_West", "CD_North", "CD_South", "CD_East",
  "UN_General", "UN_Latin_American", "UN_Chilean", "UN_South_Asian", "UN_Far_Eastern"
)

#' Display names of the families (the same as the other modules)
#' @noRd
MLT_FAMILY_LABELS <- c(
  CD_West = "Coale-Demeny West", CD_North = "Coale-Demeny North",
  CD_South = "Coale-Demeny South", CD_East = "Coale-Demeny East",
  UN_General = "UN General", UN_Latin_American = "UN Latin American",
  UN_Chilean = "UN Chilean", UN_South_Asian = "UN South Asian",
  UN_Far_Eastern = "UN Far East Asian"
)

#' Indicators a model life table can be matched on (codes are the ones of the
#' MortPak functions)
#' @noRd
MLT_INDICATORS <- c(
  e0 = "Life expectancy at birth, e0",
  `1q0` = "Infant mortality, 1q0",
  `5q0` = "Under-five mortality, 5q0",
  `45q15` = "Adult mortality, 45q15",
  `35q15` = "Adult mortality, 35q15"
)

#' Adult mortality indicators COMBIN can use
#' @noRd
MLT_ADULT_INDICATORS <- c("45q15", "35q15")

#' Sexes (the MortPak tables are by sex only: there is no both-sexes table)
#' @noRd
MLT_SEX_LABELS <- c(female = "Female", male = "Male")

#' Default open age group of the output tables
#' @noRd
MLT_DEFAULT_OPEN_AGE <- 110

#' Cache of the indicator values of every level of each family and sex
#' @noRd
mlt_cache <- new.env(parent = emptyenv())

#' Code of a sex for the MortPak functions
#' @noRd
mlt_sex_code <- function(sex) {
  if (!sex %in% names(MLT_SEX_LABELS)) {
    stop("sex should be 'female' or 'male'")
  }
  if (identical(sex, "female")) "f" else "m"
}

#' Indicators of a life table: e0, 1q0, 5q0, 45q15 and 35q15
#'
#' @param lt Life table with `Age`, `lx` and `ex` (single or abridged ages).
#' @return Named numeric vector.
#' @noRd
mlt_indicators <- function(lt) {
  l <- function(a) lt$lx[lt$Age == a][1]
  c(
    e0 = lt$ex[lt$Age == 0][1],
    `1q0` = 1 - l(1) / l(0),
    `5q0` = 1 - l(5) / l(0),
    `45q15` = 1 - l(60) / l(15),
    `35q15` = 1 - l(50) / l(15)
  )
}

#' Indicator values of every level of a family and sex
#'
#' The e0 of each level is recomputed with `DemoTools::lt_single_mx()`, as
#' the MATCH function does, so the range matches what it can reach.
#'
#' @return Data frame with one row per level and one column per indicator.
#' @noRd
mlt_levels <- function(family, sex) {
  key <- paste(family, sex)
  if (!is.null(mlt_cache[[key]])) {
    return(mlt_cache[[key]])
  }
  lookup <- MortCast::MLT1Ylookup
  lookup <- as.data.frame(lookup[lookup$type == family & lookup$sex == if (identical(sex, "female")) 2 else 1, ])
  region <- if (startsWith(family, "CD_")) tolower(substr(family, 4, 4)) else "w"
  levels <- lapply(split(lookup, lookup$e0), function(d) {
    d <- d[order(d$age), ]
    lt <- DemoTools::lt_single_mx(nMx = d$mx, Age = d$age, Sex = mlt_sex_code(sex),
                                  a0rule = "cd", region = region)
    l <- function(a) d$lx[d$age == a]
    data.frame(
      e0 = lt$ex[1],
      `1q0` = 1 - l(1) / 1e5,
      `5q0` = 1 - l(5) / 1e5,
      `45q15` = 1 - l(60) / l(15),
      `35q15` = 1 - l(50) / l(15),
      check.names = FALSE
    )
  })
  levels <- do.call(rbind, levels)
  rownames(levels) <- NULL
  mlt_cache[[key]] <- levels
  levels
}

#' Range of an indicator in a family and sex
#' @noRd
mlt_range <- function(family, sex, indicator) {
  range(mlt_levels(family, sex)[[indicator]])
}

#' Stop with a readable message when a value is outside the family's range
#' @noRd
mlt_check_value <- function(family, sex, indicator, value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    stop(sprintf("A value of %s is required", indicator))
  }
  rng <- mlt_range(family, sex, indicator)
  if (value < rng[1] || value > rng[2]) {
    digits <- if (identical(indicator, "e0")) 1 else 4
    stop(sprintf(
      "%s = %s is outside the range of this model life table family for this sex (%s to %s)",
      indicator, format(value),
      format(round(rng[1], digits), nsmall = digits), format(round(rng[2], digits), nsmall = digits)
    ))
  }
  invisible(TRUE)
}

#' Stop when a life table is not a valid one (probabilities outside 0-1 or
#' survivors that increase with age)
#' @noRd
mlt_check_table <- function(lt) {
  q <- lt$nqx[-nrow(lt)]
  if (anyNA(lt$lx) || any(q < 0 | q > 1, na.rm = TRUE) || any(diff(lt$lx) > 1e-8)) {
    stop("The values given do not fit together: they give a life table with probabilities of dying outside 0-1. Check that the child and adult mortality values are consistent")
  }
  invisible(TRUE)
}

#' Build one model life table
#'
#' @param family One of `MLT_FAMILIES`.
#' @param sex `"female"` or `"male"`.
#' @param method `"match"` (one indicator) or `"combin"` (child and adult
#'   mortality).
#' @param indicator For `"match"`: the indicator matched, one of
#'   `names(MLT_INDICATORS)`.
#' @param value For `"match"`: its value.
#' @param q1,q5 For `"combin"`: 1q0 and 5q0 (at least one).
#' @param adult_indicator,adult_value For `"combin"`: `"45q15"` or `"35q15"`
#'   and its value.
#' @param open_age Open age group of the output tables.
#' @return List with `single` (single-age life table), `abridged` (abridged
#'   life table derived from it) and `achieved` (its e0, 1q0, 5q0, 45q15 and
#'   35q15).
#' @noRd
mlt_table <- function(family, sex, method = "match", indicator = "e0", value = NULL,
                      q1 = NULL, q5 = NULL, adult_indicator = "45q15", adult_value = NULL,
                      open_age = MLT_DEFAULT_OPEN_AGE) {
  if (!family %in% MLT_FAMILIES) {
    stop(sprintf("Model family should be one of: %s", paste(MLT_FAMILIES, collapse = ", ")))
  }
  sex_code <- mlt_sex_code(sex)
  if (identical(method, "match")) {
    if (!indicator %in% names(MLT_INDICATORS)) {
      stop(sprintf("indicator should be one of: %s", paste(names(MLT_INDICATORS), collapse = ", ")))
    }
    mlt_check_value(family, sex, indicator, value)
    single <- lt_model_cdun_match_single(
      type = family, Sex = sex_code, indicator = indicator, value = value, OAnew = open_age
    )
  } else if (identical(method, "combin")) {
    if (!adult_indicator %in% MLT_ADULT_INDICATORS) {
      stop("The adult mortality indicator should be 45q15 or 35q15")
    }
    if (is.null(q1) && is.null(q5)) {
      stop("Give 1q0, 5q0 or both")
    }
    if (!is.null(q1) && !is.null(q5) && !(q1 < q5)) {
      stop("1q0 must be lower than 5q0")
    }
    mlt_check_value(family, sex, adult_indicator, adult_value)
    if (!is.null(q5)) mlt_check_value(family, sex, "5q0", q5)
    if (!is.null(q1)) mlt_check_value(family, sex, "1q0", q1)
    single <- lt_model_cdun_combin_single(
      type = family, Sex = sex_code, q1 = q1 %||% NA, q5 = q5 %||% NA,
      indicator_adult = adult_indicator, value_adult = adult_value, OAnew = open_age
    )
  } else {
    stop("method should be 'match' or 'combin'")
  }
  single <- as.data.frame(single)
  mlt_check_table(single)
  abridged <- as.data.frame(DemoTools::lt_single2abridged(lx = single$lx, nLx = single$nLx, ex = single$ex))
  rownames(abridged) <- NULL
  list(single = single, abridged = abridged, achieved = mlt_indicators(single))
}

#' Example value of each indicator (pre-filled in the input table)
#' @noRd
MLT_DEFAULT_VALUES <- c(e0 = 60, `1q0` = 0.05, `5q0` = 0.08, `45q15` = 0.2, `35q15` = 0.12)

#' What the chart can show
#' @noRd
MLT_PLOT_MEASURES <- c(
  nMx = "Death rate, m(x) (log scale)",
  nqx = "Probability of dying, q(x) (log scale)",
  lx = "Survivors, l(x)"
)

#' Messages with values, as templates for translation
#' @noRd
MLT_MESSAGE_TEMPLATES <- c(
  "%s = %s is outside the range of this model life table family for this sex (%s to %s)",
  "A value of %s is required"
)

#' Sex code from what was typed in the input table
#'
#' @param x Values of the Sex column: female / male, f / m, or their labels in
#'   the selected language (`labels`).
#' @param labels Named vector of the translated labels (`female`, `male`).
#' @return `"female"`, `"male"` or `NA`.
#' @noRd
mlt_sex_from_input <- function(x, labels = MLT_SEX_LABELS) {
  v <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(v))
  for (code in names(MLT_SEX_LABELS)) {
    accepted <- tolower(c(code, substr(code, 1, 1), MLT_SEX_LABELS[[code]], labels[[code]]))
    out[v %in% accepted] <- code
  }
  out
}

#' Build the model life tables listed in the input table
#'
#' Each row of `requests` is one life table; a row that fails is reported and
#' the others are still built.
#'
#' @param requests Data frame with `Name`, `Sex` (`"female"` / `"male"`) and,
#'   for MATCH, `value`; for COMBIN, `q1`, `q5` and `adult`.
#' @param method `"match"` or `"combin"`.
#' @param family One of `MLT_FAMILIES`.
#' @param indicator For MATCH, the indicator of `value`.
#' @param adult_indicator For COMBIN, `"45q15"` or `"35q15"`.
#' @param open_age Open age group of the output tables.
#' @return List with `summary` (one row per table built: what was asked and
#'   the indicators achieved), `single` and `abridged` (the life tables, with
#'   `Name` and `Sex`), `failed` (`Name` and `message` of the rows that could
#'   not be built; not called `errors`, which `result$error` would partially
#'   match) and `params`.
#' @noRd
run_mlt <- function(requests, method = "match", family = "CD_West", indicator = "e0",
                    adult_indicator = "45q15", open_age = MLT_DEFAULT_OPEN_AGE) {
  if (!method %in% names(MLT_METHODS)) {
    stop("method should be 'match' or 'combin'")
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  requests <- as.data.frame(requests, stringsAsFactors = FALSE)
  value_cols <- if (identical(method, "match")) "value" else c("q1", "q5", "adult")
  for (col in setdiff(c("Name", "Sex", value_cols), names(requests))) requests[[col]] <- NA
  # Rows left empty (e.g. added and not filled) are skipped
  empty <- vapply(seq_len(nrow(requests)), function(i) {
    all(is.na(num(unlist(requests[i, value_cols])))) &&
      (is.na(requests$Name[i]) || !nzchar(trimws(requests$Name[i])))
  }, logical(1))
  requests <- requests[!empty, , drop = FALSE]
  if (nrow(requests) == 0) {
    stop("Add at least one life table to the table")
  }
  names_in <- trimws(as.character(requests$Name))
  names_in[is.na(names_in) | !nzchar(names_in)] <- paste("Table", which(is.na(names_in) | !nzchar(names_in)))
  requests$Name <- make.unique(names_in, sep = " ")

  summary_rows <- list()
  single_rows <- list()
  abridged_rows <- list()
  failed <- list()
  for (i in seq_len(nrow(requests))) {
    row <- requests[i, ]
    res <- tryCatch({
      sex <- as.character(row$Sex)
      if (is.na(sex) || !sex %in% names(MLT_SEX_LABELS)) {
        stop("Sex must be Female or Male")
      }
      opt <- function(x) { x <- num(x); if (length(x) == 0 || is.na(x)) NULL else x }
      if (identical(method, "match")) {
        mlt_table(family, sex, "match", indicator, num(row$value), open_age = open_age)
      } else {
        mlt_table(family, sex, "combin", q1 = opt(row$q1), q5 = opt(row$q5),
                  adult_indicator = adult_indicator, adult_value = num(row$adult), open_age = open_age)
      }
    }, error = function(e) e)
    if (inherits(res, "error")) {
      failed[[length(failed) + 1]] <- data.frame(Name = row$Name, message = conditionMessage(res),
                                                 stringsAsFactors = FALSE)
      next
    }
    asked <- if (identical(method, "match")) {
      stats::setNames(list(num(row$value)), indicator)
    } else {
      stats::setNames(list(num(row$q1), num(row$q5), num(row$adult)), c("1q0", "5q0", adult_indicator))
    }
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      Name = row$Name, Sex = row$Sex, family = family, method = method,
      asked = paste(sprintf("%s = %s", names(asked), vapply(asked, function(v) if (is.na(v)) "-" else format(v), "")),
                    collapse = ", "),
      e0 = res$achieved[["e0"]], q1 = res$achieved[["1q0"]], q5 = res$achieved[["5q0"]],
      q45_15 = res$achieved[["45q15"]], q35_15 = res$achieved[["35q15"]],
      stringsAsFactors = FALSE
    )
    single_rows[[length(single_rows) + 1]] <- cbind(Name = row$Name, Sex = row$Sex, res$single, stringsAsFactors = FALSE)
    abridged_rows[[length(abridged_rows) + 1]] <- cbind(Name = row$Name, Sex = row$Sex, res$abridged, stringsAsFactors = FALSE)
  }
  bind <- function(x) if (length(x) == 0) NULL else { out <- do.call(rbind, x); rownames(out) <- NULL; out }
  list(
    summary = bind(summary_rows),
    single = bind(single_rows),
    abridged = bind(abridged_rows),
    failed = bind(failed) %||% data.frame(Name = character(0), message = character(0)),
    params = list(method = method, family = family,
                  indicator = if (identical(method, "match")) indicator else NULL,
                  adult_indicator = if (identical(method, "combin")) adult_indicator else NULL,
                  open_age = open_age)
  )
}

#' Settings behind a model life table run, for display
#' @noRd
mlt_settings_items <- function(params) {
  item <- function(label, value, translate = FALSE) {
    data.frame(label = label, value = value, translate_value = translate, stringsAsFactors = FALSE)
  }
  items <- rbind(
    item("Method", MLT_METHODS[[params$method]], TRUE),
    item("Model life table family", MLT_FAMILY_LABELS[[params$family]], TRUE)
  )
  if (identical(params$method, "match")) {
    items <- rbind(items, item("Indicator", MLT_INDICATORS[[params$indicator]], TRUE))
  } else {
    items <- rbind(items, item("Adult mortality indicator", params$adult_indicator))
  }
  rbind(items, item("Open age group", paste0(params$open_age, "+")))
}

#' Plot model life tables by single year of age
#'
#' @param single Single-age life tables (`run_mlt()$single`).
#' @param measure One of `names(MLT_PLOT_MEASURES)`.
#' @param title,x_label,y_label Plot texts.
#' @param age_label Word for "Age" in the hover text.
#' @return A ggplot object with one line per life table.
#' @noRd
mlt_plot <- function(single, measure = "nMx", title = "Model life tables", x_label = "Age",
                     y_label = MLT_PLOT_MEASURES[[measure]], age_label = "Age") {
  if (!measure %in% names(MLT_PLOT_MEASURES)) measure <- "nMx"
  d <- single
  # The open age group has q = 1 and is left out of the chart
  d <- d[d$Age < max(d$Age), , drop = FALSE]
  d$value <- d[[measure]]
  d$hover <- sprintf("%s<br>%s = %d<br>%s = %s", d$Name, age_label, as.integer(d$Age),
                     measure, formatC(d$value, digits = 5, format = "g"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Age, y = .data$value, color = .data$Name,
                                       group = .data$Name, text = .data$hover)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(title = title, x = x_label, y = y_label, color = NULL) +
    ggplot2::theme_minimal(base_size = 13)
  if (measure %in% c("nMx", "nqx")) {
    p <- p + ggplot2::scale_y_log10()
  }
  p
}
