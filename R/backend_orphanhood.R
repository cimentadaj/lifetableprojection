#' Adult Mortality from Orphanhood Backend
#'
#' Server-side helpers for the orphanhood module. They wrap
#' `MortIndEst::orphanhood_one()` (one census or survey, Timaeus 1992 as
#' implemented in IUSSP Tools for Demographic Estimation) and
#' `MortIndEst::orphanhood_two()` (two censuses or surveys, Timaeus or
#' Preston-Chen variants), which estimate adult mortality (45q15 and 35q15)
#' from the proportion of respondents whose mother or father is alive.
#'
#' Every run uses the one-parameter Brass logit fit against a standard life
#' table that this file builds and passes as `mlt_data_input`: either a model
#' life table of the chosen family and level, or a standard entered by the
#' user. MortIndEst's built-in family lookup is not used because it always
#' selects the male model life tables, also for maternal orphanhood.
#'
#' @name orphanhood_backend
#' @noRd
NULL

#' Model life table families available as standards
#' @noRd
ORPHANHOOD_FAMILIES <- c(
  "CD_West", "CD_North", "CD_South", "CD_East",
  "UN_General", "UN_Latin_American", "UN_Chilean", "UN_South_Asian", "UN_Far_Eastern"
)

#' Display names of the model life table families
#' @noRd
ORPHANHOOD_FAMILY_LABELS <- c(
  CD_West = "Coale-Demeny West", CD_North = "Coale-Demeny North",
  CD_South = "Coale-Demeny South", CD_East = "Coale-Demeny East",
  UN_General = "UN General", UN_Latin_American = "UN Latin American",
  UN_Chilean = "UN Chilean", UN_South_Asian = "UN South Asian",
  UN_Far_Eastern = "UN Far East Asian"
)

#' Two-census variants
#' @noRd
ORPHANHOOD_METHODS <- c("timaeus", "preston_chen")

#' Display names of the two-census variants
#' @noRd
ORPHANHOOD_METHOD_LABELS <- c(timaeus = "Timaeus", preston_chen = "Preston-Chen")

#' Ages of a user-entered standard life table
#' @noRd
ORPHANHOOD_STANDARD_AGES <- seq(15, 85, 5)

#' Standardise orphanhood input columns
#'
#' Matches column names case-insensitively (ignoring `_`, `.` and spaces), so
#' `not_orphaned`, `NotOrphaned` and `NOTORPHANED` are all accepted. When the
#' data hold `NotOrphaned2` and `NotOrphaned` (but no `NotOrphaned1`),
#' `NotOrphaned` is taken as the first observation.
#'
#' @param data Data frame of orphanhood data.
#' @return Data frame with standard column names, keeping all other columns.
#' @noRd
prepare_orphanhood_input <- function(data) {
  data <- as.data.frame(data)
  key <- gsub("[_. ]", "", tolower(names(data)))
  standard <- c(
    "Age", "NotOrphaned", "NotOrphaned1", "NotOrphaned2",
    "Date", "Date1", "Date2", "MAC", "MAC1", "MAC2", "HIVPrev"
  )
  for (std in standard) {
    hit <- which(key == tolower(std))
    if (length(hit) > 0 && !std %in% names(data)) {
      names(data)[hit[1]] <- std
    }
  }
  if ("NotOrphaned2" %in% names(data) && !"NotOrphaned1" %in% names(data) &&
      "NotOrphaned" %in% names(data)) {
    names(data)[names(data) == "NotOrphaned"] <- "NotOrphaned1"
  }
  data
}

#' Number of observations (censuses or surveys) in orphanhood data
#'
#' @param data Data frame already passed through `prepare_orphanhood_input()`.
#' @return `2` when the data hold a `NotOrphaned2` column, `1` otherwise.
#' @noRd
orphanhood_observations <- function(data) {
  if ("NotOrphaned2" %in% names(data)) 2L else 1L
}

#' Build one validation row in the app's standard format
#' @noRd
orphanhood_check_row <- function(check, failed, message) {
  data.frame(
    check = check,
    message = if (isTRUE(failed)) message else NA_character_,
    pass = if (isTRUE(failed)) "Fail" else "Pass",
    stringsAsFactors = FALSE
  )
}

#' Numeric values of a column, `NA` where the value is not a number
#' @noRd
orphanhood_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

#' Validate one group of orphanhood data
#' @noRd
check_orphanhood_group <- function(df) {
  obs <- orphanhood_observations(df)
  prop_cols <- if (obs == 2) c("NotOrphaned1", "NotOrphaned2") else "NotOrphaned"
  required <- c("Age", prop_cols)
  missing_cols <- setdiff(required, names(df))
  has_cols <- length(missing_cols) == 0

  rows <- list(orphanhood_check_row(
    "check required columns",
    !has_cols,
    paste0(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      ". Provide Age and NotOrphaned (one census or survey), or Age, NotOrphaned1 and NotOrphaned2 (two censuses or surveys)"
    )
  ))
  if (!has_cols) {
    return(do.call(rbind, rows))
  }

  vals <- lapply(df[required], orphanhood_num)
  non_numeric <- vapply(required, function(col) {
    any(is.na(vals[[col]]) & !is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
  }, logical(1))
  rows[[length(rows) + 1]] <- orphanhood_check_row(
    "check numeric columns",
    any(non_numeric),
    paste0("Columns must be numeric: ", paste(required[non_numeric], collapse = ", "))
  )

  has_na <- vapply(vals, function(x) any(is.na(x)), logical(1))
  rows[[length(rows) + 1]] <- orphanhood_check_row(
    "check missing values",
    any(has_na),
    paste0("Missing values in columns: ", paste(required[has_na], collapse = ", "))
  )

  age <- sort(vals$Age)
  ages_ok <- length(age) > 0 && !anyNA(age) &&
    all(age %% 5 == 0) && !anyDuplicated(age) &&
    all(diff(age) == 5) && sum(age >= 5 & age <= 45) >= 3
  rows[[length(rows) + 1]] <- orphanhood_check_row(
    "check age groups",
    !ages_ok,
    "Age must hold consecutive five-year groups of respondents (lower bounds 0, 5, 10, ...), each once, including at least three groups between 5 and 45"
  )

  props <- unlist(vals[prop_cols])
  rows[[length(rows) + 1]] <- orphanhood_check_row(
    "check proportions",
    any(!is.na(props) & (props <= 0 | props > 1)),
    paste0(
      paste(prop_cols, collapse = " and "),
      " must be proportions above 0 and up to 1 (for example 0.95, not 95)"
    )
  )

  # The proportion with a living parent falls with the age of the respondent.
  # A proportion that ends higher than it starts is usually the proportion
  # orphaned, or the age groups are in the wrong order.
  ordered <- order(vals$Age)
  rising <- vapply(prop_cols, function(col) {
    p <- vals[[col]][ordered]
    p <- p[!is.na(p)]
    length(p) >= 2 && p[length(p)] >= p[1]
  }, logical(1))
  rows[[length(rows) + 1]] <- orphanhood_check_row(
    "check proportions decline with age",
    any(rising),
    paste0(
      paste(prop_cols[rising], collapse = " and "),
      " must decline with age: give the proportion of respondents whose parent is alive, not the proportion orphaned"
    )
  )

  # Optional columns are checked only when present
  mac_cols <- intersect(if (obs == 2) c("MAC", "MAC1", "MAC2") else "MAC", names(df))
  for (col in mac_cols) {
    v <- unique(stats::na.omit(orphanhood_num(df[[col]])))
    rows[[length(rows) + 1]] <- orphanhood_check_row(
      paste("check", col),
      length(v) != 1 || v < 15 || v > 50,
      sprintf("%s (mean age at childbearing) must be a single value between 15 and 50 in each group", col)
    )
  }

  date_cols <- intersect(if (obs == 2) c("Date1", "Date2") else "Date", names(df))
  dates <- list()
  for (col in date_cols) {
    v <- unique(stats::na.omit(orphanhood_num(df[[col]])))
    dates[[col]] <- v
    rows[[length(rows) + 1]] <- orphanhood_check_row(
      paste("check", col),
      length(v) != 1 || v < 1900 || v > 2100,
      sprintf("%s must be a single decimal year (for example 1997.8) in each group", col)
    )
  }
  if (all(c("Date1", "Date2") %in% names(dates)) &&
      length(dates$Date1) == 1 && length(dates$Date2) == 1) {
    rows[[length(rows) + 1]] <- orphanhood_check_row(
      "check date order",
      dates$Date2 <= dates$Date1,
      "Date2 must be later than Date1"
    )
  }

  if ("HIVPrev" %in% names(df)) {
    hiv <- orphanhood_num(df$HIVPrev)
    rows[[length(rows) + 1]] <- orphanhood_check_row(
      "check HIVPrev",
      obs == 2 || any(is.na(hiv)) || any(hiv < 0 | hiv > 1),
      if (obs == 2) {
        "The HIV adjustment (HIVPrev) is available only for one census or survey"
      } else {
        "HIVPrev must be a proportion between 0 and 1 in every age group"
      }
    )
  }

  do.call(rbind, rows)
}

#' Validate orphanhood input data
#'
#' Runs all checks per group (`.id`) and returns the standard validation table
#' used by the shared data context.
#'
#' @param data Data frame of orphanhood data, optionally with `.id`.
#' @return Data frame with columns `check`, `message` and `pass`.
#' @noRd
check_data_orphanhood <- function(data) {
  data <- prepare_orphanhood_input(data)
  if (!".id" %in% names(data)) {
    data$.id <- "all"
  }
  split_data <- split(data, data$.id)
  do.call(rbind, lapply(split_data, check_orphanhood_group))
}

#' Validate Orphanhood Data
#'
#' Wrapper used by the shared data context of the orphanhood module.
#'
#' @param data Data frame to be validated.
#' @param i18n Translator object for internationalization.
#' @return Data frame containing validation results.
#' @noRd
validateData_orphanhood <- function(data, i18n) {
  check_data_orphanhood(data)
}

#' Snap a life expectancy to the levels of the model life tables
#'
#' @param e0 Life expectancy at birth.
#' @return The nearest level on the 2.5-year grid between 20 and 100.
#' @noRd
orphanhood_snap_e0 <- function(e0) {
  e0 <- suppressWarnings(as.numeric(e0))
  if (length(e0) != 1 || is.na(e0)) {
    stop("The life expectancy of the standard must be a number")
  }
  min(max(round(e0 / 2.5) * 2.5, 20), 100)
}

#' Model life table used as the Brass logit standard
#'
#' @param family Model life table family (one of `ORPHANHOOD_FAMILIES`).
#' @param maternal `TRUE` for maternal orphanhood (female tables), `FALSE` for
#'   paternal orphanhood (male tables).
#' @param e0 Life expectancy at birth of the standard; snapped to the
#'   available levels.
#' @return Data frame with `age` (0 to 100) and `lx` (survivors out of 1).
#' @noRd
orphanhood_model_standard <- function(family, maternal, e0) {
  if (!family %in% ORPHANHOOD_FAMILIES) {
    stop(sprintf(
      "Unknown model life table family '%s'. Choose one of: %s",
      family, paste(ORPHANHOOD_FAMILIES, collapse = ", ")
    ))
  }
  level <- orphanhood_snap_e0(e0)
  sex_code <- if (isTRUE(maternal)) 2 else 1
  mlt <- MortCast::MLTlookup
  keep <- mlt$type == family & mlt$sex == sex_code & mlt$e0 == level & mlt$age <= 100
  out <- data.frame(age = mlt$age[keep], lx = mlt$lx[keep] / 1e5)
  out[order(out$age), , drop = FALSE]
}

#' Check a user-entered standard life table
#'
#' @param standard Data frame with `age` and `lx`.
#' @return Character vector of problems (empty when the standard is usable).
#' @noRd
check_orphanhood_standard <- function(standard) {
  if (is.null(standard) || !all(c("age", "lx") %in% names(standard))) {
    return("The custom standard needs columns age and lx")
  }
  age <- orphanhood_num(standard$age)
  lx <- orphanhood_num(standard$lx)
  problems <- character(0)
  if (!isTRUE(all(ORPHANHOOD_STANDARD_AGES %in% age))) {
    problems <- c(problems, "The custom standard must cover ages 15 to 85 in five-year steps")
  }
  lx <- lx[order(age)]
  if (anyNA(lx)) {
    problems <- c(problems, "Enter lx for every age of the custom standard")
  } else if (any(lx <= 0 | lx > 1)) {
    problems <- c(problems, "Custom standard lx values must be survivors out of 1 (between 0 and 1)")
  } else if (any(diff(lx) > 0)) {
    problems <- c(problems, "Custom standard lx values must not increase with age")
  }
  problems
}

#' One value per group: from a data column when present, otherwise a fallback
#'
#' @param df Data for one group.
#' @param cols Candidate columns, in order of preference.
#' @param fallback Value used when none of the columns holds a value.
#' @param what Description used in the error message.
#' @noRd
orphanhood_group_value <- function(df, cols, fallback, what) {
  for (col in intersect(cols, names(df))) {
    vals <- unique(stats::na.omit(orphanhood_num(df[[col]])))
    if (length(vals) > 1) {
      stop(sprintf("%s must take a single value within each group", col))
    }
    if (length(vals) == 1) {
      return(vals)
    }
  }
  fallback <- if (is.null(fallback)) NA_real_ else suppressWarnings(as.numeric(fallback))
  if (length(fallback) != 1 || is.na(fallback)) {
    stop(sprintf("A %s is required", what))
  }
  fallback
}

#' Estimate adult mortality from orphanhood for a single group
#'
#' @param df Data frame for one group (already standardised).
#' @param maternal `TRUE` for maternal orphanhood, `FALSE` for paternal.
#' @param method Two-census variant (`"timaeus"` or `"preston_chen"`); ignored
#'   for one census or survey.
#' @param standard Standard life table (`age`, `lx`) for the Brass logit fit.
#' @param mac Mean age at childbearing, used when the data hold no MAC column.
#' @param date_obs Decimal year of the census or survey (one observation),
#'   used when the data hold no Date column.
#' @param date1,date2 Decimal years of the two observations, used when the
#'   data hold no Date1 / Date2 columns.
#' @param hiv_vert_transm,hiv_fert_ratio,hiv_prop_partner HIV adjustment
#'   parameters, used when the data hold an HIVPrev column.
#' @return List with `estimates` (one row per age group of respondents) and
#'   `lx` (fitted life tables).
#' @noRd
orphanhood_estimate_group <- function(df,
                                      maternal = TRUE,
                                      method = "timaeus",
                                      standard,
                                      mac = NULL,
                                      date_obs = NULL,
                                      date1 = NULL,
                                      date2 = NULL,
                                      hiv_vert_transm = 1 / 3,
                                      hiv_fert_ratio = 3 / 4,
                                      hiv_prop_partner = 1 / 2) {
  df <- df[order(orphanhood_num(df$Age)), , drop = FALSE]
  age <- orphanhood_num(df$Age)
  obs <- orphanhood_observations(df)
  std <- data.frame(age = orphanhood_num(standard$age), lx = orphanhood_num(standard$lx))

  if (obs == 1) {
    date_obs <- orphanhood_group_value(df, "Date", date_obs, "date of observation")
    mac <- orphanhood_group_value(df, "MAC", mac, "mean age at childbearing")
    hiv <- if ("HIVPrev" %in% names(df)) orphanhood_num(df$HIVPrev) else NULL
    # Rows with implausible values are flagged below, so the NaN warnings
    # they raise inside MortIndEst are not needed.
    res <- with_mortindest_deps(suppressWarnings(suppressMessages(MortIndEst::orphanhood_one(
      prop_not_orphan = orphanhood_num(df$NotOrphaned),
      age = age,
      maternal = maternal,
      mac = mac,
      date = date_obs,
      mlt_data_input = std,
      brass_logit = TRUE,
      HIV_prev_antenatal = if (!is.null(hiv) && maternal) hiv else NULL,
      HIV_prev = if (!is.null(hiv) && !maternal) hiv else NULL,
      HIV_vert_transm = hiv_vert_transm,
      HIV_fert_ratio = hiv_fert_ratio,
      HIV_prop_partner = hiv_prop_partner,
      verbose = FALSE
    ))))
    index <- as.data.frame(res$adult_mort_index)
    ref_date <- index$time_location
  } else {
    date1 <- orphanhood_group_value(df, "Date1", date1, "date of the first observation")
    date2 <- orphanhood_group_value(df, "Date2", date2, "date of the second observation")
    if (date2 <= date1) {
      stop("The second observation must be later than the first")
    }
    mac1 <- orphanhood_group_value(df, c("MAC1", "MAC"), mac, "mean age at childbearing")
    mac2 <- orphanhood_group_value(df, c("MAC2", "MAC"), mac, "mean age at childbearing")
    if (identical(method, "timaeus") && !all(c(15, 20) %in% age)) {
      stop("The Timaeus two-census method needs respondents aged 15-19 and 20-24")
    }
    # Rows with implausible values are flagged below, so the NaN warnings
    # they raise inside MortIndEst are not needed.
    res <- with_mortindest_deps(suppressWarnings(suppressMessages(MortIndEst::orphanhood_two(
      prop1_not_orphan = orphanhood_num(df$NotOrphaned1),
      prop2_not_orphan = orphanhood_num(df$NotOrphaned2),
      age1 = age,
      age2 = age,
      date1 = date1,
      date2 = date2,
      maternal = maternal,
      method = method,
      mac1 = mac1,
      mac2 = mac2,
      mlt_data_input = std,
      brass_logit = TRUE,
      verbose = FALSE
    ))))
    index <- as.data.frame(res$adult_mort_index)
    # Two-census estimates refer to the period between the observations
    ref_date <- rep((date1 + date2) / 2, nrow(index))
  }

  if (nrow(index) == 0) {
    stop("The method returned no estimates for these age groups")
  }
  plausible <- is.finite(index$q15_45) & index$q15_45 > 0 & index$q15_45 < 1 &
    is.finite(index$q15_35) & index$q15_35 > 0 & index$q15_35 < 1
  if (!any(plausible)) {
    stop("No plausible estimates: 45q15 fell outside 0-1 in every age group. Check that the proportions not orphaned are proportions that decline with age")
  }

  estimates <- data.frame(
    Age = index$age,
    age_label = paste0(index$age, "-", index$age + 4),
    q15_45 = index$q15_45,
    q15_35 = index$q15_35,
    ref_date = ref_date,
    plausible = plausible,
    stringsAsFactors = FALSE
  )
  if (obs == 1) {
    estimates$years_before <- date_obs - estimates$ref_date
  }

  lx_est <- as.data.frame(res$lx_estimates)
  lx <- data.frame(
    Age = lx_est$age,
    age_mlt = lx_est$age_mlt,
    lx = lx_est$lx_interp,
    stringsAsFactors = FALSE
  )
  lx <- lx[!is.na(lx$lx), , drop = FALSE]

  list(estimates = estimates, lx = lx)
}

#' Estimate adult mortality from orphanhood for all groups
#'
#' Splits the data by `.id`, applies the method to each group and keeps going
#' when a single group fails.
#'
#' @param data Data frame with `Age` and `NotOrphaned` (one observation) or
#'   `NotOrphaned1` and `NotOrphaned2` (two observations), optional `Date`,
#'   `Date1`, `Date2`, `MAC`, `MAC1`, `MAC2` and `HIVPrev` columns, and
#'   optionally `.id` / `.id_label`.
#' @param maternal `TRUE` (default) for maternal orphanhood, `FALSE` for
#'   paternal orphanhood.
#' @param method Two-census variant: `"timaeus"` (default) or
#'   `"preston_chen"`. Ignored for one census or survey.
#' @param family Model life table family of the standard.
#' @param std_e0 Life expectancy at birth of the standard model life table.
#' @param standard Optional user-entered standard: data frame with `age`
#'   (15 to 85 by 5) and `lx` (survivors out of 1). When given, `family` and
#'   `std_e0` are not used.
#' @param mac Mean age at childbearing for groups without a MAC column.
#' @param date_obs,date1,date2 Decimal years of observation for groups
#'   without date columns.
#' @param hiv_vert_transm,hiv_fert_ratio,hiv_prop_partner HIV adjustment
#'   parameters (Timaeus and Nunn 1997), used when the data hold `HIVPrev`.
#' @return List with `estimates` (all groups, with `.id`), `lx` (fitted life
#'   tables), `standard` (the standard life table used), `group_errors` (data
#'   frame of `.id` and `message`) and `params`.
#' @noRd
run_orphanhood <- function(data,
                           maternal = TRUE,
                           method = "timaeus",
                           family = "CD_West",
                           std_e0 = 60,
                           standard = NULL,
                           mac = NULL,
                           date_obs = NULL,
                           date1 = NULL,
                           date2 = NULL,
                           hiv_vert_transm = 1 / 3,
                           hiv_fert_ratio = 3 / 4,
                           hiv_prop_partner = 1 / 2) {
  if (!method %in% ORPHANHOOD_METHODS) {
    stop(sprintf(
      "Unknown two-census method '%s'. Choose one of: %s",
      method, paste(ORPHANHOOD_METHODS, collapse = ", ")
    ))
  }
  maternal <- isTRUE(maternal)

  if (is.null(standard)) {
    std_e0 <- orphanhood_snap_e0(std_e0)
    std <- orphanhood_model_standard(family, maternal, std_e0)
  } else {
    problems <- check_orphanhood_standard(standard)
    if (length(problems) > 0) {
      stop(paste(problems, collapse = "; "))
    }
    std <- data.frame(age = orphanhood_num(standard$age), lx = orphanhood_num(standard$lx))
    std <- std[order(std$age), , drop = FALSE]
  }

  data <- prepare_orphanhood_input(data)
  obs <- orphanhood_observations(data)
  by_group <- run_by_group(
    data,
    orphanhood_estimate_group,
    maternal = maternal, method = method, standard = std,
    mac = mac, date_obs = date_obs, date1 = date1, date2 = date2,
    hiv_vert_transm = hiv_vert_transm, hiv_fert_ratio = hiv_fert_ratio,
    hiv_prop_partner = hiv_prop_partner,
    check = check_orphanhood_group
  )

  hiv <- obs == 1 && "HIVPrev" %in% names(data)
  list(
    estimates = by_group[["estimates"]],
    lx = by_group[["lx"]],
    standard = std,
    group_errors = by_group[["group_errors"]],
    params = list(
      observations = obs,
      maternal = maternal,
      method = if (obs == 2) method else NA_character_,
      standard_type = if (is.null(standard)) "model" else "custom",
      family = if (is.null(standard)) family else NA_character_,
      std_e0 = if (is.null(standard)) std_e0 else NA_real_,
      mac = mac,
      date_obs = if (obs == 1) date_obs else NULL,
      date1 = if (obs == 2) date1 else NULL,
      date2 = if (obs == 2) date2 else NULL,
      hiv = hiv,
      hiv_vert_transm = if (hiv) hiv_vert_transm else NA_real_,
      hiv_fert_ratio = if (hiv) hiv_fert_ratio else NA_real_,
      hiv_prop_partner = if (hiv && !maternal) hiv_prop_partner else NA_real_
    )
  )
}

#' Settings behind an orphanhood run, for display
#'
#' @param params Parameters of a run (`run_orphanhood()$params`).
#' @return Data frame with `label`, `value` and `translate_value` (English; the
#'   module translates the labels, and the values flagged in
#'   `translate_value`), one row per setting that applies to the run.
#' @noRd
orphanhood_settings_items <- function(params) {
  two <- identical(as.integer(params$observations), 2L)
  item <- function(label, value, translate = FALSE) {
    data.frame(label = label, value = value, translate_value = translate,
               stringsAsFactors = FALSE)
  }
  data_value <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) NULL else format(x)
  }
  items <- list(
    item("Parent", if (isTRUE(params$maternal)) "Mother" else "Father", TRUE)
  )
  if (two) {
    items[[length(items) + 1]] <- item("Method", unname(ORPHANHOOD_METHOD_LABELS[params$method]))
  }
  if (identical(params$standard_type, "custom")) {
    items[[length(items) + 1]] <- item("Standard", "Custom standard", TRUE)
  } else {
    items[[length(items) + 1]] <- item(
      "Standard",
      sprintf("%s, e0 = %s", unname(ORPHANHOOD_FAMILY_LABELS[params$family]), format(params$std_e0))
    )
  }
  mac <- data_value(params$mac)
  items[[length(items) + 1]] <- item(
    "Mean age at childbearing", mac %||% "MAC column of your data", is.null(mac)
  )
  if (two) {
    d1 <- data_value(params$date1)
    d2 <- data_value(params$date2)
    items[[length(items) + 1]] <- item(
      "Dates of observation",
      if (is.null(d1) || is.null(d2)) "Date1 and Date2 columns of your data" else paste(d1, "/", d2),
      is.null(d1) || is.null(d2)
    )
  } else {
    d <- data_value(params$date_obs)
    items[[length(items) + 1]] <- item(
      "Date of observation", d %||% "Date column of your data", is.null(d)
    )
  }
  if (isTRUE(params$hiv)) {
    items[[length(items) + 1]] <- item("HIV adjustment", "Yes", TRUE)
  }
  do.call(rbind, items)
}

#' Have the controls moved away from the settings of the last run?
#'
#' Only settings that affect the run are compared. A `NULL` control value (a
#' control that is being re-rendered, e.g. after a language switch, or one that
#' is hidden because the data carry the value) counts as unchanged.
#'
#' @param params Parameters of the last run (`run_orphanhood()$params`).
#' @param current List with the current control values: `parent`
#'   (`"maternal"` / `"paternal"`), `method`, `standard_type`, `family`,
#'   `std_e0`, `mac`, `date_obs`, `date1`, `date2`, `hiv_vert_transm`,
#'   `hiv_fert_ratio`, `hiv_prop_partner`.
#' @return `TRUE` when the shown results no longer match the controls.
#' @noRd
orphanhood_settings_changed <- function(params, current) {
  differs <- function(old, new) {
    !is.null(new) && !identical(as.character(old), as.character(new))
  }
  differs_num <- function(old, new) {
    !is.null(new) && !is.null(old) && !all(is.na(old)) &&
      !isTRUE(all.equal(as.numeric(old), as.numeric(new)))
  }
  two <- identical(as.integer(params$observations), 2L)
  model <- identical(params$standard_type, "model")
  parent <- if (isTRUE(params$maternal)) "maternal" else "paternal"
  e0_now <- if (is.null(current$std_e0)) NULL else tryCatch(orphanhood_snap_e0(current$std_e0), error = function(e) NA)

  differs(parent, current$parent) ||
    (two && differs(params$method, current$method)) ||
    differs(params$standard_type, current$standard_type) ||
    (model && differs(params$family, current$family)) ||
    (model && differs_num(params$std_e0, e0_now)) ||
    differs_num(params$mac, current$mac) ||
    (!two && differs_num(params$date_obs, current$date_obs)) ||
    (two && differs_num(params$date1, current$date1)) ||
    (two && differs_num(params$date2, current$date2)) ||
    (isTRUE(params$hiv) && differs_num(params$hiv_vert_transm, current$hiv_vert_transm)) ||
    (isTRUE(params$hiv) && differs_num(params$hiv_fert_ratio, current$hiv_fert_ratio)) ||
    (isTRUE(params$hiv) && !isTRUE(params$maternal) &&
       differs_num(params$hiv_prop_partner, current$hiv_prop_partner))
}

#' Diagnostic notes for one group of orphanhood estimates
#'
#' @param estimates Estimates for a single group (rows of
#'   `run_orphanhood()$estimates`).
#' @param params Parameters of the run.
#' @param input_ages Age groups of respondents in the input data for the group.
#' @param input_props List of the proportions not orphaned in the input data
#'   for the group (one vector per observation), ordered by age.
#' @return Character vector of plain-language notes (English; translated by
#'   the caller).
#' @noRd
orphanhood_diagnostics <- function(estimates, params, input_ages = NULL, input_props = NULL) {
  if (is.null(estimates) || nrow(estimates) == 0) {
    return(character(0))
  }
  notes <- c(
    "Estimates from respondents aged 5-9 and 10-14 often understate adult mortality, because orphans are frequently adopted and reported as having their parent alive (adoption effect)."
  )
  if (!isTRUE(params$maternal)) {
    notes <- c(notes, "Paternal orphanhood estimates are less reliable than maternal ones, because fathers' ages at the birth of their children vary more.")
  }
  if (identical(as.integer(params$observations), 1L)) {
    notes <- c(notes, "Estimates from older respondents refer to more distant dates; the chart places each estimate at the date it refers to.")
  } else {
    notes <- c(notes, "Two-census estimates describe mortality between the two observations; they are placed at the midpoint of that period.")
  }
  if (!is.null(input_ages)) {
    unused <- setdiff(stats::na.omit(orphanhood_num(input_ages)), estimates$Age)
    if (length(unused) > 0) {
      notes <- c(notes, "Some age groups in your data give no estimate: the method only covers certain ages of respondents (for one census, 5-9 to 45-49 for maternal and 5-9 to 35-39 for paternal orphanhood).")
    }
  }
  if (!is.null(input_props)) {
    rises <- vapply(input_props, function(p) {
      p <- orphanhood_num(p)
      p <- p[!is.na(p)]
      any(diff(p) > 0)
    }, logical(1))
    if (any(rises)) {
      notes <- c(notes, "The proportions not orphaned rise between some consecutive age groups; check your data for those ages.")
    }
  }
  if (any(!estimates$plausible)) {
    notes <- c(notes, "Some estimates fall outside the range 0 to 1 and are left out of the chart; check the proportions not orphaned for those ages.")
  }
  if (isTRUE(params$hiv)) {
    notes <- c(notes, "Proportions not orphaned were adjusted for HIV/AIDS (Timaeus and Nunn 1997) using the HIVPrev column.")
  }
  notes
}

#' Plot orphanhood estimates
#'
#' One census or survey: 45q15 against the date each estimate refers to.
#' Two censuses or surveys: 45q15 by age group of respondents (all estimates
#' refer to the same period).
#'
#' @param estimates Estimates for a single group.
#' @param observations `1` or `2`.
#' @param title Plot title.
#' @param x_label,y_label Axis labels.
#' @return A ggplot object.
#' @noRd
orphanhood_plot <- function(estimates,
                            observations = 1,
                            title = "Adult mortality estimates",
                            x_label = "Reference date",
                            y_label = "Probability of dying between 15 and 60, 45q15") {
  estimates <- estimates[estimates$plausible, , drop = FALSE]
  estimates$hover <- sprintf(
    "Respondents %s<br>45q15 = %.4f<br>35q15 = %.4f<br>Date = %.1f",
    estimates$age_label, estimates$q15_45, estimates$q15_35, estimates$ref_date
  )
  q_range <- diff(range(estimates$q15_45, na.rm = TRUE))
  label_nudge <- if (is.finite(q_range) && q_range > 0) 0.06 * q_range else 0.005
  if (identical(as.integer(observations), 2L)) {
    estimates$x <- factor(estimates$age_label, levels = estimates$age_label[order(estimates$Age)])
    p <- ggplot2::ggplot(
      estimates,
      ggplot2::aes(x = .data$x, y = .data$q15_45, text = .data$hover)
    )
  } else {
    p <- ggplot2::ggplot(
      estimates,
      ggplot2::aes(x = .data$ref_date, y = .data$q15_45, text = .data$hover)
    ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$age_label),
        nudge_y = label_nudge, size = 3.5, color = "#1b1c1d"
      )
  }
  p +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "#1b6ec2", linewidth = 0.8) +
    ggplot2::geom_point(color = "#1b6ec2", size = 3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = 13)
}
