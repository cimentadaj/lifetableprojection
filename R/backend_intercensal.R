#' Intercensal Survival Backend
#'
#' Server-side helpers for the intercensal survival module. They wrap
#' `MortIndEst::intercensal_survival()`, which estimates adult mortality from
#' the survival of cohorts between two censuses (UN Manual X, 1983, chapter
#' VIII): the population of each five-year age group in the first census is
#' compared with the population ten (or n) years older in the second. Each
#' method finds the level of a model life table family implied at each age; the
#' median of those levels gives a complete life table, its life expectancy at
#' birth and the adult mortality indicators 45q15 and 35q15.
#'
#' @name intercensal_backend
#' @noRd
NULL

#' Estimation methods (values passed to `intercensal_survival()`)
#' @noRd
INTERCENSAL_METHODS <- c(
  match = "Survival ratios",
  bproj = "Back projection",
  fproj = "Forward projection",
  `var-r` = "Variable-r (Preston and Bennett)",
  feeney = "Feeney",
  logit = "Preston logit"
)

#' Methods that start from a reference model life table level
#' @noRd
INTERCENSAL_REFERENCE_METHODS <- c("feeney", "logit")

#' Model life table families of `MortCast::MLTlookup`
#'
#' The same families and labels as the orphanhood module (kept here because
#' files are collated alphabetically, so the orphanhood constants are not yet
#' defined when this file is loaded).
#' @noRd
INTERCENSAL_FAMILIES <- c(
  "CD_West", "CD_North", "CD_South", "CD_East",
  "UN_General", "UN_Latin_American", "UN_Chilean", "UN_South_Asian", "UN_Far_Eastern"
)

#' Display names of the model life table families
#' @noRd
INTERCENSAL_FAMILY_LABELS <- c(
  CD_West = "Coale-Demeny West", CD_North = "Coale-Demeny North",
  CD_South = "Coale-Demeny South", CD_East = "Coale-Demeny East",
  UN_General = "UN General", UN_Latin_American = "UN Latin American",
  UN_Chilean = "UN Chilean", UN_South_Asian = "UN South Asian",
  UN_Far_Eastern = "UN Far East Asian",
  HIVSpectrum = "Spectrum HIV model life tables"
)

#' Sexes of the population and model life tables
#' @noRd
INTERCENSAL_SEX_LABELS <- c(female = "Female", male = "Male")

#' Default ages whose implied levels are averaged, and accepted e0 range
#' @noRd
INTERCENSAL_DEFAULT_AGES_FIT <- seq(10, 70, 5)
INTERCENSAL_DEFAULT_E0_ACCEPT <- c(20, 100)

#' Packages `intercensal_survival()` needs on the search path: ggplot2 for the
#' Preston logit plot; for the Spectrum HIV life tables, rms (the fitted
#' models), DemoTools and MortIndEst itself (its `mods.r` data)
#' @noRd
INTERCENSAL_PACKAGES <- c("ggplot2")
INTERCENSAL_HIV_PACKAGES <- c("ggplot2", "rms", "DemoTools", "MortIndEst")

#' Messages with values, as templates for translation
#' @noRd
INTERCENSAL_MESSAGE_TEMPLATES <- c(
  "Missing required columns: %s. Provide Age, Pop1 and Pop2 (population of each census by five-year age group)",
  "Columns must be numeric: %s",
  "Missing values in columns: %s",
  "%s must take a single value within each group",
  "Survival ratios above 1 at ages %s: the second census counts more people than survived from the first (coverage errors or migration). The survival-ratio methods do not use these ages.",
  "The censuses are %s years apart. The first census was moved to %s, using the overall growth rate, so that the interval is a multiple of five years.",
  "The implied levels vary a lot across ages (coefficient of variation %s). Age misreporting or changes in coverage between the censuses are likely: review the chart and the ages used for the fit.",
  "%s implied level(s) were outside the accepted range of life expectancy at birth and were set to its limits."
)

#' Translate a diagnostic note
#'
#' Notes about a family that could not be estimated start with the family
#' name ("UN General: ..."); the name and the message are translated apart.
#'
#' @param note English note.
#' @param i18n Translator object.
#' @return Translated note.
#' @noRd
intercensal_translate_note <- function(note, i18n) {
  for (label in unname(INTERCENSAL_FAMILY_LABELS)) {
    prefix <- paste0(label, ": ")
    if (startsWith(note, prefix)) {
      rest <- substring(note, nchar(prefix) + 1)
      return(paste0(
        suppressWarnings(i18n$t(label)), ": ",
        translate_messages(rest, i18n, INTERCENSAL_MESSAGE_TEMPLATES)
      ))
    }
  }
  translate_messages(note, i18n, INTERCENSAL_MESSAGE_TEMPLATES)
}

#' Numeric values of a column, `NA` where the value is not a number
#' @noRd
intercensal_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

#' Standardise intercensal survival input columns
#'
#' Matches column names ignoring case, `_`, `.` and spaces. `Pop1` / `Pop2`
#' also accept `C1`, `Census1`, `Population1` (and the same for 2).
#'
#' @param data Data frame of census populations.
#' @return Data frame with standard column names (`Age`, `Pop1`, `Pop2`,
#'   `Date1`, `Date2`, `Sex`), keeping all other columns.
#' @noRd
prepare_intercensal_input <- function(data) {
  data <- as.data.frame(data)
  key <- gsub("[_. ]", "", tolower(names(data)))
  aliases <- list(
    Age = "age",
    Pop1 = c("pop1", "c1", "census1", "population1"),
    Pop2 = c("pop2", "c2", "census2", "population2"),
    Date1 = "date1",
    Date2 = "date2",
    Sex = "sex"
  )
  for (std in names(aliases)) {
    hit <- which(key %in% aliases[[std]])
    if (length(hit) > 0 && !std %in% names(data)) {
      names(data)[hit[1]] <- std
    }
  }
  data
}

#' Decimal year of dates given as decimal years or as text
#'
#' @param x Values: decimal years (e.g. 1960.94) or dates written
#'   `YYYY-MM-DD` or `YYYY/MM/DD`.
#' @return Decimal years (DemoTools' day-of-year convention), `NA` where a
#'   value cannot be read.
#' @noRd
intercensal_parse_date <- function(x) {
  x <- trimws(as.character(x))
  out <- intercensal_num(x)
  text <- is.na(out) & !is.na(x) & nzchar(x)
  out[text] <- vapply(x[text], function(v) {
    if (!grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$", v)) return(NA_real_)
    d <- tryCatch(as.Date(gsub("/", "-", v), format = "%Y-%m-%d"), error = function(e) NA)
    if (is.na(d)) NA_real_ else DemoTools::dec.date(d)
  }, numeric(1))
  out
}

#' Decimal year of a census date
#'
#' @param year,month,day Census date.
#' @return Decimal year (DemoTools' day-of-year convention).
#' @noRd
intercensal_decimal_date <- function(year, month = 1, day = 1) {
  parts <- suppressWarnings(as.integer(c(year, month, day)))
  if (length(parts) != 3 || anyNA(parts)) {
    stop("The census dates are required")
  }
  d <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", parts[1], parts[2], parts[3])), error = function(e) NA)
  if (is.na(d)) {
    stop("The census dates are not valid dates")
  }
  DemoTools::dec.date(d)
}

#' Sex of a group, as `"female"` or `"male"`
#'
#' @param x Values of a `Sex` column (`female`, `male`, `f`, `m`, any case).
#' @return `"female"`, `"male"` or `NA` for each value.
#' @noRd
intercensal_sex_value <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(v %in% c("f", "female", "females", "women"), "female",
         ifelse(v %in% c("m", "male", "males", "men"), "male", NA_character_))
}

#' Build one validation row in the app's standard format
#' @noRd
intercensal_check_row <- function(check, failed, message) {
  data.frame(
    check = check,
    message = if (isTRUE(failed)) message else NA_character_,
    pass = if (isTRUE(failed)) "Fail" else "Pass",
    stringsAsFactors = FALSE
  )
}

#' Validate one group of intercensal survival data
#' @noRd
check_intercensal_group <- function(df) {
  required <- c("Age", "Pop1", "Pop2")
  missing_cols <- setdiff(required, names(df))
  rows <- list(intercensal_check_row(
    "check required columns",
    length(missing_cols) > 0,
    paste0(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      ". Provide Age, Pop1 and Pop2 (population of each census by five-year age group)"
    )
  ))
  if (length(missing_cols) > 0) {
    return(do.call(rbind, rows))
  }

  vals <- lapply(df[required], intercensal_num)
  non_numeric <- vapply(required, function(col) {
    any(is.na(vals[[col]]) & !is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
  }, logical(1))
  rows[[length(rows) + 1]] <- intercensal_check_row(
    "check numeric columns",
    any(non_numeric),
    paste0("Columns must be numeric: ", paste(required[non_numeric], collapse = ", "))
  )

  has_na <- vapply(vals, function(x) any(is.na(x)), logical(1))
  rows[[length(rows) + 1]] <- intercensal_check_row(
    "check missing values",
    any(has_na),
    paste0("Missing values in columns: ", paste(required[has_na], collapse = ", "))
  )

  # Five-year groups from 0, each once (a file with both sexes that is not
  # grouped by Sex would repeat every age), the last one open
  age <- vals$Age
  ages_ok <- !anyNA(age) && !anyDuplicated(age) && length(age) >= 11 &&
    isTRUE(all(sort(age) == seq(0, by = 5, length.out = length(age))))
  rows[[length(rows) + 1]] <- intercensal_check_row(
    "check age groups",
    !ages_ok,
    "Age must hold five-year groups starting at 0 (0, 5, 10, ...), each age once, with the last (open) group starting at 50 or above. With both sexes in one file, group the data by Sex."
  )

  pops <- c(vals$Pop1, vals$Pop2)
  rows[[length(rows) + 1]] <- intercensal_check_row(
    "check populations",
    any(!is.na(pops) & pops <= 0),
    "Pop1 and Pop2 must be greater than zero in every age group"
  )

  # Optional dates: both columns, or neither (then set in the analysis step)
  has_dates <- c("Date1", "Date2") %in% names(df)
  if (any(has_dates)) {
    rows[[length(rows) + 1]] <- intercensal_check_row(
      "check Date1 and Date2",
      !all(has_dates),
      "Provide both Date1 and Date2, or neither and set the census dates in the analysis step"
    )
  }
  if (all(has_dates)) {
    d1 <- unique(stats::na.omit(intercensal_parse_date(df$Date1)))
    d2 <- unique(stats::na.omit(intercensal_parse_date(df$Date2)))
    readable <- length(d1) == 1 && length(d2) == 1 &&
      all(is.na(intercensal_parse_date(df$Date1)) == is.na(df$Date1)) &&
      all(is.na(intercensal_parse_date(df$Date2)) == is.na(df$Date2)) &&
      d1 >= 1800 && d2 <= 2200
    rows[[length(rows) + 1]] <- intercensal_check_row(
      "check census dates",
      !readable,
      "Date1 and Date2 must each hold one date per group: a decimal year (for example 1960.94) or a date such as 1960-12-11"
    )
    if (readable) {
      rows[[length(rows) + 1]] <- intercensal_check_row(
        "check intercensal interval",
        !intercensal_interval_ok(d1, d2),
        "The second census must be between 2.5 and 30 years after the first"
      )
    }
  }

  if ("Sex" %in% names(df)) {
    sx <- unique(intercensal_sex_value(df$Sex))
    rows[[length(rows) + 1]] <- intercensal_check_row(
      "check Sex",
      length(sx) != 1 || is.na(sx),
      "Sex must be female or male (or f / m), with a single value in each group. With both sexes in one file, group the data by Sex."
    )
  }

  do.call(rbind, rows)
}

#' Is the interval between two census dates usable?
#'
#' `intercensal_survival()` moves the first census so that the interval is a
#' whole number of five-year age groups (at least one) and refuses intervals
#' above 30 years.
#' @noRd
intercensal_interval_ok <- function(date1, date2) {
  t <- date2 - date1
  isTRUE(t >= 2.5 && t <= 30)
}

#' Validate intercensal survival input data
#'
#' Runs all checks per group (`.id`) and returns the standard validation table
#' used by the shared data context.
#'
#' @param data Data frame of census populations, optionally with `.id`.
#' @return Data frame with columns `check`, `message` and `pass`.
#' @noRd
check_data_intercensal <- function(data) {
  data <- prepare_intercensal_input(data)
  if (!".id" %in% names(data)) {
    data$.id <- "all"
  }
  split_data <- split(data, data$.id)
  do.call(rbind, lapply(split_data, check_intercensal_group))
}

#' Validate Intercensal Survival Data
#'
#' Wrapper used by the shared data context of the intercensal survival module.
#'
#' @param data Data frame to be validated.
#' @param i18n Translator object for internationalization.
#' @return Data frame containing validation results.
#' @noRd
validateData_intercensal <- function(data, i18n) {
  check_data_intercensal(data)
}

#' Census dates of one group
#'
#' `Date1` and `Date2` columns override the dates set in the controls. Dates
#' are rounded to two decimals, as `intercensal_survival()` does with dates
#' given as text.
#'
#' @param df Data for one group.
#' @param date1,date2 Decimal years from the controls (or `NULL`).
#' @return Numeric vector of length 2.
#' @noRd
intercensal_group_dates <- function(df, date1, date2) {
  single <- function(col) {
    v <- unique(stats::na.omit(intercensal_parse_date(df[[col]])))
    if (length(v) > 1) stop(sprintf("%s must take a single value within each group", col))
    v
  }
  if (all(c("Date1", "Date2") %in% names(df))) {
    d1 <- single("Date1")
    d2 <- single("Date2")
    if (length(d1) == 1 && length(d2) == 1) {
      date1 <- d1
      date2 <- d2
    }
  }
  dates <- suppressWarnings(as.numeric(c(date1 %||% NA, date2 %||% NA)))
  if (length(dates) != 2 || anyNA(dates)) {
    stop("The census dates are required")
  }
  if (!intercensal_interval_ok(dates[1], dates[2])) {
    stop("The second census must be between 2.5 and 30 years after the first")
  }
  round(dates, 2)
}

#' Sex of one group: the `Sex` column when there is one, else the control
#' @noRd
intercensal_group_sex <- function(df, sex) {
  if ("Sex" %in% names(df)) {
    sx <- unique(intercensal_sex_value(df$Sex))
    if (length(sx) != 1 || is.na(sx)) {
      stop("Sex must be female or male (or f / m), with a single value in each group. With both sexes in one file, group the data by Sex.")
    }
    return(sx)
  }
  if (!sex %in% names(INTERCENSAL_SEX_LABELS)) {
    stop("sex should be 'female' or 'male'")
  }
  sex
}

#' Plain-language version of an error from `intercensal_survival()`
#'
#' @param msg Error message.
#' @param family Model life table family the call used.
#' @return Message to show (English; translated by the module).
#' @noRd
intercensal_error_message <- function(msg, family) {
  if (grepl("out of range of e0_accept", msg, fixed = TRUE)) {
    return("The estimated level falls outside the accepted range of life expectancy at birth. Check the data or widen the range.")
  }
  if (grepl("out of range for family", msg, fixed = TRUE)) {
    if (identical(family, "HIVSpectrum")) {
      return("The estimated level is outside the range of the Spectrum HIV model life tables built from the HIV prevalence, ART coverage and 5q0 given. Check those values.")
    }
    return("The estimated level is outside the range of this model life table family. Try another family or method.")
  }
  if (grepl("All survival ratios greater than 1", msg, fixed = TRUE)) {
    return("All survival ratios are above 1 at the ages used for the fit: the second census counts more people than survived from the first. Check the data (coverage, migration) or the ages used.")
  }
  if (grepl("missing value where TRUE/FALSE needed", msg, fixed = TRUE) ||
      grepl("must be of size", msg, fixed = TRUE)) {
    return("None of the ages used for the fit gave a usable level. Choose other ages or another method.")
  }
  msg
}

#' Call `intercensal_survival()` for one family, keeping its messages
#'
#' @return List with `res` (the result) and `messages` (character vector of
#'   the messages it printed).
#' @noRd
intercensal_call <- function(args, hiv) {
  messages <- character(0)
  res <- with_mortindest_deps(
    withCallingHandlers(
      suppressWarnings(do.call(MortIndEst::intercensal_survival, args)),
      message = function(m) {
        messages <<- c(messages, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    extra = if (hiv) INTERCENSAL_HIV_PACKAGES else INTERCENSAL_PACKAGES
  )
  list(res = res, messages = messages)
}

#' Diagnostic notes from the messages of `intercensal_survival()`
#' @noRd
intercensal_message_notes <- function(messages) {
  notes <- character(0)
  ratios <- grep("^survival ratios greater than 1", messages, value = TRUE)
  if (length(ratios) > 0) {
    ages <- sub(".*starting on ", "", ratios[1])
    notes <- c(notes, sprintf(
      "Survival ratios above 1 at ages %s: the second census counts more people than survived from the first (coverage errors or migration). The survival-ratio methods do not use these ages.",
      ages
    ))
  }
  if (any(grepl("r-variable variant was applied", messages, fixed = TRUE))) {
    notes <- c(notes, "The censuses are not 5 or 10 years apart, so Feeney's method used its variable-r variant.")
  }
  if (any(grepl("value of e5 out of range", messages, fixed = TRUE))) {
    notes <- c(notes, "The Preston logit estimate of life expectancy at age 5 was outside the model life tables and was replaced by a limit: review the ages used for the fit.")
  }
  notes
}

#' Estimate adult mortality from two censuses for a single group
#'
#' @param df Data frame for one group (already standardised).
#' @param method One of `names(INTERCENSAL_METHODS)`.
#' @param family Model life table family, or `"all"` for every family.
#' @param sex `"female"` or `"male"`, used when the data have no `Sex` column.
#' @param date1,date2 Census dates (decimal years), used when the data have no
#'   `Date1` / `Date2` columns.
#' @param ages_fit Ages whose implied levels are averaged.
#' @param e0_accept Accepted range of the implied life expectancy at birth.
#' @param ref_e0 Reference life expectancy at birth for the Feeney and Preston
#'   logit methods; `NULL` uses, for each family, the level found by the
#'   survival-ratio (match) method.
#' @param q0,q5 Optional infant (1q0) and under-five (5q0) mortality: both are
#'   needed; they replace the first ages of the fitted life table and give the
#'   5q0 of the Spectrum HIV life tables and of the Preston logit method.
#' @param hiv `NULL`, or a list with `prev` and `art` (proportions between 0
#'   and 1) for the Spectrum HIV model life tables.
#' @return List with `summary` (one row per family), `by_age` (survival
#'   ratios and implied levels by age), `lt` (fitted life tables) and `notes`.
#' @noRd
intercensal_estimate_group <- function(df, method, family, sex, date1, date2,
                                       ages_fit = INTERCENSAL_DEFAULT_AGES_FIT,
                                       e0_accept = INTERCENSAL_DEFAULT_E0_ACCEPT,
                                       ref_e0 = NULL, q0 = NULL, q5 = NULL,
                                       hiv = NULL) {
  df <- df[order(intercensal_num(df$Age)), , drop = FALSE]
  dates <- intercensal_group_dates(df, date1, date2)
  sex <- intercensal_group_sex(df, sex)
  age <- intercensal_num(df$Age)
  c1 <- intercensal_num(df$Pop1)
  c2 <- intercensal_num(df$Pop2)
  ages_fit <- sort(unique(intercensal_num(ages_fit)))
  ages_fit <- ages_fit[ages_fit %in% age]
  if (length(ages_fit) == 0) {
    stop("Choose at least one age group for the fit")
  }
  q01_q05 <- if (!is.null(q0) && !is.null(q5)) c(q0, q5) else NULL
  use_hiv <- !is.null(hiv)
  families <- if (use_hiv) "HIVSpectrum" else if (identical(family, "all")) INTERCENSAL_FAMILIES else family

  base_args <- list(
    c1 = c1, c2 = c2, date1 = dates[1], date2 = dates[2],
    age1 = age, age2 = age,
    sex = if (identical(sex, "female")) "f" else "m",
    ages_fit = ages_fit, e0_accept = e0_accept,
    q01_q05 = q01_q05, verbose = TRUE
  )
  if (use_hiv) {
    # The Spectrum (SVD-comp) models were fitted on percentages
    base_args$HIV_prev <- hiv$prev * 100
    base_args$HIV_art <- hiv$art * 100
  }

  run_family <- function(fam) {
    args <- base_args
    # With HIV the family argument is ignored and replaced by the Spectrum tables
    args$mlt_family <- if (identical(fam, "HIVSpectrum")) "CD_West" else fam
    reference <- NA_real_
    if (method %in% INTERCENSAL_REFERENCE_METHODS) {
      reference <- ref_e0 %||% {
        m <- intercensal_call(modifyList(args, list(method = "match")), use_hiv)$res
        m$selected_level$median_e0[1]
      }
      # MortIndEst rounds the model life tables' level to 2.5 years; rounding
      # here shows the level that was actually used (the Spectrum tables use
      # the nearest of their own levels instead)
      if (!use_hiv) reference <- round(reference / 2.5) * 2.5
      args$mlt_e0_logit_feeney <- reference
    }
    args$method <- method
    out <- intercensal_call(args, use_hiv)
    out$reference <- reference
    out
  }

  results <- lapply(families, function(fam) {
    tryCatch(run_family(fam), error = function(e) {
      list(error = intercensal_error_message(conditionMessage(e), fam))
    })
  })
  names(results) <- families
  failed <- vapply(results, function(r) !is.null(r$error), logical(1))
  if (all(failed)) {
    stop(results[[1]]$error)
  }

  summary_rows <- list()
  by_age_rows <- list()
  lt_rows <- list()
  notes <- character(0)
  for (fam in families[!failed]) {
    r <- results[[fam]]
    res <- r$res
    sel <- as.data.frame(res$selected_level)
    lt <- as.data.frame(res$lt_fit)
    lt$type <- NULL
    if (any(!is.na(lt$nqx) & (lt$nqx < 0 | lt$nqx > 1))) {
      stop("The 1q0 and 5q0 given do not fit the estimated life table (they give probabilities of dying outside 0-1). Check them or leave them blank.")
    }
    adult <- as.data.frame(res$Adult)
    surv <- as.data.frame(res$surv_data)
    rank <- as.data.frame(res$rank_match)
    implied <- rank$e0_interp[match(surv$age, rank$age)]
    used <- surv$age %in% ages_fit & !is.na(implied)
    open_age <- max(surv$age)
    by_age <- data.frame(
      family = fam,
      Age = surv$age,
      age_label = ifelse(surv$age == open_age, paste0(surv$age, "+"), paste0(surv$age, "-", surv$age + 4)),
      Pop1 = surv$c1_noadj,
      Pop1_moved = surv$c1,
      Pop2 = surv$c2,
      survival_ratio = surv$nSx,
      implied_e0 = implied,
      used = used,
      stringsAsFactors = FALSE
    )
    clamped <- sum(used & (implied < e0_accept[1] | implied > e0_accept[2]))
    summary_rows[[fam]] <- data.frame(
      family = fam,
      method = method,
      sex = sex,
      date1 = dates[1],
      date2 = dates[2],
      interval = round(dates[2] - dates[1], 2),
      ages_used = sum(used),
      median_e0 = sel$median_e0[1],
      mean_e0 = sel$mean_e0[1],
      p25_e0 = sel$p25[1],
      p75_e0 = sel$p75[1],
      coef_var = sel$coef_var[1],
      reference_e0 = r$reference,
      e0 = lt$ex[lt$Age == 0][1],
      q15_45 = adult$q15_45[1],
      q15_35 = adult$q15_35[1],
      clamped = clamped,
      stringsAsFactors = FALSE
    )
    by_age_rows[[fam]] <- by_age
    lt_rows[[fam]] <- cbind(family = fam, lt, stringsAsFactors = FALSE)
    fam_notes <- intercensal_message_notes(r$messages)
    if (clamped > 0) {
      fam_notes <- c(fam_notes, sprintf(
        "%s implied level(s) were outside the accepted range of life expectancy at birth and were set to its limits.",
        clamped
      ))
    }
    notes <- c(notes, fam_notes)
  }
  for (fam in families[failed]) {
    notes <- c(notes, sprintf("%s: %s", INTERCENSAL_FAMILY_LABELS[[fam]], results[[fam]]$error))
  }

  # The first census is moved so that the interval is a multiple of five years
  interval <- dates[2] - dates[1]
  moved <- dates[1] + (interval / 5 - round(interval / 5)) * 5
  if (abs(moved - dates[1]) > 0.005) {
    notes <- c(notes, sprintf(
      "The censuses are %s years apart. The first census was moved to %s, using the overall growth rate, so that the interval is a multiple of five years.",
      format(round(interval, 2), nsmall = 2), format(round(moved, 2), nsmall = 2)
    ))
  }

  summary <- do.call(rbind, summary_rows)
  cv <- summary$coef_var[is.finite(summary$coef_var)]
  cv <- if (length(cv) > 0) max(cv) else NA
  if (!is.na(cv) && cv > 0.1) {
    notes <- c(notes, sprintf(
      "The implied levels vary a lot across ages (coefficient of variation %s). Age misreporting or changes in coverage between the censuses are likely: review the chart and the ages used for the fit.",
      format(round(cv, 2), nsmall = 2)
    ))
  }
  rownames(summary) <- NULL
  by_age <- do.call(rbind, by_age_rows)
  rownames(by_age) <- NULL
  lt <- do.call(rbind, lt_rows)
  rownames(lt) <- NULL

  list(
    summary = summary,
    by_age = by_age,
    lt = lt,
    notes = data.frame(note = unique(notes), stringsAsFactors = FALSE)
  )
}

#' Estimate adult mortality from two censuses for all groups
#'
#' Splits the data by `.id`, applies the method to each group and keeps going
#' when a single group fails.
#'
#' @param data Data frame with `Age`, `Pop1` and `Pop2`, optional `Date1`,
#'   `Date2` and `Sex`, and optionally `.id` / `.id_label`.
#' @param method One of `names(INTERCENSAL_METHODS)` (default `"match"`).
#' @param family Model life table family or `"all"` (default `"CD_West"`).
#' @param sex `"female"` (default) or `"male"`, for groups without `Sex`.
#' @param date1,date2 Census dates (decimal years) for groups without date
#'   columns.
#' @param ages_fit,e0_accept,ref_e0,q0,q5,hiv See
#'   [intercensal_estimate_group()].
#' @return List with `summary`, `by_age`, `lt` and `notes` (all groups, with
#'   `.id`), `group_errors` and `params`.
#' @noRd
run_intercensal <- function(data,
                            method = "match",
                            family = "CD_West",
                            sex = "female",
                            date1 = NULL,
                            date2 = NULL,
                            ages_fit = INTERCENSAL_DEFAULT_AGES_FIT,
                            e0_accept = INTERCENSAL_DEFAULT_E0_ACCEPT,
                            ref_e0 = NULL,
                            q0 = NULL,
                            q5 = NULL,
                            hiv = NULL) {
  if (!method %in% names(INTERCENSAL_METHODS)) {
    stop(sprintf("method should be one of: %s", paste(names(INTERCENSAL_METHODS), collapse = ", ")))
  }
  if (!family %in% c(INTERCENSAL_FAMILIES, "all")) {
    stop(sprintf("Model family should be one of: %s", paste(c(INTERCENSAL_FAMILIES, "all"), collapse = ", ")))
  }
  e0_accept <- as.numeric(e0_accept)
  if (length(e0_accept) != 2 || anyNA(e0_accept) || e0_accept[1] >= e0_accept[2]) {
    stop("The accepted range of life expectancy must have a lower limit below the upper limit")
  }
  if (xor(is.null(q0), is.null(q5))) {
    stop("Give both 1q0 and 5q0, or neither")
  }
  if (!is.null(q5) && !(q0 > 0 && q5 > q0 && q5 < 1)) {
    stop("1q0 and 5q0 must be probabilities with 0 < 1q0 < 5q0 < 1")
  }
  if (!is.null(hiv) && is.null(q5)) {
    stop("The Spectrum HIV life tables need 1q0 and 5q0")
  }
  # MortIndEst's logit method only knows the model life table families
  if (!is.null(hiv) && identical(method, "logit")) {
    stop("The Preston logit method cannot be used with the Spectrum HIV life tables. Choose another method.")
  }
  if (!is.null(ref_e0) && (!is.finite(ref_e0) || ref_e0 < 20 || ref_e0 > 100)) {
    stop("The reference e0 must be between 20 and 100")
  }

  data <- prepare_intercensal_input(data)
  sex_from_data <- "Sex" %in% names(data)
  by_group <- run_by_group(
    data,
    intercensal_estimate_group,
    method = method, family = family, sex = sex,
    date1 = date1, date2 = date2, ages_fit = ages_fit, e0_accept = e0_accept,
    ref_e0 = ref_e0, q0 = q0, q5 = q5, hiv = hiv,
    check = check_intercensal_group
  )

  list(
    summary = by_group[["summary"]],
    by_age = by_group[["by_age"]],
    lt = by_group[["lt"]],
    notes = by_group[["notes"]],
    group_errors = by_group[["group_errors"]],
    params = list(
      method = method,
      family = family,
      # NULL when each group takes its sex from the Sex column
      sex = if (sex_from_data) NULL else sex,
      date1 = date1,
      date2 = date2,
      ages_fit = sort(unique(as.numeric(ages_fit))),
      e0_accept = e0_accept,
      ref_e0 = ref_e0,
      q0 = q0,
      q5 = q5,
      hiv = hiv
    )
  )
}

#' Readable form of a decimal-year date (`YYYY-MM-DD`)
#' @noRd
intercensal_format_date <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    return(NA_character_)
  }
  year <- floor(x)
  start <- as.Date(sprintf("%04d-01-01", year))
  days <- as.numeric(as.Date(sprintf("%04d-01-01", year + 1)) - start)
  format(start + round((x - year) * days))
}

#' Compact label of a set of ages (e.g. "10-70" or "0, 5, 15-30")
#' @noRd
intercensal_ages_label <- function(ages) {
  ages <- sort(unique(as.numeric(ages)))
  if (length(ages) == 0) return("")
  runs <- split(ages, cumsum(c(1, diff(ages) != 5)))
  paste(vapply(runs, function(r) {
    if (length(r) == 1) format(r) else paste0(r[1], "-", r[length(r)])
  }, character(1)), collapse = ", ")
}

#' Settings behind an intercensal survival run, for display
#'
#' @param params Parameters of a run (`run_intercensal()$params`).
#' @return Data frame with `label` and `value` (English; the module translates
#'   the labels and the values in `translate_value`).
#' @noRd
intercensal_settings_items <- function(params) {
  item <- function(label, value, translate = FALSE) {
    data.frame(label = label, value = value, translate_value = translate, stringsAsFactors = FALSE)
  }
  hiv <- !is.null(params$hiv)
  family_value <- if (hiv) {
    INTERCENSAL_FAMILY_LABELS[["HIVSpectrum"]]
  } else if (identical(params$family, "all")) {
    "All families"
  } else {
    INTERCENSAL_FAMILY_LABELS[[params$family]] %||% params$family
  }
  dates_value <- if (is.null(params$date1) || is.null(params$date2)) {
    "Date1 and Date2 columns of your data"
  } else {
    paste(intercensal_format_date(params$date1), "-", intercensal_format_date(params$date2))
  }
  items <- rbind(
    item("Method", INTERCENSAL_METHODS[[params$method]], TRUE),
    item("Model life table family", family_value, TRUE),
    item("Sex", if (is.null(params$sex)) "Sex column of your data" else INTERCENSAL_SEX_LABELS[[params$sex]] %||% params$sex, TRUE),
    item("Census dates", dates_value, is.null(params$date1) || is.null(params$date2)),
    item("Ages used for the fit", intercensal_ages_label(params$ages_fit)),
    item("Accepted e0 range", paste0(params$e0_accept[1], "-", params$e0_accept[2]))
  )
  if (params$method %in% INTERCENSAL_REFERENCE_METHODS) {
    items <- rbind(items, if (is.null(params$ref_e0)) {
      item("Reference e0", "From the survival-ratio method", TRUE)
    } else {
      item("Reference e0", format(params$ref_e0))
    })
  }
  if (!is.null(params$q5)) {
    items <- rbind(items, item("1q0 and 5q0", paste(format(params$q0), "/", format(params$q5))))
  }
  if (hiv) {
    items <- rbind(items, item("HIV prevalence and ART coverage", paste(format(params$hiv$prev), "/", format(params$hiv$art))))
  }
  items
}

#' Have the controls moved away from the settings of the last run?
#'
#' A `NULL` control value (a control that is being re-rendered) counts as
#' unchanged; dates only count when they came from the controls.
#'
#' @param params Parameters of the last run (`run_intercensal()$params`).
#' @param current Named list of the current control values, in the form of
#'   `params`.
#' @return `TRUE` when the shown results no longer match the controls.
#' @noRd
intercensal_settings_changed <- function(params, current) {
  differs <- function(old, new) {
    !is.null(new) && !identical(as.character(old), as.character(new))
  }
  differs_num <- function(old, new) {
    if (is.null(new)) return(FALSE)
    if (is.null(old)) return(length(new) > 0)
    length(old) != length(new) || !isTRUE(all.equal(as.numeric(old), as.numeric(new)))
  }
  differs(params$method, current$method) ||
    (is.null(params$hiv) && differs(params$family, current$family)) ||
    differs(params$sex, current$sex) ||
    (!is.null(params$date1) && (differs_num(params$date1, current$date1) || differs_num(params$date2, current$date2))) ||
    differs_num(params$ages_fit, current$ages_fit) ||
    differs_num(params$e0_accept, current$e0_accept) ||
    (params$method %in% INTERCENSAL_REFERENCE_METHODS && !is.null(current$method) &&
       !identical(is.null(params$ref_e0), is.null(current$ref_e0))) ||
    (params$method %in% INTERCENSAL_REFERENCE_METHODS && differs_num(params$ref_e0, current$ref_e0)) ||
    (!is.null(current$hiv_on) && !identical(!is.null(params$hiv), isTRUE(current$hiv_on))) ||
    (!is.null(current$q_given) && !identical(!is.null(params$q5), isTRUE(current$q_given))) ||
    (!is.null(params$q5) && (differs_num(params$q0, current$q0) || differs_num(params$q5, current$q5))) ||
    (!is.null(params$hiv) && (differs_num(params$hiv$prev, current$hiv_prev) || differs_num(params$hiv$art, current$hiv_art)))
}

#' Plot the levels implied at each age
#'
#' @param by_age Rows of `run_intercensal()$by_age` for one group.
#' @param summary Rows of `run_intercensal()$summary` for the same group.
#' @param title,x_label,y_label Plot texts.
#' @param hover_labels Words used in the hover text (`ratio`, `implied`,
#'   `used`, `not_used`), so the module can pass them translated.
#' @param legend_labels Legend texts (`used`, `not_used`, `selected`).
#' @param family_labels Display names of the families (named by family).
#' @return A ggplot object: implied e0 by age, filled where the age is used
#'   for the fit, with the selected level (median) as a dashed line.
#' @noRd
intercensal_plot <- function(by_age,
                             summary,
                             title = "Levels implied by each age group",
                             x_label = "Age",
                             y_label = "Implied life expectancy at birth",
                             hover_labels = list(ratio = "Survival ratio", implied = "Implied e0",
                                                 used = "Used for the fit", not_used = "Not used for the fit"),
                             legend_labels = list(used = "Used for the fit", not_used = "Not used for the fit",
                                                  selected = "Selected level (median)"),
                             family_labels = INTERCENSAL_FAMILY_LABELS) {
  pts <- by_age[!is.na(by_age$implied_e0), , drop = FALSE]
  pts$family_label <- unname(family_labels[pts$family])
  pts$status <- ifelse(pts$used, legend_labels$used, legend_labels$not_used)
  several <- length(unique(by_age$family)) > 1
  pts$hover <- sprintf(
    "%s%s<br>%s = %s<br>%s = %.1f<br>%s",
    if (several) paste0(pts$family_label, "<br>") else "",
    pts$age_label,
    hover_labels$ratio, ifelse(is.na(pts$survival_ratio), "-", sprintf("%.4f", pts$survival_ratio)),
    hover_labels$implied, pts$implied_e0,
    ifelse(pts$used, hover_labels$used, hover_labels$not_used)
  )
  lines <- summary
  lines$family_label <- unname(family_labels[lines$family])
  p <- ggplot2::ggplot(pts, ggplot2::aes(x = .data$Age, y = .data$implied_e0, text = .data$hover))
  if (several) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = .data$family_label, color = .data$family_label), linewidth = 0.6, alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(color = .data$family_label, shape = .data$status), size = 2.5) +
      ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = .data$median_e0, color = .data$family_label),
                          linetype = "dashed", inherit.aes = FALSE) +
      ggplot2::labs(color = NULL)
  } else {
    # The selected level goes in the legend (plotly ignores text alignment,
    # so a label next to the line would be clipped)
    lines$level_label <- sprintf("%s: %.1f", legend_labels$selected, lines$median_e0)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = 1), color = "#1b6ec2", linewidth = 0.6, alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$status), color = "#1b6ec2", size = 3) +
      ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = .data$median_e0, linetype = .data$level_label),
                          color = "#c0392b", inherit.aes = FALSE) +
      ggplot2::scale_linetype_manual(values = stats::setNames("dashed", lines$level_label[1]), name = NULL)
  }
  shapes <- stats::setNames(c(16, 1), c(legend_labels$used, legend_labels$not_used))
  p +
    ggplot2::scale_shape_manual(values = shapes, name = NULL) +
    ggplot2::scale_x_continuous(breaks = by_age$Age[seq(1, nrow(by_age), by = 2)]) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = 13)
}

#' Interactive version of [intercensal_plot()]
#'
#' `ggplotly()` names legend entries after every aesthetic ("(Used for the
#' fit,1)"); this keeps only the label.
#'
#' @param plot_obj ggplot object from [intercensal_plot()].
#' @return A plotly object.
#' @noRd
intercensal_plotly <- function(plot_obj) {
  pl <- plotly::ggplotly(plot_obj, tooltip = "text")
  seen <- character(0)
  for (i in seq_along(pl$x$data)) {
    for (field in c("name", "legendgroup")) {
      value <- pl$x$data[[i]][[field]]
      if (!is.null(value)) {
        pl$x$data[[i]][[field]] <- sub("^\\((.*?)(,[^,]*)?\\)$", "\\1", value)
      }
    }
    # With several families each family repeats the used / not used entries
    name <- pl$x$data[[i]]$name
    if (!is.null(name) && nzchar(name) && isTRUE(pl$x$data[[i]]$showlegend %||% TRUE)) {
      if (name %in% seen) pl$x$data[[i]]$showlegend <- FALSE
      seen <- c(seen, name)
    }
  }
  pl |> plotly::config(displayModeBar = FALSE)
}
