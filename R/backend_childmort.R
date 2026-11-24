#' Child Mortality (Brass) Backend
#'
#' Server-side helpers for the child mortality module. They wrap
#' `MortIndEst::childmort_brass()`, which estimates probabilities of dying in
#' childhood from children ever born (CEB) and children dead (CD) or surviving
#' (CS) reported by women in five-year age groups 15-19 to 45-49 (UN Manual X,
#' 1983), and translates each estimate, through the matching model life table,
#' into infant mortality (1q0), child mortality (4q1), under-five mortality
#' (5q0) and life expectancy at birth, as Mortpak QFIVE does.
#'
#' @name childmort_backend
#' @noRd
NULL

#' Women's age groups required by the Brass method
#' @noRd
CHILDMORT_AGES <- seq(15, 45, 5)

#' Model life table families available for each multiplier variant
#' @noRd
CHILDMORT_FAMILIES <- list(
  trussell = c("North", "South", "East", "West"),
  palloni_helligman = c("Latin", "Chilean", "South_Asian", "Far_East_Asian", "General")
)

#' Display names of the model life table families
#' @noRd
CHILDMORT_FAMILY_LABELS <- c(
  North = "North", South = "South", East = "East", West = "West",
  Latin = "Latin American", Chilean = "Chilean", South_Asian = "South Asian",
  Far_East_Asian = "Far East Asian", General = "General"
)

#' Sexes of the model life tables (both sexes use a sex ratio at birth of 1.04)
#' @noRd
CHILDMORT_SEXES <- c("both", "female", "male")

#' Display names of the sexes
#' @noRd
CHILDMORT_SEX_LABELS <- c(both = "Both sexes", female = "Female", male = "Male")

#' Childhood age (in years) estimated from each women's age group
#' @noRd
CHILDMORT_X <- c(1, 2, 3, 5, 10, 15, 20)

#' Indicators that can be charted, with their axis labels
#' @noRd
CHILDMORT_INDICATORS <- c(
  q0 = "Infant mortality, 1q0",
  q0_5 = "Under-five mortality, 5q0",
  q1_4 = "Child mortality, 4q1",
  qx = "Probability of dying, q(x)"
)

#' Packages `childmort_brass()` needs on the search path: ggplot2 for its
#' plot, and MortIndEst itself for its lazy-loaded `modelLTx1_both` data
#' @noRd
CHILDMORT_PACKAGES <- c("ggplot2", "MortIndEst")

#' Messages with values (column names), as templates for translation
#' @noRd
CHILDMORT_MESSAGE_TEMPLATES <- c(
  "Missing required columns: %s. Provide Age and two of CEB, CD and CS (counts with a Women column, or averages per woman without it), or CEB and PropDead",
  "Columns must be numeric: %s",
  "Missing values in columns: %s",
  "%s must take a single value within each group"
)

#' Model life table families for a multiplier variant
#'
#' @param variant Either `"trussell"` or `"palloni_helligman"`.
#' @return Character vector of model life table family names.
#' @noRd
childmort_families <- function(variant) {
  fam <- CHILDMORT_FAMILIES[[variant]]
  if (is.null(fam)) {
    stop("variant should be 'trussell' or 'palloni_helligman'")
  }
  fam
}

#' Numeric values of a column, `NA` where the value is not a number
#' @noRd
childmort_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

#' Standardise child mortality input columns
#'
#' Matches column names ignoring case, `_`, `.` and spaces, and accepts `W` for
#' `Women`. No values are derived here: see [childmort_children()].
#'
#' @param data Data frame of child mortality data.
#' @return Data frame with standard column names (`Age`, `Women`, `CEB`, `CD`,
#'   `CS`, `PropDead`, `Date`, `Year`, `Month`), keeping all other columns.
#' @noRd
prepare_childmort_input <- function(data) {
  data <- as.data.frame(data)
  key <- gsub("[_. ]", "", tolower(names(data)))
  aliases <- list(
    Age = "age",
    Women = c("women", "w"),
    CEB = "ceb",
    CD = "cd",
    CS = "cs",
    PropDead = c("propdead", "pd"),
    Date = "date",
    Year = "year",
    Month = "month"
  )
  for (std in names(aliases)) {
    hit <- which(key %in% aliases[[std]])
    if (length(hit) > 0 && !std %in% names(data)) {
      names(data)[hit[1]] <- std
    }
  }
  data
}

#' Which children columns the data provide
#'
#' The five input types of Mortpak QFIVE: counts (with a `Women` column) of
#' CEB and CS, CEB and CD, or CS and CD; or averages per woman (no `Women`
#' column) of CEB and the proportion of children dead (`PropDead`), or CEB and
#' CS. Averages of CEB and CD, or CS and CD, also work.
#'
#' @param df Standardised data.
#' @return List with `counts` (whether the data are counts), `columns` (the
#'   children columns used) and `missing` (columns to suggest when the data do
#'   not provide enough).
#' @noRd
childmort_input_type <- function(df) {
  has <- function(col) col %in% names(df)
  counts <- has("Women")
  columns <- if (has("CEB") && has("CD")) {
    c("CEB", "CD")
  } else if (has("CEB") && has("CS")) {
    c("CEB", "CS")
  } else if (has("CS") && has("CD")) {
    c("CS", "CD")
  } else if (has("CEB") && has("PropDead")) {
    c("CEB", "PropDead")
  } else {
    character(0)
  }
  missing <- if (length(columns) > 0) {
    character(0)
  } else if (has("CEB")) {
    c("CD", "CS", "PropDead")
  } else if (has("CD")) {
    c("CEB", "CS")
  } else if (has("CS")) {
    c("CEB", "CD")
  } else {
    c("CEB", "CD", "CS")
  }
  list(counts = counts, columns = columns, missing = missing)
}

#' Children ever born, children dead and women for the Brass method
#'
#' Derives CEB and CD from any of the input types; averages per woman use
#' `W = 1`. When CEB, CD and CS are all given, CEB and CD are used.
#'
#' @param df Standardised data for one group, sorted by age.
#' @return List with numeric vectors `CEB`, `CD` and `W`.
#' @noRd
childmort_children <- function(df) {
  get <- function(col) if (col %in% names(df)) childmort_num(df[[col]]) else NULL
  ceb <- get("CEB")
  cd <- get("CD")
  cs <- get("CS")
  pd <- get("PropDead")
  if (is.null(ceb)) ceb <- cs + cd
  if (is.null(cd)) cd <- if (!is.null(cs)) ceb - cs else ceb * pd
  women <- if ("Women" %in% names(df)) childmort_num(df$Women) else rep(1, nrow(df))
  list(CEB = ceb, CD = cd, W = women)
}

#' Decimal year of the first day of a month
#'
#' @param year Year.
#' @param month Month (1 to 12); the day is the 1st, as in Mortpak.
#' @return Decimal year (DemoTools' day-of-year convention).
#' @noRd
childmort_decimal_date <- function(year, month = 1) {
  year <- as.integer(year)
  month <- as.integer(month)
  if (length(year) != 1 || length(month) != 1 || is.na(year) || is.na(month) ||
      month < 1 || month > 12) {
    stop("A date of observation is required")
  }
  DemoTools::dec.date(as.Date(sprintf("%04d-%02d-01", year, month)))
}

#' Build one validation row in the app's standard format
#' @noRd
childmort_check_row <- function(check, failed, message) {
  data.frame(
    check = check,
    message = if (isTRUE(failed)) message else NA_character_,
    pass = if (isTRUE(failed)) "Fail" else "Pass",
    stringsAsFactors = FALSE
  )
}

#' Validate one group of child mortality data
#' @noRd
check_childmort_group <- function(df) {
  type <- childmort_input_type(df)
  missing_cols <- c(if (!"Age" %in% names(df)) "Age", type$missing)
  has_cols <- length(missing_cols) == 0

  rows <- list(childmort_check_row(
    "check required columns",
    !has_cols,
    paste0(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      ". Provide Age and two of CEB, CD and CS (counts with a Women column, or averages per woman without it), or CEB and PropDead"
    )
  ))
  if (!has_cols) {
    return(do.call(rbind, rows))
  }

  required <- c("Age", if (type$counts) "Women", type$columns)
  vals <- lapply(df[required], childmort_num)
  non_numeric <- vapply(required, function(col) {
    any(is.na(vals[[col]]) & !is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
  }, logical(1))
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check numeric columns",
    any(non_numeric),
    paste0("Columns must be numeric: ", paste(required[non_numeric], collapse = ", "))
  )

  has_na <- vapply(vals, function(x) any(is.na(x)), logical(1))
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check missing values",
    any(has_na),
    paste0("Missing values in columns: ", paste(required[has_na], collapse = ", "))
  )

  age <- vals$Age
  ages_ok <- length(age) == length(CHILDMORT_AGES) &&
    !anyNA(age) && isTRUE(all(sort(age) == CHILDMORT_AGES))
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check age groups",
    !ages_ok,
    "Age must contain exactly the seven five-year groups of women 15, 20, 25, 30, 35, 40 and 45"
  )

  if (type$counts) {
    women <- vals$Women
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check women counts",
      any(!is.na(women) & women <= 0),
      "Women must be greater than zero in every age group"
    )
  }

  ordered <- df[order(childmort_num(df$Age)), , drop = FALSE]
  kids <- childmort_children(ordered)
  ceb <- kids$CEB
  cd <- kids$CD
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check non-negative children",
    any(!is.na(ceb) & ceb < 0) || any(!is.na(cd) & cd < 0),
    "Children ever born and children dead cannot be negative"
  )

  # With PropDead, children dead are derived from it: the PropDead check below
  # already covers values above 1, so this one would only repeat it
  if (!"PropDead" %in% type$columns) {
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check children dead within children ever born",
      any(!is.na(ceb) & !is.na(cd) & cd > ceb),
      "Children dead cannot exceed children ever born"
    )
  }

  if ("PropDead" %in% type$columns) {
    pd <- childmort_num(df$PropDead)
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check PropDead",
      any(!is.na(pd) & (pd < 0 | pd > 1)),
      "PropDead must be a proportion between 0 and 1 (for example 0.08, not 8)"
    )
  }

  # CEB, CD and CS all given: they must add up
  if (all(c("CEB", "CD", "CS") %in% names(df))) {
    total <- childmort_num(ordered$CD) + childmort_num(ordered$CS)
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check CEB equals CD plus CS",
      any(!is.na(total) & !is.na(ceb) & abs(total - ceb) > 1e-6 * pmax(1, abs(ceb))),
      "CEB must equal CD plus CS in every age group"
    )
  }

  # Without a Women column the children must be averages per woman: counts
  # without their denominator would give wrong parity ratios
  if (!type$counts) {
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check averages per woman",
      any(!is.na(ceb) & ceb > 15),
      "Without a Women column, CEB, CD and CS must be averages per woman (for example 2.4). Add a Women column if your data are counts"
    )
  }

  # Parity ratios P(15-19)/P(20-24) and P(20-24)/P(25-29) need positive CEB
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check children ever born in younger ages",
    any(is.na(ceb[1:3]) | ceb[1:3] <= 0),
    "Children ever born must be greater than zero for women aged 15-19, 20-24 and 25-29"
  )

  # Optional date columns: a decimal Date, or Year and Month
  if ("Date" %in% names(df)) {
    v <- unique(stats::na.omit(childmort_num(df$Date)))
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check Date",
      length(v) != 1 || v < 1900 || v > 2100,
      "Date must be a single decimal year (for example 1976.5) in each group"
    )
  } else if (any(c("Year", "Month") %in% names(df))) {
    y <- unique(stats::na.omit(childmort_num(df$Year)))
    m <- unique(stats::na.omit(childmort_num(df$Month)))
    rows[[length(rows) + 1]] <- childmort_check_row(
      "check Year and Month",
      length(y) != 1 || length(m) != 1 || y < 1900 || y > 2100 || !m %in% 1:12,
      "Year and Month must hold a single year and a single month (1 to 12) in each group"
    )
  }

  do.call(rbind, rows)
}

#' Validate child mortality input data
#'
#' Runs all checks per group (`.id`) and returns the standard validation table
#' used by the shared data context.
#'
#' @param data Data frame of child mortality data, optionally with `.id`.
#' @return Data frame with columns `check`, `message` and `pass`.
#' @noRd
check_data_childmort <- function(data) {
  data <- prepare_childmort_input(data)
  if (!".id" %in% names(data)) {
    data$.id <- "all"
  }
  split_data <- split(data, data$.id)
  do.call(rbind, lapply(split_data, check_childmort_group))
}

#' Validate Child Mortality Data
#'
#' Wrapper used by the shared data context of the child mortality module.
#'
#' @param data Data frame to be validated.
#' @param i18n Translator object for internationalization.
#' @return Data frame containing validation results.
#' @noRd
validateData_childmort <- function(data, i18n) {
  check_data_childmort(data)
}

#' Date of observation of one group
#'
#' A decimal `Date` column, or `Year` and `Month` columns, override the date
#' set in the controls.
#'
#' @param df Data for one group.
#' @param date_obs Decimal year from the controls (or `NULL`).
#' @return Decimal year.
#' @noRd
childmort_group_date <- function(df, date_obs) {
  single <- function(col) {
    v <- unique(stats::na.omit(childmort_num(df[[col]])))
    if (length(v) > 1) stop(sprintf("%s must take a single value within each group", col))
    v
  }
  if ("Date" %in% names(df)) {
    d <- single("Date")
    if (length(d) == 1) return(d)
  } else if (all(c("Year", "Month") %in% names(df))) {
    y <- single("Year")
    m <- single("Month")
    if (length(y) == 1 && length(m) == 1) return(childmort_decimal_date(y, m))
  }
  if (is.null(date_obs) || length(date_obs) != 1 || is.na(suppressWarnings(as.numeric(date_obs)))) {
    stop("A date of observation is required")
  }
  as.numeric(date_obs)
}

#' Is each estimate inside the range of its model life tables?
#'
#' `childmort_brass()` uses the nearest level, with only a message, when an
#' estimate falls outside the model life tables; this flags those rows.
#'
#' @param estimates Estimates with `family`, `x` and `lx`.
#' @param sex Sex of the model life tables.
#' @return Logical vector.
#' @noRd
childmort_in_range <- function(estimates, sex) {
  mlt <- MortIndEst::modelLTx1_both
  vapply(seq_len(nrow(estimates)), function(i) {
    ref <- mlt$lx[mlt$family == estimates$family[i] & mlt$sex == sex & mlt$age == estimates$x[i]]
    length(ref) > 0 && estimates$lx[i] >= min(ref) && estimates$lx[i] <= max(ref)
  }, logical(1))
}

#' Estimate child mortality for a single group
#'
#' @param df Data frame for one group (already standardised).
#' @param variant `"trussell"` or `"palloni_helligman"`.
#' @param mlt_model Model life table family valid for `variant`, or `"all"`
#'   for every family of the variant.
#' @param sex Sex of the model life tables: `"both"`, `"female"` or `"male"`.
#' @param date_obs Decimal year of the census or survey, used when the data
#'   carry no date columns.
#' @param mam Mean age at maternity (Palloni-Heligman only).
#' @return List with `estimates` (one row per women's age group and family)
#'   and `mlt` (interpolated model life tables).
#' @noRd
childmort_estimate_group <- function(df, variant, mlt_model, sex, date_obs, mam = 27) {
  df <- df[order(childmort_num(df$Age)), , drop = FALSE]
  date_obs <- childmort_group_date(df, date_obs)
  kids <- childmort_children(df)

  res <- with_mortindest_deps(
    suppressWarnings(suppressMessages(MortIndEst::childmort_brass(
      CEB = kids$CEB,
      CD = kids$CD,
      W = kids$W,
      age = childmort_num(df$Age),
      variant = variant,
      mlt_model = if (identical(mlt_model, "all")) NULL else mlt_model,
      Sex = sex,
      date_obs = date_obs,
      mam = mam
    ))),
    extra = CHILDMORT_PACKAGES
  )

  qx <- as.data.frame(res$qx_estimate)
  summary <- as.data.frame(res$table_output)
  est <- merge(
    qx,
    summary[, c("age", "variant", "mlt_model", "q0", "q1_4", "q0_5", "e0")],
    by = c("age", "variant", "mlt_model"), all.x = TRUE
  )
  family_order <- match(est$mlt_model, CHILDMORT_FAMILIES[[variant]])
  est <- est[order(family_order, est$age), , drop = FALSE]

  estimates <- data.frame(
    Age = est$age,
    age_label = paste0(est$age, "-", est$age + 4),
    family = est$mlt_model,
    variant = est$variant,
    sex = sex,
    x = est$x,
    q_label = paste0("q(", est$x, ")"),
    qx = est$qx,
    lx = est$lx,
    t_years = est$t,
    date_t = est$date_t,
    q0 = est$q0,
    q1_4 = est$q1_4,
    q0_5 = est$q0_5,
    e0 = est$e0,
    stringsAsFactors = FALSE
  )
  estimates$in_range <- childmort_in_range(estimates, sex)
  rownames(estimates) <- NULL

  list(estimates = estimates, mlt = as.data.frame(res$mlts_interp))
}

#' Estimate child mortality for all groups
#'
#' Splits the data by `.id`, applies the Brass method to each group and keeps
#' going when a single group fails.
#'
#' @param data Data frame with `Age` and children columns (see
#'   [childmort_input_type()]), optional `Women`, `Date` or `Year`/`Month`,
#'   and optionally `.id` / `.id_label`.
#' @param variant `"trussell"` (default) or `"palloni_helligman"`.
#' @param mlt_model Model life table family valid for `variant`, or `"all"`.
#' @param sex `"both"` (default), `"female"` or `"male"`.
#' @param date_obs Decimal year of observation, used for groups without date
#'   columns.
#' @param mam Mean age at maternity (Palloni-Heligman only).
#' @param year,month Year and month (1 to 12) of observation; when given they
#'   set `date_obs` (first day of the month).
#' @return List with `estimates` (all groups, with `.id`), `mlt` (all groups),
#'   `group_errors` (data frame of `.id` and `message`, zero rows when all groups
#'   succeed) and `params`.
#' @noRd
run_childmort <- function(data,
                          variant = "trussell",
                          mlt_model = "West",
                          sex = "both",
                          date_obs = NULL,
                          mam = 27,
                          year = NULL,
                          month = NULL) {
  families <- childmort_families(variant)
  if (!mlt_model %in% c(families, "all")) {
    stop(sprintf(
      "Model family '%s' is not available for the %s variant. Choose one of: %s",
      mlt_model, variant, paste(families, collapse = ", ")
    ))
  }
  if (!sex %in% CHILDMORT_SEXES) {
    stop("sex should be 'both', 'female' or 'male'")
  }
  if (!is.null(year)) {
    date_obs <- childmort_decimal_date(year, month %||% 1)
  }

  data <- prepare_childmort_input(data)
  by_group <- run_by_group(
    data,
    childmort_estimate_group,
    variant = variant, mlt_model = mlt_model, sex = sex,
    date_obs = date_obs, mam = mam,
    check = check_childmort_group
  )

  list(
    estimates = by_group[["estimates"]],
    mlt = by_group[["mlt"]],
    group_errors = by_group[["group_errors"]],
    params = list(
      variant = variant,
      mlt_model = mlt_model,
      sex = sex,
      date_obs = date_obs,
      year = year,
      month = if (is.null(year)) NULL else (month %||% 1),
      mam = if (identical(variant, "palloni_helligman")) mam else NA
    )
  )
}

#' Settings behind a child mortality run, for display
#'
#' @param params Parameters of a run (`run_childmort()$params`).
#' @return Data frame with `label` and `value` (English; the module translates
#'   the labels and the values in `translate_value`), one row per setting that
#'   applies to the run.
#' @noRd
childmort_settings_items <- function(params) {
  pal <- identical(params$variant, "palloni_helligman")
  date_value <- if (!is.null(params$year)) {
    sprintf("%04d-%02d", as.integer(params$year), as.integer(params$month %||% 1))
  } else if (is.null(params$date_obs) || length(params$date_obs) == 0) {
    # Date column, or Year and Month columns
    "Taken from your data"
  } else {
    format(params$date_obs)
  }
  family_value <- if (identical(params$mlt_model, "all")) {
    "All families"
  } else if (params$mlt_model %in% names(CHILDMORT_FAMILY_LABELS)) {
    unname(CHILDMORT_FAMILY_LABELS[params$mlt_model])
  } else {
    params$mlt_model
  }
  items <- data.frame(
    label = c("Multiplier variant", "Model life table family",
              "Model life table sex", "Date of observation"),
    value = c(
      if (pal) "Palloni-Heligman" else "Trussell",
      family_value,
      unname(CHILDMORT_SEX_LABELS[params$sex]) %||% params$sex,
      date_value
    ),
    translate_value = c(FALSE, TRUE, TRUE, is.null(params$year) && is.null(params$date_obs)),
    stringsAsFactors = FALSE
  )
  if (pal) {
    items <- rbind(items, data.frame(
      label = "Mean age at childbearing", value = format(params$mam),
      translate_value = FALSE, stringsAsFactors = FALSE
    ))
  }
  items
}

#' Have the controls moved away from the settings of the last run?
#'
#' Only settings that affect the run are compared: the mean age at maternity
#' only for Palloni-Heligman, and the date only when it came from the controls
#' (not from date columns). A `NULL` control value (a control that is being
#' re-rendered, e.g. after a language switch) counts as unchanged.
#'
#' @param params Parameters of the last run (`run_childmort()$params`).
#' @param current List with the current `variant`, `mlt_model`, `sex`,
#'   `year`, `month` (or `date_obs`) and `mam` control values.
#' @return `TRUE` when the shown results no longer match the controls.
#' @noRd
childmort_settings_changed <- function(params, current) {
  differs <- function(old, new) {
    !is.null(new) && !identical(as.character(old), as.character(new))
  }
  differs_num <- function(old, new) {
    !is.null(new) && !is.null(old) && !isTRUE(all.equal(as.numeric(old), as.numeric(new)))
  }
  date_changed <- if (!is.null(params$year)) {
    differs_num(params$year, current$year) || differs_num(params$month, current$month)
  } else {
    !is.null(params$date_obs) && differs_num(params$date_obs, current$date_obs)
  }
  differs(params$variant, current$variant) ||
    differs(params$mlt_model, current$mlt_model) ||
    differs(params$sex, current$sex) ||
    (identical(params$variant, "palloni_helligman") && differs_num(params$mam, current$mam)) ||
    date_changed
}

#' Diagnostic notes for one group of child mortality estimates
#'
#' @param estimates Estimates for a single group (rows of
#'   `run_childmort()$estimates`).
#' @return Character vector of plain-language notes (English; translated by
#'   the caller).
#' @noRd
childmort_diagnostics <- function(estimates) {
  if (is.null(estimates) || nrow(estimates) == 0) {
    return(character(0))
  }
  notes <- c(
    "Estimates from women aged 15-19 and 20-24 (q(1), q(2)) are usually biased upwards because young mothers' children face higher risks; interpret them with caution.",
    "Estimates from women aged 45-49 (q(20)) refer to a period long before the survey and are often affected by recall errors."
  )
  if (any(!estimates$in_range)) {
    notes <- c(notes, "Some estimates fall outside the range of the model life tables: the nearest level was used, so their 1q0, 4q1, 5q0 and life expectancy are only approximate.")
  }
  notes
}

#' Plot child mortality estimates over time
#'
#' @param estimates Estimates for a single group (one or several families).
#' @param indicator Column to chart: `"q0"`, `"q0_5"`, `"q1_4"` or `"qx"`.
#' @param title Plot title.
#' @param x_label,y_label Axis labels.
#' @param hover_labels Words used in the hover text (`women`, `date`), so the
#'   module can pass them translated.
#' @param family_labels Display names of the families (named by family).
#' @return A ggplot object of the indicator against the reference date, one
#'   line per model life table family.
#' @noRd
childmort_plot <- function(estimates,
                           indicator = "q0",
                           title = "Child mortality estimates",
                           x_label = "Reference date",
                           y_label = CHILDMORT_INDICATORS[[indicator]],
                           hover_labels = list(women = "Women", date = "Date"),
                           family_labels = CHILDMORT_FAMILY_LABELS) {
  if (!indicator %in% names(CHILDMORT_INDICATORS)) {
    indicator <- "q0"
  }
  estimates$value <- estimates[[indicator]]
  estimates$family_label <- unname(family_labels[estimates$family])
  several <- length(unique(estimates$family)) > 1
  estimates$hover <- sprintf(
    "%s%s %s<br>%s = %.4f<br>1q0 = %.4f<br>5q0 = %.4f<br>%s = %.1f",
    if (several) paste0(estimates$family_label, "<br>") else "",
    hover_labels$women, estimates$age_label,
    estimates$q_label, estimates$qx, estimates$q0, estimates$q0_5,
    hover_labels$date, estimates$date_t
  )
  # Offset labels in data units: plotly ignores vjust but keeps nudges
  v_range <- diff(range(estimates$value, na.rm = TRUE))
  label_nudge <- if (is.finite(v_range) && v_range > 0) 0.06 * v_range else 0.002
  p <- ggplot2::ggplot(
    estimates,
    ggplot2::aes(x = .data$date_t, y = .data$value, text = .data$hover)
  )
  if (several) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = .data$family_label, color = .data$family_label), linewidth = 0.8) +
      ggplot2::geom_point(ggplot2::aes(color = .data$family_label), size = 2.5) +
      ggplot2::labs(color = NULL)
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = 1), color = "#1b6ec2", linewidth = 0.8) +
      ggplot2::geom_point(color = "#1b6ec2", size = 3) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$q_label),
        nudge_y = label_nudge, size = 3.5, color = "#1b1c1d"
      )
  }
  p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = 13)
}
