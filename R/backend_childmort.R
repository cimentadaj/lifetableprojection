#' Child Mortality (Brass) Backend
#'
#' Server-side helpers for the child mortality module. They wrap
#' `MortIndEst::childmort_brass()`, which estimates probabilities of dying in
#' childhood from children ever born (CEB) and children dead (CD) reported by
#' women in five-year age groups 15-19 to 45-49 (UN Manual X, 1983).
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

#' Childhood age (in years) estimated from each women's age group
#' @noRd
CHILDMORT_X <- c(1, 2, 3, 5, 10, 15, 20)

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

#' Standardise child mortality input columns
#'
#' Matches column names case-insensitively, accepts `W` as an alias for
#' `Women`, and derives children dead from children surviving (`CD = CEB - CS`)
#' when only `CS` is supplied.
#'
#' @param data Data frame with `Age`, `Women`, `CEB` and `CD` (or `CS`).
#' @return Data frame with standard column names `Age`, `Women`, `CEB`, `CD`
#'   (and `Date` when present), keeping all other columns.
#' @noRd
prepare_childmort_input <- function(data) {
  data <- as.data.frame(data)
  lower <- tolower(names(data))
  aliases <- list(
    Age = "age",
    Women = c("women", "w"),
    CEB = "ceb",
    CD = "cd",
    CS = "cs",
    Date = "date"
  )
  for (std in names(aliases)) {
    hit <- which(lower %in% aliases[[std]])
    if (length(hit) > 0 && !std %in% names(data)) {
      names(data)[hit[1]] <- std
    }
  }
  if (!"CD" %in% names(data) && all(c("CS", "CEB") %in% names(data))) {
    data$CD <- suppressWarnings(as.numeric(data$CEB) - as.numeric(data$CS))
  }
  data
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
  required <- c("Age", "Women", "CEB", "CD")
  missing_cols <- setdiff(required, names(df))
  has_cols <- length(missing_cols) == 0

  rows <- list(childmort_check_row(
    "check required columns",
    !has_cols,
    paste0(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      ". Provide Age, Women, CEB and either CD or CS"
    )
  ))
  if (!has_cols) {
    return(do.call(rbind, rows))
  }

  vals <- lapply(df[required], function(x) suppressWarnings(as.numeric(as.character(x))))
  non_numeric <- vapply(required, function(col) {
    any(is.na(vals[[col]]) & !is.na(df[[col]]))
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

  women <- vals$Women
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check women counts",
    any(!is.na(women) & women <= 0),
    "Women must be greater than zero in every age group"
  )

  ceb <- vals$CEB
  cd <- vals$CD
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check non-negative children",
    any(!is.na(ceb) & ceb < 0) || any(!is.na(cd) & cd < 0),
    "Children ever born and children dead cannot be negative"
  )

  rows[[length(rows) + 1]] <- childmort_check_row(
    "check children dead within children ever born",
    any(!is.na(ceb) & !is.na(cd) & cd > ceb),
    "Children dead cannot exceed children ever born"
  )

  # Parity ratios P(15-19)/P(20-24) and P(20-24)/P(25-29) need positive CEB
  first3 <- order(age)[seq_len(min(3, length(age)))]
  rows[[length(rows) + 1]] <- childmort_check_row(
    "check children ever born in younger ages",
    any(is.na(ceb[first3]) | ceb[first3] <= 0),
    "Children ever born must be greater than zero for women aged 15-19, 20-24 and 25-29"
  )

  do.call(rbind, rows)
}

#' Validate child mortality input data
#'
#' Runs all checks per group (`.id`) and returns the standard validation table
#' used by the shared data context.
#'
#' @param data Data frame with `Age`, `Women`, `CEB`, `CD` (or `CS`) and
#'   optionally `.id`.
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

#' Estimate child mortality for a single group
#'
#' @param df Data frame for one group (already standardised).
#' @param variant `"trussell"` or `"palloni_helligman"`.
#' @param mlt_model Model life table family valid for `variant`.
#' @param sex Sex of the model life table, `"male"` or `"female"`.
#' @param date_obs Decimal year of the census or survey. Ignored when the data
#'   carry a `Date` column.
#' @param mam Mean age at maternity (Palloni-Helligman only).
#' @return List with `estimates` (one row per women's age group) and
#'   `mlt` (interpolated model life tables).
#' @noRd
childmort_estimate_group <- function(df, variant, mlt_model, sex, date_obs, mam = 27) {
  df <- df[order(as.numeric(df$Age)), , drop = FALSE]

  if ("Date" %in% names(df)) {
    dates <- unique(stats::na.omit(suppressWarnings(as.numeric(df$Date))))
    if (length(dates) > 1) {
      stop("Date must take a single value within each group")
    }
    if (length(dates) == 1) {
      date_obs <- dates
    }
  }
  if (is.null(date_obs) || length(date_obs) != 1 || is.na(as.numeric(date_obs))) {
    stop("A date of observation is required")
  }

  res <- suppressMessages(MortIndEst::childmort_brass(
    CEB = as.numeric(df$CEB),
    CD = as.numeric(df$CD),
    W = as.numeric(df$Women),
    age = as.numeric(df$Age),
    variant = variant,
    mlt_model = mlt_model,
    Sex = sex,
    date_obs = as.numeric(date_obs),
    mam = if (identical(variant, "palloni_helligman")) mam else NULL
  ))

  est <- as.data.frame(res$qx_estimate)
  names(est)[names(est) == "t"] <- "t_years"
  mlt <- as.data.frame(res$mlts_interp)

  e0 <- mlt[mlt$age_mlt == 0, c("age", "ex_mlt"), drop = FALSE]
  est$e0_implied <- e0$ex_mlt[match(est$age, e0$age)]

  est <- data.frame(
    Age = est$age,
    age_label = paste0(est$age, "-", est$age + 4),
    x = est$x,
    q_label = paste0("q(", est$x, ")"),
    qx = est$qx,
    lx = est$lx,
    t_years = est$t_years,
    date_t = est$date_t,
    e0_implied = est$e0_implied,
    stringsAsFactors = FALSE
  )

  list(estimates = est, mlt = mlt)
}

#' Estimate child mortality for all groups
#'
#' Splits the data by `.id`, applies the Brass method to each group and keeps
#' going when a single group fails.
#'
#' @param data Data frame with `Age`, `Women`, `CEB`, `CD` (or `CS`), optional
#'   `Date`, and optionally `.id` / `.id_label`.
#' @param variant `"trussell"` (default) or `"palloni_helligman"`.
#' @param mlt_model Model life table family valid for `variant`.
#' @param sex `"male"` or `"female"`.
#' @param date_obs Decimal year of observation, used for groups without a
#'   `Date` column.
#' @param mam Mean age at maternity (Palloni-Helligman only).
#' @return List with `estimates` (all groups, with `.id`), `mlt` (all groups),
#'   `group_errors` (data frame of `.id` and `message`, zero rows when all groups
#'   succeed) and `params`.
#' @noRd
run_childmort <- function(data,
                          variant = "trussell",
                          mlt_model = "West",
                          sex = "male",
                          date_obs = NULL,
                          mam = 27) {
  families <- childmort_families(variant)
  if (!mlt_model %in% families) {
    stop(sprintf(
      "Model family '%s' is not available for the %s variant. Choose one of: %s",
      mlt_model, variant, paste(families, collapse = ", ")
    ))
  }
  if (!sex %in% c("male", "female")) {
    stop("sex should be 'male' or 'female'")
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
      mam = if (identical(variant, "palloni_helligman")) mam else NA
    )
  )
}

#' Display names of the model life table families
#' @noRd
CHILDMORT_FAMILY_LABELS <- c(
  North = "North", South = "South", East = "East", West = "West",
  Latin = "Latin American", Chilean = "Chilean", South_Asian = "South Asian",
  Far_East_Asian = "Far East Asian", General = "General"
)

#' Settings behind a child mortality run, for display
#'
#' @param params Parameters of a run (`run_childmort()$params`).
#' @return Data frame with `label` and `value` (English; the module translates
#'   the labels and the values in `translate_value`), one row per setting that
#'   applies to the run.
#' @noRd
childmort_settings_items <- function(params) {
  pal <- identical(params$variant, "palloni_helligman")
  date_value <- if (is.null(params$date_obs) || length(params$date_obs) == 0) {
    "Date column of your data"
  } else {
    format(params$date_obs)
  }
  items <- data.frame(
    label = c("Multiplier variant", "Model life table family",
              "Model life table sex", "Date of observation"),
    value = c(
      if (pal) "Palloni-Heligman" else "Trussell",
      if (params$mlt_model %in% names(CHILDMORT_FAMILY_LABELS)) {
        unname(CHILDMORT_FAMILY_LABELS[params$mlt_model])
      } else {
        params$mlt_model
      },
      if (identical(params$sex, "female")) "Female" else "Male",
      date_value
    ),
    translate_value = c(FALSE, FALSE, TRUE, is.null(params$date_obs)),
    stringsAsFactors = FALSE
  )
  if (pal) {
    items <- rbind(items, data.frame(
      label = "Mean age at maternity", value = format(params$mam),
      translate_value = FALSE, stringsAsFactors = FALSE
    ))
  }
  items
}

#' Have the controls moved away from the settings of the last run?
#'
#' Only settings that affect the run are compared: the mean age at maternity
#' only for Palloni-Heligman, and the date only when it came from the control
#' (not from a Date column). A `NULL` control value (a control that is being
#' re-rendered, e.g. after a language switch) counts as unchanged.
#'
#' @param params Parameters of the last run (`run_childmort()$params`).
#' @param current List with the current `variant`, `mlt_model`, `sex`,
#'   `date_obs` and `mam` control values.
#' @return `TRUE` when the shown results no longer match the controls.
#' @noRd
childmort_settings_changed <- function(params, current) {
  differs <- function(old, new) {
    !is.null(new) && !identical(as.character(old), as.character(new))
  }
  differs_num <- function(old, new) {
    !is.null(new) && !isTRUE(all.equal(as.numeric(old), as.numeric(new)))
  }
  differs(params$variant, current$variant) ||
    differs(params$mlt_model, current$mlt_model) ||
    differs(params$sex, current$sex) ||
    (identical(params$variant, "palloni_helligman") && differs_num(params$mam, current$mam)) ||
    (!is.null(params$date_obs) && differs_num(params$date_obs, current$date_obs))
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
  if (any(is.na(estimates$e0_implied))) {
    notes <- c(notes, "Some estimates fall outside the range of the model life tables; their implied life expectancy could not be computed.")
  }
  notes
}

#' Plot child mortality estimates over time
#'
#' @param estimates Estimates for a single group.
#' @param title Plot title.
#' @param x_label,y_label Axis labels.
#' @return A ggplot object of q(x) against the reference date.
#' @noRd
childmort_plot <- function(estimates,
                           title = "Child mortality estimates",
                           x_label = "Reference date",
                           y_label = "Probability of dying, q(x)") {
  estimates$hover <- sprintf(
    "%s<br>Women %s<br>q(x) = %.4f<br>Date = %.1f",
    estimates$q_label, estimates$age_label, estimates$qx, estimates$date_t
  )
  # Offset labels in data units: plotly ignores vjust but keeps nudges
  q_range <- diff(range(estimates$qx, na.rm = TRUE))
  label_nudge <- if (is.finite(q_range) && q_range > 0) 0.06 * q_range else 0.002
  ggplot2::ggplot(
    estimates,
    ggplot2::aes(x = .data$date_t, y = .data$qx, text = .data$hover)
  ) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "#1b6ec2", linewidth = 0.8) +
    ggplot2::geom_point(color = "#1b6ec2", size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$q_label),
      nudge_y = label_nudge, size = 3.5, color = "#1b1c1d"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = 13)
}
