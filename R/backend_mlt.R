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
      "%s = %s is outside the range of the %s tables for this sex (%s to %s)",
      indicator, format(value), MLT_FAMILY_LABELS[[family]],
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
