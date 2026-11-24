# Intercensal survival module (MortIndEst::intercensal_survival)

# Panama females, 1960 and 1970 censuses (UN Manual X, Tables 174 and 186)
panama_ic <- data.frame(
  Age = seq(0, 75, 5),
  Pop1 = c(90071, 76598, 63635, 54431, 45634, 37818, 32179, 28724, 23974, 20618, 15068, 11999, 10283, 6737, 5242, 6756),
  Pop2 = c(114017, 106944, 85253, 73381, 63010, 50924, 40885, 36115, 29409, 25360, 21775, 17632, 13004, 10061, 6690, 9873)
)
# Exact census dates (the controls give these); the estimation rounds them to
# two decimals, as intercensal_survival() does with dates written as text
panama_exact1 <- DemoTools::dec.date(as.Date("1960-12-11"))
panama_exact2 <- DemoTools::dec.date(as.Date("1970-05-10"))
panama_d1 <- round(panama_exact1, 2)
panama_d2 <- round(panama_exact2, 2)

ic_run <- function(data = panama_ic, ...) {
  args <- list(...)
  if (is.null(args$date1) && !"Date1" %in% names(data)) args$date1 <- panama_exact1
  if (is.null(args$date2) && !"Date2" %in% names(data)) args$date2 <- panama_exact2
  do.call(run_intercensal, c(list(data), args))
}

direct_ic <- function(..., hiv = FALSE) {
  with_mortindest_deps(
    suppressMessages(suppressWarnings(MortIndEst::intercensal_survival(...))),
    extra = if (hiv) INTERCENSAL_HIV_PACKAGES else INTERCENSAL_PACKAGES
  )
}

# Stable populations from a life table: the censuses differ only by growth,
# so every method should find the level of the life table they come from
stable_censuses <- function(Lx5, r = 0.02, years = 10) {
  ages <- seq(0, by = 5, length.out = length(Lx5))
  c1 <- 1e6 * Lx5 * exp(-r * (ages + 2.5))
  data.frame(Age = ages, Pop1 = c1, Pop2 = c1 * exp(r * years))
}
cd_west_lx5 <- local({
  tab <- MortCast::MLTlookup
  tab <- tab[tab$type == "CD_West" & tab$sex == 2 & tab$e0 == 60, ]
  as.numeric(tapply(tab$Lx, pmin(trunc(tab$age / 5) * 5, 75), sum))
})
cd_west_60 <- stable_censuses(cd_west_lx5)

ic_failed <- function(res) res$check[res$pass == "Fail"]

test_that("Panama reproduces UN Manual X (variable-r, Table 186; survival ratios, Table 174)", {
  # Variable-r over ages 10-30: average level 17.1 in the Manual, e0 = 60.3
  var_r <- ic_run(method = "var-r", ages_fit = seq(10, 30, 5))
  expect_equal(nrow(var_r$group_errors), 0)
  expect_lt(abs(var_r$summary$mean_e0 - 60.3), 0.3)
  # Survival ratios without the outlying ages 10, 20, 35, 55 and 65: average
  # level 16.1 in the Manual, between e0 57.5 and 60
  match <- ic_run(method = "match", ages_fit = setdiff(seq(0, 70, 5), c(10, 20, 35, 55, 65)))
  expect_gt(match$summary$mean_e0, 57.5)
  expect_lt(match$summary$mean_e0, 60)
  expect_lt(abs(match$summary$mean_e0 - 57.75), 0.6)
})

test_that("the wrapper matches a direct call, and female differs from male", {
  res <- ic_run(method = "match")
  direct <- direct_ic(
    panama_ic$Pop1, panama_ic$Pop2, panama_d1, panama_d2, panama_ic$Age, panama_ic$Age,
    sex = "f", mlt_family = "CD_West", method = "match"
  )
  expect_equal(res$summary$median_e0, direct$selected_level$median_e0)
  expect_equal(res$summary$q15_45, direct$Adult$q15_45)
  expect_equal(res$lt$ex, direct$lt_fit$ex)
  male <- ic_run(sex = "male")
  male_direct <- direct_ic(
    panama_ic$Pop1, panama_ic$Pop2, panama_d1, panama_d2, panama_ic$Age, panama_ic$Age,
    sex = "m", mlt_family = "CD_West", method = "match"
  )
  expect_equal(male$summary$median_e0, male_direct$selected_level$median_e0)
  expect_false(isTRUE(all.equal(male$summary$median_e0, res$summary$median_e0)))
  # A Sex column overrides the control
  expect_equal(ic_run(transform(panama_ic, Sex = "M"))$summary$median_e0, male$summary$median_e0)
  expect_equal(ic_run(transform(panama_ic, Sex = "Female"), sex = "male")$summary$median_e0,
               res$summary$median_e0)
})

test_that("stable populations give back the level of their life table", {
  dates <- list(date1 = 2000, date2 = 2010)
  match <- do.call(run_intercensal, c(list(cd_west_60, method = "match"), dates))
  expect_lt(abs(match$summary$median_e0 - 60), 0.05)
  for (m in c("var-r", "feeney")) {
    res <- do.call(run_intercensal, c(list(cd_west_60, method = m), dates))
    expect_lt(abs(res$summary$median_e0 - 60), 0.5, label = m)
  }
  # The projection methods recover the level of a stationary population
  # exactly; with growth (r = 0.02 here) they come out higher (asked Ivan)
  stationary <- stable_censuses(cd_west_lx5, r = 0)
  for (m in c("bproj", "fproj")) {
    res <- do.call(run_intercensal, c(list(stationary, method = m), dates))
    expect_lt(abs(res$summary$median_e0 - 60), 0.05, label = m)
    growing <- do.call(run_intercensal, c(list(cd_west_60, method = m), dates))
    expect_gt(growing$summary$median_e0, 60, label = m)
    expect_lt(growing$summary$median_e0, 64, label = m)
  }
})

test_that("the Spectrum HIV life tables recover a known HIV level (inputs as proportions)", {
  skip_if_not_installed("rms")
  ns <- asNamespace("MortIndEst")
  # Life table from the Spectrum model: 5q0 0.08, 45q15 0.4, HIV 15%, ART 30%
  q <- unlist(with_mortindest_deps(
    ns$predictNQX("female", cm = 0.08, am = 0.4, hiv = 15, art = 30, adult = "q45"),
    extra = INTERCENSAL_HIV_PACKAGES
  ))
  lx <- DemoTools::lt_id_q_l(1 / (1 + exp(-q)))
  lt <- DemoTools::lt_abridged(lx = lx[c(0, 1, seq(5, 100, 5)) + 1], Age = c(0, 1, seq(5, 100, 5)))
  pops <- stable_censuses(as.numeric(tapply(lt$nLx, pmin(trunc(lt$Age / 5) * 5, 75), sum)))
  true_q45 <- 1 - lt$lx[lt$Age == 60] / lt$lx[lt$Age == 15]
  res <- run_intercensal(pops, date1 = 2000, date2 = 2010, q0 = 0.05, q5 = 0.08,
                         hiv = list(prev = 0.15, art = 0.30))
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(res$summary$family, "HIVSpectrum")
  expect_lt(abs(res$summary$median_e0 - lt$ex[1]), 0.1)
  expect_lt(abs(res$summary$q15_45 - true_q45), 0.005)
  # Without HIV the same populations give a different (wrong) pattern
  plain <- run_intercensal(pops, date1 = 2000, date2 = 2010)
  expect_gt(abs(plain$summary$q15_45 - true_q45), 0.02)
  # Feeney and male populations work with HIV too
  feeney <- run_intercensal(pops, date1 = 2000, date2 = 2010, method = "feeney", q0 = 0.05, q5 = 0.08,
                            hiv = list(prev = 0.15, art = 0.30))
  expect_lt(abs(feeney$summary$q15_45 - true_q45), 0.005)
  male <- run_intercensal(transform(pops, Sex = "m"), date1 = 2000, date2 = 2010, q0 = 0.05, q5 = 0.08,
                          hiv = list(prev = 0.15, art = 0.30))
  expect_equal(nrow(male$group_errors), 0)
  # HIV needs 1q0 and 5q0, and MortIndEst's logit method cannot use the HIV tables
  expect_error(run_intercensal(pops, date1 = 2000, date2 = 2010, hiv = list(prev = 0.15, art = 0.3)),
               "need 1q0 and 5q0")
  expect_error(run_intercensal(pops, date1 = 2000, date2 = 2010, method = "logit", q0 = 0.05, q5 = 0.08,
                               hiv = list(prev = 0.15, art = 0.3)), "cannot be used with the Spectrum")
})

test_that("every method runs and the outputs have the expected shape", {
  for (m in names(INTERCENSAL_METHODS)) {
    res <- ic_run(method = m)
    expect_equal(nrow(res$group_errors), 0, label = m)
    expect_equal(nrow(res$summary), 1, label = m)
    expect_equal(nrow(res$by_age), nrow(panama_ic), label = m)
    expect_true(all(c("median_e0", "mean_e0", "p25_e0", "p75_e0", "coef_var", "e0", "q15_45", "q15_35",
                      "ages_used", "reference_e0") %in% names(res$summary)), label = m)
    expect_true(res$summary$q15_45 > 0 && res$summary$q15_45 < 1, label = m)
    expect_true(res$summary$q15_35 < res$summary$q15_45, label = m)
    expect_equal(res$lt$Age[1:3], c(0, 1, 5), label = m)
    expect_gt(res$summary$ages_used, 0, label = m)
    # The selected level is the life table's e0 unless 1q0/5q0 or logit change it
    if (m != "logit") expect_lt(abs(res$summary$e0 - res$summary$median_e0), 0.05, label = m)
  }
  expect_equal(ic_run()$by_age$age_label[c(1, 16)], c("0-4", "75+"))
})

test_that("All families gives one row per family", {
  res <- ic_run(family = "all")
  expect_equal(res$summary$family, INTERCENSAL_FAMILIES)
  expect_equal(nrow(res$by_age), 9 * nrow(panama_ic))
  expect_equal(sort(unique(res$lt$family)), sort(INTERCENSAL_FAMILIES))
  west <- ic_run()
  expect_equal(res$summary$median_e0[res$summary$family == "CD_West"], west$summary$median_e0)
})

test_that("Feeney and Preston logit start from the survival-ratio level unless given one", {
  match <- ic_run(method = "match")
  expected_ref <- round(match$summary$median_e0 / 2.5) * 2.5
  for (m in INTERCENSAL_REFERENCE_METHODS) {
    res <- ic_run(method = m)
    expect_equal(res$summary$reference_e0, expected_ref, label = m)
  }
  feeney <- ic_run(method = "feeney", ref_e0 = 57.5)
  expect_equal(feeney$summary$reference_e0, 57.5)
  # A typed level is rounded to the 2.5-year grid MortIndEst uses, and shown so
  expect_equal(ic_run(method = "feeney", ref_e0 = 57)$summary$reference_e0, 57.5)
  expect_equal(ic_run(method = "feeney", ref_e0 = 57)$summary$median_e0, feeney$summary$median_e0)
  expect_error(ic_run(method = "feeney", ref_e0 = 150), "between 20 and 100")
  expect_error(ic_run(method = "logit", ref_e0 = 10), "between 20 and 100")
  direct <- direct_ic(
    panama_ic$Pop1, panama_ic$Pop2, panama_d1, panama_d2, panama_ic$Age, panama_ic$Age,
    sex = "f", mlt_family = "CD_West", method = "feeney", mlt_e0_logit_feeney = 57.5
  )
  expect_equal(feeney$summary$median_e0, direct$selected_level$median_e0)
  # With all families each family uses its own survival-ratio level
  all_logit <- ic_run(method = "logit", family = "all")
  all_match <- ic_run(method = "match", family = "all")
  expect_equal(all_logit$summary$reference_e0, round(all_match$summary$median_e0 / 2.5) * 2.5)
  # Other methods have no reference level
  expect_true(is.na(match$summary$reference_e0))
})

test_that("Japan sample reproduces the Feeney example (snapshot)", {
  japan <- utils::read.csv(app_sys("extdata", "intercensal_sample_japan.csv"))
  res <- run_intercensal(japan, method = "feeney", ages_fit = 5, ref_e0 = 72.5)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(res$summary$date1, round(DemoTools::dec.date(as.Date("1960-08-18")), 2))
  expect_lt(abs(res$summary$median_e0 - 72.7), 0.05)
})

test_that("1q0 and 5q0 replace the first ages of the life table and are checked", {
  base <- ic_run()
  res <- ic_run(q0 = 0.08, q5 = 0.12)
  expect_equal(res$lt$lx[res$lt$Age == 1], 1e5 * (1 - 0.08))
  expect_equal(res$lt$lx[res$lt$Age == 5], 1e5 * (1 - 0.12), tolerance = 1e-5)
  expect_equal(res$summary$median_e0, base$summary$median_e0)
  expect_false(isTRUE(all.equal(res$summary$e0, base$summary$e0)))
  bad <- ic_run(q0 = 0.2, q5 = 0.6)
  expect_match(bad$group_errors$message, "do not fit the estimated life table")
  expect_error(ic_run(q0 = 0.05), "Give both 1q0 and 5q0")
  expect_error(ic_run(q0 = 0.1, q5 = 0.05), "0 < 1q0 < 5q0 < 1")
})

test_that("validation catches bad input", {
  ok <- check_data_intercensal(panama_ic)
  expect_true(all(ok$pass == "Pass"))
  expect_equal(ic_failed(check_data_intercensal(panama_ic[, c("Age", "Pop1")])), "check required columns")
  expect_match(check_data_intercensal(panama_ic[, c("Age", "Pop1")])$message[1], "Missing required columns: Pop2")
  txt <- panama_ic
  txt$Pop1 <- as.character(txt$Pop1)
  txt$Pop1[3] <- "x"
  expect_true("check numeric columns" %in% ic_failed(check_data_intercensal(txt)))
  expect_true("check missing values" %in% ic_failed(check_data_intercensal(transform(panama_ic, Pop2 = replace(Pop2, 4, NA)))))
  # Ages: from 0 by 5, each once, at least up to 50
  expect_true("check age groups" %in% ic_failed(check_data_intercensal(rbind(panama_ic, panama_ic))))
  expect_true("check age groups" %in% ic_failed(check_data_intercensal(panama_ic[-1, ])))
  expect_true("check age groups" %in% ic_failed(check_data_intercensal(panama_ic[1:8, ])))
  expect_true("check age groups" %in% ic_failed(check_data_intercensal(transform(panama_ic, Age = Age * 2))))
  expect_true("check populations" %in% ic_failed(check_data_intercensal(transform(panama_ic, Pop1 = replace(Pop1, 2, 0)))))
  # Dates
  both <- transform(panama_ic, Date1 = "1960-12-11", Date2 = "1970/05/10")
  expect_true(all(check_data_intercensal(both)$pass == "Pass"))
  expect_true(all(check_data_intercensal(transform(panama_ic, Date1 = 1960.94, Date2 = 1970.35))$pass == "Pass"))
  expect_true("check Date1 and Date2" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = 1960))))
  expect_true("check census dates" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = "11/12/1960", Date2 = 1970))))
  expect_true("check census dates" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = c(1960, rep(1961, 15)), Date2 = 1970))))
  expect_true("check intercensal interval" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = 1960, Date2 = 1961))))
  expect_true("check intercensal interval" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = 1960, Date2 = 1995))))
  expect_true("check intercensal interval" %in% ic_failed(check_data_intercensal(transform(panama_ic, Date1 = 1970, Date2 = 1960))))
  # Sex
  expect_true(all(check_data_intercensal(transform(panama_ic, Sex = "f"))$pass == "Pass"))
  expect_true("check Sex" %in% ic_failed(check_data_intercensal(transform(panama_ic, Sex = "both"))))
  expect_true("check Sex" %in% ic_failed(check_data_intercensal(transform(panama_ic, Sex = rep(c("f", "m"), 8)))))
  # Column names are matched loosely
  loose <- stats::setNames(panama_ic, c("age", "census_1", "pop.2"))
  expect_true(all(check_data_intercensal(loose)$pass == "Pass"))
})

test_that("dates come from the columns, the controls or text, consistently", {
  from_text <- run_intercensal(transform(panama_ic, Date1 = "1960-12-11", Date2 = "1970-05-10"))
  from_controls <- ic_run()
  expect_equal(from_text$summary$median_e0, from_controls$summary$median_e0)
  expect_equal(from_text$summary$date1, panama_d1)
  # Columns override the controls
  override <- run_intercensal(transform(panama_ic, Date1 = "1960-12-11", Date2 = "1970-05-10"),
                              date1 = 1950, date2 = 1955)
  expect_equal(override$summary$date2, panama_d2)
  expect_equal(intercensal_decimal_date(1960, 12, 11), DemoTools::dec.date(as.Date("1960-12-11")))
  expect_error(intercensal_decimal_date(1960, 2, 30), "not valid dates")
  # No dates anywhere: a per-group error
  missing <- run_intercensal(panama_ic)
  expect_match(missing$group_errors$message, "census dates are required")
  # Any dates 2.5 to 30 years apart run without an unexpected error
  set.seed(42)
  for (i in 1:15) {
    d1 <- round(stats::runif(1, 1950, 2000), sample(0:3, 1))
    d2 <- d1 + stats::runif(1, 2.6, 29.9)
    res <- run_intercensal(panama_ic, date1 = d1, date2 = d2, method = sample(names(INTERCENSAL_METHODS), 1))
    msg <- res$group_errors$message
    expect_true(length(msg) == 0 || grepl("outside|above 1|None of the ages", msg), label = paste(d1, d2, msg))
  }
})

test_that("grouped data: each group uses its own sex and dates, and a broken group is isolated", {
  male <- transform(panama_ic, Pop1 = round(Pop1 * 1.03), Pop2 = round(Pop2 * 1.01))
  data <- rbind(
    cbind(.id = 1, .id_label = "Female", transform(panama_ic, Sex = "female")),
    cbind(.id = 2, .id_label = "Male", transform(male, Sex = "male")),
    cbind(.id = 3, .id_label = "Broken", transform(panama_ic, Sex = "female", Pop2 = replace(Pop2, 3, -5)))
  )
  res <- ic_run(data)
  expect_equal(sort(unique(res$summary$.id)), c(1, 2))
  expect_equal(res$summary$sex, c("female", "male"))
  expect_equal(res$summary$.id_label, c("Female", "Male"))
  expect_equal(res$group_errors$.id, "3")
  expect_match(res$group_errors$message, "greater than zero")
  expect_equal(res$summary$median_e0[1], ic_run()$summary$median_e0)
})

test_that("diagnostic notes explain what happened", {
  notes <- ic_run()$notes$note
  expect_true(any(grepl("^Survival ratios above 1 at ages 10:", notes)))
  expect_true(any(grepl("^The censuses are 9.41 years apart. The first census was moved to 1960.35", notes)))
  expect_true(any(grepl("^The implied levels vary a lot across ages \\(coefficient of variation 0.19\\)", notes)))
  narrow <- ic_run(e0_accept = c(50, 60))
  expect_equal(narrow$summary$clamped, 7)
  expect_true(any(grepl("^7 implied level\\(s\\) were outside", narrow$notes$note)))
  # Exact ten-year interval: no note about moving the first census
  exact <- run_intercensal(cd_west_60, date1 = 2000, date2 = 2010)
  expect_false(any(grepl("first census was moved", exact$notes$note)))
  # Messages printed by MortIndEst become notes
  expect_match(intercensal_message_notes("r-variable variant was applied for feeney method"), "variable-r variant")
  expect_match(intercensal_message_notes("value of e5 out of range, replaced by some of the limits. Review"),
               "replaced by a limit")
  expect_match(intercensal_message_notes("survival ratios greater than 1 for group ages starting on 0, 20, 25"),
               "^Survival ratios above 1 at ages 0, 20, 25:")
  # Readable errors from MortIndEst
  expect_match(ic_run(ages_fit = 10)$group_errors$message, "All survival ratios are above 1")
  expect_match(ic_run(e0_accept = c(80, 100))$group_errors$message, "accepted range of life expectancy")
})

test_that("plot is produced for one family and for all families", {
  res <- ic_run()
  p <- intercensal_plot(res$by_age, res$summary)
  expect_s3_class(p, "ggplot")
  expect_true(all(grepl("Survival ratio", p$data$hover)))
  expect_true(any(grepl("Not used for the fit", p$data$hover)))
  all <- ic_run(family = "all")
  p_all <- intercensal_plot(all$by_age, all$summary, hover_labels = list(
    ratio = "Rapport", implied = "e0", used = "Oui", not_used = "Non"))
  expect_s3_class(p_all, "ggplot")
  expect_true(all(grepl("Rapport", p_all$data$hover)))
  expect_silent(ggplot2::ggplot_build(p_all))
})

test_that("MortIndEst calls leave the search path as they found it", {
  before <- search()
  ic_run(method = "logit", family = "all")
  expect_identical(search(), before)
})

test_that("module is registered and its UI builds", {
  i18n <- usei18n_local()
  mods <- get_app_modules(i18n)
  expect_true("intercensal" %in% names(mods))
  expect_identical(mods$intercensal$server_fun, intercensal_module_server)
  html <- as.character(intercensal_module_ui(i18n))
  for (id in c("intercensal_module_page", "intercensal-file1", "intercensal-intercensal_plot",
               "intercensal-intercensal_table", "intercensal-intercensal_summary_table")) {
    expect_match(html, id, fixed = TRUE)
  }
})

test_that("every string the module shows has French and Spanish", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  src <- testthat::test_path("..", "..", "R", "mod_intercensal.R")
  skip_if_not(file.exists(src), "package sources not available")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  literals <- regmatches(code, gregexpr('i18n\\$t\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE))[[1]]
  literals <- sub('^i18n\\$t\\("', "", sub('"\\)$', "", literals))
  literals <- gsub("\\\\u2190", "←", literals)
  literals <- gsub('\\\\"', '"', literals)
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  items <- rbind(
    intercensal_settings_items(ic_run(method = "logit", family = "all")$params),
    intercensal_settings_items(run_intercensal(transform(panama_ic, Date1 = 1960.94, Date2 = 1970.35),
                                               method = "feeney", ref_e0 = 57.5, q0 = 0.05, q5 = 0.08)$params)
  )
  wanted <- unique(c(
    literals, months, unname(INTERCENSAL_METHODS), unname(INTERCENSAL_FAMILY_LABELS),
    unname(INTERCENSAL_SEX_LABELS), items$label, items$value[items$translate_value],
    "Date of the first census", "Date of the second census"
  ))
  expect_equal(setdiff(wanted, tr$en), character(0))
  hits <- tr[tr$en %in% wanted, ]
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
})

test_that("validation messages, errors and notes are translated in French and Spanish", {
  bad <- list(
    panama_ic[, 1:2], transform(panama_ic, Pop1 = replace(Pop1, 2, NA)), panama_ic[1:6, ],
    transform(panama_ic, Pop2 = replace(Pop2, 3, 0)), transform(panama_ic, Date1 = 1960),
    transform(panama_ic, Date1 = "abc", Date2 = 1970), transform(panama_ic, Date1 = 1960, Date2 = 1961),
    transform(panama_ic, Sex = "x")
  )
  txt <- panama_ic
  txt$Pop1 <- as.character(txt$Pop1)
  txt$Pop1[3] <- "x"
  checks <- do.call(rbind, lapply(c(bad, list(txt)), check_data_intercensal))
  errors <- c(
    ic_run(ages_fit = 10)$group_errors$message,
    ic_run(e0_accept = c(80, 100))$group_errors$message,
    ic_run(q0 = 0.2, q5 = 0.6)$group_errors$message,
    run_intercensal(panama_ic)$group_errors$message,
    "The estimated level is outside the range of this model life table family. Try another family or method.",
    "The estimated level is outside the range of the Spectrum HIV model life tables built from the HIV prevalence, ART coverage and 5q0 given. Check those values.",
    "None of the ages used for the fit gave a usable level. Choose other ages or another method.",
    "The Preston logit estimate of life expectancy at age 5 was outside the model life tables and was replaced by a limit: review the ages used for the fit.",
    "Choose at least one age group for the fit", "The census dates are not valid dates",
    "Give both 1q0 and 5q0, or neither", "1q0 and 5q0 must be probabilities with 0 < 1q0 < 5q0 < 1",
    "The Spectrum HIV life tables need 1q0 and 5q0",
    "The Preston logit method cannot be used with the Spectrum HIV life tables. Choose another method.",
    "The reference e0 must be between 20 and 100",
    "The accepted range of life expectancy must have a lower limit below the upper limit"
  )
  notes <- unique(c(ic_run()$notes$note, ic_run(e0_accept = c(50, 60))$notes$note,
                    intercensal_message_notes(c("r-variable variant was applied for feeney method",
                                                "value of e5 out of range, replaced by some of the limits. Review"))))
  messages <- unique(c(stats::na.omit(checks$message), errors, notes))
  for (lang in c("fr", "es")) {
    i18n <- usei18n_local()
    i18n$set_translation_language(lang)
    translated <- suppressWarnings(vapply(messages, intercensal_translate_note, character(1), i18n = i18n))
    expect_equal(messages[translated == messages], character(0), label = paste(lang, "messages"))
    names_tr <- suppressWarnings(translate_messages(unique(checks$check), i18n))
    expect_equal(unique(checks$check)[names_tr == unique(checks$check)], character(0), label = paste(lang, "checks"))
  }
  # Family-prefixed notes translate the family and the message
  fr <- usei18n_local()
  fr$set_translation_language("fr")
  note <- "UN General: The estimated level is outside the range of this model life table family. Try another family or method."
  expect_match(suppressWarnings(intercensal_translate_note(note, fr)), "^ONU G.n.rale: Le niveau estim", perl = TRUE)
})

test_that("analysis info text documents the run parameters", {
  txt <- intercensal_info_text(ic_run(method = "logit", q0 = 0.05, q5 = 0.08)$params)
  expect_match(txt, "Method: Preston logit", fixed = TRUE)
  expect_match(txt, "Model life table family: Coale-Demeny West", fixed = TRUE)
  expect_match(txt, "Census dates: 1960-12-11 - 1970-05-10", fixed = TRUE)
  expect_match(txt, "Ages used for the fit: 10-70", fixed = TRUE)
  expect_match(txt, "Reference e0: From the survival-ratio method", fixed = TRUE)
  expect_match(txt, "1q0 and 5q0: 0.05 / 0.08", fixed = TRUE)
  expect_match(intercensal_info_text(ic_run()$params, n_groups = 3), "Number of Groups: 3", fixed = TRUE)
})

test_that("settings summary lists what the run used", {
  items <- intercensal_settings_items(ic_run(ages_fit = c(0, 5, 15, 20, 25, 45))$params)
  expect_equal(items$label, c("Method", "Model life table family", "Sex", "Census dates",
                              "Ages used for the fit", "Accepted e0 range"))
  expect_equal(items$value[5], "0-5, 15-25, 45")
  from_data <- intercensal_settings_items(run_intercensal(transform(panama_ic, Date1 = 1960.94, Date2 = 1970.35))$params)
  expect_equal(from_data$value[4], "Date1 and Date2 columns of your data")
  expect_true(from_data$translate_value[4])
  # With a Sex column each group uses its own sex
  sex_col <- intercensal_settings_items(ic_run(transform(panama_ic, Sex = "f"), sex = "male")$params)
  expect_equal(sex_col$value[3], "Sex column of your data")
  expect_true(sex_col$translate_value[3])
  expect_equal(ic_run()$summary$interval, 9.41)
  hiv_items <- intercensal_settings_items(list(
    method = "match", family = "CD_West", sex = "female", date1 = 2000, date2 = 2010,
    ages_fit = 10:12, e0_accept = c(20, 100), q0 = 0.05, q5 = 0.08, hiv = list(prev = 0.1, art = 0.2)
  ))
  expect_equal(hiv_items$value[2], "Spectrum HIV model life tables")
  expect_equal(hiv_items$value[hiv_items$label == "HIV prevalence and ART coverage"], "0.1 / 0.2")
})

test_that("changed settings are detected only when they affect the run", {
  params <- ic_run(method = "feeney")$params
  same <- list(method = "feeney", family = "CD_West", sex = "female", date1 = panama_exact1, date2 = panama_exact2,
               ages_fit = seq(10, 70, 5), e0_accept = c(20, 100), ref_e0 = NULL, q0 = NULL, q5 = NULL,
               q_given = FALSE, hiv_on = FALSE)
  expect_false(intercensal_settings_changed(params, same))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(method = "match"))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(sex = "male"))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(date2 = 1971))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(ages_fit = seq(10, 30, 5)))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(e0_accept = c(30, 90)))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(ref_e0 = 60))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(hiv_on = TRUE))))
  expect_true(intercensal_settings_changed(params, modifyList(same, list(q_given = TRUE, q0 = 0.05, q5 = 0.08))))
  # Controls being re-rendered (NULL) do not count
  expect_false(intercensal_settings_changed(params, list(method = NULL, family = NULL)))
  # Dates taken from the data are not compared with the controls
  from_data <- run_intercensal(transform(panama_ic, Date1 = 1960.94, Date2 = 1970.35))$params
  expect_false(intercensal_settings_changed(from_data, modifyList(same, list(method = "match", date1 = NULL, date2 = NULL))))
})

test_that("controls overridden by the data are not used", {
  input <- list(method = "logit", mlt_family = "UN_General", sex = "male",
                date1_year = 1960, date1_month = "12", date1_day = 11,
                date2_year = 1970, date2_month = "5", date2_day = 10,
                ages_fit = c("15", "10", "20"), e0_min = 20, e0_max = 100, ref_e0 = NA,
                q0 = NA, q5 = NA, hiv = FALSE)
  values <- intercensal_control_values(input, panama_ic)
  expect_equal(values$date1, panama_exact1)
  expect_equal(values$date2, panama_exact2)
  expect_equal(values$sex, "male")
  expect_equal(values$ages_fit, c(10, 15, 20))
  expect_null(values$ref_e0)
  expect_null(values$q5)
  with_cols <- intercensal_control_values(input, transform(panama_ic, Date1 = 1960.94, Date2 = 1970.35, Sex = "f"))
  expect_null(with_cols$date1)
  expect_null(with_cols$sex)
  # The reference level only counts for Feeney and Preston logit
  expect_null(intercensal_control_values(modifyList(input, list(method = "match", ref_e0 = 60)), panama_ic)$ref_e0)
  expect_equal(intercensal_control_values(modifyList(input, list(ref_e0 = 60)), panama_ic)$ref_e0, 60)
  # Emptied 1q0 / 5q0 fields (NULL) count as "not given", so clearing them
  # after a run with them shows the settings-changed warning
  cleared <- intercensal_control_values(modifyList(input, list(q0 = NULL, q5 = NULL)), panama_ic)
  expect_false(cleared$q_given)
  with_q <- ic_run(q0 = 0.05, q5 = 0.08)$params
  expect_true(intercensal_settings_changed(with_q, modifyList(cleared, list(method = "match", sex = "female",
    date1 = panama_exact1, date2 = panama_exact2, ages_fit = seq(10, 70, 5), family = "CD_West"))))
  expect_null(intercensal_control_values(list(), panama_ic)$q_given)
  # An impossible date is NA, so the run can say it is not valid
  expect_true(is.na(intercensal_control_values(modifyList(input, list(date1_month = "2", date1_day = 30)), panama_ic)$date1))
})

test_that("bundled samples pass validation and run", {
  for (file in c("intercensal_sample_panama.csv", "intercensal_sample_japan.csv")) {
    df <- utils::read.csv(app_sys("extdata", file))
    expect_true(all(check_data_intercensal(df)$pass == "Pass"), label = file)
    res <- run_intercensal(df)
    expect_equal(nrow(res$group_errors), 0, label = file)
  }
  panama_file <- utils::read.csv(app_sys("extdata", "intercensal_sample_panama.csv"))
  expect_equal(run_intercensal(panama_file)$summary$median_e0, ic_run()$summary$median_e0)
})
