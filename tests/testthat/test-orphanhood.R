# Reference cases are the examples of MortIndEst::orphanhood_one() and
# orphanhood_two(), taken from the IUSSP Tools for Demographic Estimation
# spreadsheets ("AM_Orphanhood_OneCensus_Basic - Iraq_v2.xlsx",
# "AM_Orphanhood_OneCensus_AIDS Kenya_v2.xlsx").

# Standard life tables used by the IUSSP spreadsheets
iussp_standard <- data.frame(
  age = seq(15, 85, 5),
  lx = c(0.8868, 0.8763, 0.8621, 0.8467, 0.8296, 0.8094, 0.7845, 0.7527,
         0.7096, 0.6515, 0.5724, 0.4700, 0.3443, 0.2101, 0.0962)
)
iussp_standard_aids <- data.frame(
  age = seq(15, 85, 5),
  lx = c(0.8847, 0.8603, 0.8173, 0.7677, 0.7247, 0.6913, 0.6645, 0.6281,
         0.5783, 0.5110, 0.4225, 0.3149, 0.2012, 0.1021, 0.0377)
)

# Iraq 1997, maternal orphanhood of male respondents
iraq <- data.frame(
  Age = seq(5, 45, 5),
  NotOrphaned = c(0.9918, 0.9819, 0.9665, 0.9415, 0.9005, 0.8374, 0.7678, 0.6380, 0.4941)
)
iraq_published_45q15 <- c(0.072, 0.085, 0.096, 0.110, 0.126, 0.143, 0.137, 0.159, 0.167)

# Kenya 1999, maternal orphanhood with antenatal HIV prevalence
kenya <- data.frame(
  Age = seq(5, 45, 5),
  NotOrphaned = c(0.9732, 0.9560, 0.9336, 0.9080, 0.8771, 0.8244, 0.7691, 0.6685, 0.5653),
  HIVPrev = c(0.07, 0.01, rep(0, 7))
)
kenya_published_45q15 <- c(0.368, 0.179, 0.183, 0.189, 0.183, 0.185, 0.165, 0.161, 0.139)

# Panama 1977 and 1980, maternal orphanhood (Preston-Chen example)
panama <- data.frame(
  Age = seq(0, 40, 5),
  NotOrphaned1 = c(.9960, .9844, .9753, .9556, .9195, .8908, .8157, .7618, .6166),
  NotOrphaned2 = c(.9949, .9903, .9794, .9608, .9316, .8967, .8450, .7746, .6781)
)

failed_checks <- function(res) res$check[res$pass == "Fail"]

# Published values are rounded to 3 decimals: compare in absolute terms.
expect_close <- function(actual, expected, tol = 0.0015) {
  expect_equal(length(actual), length(expected))
  expect_lt(max(abs(actual - expected)), tol)
}

test_that("Iraq 1997 reproduces the published IUSSP 45q15", {
  res <- run_orphanhood(iraq, maternal = TRUE, standard = iussp_standard,
                        mac = 28.2807464, date_obs = 1997.79)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(res$estimates$Age, seq(5, 45, 5))
  expect_close(res$estimates$q15_45, iraq_published_45q15)
  expect_true(all(res$estimates$ref_date < 1997.79))
  expect_equal(res$estimates$years_before, 1997.79 - res$estimates$ref_date)
  expect_true(all(res$estimates$q15_35 < res$estimates$q15_45))
})

test_that("Kenya 1999 HIV adjustment reproduces the published IUSSP 45q15", {
  res <- run_orphanhood(kenya, maternal = TRUE, standard = iussp_standard_aids,
                        mac = 26.75048387, date_obs = 1999.64,
                        hiv_vert_transm = 1 / 3, hiv_fert_ratio = 3 / 4)
  expect_equal(nrow(res$group_errors), 0)
  expect_true(res$params$hiv)
  expect_lt(max(abs(res$estimates$q15_45 / kenya_published_45q15 - 1)), 0.005)
  # Without the adjustment the youngest respondents give much lower mortality
  plain <- run_orphanhood(kenya[, c("Age", "NotOrphaned")], standard = iussp_standard_aids,
                          mac = 26.75048387, date_obs = 1999.64)
  expect_lt(plain$estimates$q15_45[1], res$estimates$q15_45[1])
})

test_that("the bundled samples pass validation and run", {
  read_sample <- function(file) {
    utils::read.csv(system.file("extdata", file, package = "lifetableprojection"))
  }
  one <- read_sample("orphanhood_sample.csv")
  two <- read_sample("orphanhood_sample_two.csv")
  hiv <- read_sample("orphanhood_sample_hiv.csv")
  for (sample in list(one, two, hiv)) {
    expect_true(all(check_data_orphanhood(sample)$pass == "Pass"))
  }
  # Dates and mean ages at childbearing come from the files
  res_one <- run_orphanhood(one, standard = iussp_standard)
  expect_close(res_one$estimates$q15_45, iraq_published_45q15)
  res_two <- run_orphanhood(two, method = "preston_chen")
  expect_equal(nrow(res_two$group_errors), 0)
  expect_equal(res_two$params$observations, 2L)
  res_hiv <- run_orphanhood(hiv, standard = iussp_standard_aids)
  expect_lt(max(abs(res_hiv$estimates$q15_45 / kenya_published_45q15 - 1)), 0.005)
})

test_that("the model standard uses female tables for mothers and male tables for fathers", {
  female <- orphanhood_model_standard("CD_West", maternal = TRUE, e0 = 60)
  male <- orphanhood_model_standard("CD_West", maternal = FALSE, e0 = 60)
  mlt <- MortCast::MLTlookup
  expect_equal(female$lx, mlt$lx[mlt$type == "CD_West" & mlt$sex == 2 & mlt$e0 == 60 & mlt$age <= 100] / 1e5)
  expect_equal(male$lx, mlt$lx[mlt$type == "CD_West" & mlt$sex == 1 & mlt$e0 == 60 & mlt$age <= 100] / 1e5)
  expect_equal(range(female$age), c(0, 100))

  res <- run_orphanhood(iraq, maternal = TRUE, family = "CD_West", std_e0 = 60,
                        mac = 28.28, date_obs = 1997.79)
  direct <- with_mortindest_deps(MortIndEst::orphanhood_one(
    prop_not_orphan = iraq$NotOrphaned, age = iraq$Age, maternal = TRUE, mac = 28.28,
    date = 1997.79, mlt_data_input = female, brass_logit = TRUE, verbose = FALSE
  ))
  expect_equal(res$estimates$q15_45, direct$adult_mort_index$q15_45)
})

test_that("MortIndEst's built-in families still use male tables for mothers", {
  # The wrapper builds the standard itself because orphanhood_one() filters
  # MortCast::MLTlookup with `sex == ifelse(sex == "f", 2, 1)`, where `sex`
  # is the column, so it always selects the male tables. If this test fails,
  # MortIndEst has been fixed: the built-in lookup and the wrapper now agree.
  builtin <- with_mortindest_deps(MortIndEst::orphanhood_one(
    prop_not_orphan = iraq$NotOrphaned, age = iraq$Age, maternal = TRUE, mac = 28.28,
    date = 1997.79, mlt_family = "CD_West", brass_logit = TRUE, brass_logit_e0 = 60,
    verbose = FALSE
  ))$adult_mort_index$q15_45
  male_standard <- with_mortindest_deps(MortIndEst::orphanhood_one(
    prop_not_orphan = iraq$NotOrphaned, age = iraq$Age, maternal = TRUE, mac = 28.28,
    date = 1997.79, mlt_data_input = orphanhood_model_standard("CD_West", FALSE, 60),
    brass_logit = TRUE, verbose = FALSE
  ))$adult_mort_index$q15_45
  expect_equal(builtin, male_standard)
})

test_that("the level of the standard snaps to the model life table grid", {
  expect_equal(orphanhood_snap_e0(61), 60)
  expect_equal(orphanhood_snap_e0(61.3), 62.5)
  expect_equal(orphanhood_snap_e0(10), 20)
  expect_equal(orphanhood_snap_e0(200), 100)
  expect_error(orphanhood_snap_e0("abc"), "must be a number")
  res <- run_orphanhood(iraq, std_e0 = 61, mac = 28.28, date_obs = 1997.79)
  expect_equal(res$params$std_e0, 60)
  expect_error(run_orphanhood(iraq, family = "Nowhere", mac = 28, date_obs = 2000),
               "Unknown model life table family")
})

test_that("paternal orphanhood gives estimates for respondents aged 5 to 35", {
  res <- run_orphanhood(iraq, maternal = FALSE, mac = 33, date_obs = 1997.79)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(res$estimates$Age, seq(5, 35, 5))
  expect_false(res$params$maternal)
  notes <- orphanhood_diagnostics(res$estimates, res$params, iraq$Age)
  expect_true(any(grepl("Paternal orphanhood", notes)))
  expect_true(any(grepl("give no estimate", notes)))
})

test_that("two censuses: Timaeus and Preston-Chen run and refer to the midpoint", {
  for (method in c("timaeus", "preston_chen")) {
    res <- run_orphanhood(panama, method = method, mac = 27, date1 = 1977.5, date2 = 1980.36)
    expect_equal(nrow(res$group_errors), 0)
    expect_equal(res$params$method, method)
    expect_true(all(res$estimates$ref_date == (1977.5 + 1980.36) / 2))
    expect_true(all(res$estimates$plausible))
    expect_false("years_before" %in% names(res$estimates))
  }
  timaeus <- run_orphanhood(panama, method = "timaeus", mac = 27, date1 = 1977.5, date2 = 1980.36)
  expect_equal(timaeus$estimates$Age, seq(20, 40, 5))
  expect_error(run_orphanhood(panama, method = "other", mac = 27, date1 = 1977, date2 = 1980),
               "Unknown two-census method")
})

test_that("two censuses: Salomons 1986-1999 paternal example is unchanged", {
  # Package example (no published values): guards against changes in the
  # wrapper or in MortIndEst.
  salomons <- data.frame(
    Age = seq(5, 40, 5),
    NotOrphaned1 = c(0.9644, 0.9340, 0.8862, 0.8088, 0.7060, 0.5812, 0.4620, 0.3317),
    NotOrphaned2 = c(0.9710, 0.9454, 0.9071, 0.8493, 0.7717, 0.6568, 0.5375, 0.3891)
  )
  res <- run_orphanhood(salomons, maternal = FALSE, method = "timaeus",
                        standard = iussp_standard, mac = 34.08,
                        date1 = 1986.9, date2 = 1999.9)
  expect_equal(res$estimates$Age, seq(20, 35, 5))
  expect_equal(round(res$estimates$q15_45, 3), c(0.185, 0.196, 0.198, 0.196))
})

test_that("the Timaeus two-census method asks for respondents aged 15 to 24", {
  res <- run_orphanhood(panama[panama$Age >= 20, ], method = "timaeus", mac = 27,
                        date1 = 1977.5, date2 = 1980.36)
  expect_null(res$estimates)
  expect_match(res$group_errors$message, "aged 15-19 and 20-24")
})

test_that("validation passes on clean data and has the standard format", {
  res <- check_data_orphanhood(iraq)
  expect_named(res, c("check", "message", "pass"))
  expect_true(all(res$pass == "Pass"))
  expect_true(all(check_data_orphanhood(panama)$pass == "Pass"))
})

test_that("validation flags missing columns for one and two observations", {
  res <- check_data_orphanhood(iraq[, "Age", drop = FALSE])
  expect_equal(failed_checks(res), "check required columns")
  expect_match(res$message[res$pass == "Fail"], "NotOrphaned")
  res <- check_data_orphanhood(panama[, c("Age", "NotOrphaned2")])
  expect_match(res$message[res$pass == "Fail"], "NotOrphaned1")
})

test_that("validation flags non-numeric and missing values", {
  bad <- iraq
  bad$NotOrphaned <- as.character(bad$NotOrphaned)
  bad$NotOrphaned[3] <- "n/a"
  expect_true(all(c("check numeric columns", "check missing values") %in%
                    failed_checks(check_data_orphanhood(bad))))
  bad <- iraq
  bad$NotOrphaned[2] <- NA
  expect_equal(failed_checks(check_data_orphanhood(bad)), "check missing values")
})

test_that("validation flags bad age groups", {
  expect_true("check age groups" %in% failed_checks(check_data_orphanhood(iraq[-4, ])))
  expect_true("check age groups" %in% failed_checks(check_data_orphanhood(rbind(iraq, iraq[9, ]))))
  expect_true("check age groups" %in% failed_checks(check_data_orphanhood(transform(iraq, Age = Age + 1))))
  expect_true("check age groups" %in% failed_checks(check_data_orphanhood(iraq[1:2, ])))
  expect_true("check age groups" %in% failed_checks(check_data_orphanhood(transform(iraq, Age = Age + 50))))
})

test_that("validation flags proportions given as percentages or zero", {
  pct <- transform(iraq, NotOrphaned = NotOrphaned * 100)
  res <- check_data_orphanhood(pct)
  expect_equal(failed_checks(res), "check proportions")
  expect_match(res$message[res$pass == "Fail"], "not 95")
  zero <- iraq
  zero$NotOrphaned[9] <- 0
  expect_equal(failed_checks(check_data_orphanhood(zero)), "check proportions")
})

test_that("validation checks optional MAC, Date and HIVPrev columns", {
  expect_equal(failed_checks(check_data_orphanhood(transform(iraq, MAC = 70))), "check MAC")
  expect_equal(failed_checks(check_data_orphanhood(transform(iraq, MAC = 28 + Age / 100))), "check MAC")
  expect_equal(failed_checks(check_data_orphanhood(transform(iraq, Date = 1890))), "check Date")
  expect_equal(failed_checks(check_data_orphanhood(transform(panama, Date1 = 1980, Date2 = 1977))),
               "check date order")
  expect_equal(failed_checks(check_data_orphanhood(transform(kenya, HIVPrev = HIVPrev * 100))),
               "check HIVPrev")
  res <- check_data_orphanhood(transform(panama, HIVPrev = 0.01))
  expect_equal(failed_checks(res), "check HIVPrev")
  expect_match(res$message[res$pass == "Fail"], "only for one census")
  expect_true(all(check_data_orphanhood(transform(iraq, MAC = 28, Date = 1997.8))$pass == "Pass"))
})

test_that("column names are matched ignoring case, underscores and dots", {
  renamed <- iraq
  names(renamed) <- c("age", "not_orphaned")
  expect_true(all(check_data_orphanhood(renamed)$pass == "Pass"))
  res <- run_orphanhood(renamed, standard = iussp_standard, mac = 28.2807464, date_obs = 1997.79)
  expect_close(res$estimates$q15_45, iraq_published_45q15)
  # NotOrphaned + NotOrphaned2 is read as two observations
  two <- panama
  names(two) <- c("Age", "NotOrphaned", "Not.Orphaned2")
  prepared <- prepare_orphanhood_input(two)
  expect_true(all(c("NotOrphaned1", "NotOrphaned2") %in% names(prepared)))
  expect_equal(orphanhood_observations(prepared), 2L)
})

test_that("grouped input keeps .id and a broken group does not stop the others", {
  path <- testthat::test_path("fixtures", "orphanhood_two_regions.csv")
  data <- utils::read.csv(path)
  data$.id <- ifelse(data$Region == "North", 1L, 2L)
  data$.id_label <- data$Region
  expect_true(all(check_data_orphanhood(data)$pass == "Pass"))
  res <- run_orphanhood(data, standard = iussp_standard)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(sort(unique(res$estimates$.id)), c(1L, 2L))
  expect_equal(unique(res$estimates$.id_label[res$estimates$.id == 2]), "South")
  # Dates and MAC differ by region
  expect_close(res$estimates$q15_45[res$estimates$.id == 1], iraq_published_45q15, tol = 0.002)
  north_date <- max(res$estimates$ref_date[res$estimates$.id == 1] + res$estimates$years_before[res$estimates$.id == 1])
  south_date <- max(res$estimates$ref_date[res$estimates$.id == 2] + res$estimates$years_before[res$estimates$.id == 2])
  expect_equal(c(north_date, south_date), c(1997.79, 1998.5))
  expect_true(all(c(".id", "Age", "age_mlt", "lx") %in% names(res$lx)))

  broken <- data
  broken$NotOrphaned[broken$.id == 2][3] <- 1.5
  res <- run_orphanhood(broken, standard = iussp_standard)
  expect_equal(unique(res$estimates$.id), 1L)
  expect_equal(res$group_errors$.id, "2")
  expect_match(res$group_errors$message, "proportions")
})

test_that("Date and MAC columns override the controls; missing values are errors", {
  with_cols <- transform(iraq, Date = 2001, MAC = 28.2807464)
  res <- run_orphanhood(with_cols, standard = iussp_standard, mac = 35, date_obs = 1990)
  expect_equal(max(res$estimates$ref_date + res$estimates$years_before), 2001)
  expect_close(res$estimates$q15_45, iraq_published_45q15)

  res <- run_orphanhood(iraq, mac = 28, date_obs = NULL)
  expect_match(res$group_errors$message, "date of observation is required")
  res <- run_orphanhood(iraq, mac = NULL, date_obs = 2000)
  expect_match(res$group_errors$message, "mean age at childbearing is required")
  res <- run_orphanhood(panama, mac = 27, date1 = 1977.5, date2 = NULL)
  expect_match(res$group_errors$message, "second observation is required")
  res <- run_orphanhood(panama, mac = 27, date1 = 1980, date2 = 1977)
  expect_match(res$group_errors$message, "later than the first")
})

test_that("proportions that do not decline with age are refused", {
  # Usually the proportion orphaned entered instead of not orphaned
  orphaned <- transform(iraq, NotOrphaned = 1 - NotOrphaned)
  res <- check_data_orphanhood(orphaned)
  expect_equal(failed_checks(res), "check proportions decline with age")
  expect_match(res$message[res$pass == "Fail"], "not the proportion orphaned")
  flat <- data.frame(Age = seq(5, 45, 5), NotOrphaned = rep(0.2, 9))
  expect_equal(failed_checks(check_data_orphanhood(flat)), "check proportions decline with age")
  res <- run_orphanhood(flat, mac = 28, date_obs = 2000)
  expect_null(res$estimates)
  expect_match(res$group_errors$message, "must decline with age")
  two <- panama
  two$NotOrphaned2 <- rev(two$NotOrphaned2)
  res <- check_data_orphanhood(two)
  expect_match(res$message[res$pass == "Fail"], "^NotOrphaned2 must decline")
  # A small rise between two age groups is only a diagnostic note
  bumpy <- iraq
  bumpy$NotOrphaned[3] <- 0.9850
  expect_true(all(check_data_orphanhood(bumpy)$pass == "Pass"))
  out <- run_orphanhood(bumpy, mac = 28, date_obs = 1997.79)
  notes <- orphanhood_diagnostics(out$estimates, out$params, bumpy$Age, list(bumpy$NotOrphaned))
  expect_true(any(grepl("rise between some consecutive age groups", notes)))
  expect_false(any(grepl("rise between", orphanhood_diagnostics(out$estimates, out$params, iraq$Age,
                                                                 list(iraq$NotOrphaned)))))
})

test_that("a custom standard is checked before running", {
  expect_length(check_orphanhood_standard(iussp_standard), 0)
  short <- iussp_standard[1:10, ]
  expect_match(check_orphanhood_standard(short), "ages 15 to 85")
  blank <- transform(iussp_standard, lx = replace(lx, 3, NA))
  expect_match(check_orphanhood_standard(blank), "Enter lx")
  pct <- transform(iussp_standard, lx = lx * 100)
  expect_match(check_orphanhood_standard(pct), "between 0 and 1")
  rising <- transform(iussp_standard, lx = rev(lx))
  expect_match(check_orphanhood_standard(rising), "must not increase")
  expect_error(run_orphanhood(iraq, standard = blank, mac = 28, date_obs = 2000), "Enter lx")
})

test_that("settings summary lists what the run used", {
  one <- run_orphanhood(iraq, mac = 28.28, date_obs = 1997.79, std_e0 = 62.5)$params
  items <- orphanhood_settings_items(one)
  expect_equal(items$label, c("Parent", "Standard", "Mean age at childbearing", "Date of observation"))
  expect_equal(items$value[1:2], c("Mother", "Coale-Demeny West, e0 = 62.5"))
  expect_equal(items$translate_value, c(TRUE, FALSE, FALSE, FALSE))

  two <- run_orphanhood(panama, maternal = FALSE, method = "preston_chen", standard = iussp_standard,
                        mac = 33, date1 = 1977.5, date2 = 1980.36)$params
  items <- orphanhood_settings_items(two)
  expect_equal(items$label, c("Parent", "Method", "Standard", "Mean age at childbearing", "Dates of observation"))
  expect_equal(items$value, c("Father", "Preston-Chen", "Custom standard", "33", "1977.5 / 1980.36"))

  from_data <- run_orphanhood(transform(kenya, Date = 1999.64, MAC = 26.75), standard = iussp_standard_aids)$params
  items <- orphanhood_settings_items(from_data)
  expect_equal(items$value[items$label == "Date of observation"], "Date column of your data")
  expect_equal(items$value[items$label == "Mean age at childbearing"], "MAC column of your data")
  expect_equal(items$value[items$label == "HIV adjustment"], "Yes")
})

test_that("changed settings are detected only when they affect the run", {
  params <- run_orphanhood(iraq, mac = 28, date_obs = 1997.8, std_e0 = 60)$params
  same <- list(parent = "maternal", method = "preston_chen", standard_type = "model",
               family = "CD_West", std_e0 = 61, mac = 28, date_obs = 1997.8,
               hiv_vert_transm = 0.5)
  # Method and HIV parameters do not apply to one census without HIVPrev;
  # 61 snaps to the same level as 60.
  expect_false(orphanhood_settings_changed(params, same))
  expect_false(orphanhood_settings_changed(params, list()))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(parent = "paternal"))))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(family = "UN_General"))))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(std_e0 = 65))))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(mac = 30))))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(date_obs = 2000))))
  expect_true(orphanhood_settings_changed(params, modifyList(same, list(standard_type = "custom"))))

  two <- run_orphanhood(panama, method = "timaeus", mac = 27, date1 = 1977.5, date2 = 1980.36)$params
  now <- list(parent = "maternal", method = "timaeus", standard_type = "model", family = "CD_West",
              std_e0 = 60, mac = 27, date1 = 1977.5, date2 = 1980.36, date_obs = 1990)
  expect_false(orphanhood_settings_changed(two, now))
  expect_true(orphanhood_settings_changed(two, modifyList(now, list(method = "preston_chen"))))
  expect_true(orphanhood_settings_changed(two, modifyList(now, list(date2 = 1981))))

  # Values taken from the data: the hidden controls are NULL
  cols <- run_orphanhood(transform(iraq, Date = 1997.8, MAC = 28))$params
  expect_false(orphanhood_settings_changed(cols, list(parent = "maternal", standard_type = "model",
                                                      family = "CD_West", std_e0 = 60)))
})

test_that("diagnostics flag HIV adjustment and implausible estimates", {
  res <- run_orphanhood(kenya, standard = iussp_standard_aids, mac = 26.75, date_obs = 1999.64)
  notes <- orphanhood_diagnostics(res$estimates, res$params, kenya$Age)
  expect_true(any(grepl("adoption effect", notes)))
  expect_true(any(grepl("HIV/AIDS", notes)))
  expect_false(any(grepl("give no estimate", notes)))
  est <- res$estimates
  est$plausible[2] <- FALSE
  expect_true(any(grepl("outside the range 0 to 1", orphanhood_diagnostics(est, res$params))))
  two <- run_orphanhood(panama, mac = 27, date1 = 1977.5, date2 = 1980.36)
  expect_true(any(grepl("midpoint", orphanhood_diagnostics(two$estimates, two$params))))
  two_notes <- orphanhood_diagnostics(two$estimates, two$params, panama$Age)
  expect_true(any(grepl("two-census methods only cover", two_notes)))
  expect_false(any(grepl("for one census", two_notes)))
  expect_length(orphanhood_diagnostics(NULL, res$params), 0)
})

test_that("plots are built for one and two observations", {
  one <- run_orphanhood(iraq, mac = 28.28, date_obs = 1997.79)
  p1 <- orphanhood_plot(one$estimates, observations = 1)
  expect_s3_class(p1, "ggplot")
  expect_true(is.numeric(ggplot2::ggplot_build(p1)$data[[1]]$x))
  two <- run_orphanhood(panama, mac = 27, date1 = 1977.5, date2 = 1980.36)
  p2 <- orphanhood_plot(two$estimates, observations = 2)
  expect_s3_class(p2, "ggplot")
  expect_s3_class(plotly::ggplotly(p2), "plotly")
  # Implausible estimates are left out of the chart
  est <- one$estimates
  est$plausible[1] <- FALSE
  expect_equal(nrow(orphanhood_plot(est)$data), nrow(est) - 1)
})

test_that("successful results never partially match an `error` element", {
  res <- run_orphanhood(iraq, mac = 28.28, date_obs = 1997.79)
  expect_null(res$error)
  expect_false(any(startsWith(names(res), "error")))
})

test_that("MortIndEst calls leave the search path as they found it", {
  before <- search()
  out <- with_mortindest_deps("package:dplyr" %in% search())
  expect_true(out)
  expect_equal(search(), before)
})

test_that("module is registered and its UI builds", {
  i18n <- usei18n_local()
  mods <- get_app_modules(i18n)
  expect_true("orphanhood" %in% names(mods))
  expect_identical(mods$orphanhood$server_fun, orphanhood_module_server)
  html <- as.character(orphanhood_module_ui(i18n))
  for (id in c("orphanhood_module_page", "orphanhood-file1", "orphanhood-orphanhood_plot",
               "orphanhood-orphanhood_table", "orphanhood-custom_standard",
               "orphanhood-orphanhood_controls", "orphanhood-orphanhood_hiv_ui")) {
    expect_match(html, id, fixed = TRUE)
  }
  # The custom standard table shows only when that standard is chosen
  expect_match(html, "input.standard_type == &#39;custom&#39;", fixed = TRUE)
})

test_that("every string the module translates has French and Spanish", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  src <- testthat::test_path("..", "..", "R", "mod_orphanhood.R")
  skip_if_not(file.exists(src), "package sources not available")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  literals <- regmatches(code, gregexpr('i18n\\$t\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE))[[1]]
  literals <- sub('^i18n\\$t\\("', "", sub('"\\)$', "", literals))
  literals <- gsub("\\\\u2190", "←", literals)

  # Strings built in the backend and translated by the module
  params <- run_orphanhood(transform(kenya, Date = 1999.64, MAC = 26.75), maternal = FALSE,
                           standard = iussp_standard_aids)$params
  two <- run_orphanhood(panama, method = "preston_chen", mac = 27, date1 = 1977, date2 = 1980)$params
  items <- rbind(orphanhood_settings_items(params), orphanhood_settings_items(two),
                 orphanhood_settings_items(modifyList(two, list(date1 = NULL))))
  est <- data.frame(Age = 20, plausible = FALSE)
  notes <- c(
    orphanhood_diagnostics(est, params, c(0, 20), list(c(0.9, 0.95))),
    orphanhood_diagnostics(est, two, c(0, 20))
  )
  problems <- c(
    check_orphanhood_standard(NULL),
    check_orphanhood_standard(iussp_standard[1:3, ]),
    check_orphanhood_standard(transform(iussp_standard, lx = lx * 10)),
    check_orphanhood_standard(transform(iussp_standard, lx = rev(lx)))
  )
  wanted <- unique(c(literals, items$label, items$value[items$translate_value], notes, problems))
  missing <- setdiff(wanted, tr$en)
  expect_equal(missing, character(0))
  hits <- tr[tr$en %in% wanted, ]
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
  expect_gt(length(wanted), 100)
})

test_that("controls overridden by the data or not applying are not used", {
  input <- list(parent = "maternal", method = "preston_chen", standard_type = "model",
                family = "UN_General", std_e0 = 65, mac = 27, date_obs = 2020,
                date1 = 2010, date2 = 2020, hiv_vert_transm = 0.3,
                hiv_fert_ratio = 0.7, hiv_prop_partner = 0.4)
  one <- orphanhood_control_values(input, transform(iraq, MAC = 28, Date = 1997.79))
  expect_null(one$mac)
  expect_null(one$date_obs)
  expect_null(one$method)
  expect_null(one$hiv_vert_transm)
  expect_equal(one$family, "UN_General")

  plain <- orphanhood_control_values(input, iraq)
  expect_equal(c(plain$mac, plain$date_obs), c(27, 2020))
  expect_null(plain$date1)

  two <- orphanhood_control_values(input, transform(panama, Date1 = 1977.5))
  expect_equal(two$method, "preston_chen")
  expect_null(two$date1)
  expect_equal(two$date2, 2020)
  expect_null(two$date_obs)

  hiv <- orphanhood_control_values(modifyList(input, list(parent = "paternal", standard_type = "custom")), kenya)
  expect_equal(c(hiv$hiv_vert_transm, hiv$hiv_fert_ratio, hiv$hiv_prop_partner), c(0.3, 0.7, 0.4))
  expect_null(hiv$family)
  expect_null(hiv$std_e0)
  expect_equal(orphanhood_control_values(input, NULL)$date_obs, 2020)
})

test_that("analysis info text documents the run parameters", {
  one <- run_orphanhood(kenya, maternal = FALSE, standard = iussp_standard_aids, mac = 33,
                        date_obs = 1999.64, hiv_prop_partner = 0.4)$params
  txt <- orphanhood_info_text(one)
  expect_match(txt, "orphanhood_one", fixed = TRUE)
  expect_match(txt, "Parent: Father")
  expect_match(txt, "Standard: Custom standard")
  expect_match(txt, "whose partner is infected: 0.4")
  expect_match(txt, "years_before")
  two <- run_orphanhood(panama, method = "timaeus", mac = 27, date1 = 1977.5, date2 = 1980.36)$params
  txt <- orphanhood_info_text(two, n_groups = 2)
  expect_match(txt, "orphanhood_two", fixed = TRUE)
  expect_match(txt, "Method: Timaeus")
  expect_match(txt, "Dates of observation: 1977.5 / 1980.36")
  expect_match(txt, "Number of Groups: 2")
  expect_false(grepl("years_before", txt))
})
