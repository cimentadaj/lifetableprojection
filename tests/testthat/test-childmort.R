panama <- data.frame(
  Age = seq(15, 45, 5),
  Women = c(2695, 2095, 1828, 1605, 1362, 1128, 930),
  CEB = c(278, 1380, 2395, 3097, 3444, 3274, 2682),
  CD = c(24, 77, 172, 236, 348, 394, 354)
)
# UN Manual X (1983), Table 53: Trussell / West multipliers for Panama 1976
manual_x_qx <- c(0.0952, 0.0580, 0.0707, 0.0757, 0.1021, 0.1201, 0.1308)

# Mortpak QFIVE template (qfive.mpl): averages per woman, both sexes,
# Palloni-Heligman / Latin American, mean age at maternity 26, 1974
mortpak_template <- data.frame(
  Age = seq(15, 45, 5),
  CEB = c(0.154, 1.129, 2.562, 4.008, 5.237, 5.956, 6.243),
  CS = c(0.138, 0.999, 2.240, 3.477, 4.432, 4.946, 5.013)
)
mortpak_t <- c(1973, 1971.9, 1970.3, 1968.2, 1965.8, 1962.9, 1959.4)
mortpak_q0 <- c(0.105830924787535, 0.0975754471500075, 0.0917687827207557, 0.0872981426626247,
                0.092390952214435, 0.0931828302627686, 0.100965457016312)

# IUSSP Tools for Demographic Estimation, Malawi 2008 census (Trussell / North)
malawi <- data.frame(
  Age = seq(15, 45, 5),
  Women = c(635927, 678071, 566350, 405602, 298004, 221274, 174875),
  CEB = c(180178, 1038556, 1613374, 1697566, 1553676, 1335242, 1128423),
  CS = c(161541, 919584, 1398776, 1426516, 1266514, 1043357, 851048)
)
iussp_t <- c(2007.42, 2006.03, 2004.03, 2001.66, 1999.02, 1996.24, 1993.35)
iussp_q0 <- c(0.1063, 0.0918, 0.0912, 0.1004, 0.1089, 0.1203, 0.1232)

failed_checks <- function(res) res$check[res$pass == "Fail"]

test_that("Panama sample reproduces UN Manual X Table 53", {
  res <- run_childmort(panama, variant = "trussell", mlt_model = "West",
                       sex = "male", date_obs = 1976)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(round(res$estimates$qx, 4), manual_x_qx)
  expect_equal(res$estimates$x, c(1, 2, 3, 5, 10, 15, 20))
  expect_equal(res$estimates$q_label[1], "q(1)")
  expect_true(all(res$estimates$date_t < 1976))
  expect_true(all(res$estimates$e0 > 20 & res$estimates$e0 < 100))
  # q(1) is the infant mortality of its own model life table
  expect_equal(res$estimates$q0[1], res$estimates$qx[1], tolerance = 1e-6)
  expect_true(all(res$estimates$q0_5 > res$estimates$q0))
  expect_true(all(res$estimates$in_range))
})

test_that("the Mortpak QFIVE template is reproduced (averages per woman, both sexes)", {
  res <- run_childmort(mortpak_template, variant = "palloni_helligman", mlt_model = "Latin",
                       sex = "both", date_obs = 1974, mam = 26)
  expect_equal(nrow(res$group_errors), 0)
  # Mortpak prints the dates to one decimal
  expect_lt(max(abs(res$estimates$date_t - mortpak_t)), 0.1)
  expect_lt(max(abs(res$estimates$q0 - mortpak_q0)), 0.0005)
})

test_that("IUSSP Malawi 2008 dates match and 1q0 is close", {
  res <- run_childmort(malawi, variant = "trussell", mlt_model = "North", sex = "both",
                       date_obs = 2008.46)
  expect_lt(max(abs(res$estimates$date_t - iussp_t)), 0.02)
  # IUSSP adjusts the logit intercept instead of interpolating levels
  # linearly, so 1q0 differs slightly (documented in MortIndEst)
  expect_lt(max(abs(res$estimates$q0 - iussp_q0)), 0.015)
})

test_that("bundled samples pass validation and match their references", {
  read_sample <- function(file) {
    utils::read.csv(system.file("extdata", file, package = "lifetableprojection"))
  }
  panama_file <- read_sample("childmort_sample.csv")
  mortpak_file <- read_sample("childmort_sample_mortpak.csv")
  malawi_file <- read_sample("childmort_sample_malawi.csv")
  for (s in list(panama_file, mortpak_file, malawi_file)) {
    expect_true(all(check_data_childmort(s)$pass == "Pass"))
  }
  expect_equal(round(run_childmort(panama_file, sex = "male")$estimates$qx, 4), manual_x_qx)
  # Year and Month columns: January 1974
  mp <- run_childmort(mortpak_file, variant = "palloni_helligman", mlt_model = "Latin", mam = 26)
  expect_lt(max(abs(mp$estimates$q0 - mortpak_q0)), 0.0005)
  mw <- run_childmort(malawi_file, mlt_model = "North")
  expect_lt(max(abs(mw$estimates$date_t - iussp_t)), 0.02)
})

test_that("the five Mortpak input types give the same estimates", {
  base <- run_childmort(panama, date_obs = 1976)$estimates
  types <- list(
    ceb_cs = transform(panama, CS = CEB - CD, CD = NULL),
    cs_cd = transform(panama, CS = CEB - CD, CEB = NULL),
    mean_ceb_propdead = data.frame(Age = panama$Age, CEB = panama$CEB / panama$Women,
                                   PropDead = panama$CD / panama$CEB),
    mean_ceb_cs = data.frame(Age = panama$Age, CEB = panama$CEB / panama$Women,
                             CS = (panama$CEB - panama$CD) / panama$Women)
  )
  for (name in names(types)) {
    expect_true(all(check_data_childmort(types[[name]])$pass == "Pass"), label = name)
    res <- run_childmort(types[[name]], date_obs = 1976)$estimates
    expect_equal(res$qx, base$qx, label = name)
    expect_equal(res$q0, base$q0, label = name)
  }
  expect_true(childmort_input_type(panama)$counts)
  expect_false(childmort_input_type(types$mean_ceb_cs)$counts)
  expect_equal(childmort_input_type(types$mean_ceb_propdead)$columns, c("CEB", "PropDead"))
})

test_that("column names are matched ignoring case and W is accepted", {
  renamed <- panama
  names(renamed) <- c("age", "W", "ceb", "cd")
  expect_true(all(check_data_childmort(renamed)$pass == "Pass"))
  res <- run_childmort(renamed, sex = "male", date_obs = 1976)
  expect_equal(round(res$estimates$qx, 4), manual_x_qx)
  prop <- data.frame(age = panama$Age, ceb = panama$CEB / panama$Women, prop_dead = panama$CD / panama$CEB)
  expect_true(all(check_data_childmort(prop)$pass == "Pass"))
})

test_that("validation passes on clean data", {
  res <- check_data_childmort(panama)
  expect_true(all(res$pass == "Pass"))
  expect_named(res, c("check", "message", "pass"))
})

test_that("validation flags missing children columns", {
  res <- check_data_childmort(panama[, c("Age", "Women", "CEB")])
  expect_equal(failed_checks(res), "check required columns")
  expect_match(res$message[res$pass == "Fail"], "Missing required columns: CD, CS, PropDead")
  res <- check_data_childmort(panama[, c("Women", "CEB", "CD")])
  expect_match(res$message[res$pass == "Fail"], "Missing required columns: Age")
  res <- check_data_childmort(panama[, c("Age", "Women")])
  expect_match(res$message[res$pass == "Fail"], "CEB, CD, CS")
})

test_that("counts without a Women column are refused", {
  # Parity ratios would come out wrong silently
  no_women <- panama[, c("Age", "CEB", "CD")]
  res <- check_data_childmort(no_women)
  expect_equal(failed_checks(res), "check averages per woman")
  expect_match(res$message[res$pass == "Fail"], "Add a Women column")
})

test_that("validation checks PropDead and that CEB, CD and CS add up", {
  bad <- data.frame(Age = panama$Age, CEB = panama$CEB / panama$Women, PropDead = 8)
  expect_true("check PropDead" %in% failed_checks(check_data_childmort(bad)))
  all_three <- transform(panama, CS = CEB - CD)
  expect_true(all(check_data_childmort(all_three)$pass == "Pass"))
  all_three$CS[3] <- all_three$CS[3] + 5
  expect_equal(failed_checks(check_data_childmort(all_three)), "check CEB equals CD plus CS")
})

test_that("validation flags wrong age groups", {
  res <- check_data_childmort(panama[1:6, ])
  expect_true("check age groups" %in% failed_checks(res))
  shifted <- transform(panama, Age = Age + 5)
  expect_true("check age groups" %in% failed_checks(check_data_childmort(shifted)))
})

test_that("validation flags children dead above children ever born", {
  bad <- panama
  bad$CD[4] <- bad$CEB[4] + 1
  expect_true("check children dead within children ever born" %in%
                failed_checks(check_data_childmort(bad)))
})

test_that("validation flags non-positive women and negative children", {
  bad <- panama
  bad$Women[2] <- 0
  expect_true("check women counts" %in% failed_checks(check_data_childmort(bad)))
  bad <- panama
  bad$CEB[5] <- -1
  expect_true("check non-negative children" %in% failed_checks(check_data_childmort(bad)))
})

test_that("validation flags missing and non-numeric values", {
  bad <- panama
  bad$CEB[3] <- NA
  expect_true("check missing values" %in% failed_checks(check_data_childmort(bad)))
  bad <- panama
  bad$Women <- as.character(bad$Women)
  bad$Women[1] <- "n/a"
  expect_true("check numeric columns" %in% failed_checks(check_data_childmort(bad)))
})

test_that("validation flags zero children ever born in the youngest groups", {
  bad <- panama
  bad$CEB[1] <- 0
  bad$CD[1] <- 0
  expect_true("check children ever born in younger ages" %in%
                failed_checks(check_data_childmort(bad)))
})

test_that("validation checks the optional date columns", {
  expect_equal(failed_checks(check_data_childmort(transform(panama, Date = 1890))), "check Date")
  expect_true(all(check_data_childmort(transform(panama, Year = 1976, Month = 7))$pass == "Pass"))
  expect_equal(failed_checks(check_data_childmort(transform(panama, Year = 1976, Month = 13))),
               "check Year and Month")
  expect_equal(failed_checks(check_data_childmort(transform(panama, Year = 1976))),
               "check Year and Month")
})

test_that("variant and model family must match (no silent fallback)", {
  expect_error(run_childmort(panama, variant = "trussell", mlt_model = "Latin",
                             date_obs = 1976), "not available")
  expect_error(run_childmort(panama, variant = "palloni_helligman", mlt_model = "West",
                             date_obs = 1976), "not available")
  expect_error(run_childmort(panama, variant = "other", date_obs = 1976), "variant")
  expect_error(run_childmort(panama, sex = "other", date_obs = 1976), "sex")
  expect_equal(childmort_families("trussell"), c("North", "South", "East", "West"))
})

test_that("all families of a variant are returned together", {
  res <- run_childmort(panama, variant = "trussell", mlt_model = "all", date_obs = 1976)
  expect_equal(nrow(res$estimates), 28)
  expect_equal(unique(res$estimates$family), c("North", "South", "East", "West"))
  west <- run_childmort(panama, variant = "trussell", mlt_model = "West", date_obs = 1976)$estimates
  expect_equal(res$estimates$q0[res$estimates$family == "West"], west$q0)
  ph <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "all", date_obs = 1976)
  expect_equal(nrow(ph$estimates), 35)
})

test_that("the model life table sex changes 1q0 and e0 but not q(x)", {
  both <- run_childmort(panama, sex = "both", date_obs = 1976)$estimates
  male <- run_childmort(panama, sex = "male", date_obs = 1976)$estimates
  female <- run_childmort(panama, sex = "female", date_obs = 1976)$estimates
  expect_equal(both$qx, male$qx)
  expect_false(isTRUE(all.equal(male$q0_5, female$q0_5)))
  expect_false(isTRUE(all.equal(male$e0, both$e0)))
  expect_equal(unique(both$sex), "both")
})

test_that("Palloni-Heligman variant uses the mean age at maternity", {
  res <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "Latin",
                       sex = "female", date_obs = 1976, mam = 27)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(nrow(res$estimates), 7)
  expect_true(all(is.finite(res$estimates$qx) & res$estimates$qx > 0))
  expect_equal(res$params$mam, 27)
  res_old <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "Latin",
                           sex = "female", date_obs = 1976, mam = 30)
  expect_false(isTRUE(all.equal(res$estimates$qx, res_old$estimates$qx)))
  expect_true(is.na(run_childmort(panama, date_obs = 1976, mam = 30)$params$mam))
})

test_that("year and month set the date of observation (first of the month)", {
  res <- run_childmort(panama, year = 1976, month = 7)
  expect_equal(res$params$date_obs, DemoTools::dec.date(as.Date("1976-07-01")))
  expect_equal(res$params$month, 7)
  jan <- run_childmort(panama, year = 1976, month = 1)
  expect_equal(jan$params$date_obs, 1976)
  expect_equal(res$estimates$date_t - jan$estimates$date_t,
               rep(res$params$date_obs - 1976, 7))
  expect_error(childmort_decimal_date(1976, 13), "date of observation")
})

test_that("grouped input returns one block per group and keeps .id", {
  two <- rbind(transform(panama, .id = 1L, .id_label = "North"),
               transform(panama, .id = 2L, .id_label = "South", CD = round(CD * 1.2)))
  res <- run_childmort(two, sex = "male", date_obs = 1976)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(as.vector(table(res$estimates$.id)), c(7L, 7L))
  expect_equal(unique(res$estimates$.id_label), c("North", "South"))
  north <- res$estimates[res$estimates$.id == 1, ]
  south <- res$estimates[res$estimates$.id == 2, ]
  expect_equal(round(north$qx, 4), manual_x_qx)
  expect_true(all(south$qx > north$qx))
})

test_that("a broken group does not stop the other groups", {
  bad <- transform(panama, .id = 2L)
  bad$CD[2] <- bad$CEB[2] + 10
  two <- rbind(transform(panama, .id = 1L), bad)
  res <- run_childmort(two, date_obs = 1976)
  expect_equal(unique(res$estimates$.id), 1L)
  expect_equal(res$group_errors$.id, "2")
  expect_match(res$group_errors$message, "cannot exceed")
})

test_that("Date or Year and Month columns override the date per group", {
  two <- rbind(transform(panama, .id = 1L, Date = 1976),
               transform(panama, .id = 2L, Date = 1980))
  res <- run_childmort(two, date_obs = 2000)
  d1 <- res$estimates$date_t[res$estimates$.id == 1]
  d2 <- res$estimates$date_t[res$estimates$.id == 2]
  expect_equal(d2 - d1, rep(4, 7))
  expect_true(all(d1 < 1976))
  ym <- run_childmort(transform(panama, Year = 1976, Month = 7), year = 2000, month = 1)
  expect_equal(max(ym$estimates$date_t + ym$estimates$t_years), DemoTools::dec.date(as.Date("1976-07-01")))
})

test_that("a missing date is reported as an error", {
  res <- run_childmort(panama, date_obs = NULL)
  expect_null(res$estimates)
  expect_match(res$group_errors$message, "date of observation")
})

test_that("two-region fixture (CS input, grouped) runs end to end", {
  path <- test_path("fixtures", "childmort_two_regions.csv")
  df <- utils::read.csv(path)
  df$.id <- as.integer(factor(df$Region))
  df$.id_label <- df$Region
  expect_true(all(check_data_childmort(df)$pass == "Pass"))
  res <- run_childmort(df, date_obs = NULL)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(nrow(res$estimates), 14)
})

test_that("estimates outside the model life tables are flagged", {
  # Very high proportions dead put the youngest estimates beyond the tables
  extreme <- transform(panama, CD = round(CEB * 0.6))
  res <- run_childmort(extreme, sex = "male", date_obs = 1976)
  expect_equal(nrow(res$group_errors), 0)
  expect_true(any(!res$estimates$in_range))
  expect_true(any(grepl("outside the range of the model life tables",
                        childmort_diagnostics(res$estimates))))
  expect_false(any(grepl("outside the range",
                         childmort_diagnostics(run_childmort(panama, date_obs = 1976)$estimates))))
})

test_that("diagnostics and plot are produced for a group", {
  res <- run_childmort(panama, date_obs = 1976)
  notes <- childmort_diagnostics(res$estimates)
  expect_true(length(notes) >= 2)
  expect_match(notes[1], "15-19")
  expect_length(childmort_diagnostics(NULL), 0)
  for (indicator in names(CHILDMORT_INDICATORS)) {
    p <- childmort_plot(res$estimates, indicator = indicator)
    expect_s3_class(p, "ggplot")
    expect_equal(p$data$value, res$estimates[[indicator]])
  }
  expect_s3_class(plotly::ggplotly(childmort_plot(res$estimates), tooltip = "text"), "plotly")
  all <- run_childmort(panama, mlt_model = "all", date_obs = 1976)$estimates
  p_all <- childmort_plot(all)
  expect_equal(length(unique(p_all$data$family_label)), 4)
  expect_match(p_all$data$hover[1], "North<br>", fixed = TRUE)
})

test_that("MortIndEst calls leave the search path as they found it", {
  before <- search()
  run_childmort(panama, date_obs = 1976)
  expect_equal(search(), before)
})

test_that("successful results never partially match an `error` element", {
  # mod_simple_module_server and the render callback test `result$error`;
  # `$` partial matching must not pick up another list element.
  res <- run_childmort(panama, date_obs = 1976)
  expect_null(res$error)
  expect_false(any(startsWith(names(res), "error")))
})

test_that("module is registered and its UI builds", {
  i18n <- usei18n_local()
  mods <- get_app_modules(i18n)
  expect_true("childmort" %in% names(mods))
  expect_identical(mods$childmort$server_fun, childmort_module_server)
  ui <- childmort_module_ui(i18n)
  html <- as.character(ui)
  for (id in c("childmort_module_page", "childmort-file1", "childmort-childmort_plot",
               "childmort-childmort_table", "childmort-childmort_indicator_ui")) {
    expect_match(html, id, fixed = TRUE)
  }
})

test_that("every string the module translates has French and Spanish", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  src <- testthat::test_path("..", "..", "R", "mod_childmort.R")
  skip_if_not(file.exists(src), "package sources not available")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  literals <- regmatches(code, gregexpr('i18n\\$t\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE))[[1]]
  literals <- sub('^i18n\\$t\\("', "", sub('"\\)$', "", literals))
  literals <- gsub("\\\\u2190", "←", literals)
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  extreme <- run_childmort(transform(panama, CD = round(CEB * 0.6)), date_obs = 1976)
  items <- rbind(
    childmort_settings_items(run_childmort(panama, mlt_model = "all", date_obs = NULL,
                                           year = 1976, month = 7)$params),
    childmort_settings_items(run_childmort(panama, variant = "palloni_helligman",
                                           mlt_model = "Latin", sex = "female")$params)
  )
  wanted <- unique(c(
    literals, months, unname(CHILDMORT_INDICATORS), unname(CHILDMORT_SEX_LABELS),
    unname(CHILDMORT_FAMILY_LABELS), items$label, items$value[items$translate_value],
    childmort_diagnostics(extreme$estimates)
  ))
  expect_equal(setdiff(wanted, tr$en), character(0))
  hits <- tr[tr$en %in% wanted, ]
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
})

test_that("analysis info text documents the run parameters", {
  txt <- childmort_info_text(run_childmort(panama, variant = "palloni_helligman", mlt_model = "Latin",
                                           sex = "both", year = 1976, month = 7, mam = 28)$params)
  expect_match(txt, "Palloni-Heligman")
  expect_match(txt, "Mean age at childbearing: 28")
  expect_match(txt, "1976-07 (first day of the month", fixed = TRUE)
  expect_match(txt, "sex ratio at birth of 1.04")
  expect_match(txt, "q0, q1_4, q0_5")
  txt_all <- childmort_info_text(list(variant = "trussell", mlt_model = "all",
                                      sex = "male", date_obs = NULL, mam = NA), n_groups = 3)
  expect_match(txt_all, "All Groups")
  expect_match(txt_all, "all families of the variant")
  expect_match(txt_all, "Date or Year and Month columns")
  expect_match(txt_all, "Number of Groups: 3")
})

test_that("settings summary lists what the run used", {
  res <- run_childmort(panama, variant = "trussell", mlt_model = "West",
                       sex = "female", year = 1976, month = 7)
  items <- childmort_settings_items(res$params)
  expect_equal(items$label, c("Multiplier variant", "Model life table family",
                              "Model life table sex", "Date of observation"))
  expect_equal(items$value, c("Trussell", "West", "Female", "1976-07"))
  expect_equal(items$translate_value, c(FALSE, TRUE, TRUE, FALSE))

  ph <- run_childmort(transform(panama, Date = 1976), variant = "palloni_helligman",
                      mlt_model = "all", mam = 28.5)$params
  ph_items <- childmort_settings_items(ph)
  expect_equal(ph_items$value, c("Palloni-Heligman", "All families", "Both sexes",
                                 "Date column of your data", "28.5"))
  expect_equal(ph_items$translate_value, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(childmort_settings_items(run_childmort(panama, date_obs = 1976.5)$params)$value[4], "1976.5")
})

test_that("changed settings are detected only when they affect the run", {
  params <- run_childmort(panama, year = 1976, month = 7)$params  # trussell / West / both
  same <- list(variant = "trussell", mlt_model = "West", sex = "both",
               year = 1976, month = "7", mam = NULL)
  expect_false(childmort_settings_changed(params, same))
  expect_true(childmort_settings_changed(params, modifyList(same, list(mlt_model = "all"))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(sex = "female"))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(year = 1980))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(month = "8"))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(variant = "palloni_helligman"))))
  # Mean age at maternity is ignored for Trussell
  expect_false(childmort_settings_changed(params, modifyList(same, list(mam = 31))))
  # Controls being re-rendered (NULL) count as unchanged
  expect_false(childmort_settings_changed(params, list(variant = NULL, mlt_model = NULL)))

  ph <- run_childmort(transform(panama, Date = 1976), variant = "palloni_helligman",
                      mlt_model = "General", mam = 27)$params
  ph_same <- list(variant = "palloni_helligman", mlt_model = "General", sex = "both",
                  year = NULL, month = NULL, mam = 27)
  # The date comes from the Date column, so there are no date controls
  expect_false(childmort_settings_changed(ph, ph_same))
  expect_true(childmort_settings_changed(ph, modifyList(ph_same, list(mam = 30))))
})

test_that("controls overridden by the data are not used", {
  input <- list(variant = "trussell", mlt_model = "West", mlt_sex = "both",
                date_year = 2020, date_month = "3", mam = 29)
  plain <- childmort_control_values(input, panama)
  expect_equal(c(plain$year, plain$month), c(2020, "3"))
  expect_null(plain$mam)
  dated <- childmort_control_values(modifyList(input, list(variant = "palloni_helligman")),
                                    transform(panama, Year = 1976, Month = 7))
  expect_null(dated$year)
  expect_equal(dated$mam, 29)
})
