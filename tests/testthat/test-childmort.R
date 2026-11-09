panama <- data.frame(
  Age = seq(15, 45, 5),
  Women = c(2695, 2095, 1828, 1605, 1362, 1128, 930),
  CEB = c(278, 1380, 2395, 3097, 3444, 3274, 2682),
  CD = c(24, 77, 172, 236, 348, 394, 354)
)
# UN Manual X (1983), Table 53: Trussell / West multipliers for Panama 1976
manual_x_qx <- c(0.0952, 0.0580, 0.0707, 0.0757, 0.1021, 0.1201, 0.1308)

failed_checks <- function(res) res$check[res$pass == "Fail"]

test_that("Panama sample reproduces UN Manual X Table 53", {
  res <- run_childmort(panama, variant = "trussell", mlt_model = "West",
                       sex = "male", date_obs = 1976)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(round(res$estimates$qx, 4), manual_x_qx)
  expect_equal(res$estimates$x, c(1, 2, 3, 5, 10, 15, 20))
  expect_equal(res$estimates$q_label[1], "q(1)")
  expect_true(all(res$estimates$date_t < 1976))
  expect_true(all(res$estimates$e0_implied > 20 & res$estimates$e0_implied < 100))
})

test_that("bundled sample file passes validation and matches Manual X", {
  path <- system.file("extdata", "childmort_sample.csv", package = "lifetableprojection")
  sample <- utils::read.csv(path)
  checks <- check_data_childmort(sample)
  expect_true(all(checks$pass == "Pass"))
  res <- run_childmort(sample, date_obs = NULL)
  expect_equal(round(res$estimates$qx, 4), manual_x_qx)
})

test_that("children surviving input gives the same result as children dead", {
  cs_input <- transform(panama, CS = CEB - CD)
  cs_input$CD <- NULL
  res_cd <- run_childmort(panama, date_obs = 1976)
  res_cs <- run_childmort(cs_input, date_obs = 1976)
  expect_equal(res_cs$estimates$qx, res_cd$estimates$qx)
})

test_that("column names are matched case-insensitively and W is accepted", {
  renamed <- panama
  names(renamed) <- c("age", "W", "ceb", "cd")
  expect_true(all(check_data_childmort(renamed)$pass == "Pass"))
  res <- run_childmort(renamed, date_obs = 1976)
  expect_equal(round(res$estimates$qx, 4), manual_x_qx)
})

test_that("validation passes on clean data", {
  res <- check_data_childmort(panama)
  expect_true(all(res$pass == "Pass"))
  expect_named(res, c("check", "message", "pass"))
})

test_that("validation flags missing columns", {
  res <- check_data_childmort(panama[, c("Age", "Women", "CEB")])
  expect_equal(failed_checks(res), "check required columns")
  expect_match(res$message[res$pass == "Fail"], "CD")
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

test_that("variant and model family must match (no silent fallback)", {
  expect_error(run_childmort(panama, variant = "trussell", mlt_model = "Latin",
                             date_obs = 1976), "not available")
  expect_error(run_childmort(panama, variant = "palloni_helligman", mlt_model = "West",
                             date_obs = 1976), "not available")
  expect_error(run_childmort(panama, variant = "other", date_obs = 1976), "variant")
  expect_equal(childmort_families("trussell"), c("North", "South", "East", "West"))
})

test_that("Palloni-Helligman variant runs with mean age at maternity", {
  res <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "Latin",
                       sex = "female", date_obs = 1976, mam = 27)
  expect_equal(nrow(res$group_errors), 0)
  expect_equal(nrow(res$estimates), 7)
  expect_true(all(is.finite(res$estimates$qx) & res$estimates$qx > 0))
  expect_equal(res$params$mam, 27)
  res_old <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "Latin",
                           sex = "female", date_obs = 1976, mam = 30)
  expect_false(isTRUE(all.equal(res$estimates$qx, res_old$estimates$qx)))
})

test_that("grouped input returns one block per group and keeps .id", {
  two <- rbind(transform(panama, .id = 1L, .id_label = "North"),
               transform(panama, .id = 2L, .id_label = "South", CD = round(CD * 1.2)))
  res <- run_childmort(two, date_obs = 1976)
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

test_that("a Date column overrides the date of observation per group", {
  two <- rbind(transform(panama, .id = 1L, Date = 1976),
               transform(panama, .id = 2L, Date = 1980))
  res <- run_childmort(two, date_obs = 2000)
  d1 <- res$estimates$date_t[res$estimates$.id == 1]
  d2 <- res$estimates$date_t[res$estimates$.id == 2]
  expect_equal(d2 - d1, rep(4, 7))
  expect_true(all(d1 < 1976))
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

test_that("diagnostics and plot are produced for a group", {
  res <- run_childmort(panama, date_obs = 1976)
  notes <- childmort_diagnostics(res$estimates)
  expect_true(length(notes) >= 2)
  expect_match(notes[1], "15-19")
  expect_length(childmort_diagnostics(NULL), 0)
  p <- childmort_plot(res$estimates)
  expect_s3_class(p, "ggplot")
  expect_s3_class(plotly::ggplotly(p, tooltip = "text"), "plotly")
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
  expect_match(html, "childmort_module_page", fixed = TRUE)
  expect_match(html, "childmort-file1", fixed = TRUE)
  expect_match(html, "childmort-childmort_plot", fixed = TRUE)
  expect_match(html, "childmort-childmort_table", fixed = TRUE)
})

test_that("every new module string has French and Spanish translations", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  module_strings <- c(
    "Child Mortality (Brass Method)", "Multiplier variant", "Model life table family",
    "Model life table sex", "Mean age at maternity", "Estimate Child Mortality",
    "Child mortality estimates", "Implied life expectancy at birth",
    "Results for", "Date of observation", "Date column of your data", "Male", "Female",
    "The settings have changed. Click 'Estimate Child Mortality' to update the results.",
    "Only changes the implied life expectancy at birth; the q(x) estimates stay the same.",
    "Reading the results", "References",
    "Each point on the chart is an estimate of q(x), the probability of dying before age x, placed at the date it refers to. Estimates from younger women refer to more recent dates.",
    "The proportion of children dead drives the estimates; the multiplier variant and model life table family only adjust them slightly. The model life table sex changes only the implied life expectancy at birth.",
    "Implied life expectancy at birth is that of the model life table with the same q(x). It puts the estimates for different ages x on one scale, so you can read the trend over time.",
    "Give less weight to estimates from women aged 15-19 and 20-24, which are often too high, and to those from women aged 45-49, which refer to distant dates and suffer from recall errors.",
    childmort_diagnostics(data.frame(x = 1, qx = 0.1, date_t = 1970, e0_implied = NA))
  )
  hits <- tr[tr$en %in% module_strings, ]
  expect_setequal(hits$en, module_strings)
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
})

test_that("analysis info text documents the run parameters", {
  txt <- childmort_info_text(list(variant = "palloni_helligman", mlt_model = "Latin",
                                  sex = "female", date_obs = 1976, mam = 28))
  expect_match(txt, "Palloni-Heligman")
  expect_match(txt, "Mean age at maternity: 28")
  txt_all <- childmort_info_text(list(variant = "trussell", mlt_model = "West",
                                      sex = "male", date_obs = NULL, mam = NA), n_groups = 3)
  expect_match(txt_all, "All Groups")
  expect_match(txt_all, "taken from the Date column")
  expect_match(txt_all, "Number of Groups: 3")
})

test_that("settings summary lists what the run used", {
  res <- run_childmort(panama, variant = "trussell", mlt_model = "West",
                       sex = "female", date_obs = 1976)
  items <- childmort_settings_items(res$params)
  expect_equal(items$label, c("Multiplier variant", "Model life table family",
                              "Model life table sex", "Date of observation"))
  expect_equal(items$value, c("Trussell", "West", "Female", "1976"))

  ph <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "Far_East_Asian",
                      date_obs = NULL, mam = 28.5)$params
  ph_items <- childmort_settings_items(ph)
  expect_equal(ph_items$value, c("Palloni-Heligman", "Far East Asian", "Male",
                                 "Date column of your data", "28.5"))
  # The family name is translated (e.g. "Latino-américaine"), the variant is a name
  expect_equal(ph_items$translate_value, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("changed settings are detected only when they affect the run", {
  params <- run_childmort(panama, date_obs = 1976)$params  # trussell / West / male
  same <- list(variant = "trussell", mlt_model = "West", sex = "male",
               date_obs = 1976, mam = 27)
  expect_false(childmort_settings_changed(params, same))
  expect_true(childmort_settings_changed(params, modifyList(same, list(mlt_model = "North"))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(sex = "female"))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(date_obs = 1980))))
  expect_true(childmort_settings_changed(params, modifyList(same, list(variant = "palloni_helligman"))))
  # Mean age at maternity is ignored for Trussell
  expect_false(childmort_settings_changed(params, modifyList(same, list(mam = 31))))
  # Controls being re-rendered (NULL) count as unchanged
  expect_false(childmort_settings_changed(params, list(variant = NULL, mlt_model = NULL)))

  ph <- run_childmort(panama, variant = "palloni_helligman", mlt_model = "General",
                      date_obs = NULL, mam = 27)$params
  ph_same <- list(variant = "palloni_helligman", mlt_model = "General", sex = "male",
                  date_obs = 2020, mam = 27)
  # The date comes from the Date column, so the date control is ignored
  expect_false(childmort_settings_changed(ph, ph_same))
  expect_true(childmort_settings_changed(ph, modifyList(ph_same, list(mam = 30))))
})
