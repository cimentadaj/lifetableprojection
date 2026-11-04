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
  expect_equal(nrow(res$errors), 0)
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
  expect_equal(nrow(res$errors), 0)
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
  expect_equal(nrow(res$errors), 0)
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
  expect_equal(res$errors$.id, "2")
  expect_match(res$errors$message, "cannot exceed")
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
  expect_match(res$errors$message, "date of observation")
})

test_that("two-region fixture (CS input, grouped) runs end to end", {
  path <- test_path("fixtures", "childmort_two_regions.csv")
  df <- utils::read.csv(path)
  df$.id <- as.integer(factor(df$Region))
  df$.id_label <- df$Region
  expect_true(all(check_data_childmort(df)$pass == "Pass"))
  res <- run_childmort(df, date_obs = NULL)
  expect_equal(nrow(res$errors), 0)
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
