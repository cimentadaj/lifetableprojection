# Model life tables (T8): MATCH and COMBIN on MortCast's single-age tables,
# through Sara Hertog's MortPak implementation (R/vendor_mortpak_mlt.R).
# These are self-consistency checks; Mortpak outputs are still needed (T10).

mlt_level_near <- function(family, sex, e0) {
  lk <- as.data.frame(MortCast::MLT1Ylookup)
  d <- lk[lk$type == family & lk$sex == if (identical(sex, "female")) 2 else 1, ]
  d <- d[d$e0 == d$e0[which.min(abs(d$e0 - e0))], ]
  d[order(d$age), ]
}

test_that("MATCH on a tabulated level's own 5q0 returns that level (family and sex mapping)", {
  for (family in c("CD_West", "UN_South_Asian")) {
    for (sex in c("female", "male")) {
      level <- mlt_level_near(family, sex, 60)
      q5 <- 1 - level$lx[level$age == 5] / 1e5
      res <- mlt_table(family, sex, "match", "5q0", q5)
      expect_lt(max(abs(res$single$nMx[1:101] - level$mx[1:101])), 1e-12, label = paste(family, sex))
    }
  }
  # Female and male tables differ
  f <- mlt_table("CD_West", "female", "match", "e0", 60)
  m <- mlt_table("CD_West", "male", "match", "e0", 60)
  expect_false(isTRUE(all.equal(f$achieved[["45q15"]], m$achieved[["45q15"]])))
})

test_that("MATCH reaches the value asked for, on every indicator", {
  for (indicator in names(MLT_INDICATORS)) {
    rng <- mlt_range("UN_General", "male", indicator)
    value <- rng[1] + 0.4 * diff(rng)
    res <- mlt_table("UN_General", "male", "match", indicator, value)
    tol <- if (identical(indicator, "e0")) 0.1 else 0.001
    expect_lt(abs(res$achieved[[indicator]] - value), tol, label = indicator)
  }
})

test_that("matching on e0, or on that table's 5q0 or 45q15, gives nearly the same table", {
  a <- mlt_table("CD_West", "female", "match", "e0", 63.4)
  b <- mlt_table("CD_West", "female", "match", "5q0", a$achieved[["5q0"]])
  c <- mlt_table("CD_West", "female", "match", "45q15", a$achieved[["45q15"]])
  expect_lt(abs(b$achieved[["e0"]] - a$achieved[["e0"]]), 0.15)
  expect_lt(abs(c$achieved[["e0"]] - a$achieved[["e0"]]), 0.15)
})

test_that("COMBIN fed the child and adult values of a MATCH table rebuilds it", {
  for (family in c("CD_West", "UN_General")) {
    m <- mlt_table(family, "male", "match", "e0", 58)
    cb <- mlt_table(family, "male", "combin", q1 = m$achieved[["1q0"]], q5 = m$achieved[["5q0"]],
                    adult_indicator = "45q15", adult_value = m$achieved[["45q15"]])
    expect_lt(abs(cb$achieved[["e0"]] - m$achieved[["e0"]]), 0.05, label = family)
    expect_lt(max(abs(cb$single$lx - m$single$lx)) / 1e5, 0.001, label = family)
  }
})

test_that("COMBIN works with 1q0, 5q0 or both, and keeps the values given", {
  run <- function(...) mlt_table("UN_Latin_American", "female", "combin", adult_indicator = "35q15",
                                 adult_value = 0.12, ...)
  q1_only <- run(q1 = 0.05)
  q5_only <- run(q5 = 0.08)
  both <- run(q1 = 0.05, q5 = 0.08)
  # The graduation to single ages moves 5q0 by up to about 0.0002
  expect_lt(abs(q1_only$achieved[["1q0"]] - 0.05), 1e-4)
  expect_lt(abs(q5_only$achieved[["5q0"]] - 0.08), 5e-4)
  expect_lt(abs(both$achieved[["1q0"]] - 0.05), 1e-4)
  expect_lt(abs(both$achieved[["5q0"]] - 0.08), 5e-4)
  for (res in list(q1_only, q5_only, both)) {
    expect_lt(abs(res$achieved[["35q15"]] - 0.12), 0.002)
  }
})

test_that("single-age and abridged tables agree", {
  res <- mlt_table("CD_South", "female", "match", "5q0", 0.12)
  expect_equal(res$single$Age, 0:110)
  expect_equal(res$abridged$Age, c(0, 1, seq(5, 110, 5)))
  for (age in c(0, 1, 5, 15, 50, 60)) {
    expect_equal(res$abridged$lx[res$abridged$Age == age], res$single$lx[res$single$Age == age])
  }
  expect_equal(res$abridged$ex[1], res$single$ex[1])
  expect_equal(mlt_indicators(res$abridged), mlt_indicators(res$single))
  # The open age group can be changed
  expect_equal(max(mlt_table("CD_South", "female", "match", "5q0", 0.12, open_age = 100)$single$Age), 100)
})

test_that("values outside a family's range, both sexes and inconsistent inputs are refused", {
  expect_error(mlt_table("CD_West", "female", "match", "e0", 15), "outside the range of this model life table family")
  expect_error(mlt_table("CD_West", "female", "match", "e0", 120), "19.9 to 114.8")
  expect_error(mlt_table("CD_West", "female", "match", "5q0", 0.9), "outside the range")
  expect_error(mlt_table("CD_West", "both", "match", "e0", 60), "'female' or 'male'")
  expect_error(mlt_table("CD_West", "female", "match", "e0", NA), "is required")
  expect_error(mlt_table("Brass", "female", "match", "e0", 60), "Model family")
  expect_error(mlt_table("CD_West", "female", "match", "4q1", 0.1), "indicator should be")
  expect_error(mlt_table("CD_West", "female", "combin", adult_value = 0.2), "Give 1q0, 5q0 or both")
  expect_error(mlt_table("CD_West", "female", "combin", q1 = 0.08, q5 = 0.05, adult_value = 0.2), "lower than 5q0")
  expect_error(mlt_table("CD_West", "female", "combin", q1 = 0.05, adult_indicator = "e0", adult_value = 60), "45q15 or 35q15")
  expect_error(mlt_table("CD_West", "female", "combin", q1 = 0.05, adult_value = 0.95), "outside the range")
  # The range check reads every level; ranges are cached per family and sex
  expect_equal(round(mlt_range("CD_West", "female", "e0"), 1), c(19.9, 114.8))
})

test_that("an invalid life table is refused", {
  lt <- mlt_table("CD_West", "female", "match", "e0", 60)$single
  bad <- lt
  bad$nqx[10] <- -0.1
  expect_error(mlt_check_table(bad), "probabilities of dying outside 0-1")
  bad <- lt
  bad$lx[20] <- bad$lx[19] + 10
  expect_error(mlt_check_table(bad), "outside 0-1")
  expect_silent(mlt_check_table(lt))
})

test_that("run_mlt builds each row, reports the failing ones and names blank rows", {
  req <- data.frame(
    Name = c("A", "", "C", NA, "A"),
    Sex = c("female", "male", "female", NA, "male"),
    value = c(60, 55, 150, NA, 65),
    stringsAsFactors = FALSE
  )
  res <- run_mlt(req)
  # The empty fourth row is skipped; names are made unique
  expect_equal(res$summary$Name, c("A", "Table 2", "A 1"))
  expect_equal(res$failed$Name, "C")
  expect_match(res$failed$message, "^e0 = 150 is outside the range")
  expect_equal(nrow(res$single), 3 * 111)
  expect_equal(nrow(res$abridged), 3 * 24)
  expect_equal(res$summary$e0, c(60, 55, 65), tolerance = 0.002)
  expect_equal(res$summary$asked[1], "e0 = 60")
  expect_error(run_mlt(data.frame(Name = NA, Sex = NA, value = NA)), "Add at least one life table")
  # Sex must be known
  bad_sex <- run_mlt(data.frame(Name = "x", Sex = NA, value = 60))
  expect_null(bad_sex$summary)
  expect_equal(bad_sex$failed$message, "Sex must be Female or Male")
  # COMBIN with one child value missing
  cb <- run_mlt(data.frame(Name = "c", Sex = "male", q1 = 0.05, q5 = NA, adult = 0.2),
                method = "combin", family = "UN_General")
  expect_equal(cb$summary$asked, "1q0 = 0.05, 5q0 = -, 45q15 = 0.2")
  expect_lt(abs(cb$summary$q1 - 0.05), 1e-4)
  expect_equal(cb$params$adult_indicator, "45q15")
  expect_null(cb$params$indicator)
})

test_that("the input table is read back whatever the language and headers", {
  shown <- data.frame(Nom = c("a", "b", "c"), Sexe = c("Femme", "Homme", "?"), `5q0` = c("0.1", "x", NA),
                      check.names = FALSE, stringsAsFactors = FALSE)
  req <- mlt_requests_from_table(shown, "match", c(female = "Femme", male = "Homme"))
  expect_equal(names(req), c("Name", "Sex", "value"))
  expect_equal(req$Sex, c("female", "male", NA))
  expect_equal(req$value, c(0.1, NA, NA))
  expect_equal(mlt_sex_from_input(c("F", "male", "Mujer", "Hombre"), c(female = "Mujer", male = "Hombre")),
               c("female", "male", "female", "male"))
  # COMBIN tables have three value columns; a short table is padded
  short <- mlt_requests_from_table(data.frame(a = "x", b = "f", c = 0.05), "combin")
  expect_equal(names(short), c("Name", "Sex", "q1", "q5", "adult"))
  expect_true(is.na(short$adult))
})

test_that("the starting table keeps names and sexes when the indicator changes", {
  start <- mlt_default_requests("match")
  expect_equal(start$value, c(60, 60))
  edited <- transform(start, Name = c("Uganda F", "Uganda M"), value = c(63, 58))
  q5 <- mlt_default_requests("match", "5q0", previous = edited)
  expect_equal(q5$Name, c("Uganda F", "Uganda M"))
  expect_equal(q5$value, c(0.08, 0.08))
  cb <- mlt_default_requests("combin", adult_indicator = "35q15", previous = edited)
  expect_equal(names(cb), c("Name", "Sex", "q1", "q5", "adult"))
  expect_equal(cb$adult, c(0.12, 0.12))
})

test_that("plot, settings and info text describe the run", {
  res <- run_mlt(data.frame(Name = c("a", "b"), Sex = c("female", "male"), value = c(0.05, 0.08)),
                 indicator = "5q0", family = "UN_South_Asian", open_age = 100)
  for (measure in names(MLT_PLOT_MEASURES)) {
    p <- mlt_plot(res$single, measure)
    expect_s3_class(p, "ggplot")
    expect_equal(max(p$data$Age), 99)
  }
  items <- mlt_settings_items(res$params)
  expect_equal(items$value, c("One indicator (MATCH)", "UN South Asian", "Under-five mortality, 5q0", "100+"))
  txt <- mlt_info_text(res$params, 2)
  expect_match(txt, "Model life table family: UN South Asian", fixed = TRUE)
  expect_match(txt, "Number of life tables: 2", fixed = TRUE)
  expect_match(txt, "sarahertog/MortpakAbacusLegacyMLT", fixed = TRUE)
})

test_that("module is registered and its UI builds", {
  i18n <- usei18n_local()
  mods <- get_app_modules(i18n)
  expect_true("mlt" %in% names(mods))
  expect_identical(mods$mlt$server_fun, mlt_module_server)
  html <- as.character(mlt_module_ui(i18n))
  for (id in c("mlt_module_page", "mlt-mlt_input_table", "mlt-mlt_plot", "mlt-mlt_summary_table",
               "mlt-mlt_life_table", "mlt-results_container")) {
    expect_match(html, id, fixed = TRUE)
  }
  # No file upload in this module
  expect_false(grepl("mlt-file1", html, fixed = TRUE))
})

test_that("every string the module shows has French and Spanish", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  src <- testthat::test_path("..", "..", "R", "mod_mlt.R")
  skip_if_not(file.exists(src), "package sources not available")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  literals <- regmatches(code, gregexpr('i18n\\$t\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE))[[1]]
  literals <- sub('^i18n\\$t\\("', "", sub('"\\)$', "", literals))
  literals <- gsub("\\\\u2190", "←", literals)
  items <- rbind(
    mlt_settings_items(list(method = "match", family = "CD_West", indicator = "e0", open_age = 110)),
    mlt_settings_items(list(method = "combin", family = "UN_General", adult_indicator = "45q15", open_age = 110))
  )
  wanted <- unique(c(literals, unname(MLT_METHODS), unname(MLT_INDICATORS), unname(MLT_PLOT_MEASURES),
                     unname(MLT_SEX_LABELS), unname(MLT_FAMILY_LABELS), items$label,
                     items$value[items$translate_value]))
  expect_equal(setdiff(wanted, tr$en), character(0))
  hits <- tr[tr$en %in% wanted, ]
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
  # Run-time messages, through their templates
  messages <- c(
    run_mlt(data.frame(Name = "x", Sex = "female", value = 150))$failed$message,
    run_mlt(data.frame(Name = "x", Sex = "female", value = NA_real_))$failed$message,
    "Sex must be Female or Male", "Add at least one life table to the table", "Give 1q0, 5q0 or both",
    "1q0 must be lower than 5q0", "The adult mortality indicator should be 45q15 or 35q15",
    "The values given do not fit together: they give a life table with probabilities of dying outside 0-1. Check that the child and adult mortality values are consistent",
    "A value of e0 is required"
  )
  messages <- unique(stats::na.omit(messages))
  for (lang in c("fr", "es")) {
    i18n <- usei18n_local()
    i18n$set_translation_language(lang)
    translated <- suppressWarnings(translate_messages(messages, i18n, MLT_MESSAGE_TEMPLATES))
    expect_equal(messages[translated == messages], character(0), label = lang)
  }
})

test_that("successful results never partially match an `error` element", {
  res <- run_mlt(data.frame(Name = "a", Sex = "female", value = 60))
  expect_null(res$error)
  expect_null(res[["error", exact = FALSE]])
})

test_that("the info tab labels follow the language", {
  src <- testthat::test_path("..", "..", "R", "mod_mlt.R")
  skip_if_not(file.exists(src), "package sources not available")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  expect_match(code, "output$info_tab_upload_label <- shiny::renderUI", fixed = TRUE)
  expect_match(code, "output$info_tab_docs_label <- shiny::renderUI", fixed = TRUE)
})

test_that("low mortality is reached too, for MATCH and COMBIN", {
  cases <- list(c("e0", 80), c("e0", 85), c("5q0", 0.004), c("1q0", 0.003), c("45q15", 0.05), c("35q15", 0.02))
  for (family in c("CD_West", "UN_Far_Eastern")) {
    for (sex in c("female", "male")) {
      for (case in cases) {
        value <- as.numeric(case[2])
        res <- expect_silent(mlt_table(family, sex, "match", case[1], value))
        tol <- if (identical(case[1], "e0")) 0.1 else max(0.0005, 0.05 * value)
        expect_lt(abs(res$achieved[[case[1]]] - value), tol, label = paste(family, sex, case[1]))
      }
      cb <- mlt_table(family, sex, "combin", q1 = 0.003, q5 = 0.004, adult_indicator = "45q15", adult_value = 0.05)
      expect_lt(abs(cb$achieved[["45q15"]] - 0.05), 0.001, label = paste(family, sex, "COMBIN"))
      expect_gt(cb$achieved[["e0"]], 78)
    }
  }
})

test_that("the open age group does not change the indicators", {
  base <- mlt_table("CD_West", "female", "match", "e0", 70)
  for (open_age in c(85, 100, 130)) {
    res <- mlt_table("CD_West", "female", "match", "e0", 70, open_age = open_age)
    expect_equal(max(res$single$Age), open_age)
    expect_equal(max(res$abridged$Age), open_age)
    expect_equal(res$achieved, base$achieved, tolerance = 1e-6)
    cb <- mlt_table("CD_West", "female", "combin", q1 = 0.03, q5 = 0.04, adult_value = 0.15, open_age = open_age)
    expect_equal(max(cb$abridged$Age), open_age)
  }
})

test_that("pasted numbers with a decimal comma are read", {
  expect_equal(mlt_parse_number(c("0,05", "0.05", " 63,4 ", "1,234,5", "x", NA, 60)),
               c(0.05, 0.05, 63.4, NA, NA, NA, 60))
  req <- mlt_requests_from_table(data.frame(n = "a", s = "Female", v = "0,08"), "match")
  expect_equal(req$value, 0.08)
})
