# Messages built while the app runs (validation, per-group errors, chart hover
# text) are translated in the child mortality and orphanhood modules.

translator <- function(lang) {
  i18n <- usei18n_local()
  i18n$set_translation_language(lang)
  i18n
}
quietly <- function(expr) suppressWarnings(expr)

cm_panama <- data.frame(
  Age = seq(15, 45, 5), Women = c(2695, 2095, 1828, 1605, 1362, 1128, 930),
  CEB = c(278, 1380, 2395, 3097, 3444, 3274, 2682), CD = c(24, 77, 172, 236, 348, 394, 354)
)
or_iraq <- data.frame(
  Age = seq(5, 45, 5),
  NotOrphaned = c(0.9918, 0.9819, 0.9665, 0.9415, 0.9005, 0.8374, 0.7678, 0.6380, 0.4941)
)
or_panama <- data.frame(
  Age = seq(0, 40, 5),
  NotOrphaned1 = c(.9960, .9844, .9753, .9556, .9195, .8908, .8157, .7618, .6166),
  NotOrphaned2 = c(.9949, .9903, .9794, .9608, .9316, .8967, .8450, .7746, .6781)
)

# Every message the validators and the estimation can produce for bad input
childmort_messages <- function() {
  bad <- list(
    cm_panama[, 1:3], transform(cm_panama, Women = replace(Women, 2, NA)), cm_panama[1:6, ],
    transform(cm_panama, Women = replace(Women, 2, 0)), transform(cm_panama, CEB = replace(CEB, 5, -1)),
    transform(cm_panama, CD = replace(CD, 4, 5000)), transform(cm_panama, CEB = replace(CEB, 1, 0))
  )
  text <- cm_panama
  text$CD <- as.character(text$CD)
  text$CD[3] <- "x"
  checks <- do.call(rbind, lapply(c(bad, list(text)), check_data_childmort))
  list(
    names = unique(checks$check),
    messages = unique(c(
      stats::na.omit(checks$message),
      run_childmort(transform(cm_panama, Date = c(1976, rep(1977, 6))))$group_errors$message,
      run_childmort(cm_panama, date_obs = NULL)$group_errors$message
    ))
  )
}
orphanhood_messages <- function() {
  bad <- list(
    or_iraq[, "Age", drop = FALSE], transform(or_iraq, NotOrphaned = replace(NotOrphaned, 2, NA)),
    or_iraq[-4, ], transform(or_iraq, NotOrphaned = NotOrphaned * 100), transform(or_iraq, NotOrphaned = 1 - NotOrphaned),
    transform(or_iraq, MAC = 70), transform(or_iraq, Date = 1890), transform(or_panama, Date1 = 1980, Date2 = 1977),
    transform(or_panama, MAC1 = 70, MAC2 = 10), transform(or_iraq, HIVPrev = 2), transform(or_panama, HIVPrev = 0.01),
    transform(or_panama, NotOrphaned2 = rev(NotOrphaned2))
  )
  text <- or_iraq
  text$NotOrphaned <- as.character(text$NotOrphaned)
  text$NotOrphaned[3] <- "x"
  checks <- do.call(rbind, lapply(c(bad, list(text)), check_data_orphanhood))
  runs <- list(
    run_orphanhood(or_iraq, mac = 28, date_obs = NULL), run_orphanhood(or_iraq, mac = NULL, date_obs = 2000),
    run_orphanhood(or_panama, mac = 27, date1 = NULL, date2 = 1980), run_orphanhood(or_panama, mac = 27, date1 = 1977, date2 = NULL),
    run_orphanhood(or_panama, mac = 27, date1 = 1980, date2 = 1977),
    run_orphanhood(or_panama[or_panama$Age >= 20, ], method = "timaeus", mac = 27, date1 = 1977, date2 = 1980),
    run_orphanhood(transform(or_iraq, Date = c(1997, rep(1998, 8))), mac = 28),
    run_orphanhood(or_iraq, mac = 28, date_obs = 2000, spectrum = list(art = 0, q5 = 0.1))
  )
  list(
    names = unique(checks$check),
    messages = unique(c(
      stats::na.omit(checks$message),
      unlist(lapply(runs, function(r) r$group_errors$message)),
      "The method returned no estimates for these age groups",
      "No plausible estimates: 45q15 fell outside 0-1 in every age group. Check that the proportions not orphaned are proportions that decline with age"
    ))
  )
}

test_that("messages with values are translated through their templates", {
  fr <- translator("fr")
  templates <- ORPHANHOOD_MESSAGE_TEMPLATES
  expect_equal(
    quietly(translate_messages("Missing values in columns: Age, NotOrphaned", fr, templates)),
    "Valeurs manquantes dans les colonnes : Age, NotOrphaned"
  )
  # Templates with brackets and dots, values at the start of the message
  expect_equal(
    quietly(translate_messages("MAC (mean age at childbearing) must be between 15 and 50: one value per group, or one per age group", fr, templates)),
    "MAC (âge moyen à la procréation) doit être compris entre 15 et 50 : une valeur par groupe, ou une par groupe d'âges"
  )
  # Several messages joined with "; " (a group that failed several checks)
  expect_equal(
    quietly(translate_messages("Date2 must be later than Date1; Columns must be numeric: Age", fr, templates)),
    "Date2 doit être postérieure à Date1; Ces colonnes doivent être numériques : Age"
  )
  # Unknown messages (e.g. from MortIndEst) and NA are returned unchanged
  expect_equal(quietly(translate_messages(c("something from MortIndEst", NA), fr, templates)),
               c("something from MortIndEst", NA))
  expect_equal(quietly(translate_messages("Missing values in columns: Age", translator("en"), templates)),
               "Missing values in columns: Age")
})

test_that("every validation and estimation message is translated in French and Spanish", {
  sets <- list(
    childmort = c(childmort_messages(), list(templates = list(CHILDMORT_MESSAGE_TEMPLATES))),
    orphanhood = c(orphanhood_messages(), list(templates = list(ORPHANHOOD_MESSAGE_TEMPLATES)))
  )
  expect_gt(length(sets$childmort$messages), 8)
  expect_gt(length(sets$orphanhood$messages), 18)
  for (lang in c("fr", "es")) {
    i18n <- translator(lang)
    for (module in names(sets)) {
      s <- sets[[module]]
      msgs <- quietly(translate_messages(s$messages, i18n, s$templates[[1]]))
      expect_equal(s$messages[msgs == s$messages], character(0), label = paste(module, lang, "messages"))
      names_tr <- quietly(translate_messages(s$names, i18n))
      # Column names (MAC, Date, ...) stay as they are
      untranslated <- s$names[names_tr == s$names]
      expect_equal(untranslated, character(0), label = paste(module, lang, "check names"))
    }
  }
})

test_that("model life table family names have French and Spanish", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  labels <- c(unname(CHILDMORT_FAMILY_LABELS), unname(ORPHANHOOD_FAMILY_LABELS))
  hits <- tr[tr$en %in% labels, ]
  expect_setequal(hits$en, labels)
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
  expect_equal(hits$fr[hits$en == "Coale-Demeny West"], "Coale-Demeny Ouest")
  # Settings show the family name translated
  expect_true(childmort_settings_items(run_childmort(cm_panama, date_obs = 1976)$params)$translate_value[2])
})

validation_module <- function(id, df, validation, templates) {
  shiny::moduleServer(id, function(input, output, session) {
    session$userData$language_version <- shiny::reactiveVal(0)
    i18n <- usei18n_local()
    shared <- create_shared_data_context(
      id, input, output, session, i18n,
      sample_loader = function() df, validation_function = validation
    )
    render_translated_validation(output, session, i18n, shared, templates)
  })
}

test_that("validation summary and table follow the selected language", {
  bad <- transform(or_iraq, NotOrphaned = NotOrphaned * 100)
  quietly(shiny::testServer(validation_module, args = list(
    id = "orphanhood", df = bad, validation = validateData_orphanhood,
    templates = ORPHANHOOD_MESSAGE_TEMPLATES
  ), {
    session$setInputs(use_sample_data = 1)
    expect_match(output$validation_summary$html, "must be proportions above 0", fixed = TRUE)
    table_json <- paste(as.character(output$validation_table), collapse = "")
    expect_match(table_json, "<th>Check<", fixed = TRUE)

    shiny.i18n::update_lang("fr", session)
    session$userData$language_version(1)
    session$flushReact()
    expect_match(output$validation_summary$html, "doit contenir des proportions", fixed = TRUE)
    expect_match(output$validation_summary$html, "Échecs des contrôles", fixed = TRUE)
    # Rows are sent separately (server-side table); headers and pager are here
    table_json <- paste(as.character(output$validation_table), collapse = "")
    expect_match(table_json, "<th>Contrôle<", fixed = TRUE)
    expect_match(table_json, "Précédent", fixed = TRUE)
    expect_match(table_json, "Affichage de _START_", fixed = TRUE)

    shiny.i18n::update_lang("es", session)
    session$userData$language_version(2)
    session$flushReact()
    expect_match(output$validation_summary$html, "debe contener proporciones", fixed = TRUE)
  }))

  bad_cm <- cm_panama[, 1:3]
  quietly(shiny::testServer(validation_module, args = list(
    id = "childmort", df = bad_cm, validation = validateData_childmort,
    templates = CHILDMORT_MESSAGE_TEMPLATES
  ), {
    session$setInputs(use_sample_data = 1)
    shiny.i18n::update_lang("es", session)
    session$userData$language_version(1)
    session$flushReact()
    expect_match(output$validation_summary$html, "Faltan columnas obligatorias: CD", fixed = TRUE)
  }))
})

test_that("chart hover text uses the words passed by the module", {
  est <- run_childmort(cm_panama, date_obs = 1976)$estimates
  p <- childmort_plot(est, hover_labels = list(women = "Femmes", date = "Date"))
  expect_true(all(grepl("Femmes 15-19", p$data$hover[1], fixed = TRUE)))
  expect_match(childmort_plot(est)$data$hover[1], "Women 15-19", fixed = TRUE)
  or_est <- run_orphanhood(or_iraq, mac = 28, date_obs = 1997.79)$estimates
  p2 <- orphanhood_plot(or_est, hover_labels = list(respondents = "Encuestados", date = "Fecha"))
  expect_match(p2$data$hover[1], "Encuestados 5-9", fixed = TRUE)
  expect_match(p2$data$hover[1], "Fecha = ", fixed = TRUE)
})

test_that("validation table rows are translated", {
  results <- check_data_orphanhood(transform(or_iraq, NotOrphaned = NotOrphaned * 100))
  fr <- quietly(translated_validation_table(results, translator("fr"), ORPHANHOOD_MESSAGE_TEMPLATES))
  expect_named(fr, c("Contrôle", "Message", "Résultat"))
  expect_equal(fr[[1]][1], "colonnes requises")
  failed <- fr[fr[[3]] == "Échec", ]
  expect_equal(nrow(failed), 1)
  expect_match(failed[[2]][1], "^NotOrphaned doit contenir des proportions")
  expect_equal(unique(fr[[3]]), c("Réussi", "Échec"))
  expect_equal(fr[[2]][1], "")
  es <- quietly(translated_validation_table(check_data_childmort(cm_panama[, 1:3]), translator("es"), CHILDMORT_MESSAGE_TEMPLATES))
  expect_equal(es[[2]], "Faltan columnas obligatorias: CD. Incluya Age, Women, CEB y CD o CS")
})
