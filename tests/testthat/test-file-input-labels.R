fake_session <- function() {
  sent <- new.env()
  list(
    sent = sent,
    sendCustomMessage = function(type, message) {
      sent$type <- type
      sent$message <- message
    }
  )
}

test_that("file upload texts are sent in the current language", {
  for (lang in c("en", "fr", "es")) {
    i18n <- usei18n_local()
    i18n$set_translation_language(lang)
    session <- fake_session()
    send_file_input_labels(session, i18n)
    expect_equal(session$sent$type, "file-input-labels")
    expect_named(session$sent$message, c("browse", "complete", "finishing", "placeholder"))
    expected_browse <- c(en = "Browse...", fr = "Parcourir...", es = "Explorar...")[[lang]]
    expect_equal(session$sent$message$browse, expected_browse)
  }
})

test_that("every file upload text has French and Spanish translations", {
  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  strings <- c("Browse...", "Upload complete", "Finishing upload", "no file selected")
  hits <- tr[tr$en %in% strings, ]
  expect_setequal(hits$en, strings)
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es) & hits$fr != hits$en))
})

test_that("the translation script is part of the app page", {
  expect_true(file.exists(app_sys("app/www/file_input_i18n.js")))
  html <- paste(as.character(app_ui(NULL)), collapse = "")
  expect_match(html, 'addCustomMessageHandler("file-input-labels"', fixed = TRUE)
})
