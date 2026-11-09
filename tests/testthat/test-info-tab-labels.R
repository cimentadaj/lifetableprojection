module_ids <- c("heaping", "smoothing", "graduation", "odap", "childmort", "orphanhood")

test_that("every module builds its info tabs with translatable labels", {
  i18n <- usei18n_local()
  mods <- get_app_modules(i18n)
  for (id in module_ids) {
    html <- as.character(mods[[id]]$ui)
    expect_match(html, sprintf('id="%s-info_tab_upload_label"', id), fixed = TRUE)
    expect_match(html, sprintf('id="%s-info_tab_docs_label"', id), fixed = TRUE)
  }
})

shared_context_module <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    session$userData$language_version <- shiny::reactiveVal(0)
    i18n <- usei18n_local()
    create_shared_data_context(id, input, output, session, i18n)
  })
}

test_that("info tab labels follow the selected language", {
  shiny::testServer(shared_context_module, args = list(id = "childmort"), {
    expect_match(output$info_tab_upload_label$html, "Upload Instructions")
    expect_match(output$info_tab_docs_label$html, "Method Documentation")

    shiny.i18n::update_lang("fr", session)
    session$userData$language_version(1)
    session$flushReact()
    expect_match(output$info_tab_upload_label$html, "Instructions de T")
    expect_match(output$info_tab_docs_label$html, "Documentation des M")

    shiny.i18n::update_lang("es", session)
    session$userData$language_version(2)
    session$flushReact()
    expect_match(output$info_tab_upload_label$html, "Instrucciones de Carga")
  })
})
