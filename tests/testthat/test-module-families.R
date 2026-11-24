# Landing page grouped into module families (MODULE_FAMILIES)

test_that("every module belongs to exactly one family", {
  modules <- names(get_app_modules(usei18n_local()))
  in_families <- unlist(lapply(MODULE_FAMILIES, `[[`, "modules"), use.names = FALSE)
  expect_setequal(in_families, modules)
  expect_false(anyDuplicated(in_families) > 0)
  expect_setequal(names(MODULE_NAMES), modules)
  # The names listed on the family cards are the modules' own names
  mods <- get_app_modules(usei18n_local())
  for (id in modules) expect_equal(unname(MODULE_NAMES[[id]]), mods[[id]]$name, label = id)
})

test_that("the landing page shows the family menu, with each family's modules in its section", {
  # untheme warns about its own header and footer HTML files
  html <- suppressWarnings(as.character(app_ui(NULL)))
  expect_match(html, 'id="family_menu"', fixed = TRUE)
  for (family_id in names(MODULE_FAMILIES)) {
    expect_match(html, paste0('id="family_card_', family_id, '"'), fixed = TRUE)
    expect_match(html, paste0('id="family_section_', family_id, '"'), fixed = TRUE)
  }
  # Module cards sit inside their family's section, in the family's order
  section <- function(family_id) {
    start <- regexpr(paste0('id="family_section_', family_id, '"'), html, fixed = TRUE)
    rest <- substring(html, start)
    nxt <- regexpr('id="family_section_', substring(rest, 2), fixed = TRUE)
    if (nxt > 0) substring(rest, 1, nxt) else rest
  }
  for (family_id in names(MODULE_FAMILIES)) {
    s <- section(family_id)
    pos <- vapply(MODULE_FAMILIES[[family_id]]$modules, function(id) {
      regexpr(paste0('id="', id, '_module_card"'), s, fixed = TRUE)
    }, numeric(1))
    expect_true(all(pos > 0), label = family_id)
    expect_false(is.unsorted(pos), label = family_id)
  }
})

test_that("family cards and headers have their buttons and translate", {
  en <- usei18n_local()
  card <- as.character(create_family_card("indirect", en))
  expect_match(card, 'id="goto_family_indirect"', fixed = TRUE)
  expect_match(card, "Open (3 modules)", fixed = TRUE)
  expect_match(card, "Adult Mortality from Intercensal Survival", fixed = TRUE)
  header <- as.character(create_family_header("lifetables", en))
  expect_match(header, 'id="back_to_families_lifetables"', fixed = TRUE)

  fr <- usei18n_local()
  fr$set_translation_language("fr")
  card_fr <- suppressWarnings(as.character(create_family_card("indirect", fr)))
  expect_match(card_fr, "Estimation indirecte de la mortalit", fixed = TRUE)
  expect_match(card_fr, "Ouvrir (3 modules)", fixed = TRUE)

  tr <- jsonlite::fromJSON(app_sys("extdata", "translation.json"))$translation
  wanted <- c(unlist(lapply(MODULE_FAMILIES, function(f) c(f$name, f$description))),
              unname(MODULE_NAMES), "Open (%d modules)", "← All families")
  expect_equal(setdiff(wanted, tr$en), character(0))
  hits <- tr[tr$en %in% wanted, ]
  expect_true(all(nzchar(hits$fr) & nzchar(hits$es)))
})
