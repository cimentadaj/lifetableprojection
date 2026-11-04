two_regions <- data.frame(
  Region = rep(c("North", "South"), each = 3),
  Age = rep(c(0, 1, 5), 2),
  Deaths = 1:6
)

# A minimal module that wires handle_group_selection_modal() to reactive values
# the test can inspect.
grouping_test_module <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactiveVal(df)
    passed <- shiny::reactiveVal(FALSE)
    vars <- shiny::reactiveVal(character(0))
    handle_group_selection_modal(input, output, session, data, passed, vars,
                                 usei18n_local())
  })
}

modal_error_text <- function(output) {
  paste(as.character(output$modal_error_message$html), collapse = "")
}

test_that("Confirm without columns or the no-grouping tick is refused", {
  shiny::testServer(grouping_test_module, args = list(df = two_regions), {
    session$setInputs(id_columns = NULL, skip_grouping = FALSE)
    session$setInputs(confirm_column_selection = 1)
    expect_false(passed())
    expect_false(".id" %in% names(data()))
    expect_match(modal_error_text(output), "Select at least one grouping column")
  })
})

test_that("Confirm with a grouping column tags one group per region", {
  shiny::testServer(grouping_test_module, args = list(df = two_regions), {
    session$setInputs(id_columns = "Region", skip_grouping = FALSE)
    session$setInputs(confirm_column_selection = 1)
    expect_true(passed())
    expect_equal(length(unique(data()$.id)), 2)
    expect_equal(vars(), "Region")
  })
})

test_that("Ticking no grouping still puts all rows in one group", {
  shiny::testServer(grouping_test_module, args = list(df = two_regions[1:3, ]), {
    session$setInputs(id_columns = NULL, skip_grouping = TRUE)
    session$setInputs(confirm_column_selection = 1)
    expect_true(passed())
    expect_equal(unique(data()$.id), 1L)
  })
})

