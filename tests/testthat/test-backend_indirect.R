two_tables <- function(df, scale = 1) {
  if (any(df$value < 0)) stop("negative value")
  list(
    main = data.frame(x = df$x, y = df$value * scale),
    extra = data.frame(total = sum(df$value))
  )
}

grouped <- data.frame(
  .id = rep(c(1L, 2L), each = 3),
  .id_label = rep(c("North", "South"), each = 3),
  x = rep(1:3, 2),
  value = c(1, 2, 3, 10, 20, 30)
)

test_that("run_by_group binds every table across groups and keeps .id first", {
  res <- run_by_group(grouped, two_tables, scale = 2)
  expect_named(res, c("main", "extra", "group_errors"))
  expect_equal(names(res$main)[1], ".id")
  expect_equal(res$main$.id, rep(c(1L, 2L), each = 3))
  expect_equal(res$main$y, c(2, 4, 6, 20, 40, 60))
  expect_equal(res$extra$total, c(6, 60))
  expect_equal(nrow(res$group_errors), 0)
})

test_that("run_by_group attaches .id_label to every table", {
  res <- run_by_group(grouped, two_tables)
  expect_equal(res$main$.id_label, rep(c("North", "South"), each = 3))
  expect_equal(res$extra$.id_label, c("North", "South"))
})

test_that("run_by_group treats data without .id as one group", {
  res <- run_by_group(grouped[1:3, c("x", "value")], two_tables)
  expect_equal(unique(res$main$.id), 1L)
  expect_false(".id_label" %in% names(res$main))
  expect_equal(nrow(res$group_errors), 0)
})

test_that("an error in one group is reported and the others still compute", {
  bad <- grouped
  bad$value[bad$.id == 2][1] <- -1
  res <- run_by_group(bad, two_tables)
  expect_equal(unique(res$main$.id), 1L)
  expect_equal(res$group_errors$.id, "2")
  expect_equal(res$group_errors$message, "negative value")
  expect_type(res$group_errors$.id, "character")
})

test_that("groups failing the check are skipped with the check messages", {
  check_small <- function(df) {
    fail <- any(df$value > 15)
    data.frame(
      check = c("check size", "check other"),
      message = c(if (fail) "value too large" else NA_character_, NA_character_),
      pass = c(if (fail) "Fail" else "Pass", "Pass"),
      stringsAsFactors = FALSE
    )
  }
  res <- run_by_group(grouped, two_tables, check = check_small)
  expect_equal(unique(res$main$.id), 1L)
  expect_equal(res$group_errors$message, "value too large")
})

test_that("when every group fails there are no tables, only errors", {
  bad <- grouped
  bad$value <- -1
  res <- run_by_group(bad, two_tables)
  expect_null(res[["main"]])
  expect_equal(res$group_errors$.id, c("1", "2"))
})

test_that("run_by_group output never partially matches an `error` element", {
  res <- run_by_group(grouped, two_tables)
  expect_null(res$error)
  expect_false(any(startsWith(names(res), "error")))
})
