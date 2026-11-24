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
  expect_error(mlt_table("CD_West", "female", "match", "e0", 15), "outside the range of the Coale-Demeny West tables")
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
