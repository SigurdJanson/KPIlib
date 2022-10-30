
test_that("or operator works (both vectors valid)", {
  v1 <- c(TRUE, TRUE, FALSE, FALSE)
  v2 <- c(TRUE, FALSE, TRUE, FALSE)
  expect_identical(lg(v1, v2, `|`), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(lg(v2, v1, `|`), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("and operator works (both vectors valid)", {
  v1 <- c(TRUE, TRUE, FALSE, FALSE)
  v2 <- c(TRUE, FALSE, TRUE, FALSE)
  # default
  expect_identical(lg(v1, v2), c(TRUE, FALSE, FALSE, FALSE))
  # v1 - v2
  expect_identical(lg(v1, v2, `&`), c(TRUE, FALSE, FALSE, FALSE))
  # v2 - v1
  expect_identical(lg(v2, v1, `&`), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("non-logicals are ignored", {
  v1 <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(lg(v1, NULL, `|`), v1)
  expect_identical(lg(NULL, v1, `|`), v1)
  expect_identical(lg(v1, 1:4, `|`), v1)
  expect_identical(lg(1:4, v1, `|`), v1)
})

test_that("vectors with NA are ignored", {
  v1 <- c(TRUE, TRUE, FALSE, FALSE)
  v2 <- c(TRUE, FALSE, TRUE, FALSE)
  v2[sample.int(4, 1L)] <- NA
  expect_identical(lg(v1, v2, `|`), v1)
  expect_identical(lg(v2, v1, `|`), v1)
})

test_that("empty vectors are ignored", {
  v1 <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(lg(v1, logical(), `|`), v1)
  expect_identical(lg(logical(), v1, `|`), v1)
})


