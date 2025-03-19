
test_that("identical lists yield TRUE", {
  # mixed lists of length 1
  v1 <- list(67, "a", TRUE, 0.1876)
  expect_true(listEqual(v1, 67, "a", TRUE, 0.1876))

  # mixed lists of length 1 incl. `NULL`
  v1 <- list(37, "a", NULL, TRUE, 0.1876)
  expect_true(listEqual(v1, 37, "a", NULL, TRUE, 0.1876))

  v1 <- list(NULL)
  expect_true(listEqual(v1, NULL))
  
  # mixed lists of incl. `NA`
  v1 <- list(1:3, NA_real_, "a", NA_integer_, TRUE, NA_character_, 0.1876)
  expect_true(listEqual(v1, 1:3, NA_real_, "a", NA_integer_, TRUE, NA_character_, 0.1876))
  
  # mixed list with differing length
  v1 <- list(99:12, letters[8:12], c(TRUE, FALSE), 0.48)
  expect_true(listEqual(v1, 99:12, letters[8:12], c(TRUE, FALSE), 0.48))
})



test_that("Empty lists yield TRUE", {
  expect_true(listEqual(list()))
  expect_true(listEqual(list(NULL), NULL))
})



test_that("NULL yields FALSE", {
  expect_false(listEqual(NULL, NULL)) # compares NULL == list(NULL)
  expect_false(listEqual(NULL, NULL, NULL)) # compares NULL == list(NULL, NULL)
})



test_that("Different lists yield FALSE", {
  # mixed lists of length 1
  v1 <- list(67, "a", TRUE, 0.1876)
  expect_false(listEqual(v1, 67, "b", TRUE, 0.1876))
  expect_false(listEqual(v1, 66, "a", TRUE, 0.1876))
  expect_false(listEqual(v1, 67, "a", TRUE, 0.1876001))
  
  # mixed list with differing length
  v1 <- list(99:12, letters[8:12], c(TRUE, FALSE), 0.48)
  expect_false(listEqual(v1, 99:12, letters[7:11], c(TRUE, FALSE), 0.48))
  
  # mixed list with differing length incl. a NULL element
  v1 <- list(67, "a", TRUE, 0.1876)
  expect_false(listEqual(v1, 67, "a", TRUE, 0.1876, NULL)) # NULL is last
  expect_false(listEqual(v1, 67, "a", NULL, TRUE, 0.1876)) # middle
  expect_false(listEqual(v1, NULL, 67, "a", TRUE, 0.1876)) # NULL is first
  
  # mixed list with differing length incl. a NA element
  v1 <- list(67, "a", TRUE, 0.1876)
  expect_false(listEqual(v1, 67, "a", TRUE, 0.1876, NA)) # NA is last
  expect_false(listEqual(v1, 67, "a", NA, TRUE, 0.1876)) # middle
  expect_false(listEqual(v1, NA, 67, "a", TRUE, 0.1876)) # NA is first
})


