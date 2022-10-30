
test_that("identical atomic vectors yield TRUE", {
  # integer
  v1 <- sample.int(10L)
  v2 <- v1
  expect_true(vecEqual(v1, force(v2)))
  
  # double
  v1 <- sample.int(10L) + 0.1321
  v2 <- v1
  expect_true(vecEqual(v1, force(v2)))
  
  # character
  v1 <- sample(letters, 10L)
  v2 <- v1
  expect_true(vecEqual(v1, force(v2)))
  
  # logical
  v1 <- sample(c(TRUE, FALSE), 10L, replace = TRUE)
  v2 <- v1
  expect_true(vecEqual(v1, force(v2)))
  
  # complex
  v1 <- complex(real = sample.int(10L), imaginary = sample.int(10L))
  v2 <- v1
  expect_true(vecEqual(v1, force(v2)))
})



test_that("unordered vectors still yield TRUE", {
  L <- 10L
  # integer
  v1 <- sample.int(L)
  v2 <- v1[sample.int(L)]
  expect_true(vecEqual(v1, v2))
  
  # double
  v1 <- sample.int(L) + 0.1321
  v2 <- v1[sample.int(L)]
  expect_true(vecEqual(v1, force(v2)))
  
  # character
  v1 <- sample(letters, L)
  v2 <- v1[sample.int(L)]
  expect_true(vecEqual(v1, v2))
  
  # logical
  v1 <- sample(c(TRUE, FALSE), L, replace = TRUE)
  v2 <- v1[sample.int(L)]
  expect_true(vecEqual(v1, v2))
  
  # complex
  v1 <- complex(real = sample.int(L), imaginary = sample.int(L))
  v2 <- v1[sample.int(L)]
  expect_true(vecEqual(v1, v2))
})



test_that("vectors with same elements but different length yield FALSE", {
  L <- 10L
  # integer
  v1 <- sample(1:2L, L, replace=TRUE)
  v2 <- c(v1, sample(v1, 1L))
  expect_false(vecEqual(v1, v2))
  
  # double
  v1 <- sample(1:2L, L, replace=TRUE) + 0.321
  v2 <- c(v1, sample(v1, 1L))
  expect_false(vecEqual(v1, v2))
  
  # character
  v1 <- sample(letters, 10L)
  v2 <- c(v1, sample(v1, 1L))
  expect_false(vecEqual(v1, v2))
})


test_that("weird values are correctly identified", {
  expect_true(vecEqual(integer(), logical()))
  expect_true(vecEqual(NULL, logical()))
})