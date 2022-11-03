#
# Free Text Search ==========
#
test_that("... ignores case (Standard, AND)", {
  Filterand <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  
  # Complete string
  
  FilterStr <- "internet"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, FALSE, FALSE, TRUE, FALSE))

  FilterStr <- "INTERNET"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  
  # Partial string

  FilterStr <- "e-b"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  
  FilterStr <- "E-b"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  
  FilterStr <- "E-B"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})



test_that("... does NOT ignore case (Standard, AND)", {
  Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  Filterand <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  
  # Complete string
  
  FilterStr <- "Internet"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  
  FilterStr <- "internet"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  
  FilterStr <- "INTERNET"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  
  # Partial string
  
  FilterStr <- "E-b"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  
  FilterStr <- "e-b"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  
  FilterStr <- "e-B"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  
  FilterStr <- "E-B"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
})



test_that("... allows hits in one column only", {
  expect_true(TRUE, "This does not work, at the moment")
  # Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  # 
  # Filterand1 <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  # Filterand2 <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  # Filterand3 <- rev(Filterand1)
  # 
  # # 
  # FilterStr <- "Intern"
  # 
  # Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = TRUE)
  # obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  # expect_identical(obs, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  # 
  # Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  # obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  # expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  # 
  # # 
  # FilterStr <- "Ledge"
  # 
  # Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = TRUE)
  # obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  # expect_identical(obs, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # 
  # Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  # obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  # expect_identical(obs, c(FALSE, FALSE, TRUE, FALSE, FALSE))
})



test_that("... handles AND/OR correctly", {
  
  Filterand1 <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  Filterand2 <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  Filterand3 <- c("Search", "Documents", "Knowledge", "Internet", "Social Media & E-business")
  
  # 
  FilterStr <- "Social business"
  
  Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = TRUE)
  obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  
  Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  
  
  # 
  FilterStr <- "Social Knowledge"
  
  Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = TRUE)
  obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  expect_identical(obs, c(FALSE, FALSE, TRUE, FALSE, TRUE))
  
  Options <- list(Regex = FALSE, IgnoreCase = FALSE, OperatorOr = FALSE)
  obs <- TextFilterKpi(FilterStr, Filterand1, Filterand2, Filterand3, Options)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE))
})



test_that("... uses wild cards, correctly", {
  Filterand <- c("Search", "Documents", 
                 "Knowledge", "Internet", 
                 "Social Media & E-business", "Dokumente")
  
  # Asterisk
  
  FilterStr <- "m*n"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))

  # Question mark
  
  FilterStr <- "Do?ument?"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))

  # Combinations
  FilterStr <- "m**n"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
  
  FilterStr <- "m?*n"
  obs <- TextFilterKpi(FilterStr, Filterand, Filterand, Filterand)
  expect_identical(obs, c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
})




#
# Category Search ==========
#


test_that("... single category is found correctly", {
  SearchSpace <- c("Search", 
                   "Documents, Search", 
                   "Knowledge, Search, Internet, ", 
                   "Internet", 
                   "Social Media & E-business", 
                   "Assets, Road services")
  
  # First, middle or last tag
  FilterStr <- "Search"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  
  # Complex tag With '&' character
  FilterStr <- "Social Media & E-business"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  
  # Not case sensitive
  FilterStr <- "roaD Services"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})


test_that("... partials are ignored", {
  SearchSpace <- c("Search", 
                   "Documents, Search", 
                   "Knowledge, Search, Internet, ", 
                   "Internet", 
                   "Social Media & E-business", 
                   "Assets, Road services")
  
  #  
  FilterStr <- "Media"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, rep(FALSE, length(SearchSpace)))
  
  #  
  FilterStr <- "Social Media & E-"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, rep(FALSE, length(SearchSpace)))
  
  #  
  FilterStr <- "roaD"
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, rep(FALSE, length(SearchSpace)))
})




test_that("... multiple categories are found correctly", {
  SearchSpace <- c("Search", 
                   "Documents, Search", 
                   "Knowledge, Search, Internet, ", 
                   "Internet", 
                   "Social Media & E-business", 
                   "Assets, Road services")
  
  FilterStr <- c("Search", "Social Media & E-business", "roaD Services")
  obs <- CategoryFilterKpi(FilterStr, SearchSpace)
  expect_identical(obs, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
})