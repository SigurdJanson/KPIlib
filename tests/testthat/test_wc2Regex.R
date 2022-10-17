
# =================


test_that("wc2Regex: ", { # taken from glob2rx()
  expect_identical(wc2Regex("abc*"), "^.*abc.*$")
  expect_identical(wc2Regex("abc*."), "^.*abc.*\\\\..*$")
  expect_identical(wc2Regex("*.t*"), "^.*\\\\.t.*$")
  expect_identical(wc2Regex("*.t??t"), "^.*\\\\.t..t.*$")
  expect_identical(wc2Regex("*[*"), "^.*\\[.*$")
})

test_that("Multiple spaces are removed", {
  expect_identical(wc2Regex("abc *"), "^.*abc.*$")
  expect_identical(wc2Regex("abc  *"), "^.*abc.*$")
  expect_identical(wc2Regex(" abc  *"), "^.*abc.*$")
  expect_identical(wc2Regex("  abc  *"), "^.*abc.*$")
  expect_identical(wc2Regex("  abc  *  "), "^.*abc.*$")
  expect_identical(wc2Regex("abc cdf"), "^.*abc|cdf.*$")
  expect_identical(wc2Regex("abc  cdf"), "^.*abc|cdf.*$")
  expect_identical(wc2Regex("  abc  cdf  "), "^.*abc|cdf.*$")
})


test_that("wc2Regex handles multiple leading/trailing wild cards", {
  expect_identical(wc2Regex("*x"), "^.*x.*$")
  expect_identical(wc2Regex("x*"), "^.*x.*$")
  expect_identical(wc2Regex("*x*"), "^.*x.*$")
  expect_identical(wc2Regex("?x*"), "^.*x.*$")
  
  expect_identical(wc2Regex("**x"), "^.*x.*$")
  expect_identical(wc2Regex("x**"), "^.*x.*$")
  expect_identical(wc2Regex("*x**"), "^.*x.*$")
  expect_identical(wc2Regex("**x*"), "^.*x.*$")
  expect_identical(wc2Regex("**x**"), "^.*x.*$")
  
  expect_identical(wc2Regex("?x?"), "^.*x.*$")
  expect_identical(wc2Regex("?*x*?"), "^.*x.*$")
  expect_identical(wc2Regex("??x??"), "^.*x.*$")
  expect_identical(wc2Regex("*?x?*"), "^.*x.*$")
  
  expect_identical(wc2Regex("*?*?*?x*?*?*?"), "^.*x.*$")
})


# test_that("wc2Regex handles attempts to escape wild cards", {
#   expect_identical(wc2Regex("\\*"), "^.*\\\\.*$")
# })


# =================

Data <- data.frame(
  Input = c(
    c("pen?tration", "M*.", "rate", "\"rate\"", "(TWA)", "penetration penatration", "rate TWA", "TWA rate"),
    c("M.*\\.", "\\(TWA\\)", "Member.s.*\\([A-Z]{3}\\)", "pena?tration", "pena*tration")),
  WildcardOr  = c(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 
    c(FALSE, FALSE, FALSE, FALSE, TRUE)),
  WildcardAnd = c(
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), 
    c(FALSE, FALSE, FALSE, FALSE, TRUE)),
  RegEx = c(
    c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), 
    c(TRUE, TRUE, TRUE, TRUE, TRUE))
)

Title <- "Member's giftedness \\ penatration \"rate\" per Total Worship Attendance (TWA)."

test_that("wc2Regex: WildCard", {
  IgnoreCase <- TRUE
  
  for (Expected in Data[c("WildcardOr", "WildcardAnd")]) { # , "RegEx"
    UseOrOperator <- all(Expected == Data$WildcardOr)
      
    for (i in seq_along(Data$Input)) {
      inpSearchString <- Data$Input[i]

      SearchString <- wc2Regex(inpSearchString, OR = UseOrOperator)
      Result <- grepl(SearchString, Title, fixed = FALSE, ignore.case = IgnoreCase, perl = TRUE)
      #cat(inpSearchString, "->", SearchString, "->", Result, "\n")
      expect_identical(Result, Expected[i], expected.label = inpSearchString)
    }
  }
})


