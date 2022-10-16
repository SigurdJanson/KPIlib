# glob2rx <- function (pattern, trim.head = FALSE, trim.tail = TRUE) 
# {
#   if (!length(pattern)) 
#     return(character())
#   p <- gsub(".", "\\.", paste0("^", pattern, "$"), fixed = TRUE)
#   p <- gsub("?", ".", gsub("*", ".*", p, fixed = TRUE), fixed = TRUE)
#   p <- gsub("([^\\])\\(", "\\1\\\\(", p)
#   p <- gsub("([^\\])\\[", "\\1\\\\[", p)
#   p <- gsub("([^\\])\\{", "\\1\\\\{", p)
#   if (trim.tail) 
#     p <- sub(".*$", "", p, fixed = TRUE)
#   if (trim.head) 
#     p <- sub("^.*", "", p, fixed = TRUE)
#   p
# }



test_that("glob2rx: ", {
  expect_identical(glob2rx("abc.*"), "^abc\\.")
  expect_identical(glob2rx("*.t*"), "^.*\\.t")
  expect_identical(glob2rx("*.t??"), "^.*\\.t..$")
  expect_identical(glob2rx("*[*"), "^.*\\[")
})

test_that("glob2rx: trim.head works", {
  expect_identical(glob2rx("*.doc"), "^.*\\.doc$")
  expect_identical(glob2rx("*.doc", trim.head = FALSE), "^.*\\.doc$") # default
  expect_identical(glob2rx("*.doc", trim.head = TRUE), "\\.doc$")
})

test_that("glob2rx: trim.tail works", {
  expect_identical(glob2rx("a?b.*"), "^a.b\\.")
  expect_identical(glob2rx("a?b.*", trim.tail = TRUE), "^a.b\\.") # default
  expect_identical(glob2rx("a?b.*", trim.tail = FALSE), "^a.b\\..*$")
})

test_that("glob2rx", {
  expect_identical(glob2rx("¯\\(°_o)/¯"), "^¯\\(°_o)/¯$")
})