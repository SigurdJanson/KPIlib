
# as.tagstr() ---------
test_that("a single string is correctly converted to 'tagstr'", {
  expect_type(
    as.tagstr("design system"), "character"
  )
  expect_s3_class(
    as.tagstr("design system"), "tagstr"
  )
  expect_match(as.tagstr("design system"), "design system")
  
  expect_type(
    as.tagstr("design system, design, operations"), "character"
  )
  expect_s3_class(
    as.tagstr("design system, design, operations"), "tagstr"
  )
  expect_match(
    as.tagstr("design system, design, operations"), 
    "design system,design,operations"
  )
})


test_that("multiple strings are correctly converted to 'tagstr'", {
  # Vector with 2 string
  expect_type(
    as.tagstr(c("design system", "design")), "character"
  )
  expect_s3_class(
    as.tagstr(as.tagstr(c("design system", "design"))), "tagstr"
  )
  expect_match(as.tagstr(c("design system", "design")), "design system,design")
  # Vector with 3 strings
  expect_type(
    as.tagstr(c("design system", "design", "operations")), "character"
  )
  expect_s3_class(
    as.tagstr(c("design system", "design", "operations")), "tagstr"
  )
  expect_match(
    as.tagstr(c("design system", "design", "operations")), 
    "design system,design,operations"
  )
  
  # Vector with 2 strings but 4 tags
  expect_type(
    as.tagstr(c("design system, styleguide", "design, operations")), "character"
  )
  expect_s3_class(
    as.tagstr(c("design system, styleguide", "design, operations")), "tagstr"
  )
  expect_match(
    as.tagstr(c("design system, styleguide", "design, operations")), 
    "design system,styleguide,design,operations"
  )
})


test_that("duplicate tags are being eliminated", {
  expect_identical(
    as.tagstr("design system,styleguide, guideline, styleguide"),
    as.tagstr("design system,styleguide, guideline"))
  expect_identical(
    as.tagstr(c("design system,styleguide", "guideline, styleguide")),
    as.tagstr("design system,styleguide, guideline"))
})



test_that("abnormal strings are fixed", {
  expect_match(as.tagstr(" , ,design system, , "), "design system", fixed = TRUE)
  expect_match(
    as.tagstr("design system, ,design, ,operations  ,,  ,"), 
    "design system,design,operations"
  )
})



#
#
#
# + (add) ----------------------
test_that("tags are added", { 
  expect_identical(
    as.tagstr("design system, design, operations") + "newtag", 
    as.tagstr("design system,design,operations,newtag")
  )
  expect_identical(
    as.tagstr("design system, design, operations") + "newtag , other tag", 
    as.tagstr("design system,design,operations,newtag,other tag")
  )
  expect_identical(
    as.tagstr("design system, design, operations") + c("newtag", "other"), 
    as.tagstr("design system,design,operations,newtag,other")
  )
  
  expect_identical(
    as.tagstr("design system, design, operations") + c("new,tag", "other,tag"), 
    as.tagstr("design system,design,operations,new,tag,other,tag")
  )
})



test_that("only tagstr objects are allowed for additions", {
  expect_error(
    `+.tagstr`(
      list(
        as.tagstr("design system, design, operations"),
        as.tagstr("styleguide, award, guideline")
      ), 
      "newtag"
    ),
    "x must be of type 'tagstr'"
  )
})



test_that("existing tags are not duplicated", {
  expect_identical(
    as.tagstr("design system,design,operations") + "design system", 
    as.tagstr("design system,design,operations")
  )
  expect_identical(
    as.tagstr("design system,design,operations") + "design", 
    as.tagstr("design system,design,operations")
  )
  expect_identical(
    as.tagstr("design system,design,operations") + "operations", 
    as.tagstr("design system,design,operations")
  )
  expect_identical(
    as.tagstr("design system,design,operations") + "design system" + "operations", 
    as.tagstr("design system,design,operations")
  )
})


#
#
#
# %isin% --------------------------

test_that("%isin% works normally with 1-length vectors", {
  elements <- c("design system", "styleguide", "design", "operations")
  for (e in elements)
    expect_true(e %isin% as.tagstr("design system,styleguide,design,operations"))
  
  nonelements <- c("design  system", "guideline", "style guide", "ngised", "operaciones")
  for (e in nonelements)
    expect_false(e %isin% as.tagstr("design system,styleguide,design,operations"))
})

test_that("%isin% handles extra white space in first argument", {
  elements <- c("  design system ", " styleguide  ", " design ", "  operations  ")
  for (e in elements)
    expect_true(e %isin% as.tagstr("design system,styleguide,design,operations"))
})

test_that("%isin% works with multiple elemts in 'x'", {
  expect_identical(
    c("styleguide", "operations", "system design") %isin% as.tagstr("design system,styleguide,design,operations"),
    c(TRUE, TRUE, FALSE))
})

test_that("%isin% does not allow multi-tag strings", {
  expect_error("a,c" %isin% as.tagstr("design system,styleguide,design,operations"))
})


#
#
#
# length ----------------

test_that("length works", {
  expect_identical(length.tagstr("a,c"), 2L)
  expect_identical(length(as.tagstr("design system")), 1L)
  expect_identical(length(as.tagstr("design system,styleguide,design,operations")), 4L)
  
  # weird but possible
  expect_identical(
    length.tagstr(
      c(
        as.tagstr("design system,styleguide,design,operations"),
        "a,c"
      )
    ),
    c(4L, 2L))
})


#
#
#
# - (minus) -------------------------
test_that("minus works (length x & y == 1)", {
  # removal
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "ijk",
    as.tagstr("abc, def, uvw, xyz")
  )
  # No removal
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "ikk",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
  # No removal of a "startswith" string
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "ij",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )  
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "ab",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "xy",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
  # No removal of an "endswith" string
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "jk",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "bc",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "yz",
    as.tagstr("abc, def, ijk, uvw, xyz")
  )
})


test_that("minus works (length x == 1; length y > 1)", {
  # removal of 2
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - c("ijk", "def"),
    as.tagstr("abc, uvw, xyz")
  )
  # removal of 2
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - "ijk, def",
    as.tagstr("abc, uvw, xyz")
  )
  # 
  expect_identical(
    as.tagstr("abc, def, ijk, uvw, xyz") - c("ijk, ab, bc", "def"),
    as.tagstr("abc, uvw, xyz")
  )
})

test_that("minus only works with tagstr objects", {
  # no `tagstr` yields an error
  expect_error(
    `-.tagstr`(
      list(
        as.tagstr("abc, def, ijk, uvw, xyz"),
        as.tagstr("abc, def, ijk, uvw, xyz")
      ), c("ijk", "def")
    ), "x must be of type 'tagstr'"
  )
})