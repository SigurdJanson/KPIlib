test_that("<a>...</a> is stripped away", {
  expect_identical(stripHTML_a("This is a <a>link</a>"), "This is a link")
})

test_that("<a href=\"\">...</a> is stripped away", {
  expect_identical(stripHTML_a("This is a <a href\"her goes the link\">link</a>"), "This is a link")
})

test_that("<a></a> is being removed regardless of it's attributes", {
  expect_identical(stripHTML_a("This is a <a xref=\"no reference\">link</a>"), 
                   "This is a link")
  
  expect_identical(stripHTML_a("This is a <a xref=\"no reference\" disabled title=\"a title\" >link</a>"), 
                   "This is a link")
})

test_that("Several <a>...</a> are stripped away correctly", {
  expect_identical(stripHTML_a("This is a <a>link</a>, and another <a href=\"1\">One</a>. And - you know - a <a href\"www.333.space/go\">Number Three link</a>."), 
                   "This is a link, and another One. And - you know - a Number Three link.")
})


test_that("<a/> is not touched", {
  expect_identical(stripHTML_a("This is <a/> <a>link</a>"), 
                   "This is <a/> link")
})

test_that("<.../> (that is not a link) is not touched", {
  expect_identical(stripHTML_a("This is <a/> <abbr>link</abbr>..."), 
                   "This is <a/> <abbr>link</abbr>...")
})
