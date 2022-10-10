#
# S3 Class for "Tag" Strings
#
# A string may contain tags separated by comma.

as.tagstr <- function(x) {
  class(x) <- "tagstr"
  attr(x, "separator") <- ","
  return(x)
}

#`%in%.tagstr` <- function()

  
#' `%isin%`
#'
#' @param x 
#' @param t 
#' @details The function does not test whether x is actually a `tagstr` object.
#' Hence, any string can be used as argument.
#' @return
#' @export
#'
#' @examples
`%isin%` <- function(x, t) {
  if (!is.null(attr(x, "separator")) && attr(x, "separator") != ",") 
    stop("Other separators than 'comma' are not supported")
  
  if (length(x) > 0) {
    x <- paste0(r"{(?<=,|^)\s*}", x, r"{\s*(?=,|$)}")
    if (length(x) > 1) {
      sapply(x, \(y) grepl(y, t, ignore.case = TRUE, perl = TRUE))
    } else {
        grepl(x, t, ignore.case = TRUE, perl = TRUE)
    }
  } else {
    stop("Invalid argument 'x'")
  }
}


length.tagstr <- function(x) {
  sep <- attr(x, "separator")
  if (is.null(sep)) sep <- ","
  strsplit(x, sep) |>
    lengths()
}



# "a" %isin% c("a, b, c", "b, c, d", "c, a")
# "a" %isin% c("ab, c", "b, c, d", "c, a")
# "ab" %isin% c("ab, c", "b, c, d", "c, a")
# "c, d" %isin% c("ab, c", "b, c, d", "c, a")

# c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a")
# apply(c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a"), 1L, any)

