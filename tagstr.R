#
# S3 Class for "Tag" Strings
#
# A string may contain tags separated by comma.
#

#' as.tagstr
#' 
#' Create a `tagstr` object out of a vector of strings.
#'
#' @param x A character vector
#' @details Any `NA` will be omitted. If any of the strings contains the separator
#' character these will be assumed to be multiple tags. Vectors will be collapsed
#' into a single `tagstr` object.
#' @return A `tagstr` object with all tags and strings collapsed into a character string
#' of vector length 1.
as.tagstr <- function(x) {
  tryCatch(x <- as.character(x), 
           error = function(e) stop("Cannot coerce 'x' to type 'tagstr'"))
  sep <- trimws(",") # !!hard-coded
  # split and remove duplicates and empty tags
  xn <- strsplit(x, sep) |> unlist() |> trimws() |> unique()
  xn <- grep("^\\s*$", xn, value = TRUE, invert = TRUE)
  # (Re-) Collapse into one string
  x <- paste0(xn, collapse = sep)
  # Classify
  class(x) <- "tagstr"
  attr(x, "separator") <- sep
  return(x)
}



  
#' `%isin%`
#'  
#' Checks occurrence of a string in a tag-containing string (`tagstr`).
#'
#' @param x a character vector
#' @param t A `tagstr` object
#' @details 
#' @note The function does not test whether x is actually a `tagstr` object.
#' Hence, any string can be used as argument while assuming the separator is a comma.
#' @return
#' @nord
`%isin%` <- function(x, t) {
  sep <- attr(x, "separator")
  if (is.null(sep)) sep <- ","
  
  if (length(x) > 0) {
    if (any(grepl(sep, x, fixed = TRUE))) stop("strings in 'x' can only contain a single tag")

    x <- trimws(x)
    x <- paste0(r"{(?<=}", sep, r"{|^)\s*}", x, r"{\s*(?=}", sep, r"{|$)}")
    if (length(x) > 1) {
      sapply(x, \(y) grepl(y, t, ignore.case = TRUE, perl = TRUE)) |> unname()
    } else {
      grepl(x, t, ignore.case = TRUE, perl = TRUE) |> unname()
    }
  } else {
    stop("Invalid argument 'x'")
  }
}


#' length.tagstr
#' 
#' Gives the number of tags in a tagged string.
#' 
#' @method length tagstr
#' @param x A `tagstr` object
#' @note The function does not test whether x is actually a `tagstr` object.
#' Hence, any string can be used as argument while assuming the separator 
#' is a comma. It will not check if the tags are not empty.
#' @return An integer with the number of tags in the `tagstr` object.
length.tagstr <- function(x) {
  sep <- attr(x, "separator")
  if (is.null(sep)) sep <- ","
  strsplit(x, sep) |>
    lengths()
}


`+.tagstr` <- function(x, y) {
  if (!is.character(y)) stop("y must be of type 'character' or 'tagstr'")
  if (!inherits(x, "tagstr")) stop("x must be of type 'tagstr'")

  sep <- attr(x, "separator")
  if (is.null(sep)) sep <- ","

  if (length(y) == 1L && !grepl(sep, y, fixed = TRUE)) {
    # case 1: y is a single string with a single tag
    if (y %isin% x)
      return(x)
    else
      return(as.tagstr(paste(x, y, sep = sep)))
  }
  # case 2: y is a single string with multiple tags
  # case 3: y is a vector of multiple strings with single/multiple tags
  paste(x, paste(y, collapse = sep), sep = sep, collapse = sep) |>
    as.tagstr()
}



`-.tagstr` <- function(x, y) {
  if (!is.character(y)) stop("y must be of type 'character' or 'tagstr'")
  if (!inherits(x, "tagstr")) stop("x must be of type 'tagstr'")
  
  sep <- attr(x, "separator")
  if (is.null(sep)) sep <- ","
  
  ally <- strsplit(y, ",") |> unlist() |> trimws()
  
  for (subtrahend in ally) {
    pattern <- paste0("((?<=^|", sep,")\\s*)(", subtrahend, ")(\\s*(?=", sep, "|$))")
    x <- gsub(pattern, "\\1\\3", x, perl = TRUE)
  }
  return(as.tagstr(x))
}


# "a" %isin% c("a, b, c", "b, c, d", "c, a")
# "a" %isin% c("ab, c", "b, c, d", "c, a")
# "ab" %isin% c("ab, c", "b, c, d", "c, a")
# "c, d" %isin% c("ab, c", "b, c, d", "c, a")

# c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a")
# apply(c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a"), 1L, any)

