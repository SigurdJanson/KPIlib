#
# S3 Class for "Tag" Strings
#
# A string may contain tags separated by comma.

#' as.tagstr
#' 
#' Create a `tagstr` object out of a vector of strings.
#'
#' @param x A character vector
#' @details Any `NA` will be omitted. If any of the strings contains the separator
#' character these will be assumed to be multiple tags.
#' @return A `tagstr` object with all tags and strings collapsed into a character string
#' of vector length 1.
as.tagstr <- function(x) {
  tryCatch(x <- as.character(x), 
           error = function(e) stop("Cannot coerce 'x' to type 'tagstr'"))
  sep <- trimws(",") # hard-coded
  # Fix the string to desired pattern by removing white spaces
  pattern <- paste0("\\s*", sep, "\\s*") #--paste0("\\s*(", sep, ")\\s*")
  x <- gsub(pattern, sep, x)
  # Remove leading/trailing separators
  pattern <- paste0("^", sep, "*|", sep, "*$")
  x <- gsub(pattern, "", x)
  # Remove duplicate separators ",{2,}"
  pattern <- paste0(sep, "{2,}")
  x <- gsub(pattern, sep, x)
  # Collapse into one string
  x <- paste0(x, collapse = sep)
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
#' @details The function does not test whether x is actually a `tagstr` object.
#' Hence, any string can be used as argument.
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
      return(as.tagstr(x))
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
  
}


# "a" %isin% c("a, b, c", "b, c, d", "c, a")
# "a" %isin% c("ab, c", "b, c, d", "c, a")
# "ab" %isin% c("ab, c", "b, c, d", "c, a")
# "c, d" %isin% c("ab, c", "b, c, d", "c, a")

# c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a")
# apply(c("a", "x") %isin% c("a, b, c", "b, c, d", "c, a"), 1L, any)

