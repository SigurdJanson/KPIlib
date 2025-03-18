

#' escapeRegex
#' 
#' @return Escapes characters in a string so that it can be used 
#' as regular expression.
#' @noRd
#' @see Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}


#' wc2Regex
#' 
#' Change a string from a wildcard syntax into regular expression syntax.
#' @param x A string
#' @param OR Process several space-separated words with `OR` operator (`TRUE`, default)
#' or with `AND` operator (`FALSE`).
#' @return A string that works as regular expression and yields the same result
#' as a wildcard search would have
#' @noRd
wc2Regex <- function(x, OR = TRUE) {
  # Remove leading, trailing and repeated white spaces
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE)
  # Remove surrounding spaces around an asterisk
  x <- gsub("\\s*(\\*)\\s*", "\\1", x)
  
  if (OR) {
    # Replace leading/Trailing wild cards by single ones
    x <- gsub("^[\\*\\?]*(.*?)[\\*\\?]*$", "*\\1*", x)
    # 'escapeRegex' without wild card characters
    x <- gsub("([.|()\\^{}+$]|\\[|\\])", "\\\\\\1", x)
    # Make it an 'OR' expression
    x <- gsub(" ", "|", x)
    x <- glob2rx(x, trim.head = FALSE, trim.tail = FALSE)
  }
  else { # AND - use "(?=.*s1)(?=.*s2)"
    # Remove leading/Trailing wild cards
    x <- gsub("^[\\*\\?]*(.*?)[\\*\\?]*$", "\\1", x)
    # 'escapeRegex' without wild card characters
    x <- gsub("([.|()\\^{}+$]|\\[|\\])", "\\\\\\1", x)
    # Make it an 'AND' expression
    Operands <- strsplit(x, " ") |> 
      unlist() |>
      glob2rx(trim.head = TRUE, trim.tail = FALSE)
    Operands <- sub("^\\^(.*)\\$$", "\\1", Operands)
    x <- paste0("(?=.*", Operands, ")", collapse = "")
  }
  x
}



#' @Note This function does not check the length of Col1-3 or any 
#' other preconditions.
TextFilterKpi <- function(SearchString, Col1, Col2, Col3,
                          Options = list(Regex = FALSE, IgnoreCase = TRUE, MatchAny = FALSE)) {
  
  if (!Options$Regex) { # standard search
    SearchString <- wc2Regex(SearchString, Options$MatchAny)
  }
  
  RowFilter <- 
    grepl(SearchString, Col1, 
          fixed = FALSE, ignore.case = Options$IgnoreCase, perl = TRUE) | 
    grepl(SearchString, Col2, 
          fixed = FALSE, ignore.case = Options$IgnoreCase, perl = TRUE) | 
    grepl(SearchString, Col3, 
          fixed = FALSE, ignore.case = Options$IgnoreCase, perl = TRUE)
  
  return(RowFilter)
}






CategoryFilterKpi <- function(SearchCategories, Col) {

  CategoryFilter <- escapeRegex(SearchCategories)
  if (length(CategoryFilter) > 1)
    RowFilter <- apply(CategoryFilter %isin% Col, 1L, any)
  else
    RowFilter <- CategoryFilter %isin% Col
  
  return(RowFilter)
}