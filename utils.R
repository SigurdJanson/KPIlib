# 'Constants' 
strEmptySearchResult <- "Search does not yield any results"
ShownTableLabels <- c(Title = "title", Description = "description", 
                      Direction = "direction", `Unit Type` = "unit", 
                      Tags = "tags", Domains = "domain")



#' Load the data from the files in "location"
readKpiData <- function(location) {
  x <- NULL
  for (.loc in location) {
    x2 <- read_json(.loc, simplifyVector = TRUE)
    x2$direction <- as.factor(x2$direction)
    x2$unit <- as.factor(x2$unit)
    x2$created_at <- as.POSIXct(x2$created_at)
    x2$updated_at <- as.POSIXct(x2$updated_at)
    if (is.null(x))
      x <- x2
    else
      x <- rbind(x, x2)
  }
  return(x)
}


#' filterKpiData
#' Filter the dataset according to pre-defined criteria.
#' * Is the flag `intervention_needed` set? 
#' @param x The data
#' @return A filtered instance of x
#' @noRd
filterKpiData <- function(x) {
  .Filter <- !sapply(x$intervention_needed, isTRUE)
  return(x[.Filter,])
}



mapField2Label <- function(x) {
  #ShownTableLabels
  names(ShownTableLabels[ShownTableLabels == x])
}



#' Maps units from the KPI data set to icons
#'
#' @param unit A character vector of units.
#'
#' @return A vector of icons
#' @noRd
mapUnit2Icon <- function(unit, addDiv = TRUE) {
  .icon <- switch (
    as.character(unit),
          Area     = icon("square-full", verify_fa = FALSE),
          Data     = icon("server", verify_fa = FALSE),
          Distance = icon("ruler-horizontal", verify_fa = FALSE),
          Energy   = icon("battery-bolt", verify_fa = FALSE),
          Money    = icon("money-bill", verify_fa = FALSE),
          Number   = icon("hashtag", verify_fa = FALSE),
          Percentage = icon("percent", verify_fa = FALSE),
          Ratio    = icon("divide", verify_fa = FALSE),
          Score    = icon("star-half-alt", verify_fa = FALSE),
          Time     = icon("stopwatch", verify_fa = FALSE),
          Volume   = icon("cube", verify_fa = FALSE),
          Weight   = icon("balance-scale", verify_fa = FALSE),
          icon("question", verify_fa = FALSE)
          #thermometer-half
  )
  #browser()
  if (addDiv) 
    return(tags$div(title=paste("Unit:", unit), .icon))
  else
    return(.icon)
}



#' Maps directions from the KPI data set to icons
#'
#' @param unit A character vector of directions.
#'
#' @return A vector of icons
#' @noRd
mapDirection2Icon <- function(dir, addDiv = TRUE) {
  .icon <- switch (
    as.character(dir),
    Minimize = icon("arrow-down", verify_fa = FALSE),
    Maximize = icon("arrow-up", verify_fa = FALSE),
    Range    = icon("compress-alt", verify_fa = FALSE),
    icon("question", verify_fa = FALSE)
  )
  if (addDiv) 
    tags$div(title=paste("Direction:", dir), .icon)
  else
    return(.icon)
}


#' RuningLocally
#' @return If the app is running locally it returns `TRUE` otherwise `FALSE`.
#' @noRd
RuningLocally <- function() {
  grepl(r"{\\JS\\}", tempdir())
}


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
  # 'escapeRegex' without wild card characters
  x <- gsub('([.|()\\^{}+$]|\\[|\\])', '\\\\\\1', x)
  
  if (OR) {
    x <- gsub(" ", '|', x)
    if (substring(x, 1L, 1L) != "*")
      x <- paste0("*", x)
    if (substring(x, nchar(x), nchar(x)) != "*")
      x <- paste0(x, "*")
    x <- glob2rx(x, trim.head = FALSE, trim.tail = FALSE)
  }
  else { # AND - use "(?=.*s1)(?=.*s2)"
    Operands <- strsplit(x, " ") |> 
      unlist() |>
      glob2rx(trim.head = TRUE, trim.tail = FALSE)
    Operands <- sub("^\\^(.*)\\$$", "\\1", Operands)
    x <- paste0("(?=.*", Operands, ")", collapse = "")
  }
  x
}




renderTableButton <- function(id) {
  # "<div class = "btn-group">" # not needed at the moment
  prefix <- "view_"
  shiny::actionButton(
    inputId = paste0(prefix, "%s"),
    label = "",
    icon = shiny::icon("eye-open", lib="glyphicon"),# shiny::icon("eye"),
    class = "btn-light btn-sm",
    onclick = "Shiny.onInputChange(\"btnViewKpi\", this.id, {priority: \"event\"})",
    title = "View KPI details") |>
    paste0() |> 
    sprintf(id)
}
