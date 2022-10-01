

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

filterKpiData <- function(x) {
  .Filter <- !sapply(x$intervention_needed, isTRUE)
  return(x[.Filter,])
}




mapUnit2Icon <- function(unit) {
  .icon <- switch (
    as.character(unit),
          Area     = icon("square-full", verify_fa = FALSE),
          Data     = icon("server", verify_fa = FALSE),
          Distance = icon("exchange", verify_fa = FALSE),
          Energy   = icon("battery-bolt", verify_fa = FALSE),
          Money    = icon("money-bill", verify_fa = FALSE),
          Number   = icon("abacus", verify_fa = FALSE),
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
  tags$div(title=paste("Unit:", unit), .icon)
}


RuningLocally <- function() {
  grepl(r"{\\JS\\}", tempdir())
}


#' Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}