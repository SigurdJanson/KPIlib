# 'Constants' 
strEmptySearchResult <- "Search does not yield any results"
ShownTableLabels <- c(Title = "title", Description = "description", 
                      Direction = "direction", `Unit Type` = "unit", 
                      Tags = "tags", Domains = "domain")


# LOADING KPIS ================

#' Load the data from the files in "location"
readKpiData <- function(location) {
  x <- NULL
  for (.loc in location) {
    x2 <- try(read_json(.loc, simplifyVector = TRUE))
    if (inherits(x2, "try-error")) next
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


parseDomains <- function(Domains) {
  sapply(Domains, \(x) strsplit(x, ",")) |>
    unlist() |>
    unname() |>
    trimws() |>
    tolower() |>
    na.omit() |>
    unique() |>
    sort()
}

parseTags <- function(Tags) {
  sapply(Tags, \(x) strsplit(x, ",")) |>
    unlist() |>
    unname() |>
    trimws() |>
    tolower() |>
    na.omit() |>
    unique() |>
    sort()
}



# PROCESSING KPIS ================

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



# ... ================

#' RuningLocally
#' @return If the app is running locally it returns `TRUE` otherwise `FALSE`.
#' @noRd
RuningLocally <- function() {
  grepl(r"{\\JS\\}", tempdir())
}



# RENDERING ================


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


stripHTML_a <- function(x) {
  gsub(r"{<a>|</a>|<\s*a\s+.*?>}", "", x)
}



# PRESETS ================

#' Load the presets from the file
readKpiPresets <- function(filename) {
  x <- try(read_json(filename, simplifyVector = FALSE))
  if (inherits(x, "try-error")) return(NULL)
  return(x)
}



# UTILITIES =============



#' Checks if two vectors are equal while ignoring the order of elements.
#' @param v1,v2 a vector of **unique** elements
#' @details
#' * Empty vectors are considered equal, regardless of the type.
#' * `NULL` is considered equal to an empty vector.
vecEqual <- function(v1, v2) {
  length(v1) == length(v2) && setequal(v1, v2)
}



#' Gracious logical gate. If one of the arguments is "falsy" (at least roughly),
#' `lg` returns the value of the truthy vector.
lg <- function(x1, x2, operator = `&`) {
  isValid <- \(x) is.logical(x) && length(x) > 0 && !any(is.na(x))
  
  if (isValid(x1)) {
    if (isValid(x2)) {
      return(operator(x1, x2))
    } else {
      return(x1)
    }
  } else {
    if (isValid(x2)) {
      return(x2)
    } else {
      stop("Both arguments are falsy.")
    }
  }
}
