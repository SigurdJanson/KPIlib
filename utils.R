

readKpiData <- function(location) {
  x <- read_json(location, simplifyVector = TRUE)
  x$direction <- as.factor(x$direction)
  x$unit <- as.factor(x$unit)
  x$created_at <- as.POSIXct(x$created_at)
  x$updated_at <- as.POSIXct(x$updated_at)
  return(x)
}


