

readKpiData <- function(location) {
  x <- read_json(location, simplifyVector = TRUE)
  x$direction <- as.factor(x$direction)
  x$unit <- as.factor(x$unit)
  x$created_at <- as.POSIXct(x$created_at)
  x$updated_at <- as.POSIXct(x$updated_at)
  return(x)
}


# TODO: Icons: "Distance", "Energy", "Money", "Number", "Percentage", "Ratio", 
# "Score", "Time", "Volume", "Weight", "Weight/m²" 

mapUnit2Icon <- function(unit) {
  switch (as.character(unit),
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
}
