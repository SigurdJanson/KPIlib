

# This solution is not perfect and only an approximation

source("utils.R")

library(jsonlite)
filename <- "./www/kpis.json"
x <- readKpiData(filename)

x$title <- tools::toTitleCase(x$title)

jsonlite::write_json(x, "./www/kpis.json")


