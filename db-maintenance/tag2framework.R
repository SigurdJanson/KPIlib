#
# MAINTENANCE OF TAGS
# 

source("tagstr.R")
source("utils.R")

#' MoveTagsToFramework
#'
#' @param dt a data frame
#' @param toMoveThe tags that should be moved
#'
#' @return A new data frame
MoveTagsToFramework <- function(dt, toMove) {
  for (tag in toMove) {
    Found <- which(tag %isin% dt[["tags"]])
    for (f in Found) {
      dt[f, "framework"] <- unclass(as.tagstr(dt[f, "framework"]) + tag) # 
      dt[f, "tags"] <- unclass(as.tagstr(dt[f, "tags"]) - tag) # 
    }
  }
  return(dt)
}


kpi <- jsonlite::read_json("./www/kpis_Be4MovingFrameworkTags_20221025.json", simplifyVector = TRUE) #readKpiData(c("./www/kpis.json", )) "./www/kpis_digitalproducts.json"
kpi <- MoveTagsToFramework(kpi, c("ASL", "BiSL", "BMF", "Cobit", "EBM", "HEART", "ITIL", "MOF", "PMBOK", "SOX", "SCOR", "UBPR", "VRM"))
jsonlite::write_json(kpi, "./www/kpis.json", pretty=TRUE)

# TEST ##############################
# x <- data.frame(
#   tags = c("style, guide, ITIL, MOF, ASL, cost, sales",
#            "MOF, ASL, cost, sales",
#            "style, guide, MOF, ASL, cost, sales",
#            "VRM, guide, ITIL, cost, sales"),
#   framework = c("SCHONDA", "SCHONDA, NOCHN", "", "")
# )
# print(x)
# print(MoveTagsToFramework(x, c("MOF", "ITIL", "ASL", "VRM")))

#which("MOF" %isin% x[["tags"]])
