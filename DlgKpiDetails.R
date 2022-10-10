# Return the UI for a modal dialog with data selection input.
KpiDetailsModal <- function(Entry) {
  stopifnot(!missing(Entry))
  
  # CREATE FORMULA OBJECT
  if (isTruthy(Entry$formula)) {
    if (grepl("$$", Entry$formula, fixed=TRUE)) {
      Formula <- p(withMathJax(Entry$formula))
    } else {
      Formula <- p(Entry$formula, class="truncate")
    }
  } else {
    Formula <- NULL # Formula <- p("Formula not available", class="h-inline")
  }
  
  modalDialog(
    tabsetPanel(
      tabPanel("Description", 
               #p(Entry$title),
               fluidPage(
               p(Entry$description),
               div(
                 span("domain", class="h-inline"), 
                 span(Entry$domain, class="highlight")
               ))
      ),
      tabPanel("Interpretation",
               if (isTruthy(Entry$interpretation)) p(Entry$interpretation) else NULL,
               tags$table(
                 tags$tr(
                   tags$td(span("unit", class="h-inline")), 
                   tags$td(span(Entry$unit, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span("direction", class="h-inline")), 
                   tags$td(span(Entry$direction, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span("tags", class="h-inline")),
                   tags$td(span(Entry$tags, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span("last update", class="h-inline")),
                   tags$td(span(
                     format(max(Entry$updated_at, Entry$created_at), "%B %Y"), class="highlight"))
                 )
               )
      ),
      tabPanel("Formula", Formula)
    ),
    footer = tagList(
      modalButton("Close")#,
      #actionButton("ok", "OK")
    ),
    size = "l",
    title = Entry$title,
    easyClose = TRUE
  )
}