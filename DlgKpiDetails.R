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
  
  domainLabel <- ifelse(length.tagstr(Entry$domain) > 1, "Domains", "Domain")
  tagLabel <- ifelse(length.tagstr(Entry$tags) > 1, "Tags", "Tag")
  
  modalDialog(
    tabsetPanel(
      tabPanel("Description", 
               #p(Entry$title),
               fluidPage(
               p(Entry$description),
               if (isTruthy(Entry$url)) {
                 p(
                   tags$a("Further details", icon("link"), href=Entry$url)
                 )
               } else {
                 NULL
               },
               div(
                 span(domainLabel, class="h-inline"), 
                 span(Entry$domain, class="highlight")
               ))
      ),
      tabPanel("Interpretation",
               if (isTruthy(Entry$interpretation)) p(Entry$interpretation) else NULL,
               tags$table(
                 tags$tr(
                   tags$td(span(mapField2Label("unit"), class="h-inline")), 
                   tags$td(span(Entry$unit, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span(mapField2Label("direction"), class="h-inline")), 
                   tags$td(span(Entry$direction, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span(tagLabel, class="h-inline")),
                   tags$td(span(Entry$tags, class="highlight"))
                 ),
                 tags$tr(
                   tags$td(span("Last update", class="h-inline")),
                   tags$td(span(
                     format(max(Entry$updated_at, Entry$created_at), "%B %Y"), class="highlight"))
                 )
               )
      ),
      if (isTruthy(Formula)) tabPanel("Formula", Formula) else NULL
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
