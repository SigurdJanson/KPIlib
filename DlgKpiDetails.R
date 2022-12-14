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
    Formula <- NULL
  }
  
  domainLabel <- ifelse(length.tagstr(Entry$domain) > 1, "Domains", "Domain")
  tagLabel <- ifelse(length.tagstr(Entry$tags) > 1, "Tags", "Tag")
  
  HelpfulRatings <- as.integer(ceiling(Entry$rating_count * (Entry$rating_avg / 5)))
  
  modalDialog(
    tabsetPanel(
      type = "pills",
      tabPanel("Description", 
               div(
                 p(HTML(Entry$description)),
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
                 ),
                 class = "kk-tabcontent"
               )
      ),
      tabPanel("Interpretation",
               div(
                 if (isTruthy(Entry$interpretation)) p(Entry$interpretation) else NULL,
                 tags$table(
                   tags$tr(
                     tags$td(span(mapField2Label("unit"), class="h-inline")), 
                     tags$td(span(Entry$unit, mapUnit2Icon(Entry$unit, FALSE), class="highlight"))
                   ),
                   tags$tr(
                     tags$td(span(mapField2Label("direction"), class="h-inline")), 
                     tags$td(span(Entry$direction, mapDirection2Icon(Entry$direction, FALSE), class="highlight"))
                   ),
                   tags$tr(
                     tags$td(span("Helpful flags", class="h-inline")),
                     tags$td(span(HelpfulRatings, icon("thumbs-up"), class="highlight"))
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
                 ),
               class = "kk-tabcontent"
               )
               
      ),
      if (isTruthy(Formula)) 
        tabPanel("Formula", div(Formula, class = "kk-tabcontent")) 
      else 
        NULL
    ),
    footer = tagList(
      modalButton("Close")
    ),
    size = "l",
    title = Entry$title,
    easyClose = TRUE
  )
}
