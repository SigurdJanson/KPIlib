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
    Range    = icon("ruler-horizontal", verify_fa = FALSE),
    Value    = icon("arrows-to-circle", verify_fa =FALSE),
    Contextual= icon("people-arrows", verify_fa =FALSE),
    Monitor  =  icon("magnifying-glass-chart", verify_fa =FALSE),
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

#' clearTagList
#' Removes all `NULL` elements from a tag list. Some functions cannot handle `NULL`
#' elements in `tagList`s, especially extra packages. Therefore they must be removed.
#' @param x A tag list element
#' @return A modified tag list with all entries being `NULL` removed.
clearTagList <- function(x) {
  x[!sapply(x, is.null)]
}



#' renderNavBarMenuItem
#' Creates a fake menu item for the Nav Bar. This is a little of a hack to
#' allow a navbar menu in a shiny dashboard. Code has mainly been taken from
#' `dropdownMenu()`.
#' @param text A title to display.
#' @param ... 
#' @param icon Optional icon to appear next to the nav item's title.
#' @param badgeLabel Not used.
#' @param badgeColor Not used.
#' @param tabName Internal id to refer to the tab (will be part of the #anchor).
#' @param href Not used.
#' @param selected Is the rendered tab the selected one.
#' @param hidden Shall `NULL` be returned instead of a hidden tab.
#'
#' @return
#' @export
#'
#' @examples
renderNavBarMenuItem <- function(text, ..., icon = NULL, 
                                 badgeLabel = NULL, badgeColor = "green", 
                                 tabName = NULL, href = NULL, selected = NULL, 
                                 hidden=FALSE) {
  if (hidden) return(NULL)
  
  if (!is.null(icon))
    shinydashboardPlus:::tagAssert(icon, type = "i")

  isTabItem <- FALSE
  target <- NULL
  if (!is.null(tabName)) {
    shinydashboardPlus:::validateTabName(tabName)
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab) 
      target <- "_blank"
  }
  
  isExpanded <- FALSE
  expandedName = as.character(gsub("[[:space:]]", "", text))
  subItems <- list(...)
  
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-", badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }

  if (length(subItems) == 0) {
    return(tags$li(
      class=if (isTRUE(selected)) "dropdown active" else "dropdown",
      a(icon, text, href = href, 
        class="dropdown-toggle",
        `data-toggle` = if (isTabItem) "tab", 
        `data-value` = if (!is.null(tabName)) tabName, 
        `data-start-selected` = if (isTRUE(selected)) 1 else NULL, 
        `aria-selected` = if (isTRUE(selected)) "true" else "false", 
        `aria-expanded`="true",
        tabindex = if (isTRUE(selected)) 0 else -1, 
        target = target, 
        badgeTag)))
  }
  
  # Currently not used ... but taken from 
  tags$li(
    class = "dropdown", 
    a(href = href, 
      icon, 
      span(text), 
      shiny::icon("angle-left", class = "pull-right"),
      `data-toggle`="tab", 
      `data-value`="ContentArea", tabindex=-1, 
      `aria-expanded`="false", `aria-selected`="false"), 
    do.call(tags$ul, 
            c(class = paste0("treeview-menu", 
            if (isExpanded) " menu-open" else ""), 
            style = paste0("display: ", if (isExpanded) "block;" else "none;"), 
            `data-expanded` = expandedName, subItems))
    )
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


stripHTML_a <- function(x) {
  gsub(r"{<a>|</a>|<\s*a\s+.*?>}", "", x)
}


renderAboutContent <- function() {
  column(width = 8L, offset = 2L,
         wellPanel(
           h2("About KPi Kluster"),
           div(
             p("The KPi Kluster is a huge database to get inspiration for (key) performance indicators you might want to track in your organisation."),
             
             h4("Origins"),
             p("Some of the KPIs in the cluster I have collected from various sources such as books and my own professional experience. 
               Many of them came from the former KPI Library. For almost 10 years, this library has provided a platform for professionals to share, 
               search and discuss KPIs. It is the result of a community effort. Now it is available for download. 
               The platform behind this was the KPI Library. It was founded by Mirror42 in 2007. Mirror42 was acquired by ServiceNow in July 2013."),
             p("This data has been extensively enhanced. Over 2500 KPIs have been merged or eliminated."),
             tags$ul(
               tags$li("Removal of indicators without value to the user like duplicates or empty indicators"),
               tags$li("Removal of indicators not complete enough to be understandable"),
               tags$li("Spelling correction"),
               tags$li("Removal of 'lonely' tags that existed only once for clarity"),
               tags$li("Combination of redundancies between KPIs for clarity"),
               tags$li("Correction of empty fields, adding meaningful names, descriptions, and formulas")
             ),
             p("Despite these efforts, the library may still contain ambiguities, inaccuracies and errors. 
               It has not yet been possible to fill in all the missing information. But I still improve it 
               every now and then, when I find the time.",
               tags$b("Please note that you use KPi Kluster at your own responsibility.")),
             
             h4("Working with KPIs"),
             p("Looking up KPIs in a database means that the function you want to improve has become a 'hygiene factor' of your business. 
               You cannot outperform your competitors in this area. 
               You have nothing to gain by improving it... but you lose if you don't."),
             p("However, if you have to pursue something in order to compete, you need to find ways to differentiate yourself from your competitors. 
               That means you need to differentiate your KPIs. 
               After all, if two organisations measure the same thing, they will end up doing the same thing."),
             p("If you want to find ways to be different, you can use a KPI database for inspiration."),
             p("But you will track and work with your KPIs in very different ways.
                If you have never worked intensively with numbers and KPIs, I can only recommend that you get some help. 
                For example, if you have never heard of 'spurious correlation' or 'scale level', I can only recommend that you get some help."),
             
             h4("Version"),
             p("This app is KPI Kluster, Version 1.4"),
             p("If you find any issues, you can post it on", 
               tags$a("Github", href="https://github.com/SigurdJanson/KPIlib/issues", 
                      .noWS = "after", target="_blank"),
               "."),
             
             h4("Mentions"),
             p("I am a Changitor and I probably would never have done this without the inspiration
                in the ", 
               tags$a("Changitor team", href="https://www.changitors.com/en", .noWS = "after"), 
               "."),
             p("Thank you", tags$a("ServiceNow", href="https://www.servicenow.com"), 
               "for hosting the KPI library for many years.")
           )
         )
  )#col
}



renderFilterSideBar <- function() {
  dashboardControlbar(
    width = 280L, overlay = FALSE, collapsed = FALSE,
    div(class="content",
        h4("Find a KPI"),
        tags$div(title="Title and description fields",
                 searchInput(
                   inputId = "filterFree", label = NULL,
                   placeholder = "By title / description",
                   btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
                   width = "auto"
                 )
        ),
        div(id="TextSearchOptions-Parent", shinydashboardPlus::box(
          id = "TextSearchOptions",
          title = div("Advanced Settings"),
          solidHeader = FALSE, headerBorder = FALSE,
          collapsible = TRUE, collapsed = TRUE,
          closable = FALSE, 
          boxToolSize = "lg",
          width = 12L,
          tags$div(title="Use free text search with wild cards * and ? or regular expressions",
                   pickerInput(
                     inputId = "cbSearchMode",
                     choices = c(`Standard Mode`="SearchDefault", `Reg.*Expressions`="Regex"),
                     options = list(mobile=FALSE, showSubtext=FALSE, showTick=TRUE),
                     width="100%"
                   )
          ),
          tags$div(title="If activated the search will be case sensitive",
                   checkboxGroupButtons(
                     inputId = "cbFreeTextCasesense", label = NULL,
                     choices = c(`Case sensitive <b>Aa</b>` = "CaseSensitive"),
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square"),
                       no = tags$i(class = "fa fa-square-o")),
                     justified = TRUE
                   )
          ),
          conditionalPanel(
            condition = "input.cbSearchMode == 'SearchDefault'",
            tags$div(title="Search result must contain all search terms or just any of them",
                     radioGroupButtons(
                       inputId = "cbSearchOperator",
                       choices = c(`<b>||||</b> All` = "AND",
                                   `<b>..|.</b> Any` = "OR"),
                       justified = TRUE, size = "normal",
                       disabled = FALSE
                     )
            )
          )
        )),
        # Domains
        tags$div(title="Filter for one or several domains",
                 pickerInput(
                   inputId = "filterDomain",
                   label = "Domain",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE, size = 10L,
                     `live-search` = TRUE, `dropdown-align-right` = "auto"
                   ),
                   multiple = TRUE,
                   width = "100%"
                 )
        ),
        tags$div(title="Filter for one or several tags",
                 pickerInput(
                   inputId = "filterTag",
                   label = "Tag", 
                   choices = "",
                   options = list(
                     `actions-box` = TRUE, size = 10L,
                     `live-search` = TRUE, `dropdown-align-right` = TRUE
                   ),
                   multiple = TRUE,
                   width = "100%"
                 )),
        br(),
        h4("Search Result"),
        tableOutput("dataInfo") |> 
          withSpinner(color="white", proxy.height="75px"),
        sliderTextInput(
          inputId = "inPageLength",
          label = "Maximum to display", 
          choices = c(20, 30, 40, 50, 100, 150, 200, 250),
          grid = TRUE
        )
    )
  )
}


# PRESETS ================

#' Load the presets from the file
readKpiPresets <- function(filename) {
  x <- try(read_json(filename, simplifyVector = FALSE))
  if (inherits(x, "try-error")) return(NULL)
  return(x)
}



# UTILITIES =============

#' Tests if two lists are strictly identical
#' The test is not recursive.
#' @param l1
#' @param ...
listEqual <- function(l1, ...) {
  l2 = list(...)
  return(identical(l1, l2))
  # if (length(l1) != length(l2)) 
  #   return(FALSE)
  # result <- mapply(vecEqual, l1, l2) |>
  #   all()
  # return(result)
}


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
