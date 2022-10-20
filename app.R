library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(jsonlite)
library(DT)

source("utils.R")
source("tagstr.R")
source("DlgKpiDetails.R")


RunningMode <- ifelse(RuningLocally(), "Admin", FALSE)

ShownTableCols <- c("title", "description", "direction", "unit", "tags", "domain")



# UI ==================================
ui <- function(request) { 
  dashboardPage(
    skin = "black", # try "midnight" eventually
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = "KPi Kluster",
      leftUi = tagList(
        radioGroupButtons(
          inputId = "KpiViewingMode",
          choices = 
            c(`<i class='fa fa-table' aria-label='View as table'></i>` = "table", 
              `<i class='fa fa-grip-horizontal' aria-label='View as tiles'></i>` = "grid"),
          justified = FALSE, size = "normal",
          disabled = FALSE
        )
      ),
      userOutput("uiAuthorAbout"),
      controlbarIcon = shiny::icon("filter")
    ),
    
    sidebar = dashboardSidebar(
      minified = (RunningMode == "Admin"), collapsed = TRUE,
      sidebarMenu(
        menuItem("KPIs", tabName = "ContentArea", 
                 icon = icon("dashboard", verify_fa = FALSE)),
        menuItemOutput("AdminMenu"),
        menuItem("About KPi Kluster", tabName = "AboutApp",
                 icon = icon("hand-sparkles", verify_fa = FALSE))
      )
    ),
    
    ## filter control side bar =====
    controlbar = dashboardControlbar(
      width = 280L, overlay = FALSE, collapsed = FALSE,
      div(class="content",
          h4("Find a KPI"),
          tags$div(title="Search title and description fields",
                   searchInput(
                     inputId = "filterFree", label = NULL,
                     placeholder = "Enter text to search for KPIs",
                     btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
                     width = "auto"
                   )
          ),
          div(id="TextSearchOptions-Parent", box(
            id = "TextSearchOptions",
            title = div("Text Search Settings"), #, `data-widget`="collapse" # has side effects
            solidHeader = TRUE, headerBorder = FALSE,
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
          tableOutput("dataInfo"),
          sliderTextInput(
            inputId = "inPageLength",
            label = "Maximum to display", 
            choices = c(20, 30, 40, 50, 100, 150, 200, 250),
            grid = TRUE
          )
      )
    ),
    
    ## body =====
    body = dashboardBody(
      tags$html(lang="en"),
      includeCSS("www/custom.css"),
      setShadow(class = "dropdown-menu"),
      tabItems(
        tabItem(tabName = "ContentArea",
                h2("KPIs"),
                uiOutput("KpiList")
        ),
        
        tabItem(
          tabName = "AdminArea",
          h2("KPI Administration"),
          column(3L,
                 infoBoxOutput("AdminExactDuplicateCount", width = 12L),
                 infoBoxOutput("AdminLongTitleCount", width = 12L),
                 infoBoxOutput("AdminTitleDuplicateCount", width = 12L),
                 box(tableOutput("AdminTitleDuplicates"), "Duplicated Titles", 
                     width = 12L, height = 400, collapsible = TRUE)
          ),
          column(3L,
                 infoBoxOutput("AdminUpperLowerTagCount", width = 12L),
                 box(tableOutput("AdminUpperLowerTags"), "Mixed Case Tags", 
                     width = 12L, height = 400, collapsible = TRUE),
                 infoBoxOutput("AdminLonelyTagCount", width = 12L),
                 box(tableOutput("AdminLonelyTags"), "Lonely Tags", 
                     width = 12L, height = 400, collapsible = TRUE)
          ),
          column(3L,
                 infoBoxOutput("AdminDomainCount", width = 12L),
                 infoBoxOutput("AdminLonelyNameCount", width = 12L),
                 box(tableOutput("AdminLonelyNames"), "Lonely Domains", 
                     width = 12L, height = 400, collapsible = TRUE)
          ),
          column(
            3L,
            infoBoxOutput("AdminMissingNameCount", width = 12L),
            infoBoxOutput("AdminMissingDescrCount", width = 12L),
            infoBoxOutput("AdminMissingUnitCount", width = 12L),
            infoBoxOutput("AdminMissingTagsCount", width = 12L),
            infoBoxOutput("AdminMissingFormulaCount", width = 12L)
          )
        ),
        
        tabItem(
          tabName = "AboutApp",
          column(width = 8L, offset = 2L,
                 wellPanel(
                   h2("About KPi Kluster"),
                   div(
                     p("The KPi Kluster is a large data base to get inspiration for 
                (Key) Performance Indicators you may want to track in your organisation."),
                h4("Origins"),
                p("Some of the KPIs in the cluster are provided by the author collected
                from various sources like books and professional experience."),
                p("Some of the KPIs have been gathered from the KPI Library. For almost 10 years 
                the KPI Library offered professionals a platform to share, search, and discuss KPIs. 
                The collection now available for download is the result of a community effort. 
                The platform behind it was the KPI Library. It was founded in 2007 by Mirror42. 
                Mirror42 was acquired by ServiceNow in July 2013."),
                p("These data were extensively improved by the author. Over 2500 KPIs were merged 
                or eliminated."),
                tags$ul(
                  tags$li("Removal of indicators without value to the user like duplicates or empty indicators"),
                  tags$li("Removal of indicators not complete enough to be understandable"),
                  tags$li("Spelling correction"),
                  tags$li("Removal of \"lonely\" tags that existed only once for clarity"),
                  tags$li("Combination of redundancies between KPIs for clarity"),
                  tags$li("Correction of empty fields")
                ),
                p("Despite all these efforts the library may still contain ambiguities, vagueness, 
                and mistakes. It was not possible so far to fill all missing information.",
                tags$b("Please note that you use KPi Kluster at your own responsibility.")),
                h4("Working with KPIs"),
                p("Looking up KPIs in a data base means that the function you want to improve has
                become a 'hygiene factor' of your business. You cannot excel in this area over
                your competitors. You cannot win anything by improving it ... but you will lose
                if you don't."),
                p("If you - however - need to track something to compete over, you need to find
                ways to differentiate yourself from your competitors. Which means: you need
                to differentiate your KPIs. Because - in general - if two organisations measure
                the same thing, they will end up doing the same thing."),
                p("If you want to find ways to be different, you can use a KPI data base as inspiration.
                But you will track and work your KPIs in very different ways."),
                p("If you have never worked intensively with numbers and KPIs, I can only recommend
                that you get some help. If you e.g. have never heard the terms 'spurious correlation' or
                'scale level', I can only recommend that you get some help."),
                h4("Version"),
                p("This App is KPI Kluster, Version 0.9"),
                h4("Mentions"),
                p("I am a Changitor and I probably would never have done this without the support
                from the ", 
                tags$a("Changitor team", href="https://www.changitors.com/en", .noWS = "after"), 
                "."),
                p("Thank you", tags$a("ServiceNow", href="https://www.servicenow.com"), 
                  "for hosting the KPI library for many years.")
                   )
                 )
          )#col
        )#tabItem
      )
    )#body
  )
}





# 
#
# SERVER ==================================
server <- function(input, output, session) {

  kpi <- readKpiData(c("./www/kpis.json", "./www/kpis_digitalproducts.json"))
  kpi <- filterKpiData(kpi)
  
  LiveKpi <- reactiveVal(kpi)
  
  ShowPageLength <- reactiveVal(20L)

  output$dataInfo <- renderTable({
    filterCount <- sum(sapply(c(input$filterDomain, input$filterFree, input$filterTag), isTruthy))
    resultCount <- nrow(LiveKpi())
    matchStr <- ifelse(resultCount == 1, "matches", "match")
    filterStr <- if(filterCount == 0) 
                    "same because unfiltered"
                 else if (filterCount == 1)
                    paste(matchStr, "the filter")
                 else
                    paste(matchStr, "the filters")
    data.frame(
      Value = format(
        c(nrow(kpi), resultCount, min(ShowPageLength(), nrow(LiveKpi()))), 
        justify="right", width=6L, big.mark = ","), 
      Label = c("available in total", filterStr, "displayed")
    )
  }, rownames=FALSE, colnames = FALSE, align="rl", spacing="xs")

  
  
  #
  #
  #
  #
  # KPI FILTER ========================
  
  observeEvent(input$inPageLength, {
    ShowPageLength(input$inPageLength)
  })
  
  # Update drop down lists once it's been initialized
  observeEvent(LiveKpi, {
    Domains <- parseDomains(LiveKpi()$domain)
    updatePickerInput(
      session = session, inputId = "filterDomain",
      choices = Domains, selected = input$filterDomain
    )
    
    Tags <- parseTags(LiveKpi()$tags)
    updatePickerInput(
      session = session, inputId = "filterTag", 
      choices = Tags, selected = input$filterTag)
  }, ignoreInit = FALSE)
  
  
  
  #
  observeEvent(
    list(input$filterDomain, input$filterFree, input$filterTag, 
         input$cbSearchMode, input$cbFreeTextCasesense, input$cbSearchOperator), 
    {
    Regex <- "Regex" %in% input$cbSearchMode
    IgnoreCase <- !("CaseSensitive" %in% input$cbFreeTextCasesense)
    OperatorOr <- "OR" %in% input$cbSearchOperator

    RowFilter <- rep(FALSE, nrow(kpi))
    if (isTruthy(input$filterDomain) || isTruthy(input$filterFree) || isTruthy(input$filterTag)) {
      # Free text search
      if (isTruthy(input$filterFree)) {
        SearchString <- input$filterFree 
        if (!Regex) { # standard search
          SearchString <- wc2Regex(input$filterFree, OperatorOr)
        }
        RowFilter <- RowFilter | 
          grepl(SearchString, kpi$title, 
                fixed = FALSE, ignore.case = IgnoreCase, perl = TRUE) | 
          grepl(SearchString, kpi$description, 
                fixed = FALSE, ignore.case = IgnoreCase, perl = TRUE) | 
          grepl(SearchString, kpi$interpretation, 
                fixed = FALSE, ignore.case = IgnoreCase, perl = TRUE)
      }

      if (isTruthy(input$filterDomain)) {
        domainFilter <- escapeRegex(input$filterDomain)
        if (length(domainFilter) > 1)
          RowFilter <- RowFilter | apply(domainFilter %isin% kpi$domain, 1L, any)
        else
          RowFilter <- RowFilter | domainFilter %isin% kpi$domain
      }

      if (isTruthy(input$filterTag)) {
        if (length(input$filterTag) > 1)
          RowFilter <- RowFilter | apply(input$filterTag %isin% kpi$tags, 1L, any)
        else
          RowFilter <- RowFilter | input$filterTag %isin% kpi$tags
      }
      
      LiveKpi(kpi[RowFilter, ])
    }
    else
      LiveKpi(kpi)
  })
  
  
  
  # OUTPUT ===================
  
  MakeKpiBox <- function(x) { # x is a kpi row
    domainLabel <- ifelse(length.tagstr(x$domain) > 1, "Domains", "Domain")
    #tagLabel <- ifelse(length.tagstr(x$tags) > 1, "Tags", "Tag")

    UnitIcon <- mapUnit2Icon(x$unit)
    UnitIcon <- tagAppendAttributes(UnitIcon, class="tileicon")
    DirectionIcon <- mapDirection2Icon(x$direction)
    DirectionIcon <- tagAppendAttributes(DirectionIcon, class="tileicon")
    
    if (nchar(x$description) > 280) 
      descr <- paste(substr(x$description, 1L, 280L), "…")
    else
      descr <- x$description
    
    Box <- shinydashboard::box(
      id = x$id,
      width = 6L, # equals class="col-sm-6"
      height = "200px",
      solidHeader = TRUE, status = NULL,
      title = x$title,
      column(12L, p(descr), class = "text-left"),
      column(12L,
        div(span(domainLabel, class="h-inline"),
        span(x$domain, class="highlight truncate"))),
      footer = div(UnitIcon, DirectionIcon,
             HTML(renderTableButton(x$id)),
             class="box-icons")
    )
    
    Box <- tagAppendAttributes(Box, class="truncate", .cssSelector = ".box-title")
    Box <- tagAppendAttributes(Box, title = x$title, .cssSelector = ".box-header")
    Box <- tagAppendAttributes(Box, class="col-xs-12 col-md-6 col-lg-4")
    Box
  }
  
  
  
  output$KpiTable <- DT::renderDataTable({
      result <- head(LiveKpi(), n = ShowPageLength())
      result <- cbind(result, Actions = renderTableButton(result$id))
      return(result[, c(ShownTableCols, "Actions")])
    }, 
    options = list(
      searching=FALSE, 
      paging = FALSE, 
      pageLength = ShowPageLength(), 
      info=FALSE,
      language = list(emptyTable = strEmptySearchResult),
      processing = FALSE), # needed for buttons?
    rownames = FALSE,
    colnames = ShownTableLabels,
    selection = "single",
    escape = FALSE
  )
  
  
  
  output$KpiList <- renderUI({
    if (input$KpiViewingMode == "table") {
      dataTableOutput("KpiTable")
    } else if (input$KpiViewingMode == "grid") {
      data <- head(LiveKpi(), n = ShowPageLength())
      if (nrow(data) > 0L) {
        Boxes <- tagList()
        for (i in 1:nrow(data)) {
          Boxes <- c(Boxes, tagList(MakeKpiBox(data[i,])))
        }
        .html <- tagList(
          fluidPage(fluidRow(Boxes))
        )
      } else {
        .html <- tagList(
          fluidPage(fluidRow(strEmptySearchResult))
        )
      }
      return(.html)
    }
  })
  
  
  # when "view" button is clicked show modal dialog with details
  observeEvent(input$btnViewKpi, {
    selectedId <- as.numeric(strsplit(input$btnViewKpi, "_")[[1]][2])
    selectedRow <- LiveKpi()$id == selectedId
    LiveKpi()[selectedRow, ] |>
      KpiDetailsModal() |>
      showModal()
  })

  
  

  
  #
  #
  #
  # ADMIN SSECTION ========================

  
  output$AdminMenu <- renderMenu(
    if (RunningMode == "Admin") {
      menuItem("Admin", tabName = "AdminArea", icon = icon("users-cog", verify_fa = FALSE))
    }
  )
  
  ## Exact Duplicates =====
  output$AdminExactDuplicateCount <- renderInfoBox({
    Count <- sum(duplicated(kpi))
    
    if (Count < 1L) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "No exactly duplicate KPIs"
    } else if (Count < 5L) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 5 duplicated KPIs"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 5 duplicated KPIs"
    }
      
    infoBox(
      "Exact Duplicates", InfoTxt, "Exact Duplicate (complete entry)",
      icon = Icon, color = Color
    )
  })
  
  
  ## Title Duplicates =====
  output$AdminTitleDuplicateCount <- renderInfoBox({
    Count <- sum(duplicated(tolower(kpi$title)))
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "Less than 5% duplicate titles"
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 10% duplicated titles"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 10% duplicated titles"
    }
    
    infoBox(
      "Title Duplicates", InfoTxt, "Duplicated titles (case insensitive)",
      icon = Icon, color = Color
    )
  })
  output$AdminTitleDuplicates <- renderTable({
    Dups <- kpi$title |> tolower() |> duplicated()
    Dups <- kpi$title[Dups] |> unique()
    data.frame(Duplicates = Dups)
  })
  
  
  
  
  ## Long Titles =====
  output$AdminLongTitleCount <- renderInfoBox({
    Threshold <- 40L
    Count <- sum(nchar(kpi$title) > Threshold)
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "Less than 5% long titles"
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 10% long titles"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 10% long titles"
    }
    
    infoBox(
      "Long Titles", InfoTxt, 
      sprintf("Long titles with > %d characters", Threshold),
      icon = Icon, color = Color
    )
  })
  

    
  ## Domains =====
  output$AdminDomainCount <- renderInfoBox({
    Doms <- sapply(kpi$domain, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      na.omit() |> tolower() |> unique()
    infoBox(
      "Total Domains", length(Doms), 
      "All domains",
      icon = icon("Flag"), color = "black"
    )
  })
  
  AdminLonelyNames <- reactive({
    Names <- sapply(kpi$domain, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      na.omit() |>
      sort()
    Runs <- Names |> rle()
    Runs$values[Runs$lengths == 1]
  })
  
  output$AdminLonelyNameCount <- renderInfoBox({
    Count <- length(AdminLonelyNames())
    
    if (Count < nrow(kpi) * 0.01) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "Less than 1% lonely domains"
    } else if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 5% lonely domains"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 5% lonely domains"
    }
    
    infoBox(
      "Lonely Domains", InfoTxt, paste0("Domains existing only once (", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  output$AdminLonelyNames <- renderTable({
    return(data.frame(Lonelies = AdminLonelyNames()))
  })
  
  
  ## Tags  =====
  AdminUpperLowerTags <- reactive({
    Tags <- sapply(LiveKpi()$tags, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      na.omit() |>
      unique()
    TagsIroned <- tolower(Tags)
    TagsIroned
  })
  
  output$AdminUpperLowerTagCount <- renderInfoBox({
    Dups <- AdminUpperLowerTags() |> duplicated()
    Count <- sum(Dups)
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "Less than 5% mixed tags"
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 10% mixed tags"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 10% mixed tags"
    }
    
    infoBox(
      "Mixed Tags", InfoTxt, "Tags with mixed upper/lower case",
      icon = Icon, color = Color
    )
  })
  
  
  output$AdminUpperLowerTags <- renderTable({
    Dups <- AdminUpperLowerTags() |> duplicated()
    data.frame(Duplicates = AdminUpperLowerTags()[Dups])
  })

  
  ## Lonely Tags  =====
  AdminLonelyTags <- reactive({
    Tags <- sapply(kpi$tags, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      na.omit() |>
      sort()
    Runs <- Tags |> rle()
    Runs$values[Runs$lengths == 1]
  })
  
  
  output$AdminLonelyTagCount <- renderInfoBox({
    Count <- length(AdminLonelyTags())
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- "Less than 5% lonely tags"
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- "Less than 10% lonely tags"
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- "MORE than 10% lonely tags"
    }
    
    infoBox(
      "Lonely Tags", InfoTxt, "Tags that exist only once",
      icon = Icon, color = Color
    )
  })
  
  
  output$AdminLonelyTags <- renderTable({
    return(data.frame(Lonelies = AdminLonelyTags()))
  })
  
  
  
  
  ## Missing Pieces ======
  output$AdminMissingDescrCount <- renderInfoBox({
    Count <- nrow(kpi) - sum(sapply(kpi$description, isTruthy))
    what <- "missing descriptions"
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- paste("Less than 5%", what)
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- paste("Less than 10%", what)
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- paste("MORE than 10%", what)
    }
    
    infoBox(
      "Missing Descriptions", InfoTxt, paste0("(", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  
  output$AdminMissingNameCount <- renderInfoBox({
    Count <- nrow(kpi) - sum(sapply(kpi$domain, isTruthy))
    what <- "missing domains"
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- paste("Less than 5%", what)
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- paste("Less than 10%", what)
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- paste("MORE than 10%", what)
    }
    
    infoBox(
      "Missing Domains", InfoTxt, paste0("(", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  
  
  output$AdminMissingTagsCount <- renderInfoBox({
    Count <- nrow(kpi) - sum(sapply(kpi$tags, isTruthy))
    what <- "missing tags"
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- paste("Less than 5%", what)
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- paste("Less than 10%", what)
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- paste("MORE than 10%", what)
    }
    
    infoBox(
      "Missing Tags", InfoTxt, paste0("(", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  
  output$AdminMissingUnitCount <- renderInfoBox({
    Count <- nrow(kpi) - sum(sapply(kpi$unit, isTruthy))
    what <- "missing units"
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- paste("Less than 5%", what)
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- paste("Less than 10%", what)
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- paste("MORE than 10%", what)
    }
    
    infoBox(
      "Missing Units", InfoTxt, paste0("(", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  
  output$AdminMissingFormulaCount <- renderInfoBox({
    Count <- nrow(kpi) - sum(sapply(kpi$formula, isTruthy))
    what <- "missing formulae"
    
    if (Count < nrow(kpi) * 0.05) {
      Icon <- icon("thumbs-up", lib = "glyphicon")
      Color <- "green"
      InfoTxt <- paste("Less than 5%", what)
    } else if (Count < nrow(kpi) * 0.10) {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "yellow"
      InfoTxt <- paste("Less than 10%", what)
    } else {
      Icon <- icon("thumbs-down", lib = "glyphicon")
      Color <- "red"
      InfoTxt <- paste("MORE than 10%", what)
    }
    
    infoBox(
      "Missing Formulas", InfoTxt, paste0("(", Count, ")"),
      icon = Icon, color = Color
    )
  })
  
  #
  #
  #
  # HEADER SECTION ========================
  
  
  output$uiAuthorAbout <- renderUser({
    dashboardUser(
      name = "Author", 
      image = "https://www.seifseit.de/images/Jan2013c.jpg", 
      title = "Dr. Jan Seifert",
      subtitle = "", 
      footer = p("This is a personal project by Jan Seifert under the patronage of", 
                 tags$a("Changitors", href="https://www.changitors.com/en"), ".", 
                 class = "text-center"),
      fluidRow(
        dashboardUserItem(
          width = 4L,
          socialButton(
            href = "https://twitter.com/usernaut",
            icon = icon("twitter", style="color:white") # overwrite color in shinydashboardPlus v. 2.0.3
          )
        ),
        dashboardUserItem(
          width = 4L,
          socialButton(
            href = "https://github.com/SigurdJanson",
            icon = icon("github", style="color:white") # overwrite color in shinydashboardPlus v. 2.0.3
          )
        ),
        dashboardUserItem(
          width = 4L,
          socialButton(
            href = "https://de.linkedin.com/in/jan-seifert-3194951b1",
            icon = icon("linkedin", style="color:white") # overwrite color in shinydashboardPlus v. 2.0.3
          )
        )
      )
    )
  })



  #
  # BOOKMARKS ==========
  
  ReadSetFromUrl <- observe({
    SetId <- "set"
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[[SetId]])) {
      if (query[[SetId]] == "\"ux\"") {
        uxdomains <- tolower(c("Social Media & E-business", "Research"))
        uxtags <- c("e-commerce", "web analytics", 
                    "user", "experience", "design system", 
                    "research ops")
      }
      Domains <- parseDomains(LiveKpi()$domain)
      updatePickerInput(
        session = session, inputId = "filterDomain",
        choices = Domains, selected = uxdomains
      )
      
      Tags <- parseTags(LiveKpi()$tags)
      updatePickerInput(
        session = session, inputId = "filterTag",
        choices = Tags, selected = uxtags)
    }
    # Destroy itself
    ReadSetFromUrl$destroy()
  })
}


# Run the application 
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)
