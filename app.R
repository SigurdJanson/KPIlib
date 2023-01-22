library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(jsonlite)
library(DT)

source("utils.R")
source("filter.R")
source("tagstr.R")
source("DlgKpiDetails.R")
source("AdminModule.R")


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
          tags$div(title="Title and description fields",
                   searchInput(
                     inputId = "filterFree", label = NULL,
                     placeholder = "By title / description",
                     btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
                     width = "auto"
                   )
          ),
          div(id="TextSearchOptions-Parent", box(
            id = "TextSearchOptions",
            title = div("Advanced Settings"), #, `data-widget`="collapse" # has side effects
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
          tableOutput("dataInfo") |> 
            withSpinner(color="white", proxy.height="75px"),
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
          KlusterAdminUI("KpiKlusterAdminSection")
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
                p("Some of the KPIs in the cluster I have collected 
                from various sources like books and my own professional experience.
                Some of them I got from the KPI Library. For almost 10 years 
                this library offered professionals a platform to share, search, and discuss KPIs. 
                It is the result of a community effort. Now it is now available for download.
                The platform behind it was the KPI Library. It was founded in 2007 by Mirror42. 
                Mirror42 was acquired by ServiceNow in July 2013."),
                p("These data were extensively improved. Over 2500 KPIs were merged 
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
                p("This App is KPI Kluster, Version 1.2"),
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
        )#tabItem
      )
    )#body
  )
}





# 
#
# SERVER ==================================
server <- function(input, output, session) {
  # Load data and remove those with `intervention == true`
  kpi <- readKpiData(c("./www/kpis.json", "./www/kpis_digitalproducts.json"))
  if (!is.null(kpi)) kpi <- filterKpiData(kpi)
  
  ShowPageLength <- reactiveVal(20L)

  # Provide the number of filtered table entries in the filter sidebar
  output$dataInfo <- renderTable({
    shiny::validate(need(LiveKpi(), "No data"))
    
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
  # KPI FILTER ============================================
  LiveKpi <- reactiveVal(kpi) # the filtered data
  
  TextSearchId = c("title", "description", "interpretation")
  DomainSearchId = "domain"
  TagSearchId = "tags"
  
  # Buffer data to avoid running the same search twice
  Prev_SearchStr <- "" # use widget default here
  Prev_TagsSelected <- NULL # use widget default here
  Prev_DomainsSelected <- NULL # use widget default here
  Prev_FreeTextResult <- NULL # logical vector of hits
  Prev_CategoryResult <- NULL # logical vector of hits
  
  # 
  CreateFilter <- function(Data, SearchStr, Domains, Tags) {
    if (!vecEqual(SearchStr, Prev_SearchStr)) {
      if (isTruthy(SearchStr)) {
        Prev_FreeTextResult <<- TextFilterKpi(
          SearchStr, 
          Data[[TextSearchId[1L]]], Data[[TextSearchId[2L]]], Data[[TextSearchId[3L]]],
          list(Regex = FALSE, IgnoreCase = TRUE, OperatorOr = FALSE))
      } else {
        Prev_FreeTextResult <<- ""
      }
      Prev_SearchStr <<- SearchStr
    }
    
    if (!vecEqual(Domains, Prev_DomainsSelected) || !vecEqual(Tags, Prev_TagsSelected)) {
      if (isTruthy(Domains)) {
        Prev_CategoryResult <<- CategoryFilterKpi(Domains, Data[[DomainSearchId]])
      } else {
        Prev_CategoryResult <<- NULL
      }
      Prev_DomainsSelected <<- Domains
      
      if (isTruthy(Tags)) {
        Prev_CategoryResult <<- lg(CategoryFilterKpi(Tags, Data[[TagSearchId]]), Prev_CategoryResult, `|`)
      }
      Prev_TagsSelected <<- Tags
    }

    return(lg(Prev_FreeTextResult, Prev_CategoryResult, `&`))
  }
  
  
  #
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
  
  
  
  # Apply the filter to the table whenever it changes
  observeEvent(
    list(input$filterDomain, input$filterFree, input$filterTag, 
         input$cbSearchMode, input$cbFreeTextCasesense, input$cbSearchOperator), 
    {
      req(LiveKpi())
    
      if (isTruthy(input$filterDomain) || isTruthy(input$filterFree) || isTruthy(input$filterTag)) {    
        RowFilter <- CreateFilter(kpi, input$filterFree, input$filterDomain, input$filterTag)
        LiveKpi(kpi[RowFilter, ])
      }
      else
        LiveKpi(kpi)
  })
  
  
  
  # OUTPUT ===================
  
  # Render a tile with a KPI
  MakeKpiBox <- function(x) { # x is a kpi row
    domainLabel <- ifelse(length.tagstr(x$domain) > 1, "Domains", "Domain")

    UnitIcon <- mapUnit2Icon(x$unit)
    UnitIcon <- tagAppendAttributes(UnitIcon, class="tileicon")
    DirectionIcon <- mapDirection2Icon(x$direction)
    DirectionIcon <- tagAppendAttributes(DirectionIcon, class="tileicon")
    
    if (nchar(x$description) > 280L) 
      descr <- paste(substr(stripHTML_a(x$description), 1L, 280L), "…")
    else
      descr <- HTML(x$description)
    
    Box <- shinydashboard::box(
      id = x$id,
      width = 6L,
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
  
  
  # Render `KpiList` as table
  output$KpiTable <- DT::renderDataTable({
      #req(LiveKpi()) # not needed because this is nested in out..$KpiList
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
  
  
  # Output for the KPI list which is either a table or a list of tiles
  output$KpiList <- renderUI({
    shiny::validate(need(LiveKpi(), "KPi Kluster did not manage to load any KPIs"))
      
    if (input$KpiViewingMode == "table") {
      DT::dataTableOutput("KpiTable")
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
  # ADMIN SECTION ========================
  output$AdminMenu <- renderMenu(
    if (RunningMode == "Admin") {
      menuItem("Admin", tabName = "AdminArea", icon = icon("users-cog", verify_fa = FALSE))
    }
  )
  if (RunningMode == "Admin") {
    KlusterAdminServer("KpiKlusterAdminSection", kpi = kpi)
  }

  
  
  #
  #
  #
  # HEADER SECTION ========================
  
  
  output$uiAuthorAbout <- renderUser({
    dashboardUser(
      name = "About me", 
      image = "https://www.seifseit.de/images/Jan2013c.jpg", 
      title = "Dr. Jan Seifert",
      subtitle = "", 
      footer = p("This is a personal project by me under the patronage of", 
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
  
  # Apply a filter from a given preset
  ReadSetFromUrl <- observe({
    SetId <- "set"
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[[SetId]])) {
      Presets <- readKpiPresets("./www/presets.json")
      
      if (is.null(Presets))  return
      
      for (set in Presets) {
        if (paste0("\"", set$id, "\"") == query[[SetId]]) {
          Domains <- parseDomains(LiveKpi()$domain)
          updatePickerInput(
            session = session, inputId = "filterDomain",
            choices = Domains, selected = tolower(set$domains)
          )
          
          Tags <- parseTags(LiveKpi()$tags)
          updatePickerInput(
            session = session, inputId = "filterTag",
            choices = Tags, selected = tolower(set$tags))
          
          break
        }
      }
    }
    # Destroy itself - once used, it is not needed anymore
    ReadSetFromUrl$destroy()
  })
}


# Run the application 
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)
