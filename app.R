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
    controlbar = renderFilterSideBar(),

    
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
          renderAboutContent()
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
      footer = p("This is a personal project by Jan Seifert inspired by my contributions to the", 
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
