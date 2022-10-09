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





# UI ==================================
ui <- dashboardPage(
  skin = "black", # try "midnight" eventually
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = "KPi Kluster",
    leftUi = tagList(
      radioGroupButtons(
        inputId = "KpiViewingMode",
        choices = c(`<i class='fa fa-table'></i>` = "table", 
                    `<i class="fa fa-grip-horizontal"></i>` = "grid"),
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
      menuItem("KPIs", tabName = "ContentArea", icon = icon("dashboard", verify_fa = FALSE)),
      menuItemOutput("AdminMenu"),
      menuItem("About KPi Kluster", tabName = "AboutApp", icon = icon("hand-sparkles", verify_fa = FALSE))
    )
  ),
  
  ## filter control side bar =====
  controlbar = dashboardControlbar(
    width = 280L, overlay = FALSE, collapsed = FALSE,
    div(class="content",
        tags$div(title="Search title and description fields",
                 searchInput(
                   inputId = "filterFree", label = "Find a KPI",
                   placeholder = "Enter text to search for KPIs",
                   btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
                   width = "auto"
                 )),
        #tooltip("filterFree", "Search title and description fields"),
        tags$div(title="If activated the search will be case sensitive",
                 checkboxGroupButtons(
                   inputId = "cbCaseSensitivity", label = NULL,
                   choices = c(`<b>Aa</b>` = "CaseSensitive") # fa-font-case
                 )),
        # tooltip("cbCaseSensitivity", "If activated this option the search will be case sensitive"),
        
        tags$div(title="Filter for one or several domains",
                 pickerInput(
                   inputId = "filterName",
                   label = "Domain", 
                   choices = "",
                   options = list(
                     `actions-box` = TRUE, size = 10L,
                     `live-search` = TRUE, `dropdown-align-right` = "auto"
                   ),
                   multiple = TRUE,
                   width = "100%"
                 )),
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
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # ),
    includeCSS("www/custom.css"),
    setShadow(class = "dropdown-menu"),
    tabItems(
      tabItem(tabName = "ContentArea",
              h2("KPIs"),
              uiOutput("KpiList")
              #dataTableOutput("KpiTable")
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
          infoBoxOutput("AdminLonelyNameCount", width = 12L),
          box(tableOutput("AdminLonelyNames"), "Lonely Names", 
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
              p("These data were extensively improved by the author. Over 2000 KPIs were merged 
                or eliminated."),
              tags$ul(
                tags$li("Removal of duplicate indicators"),
                tags$li("Removal of indicators not complete enough to be understandable"),
                tags$li("Spelling correction"),
                tags$li("Removal of \"lonely\" tags that existed only once for clarity"),
                tags$li("Combination of redundancies between KPIs for clarity"),
                tags$li("Correction of empty fields")
              ),
              p("Despite all these efforts the library may still contain ambiguities, vagueness, 
                and mistakes. It was not possible so far to fill all missing information.",
                tags$b("Please note that you use KPi Kluster at your own risk.")),
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
      )
    )
  )
)





# 
#
# SERVER ==================================
server <- function(input, output, session) {

  kpi <- readKpiData(c("./www/kpis.json", "./www/kpis_digitalproducts.json"))
  kpi <- filterKpiData(kpi)
  
  LiveKpi <- reactiveVal(kpi)
  
  ShowPageLength <- reactiveVal(20L)

  output$dataInfo <- renderTable({
    filterCount <- sum(sapply(c(input$filterName, input$filterFree, input$filterTag), isTruthy))
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
    # Names <- LiveKpi()$name |>
    #   unique() |>
    #   sort()
    Names <- sapply(LiveKpi()$name, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      tolower() |>
      na.omit() |>
      unique() |>
      sort()
    
    updatePickerInput(
      session = session, inputId = "filterName",
      choices = Names
    )
    
    Tags <- sapply(LiveKpi()$tags, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      tolower() |>
      na.omit() |>
      unique() |>
      sort()
    
    updatePickerInput(
      session = session, inputId = "filterTag", choices = Tags)
  }, ignoreInit = FALSE)
  
  
  
  observeEvent(list(input$filterName, input$filterFree, input$filterTag, input$cbCaseSensitivity), {
    IgnoreCase <- length(input$cbCaseSensitivity) == 0

    RowFilter <- rep(FALSE, nrow(kpi))
    if (isTruthy(input$filterName) || isTruthy(input$filterFree) || isTruthy(input$filterTag)) {
      
      if (isTruthy(input$filterFree))
        RowFilter <- RowFilter | 
          grepl(escapeRegex(input$filterFree), kpi$title, 
                fixed = FALSE, ignore.case = IgnoreCase)  | 
          grepl(escapeRegex(input$filterFree), kpi$description, 
                fixed = FALSE, ignore.case = IgnoreCase) | 
          grepl(escapeRegex(input$filterFree), kpi$interpretation, 
                fixed = FALSE, ignore.case = IgnoreCase)

      if (isTruthy(input$filterName)) {
        if (length(input$filterName) > 1)
          RowFilter <- RowFilter | apply(input$filterName %isin% kpi$name, 1L, any)
        else
          RowFilter <- RowFilter | input$filterName %isin% kpi$name
      }
      #  RowFilter <- RowFilter | kpi$name %in% input$filterName
      
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
    UnitIcon <- mapUnit2Icon(x$unit)
    UnitIcon <- tagAppendAttributes(UnitIcon, class="tileicon")
    
    if (isTruthy(x$formula)) {
      if (grepl("$$", x$formula, fixed=TRUE)) {
        Formula <- p(withMathJax(x$formula))
      } else {
        Formula <- p(x$formula, class="truncate")
      }
    } else {
      Formula <- NULL # Formula <- p("Formula not available", class="h-inline")
    }
    
    Box <- flipBox(
      id = x$title,
      width = 6L, # width: class="col-sm-6"
      front = fixedRow(
        column(10L,
          class = "text-left",
          #height = "400px",
          div(
            h4(x$title, class="truncate"),
            title=x$title
          ),
          p(x$description),
          div(span("domain", class="h-inline"), 
              span(x$name, class="highlight"))
        ), 
        column(2L, UnitIcon),
        class = "box-body"
      ),
      back = fixedRow(
        column(12L,
          class = "text-left",
          h4(x$title, class="truncate"),
          Formula,
          tags$table(
            tags$tr(
              tags$td(span("direction", class="h-inline")), 
              tags$td(span(x$direction, class="highlight"))
            ),
            tags$tr(
              tags$td(span("tags", class="h-inline")),
              tags$td(span(x$tags, class="highlight"))
            )
          )
        ),
        class = "box-body"
      )
    )#flipbox
    
    Box <- tagAppendAttributes(Box, class="col-xs-12 col-md-6 col-lg-4")
    Box
  }
  
  
  
  output$KpiTable <- DT::renderDataTable({
      result <- head(LiveKpi()[, c(1, 2, 3, 4, 7, 8)], n = ShowPageLength())
      # rename
      index <- which(colnames(result) == "name")
      colnames(result)[index] <- "domain"
      result
    }, 
    options = list(
      searching=FALSE, 
      paging = FALSE, 
      pageLength = ShowPageLength(), info=FALSE), 
    selection = "single"
  )
  
  
  
  output$KpiList <- renderUI({
    if (input$KpiViewingMode == "table") {
      dataTableOutput("KpiTable")
    } else if (input$KpiViewingMode == "grid") {
      data <- head(LiveKpi(), n = ShowPageLength())
      Boxes <- tagList()
      for (i in 1:nrow(data)) {
        Boxes <- c(Boxes, tagList(MakeKpiBox(data[i,])))
      }
      #Boxes <- tagList(Boxes)
      
      .html <- tagList(
        fluidPage(fluidRow(Boxes))
      )
      return(.html)
    }
  })
  
  
  observeEvent(input$KpiTable_rows_selected, {
    req(input$KpiTable_rows_selected)
    showModal(KpiDetailsModal(LiveKpi()[input$KpiTable_rows_selected, ]))
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
  

    
  ## Lonely Domains =====
  AdminLonelyNames <- reactive({
    Names <- sapply(kpi$name, \(x) strsplit(x, ",")) |>
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
      "Lonely Domains", InfoTxt, "Domains existing only once",
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
    Count <- nrow(kpi) - sum(sapply(kpi$name, isTruthy))
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
  # HEADER SSECTION ========================
  
  
  output$uiAuthorAbout <- renderUser({
    dashboardUser(
      name = "Author", 
      image = "https://www.seifseit.de/images/Jan2013c.jpg", 
      title = "Dr. Jan Seifert",
      subtitle = "", 
      footer = p("A service secretly supported by", 
                 tags$a("changitors", href="https://www.changitors.com/en"), 
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
