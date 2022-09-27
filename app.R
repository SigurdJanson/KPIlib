library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(jsonlite)
library(DT)

source("utils.R")
source("tagstr.R")


RunningMode <- "Admin"






# UI ==================================
ui <- dashboardPage(
  skin = "black", # try "midnight" eventually
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = "KPI Lib",
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
    dropdownBlock(
      id = "uiAppABout",
      title = "About KPIlab",
      icon = icon("chalkboard", verify_fa = FALSE),
      badgeStatus = NULL,
      div("")
    ),
    controlbarIcon = shiny::icon("filter")
  ),
  
  sidebar = dashboardSidebar(
    minified = (RunningMode == "Admin"), collapsed = TRUE,
    sidebarMenu(
      menuItem("KPIs", tabName = "ContentArea", icon = icon("dashboard", verify_fa = FALSE)),
      menuItemOutput("AdminMenu")
    )
  ),
  
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
                     `live-search` = TRUE, dropdownAlignRight = TRUE
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
                     `live-search` = TRUE, dropdownAlignRight = "auto"
                   ),
                   multiple = TRUE,
                   width = "100%"
        )),
        br(),
        "KPIs",
        textOutput("infoNFiltered"),
        textOutput("infoNTotal")
    )
  ),
  
  
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
          infoBoxOutput("AdminDuplicateCount", width = 12L),
          box(tableOutput("AdminDuplicates"), "Duplicated Titles", width = 12L, height = 400)
        ),
        column(3L,
          box(tableOutput("AdminUpperLowerTags"), "Mixed Case Tags", width = 12L, height = 400),
          infoBoxOutput("AdminLonelyTagCount", width = 12L),
          box(tableOutput("AdminLonelyTags"), "Lonely Tags", width = 12L, height = 400)
        ),
        column(3L,
          infoBoxOutput("AdminLonelyNameCount", width = 12L),
          box(tableOutput("AdminLonelyNames"), "Lonely Titles", width = 12L, height = 400)
        )
      )
    )
  )
)





# 
#
# SERVER ==================================
server <- function(input, output, session) {

  kpi <- readKpiData(c("./www/kpis.json", "./www/kpis_digitalproducts.json"))
  
  LiveKpi <- reactiveVal(kpi)
  
  ShowPageLength <- reactiveVal(25L)
  
  output$infoNFiltered <- renderText(
    paste("Selected:", nrow(LiveKpi()))
  )
  output$infoNTotal <- renderText(
    paste("Available:", nrow(kpi))
  )
  
  
  
  
  #
  #
  #
  #
  # KPI FILTER ========================
  
  # Update drop down lists once it's been initialized
  observeEvent(LiveKpi, {
    Names <- LiveKpi()$name |>
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
      
      if (isTruthy(input$filterName))
        RowFilter <- RowFilter | kpi$name %in% input$filterName

      if (isTruthy(input$filterFree))
        RowFilter <- RowFilter | 
          grepl(input$filterFree, kpi$title, 
                fixed = FALSE, ignore.case = IgnoreCase)

      if (isTruthy(input$filterTag)) {
        if (length(input$filterTag) > 1)
          RowFilter <- RowFilter | apply(input$filterTag %isin% kpi$tags, 1L, any)
        else
          RowFilter <- RowFilter | input$filterTag %isin% kpi$tags
        #RowFilter <- RowFilter | grepl(input$filterTag, kpi$tags, fixed = TRUE)
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
    
    if (isTruthy(x$formula_description)) {
      if (grepl("$$", x$formula_description, fixed=TRUE)) {
        Formula <- p(withMathJax(x$formula_description))
      } else {
        Formula <- p(x$formula_description, class="truncate")
      }
    } else {
      Formula <- p("Formula not available", class="h-inline")
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
          Formula, #if (isTruthy(Formula)) 
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
    head(LiveKpi()[, c(1, 2, 3, 4, 7, 8)], n = ShowPageLength())
  }, options = list(searching=FALSE, pageLength = ShowPageLength()))
  
  
  
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

  
  #
  #
  #
  # ADMIN SSECTION ========================

  
  output$AdminMenu <- renderMenu(
    if (RunningMode == "Admin") {
      menuItem("Admin", tabName = "AdminArea", icon = icon("dashboard"))
    }
  )
  
  
  output$AdminDuplicateCount <- renderInfoBox({
    Count <- sum(duplicated(kpi$title))
    Icon <- if (Count < nrow(kpi) * 0.1) 
              icon("thumbs-up", lib = "glyphicon")
            else
              icon("thumbs-down", lib = "glyphicon")
    Color <- if (Count < nrow(kpi) * 0.1)
               "green"
             else
               "yellow"
    infoBox(
      "Duplicates", Count, "Duplicated by Title",
      icon = Icon,
      color = Color
    )
  })
  
  output$AdminDuplicates <- renderTable({
    Dups <- kpi$title |> duplicated()
    Dups <- kpi$title[Dups] |> unique()
    data.frame(Duplicates = Dups)
  })
  
  
  
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
    Icon <- if (Count < nrow(kpi) * 0.01) 
      icon("thumbs-up", lib = "glyphicon")
    else
      icon("thumbs-down", lib = "glyphicon")
    Color <- if (Count < nrow(kpi) * 0.01)
      "green"
    else
      "yellow"
    
    infoBox(
      "Lonelies", Count, 
      "Singular Names",
      icon = Icon,
      color = Color
    )
  })
  
  output$AdminLonelyNames <- renderTable({
    return(data.frame(Lonelies = AdminLonelyNames()))
  })
  
  
  output$AdminUpperLowerTags <- renderTable({
    Tags <- sapply(LiveKpi()$tags, \(x) strsplit(x, ",")) |>
      unlist() |>
      unname() |>
      trimws() |>
      na.omit() |>
      unique()
    TagsIroned <- tolower(Tags)
    Dups <- TagsIroned |> duplicated()
    data.frame(Duplicates = Tags[Dups])
  })
  
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
    Icon <- if (Count < nrow(kpi) * 0.1) 
      icon("thumbs-up", lib = "glyphicon")
    else
      icon("thumbs-down", lib = "glyphicon")
    Color <- if (Count < nrow(kpi) * 0.1)
      "green"
    else
      "yellow"
    
    infoBox(
      "Lonelies", Count, 
      "Singular Tags",
      icon = Icon,
      color = Color
    )
  })
  
  output$AdminLonelyTags <- renderTable({
    return(data.frame(Lonelies = AdminLonelyTags()))
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
      footer = p("The footer", class = "text-center"),
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
