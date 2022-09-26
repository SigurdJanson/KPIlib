library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(jsonlite)
library(DT)

source("utils.R")



RunningMode <- FALSE #"Admin"






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
    width = 280L, overlay = FALSE,
    div(class="content",
      searchInput(
        inputId = "filterFree", label = "Find a KPI",
        placeholder = "Enter text",
        btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
        width = "auto"
      ),
      pickerInput(
        inputId = "filterName",
        label = "Names", 
        choices = "",
        options = list(
          `actions-box` = TRUE, size = 10L
        ),
        multiple = TRUE,
        width = "100%"
      ),
      pickerInput(
        inputId = "filterTag",
        label = "Tags", 
        choices = "",
        options = list(
          `actions-box` = TRUE, size = 10L
        ),
        multiple = TRUE,
        width = "100%"
      ),
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
        infoBoxOutput("AdminDuplicateCount", width = 4L),
        box(tableOutput("AdminDuplicates"), "Duplicated Titles", width = 4L, height = 400),
        box(tableOutput("AdminUppwerLowerTags"), "Tags", width = 4L, height = 400)
      )
    )
  )#,
  
  #footer = dashboardFooter(left = "", right = "Created by Jan Seifert")
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
  
  
  
  observeEvent(list(input$filterName, input$filterFree, input$filterTag), {
    RowFilter <- rep(FALSE, nrow(kpi))
    if (isTruthy(input$filterName) || isTruthy(input$filterFree) || isTruthy(input$filterTag)) {
      
      if (isTruthy(input$filterName))
        RowFilter <- RowFilter | kpi$name %in% input$filterName
      if (isTruthy(input$filterFree))
        RowFilter <- RowFilter | grepl(input$filterFree, kpi$title, fixed = TRUE)
      if (isTruthy(input$filterTag))
        RowFilter <- RowFilter | grepl(input$filterTag, kpi$tags, fixed = TRUE)
      
      LiveKpi(kpi[RowFilter, ])
    }
    else
      LiveKpi(kpi)
  })
  
  
  
  # OUTPUT ===================
  
  MakeKpiBox <- function(x) { # x is a kpi row
    # TODO: Icons: "Distance", "Energy", "Money", "Number", "Percentage", "Ratio", 
    # "Score", "Time", "Volume", "Weight", "Weight/m²" 
    UnitIcon <- mapUnit2Icon(x$unit)
    UnitIcon <- tagAppendAttributes(UnitIcon, style="font-size: 32px")
    
    if (!is.null(x$formula_description)) {
      if (grepl("$$", x$formula_description, fixed=TRUE)) {
        Formula <- p(withMathJax(x$formula_description))
      } else {
        Formula <- p(x$formula_description, class="truncate")
      }
    } else {
      Formula <- p("Not available", class="h-inline")
    }
    
    flipBox(
      id = x$title,
      width = 4L,
      front = fixedRow(
        column(10L,
          class = "text-left",
          height = "340px",
          h4(x$title, class="truncate"),
          p(x$description),
          div(span("domain", class="h-inline"), 
              span(x$name, class="highlight"))
        ), 
        column(2L, UnitIcon),
        class = "box-body"
      ),
      back = fixedRow( 
        column(11L,
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
        fluidRow(Boxes)
      )
      return(.html)
    }
  })

  
  #
  #
  #
  # ADMIN SSECTION ========================
  #
  
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
  
  
  output$AdminUppwerLowerTags <- renderTable({
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
            icon = icon("twitter")
          )
        ),
        dashboardUserItem(
          width = 4L,
          socialButton(
            href = "https://github.com/SigurdJanson",
            icon = icon("github")
          )
        ),
        dashboardUserItem(
          width = 4L,
          socialButton(
            href = "https://de.linkedin.com/in/jan-seifert-3194951b1",
            icon = icon("linkedin")
          )
        )
      )
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
