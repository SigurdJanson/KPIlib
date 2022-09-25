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
        disabled = TRUE
      )
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
    tabItems(
      tabItem(tabName = "ContentArea",
              h2("KPIs"),
              # fluidRow(
              #   uiOutput("KpiList")
              # )
              dataTableOutput("KpiTable")
      ),
      
      tabItem(
        tabName = "AdminArea",
        h2("KPI Administration"),
        infoBoxOutput("AdminDuplicateCount", width = 4L),
        box(tableOutput("AdminDuplicates"), "Duplicated Titles", width = 4L, height = 400),
        box(tableOutput("AdminUppwerLowerTags"), "Tags", width = 4L, height = 400)
      )
    )
  ),
  
  footer = dashboardFooter(left = "", right = "Created by Jan Seifert")
)





# 
#
# SERVER ==================================
server <- function(input, output, session) {

  kpi <- readKpiData("./www/kpis.json")
  
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
  
  
  output$KpiTable <- DT::renderDataTable({
    head(LiveKpi()[, c(1, 2, 3, 4, 7, 8)], n = ShowPageLength())
  }, options = list(searching=FALSE, pageLength = ShowPageLength()))
  
  
  
  # output$KpiList <- renderUI({
  #   data <- head(LiveKpi())
  #   .html <- tagList(
  #     mapply(\(t, descr) box(title=t, solidHeader = TRUE, width = 3L, descr), data$title, data$description, SIMPLIFY = FALSE)
  #   )
  #   .html
  # })

  
  #
  #
  #
  ### ADMIN SSECTION ========================
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
}

# Run the application 
shinyApp(ui = ui, server = server)
