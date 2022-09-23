

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(jsonlite)

source("utils.R")

RunningMode <- FALSE#"Admin"




ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(
    title = "KPI Lib"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("KPIs", tabName = "ContentArea", icon = icon("dashboard")),
      menuItemOutput("AdminMenu")
    ),
    searchInput(
      inputId = "filterFree", label = "Find a KPI",
      placeholder = "Enter text",
      btnSearch = icon("magnifying-glass"), btnReset = icon("xmark"),
      width = "auto"
    ),
    helpText("... help ..."),
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Search..."),
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
    br(),
    div(class="form-group shiny-input-container",
      "KPIs",
      textOutput("infoNFiltered"),
      textOutput("infoNTotal")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "ContentArea",
              h2("KPIs"),
              fluidRow(
                uiOutput("KpiList")
              )
      ),
      
      tabItem(
        tabName = "AdminArea",
        h2("KPI Administration"),
        infoBoxOutput("AdminDuplicateCount", width = 4L),
        box(tableOutput("AdminDuplicates"), "Duplicated Titles", width = 4L, height = 400),
        box(tableOutput("AdminUppwerLowerTags"), "Tags", width = 4L, height = 400)
      )
    )
    
  )
)





# 
#
#
server <- function(input, output, session) {

  kpi <- readKpiData("./www/kpis.json")
  
  LiveKpi <- reactiveVal(kpi)
  
  output$infoNFiltered <- renderText(
    paste("Selected:", nrow(LiveKpi()))
  )
  output$infoNTotal <- renderText(
    paste("Available:", nrow(kpi))
  )
  
  
  
  observeEvent(LiveKpi, {
    updatePickerInput(
      session = session, inputId = "filterName",
      choices = unique(LiveKpi()$name)
    )
  })
  
  
  
  observeEvent(list(input$filterName, input$filterFree), {
    RowFilter <- rep(FALSE, nrow(kpi))
    if (isTruthy(input$filterName) || isTruthy(input$filterFree)) {
      
      if (isTruthy(input$filterName))
        RowFilter <- RowFilter | kpi$name %in% input$filterName
      if (isTruthy(input$filterFree))
        RowFilter <- RowFilter | grepl(input$filterFree, kpi$title, fixed = TRUE)
      
      LiveKpi(kpi[RowFilter, ])
    }
    else
      LiveKpi(kpi)
  })
  
  
  
  output$KpiList <- renderUI({
    .html <- tagList(
    statiCard(
      "KPI",
      "Long value text with icon. THis KPI is meant to measure absolutekly nothing.",
      icon = icon("gauge"),
      left = TRUE
    ),
    box(
      title = "Inputs", solidHeader = TRUE,
      "Box content here", br(), "More box content",
      sliderInput("slider", "Slider input:", 1, 100, 50),
      textInput("text", "Text input:")
    ))
    .html
  })
  
  
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
