#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("utils.R")



ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(
    title = "KPI Lib"
  ),
  sidebar = dashboardSidebar(
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
    uiOutput("KpiList")
  )
)


# Define server logic required to draw a histogram
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
}

# Run the application 
shinyApp(ui = ui, server = server)
