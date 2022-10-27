KlusterAdminUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("KPI Administration"),
    column(
      3L,
      infoBoxOutput(ns("ExactDuplicateCount"), width = 12L),
      infoBoxOutput(ns("LongTitleCount"), width = 12L),
      infoBoxOutput(ns("RedundantDescCount"), width = 12L),
      infoBoxOutput(ns("TitleDuplicateCount"), width = 12L),
      box(tableOutput(ns("TitleDuplicates")), "Duplicated Titles", 
          width = 12L, height = 400, collapsible = TRUE)
    ),
    column(
      3L,
      h3("Domains"),
      infoBoxOutput(ns("DomainCount"), width = 12L),
      infoBoxOutput(ns("LonelyNameCount"), width = 12L),
      box(tableOutput(ns("LonelyNames")), "Lonely Domains", 
          width = 12L, height = 400, collapsible = TRUE)
    ),
    column(
      3L,
      h3("Tags"),
      infoBoxOutput(ns("TagCount"), width = 12L),
      infoBoxOutput(ns("UpperLowerTagCount"), width = 12L),
      box(tableOutput(ns("AdminUpperLowerTags")), "Mixed Case Tags", 
          width = 12L, height = 400, collapsible = TRUE),
      infoBoxOutput(ns("LonelyTagCount"), width = 12L),
      box(tableOutput(ns("LonelyTags")), "Lonely Tags", 
          width = 12L, height = 400, collapsible = TRUE)
    ),
    column(
      3L,
      infoBoxOutput(ns("MissingNameCount"), width = 12L),
      infoBoxOutput(ns("MissingDescrCount"), width = 12L),
      infoBoxOutput(ns("MissingUnitCount"), width = 12L),
      infoBoxOutput(ns("MissingTagsCount"), width = 12L),
      infoBoxOutput(ns("MissingFormulaCount"), width = 12L)
    )
  )
}



KlusterAdminServer <- function(id, kpi = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      .InfoBox <- function(Count, Total, Thresh1, Thresh2, Name) {
        if (Count == 0) {
          Icon <- icon("medal")
          Color <- "green"
          InfoTxt <- paste(Name, ": None")
        } else if (Count < Thresh1) {
          Icon <- icon("thumbs-up", lib = "glyphicon")
          Color <- "yellow"
          InfoTxt <- sprintf("Less than %5.1f", Thresh1)
        } else if (Count < Thresh2) {
          Icon <- icon("thumbs-down", lib = "glyphicon")
          Color <- "yellow"
          InfoTxt <- sprintf("Less than %5.1f", Thresh2)
        }else {
          Icon <- icon("thumbs-down", lib = "glyphicon")
          Color <- "red"
          InfoTxt <- sprintf("MORE than %5.1f", Thresh2)
        }
        
        infoBox(
          Name, 
          paste0("#", Count, "=", format(Count/Total*100, digits = 2, nsmall = 1), "%"), 
          InfoTxt,
          icon = Icon, color = Color
        )
        
      }
      
      ## Exact Duplicates =====
      output$ExactDuplicateCount <- renderInfoBox({
        req(kpi)
        .InfoBox(Count = sum(duplicated(kpi)),
                 Total = nrow(kpi), 
                 Thresh1 = 5L, Thresh2 = 10L,
                 Name = "Exact Duplicates")
      })
      
      
      
      ## Title Duplicates =====
      output$TitleDuplicateCount <- renderInfoBox({
        req(kpi)
        .InfoBox(Count = sum(duplicated(tolower(kpi$title))), 
                 Total = nrow(kpi), 
                 Thresh1 = nrow(kpi) * 0.05,
                 Thresh2 = nrow(kpi) * 0.10,
                 Name = "Title Duplicates")
      })
      output$TitleDuplicates <- renderTable({
        Dups <- kpi$title |> tolower() |> duplicated()
        Dups <- kpi$title[Dups] |> unique()
        data.frame(Duplicates = Dups)
      })
      
      
      
      
      ## Long Titles =====
      output$LongTitleCount <- renderInfoBox({
        req(kpi)
        Threshold <- 40L

        .InfoBox(Count = sum(nchar(kpi$title) > Threshold), 
                 Total = nrow(kpi), 
                 Thresh1 = nrow(kpi) * 0.05,
                 Thresh2 = nrow(kpi) * 0.10,
                 Name = "Long Titles")

      })
      
      ## Redundant Descriptions =====
      output$RedundantDescCount <- renderInfoBox({
        Distance <- mapply(adist, x$title, x$description, 
                           MoreArgs = list(ignore.case = TRUE))
        .InfoBox(Count = sum(Distance < 5L, na.rm = TRUE), 
                 Total = nrow(kpi), 
                 Thresh1 = nrow(kpi) * 0.05,
                 Thresh2 = nrow(kpi) * 0.10,
                 Name = "Redundant Descriptions")
      })
      
      
      ## Domains =====
      output$DomainCount <- renderInfoBox({
        Doms <- sapply(kpi$domain, \(x) strsplit(x, ",")) |>
          unlist() |>
          unname() |>
          trimws() |>
          na.omit() |> tolower() |> unique()
        infoBox(
          "Total Domains", length(Doms), 
          "All domains",
          icon = icon("flag"), color = "black"
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
      
      output$LonelyNameCount <- renderInfoBox({
        .InfoBox(Count = length(AdminLonelyNames()), 
                 Total = nrow(kpi), 
                 Thresh1 = nrow(kpi) * 0.01,
                 Thresh2 = nrow(kpi) * 0.05,
                 Name = "Lonely Domains")

      })
      
      output$LonelyNames <- renderTable({
        return(data.frame(Lonelies = AdminLonelyNames()))
      })
      
      
      ## Tags  =====
      AdminUpperLowerTags <- reactive({
        Tags <- sapply(kpi$tags, \(x) strsplit(x, ",")) |>
          unlist() |>
          unname() |>
          trimws() |>
          na.omit() |>
          unique()
        TagsIroned <- tolower(Tags)
        TagsIroned
      })
      
      output$TagCount <- renderInfoBox({
        Tags <- AdminUpperLowerTags() |> tolower() |> unique()
        infoBox(
          "Total tags", length(Tags), 
          "All Tags",
          icon = icon("flag"), color = "black"
        )
      })
      
      output$UpperLowerTagCount <- renderInfoBox({
        Dups <- AdminUpperLowerTags() |> duplicated()
        .InfoBox(Count = sum(Dups), 
                 Total = nrow(kpi), 
                 Thresh1 = nrow(kpi) * 0.05,
                 Thresh2 = nrow(kpi) * 0.10,
                 Name = "Mixed Tags")

      })
      
      
      output$UpperLowerTags <- renderTable({
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
      
      
      output$LonelyTagCount <- renderInfoBox({
        Count <- length(AdminLonelyTags())
        
        if (Count < nrow(kpi) * 0.05) {
          Icon <- icon("thumbs-up", lib = "glyphicon")
          Color <- "green"
          InfoTxt <- "Less than 5%"
        } else if (Count < nrow(kpi) * 0.10) {
          Icon <- icon("thumbs-down", lib = "glyphicon")
          Color <- "yellow"
          InfoTxt <- "Less than 10%"
        } else {
          Icon <- icon("thumbs-down", lib = "glyphicon")
          Color <- "red"
          InfoTxt <- "MORE than 10%"
        }
        
        infoBox(
          "Lonely Tags", InfoTxt, 
          paste("# existing only once", Count),
          icon = Icon, color = Color
        )
      })
      
      
      output$LonelyTags <- renderTable({
        return(data.frame(Lonelies = AdminLonelyTags()))
      })
      
      
      
      
      ## Missing Pieces ======
      output$MissingDescrCount <- renderInfoBox({
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
      
      
      output$MissingNameCount <- renderInfoBox({
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
      
      
      
      output$MissingTagsCount <- renderInfoBox({
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
      
      
      output$MissingUnitCount <- renderInfoBox({
        Count <- nrow(kpi) - sum(sapply(kpi$unit, isTruthy))
        what <- ""
        
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
          "Missing Units", InfoTxt, 
          paste0("#", Count, "=", format(Count/nrow(kpi)*100, digits = 2, nsmall = 1), "%"),
          icon = Icon, color = Color
        )
      })
      
      
      output$MissingFormulaCount <- renderInfoBox({
        Count <- nrow(kpi) - sum(sapply(kpi$formula, isTruthy))
        what <- ""
        
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
      
    }
  )
}