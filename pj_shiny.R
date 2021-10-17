library(shiny)
library(DT)
library(openxlsx)
library(readxl)
 
library(ggplot2)
library(tidyverse)
library(plotly)
library(lubridate)
library(RColorBrewer)

source("pj_fig.R")

Sys.setlocale(category = "LC_TIME", locale = "C")

# Initial Matrix ----
pj.matx <- data.frame(project = character(1), task = character(1),
                      start_dt = format(Sys.Date(), "%Y-%m-%d"),
                      end_dt = format(Sys.Date(), "%Y-%m-%d"),
                      note = character(1),
                      timepoint = format(Sys.Date(), "%Y-%m-%d"))[-1,]

# Shiny UI ----
ui <- fluidPage(

  titlePanel(title = "Overview Projects Timeline", windowTitle = "TimelineOverview"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
      uiOutput("read.excel"),
      
      textInput("pj.name", label = p("Project"), value = "AAA"),
      textInput("task.name", label = p("Task"), value = "BBB"),
      
      dateInput("sdt", label = p("Start Date input"), 
                value = format(Sys.Date(), "%Y-%m-%d")),
      
      dateInput("edt", label = p("End Date input"), 
                value = format(Sys.Date(), "%Y-%m-%d")),
      
      textInput("note.name", label = p("Note"), value = "DBL"),
      dateInput("note.tp", label = p("Timepoint"), 
                value = format(Sys.Date(), "%Y-%m-%d")),
      
      actionButton("Add", "Add", style = "padding:6px; font-size:100%"),
      hr(),
      uiOutput("note.uilist"),
      
      textInput("ui.file.name", label = p("Excel File Name"), value = "timeline"),
      downloadButton("download_excel", "Download (*xlsx)", style = "padding:6px; font-size:100%")
      
    ),
    
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Project Timeline Table",
                 DT::dataTableOutput("ds.edit")
        ),
        tabPanel("Project Timeline Figure",
          plotlyOutput("pj.plotly", height = 600)
        )
      )

    )
  )
)

# Shiny Server ----
server <- function(input, output) {
  
  # Import Excel ---- 
  output$read.excel <- renderUI({
    if (is.null(values$pj.matx)) {
      fileInput("in.file", "Choose xlsx file", accept = c(".xlsx"))
    }
  })

  rd.xlsx <- reactive({
    if (!is.null(input$in.file)) {
      read_excel(input$in.file$datapath)
    }
  })

  # Invoke initial matrix ----
  values <- reactiveValues()
  values$pj.matx <- pj.matx
  
  addData <- observe({
    
    if (input$Add == 0) {
      values$pj.matx <- rd.xlsx()
    } else if (input$Add > 0) {
      # create the new line to be added from your inputs
      new.line <- isolate(
        data.frame(project = input$pj.name, task = input$task.name,
                   start_dt = format(as.Date(input$sdt), "%Y-%m-%d"), 
                   end_dt = format(as.Date(input$edt), "%Y-%m-%d"),
                   note = input$note.name, 
                   timepoint = format(as.Date(input$note.tp), "%Y-%m-%d"), 
                   stringsAsFactors= FALSE)
      )
      
      values$pj.matx <- isolate(rbind(values$pj.matx, new.line))
    }
  })
  
  # Display list of note ----    
  output$note.uilist <- renderUI({
    if (!is.null(values$pj.matx)) {
      selectInput(
        inputId = "note.list.in",
        label = "Choose highlighted Note (Max: 9 Notes)",
        choices = values$pj.matx[,5],
        selected = NULL,
        multiple = TRUE
      )
    }
  })
  
  output$ds.edit <- DT::renderDataTable({
    DT::datatable(values$pj.matx, editable = "cell")
  })
  
  proxy <- dataTableProxy("ds.edit")
  observeEvent(input$ds.edit_cell_edit, {
    info = input$ds.edit_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    
    values$pj.matx[i, j] <- isolate(DT::coerceValue(v, values$pj.matx[i, j]))
  })
  
  # Export to Excel ----
  output$download_excel <- downloadHandler(
    filename = function() {
      paste(input$ui.file.name, ".xlsx", sep = "")
    },
    content = function(file) {
      pj.workbook <- createWorkbook()
      
      addWorksheet(
        wb = pj.workbook,
        sheetName = "Project Timeline"
      )
      
      writeData(
        pj.workbook,
        sheet = 1,
        values$pj.matx,
        startRow = 1,
        startCol = 1
      )
      
      saveWorkbook(pj.workbook, file)
    }
  )
  
  # Draw Time-line Figure ----
  output$pj.plotly <- renderPlotly({
    if (!is.null(values$pj.matx)) {
      pj.timeline.fig(
        ui.excel = values$pj.matx, 
        ui.highlight.task = input$note.list.in)
    }
  })
}

shinyApp(ui = ui, server = server)