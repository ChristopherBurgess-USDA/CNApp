#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(rvest)
library(readxl)

library(googlesheets4)
library(writexl)

library(gt)
library(ggplot2)
library(scales)
library(cowplot)


library(shiny)
library(shinydashboard)
library(shinyjs)

app_server <- function( input, output, session ) {
  # Your application server logic 
  
  output$menu = renderMenu({
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload"))
    )
  })
  
  output$tabs = renderUI({
    tabItems(
      tabItem(
        tabName = "upload",
        box(
          title = h3("Upload Data and Choose Standard Type"),
          selectInput(
            label = h4("Choose Standard Type"), inputId = "method",
            choices = list(
              " " = "",
              "NAPT" = "NAPT",
              "CTFS" = "CTFS"
            ),
            selected = ""
          ),
          fileInput(
            inputId = "upload_std",
            label = h4("Upload Standards"),
            multiple = F,
            placeholder = "Browse",
            accept = c(".xlsx")
          ),
          fileInput(
            inputId = "upload",
            label = h4("Upload Elmentar Data"),
            multiple = F,
            placeholder = "Browse",
            accept = c(".xls")
          )
        ),
      ),
      tabItem(
        tabName = "std_qc", h3("Control Plots"),
        div(style = "margin: auto; width: 80%", uiOutput("date_slider")),
        plotOutput(outputId = "std_gg")
      ),
      tabItem(
        tabName = "explore", h3("Run Standards and Data QC"),
        gt_output(outputId = "std_tab"),
        plotOutput(outputId = "sam_graph", click = "sam_click"),
        verbatimTextOutput(outputId = "sam_info")
      ),
      tabItem(
        tabName = "download", h3("Download Data"),
        box(
          downloadButton(outputId = "download", label = "Download Data")
        ),
        box(
          downloadButton(outputId = "download_std", label = "Download Updated Standards")
        )
      )
    )
  })

  import_std_data = eventReactive(input$upload_std, {
    read_excel(input$upload_std$datapath)
  })
  std_data = reactive({
    req(import_std_data)
    import_std_data() %>%
      pivot_longer(
        -date_time,
        names_to = "element", values_to = "value"
      ) %>%
      filter(!is.na(value))
  })
  
  input_data = eventReactive(input$upload, {
    ext = tools::file_ext(input$upload$name)
    validate(need(
      ext %in% c("xls"),
      "Please upload raw Elementar data"
    ))
    load_data(input$upload$datapath) 
  })
  
  std_cutoff = reactive({
    req(std_data)
    std_data() %>%
      group_by(element) %>%
      summarise(
        mean = mean(value, na.rm = T),
        stderr = sd(value, na.rm = T)
      ) %>%
      mutate(
        act_low = mean-3*stderr,
        warn_low = mean-2*stderr,
        warn_high = mean+2*stderr,
        act_high = mean+2*stderr
      ) %>%
      return()
  })
  
  
  checked_std = reactive({
    req(input_data(), std_cutoff())
    check_std(input_data(), std_cutoff())
  })
  
  data = reactive({
    req(input_data(), checked_std())
    check_sample(input_data(), checked_std())
  })
  
  updated_std = reactive({
    req(import_std_data(), checked_std())
    update_std(import_std_data(), checked_std())
  })
  
  observe({
    req(input$method, input$upload, input$upload_std)
    output$menu = renderMenu({
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Control Plots", tabName = "std_qc", icon = icon("chart-line")),
        menuItem("Data QC", icon = icon("heartbeat"), tabName = "explore")
      )
    })
  })

  output$date_slider <- renderUI({
    req(updated_std())
    dates <- updated_std()$date_time
    sliderInput(
      "date_range", label = h4("Select Date Range to Plot"),
      min = min(dates), max = max(dates),
      value = c(min(dates), max(dates)), width = "100%"
    )
  })

  
  
  
  sam_data= eventReactive(
    input$sam_click,
    {
     temp =  data() %>%
        select(-std_check) %>%
        pivot_longer(
          -c("Name", "date_time"), names_to = "element", values_to = "value"
        ) %>%
        nearPoints(input$sam_click, xvar = "date_time", yvar = "value")
     filter(data(), Name == temp$Name[1])
      
    }
  )
  
  output$sam_graph = renderPlot({sam_graph_format(data())})
  

  
  output$sam_info = renderPrint({
    sam_data() %>%
      print.data.frame()
  })
  

  
  output$std_tab = render_gt(std_table_create(checked_std()))
  
  output$std_gg = renderPlot({
    std_graph_format(updated_std(), std_cutoff(), input$date_range)
  })
  

  
  output$sam_graph = renderPlot({sam_graph_format(data())})
  output$sam_info = renderPrint({
    sam_data() %>%
      print.data.frame()
  })
  
  # Allows the user to download data.
  output$download = downloadHandler(
    filename = paste0(format(Sys.Date(), "%Y%m%d"), "_parsed_cn_data.xlsx"),
    content = function(file){
      data() %>%
        write_xlsx(file)
    }
  )
  
  output$download_std = downloadHandler(
    filename = paste0(
      "CN", "_STD_", input$method, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"
    ),
    content = function(file){
      updated_std() %>%
        write_xlsx(file)
    }
  )
  
}
