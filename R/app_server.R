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

library(googlesheets4)
library(writexl)

library(gt)
library(ggplot2)
library(scales)
library(cowplot)


library(shiny)
library(shinydashboard)
library(shinyjs)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

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
          title = h3("Upload and Choose Standard Type"),
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
            inputId = "upload",
            label = h4("Upload"),
            multiple = F,
            placeholder = "Browse",
            accept = c(".xls")
          )
        ),
      ),
      tabItem(
        tabName = "std_qc", h3("Quality Check of Standards"),
        box(width=12, gt_output(outputId = "std_tab"))
      ),
      tabItem(
        tabName = "explore",
        plotOutput(outputId = "sam_graph", click = "sam_click"),
        verbatimTextOutput(outputId = "sam_info")
      ),
      tabItem(
        tabName = "download", h3("Download Data"),
        box(
          helpText("Download data as excel file."),
          downloadButton(outputId = "download", label = "Download")
        )
      )
    )
  })

  std_data = eventReactive(input$method, {
    read_google_std(input$method) %>%
      return()
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
  
  input_data = eventReactive(input$upload, {
    ext = tools::file_ext(input$upload$name)
    validate(need(
      ext %in% c("xls"),
      "Please upload raw Elementar data"
    ))
    load_data(input$upload$datapath) 
  })
  
  checked_std = reactive({
    req(input_data(), std_cutoff())
    check_std(input_data(), std_cutoff())
  })
  
  data = reactive({
    req(input_data(), checked_std())
    check_sample(input_data(), checked_std())
  })
  
  
  
  observe({
    ## The observe is like ta reactive accept it doesn't return anything
    ## Since we do not know how many or which files the user will be uploading the observe
    ## is constantly updating the multiple choice list with the name of the files.
    ele_list = data()$element
    updateSelectInput(
      session,
      inputId = "element",
      choices = ele_list,
      selected = NULL
    )
    output$menu = renderMenu({
      sidebarMenu(
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Standard QC", tabName = "std_qc", icon = icon("heartbeat")),
        menuItem("Data Explore", icon = icon("search"), tabName = "explore")
      )
    })
    
  })
  
  sam_data= eventReactive(
    input$sam_click,
    {
     temp =  data() %>%
        select(-std_check) %>%
        pivot_longer(-c("Name", "date_time"), names_to = "element", values_to = "value") %>%
        nearPoints(input$sam_click, xvar = "date_time", yvar = "value")
     filter(data(), Name == temp$Name[1])
      
    }
  )
    output$sam_graph = renderPlot({sam_graph_format(data(), input$element)})
    output$sam_info = renderPrint({
      sam_data() %>%
        print.data.frame()
    })
  
  output$std_tab = render_gt(std_table_create(checked_std()))
  
  # Allows the user to download data.
  output$download = downloadHandler(
    filename = "parsed_cn_data.xlsx",
    content = function(file){
      data() %>%
        write_xlsx(file)
    }
  )
}
