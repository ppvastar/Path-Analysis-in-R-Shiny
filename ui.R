library(visNetwork) 
library(shiny)
library(shinydashboard)
#library(plotly)
library(V8) 
library(shinyjs)

# Global variables can go here

# Define the UI

  header<-dashboardHeader(title="Path Analysis")
  
  market_list<-c('KR','JP')
  platform_list<-c('IOS APP','Android APP')
  
  sidebar<-dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("hr {border-top: 2px solid #F08080;}"))
    ),
    width = 200,
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { history.go(0);}"),
    
    h4("Upload data",align="center"),
    fileInput(inputId="file", "",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"),width=200),
    
    h4("Choose data alternatively",align="center"),
    
    uiOutput(outputId="choose_market"),
    uiOutput(outputId="choose_platform"),
    
    actionButton("loaddata", "Load data"),
    
    tags$hr(),
    
    h4("Visualization filters",align="center"),
    sliderInput(inputId="weight_threshold", 
                "Node filter by relative volume (power of 10)", 
                min =-10,max = 0,
                step=1,value=-10,width=200),
    
    
    sliderInput(inputId="transition_threshold", 
                "Edge filter by transition probability", 
                min = 0,max = 1,
                step=0.01,value=0,width=200),
    
    
    selectInput(inputId="edge_smooth", "Smooth edge?", 
                choices = c("FALSE","TRUE"),width=200)

  )
  
  body<-dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$hr(),
    
    fluidRow(
      
      tags$head(tags$style("#full_plot{height:90vh !important;}")),
      box(title = "Full network visualization", status = "primary", solidHeader = TRUE,collapsed = FALSE,width=12,
          collapsible = TRUE,
          h4("Visualize full Markovian network based on raw input data"),
          visNetworkOutput(outputId="full_plot")
      ),
      
      
      tags$head(tags$style("#custom_plot{height:90vh !important;}")),
      box(title = "Sub network visualization", status = "primary", solidHeader = TRUE,collapsed = TRUE,width=12,
          collapsible = TRUE,
          h4("Visualize sub network constituted by nodes existing in all posssible paths from 'start' to 'end'"),
          uiOutput(outputId="custom_from"),
          uiOutput(outputId="custom_to"),
          uiOutput(outputId="path_analysis"),
          
          visNetworkOutput(outputId="custom_plot")
      ),
      
      
      box(title = "Attribution analysis", status = "primary", solidHeader = TRUE,collapsed = TRUE,width=12,
          collapsible = TRUE,
          h4("Given 'start' and 'end' of conversion sessions, study the change in CVR if any other node is removed (1st order Markovian MTA analysis); also study CVR torwards given 'end' node from any other node"),
          uiOutput(outputId="conversion_start"),
          uiOutput(outputId="conversion_stop"),
          uiOutput(outputId="attribution_analysis"),
          tags$hr(),
          tabBox(
            # The id lets us use inut$tabset1 on the server to find the current tab
            id = "tabBox", width=12, 
            tabPanel("Conversion attribution analysis", dataTableOutput(outputId="attribution_table")),
            tabPanel("Position-dependent CVR", dataTableOutput(outputId="to_target_cvr_table"))
          )
          
      ),
      
      box(title = "Input data view", status = "primary", solidHeader = TRUE,collapsed = TRUE,width=12,
          collapsible = TRUE,
          h4("Output raw input data"),
          dataTableOutput(outputId="raw_data")
      )
      
      
    )

)
  

dashboardPage(skin="blue",header,sidebar,body)
  
