library(shiny)
library(dygraphs)
library(plotly)
library(janitor)
library(htmlwidgets)
library(lubridate)
library(xts)

# dat <- read.csv("shiny/primaryqcApp/data/gtmfmwq011023_QC.csv") %>% janitor::clean_names() %>% mutate(datetimestamp = lubridate::mdy_hm(paste(date, time)))

# 2022-12-19 comments: 
# interactive scatter/line plot of all parameters in file
# populate site name
# emperical formulas for outlier determinations
# summary statistics 
# be able to export tables (summary and outliers)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(p("Primary QC App", style = "color:#47a49d")),
  
  # sidebar layout with input and output definitions ----
  sidebarLayout(
    
    position = "left",
    
    # sidebar components             
    sidebarPanel(
      
      helpText("Navigate to Primary QC folder",
               "and select the file of interest."),
      
      # selectInput("site", 
      #             label = "Choose a station",
      #             choices = list("PI",
      #                            "SS",
      #                            "FM",
      #                            "PC"),
      #             selected = "PI"),
      # br(),
      
      # file input to select QC file directly from Explorer ----
      fileInput("file", 
                label = "Choose CSV Deployment File", 
                accept = ".csv",
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      
      img(src = "Friends-web.png",
          width = "70px", height = "70px"),
      
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"), "."),
      
    ),
    
    # main panel for displaying outputs ----
    mainPanel("mainpanel",
              
              textOutput("textoutput"),
              
              tableOutput("contents"),
              
              dygraphOutput("qcplots"),
              
              plotlyOutput("plot")
              
              )
    )
  
)

# Define server logic ----
server <- function(input, output) {
  
  data <- reactive({
          req(input$file)
    
          read.csv(input$file$datapath)})
  
  
  
  # output$contents <- renderTable({
  #   file <- input$file
  #   ext <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   validate(need(ext == "csv", "Please upload a csv file"))
  #   
  #   read.csv(file$datapath, header = input$header)
  # })
  
  output$textoutput <- renderText({
    paste(input$file)
    })
  
  output$qcplots <- renderDygraph({
    
    dat <- data()
  req(dat)
    dat <- dat %>% janitor::clean_names() %>% mutate(datetimestamp = lubridate::mdy_hm(paste(date, time)))
    
    dat_temp <- dat %>% select(datetimestamp, temp)
    dat_temp <- xts(dat_temp[,-1], order.by=dat_temp[,1])
    
    dat_sal <- dat %>% select(datetimestamp, sal)
    dat_sal <- xts(dat_sal[,-1], order.by=dat_sal[,1])
    
    dat_spcond <- dat %>% select(datetimestamp, sp_cond)
    dat_do_mgl <- dat %>% select(datetimestamp, do_mgl)
    dat_do_pct <- dat %>% select(datetimestamp, do_pct)
    
    dygraph(dat_temp, xlab = "DateTimeStamp", ylab = "Temperature") %>% dyRangeSelector()
    
    # dygraph(dat_sal, xlab = "DateTimeStamp", ylab = "Salinity") %>% dyRangeSelector()
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

# # convert dat to xts object
# dat <- dat %>% mutate(datetimestamp = lubridate::mdy_hm(paste(date, time))) %>% 
#   select(datetimestamp, sal)
# 
# 
# 
# dat_ts <- xts(dat[,-1], order.by=dat[,1])
# 
# dygraph(dat_ts) %>% dyRangeSelector()

