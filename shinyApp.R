#set working directory
#setwd()


library(shiny)
library(ggplot2)
library(ggcorrplot)
library(rmarkdown)

ui <- fluidPage(
  
  #1. titles
  
  titlePanel("Projekt zaliczeniowy - RShiny"),
  
  h3("Prezentacja i wizualizacja danych", align = "center", style ="color:blue"),
  
  h3("17-06-2019", align = "center", style ="color:blue"),
  
  h3("JS", align = "center", style ="color:blue"),
  
  div(img(src = "sgh.png", height = 150, width = 150), style="text-align: right;"),
  
  
  
  #2. side
  
  sidebarLayout(
    
    sidebarPanel(
      
      #a) generate report
      flowLayout(
                 downloadButton('report', h3("Wygeneruj raport"))),
      
      #b) choose year
      selectInput("chosenYear", h3("Wybierz rok"), 
                  choices = list("2019" = 2019, "2018" = 2018, "2017" = 2017, "2016" = 2016, 
                                 "2015" = 2015, "2014" = 2014, "2013" = 2013), selected = 2019),
                 
      #c) choose currency
      selectInput("chosenCurrency", h3("Wybierz walute 1"), 
                choices = list("THB" = 2, "USD" = 3, "AUD" = 4, "HKD" = 5, 
                               "CAD" = 6, "NZD" = 7, "SGD" = 8, "EUR"= 9,
                               "100*HUF" = 10, "CHF" = 11, "GBP"= 12, "UAH" = 13,
                               "100*JPY"= 14, "CZK" = 15, "DKK" = 16, "100*ISK" = 17,
                               "NOK" = 18, "SEK" = 19, "HRK" = 20, "RON" = 21, "BGN" = 22,
                               "TRY" = 23, "ILS" = 24, "100*CLP" = 25, "PHP" = 26, "MXN" = 27,
                               "ZAR" = 28, "BRL" = 29, "MYR" = 30, "RUB" = 31, "10000*IDR" = 32, "100*INR" = 33,
                               "100*KRW" = 34, "CNY" = 35, "XDR" = 36), selected = 2),
      
      #d) choose second currency
      selectInput("chosenCurrency2", h3("Wybierz walute 2"), 
                  choices = list("THB" = 2, "USD" = 3, "AUD" = 4, "HKD" = 5, 
                                 "CAD" = 6, "NZD" = 7, "SGD" = 8, "EUR"= 9,
                                 "100*HUF" = 10, "CHF" = 11, "GBP"= 12, "UAH" = 13,
                                 "100*JPY"= 14, "CZK" = 15, "DKK" = 16, "100*ISK" = 17,
                                 "NOK" = 18, "SEK" = 19, "HRK" = 20, "RON" = 21, "BGN" = 22,
                                 "TRY" = 23, "ILS" = 24, "100*CLP" = 25, "PHP" = 26, "MXN" = 27,
                                 "ZAR" = 28, "BRL" = 29, "MYR" = 30, "RUB" = 31, "10000*IDR" = 32, "100*INR" = 33,
                                 "100*KRW" = 34, "CNY" = 35, "XDR" = 36), selected = 2),
      
      #e) choose currencies for correlation table
      checkboxGroupInput("chosenCurrencies3", h3("Wybierz waluty do macierzy korelacji"),
                         choices = list("THB" = 2, "USD" = 3, "AUD" = 4, "HKD" = 5, 
                                        "CAD" = 6, "NZD" = 7, "SGD" = 8, "EUR"= 9,
                                        "100*HUF" = 10, "CHF" = 11, "GBP"= 12, "UAH" = 13,
                                        "100*JPY"= 14, "CZK" = 15, "DKK" = 16, "100*ISK" = 17,
                                        "NOK" = 18, "SEK" = 19, "HRK" = 20, "RON" = 21, "BGN" = 22,
                                        "TRY" = 23, "ILS" = 24, "100*CLP" = 25, "PHP" = 26, "MXN" = 27,
                                        "ZAR" = 28, "BRL" = 29, "MYR" = 30, "RUB" = 31, "10000*IDR" = 32, "100*INR" = 33,
                                        "100*KRW" = 34, "CNY" = 35, "XDR" = 36), selected = c(2,3,4,5))
                   ),
    
  mainPanel(
    tabsetPanel(type = "tabs",
                
                
                # show time series
                tabPanel("Szereg czasowy", plotOutput("plot1")),
                # show scatter plot
                tabPanel("Wykres punktowy", plotOutput("plot2")),
                #show correlation table
                tabPanel("Tablica korelacji", tableOutput("corrTable")),
                # show correlation plot
                tabPanel("Wykres korelacji", plotOutput("corrPlot"))
                
                
      )

    )
  )

)

source("pobieranie_danych.r")

server <- function(input, output){
  
  # download and read data
  dataIn <- reactive({
  data <- getNBPData(input$chosenYear)
                      })
  
  #show time series
  output$plot1 <- renderPlot({
    
    d <- as.data.frame(dataIn())
    d[1] <- as.Date(d[,1])
    
    wyk <- (
      ggplot(d, aes(x = d[,1], y =  d[,as.numeric(input$chosenCurrency)] ))
      +
        geom_line(aes(y = d[,as.numeric(input$chosenCurrency)]), col = "orchid4", size = 1)
      +
        ggtitle(paste("Kurs ", colnames(d)[as.numeric(input$chosenCurrency)], input$chosenYear))
      +
        xlab("Data")
      +
        ylab("Kurs")
      +
        theme(plot.title=element_text(size=12, color = "magenta4", face ="bold", hjust = 0.5), panel.background = element_rect(fill = "transparent",colour = NA))
      )
    
    return(wyk)    
    
  }) 
  
  #show scatter plot
  output$plot2 <- renderPlot({
    
    d <- as.data.frame(dataIn())
    d[1] <- as.Date(d[,1])
    
    wyk <- (
        ggplot(d, aes(x = d[,1]))
      +
        geom_point(aes(y = d[,as.numeric(input$chosenCurrency)]), col = "navy")
      +
        geom_point(aes(y = d[,as.numeric(input$chosenCurrency2)]), col = "green4")
      +
        ggtitle(paste("Kurs ", colnames(d)[as.numeric(input$chosenCurrency)], colnames(d)[as.numeric(input$chosenCurrency2)], input$chosenYear))
      +
        xlab("Data")
      +
        ylab("Kurs")
      +
        theme(plot.title=element_text(size=12, face ="bold", hjust = 0.5), panel.background = element_rect(fill = "transparent",colour = NA))
    )
        
    
    
    return(wyk)    
    
  }) 
  
  
  #correlation table
  output$corrTable <- renderTable ({
      
      d  <- as.data.frame(dataIn())
      d2 <- d[,as.numeric(input$chosenCurrencies3)]
      d3 <- data.frame(diff(as.matrix(d2)))
      res <- cor(d3, d3)
      round(res, 2)
     
   }, rownames = TRUE, colnames = TRUE)
  
  
  
  #correlation visualisation
  output$corrPlot<-renderPlot({
    
    d  <- as.data.frame(dataIn())
    d2 <- d[,as.numeric(input$chosenCurrencies3)]
    d3 <- data.frame(diff(as.matrix(d2)))
    res <- cor(d3, d3)
    wyk <- ggcorrplot(res, hc.order = TRUE, type = "lower",lab = TRUE, title = "Wykres korelacji wybranych walut")
    
    return(wyk)
  })

  output$report <- downloadHandler(
    
    filename = "report.html",
    content = function(file) {

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      
      params <- list(n = input$chosenYear, x = input$chosenCurrency, y= input$chosenCurrency2, z = input$chosenCurrencies3)
      

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui = ui, server =  server)

