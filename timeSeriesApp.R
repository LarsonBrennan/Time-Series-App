library(shiny)
library(datasets)
library(zoo)
library(xts)
library(reshape2)
library(devtools)
library(forecastHybrid)
library(data.table)
library(DT)
library(plotly)
library(dygraphs)
library(dplyr)
library(forecast)
library(ggplot2)

#ui portion of the app
ui <- pageWithSidebar(
  headerPanel("TIME SERIES FORECASTING APP"),
  
  #this portion of the app allows the user to select which dataset they would like to
  #use the time series models on.
  sidebarPanel(
    selectInput("variable", "DATASET:",
                list("AIR PASSENGERS" = "AirPassengers", 
                     "WOOL PRODUCTION" = "woolyrnq", 
                     "GOLD PRODUCTION" = "gold",
                     "WINE SALES" = "wineind",
                     "GAS PRODUCTION" = "gas")),
    
    #allows the user to choose how many months ahead they would like to forecast for
    numericInput("ahead", "CHOOSE THE NUMBER OF MONTHS TO FORECAST AHEAD:", 3),
    #updates the time series info after the number of months to forecast ahead have
    #been chosen
    submitButton("UPDATE TIME SERIES INFO")
  ),
  
  
  
  mainPanel(
    tabsetPanel(
      #these tabpanels output the time series models graphs
      tabPanel("EXPONENTIAL SMOOTHING FORECAST", plotOutput("etsForecastPlot")), 
      tabPanel("ARIMA FORECAST", plotOutput("arimaForecastPlot")),
      tabPanel("TBATS FORECAST", plotOutput("tbatsForecastPlot")),
      tabPanel("TIME SERIES DECOMPOSITION", plotOutput("decompositionPlot"))
    ),
    
    tabsetPanel(
      #these tabpanels display the performance metrics and the upper and lower bounds of the
      #various time series models
      tabPanel("EXPONENTIAL SMOOTHING PERFORMANCE METRICS", dataTableOutput("performance_metrics")),
      tabPanel("EXPONENTIAL SMOOTHING UPPER AND LOWER BOUNDS", dataTableOutput("etsPerformance_metrics")),
      tabPanel("ARIMA PERFORMANCE METRICS", dataTableOutput("ARIMASperformance_metrics")),
      tabPanel("ARIMA UPPER AND LOWER BOUNDS", dataTableOutput("ARIMAIntervals")),
      tabPanel("TBATS PERFORMANCE METRICS", dataTableOutput("TBATSperformance_metrics")),
      tabPanel("TBATS UPPER AND LOWER BOUNDS", dataTableOutput("TBATSIntervals"))
    )
  )
)

#this is the server portion of the app
server <- function(input, output) {
  
  #this reactive statement is for users to select which dataset they would like to
  #use the time series models with. The input$variable takes in the name of the dataset
  #and with the above selectInput statement to get the dataset for the analysis.
  getDataset <- reactive({
    if (input$variable == "AirPassengers")
    {
      return(AirPassengers)
    }
    else if (input$variable == "woolyrnq")
    {
      return(woolyrnq)
    }
    else if (input$variable == "gold")
    {
      return(gold)
    }
    else if (input$variable == "gas")
    {
      return(gas)
    }
    else
    {
      return(wineind)
    }
  })
  
  #this renderText statement returns the name of the dataset the user selected and
  #puts the name on the page
  output$dataSetChoice <- renderText({
    paste("DATASET: ", input$variable)
  })
  
  #this takes in the dataset and plots the model
  output$decompositionPlot <- renderPlot({
    timeSeriesDecompPlotData <- ts(getDataset(), frequency=12)
    timeSeriesDecompPlot <- decompose(timeSeriesDecompPlotData)
    plot(timeSeriesDecompPlot)
  })

  #this renderPlot takes in the dataset and plots the model  
  output$arimaForecastPlot <- renderPlot({
    modelFit <- auto.arima(getDataset())
    plot(forecast(modelFit, h=input$ahead))
  })
  
  #this renderDataTable outputs various performance metrics for the ARIMA model
  output$ARIMASperformance_metrics <- renderDataTable({
    modelFit <- arima(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    accuracyDataFrame <- accuracy(TS_mySeries_SES_daily)
    accuracyDataFrame <- as.data.frame(as.table(accuracyDataFrame))
    accuracyDataFrame <- accuracyDataFrame %>%
      select(metric = Var2, value = Freq) %>%
      mutate(value = round(value, 3))
  })
  
  
  output$ARIMAIntervals <- renderDataTable({
    modelFit <- arima(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    timeSeriesUpperConfidence<-TS_mySeries_SES_daily$upper
    colnames(timeSeriesUpperConfidence)[1] <- "Upper80%"
    colnames(timeSeriesUpperConfidence)[2] <- "Upper95%"
    timeSeriesLowerConfidenceer<-TS_mySeries_SES_daily$lower
    colnames(timeSeriesLowerConfidenceer)[1] <- "Lower80%"
    colnames(timeSeriesLowerConfidenceer)[2] <- "Lower95%"
    timeSeriesBounds<-merge(timeSeriesLowerConfidenceer, timeSeriesUpperConfidence)
  })

  
  #this takes in the dataset and plots the model
  output$etsForecastPlot <- renderPlot({
    modelFit <- ets(getDataset())
    plot(forecast(modelFit, h=input$ahead))
  })
  
  #this renderDataTable outputs various performance metrics for the exponential
  #smoothing model
  output$performance_metrics <- renderDataTable({
    modelFit <- ets(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    accuracyDataFrame <- accuracy(TS_mySeries_SES_daily)
    accuracyDataFrame <- as.data.frame(as.table(accuracyDataFrame))
    accuracyDataFrame <- accuracyDataFrame %>%
      select(metric = Var2, value = Freq) %>%
      mutate(value = round(value, 3))
  })
  
  #this renderDataTable outputs various confidence interval metrics for the TBATS model
  output$etsPerformance_metrics <- renderDataTable({
    modelFit <- ets(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    timeSeriesUpperConfidence<-TS_mySeries_SES_daily$upper
    colnames(timeSeriesUpperConfidence)[1] <- "Upper80%"
    colnames(timeSeriesUpperConfidence)[2] <- "Upper95%"
    timeSeriesLowerConfidenceer<-TS_mySeries_SES_daily$lower
    colnames(timeSeriesLowerConfidenceer)[1] <- "Lower80%"
    colnames(timeSeriesLowerConfidenceer)[2] <- "Lower95%"
    timeSeriesBounds<-merge(timeSeriesLowerConfidenceer, timeSeriesUpperConfidence)
  })
  
  #this rednerPlot takes in the dataset and plots the TBATS model
  output$tbatsForecastPlot <- renderPlot({
    modelFit <- tbats(getDataset())
    plot(forecast(modelFit, h=input$ahead))
  })
  
  #this renderDataTable outputs various performance metrics for the ARIMA model
  output$TBATSperformance_metrics <- renderDataTable({
    modelFit <- tbats(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    accuracyDataFrame <- accuracy(TS_mySeries_SES_daily)
    accuracyDataFrame <- as.data.frame(as.table(accuracyDataFrame))
    accuracyDataFrame <- accuracyDataFrame %>%
      select(metric = Var2, value = Freq) %>%
      mutate(value = round(value, 3))
  })
  
  #this renderDataTable outputs various confidence interval metrics for the TBATS model
  output$TBATSIntervals <- renderDataTable({
    modelFit <- tbats(getDataset())
    TS_mySeries_SES_daily <-forecast(modelFit, h=input$ahead)
    timeSeriesUpperConfidence<-TS_mySeries_SES_daily$upper
    colnames(timeSeriesUpperConfidence)[1] <- "Upper80%"
    colnames(timeSeriesUpperConfidence)[2] <- "Upper95%"
    timeSeriesLowerConfidenceer<-TS_mySeries_SES_daily$lower
    colnames(timeSeriesLowerConfidenceer)[1] <- "Lower80%"
    colnames(timeSeriesLowerConfidenceer)[2] <- "Lower95%"
    timeSeriesBounds<-merge(timeSeriesLowerConfidenceer, timeSeriesUpperConfidence)
  })
  
}

shinyApp(ui = ui, server = server)

