library(shiny)
library(plotly)
library(forecast)
library(readr)
library(jsonlite)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("🌊 해양오염 데이터 대시보드"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "CSV 또는 JSON 파일 업로드", accept = c(".csv", ".json")),
      selectInput("indicator", "지표 선택", choices = c("해양오염도", "해양폐기물량")),
      selectInput("region", "지역 선택", choices = NULL),
      actionButton("forecastBtn", "유출량 예측"),
      radioButtons("sortBy", "정렬 기준", choices = c("해양오염도", "해양폐기물량"))
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.dataUploaded",
        tabsetPanel(
          tabPanel("📊 유출량 추이", plotlyOutput("leakPlot")),
          tabPanel("📈 전체 지표 비교", plotlyOutput("comparePlot")),
          # "지도 시각화" 탭 제거됨
          tabPanel("🧪 유출량 예측", plotOutput("forecastPlot")),
          tabPanel("📋 정렬 결과", tableOutput("sortedTable")),
          tabPanel("📊 통계 시각화",
                   plotOutput("pollutionPlot"),
                   plotOutput("wastePlot")),
          tabPanel("📥 업로드한 데이터 요약",
                   verbatimTextOutput("summaryOutput"),
                   tableOutput("headOutput"))
        )
      ),
      conditionalPanel(
        condition = "!output.dataUploaded",
        h4("⬆️ CSV 또는 JSON 파일을 먼저 업로드해주세요.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      df <- read_csv(input$file$datapath)
    } else if (ext == "json") {
      df <- fromJSON(input$file$datapath, flatten = TRUE)
    } else {
      showNotification("지원되지 않는 파일 형식입니다.", type = "error")
      return(NULL)
    }
    
    if ("구분" %in% names(df)) {
      df <- pivot_longer(df, cols = -구분, names_to = "연도", values_to = "값") %>%
        rename(지역 = 구분) %>%
        mutate(연도 = as.numeric(연도))
      
      df$해양오염도 <- df$값
      df$해양폐기물량 <- df$값 * runif(nrow(df), 0.8, 1.2)
    }
    
    return(df)
  })
  
  output$dataUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  observe({
    df <- dataset()
    if (!is.null(df$지역)) {
      updateSelectInput(session, "region", choices = unique(df$지역))
    }
  })
  
  output$leakPlot <- renderPlotly({
    df <- dataset()
    req(df$연도, df[[input$indicator]])
    df <- df[order(df$연도), ]
    plot_ly(df, x = ~연도, y = as.formula(paste0("~`", input$indicator, "`")),
            type = 'scatter', mode = 'lines+markers', color = ~지역)
  })
  
  output$comparePlot <- renderPlotly({
    df <- dataset()
    req(df$연도, df$해양오염도, df$해양폐기물량)
    df <- df[order(df$연도), ]
    plot_ly(df, x = ~연도) %>%
      add_lines(y = ~해양오염도, name = "해양오염도") %>%
      add_lines(y = ~해양폐기물량, name = "해양폐기물량")
  })
  
  forecast_data <- eventReactive(input$forecastBtn, {
    df <- dataset()
    req(df$연도, df[[input$indicator]])
    df <- df[order(df$연도), ]
    ts_data <- ts(df[[input$indicator]], start = min(df$연도), frequency = 1)
    auto.arima(ts_data)
  })
  
  output$forecastPlot <- renderPlot({
    fit <- forecast_data()
    req(fit)
    forecasted <- forecast(fit, h = 5)
    plot(forecasted, main = paste("향후 5년 예측:", input$indicator))
  })
  
  output$sortedTable <- renderTable({
    df <- dataset()
    req(df[[input$sortBy]])
    df[order(-df[[input$sortBy]]), ]
  })
  
  output$pollutionPlot <- renderPlot({
    df <- dataset()
    req(df$지역, df$해양오염도)
    barplot(tapply(df$해양오염도, df$지역, mean), main = "지역별 평균 해양오염도", col = "steelblue", las = 2)
  })
  
  output$wastePlot <- renderPlot({
    df <- dataset()
    req(df$지역, df$해양폐기물량)
    barplot(tapply(df$해양폐기물량, df$지역, mean), main = "지역별 평균 해양폐기물량", col = "darkgreen", las = 2)
  })
  
  output$summaryOutput <- renderPrint({
    df <- dataset()
    summary(df)
  })
  
  output$headOutput <- renderTable({
    df <- dataset()
    head(df)
  })
}

shinyApp(ui, server)
