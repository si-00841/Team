library(shiny)
library(plotly)
library(leaflet)
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
          tabPanel("📍 지도 시각화", leafletOutput("mapPlot", height = "500px")),
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
    
    # Wide 포맷 처리: "구분" 열이 있을 경우 long 포맷으로 변환
    if ("구분" %in% names(df)) {
      df <- pivot_longer(df, cols = -구분, names_to = "연도", values_to = "값") %>%
        rename(지역 = 구분) %>%
        mutate(연도 = as.numeric(연도))
      
      # 기본 지표 설정 (예시로 두 지표 생성)
      df$해양오염도 <- df$값
      df$해양폐기물량 <- df$값 * runif(nrow(df), 0.8, 1.2)
    }
    
    # 위도/경도 없는 경우 예시 좌표 추가
    if (!("위도" %in% names(df)) || !("경도" %in% names(df))) {
      sample_coords <- data.frame(
        지역 = c("서울", "부산", "인천", "제주", "대구"),
        위도 = c(37.5665, 35.1796, 37.4563, 33.4996, 35.8722),
        경도 = c(126.9780, 129.0756, 126.7052, 126.5312, 128.6025)
      )
      df <- left_join(df, sample_coords, by = "지역")
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
  
  output$mapPlot <- renderLeaflet({
    df <- dataset()
    req(df$위도, df$경도, df[[input$indicator]])
    leaflet(df) %>%
      addTiles() %>%
      addCircles(lng = ~경도, lat = ~위도, weight = 1,
                 radius = ~as.numeric(df[[input$indicator]]) * 100,
                 popup = ~paste(지역, "<br>", input$indicator, ": ", df[[input$indicator]]))
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

