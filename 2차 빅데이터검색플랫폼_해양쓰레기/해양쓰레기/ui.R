# UI 정의
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel(div(icon("recycle"), "해양쓰레기 데이터 분석")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "파일 업로드", accept = c(".csv", ".xls", ".xlsx")),
      p("파일 업로드 후 선택한 파일의 정보가 아래에 표시됩니다."),
      uiOutput("fileInfo"),  # 동적으로 업데이트될 UI
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("데이터 요약", icon = icon("file"),
                 h4("데이터 요약"),
                 fluidRow(
                   column(3,
                          wellPanel(
                            sliderInput("yearRange", "연도 범위 선택(2016~2023)", min = 2016, max = 2023, value = c(2016, 2023), step = 1),
                          )
                   ),
                   column(9,       
                          verbatimTextOutput("summaryOutput")
                   )
                 )
        ),
        
        tabPanel("데이터 테이블", icon = icon("table"),
                 h4("데이터 테이블"),
                 DT::dataTableOutput("dataTable")
        ),
        
        tabPanel("지역별 쓰레기량 추이", icon = icon("chart-line"),
                 fluidRow(
                   column(3,
                          wellPanel(
                            selectInput("region", "도시 선택", choices = NULL, selected = NULL, multiple = TRUE),
                            actionButton("resetRegion", "초기화"),
                            actionButton("selectAllRegion", "모두 선택")
                          )
                   ),
                   column(9,
                          plotOutput("marineTrashPlot")
                   )
                 )
        ),
        
        tabPanel("연도별 쓰레기량 추이", icon = icon("chart-bar"),
                 fluidRow(
                   column(3,
                          wellPanel(
                            sliderInput("yearRangeWaste", "연도 범위 선택:", 
                                        min = 2016, max = 2023, 
                                        value = c(2016, 2023), step = 1),
                            hr(),
                            checkboxInput("showAverage", "평균선 표시", value = TRUE),
                            hr(),
                            downloadButton("downloadPlot", "그래프 다운로드", class = "btn-primary")
                          )
                   ),
                   column(9,
                          plotOutput("yearlyWastePlot", height = "500px"),
                   )
                 )
        ),
        
        tabPanel("지도 시각화", icon = icon("map"),
                 fluidRow(
                   column(3,
                          wellPanel(
                            selectInput("selectedYear", "연도 선택:", choices = NULL),
                            helpText("지도에 표시할 연도를 선택하세요.")
                          )
                   ),
                   column(9,
                          leafletOutput("wasteMap", height = "500px"),
                          hr(),
                          plotlyOutput("wasteBarChart", height = "300px")
                   )
                 )
        ), 
        
        tabPanel("데이터 기반 예측", icon = icon("chart-line"),
                 fluidRow(
                   column(3,
                          wellPanel(
                            selectInput("predict_regions", "예측할 지역 선택:", 
                                        choices = NULL, multiple = TRUE),
                            numericInput("forecast_years", "예측 기간(연도):", 
                                         value = 5, min = 1, max = 10),
                            selectInput("predict_method", "예측 방법:", 
                                        choices = c("ETS" = "ets", 
                                                    "ARIMA" = "arima", 
                                                    "Holt-Winters" = "hw"),
                                        selected = "ets"),
                            actionButton("run_prediction", "예측 실행", 
                                         class = "btn-primary")
                          ),
                          wellPanel(
                            selectInput("prediction_map_year", "지도에 표시할 예측 연도:", 
                                        choices = NULL),
                            helpText("예측 결과가 나타난 후 지도에 표시할 연도를 선택하세요.")
                          )
                   ),
                   column(9,
                          tabsetPanel(
                            tabPanel("예측 그래프", 
                                     plotlyOutput("predictionChart", height = "500px")
                            ),
                            tabPanel("예측 지도", 
                                     leafletOutput("predictionMap", height = "500px")
                            )
                          )
                  )
                )
        )
      )
    )
  )
)
