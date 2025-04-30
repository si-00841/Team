# 서버 정의
server <- function(input, output, session) {
  
  # 파일 업로드 시 파일 이름과 형태를 표시
  output$fileInfo <- renderUI({
    req(input$file)  # 파일이 업로드되었는지 확인
    df <- read_data(input$file$datapath)
    
    file_name <- input$file$name  # 파일 이름
    file_type <- input$file$type  # 파일 형태 (MIME 타입)
    file_size <- paste(round(input$file$size / 1024, 2), "KB")
    file_RowCol <- paste(nrow(df), "행,", ncol(df), "열")
    file_NA <- sum(is.na(df))

    
    # 파일 정보 텍스트 출력
    tagList(
      p(strong("파일 이름 : "), file_name),
      p(strong("파일 형태 : "), file_type),
      p(strong("파일 사이즈 : "), file_size),
      p(strong("행/열 수 : "), file_RowCol),
      p(strong("결측치 수 : "), file_NA)
    )
  })
  
  # 초기화 버튼 클릭 시 선택된 값 초기화========================================
  observeEvent(input$resetRegion, {
    updateSelectInput(session, "region", selected = character(0))
  })
  
  # 모두 선택 버튼 클릭 시 모든 지역 선택=======================================
  observeEvent(input$selectAllRegion, {
    updateSelectInput(session, "region", selected = regionData())
  })
  
  # 파일 업로드 후 지역 목록을 선택박스에 적용==================================
  observe({
    req(regionData())  # regionData()가 유효할 때만 실행
    updateSelectInput(session, "region", choices = regionData())
  })
  
  # 업로드한 파일 읽음==========================================================
  dataset <- reactive({
    req(input$file)
    df <- read_data(input$file$datapath)
    
    if (is.null(df)) {
      showNotification("지원되지 않는 파일 형식이거나 잘못된 데이터입니다.", type = "error")
      return(NULL) 
    }
    return(df)
  })
  
  # 지역만 따로 추출============================================================
  regionData <- reactive({
    req(dataset())
    data <- dataset()
    unique(dataset()$구분)
  })
  
  # 읽은 파일 전처리1 ==========================================================
  dataProcess <- reactive({
    req(dataset())  # dataset()이 호출되어야만 실행됨
    df <- process_data(dataset())  # 데이터 전처리
    
    if (is.null(df)) {
      showNotification("데이터 전처리 오류", type = "error")
    }
    return(df)  # 전처리된 데이터 반환
  })
  
  # 읽은 파일 전처리2 ==========================================================
  datalocation <- reactive({
    req(dataset())
    df <- location_data(dataset())
    
    if (is.null(df)) {
      showNotification("데이터 전처리 오류", type = "error")
    }
    return(df)  # 전처리된 데이터 반환
  })
  
  # 지도 시각화를 위한 데이터 준비 =============================================
  mapData <- reactive({
    req(datalocation())
    df <- datalocation()
    
    # 선택된 연도에 대한 데이터 필터링
    selectedYear <- input$selectedYear
    
    # 해당 연도 데이터만 추출
    year_data <- df %>%
      filter(연도 == selectedYear) %>%
      select(지역, 해양쓰레기양, 위도, 경도)
    
    # 위도, 경도가 없는 경우 처리
    if(!("위도" %in% colnames(year_data)) || !("경도" %in% colnames(year_data))) {
      showNotification("데이터에 위도와 경도 정보가 없습니다.", type = "warning")
      return(NULL)
    }
    
    return(year_data)
  })
  
  # 연도 드롭다운 메뉴 업데이트  ===============================================
  observe({
    req(datalocation())
    years <- unique(datalocation()$연도)
    years <- sort(years)
    
    updateSelectInput(session, "selectedYear", 
                      choices = years,
                      selected = max(years))
  })
  
  # 전처리한 데이터 사용자 선택에 따른 필터링===================================
  filteredData <- reactive({
    req(datalocation())
    df <- datalocation()
    
    # 연도 필터링
    df <- df %>%
      dplyr::filter(연도 >= as.numeric(input$yearRange[1]) & 연도 <= as.numeric(input$yearRange[2]))
    
    # 지역 필터링
    if (!is.null(input$region) && length(input$region) > 0) {
      df <- df %>% filter(지역 %in% input$region)
    }
    return(df)
  })
  
  # 쓰레기량 예측 데이터 생성 ==================================================
  predictedData <- reactive({
    req(datalocation())
    df <- datalocation()
    
    # 선택된 지역들에 대한 데이터 필터링
    if (is.null(input$predict_regions) || length(input$predict_regions) == 0) {
      showNotification("예측할 지역을 선택해주세요.", type = "warning")
      return(NULL)
    }
    
    # 예측 연도 수
    forecast_years <- input$forecast_years
    
    # 결과를 저장할 데이터프레임 초기화
    result_df <- data.frame()
    
    # 각 지역별로 예측 수행
    for (region in input$predict_regions) {
      # 해당 지역 데이터 추출
      region_data <- df %>% 
        filter(지역 == region) %>%
        arrange(연도)
      
      # 지역 데이터가 충분한지 확인 (최소 3개 이상의 데이터 포인트 필요)
      if (nrow(region_data) < 3) {
        showNotification(paste("지역 '", region, "'의 데이터가 불충분합니다."), type = "warning")
        next
      }
      
      # 위도, 경도 정보 저장
      lat <- region_data$위도[1]
      lon <- region_data$경도[1]
      
      # 시계열 객체 생성
      ts_data <- ts(region_data$해양쓰레기양, frequency = 1, start = min(region_data$연도))
      
      # 예측 모델 (Holt-Winters, ARIMA, ETS 중 선택)
      pred_method <- input$predict_method
      
      tryCatch({
        if (pred_method == "hw") {
          # Holt-Winters 방법 사용
          fit <- HoltWinters(ts_data)
          forecast_result <- forecast(fit, h = forecast_years)
        } else if (pred_method == "arima") {
          # ARIMA 모델 사용
          fit <- auto.arima(ts_data)
          forecast_result <- forecast(fit, h = forecast_years)
        } else {
          # ETS 모델 사용 (기본)
          fit <- ets(ts_data)
          forecast_result <- forecast(fit, h = forecast_years)
        }
        
        # 예측 결과 추출
        last_year <- max(region_data$연도)
        forecast_years_seq <- (last_year + 1):(last_year + forecast_years)
        
        # 예측값 및 신뢰구간 추출
        point_forecast <- as.numeric(forecast_result$mean)
        lower_80 <- as.numeric(forecast_result$lower[, 1])
        upper_80 <- as.numeric(forecast_result$upper[, 1])
        lower_95 <- as.numeric(forecast_result$lower[, 2])
        upper_95 <- as.numeric(forecast_result$upper[, 2])
        
        # 결과 데이터프레임에 추가
        for (i in 1:length(forecast_years_seq)) {
          # 음수 값 0으로 처리 (쓰레기량은 음수가 될 수 없음)
          point_val <- max(0, point_forecast[i])
          lower_80_val <- max(0, lower_80[i])
          upper_80_val <- max(0, upper_80[i])
          lower_95_val <- max(0, lower_95[i])
          upper_95_val <- max(0, upper_95[i])
          
          temp_df <- data.frame(
            지역 = region,
            연도 = forecast_years_seq[i],
            해양쓰레기양 = point_val,
            lower_80 = lower_80_val,
            upper_80 = upper_80_val,
            lower_95 = lower_95_val,
            upper_95 = upper_95_val,
            예측여부 = TRUE,
            위도 = lat,
            경도 = lon
          )
          
          result_df <- rbind(result_df, temp_df)
        }
        
      }, error = function(e) {
        showNotification(paste("지역 '", region, "'의 예측 중 오류 발생:", e$message), type = "error")
      })
    }
    
    # 예측 데이터와 실제 데이터 결합
    actual_data <- df %>%
      filter(지역 %in% input$predict_regions) %>%
      mutate(예측여부 = FALSE,
             lower_80 = NA, upper_80 = NA,
             lower_95 = NA, upper_95 = NA)
    
    combined_data <- rbind(
      actual_data,
      result_df
    )
    
    return(combined_data)
  })
  
  #===========================================================================================================
  
  # 데이터 시각화
  
  # 1. 데이터 요약============================================================== 1
  output$summaryOutput <- renderPrint({
    req(filteredData())
    df <- filteredData()
    
    # 연도와 해양쓰레기양에 대한 기본 summary 출력
    print(summary(df[, c("연도", "해양쓰레기양")]))
    
    # 지역에 대한 고유 값 개수 출력
    cat("🗺️ 지역 수:", dplyr::n_distinct(df$지역), "개")
    # 총합 및 평균
    total_waste <- sum(df$해양쓰레기양, na.rm = TRUE)
    avg_waste <- mean(df$해양쓰레기양, na.rm = TRUE)
    cat("📦 총 해양쓰레기량:", format(round(total_waste, 0), big.mark = ","), "kg")
    cat("📈 평균 해양쓰레기량:", format(round(avg_waste, 0), big.mark = ","), "kg")
    
  })
  
  # 2. 데이터 테이블============================================================ 2
  output$dataTable <- DT::renderDataTable({
    req(dataset())
    DT::datatable(dataset())
  })
  
  # 3. 지역별 쓰레기량 추이 ==================================================== 3
  output$marineTrashPlot <- renderPlot({
    req(filteredData())  # 필터링된 데이터가 있어야만 실행
    
    # 선택된 지역에 해당하는 데이터만 필터링
    df <- filteredData()
    
    # ggplot2로 지역별 쓰레기량 추이 그래프
    ggplot(df, aes(x = 연도, y = 해양쓰레기양, color = 지역, group = 지역)) +
      geom_line() +
      geom_point() +
      labs(title = "지역별 쓰레기량 추이", x = "연도", y = "해양쓰레기량(kg)") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 4. 연도별 해양쓰레기 추이 그래프 출력======================================= 4
  output$yearlyWastePlot <- renderPlot({
    req(dataProcess())  # 전처리된 데이터가 존재할 경우에만 실행
    
    df <- dataProcess()
    
    # 연도 필터링
    df <- df %>%
      filter(연도 >= input$yearRangeWaste[1] & 연도 <= input$yearRangeWaste[2])
    
    # 연도별 합계 계산
    yearly_summary <- df %>%
      group_by(연도) %>%
      summarise(총해양쓰레기양 = sum(해양쓰레기양, na.rm = TRUE))
    
    # 평균 계산
    mean_val <- mean(yearly_summary$총해양쓰레기양, na.rm = TRUE)
    
    # 그래프 그리기
    p <- ggplot(yearly_summary, aes(x = 연도, y = 총해양쓰레기양)) +
      geom_col(fill = "#0073C2FF") +
      labs(title = "연도별 해양쓰레기 발생 추이",
           x = "연도",
           y = "총 해양쓰레기량 (톤)") +
      theme_minimal(base_family = "NanumGothic") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13)
      )
    
    # 평균선 표시 옵션이 선택되었을 경우
    if (input$showAverage) {
      p <- p + geom_hline(yintercept = mean_val, linetype = "dashed", color = "red", size = 1) +
        annotate("text", x = min(yearly_summary$연도), y = mean_val,
                 label = paste("평균:", round(mean_val, 2)), vjust = -1, hjust = 0, color = "red")
    }
    
    print(p)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("연도별_해양쓰레기_추이_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- dataProcess() %>%
        filter(연도 >= input$yearRangeWaste[1] & 연도 <= input$yearRangeWaste[2]) %>%
        group_by(연도) %>%
        summarise(총해양쓰레기양 = sum(해양쓰레기양, na.rm = TRUE))
      
      mean_val <- mean(df$총해양쓰레기양, na.rm = TRUE)
      
      p <- ggplot(df, aes(x = 연도, y = 총해양쓰레기양)) +
        geom_col(fill = "#0073C2FF") +
        labs(title = "연도별 해양쓰레기 발생 추이",
             x = "연도",
             y = "총 해양쓰레기량 (톤)") +
        theme_minimal(base_family = "NanumGothic") +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 13)
        )
      
      if (input$showAverage) {
        p <- p + geom_hline(yintercept = mean_val, linetype = "dashed", color = "red", size = 1) +
          annotate("text", x = min(df$연도), y = mean_val,
                   label = paste("평균:", round(mean_val, 2)), vjust = -1, hjust = 0, color = "red")
      }
      
      ggsave(file, plot = p, width = 10, height = 6)
    }
  )
  
  # 5. 지도 출력================================================================ 5
  # Leaflet 지도 출력
  output$wasteMap <- renderLeaflet({
    req(mapData())
    
    map_data <- mapData()
    
    # NA 값 제거
    map_data <- map_data %>%
      filter(!is.na(위도) & !is.na(경도) & !is.na(해양쓰레기양))
    
    # 색상 팔레트 설정
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$해양쓰레기양
    )
    
    # 최댓값과 최솟값 계산
    max_waste <- max(map_data$해양쓰레기양, na.rm = TRUE)
    min_waste <- min(map_data$해양쓰레기양, na.rm = TRUE)
    
    # Leaflet 지도 생성
    leaflet(map_data) %>%
      addTiles() %>%  # 기본 지도 타일 추가
      setView(lng = 127.5, lat = 36, zoom = 7) %>%  # 한국 중심으로 뷰 설정
      addCircleMarkers(
        ~경도, ~위도,
        radius = ~sqrt(해양쓰레기양) / 5 + 5,  # 쓰레기양에 비례한 크기
        color = ~pal(해양쓰레기양),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste("<strong>지역:</strong> ", 지역, "<br>",
                       "<strong>해양쓰레기량:</strong> ", 해양쓰레기양)
      ) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~해양쓰레기양,
                title = paste(input$selectedYear, "년 해양쓰레기량"),
                labFormat = labelFormat(suffix = "kg"))
  })
  
  # 지역별 쓰레기량 막대 그래프
  output$wasteBarChart <- renderPlotly({
    req(mapData())
    
    map_data <- mapData() %>%
      arrange(desc(해양쓰레기양)) %>%
      head(15)  # 상위 15개 지역만 표시
    
    p <- ggplot(map_data, aes(x = reorder(지역, 해양쓰레기양), y = 해양쓰레기양, fill = 해양쓰레기양)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "darkred") +
      labs(title = paste(input$selectedYear, "년 지역별 해양쓰레기 상위 15개 지역"),
           x = "지역", y = "해양쓰레기량") +
      theme_minimal()
    
    ggplotly(p)
  })
  # 6. 데이터 기반 예측========================================================= 6
  # 예측 결과 시계열 그래프
  output$predictionChart <- renderPlotly({
    req(predictedDataReactive())
    
    df <- predictedDataReactive()
    
    # 각 지역별로 별도의 트레이스 생성
    plot_data <- plot_ly()
    
    # 색상 팔레트
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    
    regions <- unique(df$지역)
    
    for (i in 1:length(regions)) {
      region <- regions[i]
      color_index <- (i - 1) %% length(colors) + 1
      
      region_data <- df %>% filter(지역 == region)
      
      # 실제 데이터
      actual <- region_data %>% filter(!예측여부)
      
      # 예측 데이터
      predicted <- region_data %>% filter(예측여부)
      
      # 실제 데이터 트레이스
      plot_data <- plot_data %>% add_trace(
        data = actual,
        x = ~연도,
        y = ~해양쓰레기양,
        type = 'scatter',
        mode = 'lines+markers',
        name = paste('실제 -', region),
        line = list(color = colors[color_index]),
        marker = list(color = colors[color_index])
      )
      
      # 예측 데이터 트레이스
      if (nrow(predicted) > 0) {
        plot_data <- plot_data %>% add_trace(
          data = predicted,
          x = ~연도,
          y = ~해양쓰레기양,
          type = 'scatter',
          mode = 'lines+markers',
          name = paste('예측 -', region),
          line = list(color = colors[color_index], dash = 'dash'),
          marker = list(color = colors[color_index])
        )
        
        # 신뢰구간 추가 (95%)
        plot_data <- plot_data %>% add_ribbons(
          data = predicted,
          x = ~연도,
          ymin = ~lower_95,
          ymax = ~upper_95,
          name = paste('95% 신뢰구간 -', region),
          line = list(color = 'transparent'),
          fillcolor = adjustcolor(colors[color_index], alpha.f = 0.1),
          showlegend = FALSE
        )
      }
    }
    
    # 레이아웃 설정
    plot_data %>% layout(
      title = "해양쓰레기량 예측",
      xaxis = list(title = "연도"),
      yaxis = list(title = "해양쓰레기량"),
      hovermode = "closest"
    )
  })
  
  # 예측 지도 시각화
  output$predictionMap <- renderLeaflet({
    req(predictedDataReactive())
    
    # 예측 데이터 필터링 (특정 연도)
    selected_pred_year <- input$prediction_map_year
    
    map_data <- predictedData() %>%
      filter(연도 == selected_pred_year)
    
    # 데이터가 없으면 기본 지도 표시
    if (nrow(map_data) == 0) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = 127.5, lat = 36, zoom = 7))
    }
    
    # 색상 팔레트 설정
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$해양쓰레기양
    )
    
    # Leaflet 지도 생성
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 127.5, lat = 36, zoom = 7) %>%
      addCircleMarkers(
        ~경도, ~위도,
        radius = ~sqrt(해양쓰레기양) / 5 + 5,
        color = ~pal(해양쓰레기양),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste(
          "<strong>지역:</strong> ", 지역, "<br>",
          "<strong>연도:</strong> ", 연도, "<br>",
          "<strong>예측 해양쓰레기량:</strong> ", round(해양쓰레기양, 2), "<br>",
          "<strong>신뢰구간(95%):</strong> [", round(lower_95, 2), " - ", round(upper_95, 2), "]"
        )
      ) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~해양쓰레기양,
                title = paste(selected_pred_year, "년 예측 해양쓰레기량"),
                labFormat = labelFormat(suffix = "톤"))
  })
  
  # 예측 연도 선택 옵션 업데이트
  observe({
    req(predictedData())
    
    # 예측 데이터에서 가능한 연도 추출
    pred_years <- predictedData() %>%
      filter(예측여부 == TRUE) %>%
      pull(연도) %>%
      unique() %>%
      sort()
    
    if (length(pred_years) > 0) {
      updateSelectInput(session, "prediction_map_year", 
                        choices = pred_years,
                        selected = min(pred_years))
    }
  })
  
  # 지역 목록 업데이트 (예측 탭용)
  observe({
    req(regionData())
    updateSelectInput(session, "predict_regions", choices = regionData())
  })
  
  # 예측 실행 버튼 클릭 이벤트
  predictedDataReactive <- reactiveVal(NULL)
  
  observeEvent(input$run_prediction, {
    withProgress(message = '예측 분석 중...', value = 0.1, {
      # 예측 실행
      result <- predictedData()
      predictedDataReactive(result)
      
      # 진행 상태 업데이트
      incProgress(0.9)
    })
  })
}

