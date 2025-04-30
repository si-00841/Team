# 라이브러리 (base R에 내장된 함수 이용, 때에 따라선 DB관련 함수 이용)
library(shiny)       # R로 웹 애플리케이션을 만들 수 있는 패키지
library(bslib)       # Bootstrap 테마를 사용하기 위한 패키지지
library(shinyWidgets)# 더 세련된 위젯
library(fontawesome) # 더 다양한 아이콘 지원

library(jsonlite)    # JSON 파일을 읽고 쓸 수 있게 해주는 패키지
library(readr)       # CSV, TXT 등 텍스트 파일을 읽고 쓸 수 있게 해주는 패키지
library(openxlsx)    # Excel 파일을 읽고 쓸 수 있게 해주는 패키지
library(xml2)        # 파일 속 데이터 파싱 및 생성

library(ggplot2)     # 데이터 시각화를 위한 패키지
library(dplyr)       # 데이터 조작을 위한 패키지
library(leaflet)     # 지도 기반의 인터랙티브 시각화를 위한 패키지
library(tidyr)       # 데이터프레임 구조를 "wide ↔ long" 형식으로 변환하거나 결측치, 열 정리 등에 사용 
library(plotly)      # 인터랙티브(상호작용형) 그래프 생성
library(forecast)    # 시계열 예측 및 분석
library(DT)          # 인터랙티브 데이터 테이블
library(rmarkdown)   # 리포트 생성
library(sf)          # 공간 데이터 처리 (지도 관련)
library(viridis)     # 컬러 스케일을 제공
library(scales)      # 축 라벨 포맷팅, 값 스케일 조절 등을 위한 도구 제공

## 데이터 불러오기 (현재 ui에서 파일을 업로드 하기 때문에 server에서 불러올 수 밖에 없음) 

## 전역 설정 (변수 및 함수)
# 변수 (ui에서 파일을 업로드 하고 server에서 가공하는 지금의 방식 대신 global에서 불러와 가공하면 여기서 변수 지정 가능)

# 파일을 읽어오는 read_data() 함수
read_data <- function(file_path) {
  ext <- tools::file_ext(file_path)
  
  if (ext == "csv") {
    df <- readr::read_csv(file_path)
  } else if (ext == "xls" || ext == "xlsx") {
    df <- openxlsx::read_excel(file_path)
  } else {
    return(NULL)
  }
  return(df)
}

# 데이터를 전처리하는 process_data()함수
process_data <- function(df) {
  # 데이터 전처리 (구분 열을 기준으로 변환)
  if ("구분" %in% names(df)) {
    df <- tidyr::pivot_longer(df, cols = -구분, names_to = "연도", values_to = "해양쓰레기양") %>%
      dplyr::rename(지역 = 구분) %>%
      dplyr::mutate(연도 = as.numeric(연도), 해양쓰레기양 = as.numeric(해양쓰레기양)) %>%
      dplyr::filter(연도 >= 2016 & 연도 <= 2023)
  } else {
    warning("데이터에 '구분' 열이 없습니다.")
    return(NULL)
  }
  return(df)
}

# 위도와 경도 데이터를 전처리하는 location_data()함수 
location_data <- function(df) {
  # 데이터 전처리 (구분 열을 기준으로 변환)
  if ("구분" %in% names(df)) {
    # 위도와 경도 열 확인 및 저장
    has_coords <- "위도" %in% names(df) && "경도" %in% names(df)
    
    if (has_coords) {
      # 위도와 경도 정보를 별도로 저장
      coords <- df %>% 
        select(구분, 위도, 경도)
    }
    
    # 데이터 형태 변환 (연도별 데이터를 행으로 변환)
    df <- df %>%
      select(-any_of(c("위도", "경도"))) %>%  # 위도, 경도 열 제외
      tidyr::pivot_longer(cols = -구분, names_to = "연도", values_to = "해양쓰레기양") %>%
      dplyr::rename(지역 = 구분) %>%
      dplyr::mutate(연도 = as.numeric(연도), 해양쓰레기양 = as.numeric(해양쓰레기양)) %>%
      dplyr::filter(연도 >= 2016 & 연도 <= 2023)
    
    if (has_coords) {
      # 위도와 경도 정보를 다시 추가
      df <- df %>%
        left_join(coords %>% rename(지역 = 구분), by = "지역")
    }
  } else {
    warning("데이터에 '구분' 열이 없습니다.")
    return(NULL)
  }
  return(df)
}



