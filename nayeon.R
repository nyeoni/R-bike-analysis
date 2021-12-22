CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

# (1) 서울시 전체 / 구별 가장 적은/많은 대여소 분석

# 필요패키지
install.packages("dplyr")
library(dplyr)

# raw data 불러오기
rent.location.df <- read.csv("public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")
View(rent.location.df)

# 서울시 전체 기준 가장 대여건수가 적은 대여소
# 서초구 서울추모공원 입구 / 대여건수:  1 / 대여소번호: 4322
rent.location.df %>% arrange(rent.location.df$대여건수)

# 서울시 전체 기준 가장 대여건수가 많은 대여소
# 광진구 뚝섬유원지역 1번출구 앞 / 대여건수: 15061 / 대여소번호: 502
rent.location.df %>% arrange(desc(rent.location.df$대여건수))

# 서울시 구별 대여소 이용 총량
rent.location.df %>% 
  group_by(자치구) %>%
  summarise(대여소이용량 = sum(대여건수)) %>%
  arrange(desc(대여소이용량)) %>%
  na.omit()

# 서울시 구별 가장 많은 대여건수를 가진 대여소 top5                
top.city.df <- rent.location.df %>% 
    arrange(desc(대여건수)) %>%
    group_by(자치구) %>%
    slice(1:5) %>%
    na.omit()

View(top.city.df)

# 서울시 구별 가장 적은 대여건수를 가진 대여소 bottom5                
bottom.city.df <- rent.location.df %>% 
  arrange(대여건수) %>%
  group_by(자치구) %>%
  slice(1:5) %>%
  na.omit()

View(bottom.city.df)

# (2) 서울시 지하철 위도 경도 데이터 조작 및 시각화

# 필요 패키지
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stringr")
install.packages("readxl")
devtools::install_github("dkahle/ggmap")
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(ggmap)

# raw 데이터 읽어오기
raw.subway <- read_excel("서울교통공사_역주소 및 전화번호_20200715.xlsx")
names(raw.subway)
View(raw.subway)
head(raw.subway)

# 필요 데이터만 전처리
data.subway <- raw.subway %>%
  select(c(3, 4, 6)) %>%
  rename(line = 호선, station = 역명, address = 역주소) %>%
  separate(address, c('address', 'etc'), sep = '[(]') %>%
  select(-etc) %>%
  na.omit()

names(data.subway)
View(data.subway)

# 구글 API 키 등록 및 위도 경도 데이터 받아오기
register_google(key='AIzaSyDMBSD61X0HEwJiU2FL9iuCkEBodxfAAfY')
coordinate = data.subway$address %>% enc2utf8() %>% geocode()

View(coordinate)
subway.lon.lat <- bind_cols(data.subway, coordinate)
subway.lon.lat
summary(subway.lon.lat)

# 서울시 지하철역 위도 경도 데이터 셋 export
write.csv(subway.lon.lat, "서울시지하철위도경도.csv", row.names = TRUE,fileEncoding = "euc-kr")

# 서울시 지하철역 위도 경도 구글 맵에 위치 찍기
map.seoul <- get_googlemap('seoul', maptype = 'roadmap', zoom = 11)
ggmap(map.seoul) + geom_point(data = subway.lon.lat, aes(x = lon, y = lat), color = 'blue')
