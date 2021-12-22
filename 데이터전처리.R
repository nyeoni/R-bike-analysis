CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

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

# 1. 따릉이 데이터 전처리

# 공공자전거 대여소정보
bike1 <- read.csv( "public_bicycle_rentalshop_2106.csv", header = T, fileEncoding = "euc-kr" )
# 공공자전거 이용정보( 시간대별 )
bike2 <- read.csv( "public_bicycle_using_time_2106.csv", header = T, fileEncoding = "euc-kr")
# 공공자전거 대여소별 이용정보
bike3 <- read.csv( "public_bicycle_rentalshop_using_2102_2106.csv", header = T, fileEncoding = "euc-kr")

# (1) bike1 데이터의 컬럼명 변경
View( bike1 )
names( bike1 )
bike1_newcolname <- c("대여소번호", "대여소명", "자치구", "상세주소", "위도", "경도", "설치시기", "거치대수LCD", "거치대수QR", "운영방식")
names( bike1 ) <- bike1_newcolname
bike1 <- bike1[-c(1:4), ]
View( bike1 )

# (2) bike3 데이터의 컬럼명 변경
View( bike3 )
bike3_newcolname <- c("자치구","대여소명","대여일자/월","대여건수")
names( bike3 ) <- bike3_newcolname

# (3) bike3 이용일 202106 데이터만 사용하기
bike3 <- subset( bike3, bike3$`대여일자/월` == "202106" )

# 대여소명 앞에 붙어있는 대여소번호 분리 및 컬럼추가
bike3 <- mutate(bike3, 대여소번호 = as.integer(str_split_fixed(대여소명, "\\.", n = 2)[,1]))
bike3 <- subset(bike3, bike3$자치구 != "정비센터")

View(bike3)
str(bike3)
summary(bike3)
View( bike3 )

# (4) bike2 사용시간이 최대 사용시간인 12시간이 넘어가는 데이터 삭제하기
View( bike2 )
names(bike2) # "대여일자","대여시간","대여소번호","대여소명","대여구분코드","성별","연령대코드","이용건수","운동량","탄소량","이동거리","사용시간" 
bike2 <- subset( bike2, bike2$사용시간 < 12 )

# (5) 대여소번호가 na이거나 "정비센터"인 데이터 삭제
table( is.na( bike1$대여소번호 ) )
bike1 <- na.omit( bike1 )

# (6) bike2에서 성별이 F,f 를 모두 F로, M, m을 모두 M으로 변경
table( bike2$성별 )
bike2$성별<-ifelse(bike2$성별=="m"|bike2$성별=="M", "M", ifelse( bike2$성별=="f"|bike2$성별=="F", "F",""))


# (7) 데이터들의 컬럼들 영어이름으로 변경
bike1_newcolname <- c("rno", "rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike1 ) <- bike1_newcolname

View( bike3 )
bike3_newcolname <- c("city","rname","rdate","rent")
names( bike3 ) <- bike3_newcolname

bike2_newcolname <- c("rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname


# 데이터 전처리 후 export
names(bike1) # "대여소번호", "대여소명", "자치구", "상세주소", "위도", "경도", "설치시기", "거치대수LCD", "거치대수QR", "운영방식"
names(bike2) # "대여일자","대여시간","대여소번호","대여소명","대여구분코드","성별","연령대코드","이용건수","운동량","탄소량","이동거리","사용시간" 
names(bike3) # "자치구","대여소명","대여일자/월","대여건수"
write.csv(bike1, "public_bicycle_rentalshop.csv", row.names = TRUE,fileEncoding = "euc-kr")
write.csv(bike2, "public_bicycle_using_time.csv", row.names = TRUE,fileEncoding = "euc-kr")
write.csv(bike3, "public_bicycle_rentalshop_using.csv", row.names = TRUE,fileEncoding = "euc-kr")

# 2. 구별 지하철 위도 경도 데이터 

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

# 자치구만 따로 추출
dim(raw.subway)
gu.df <- strsplit(raw.subway$역주소, split = " ")

gu.df <- sapply(gu.df, '[[', 2)
class(gu.df)
gu.df <- t(t(gu.df))
gu.df <- as.data.frame(gu.df)
names(gu.df) <- c("city")
gu.df

# 자치구랑 data.subway join
data.subway <- bind_cols(data.subway, gu.df)
head(data.subway)
View(data.subway)

# 구글 API 키 등록 및 위도 경도 데이터 받아오기
register_google(key='AIzaSyDMBSD61X0HEwJiU2FL9iuCkEBodxfAAfY')
# 아래 코드는 api 요청이므로 필요시만 사용할 것
# coordinate = data.subway$address %>% enc2utf8() %>% geocode()

View(coordinate)
subway.lon.lat <- bind_cols(data.subway, coordinate)
subway.lon.lat
summary(subway.lon.lat)

View(subway.lon.lat)
# 서울시 지하철역 위도 경도 데이터 셋 export
write.csv(subway.lon.lat, "서울시지하철위도경도.csv", row.names = TRUE,fileEncoding = "euc-kr")

# 3. 서울시 구별 대학교 데이터

university.df <- read.csv("서울시 대학 및 전문대학 데이터.csv", header = T, fileEncoding = "euc-kr")
head(university.df)
names(university.df)

university.df <- university.df %>%
  select(c(4, 9, 12)) %>%
  rename(name = 학교명, address = 주소, city = 행정구) %>%
  na.omit()

head(university.df)

write.csv(university.df, "서울시자치구별대학교.csv", row.names = TRUE,fileEncoding = "euc-kr")

# 4. 서울시 구별 고등학교 데이터

highschool.df <- read.csv("서울특별시 고등학교 데이터.csv", header = T, fileEncoding = "euc-kr")
head(highschool.df)
names(highschool.df)

highschool.df <- highschool.df %>%
  select(c(4, 8)) %>%
  rename(name = 학교명, address = 도로명주소) %>%
  na.omit()

highschool.df[209, 2] <- c("서울특별시 금천구 시흥대로38길 62")
highschool.df[209,]

city.df <- strsplit(highschool.df$address, split = " ")
lengths(city.df)

city.df <- sapply(city.df, '[[', 2)
class(city.df)
city.df <- t(t(city.df))
city.df <- as.data.frame(city.df)
names(city.df) <- c("city")
city.df

dim(highschool.df)
dim(city.df)

# 구만 추출해낸 컬럼과 병합
highschool.df <- bind_cols(highschool.df, city.df)

View(highschool.df)
write.csv(highschool.df, "서울시자치구별고등학교.csv", row.names = TRUE,fileEncoding = "euc-kr")

# 5. 서울시 구별 초등학교 / 중학교 데이터

restschool.df <- read.csv("서울특별시 초중학교 데이터.csv", header = T, fileEncoding = "euc-kr")
names(restschool.df)
head(restschool.df)

restschool.df <- restschool.df %>%
  select(c(4, 8)) %>%
  rename(name = 학교명, address = 도로명주소) %>%
  mutate(address = ifelse(address == "", NA, address)) %>%
  na.omit()

city.df <- strsplit(restschool.df$address, split = " ")
lengths(city.df)

city.df <- sapply(city.df, '[[', 2)
city.df <- t(t(city.df))
city.df <- as.data.frame(city.df)
names(city.df) <- c("city")
city.df

dim(restschool.df)
dim(city.df)

# 구만 추출해낸 컬럼과 병합
restschool.df <- bind_cols(restschool.df, city.df)

View(restschool.df)
write.csv(restschool.df, "서울시자치구별초중학교.csv", row.names = TRUE,fileEncoding = "euc-kr")
