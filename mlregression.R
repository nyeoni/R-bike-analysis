
############################ 라이브러리 ##############################

library(readr)
library(gains)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(plyr)
library(forecast)


################ 데이터 불러오기 ######################################

CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

# 전처리된 가공 데이터 불러오기
bike2 <- read.csv("public_bicycle_using_time.csv", header = T, fileEncoding = "euc-kr")
bike3 <- read.csv( "public_bicycle_rentalshop.csv" , header = T, fileEncoding = "euc-kr")

View(bike3)
View(bike2)

bike2_newcolname <- c("idnex","rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname

bike3_newcolname <- c("index", "rno","rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike3 ) <- bike3_newcolname

############## 다중 선형 회귀 모델 테스트를 위한 데이터 #####################

# (1) 구별 공공자전거 대여소 개수
bike3_stop <- bike3 %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
bike3_stop

# 최종 데이터: bike3_stop


# (2) 구별 지하철 역 수
station <- read.csv("서울시지하철위도경도.csv" , header = T, fileEncoding = "euc-kr" )
View(station)

station_num <- station %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
station_num
# 최종 데이터: station_num

# (3) 구별 초중고 학교 수

high.df <- read.csv("서울시자치구별고등학교.csv", header = T, fileEncoding = "euc-kr")
rest.df <- read.csv("서울시자치구별초중학교.csv", header = T, fileEncoding = "euc-kr")
head(high.df)
head(rest.df)

school.df <- rbind(high.df, rest.df)
head(school.df)

school.city <- school.df %>%
  group_by(city) %>%
  dplyr::summarise(school_cnt = n())
school.city
# 최종 데이터: school.city

# (4) 구별 대학교 수

university.df <- read.csv("서울시자치구별대학교.csv", header = T, fileEncoding = "euc-kr")
head(university.df) 

university.city <- university.df %>%
  group_by(city) %>%
  dplyr::summarise(university_cnt = n())
university.city
# 최종 데이터: university.city

# (5) 구별 대여 건수 총합

bike2_sub <- bike2[, c(4,5,8,9)]
bike3_sub <- bike3[,c(2,3,4)]

head(bike3_sub)
head(bike2_sub)

bike_join <- full_join(bike2_sub, bike3_sub, by = "rno")
bike_join<- na.omit(bike_join)
View(bike_join)

# calculate sum rent for city 
region <- bike_join %>%
  group_by(city) %>%
  dplyr::summarise(sum_rent = sum(rent))
region
# 최종 데이터 : region

########################## 다중 선형 회귀 분석 모델1 #####################
#########################################################################
# (1) 컬럼명 만들기
fname <- c("city", "stopCnt", "stationCnt", "schoolCnt", "universityCnt","rentCnt" )

# (2) 최종 데이터 하나로 합치기
# // 구별 자전거 대여소 개수, 지하철역 개수, 초중고 개수, 대학교 개수, 대여 건수
bike3_stop
station_num
school.city
university.city
region

# // city 기준으로 join
full <- join_all(list(bike3_stop, station_num, school.city, university.city, region) , by = "city", type="left")
head(full)
table(is.na(full))
View(full)
# // 컬럼명 바꾸기
names(full) <- fname
# // NA 제거
full <- na.omit(full)
# // 독립변수로 대여소 개수는 제거
selected_var <- c(1,3,4,5,6)

# (3) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(full)[1]), dim(full)[1]*0.6)
train.df <- full[train.index,selected_var]
valid.df <- full[-train.index,selected_var]

# (4) 다중 선형 회귀 모델 with training set
bike.lm <- lm(rentCnt ~ stationCnt + universityCnt + schoolCnt, data = train.df)
options(scipen=999)
# // show the output
summary(bike.lm)

# (5) validation set for accuracy

bike.lm.pred <- predict(bike.lm, valid.df)
options(scipen=999, digits = 0)

# // check the residuals
some.residuals <- valid.df$rentCnt[1:5] - bike.lm.pred[1:5]

# // df for residuals
data.frame("Predicted" = bike.lm.pred[1:5], "Acutal" = valid.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# // check the accuracy
accuracy(bike.lm.pred, valid.df$rentCnt)

# // show all residuals with visualization
all.residuals <- valid.df$rentCnt - bike.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# (6) new data test
new.df <- data.frame(stationCnt = 6 , universityCnt = 3, schoolCnt = 40)
new.pred <- predict(bike.lm, new.df)
options(scipen=999, digits = 0)
new.pred

########################## 다중 선형 회귀 분석 모델 2 #####################
#########################################################################

# 사업체수 데이터 불러오기기
library(readr)
seoul_enterprise <- read_csv("seoul_enterprise.csv")
View(seoul_enterprise)

ent_names <- c("year","city","population")
names(seoul_enterprise) <- ent_names
# 1행 1열 제거
seoul_enterprise <- seoul_enterprise[-1,-1]


# (1) 컬럼명 만들기
fname <- c("city", "stopCnt", "stationCnt", "schoolCnt", "universityCnt","rentCnt", "population" )

# (2) 최종 데이터 하나로 합치기
# // 구별 자전거 대여소 개수, 지하철역 개수, 초중고 개수, 대학교 개수, 대여 건수, 종사자 수
bike3_stop
station_num
school.city
university.city
region
seoul_enterprise

# // city 기준으로 join
full <- join_all(list(bike3_stop, station_num, school.city, university.city, region, seoul_enterprise) , by = "city", type="left")
head(full)
table(is.na(full))

# // 컬럼명 바꾸기
names(full) <- fname

# // NA 제거
full <- na.omit(full)
selected_var <- c(1,3,4,5,6,7)

# (3) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(full)[1]), dim(full)[1]*0.6)
train.df <- full[train.index,selected_var]
valid.df <- full[-train.index,selected_var]

# (4) 다중 선형 회귀 모델 with training set
bike.lm <- lm(rentCnt ~ stationCnt + universityCnt + schoolCnt + population, data = train.df)
options(scipen=999)
# // show the output
summary(bike.lm)

# (5) validation set for accuracy

bike.lm.pred <- predict(bike.lm, valid.df)
options(scipen=999, digits = 0)

# // check the residuals
some.residuals <- valid.df$rentCnt[1:5] - bike.lm.pred[1:5]

# // df for residuals
data.frame("Predicted" = bike.lm.pred[1:5], "Acutal" = valid.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# // check the accuracy
accuracy(bike.lm.pred, valid.df$rentCnt)

# // show all residuals with visualization
all.residuals <- valid.df$rentCnt - bike.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
