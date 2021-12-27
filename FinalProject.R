######################################################################
# 필요 패키지

#구글 맵 패키지 install and vitalization
install.packages("usethis")
install.packages("devtools")

library(usethis)
library(devtools)
devtools::install_github("dkahle/ggmap")

install.packages("dplyr")
install.packages("usethis")
install.packages("ggmap")
install.packages("scales")
install.packages("plyr")
install.packages("foreceast")
install.packages("caret")
install.packages("ggplot2")

library( dplyr )
library(usethis)
library(ggmap)
library(scales)
library(plyr)
library(forecast)
library(caret)
library(ggplot2)
library(readr)

#구글 api 인증
register_google(key = 'AIzaSyBUDeKORQeXAM-BlkY3EA1Zwt8EWssIj74')


######################################################################
# 데이터 전처리

CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

## 데이터 로드
# 공공자전거 대여소정보
rental.stop <- read.csv( "public_bicycle_rentalshop_2106.csv", header = T, fileEncoding = "euc-kr" )
# 공공자전거 이용정보( 시간대별 )
bike.use <- read.csv( "public_bicycle_using_time_2106.csv", header = T, fileEncoding = "euc-kr")
# 지하철역 위도 경도 정보
station <- read.csv("서울시지하철위도경도.csv", header = T, fileEncoding = "euc-kr")
# 초중고 정보
high.school <- read.csv("서울시자치구별고등학교.csv", header = T, fileEncoding = "euc-kr")
rest.school <- read.csv("서울시자치구별초중학교.csv", header = T, fileEncoding = "euc-kr")
# 대학교 정보
university <- read.csv("서울시자치구별대학교.csv", header = T, fileEncoding = "euc-kr")
# 사업체수 정보
seoul.enterprise <- read_csv("seoul_enterprise.csv")


# 필요한 컬럼 추출 및 컬럼명 변경
rental.stop <- rental.stop[,c(1,2,3,5,6)]
names(rental.stop) <- c("rno","rname","city","latitude", "longitude")
bike.use <- bike.use[,c(2,3,7,8)]
names(bike.use) <- c("rtime","rno","age","rent")

# na 값 삭제
rental.stop <- na.omit( rental.stop )


# 대여소가 아닌 rno 삭제
rental.stop <- rental.stop %>%
  filter(!(rno<100|rno>9999)) 
bike.use <- bike.use %>%
  filter(!(rno<100|rno>9999))


######################################################################
# 데이터 분석


# 대여소별 한달 전체 이용건수

bike.use.all <- bike.use[,c(2,4)]
View( bike.use.all )

bike.use.all <- bike.use.all %>%
  group_by( rno ) %>%
  dplyr::summarise( sum = sum(rent))
View( bike.use.all )

bike.use.all <- bike.use.all[order(bike.use.all$sum),]
head(bike.use.all)
View( bike.use.all )


# 서울시의 대여소 분포와 이용건수 시각화
rental.bike <- inner_join( rental.stop, bike.use.all, by="rno")
View(rental.bike)

# longtitude와 latitude 값 numeric으로 변경
rental.bike$longitude <- as.numeric(rental.bike$longitude)
rental.bike$latitude <- as.numeric(rental.bike$latitude)

# 서울 지도 얻기
kor <- get_map("seoul", zoom = 11, maptype = "roadmap")

# theme_set 변경하여 geom_point() 사용 시 한글 보이게 하기
theme_set(theme_grey(base_family='batang'))

# 자치구별, 이용건수 별 구분하여 지도에 점 찍기
kor.map <- ggmap(kor) + geom_point(data=rental.bike, mapping = aes(x = longitude, y = latitude, size = sum, color = city), alpha=0.5, na.rm = T) + geom_point(data = station, mapping = aes(x = lon, y = lat), size = 0.1, color = 'black', alpha=0.5, na.rm = T)
kor.map

# 자치구별 이용건수 순위 확인
rental.bike.city <- rental.bike %>%
  group_by(city) %>%
  dplyr::summarise(sum_city = sum(sum))

View(rental.bike.city)
options(scipen=999)
ggplot(data = rental.bike.city, aes(x = reorder(city,-sum_city), y= sum_city)) + geom_bar(stat = "identity") + coord_cartesian(ylim = c(40000, 320000))+ scale_y_continuous(labels = scales::comma) + xlab("자치구") + ylab("이용건수")
# 이용건수가 가장 많은 자치구는 "강서구"이다.



# 연령대별 이용건수 시각화
bike.use.age <- bike.use %>%
filter(!is.na(rent)) %>%
  group_by(age) %>%
  dplyr::summarise(sum_rent = sum(rent))
bike.use.age

ggplot(data = bike.use.age, aes(x= age, y = sum_rent)) + geom_col() + scale_y_continuous(labels = scales::comma) + xlab("연령대") + ylab("이용건수")
# 가장 많은 이용률을 보인 연령대는 20대와 30대이다.



# 자치구별 시간대에 따른 이용건수 순위 확인
bike.use.time <- bike.use %>%
  group_by( rno, rtime ) %>%
  dplyr::summarise( rent = sum(rent))
View( bike.use.time )

bike.use.time <- inner_join( rental.stop, bike.use.time, by="rno")
View( bike.use.time )

bike.use.city.time <- bike.use.time %>%
  group_by( city, rtime ) %>%
  dplyr::summarise( rent = sum(rent) )
View( bike.use.city.time )
bike.use.city.time.10 <- inner_join( bike.use.city.time,head(rental.bike.city[order(-rental.bike.city$sum_city),],10), by="city")

# 전체 이용건수가 많은 순으로 정렬
ggplot(data=bike.use.city.time.10, aes(x=rtime, y=rent, colour=city, group=city)) + 
  geom_line() + geom_point(size=3) +ggtitle("이용건수 Top10의 자치구별 + 시간대별 이용현황") + scale_y_continuous(labels = scales::comma) + xlab("시간대") + ylab("이용건수")
# 가장 많은 이용률을 보인 시간은 7~9시와 17~19 이다.
# 시간대별도 마찬가지로 강서구에서 가장 많은 이용건수를 확인할 수 있다.



# 출근시간과 퇴근시간 강서구의 이용현황 파악
bike.use.morning <- bike.use.time %>%
  filter( rtime>=7 & rtime < 9 ) %>%
  group_by( rno ) %>%
  dplyr::summarise( rent_morning = sum(rent) )
bike.use.evening <- bike.use.time %>%
  filter( rtime>=17 & rtime < 19 ) %>%
  group_by( rno ) %>%
  dplyr::summarise( rent_evening = sum(rent) )

bike.use.many <- inner_join(bike.use.morning, bike.use.evening, by="rno")
bike.use.many <- inner_join(bike.use.many, rental.stop, by="rno")
View( bike.use.many )

# rent_morning과 rent_evening 에 따른 표기 필요
bike.use.many$longitude <- as.numeric(bike.use.many$longitude)
bike.use.many$latitude <- as.numeric(bike.use.many$latitude)

Gangseogu <- get_map("Gangseogu", zoom = 14, maptype = "roadmap")
gangseo.map <- ggmap(Gangseogu) + 
  geom_point(data=bike.use.many, mapping = aes(x = longitude, y = latitude, size = rent_morning), color = 'red', alpha=0.5,na.rm = T, pch=17) + 
  geom_point(data=bike.use.many, mapping = aes(x = longitude, y = latitude, size = rent_evening), color = 'blue', na.rm = T,pch=1 ) + 
  geom_point(data = station, mapping = aes(x = lon, y = lat), size = 2, color = 'yellow', alpha=1, na.rm = T) +
  geom_text(data = station, mapping = aes(x = lon, y = lat, label = station), size = 3, na.rm = T, family = "batang")
gangseo.map + xlab("위도") + ylab("경도")




######################################################################
# 데이터 모델링

# (1) 구별 공공자전거 대여소 개수
rental.stop.cnt <- rental.stop %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
rental.stop.cnt

# (2) 구별 지하철 역 수
station.cnt <- station %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
station.cnt

# (3) 구별 초중고 학교 수
school <- rbind(high.school, rest.school)
head(school)

school.cnt <- school %>%
  group_by(city) %>%
  dplyr::summarise(school_cnt = n())
school.cnt

# (4) 구별 대학교 수
university.cnt <- university %>%
  group_by(city) %>%
  dplyr::summarise(university_cnt = n())
university.cnt

# (5) 구별 대여 건수 총합 : rental.bike.city 



########################## 다중 선형 회귀 분석 모델1 #####################
#########################################################################

# (1) 최종 데이터 하나로 합치기
city.rent.df <- plyr::join_all(list(rental.stop.cnt, station.cnt, school.cnt, university.cnt, rental.bike.city), by = "city", type="left")
head(city.rent.df)
View(city.rent.df)

# (2) 컬럼명 변경
names(city.rent.df) <- c("city", "stopCnt", "stationCnt", "schoolCnt", "universityCnt", "rentCnt" )

# (3) NA 값 0으로 변경
city.rent.df[is.na(city.rent.df)] <- 0
table(is.na(city.rent.df))

### 이용건수 예측 ###
# 독립변수(stationCnt, schoolCnt, universityCnt)와 종속변수(rentCnt) 추출
city.rent.df <- city.rent.df[,c(3:6)]
new.df <- data.frame(stationCnt = 10, universityCnt = 3, schoolCnt = 100)

# (4) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(city.rent.df)[1]), dim(city.rent.df)[1]*0.6)
train.df <- city.rent.df[train.index,]
valid.df <- city.rent.df[-train.index,]

# (5) 정규화 진행
train.norm.df <- train.df
valid.norm.df <- valid.df
city.rent.norm.df <- city.rent.df
new.norm.df <- new.df
norm.values <- preProcess( train.df[,1:3], method=c("center","scale"))
train.norm.df[,1:3] <- predict( norm.values, train.df[,1:3] )
valid.norm.df[,1:3] <- predict( norm.values, valid.df[,1:3] )
city.rent.norm.df[,1:3] <- predict( norm.values, city.rent.df[,1:3] )
new.norm.df <- predict( norm.values, new.df )

# (6) 다중 선형 회귀 모델 with training set
bike.rent.lm <- lm(rentCnt ~ ., data = train.norm.df)
options(scipen=999)
# show the output
summary(bike.rent.lm)

# (7) validation set for accuracy
bike.rent.lm.pred <- predict(bike.rent.lm, valid.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- valid.norm.df$rentCnt[1:5] - bike.rent.lm.pred[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred[1:5], "Acutal" = valid.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred, valid.norm.df$rentCnt)

# (8) train set for accuracy
bike.rent.lm.pred.train <- predict(bike.rent.lm, train.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- train.norm.df$rentCnt[1:5] - bike.rent.lm.pred.train[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred.train[1:5], "Acutal" = train.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred.train, train.norm.df$rentCnt)

# // show all residuals with visualization
all.residuals <- valid.norm.df$rentCnt - bike.rent.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# (9) new data test
new.pred <- predict(bike.rent.lm, new.norm.df)
options(scipen=999, digits = 0)
new.pred



########################## 다중 선형 회귀 분석 모델 2 - 대기업 데이터 #####################
#########################################################################

# 사업체 데이터 정리
names(seoul.enterprise) <- c("year","city","population","major","small")
seoul_enterprise <- seoul.enterprise[-1,-1]
seoul_enterprise <- seoul_enterprise[,c(1,3)]
View( seoul_enterprise )
# (1) 최종 데이터 하나로 합치기
city.df <- plyr::join_all(list(rental.stop.cnt, seoul_enterprise, station.cnt, school.cnt, university.cnt, rental.bike.city), by = "city", type="left")
head(city.df)
View(city.df)

# (2) 컬럼명 변경
names(city.df) <- c("city", "stopCnt", "major", "stationCnt", "schoolCnt", "universityCnt", "rentCnt" )

# (3) NA 값 0으로 변경
city.df[is.na(city.df)] <- 0
table(is.na(city.df))

### 이용건수 예측 ###
# 독립변수(stationCnt, schoolCnt, universityCnt)와 종속변수(rentCnt) 추출
city.rent.df <- city.df[,c(3:7)]
View( city.rent.df )
new.df <- data.frame(major=300, stationCnt = 10, universityCnt = 3, schoolCnt = 100)

# (4) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(city.rent.df)[1]), dim(city.rent.df)[1]*0.6)
train.df <- city.rent.df[train.index,]
valid.df <- city.rent.df[-train.index,]

# (5) 정규화 진행
train.norm.df <- train.df
valid.norm.df <- valid.df
city.rent.norm.df <- city.rent.df
new.norm.df <- new.df
norm.values <- preProcess( train.df[,1:4], method=c("center","scale"))
train.norm.df[,1:4] <- predict( norm.values, train.df[,1:4] )
valid.norm.df[,1:4] <- predict( norm.values, valid.df[,1:4] )
city.rent.norm.df[,1:4] <- predict( norm.values, city.rent.df[,1:4] )
new.norm.df <- predict( norm.values, new.df )

# (6) 다중 선형 회귀 모델 with training set
bike.rent.lm <- lm(rentCnt ~ ., data = train.norm.df)
options(scipen=999)
# show the output
summary(bike.rent.lm)

# (7) validation set for accuracy
bike.rent.lm.pred <- predict(bike.rent.lm, valid.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- valid.norm.df$rentCnt[1:5] - bike.rent.lm.pred[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred[1:5], "Acutal" = valid.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred, valid.norm.df$rentCnt)

# (8) train set for accuracy
bike.rent.lm.pred.train <- predict(bike.rent.lm, train.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- train.norm.df$rentCnt[1:5] - bike.rent.lm.pred.train[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred.train[1:5], "Acutal" = train.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred.train, train.norm.df$rentCnt)

# show all residuals with visualization
all.residuals <- valid.norm.df$rentCnt - bike.rent.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# (9) new data test
new.pred <- predict(bike.rent.lm, new.norm.df)
options(scipen=999, digits = 0)
new.pred

# 대기업 종사자는 따릉이를 많이 타지 않을 것이기 때문에 영향력이 오히려 -


########################## 다중 선형 회귀 분석 모델 2 - 중소기업 #####################
#########################################################################

# 사업체 데이터 정리
seoul_enterprise <- seoul.enterprise[-1,-1]
seoul_enterprise <- seoul_enterprise[,c(1,4)]
View( seoul_enterprise )
# (1) 최종 데이터 하나로 합치기
city.df <- plyr::join_all(list(rental.stop.cnt, seoul_enterprise, station.cnt, school.cnt, university.cnt, rental.bike.city), by = "city", type="left")
head(city.df)
View(city.df)

# (2) 컬럼명 변경
names(city.df) <- c("city", "stopCnt", "small", "stationCnt", "schoolCnt", "universityCnt", "rentCnt" )

# (3) NA 값 0으로 변경
city.df[is.na(city.df)] <- 0
table(is.na(city.df))

### 이용건수 예측 ###
# 독립변수(stationCnt, schoolCnt, universityCnt)와 종속변수(rentCnt) 추출
city.rent.df <- city.df[,c(3:7)]
new.df <- data.frame(small=30000, stationCnt = 10, universityCnt = 3, schoolCnt = 180)

# (4) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(city.rent.df)[1]), dim(city.rent.df)[1]*0.6)
train.df <- city.rent.df[train.index,]
valid.df <- city.rent.df[-train.index,]

# (5) 정규화 진행
train.norm.df <- train.df
valid.norm.df <- valid.df
city.rent.norm.df <- city.rent.df
new.norm.df <- new.df
norm.values <- preProcess( train.df[,1:4], method=c("center","scale"))
train.norm.df[,1:4] <- predict( norm.values, train.df[,1:4] )
valid.norm.df[,1:4] <- predict( norm.values, valid.df[,1:4] )
city.rent.norm.df[,1:4] <- predict( norm.values, city.rent.df[,1:4] )
new.norm.df <- predict( norm.values, new.df )

# (6) 다중 선형 회귀 모델 with training set
bike.rent.lm <- lm(rentCnt ~ ., data = train.norm.df)
options(scipen=999)
# show the output
summary(bike.rent.lm)

# (7) validation set for accuracy
bike.rent.lm.pred <- predict(bike.rent.lm, valid.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- valid.norm.df$rentCnt[1:5] - bike.rent.lm.pred[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred[1:5], "Acutal" = valid.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred, valid.norm.df$rentCnt)

# (8) train set for accuracy
bike.rent.lm.pred.train <- predict(bike.rent.lm, train.norm.df)
options(scipen=999, digits = 0)
# check the residuals
some.residuals <- train.norm.df$rentCnt[1:5] - bike.rent.lm.pred.train[1:5]
# df for residuals
data.frame("Predicted" = bike.rent.lm.pred.train[1:5], "Acutal" = train.norm.df$rentCnt[1:5], "Residual" = some.residuals)
options(scipen =999, digits = 3)
# check the accuracy
forecast::accuracy(bike.rent.lm.pred.train, train.norm.df$rentCnt)

# show all residuals with visualization
all.residuals <- valid.norm.df$rentCnt - bike.rent.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# (9) new data test
new.pred <- predict(bike.rent.lm, new.norm.df)
options(scipen=999, digits = 0)
new.pred

