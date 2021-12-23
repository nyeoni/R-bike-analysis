CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

# 전처리된 가공 데이터 불러오기
bike1 <-  read.csv("public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")
bike2 <- read.csv("public_bicycle_using_time.csv", header = T, fileEncoding = "euc-kr")
bike3 <- read.csv("public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")

View(bike1)
View(bike2)
View(bike3)

bike1_newcolname <- c("rno", "rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike1 ) <- bike1_newcolname

bike2_newcolname <- c("idnex","rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname

# 필요한 패키지 
install.packages("tidyverse")
install.packages("gains")
install.packages("plyr")
library(gains)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(plyr)
library(forecast)

# 연령대별 공공자전거 대여건수순으로 정렬해 상관관계 찾기
age_rent <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(age) %>%
  summarise(sum_rent = sum(rent))
age_rent

ggplot(data = age_rent, aes(x= age, y = sum_rent)) + geom_col()

# 연령대 class 3개로 다시 분류
bike2 <- bike2 %>%
  mutate(ageRank = ifelse(age == "~10대" | age =="20대", "young", ifelse(age == "30대" | age == "40대", "middle", "old")))
qplot(bike2$ageRank)

ageRank <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(ageRank) %>%
  summarise(sum_rent = sum(rent)) 
ageRank

ggplot(data = ageRank, aes(x=ageRank, y= sum_rent)) + geom_col() + scale_x_discrete(limits = c("young", "middle", "old"))

ageRank_m <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(ageRank) %>%
  summarise(mean_rent = mean(rent))
ageRank_m

# ~ 20대 : young, 3-40대 : middle, 50대~ : old로 구분하였을 때, young,middle,old 순으로 대여건수 순이 많음. 대여건수의 평균이 아닌 합계를 구한 이유는 각 나이대에 1건의 대여만 갖는 데이터가 많아서 평균으로 비교하는 것은 의미가 없음.


# bike1 and bike2 full outer join for capturing distribution
bike1_sub <- bike1[,1:3]
View(bike1_sub)
bike2_sub <- bike2[, c(3,4,7,8)]
View(bike2_sub)

bike_join <- full_join(bike2_sub, bike1_sub, by = "rno")
View(bike_join)

# 지역으로 분류하기 지역별 연령대 분포 파악하기

# 구별 대여건수 총 합

# find and remove missing value in bike_join that join between bike1 and bike2
table(is.na(bike_join$city))
bike_join<- na.omit(bike_join)
# calculate sum rent for city 
region <- bike_join %>%
  filter(!is.na(rent)) %>%
  group_by(city) %>%
  summarise(sum_rent = sum(rent))
region

# visualization for categorical variable (city) with bar chart
ggplot(data = region, aes(x = city, y= sum_rent)) + geom_bar(stat = "identity")

# 구별 연령대 분포
ggplot(bike_join, aes(x=city, fill = age)) + geom_bar(position = "stack")

# 구별 연령대와 대여건수 관계
bike_join <- bike_join %>% 
  mutate(ageRank = ifelse(age == "~10대" | age =="20대", "young", ifelse(age == "30대" | age == "40대", "middle", "old")))

region_rent <- bike_join %>%
  filter(!is.na(rent)) %>%
  group_by(city, ageRank) %>%
  summarise(sum_rent = sum(rent))
region_rent

ggplot(data = region_rent, aes(x=city, y = sum_rent, fill= ageRank)) + geom_col(position = "stack")

#구별 공공자전거 대여소 개수
bike1_stop <- bike1 %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  summarise(n=n())
bike1_stop

ggplot(data = bike1_stop, aes(x = reorder(city, -n) , y = n)) + geom_bar(stat = "identity")


# 나이브 베이즈 알고리즘
# 대여소 근처 지하철역의 인접 여부, 월 > 대여 건수가 임계치를 넘어설 확률 구하기. 임계치 = 대여건수가 가장 많은 대여소의 평균 대여 건수

bike3_subway <- bike3
bike3_subway <- bike3 %>%
  mutate(subway = ifelse(str_detect(rname, '역') == TRUE, 'yes', 'no'))
View(bike3_subway)
table(bike3_subway$subway)

# create training and validation sets
train.index <- sample(c(1:dim(bike3_subway)[1]), dim(bike3_subway)[1]*0.6)
train.df <- bike3_subway[train.index,]
valid.df <- bike3_subway[-train.index,]
train.df

# run naive bayes
bike.nb <- naiveBayes(subway ~. , data = train.df, na.action = na.pass)
bike.nb

pred.prob <- predict(bike.nb , newdata = valid.df, type = "raw")
pred.class <-predict(bike.nb, newdata = valid.df)

# 구글링 코드
pred <- predict(bike.nb , bike3_subway)
table(pred,bike3_subway$subway)

# confusionMatirx for accurate performance
pred.class <- predict(bike.nb , newdata = train.df)
confusionMatrix(factor(pred.class), factor(train.df$subway))

pred.class <- predict(bike.nb , newdata = valid.df)
confusionMatrix(factor(pred.class), factor(valid.df$subway))

# lift chart for accurate performance
gain <- gains(ifelse(valid.df$subway == "yes", 1, 0), pred.prob[,1], groups = 100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$subway=="yes"))~c(0,gain$cume.obs), xlab = "# cases", ylab="Cumulative", main="")
lines(c(0, sum(valid.df$subway=="yes"))~c(0,dim(valid.df)[1]), lty=2)

############## 다중 선형 회귀 모델 테스트를 위한 데이터 #####################

# (1) 구별 공공자전거 대여소 개수
bike1_stop <- bike1 %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
bike1_stop
# 최종 데이터: bike1_stop


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

bike1_sub <- bike1[,1:3]
bike2_sub <- bike2[, c(4,5,8,9)]

bike_join <- full_join(bike2_sub, bike1_sub, by = "rno")
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
bike1_stop
station_num
school.city
university.city
region

# // city 기준으로 join
full <- join_all(list(bike1_stop, station_num, school.city, university.city, region) , by = "city", type="left")
head(full)
table(is.na(full))
View(full)
# // 컬럼명 바꾸기
names(full) <- fname
# // NA 제거
full <- na.omit(full)
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
bike1_stop
station_num
school.city
university.city
region
seoul_enterprise

# // city 기준으로 join
full <- join_all(list(bike1_stop, station_num, school.city, university.city, region, seoul_enterprise) , by = "city", type="left")
head(full)
table(is.na(full))

# // 컬럼명 바꾸기
names(full) <- fname

# // NA 제거
full <- na.omit(full)
# selected_var <- c(1,3,4,5,6,7)

# (3) partitioning for training and validation
set.seed(1)
train.index <- sample(c(1:dim(full)[1]), dim(full)[1]*0.6)
train.df <- full[train.index,]
valid.df <- full[-train.index,]

############################################# 컬럼을 추출해서 하는 게 맞는 것인가 #########################################
train.df <- full[train.index,selected_var]
valid.df <- full[-train.index,selected_var]
############################################# 컬럼을 추출해서 하는 게 맞는 것인가 #########################################

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
