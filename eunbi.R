CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()
# 공공자전거 대여소정보
bike1 <- read.csv( "public_bicycle_rentalshop_2106.csv", header = T, fileEncoding = "euc-kr" )
# 공공자전거 이용정보( 시간대별 )
bike2 <- read.csv( "public_bicycle_using_time_2106.csv", header = T, fileEncoding = "euc-kr")
# 공공자전거 대여소별 이용정보
bike3 <- read.csv( "public_bicycle_rentalshop_using_2102_2106.csv", header = T, fileEncoding = "euc-kr")

# 1. 데이터 전처리
# (1) bike1 데이터의 컬럼명 변경
View( bike1 )
names( bike1 )
bike1_newcolname <- c("대여소번호", "대여소명", "자치구", "상세주소", "위도", "경도", "설치시기", "거치대수LCD", "거치대수QR", "운영방식")
names( bike1 ) <- bike1_newcolname
bike1 <- bike1[-c(1:4), ]
View( bike1 )

# 데이터 전처리 후 export
write.csv(bike1, "public_bicycle_rentalshop.csv", row.names = TRUE,fileEncoding = "euc-kr")

# (2) bike3 데이터의 컬럼명 변경
View( bike3 )
bike3_newcolname <- c("자치구","대여소명","대여일자/월","대여건수")
names( bike3 ) <- bike3_newcolname

# (3) bike3 이용일 202106 데이터만 사용하기
bike3 <- subset( bike3, bike3$`대여일자/월` == "202106" )
# 대여소명 앞에 붙어있는 대여소번호 분리 및 컬럼추가
bike3 <- mutate(bike3, 대여소번호 = as.integer(str_split_fixed(대여소명, "\\.", n = 2)[,1]))

View(bike3)
str(bike3)
summary(bike3)
View( bike3 )

# 데이터 전처리 후 export
write.csv(bike3, "public_bicycle_rentalshop_using.csv", row.names = TRUE,fileEncoding = "euc-kr")
check <- read.csv( "public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")
View(check)

# (4) bike2 사용시간이 최대 사용시간인 12시간이 넘어가는 데이터 삭제하기
View( bike2 )
bike2 <- subset( bike2, bike2$사용시간 < 12 )

# (5) 대여소번호가 na이거나 "정비센터"인 데이터 삭제
table( is.na( bike1$대여소번호 ) )
bike1 <- na.omit( bike1 )

# (6) bike2에서 성별이 F,f 를 모두 F로, M, m을 모두 M으로 변경
table( bike2$성별 )
bike2$성별<-ifelse(bike2$성별=="m"|bike2$성별=="M", "M", ifelse( bike2$성별=="f"|bike2$성별=="F", "F",""))

# 데이터 전처리 후 export
write.csv(bike2, "public_bicycle_using_time.csv", row.names = TRUE,fileEncoding = "euc-kr")

bike1_newcolname <- c("rno", "rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike1 ) <- bike1_newcolname

View( bike3 )
bike3_newcolname <- c("city","rname","rdate","rent")
names( bike3 ) <- bike3_newcolname

bike2_newcolname <- c("rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname

# 연령대별 공공자전거 대여건수순으로 정렬해 상관관계 찾기
library(dplyr)
library(ggplot2)

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


# bike1 and bike2 full outer join for captruring distribution
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
library(e1071)
install.packages("tidyverse")
library(tidyverse)
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

library(caret)
pred.class <- predict(bike.nb , newdata = train.df)
confusionMatrix(factor(pred.class), factor(train.df$subway))

pred.class <- predict(bike.nb , newdata = valid.df)
confusionMatrix(factor(pred.class), factor(valid.df$subway))
install.packages("gains")
library(gains)
gain <- gains(ifelse(valid.df$subway == "yes", 1, 0), pred.prob[,1], groups = 100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$subway=="yes"))~c(0,gain$cume.obs), xlab = "# cases", ylab="Cumulative", main="")
lines(c(0, sum(valid.df$subway=="yes"))~c(0,dim(valid.df)[1]), lty=2)
