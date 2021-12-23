########################### 필요한 패키지 #########################
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
library(readr)

########################### 데이터 준비하기 ###########################

CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

# 전처리된 가공 데이터 불러오기

bike2 <- read.csv("public_bicycle_using_time.csv", header = T, fileEncoding = "euc-kr")
bike3 <- read.csv( "public_bicycle_rentalshop.csv" , header = T, fileEncoding = "euc-kr")

View(bike2)
View(bike3)

bike2_newcolname <- c("idnex","rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname

bike3_newcolname <- c("index", "rno","rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike3 ) <- bike3_newcolname

#################################### 연령대별 공공자전거 대여건수순으로 정렬해 상관관계 찾기
age_rent <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(age) %>%
  dplyr::summarise(sum_rent = sum(rent))
age_rent

ggplot(data = age_rent, aes(x= age, y = sum_rent)) + geom_col()

################################ 연령대 class 3개로 다시 분류
bike2 <- bike2 %>%
  mutate(ageRank = ifelse(age == "~10대" | age =="20대", "young", ifelse(age == "30대" | age == "40대", "middle", "old")))
qplot(bike2$ageRank)

ageRank <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(ageRank) %>%
  dplyr::summarise(sum_rent = sum(rent)) 
ageRank

ggplot(data = ageRank, aes(x=ageRank, y= sum_rent)) + geom_col() + scale_x_discrete(limits = c("young", "middle", "old"))

ageRank_m <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(ageRank) %>%
  dplyr::summarise(mean_rent = mean(rent))
ageRank_m

# ~ 20대 : young, 3-40대 : middle, 50대~ : old로 구분하였을 때, young,middle,old 순으로 대여건수 순이 많음. 대여건수의 평균이 아닌 합계를 구한 이유는 각 나이대에 1건의 대여만 갖는 데이터가 많아서 평균으로 비교하는 것은 의미가 없음.


################################## bike3 and bike2 full outer join for capturing distribution

bike3_sub <- bike3[,c(2,4)]
View(bike3_sub)
bike2_sub <- bike2[, c(4,5,8,9)]
View(bike2_sub)

bike_join <- inner_join(bike2_sub, bike3_sub, by = "rno")
View(bike_join)

########################################### 지역으로 분류하기 지역별 연령대 분포 파악하기

####################################### 구별 대여건수 총 합

# find and remove missing value in bike_join that join between bike2 and bike3
table(is.na(bike_join$city))


# calculate sum rent for city 
region <- bike_join %>%
  group_by(city) %>%
  dplyr::summarise(rentCnt = sum(rent)) %>%
  na.omit()
region

# visualization for categorical variable (city) with bar chart
ggplot(data = region, aes(x = city, y= rentCnt)) + geom_bar(stat = "identity")

######################################## 구별 연령대 분포

ggplot(data = bike_join, aes(x=city, fill = age)) + geom_bar(position = "stack")

############################################# 구별 연령대와 대여건수 관계
bike_join <- bike_join %>% 
  mutate(ageRank = ifelse(age == "~10대" | age =="20대", "young", ifelse(age == "30대" | age == "40대", "middle", "old")))

region_rent <- bike_join %>%
  group_by(city, ageRank) %>%
  dplyr::summarise(sum_rent = sum(rent)) %>%
  na.omit()
region_rent

ggplot(data = region_rent, aes(x=city, y = sum_rent, fill= ageRank)) + geom_col(position = "stack")

######################################### 구별 공공자전거 대여소 개수
bike3_stop <- bike3 %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  dplyr::summarise(n=n())
bike3_stop

ggplot(data = bike3_stop, aes(x = reorder(city, -n) , y = n)) + geom_bar(stat = "identity")


############################################# 나이브 베이즈 알고리즘 #######################################################
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


