CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

install.packages("extrafont") 
library(extrafont)
font_import()

# 공공자전거 대여소정보
bike1 <- read.csv( "public_bicycle_rentalshop.csv", header = T, fileEncoding = "euc-kr" )
# 공공자전거 이용정보( 시간대별 )
bike2 <- read.csv( "public_bicycle_using_time.csv", header = T, fileEncoding = "euc-kr")
# 공공자전거 대여소별 이용정보
bike3 <- read.csv( "public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")

# 연령대별 공공자전거 대여건수순으로 정렬해 상관관계 찾기
library(dplyr)
library(ggplot2)
theme_set(theme_grey(base_family='NanumGothic'))

age_rent <- bike2 %>%
  filter(!is.na(rent)) %>%
  group_by(age) %>%
  summarise(sum_rent = sum(rent))
age_rent

# (0). 연령별 대여건수 시각화
# 20 > 50 > 30 > 40 > ~10 > 60 > 70 대 순의 이용률을 보여주고 있다.
# 20대가 가장 많고, 다음으로 50대가 가장 많다.
ggplot(data = age_rent, aes(x= age, y = sum_rent)) + geom_col()

# 연령대 class 3개로 다시 분류
# 20대의 이용률이 압도적으로 많다는 것을 알 수 있음
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

bike_join <- inner_join(bike2_sub, bike1_sub, by = "rno")
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

# (1) 구별 이용건수 시각화
# visualization for categorical variable (city) with bar chart
ggplot(data = region, aes(x = city, y= sum_rent)) + geom_bar(stat = "identity")

# (2) 구별 대여건수에 따른 연령대 시각화
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

# (4) 구별 대여소 설치 수 시각화
#구별 공공자전거 대여소 개수
bike1_stop <- bike1 %>%
  filter(!is.na(city)) %>%
  group_by(city) %>%
  summarise(n=n())
bike1_stop

ggplot(data = bike1_stop, aes(x = reorder(city, -n) , y = n)) + geom_bar(stat = "identity")

# (4) 구별 시간대별 이용률 -> 규리님 파일 필요


# (5) 구별 지하철역 수

subway.df <- read.csv("서울시지하철위도경도.csv", header = T, fileEncoding = "euc-kr")
names(subway.df)
head(subway.df)

subway.city <- subway.df %>%
  group_by(city) %>%
  summarise(station_cnt = n())
subway.city

# 자치구별 지하철 갯수 시각화
ggplot(data = subway.city, aes(x = city, y= station_cnt)) + geom_bar(stat = "identity")

# 서울시 지하철역 위도 경도 구글 맵에 위치 찍기
register_google(key='AIzaSyDMBSD61X0HEwJiU2FL9iuCkEBodxfAAfY')

map.seoul <- get_googlemap('seoul', maptype = 'roadmap', zoom = 11)
ggmap(map.seoul) + geom_point(data = subway.df, aes(x = lon, y = lat), color = 'blue')

# (6) 구별 초등학교+중학교+고등학교 갯수

high.df <- read.csv("서울시자치구별고등학교.csv", header = T, fileEncoding = "euc-kr")
rest.df <- read.csv("서울시자치구별초중학교.csv", header = T, fileEncoding = "euc-kr")
head(high.df)
head(rest.df)

school.df <- rbind(high.df, rest.df)
head(school.df)

school.city <- school.df %>%
  group_by(city) %>%
  summarise(school_cnt = n())
school.city

ggplot(data = school.city, aes(x = city, y= school_cnt)) + geom_bar(stat = "identity")


# (7) 구별 대학교 갯수

university.df <- read.csv("서울시자치구별대학교.csv", header = T, fileEncoding = "euc-kr")
head(university.df) 
  
university.city <- university.df %>%
  group_by(city) %>%
  summarise(university_cnt = n())
university.city  

ggplot(data = university.city, aes(x = city, y= university_cnt)) + geom_bar(stat = "identity")
  
  
  
  