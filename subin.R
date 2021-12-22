#구글 맵 패키지 install and vitalization
install.packages("usethis")
install.packages("devtools")
install.packages("dplyr")

devtools::install_github("dkahle/ggmap")

library(usethis)
library(devtools)
library(ggmap)
library(dplyr)

#구글 api 인증
register_google(key = 'AIzaSyBUDeKORQeXAM-BlkY3EA1Zwt8EWssIj74')

# 각 정류소 별 위도 경도와 대여건수를 같이 보기 위하여 bike1, bike3 join 후 null 값 삭제

bike1 <- read.csv("public_bicycle_rentalshop.csv",header = T, fileEncoding = "euc-kr")

bike3 <- read.csv("public_bicycle_rentalshop_using.csv", header = T, fileEncoding = "euc-kr")

bike3$rnum <- NULL

bike_full <- left_join(bike1, bike3, by = 'rno')

#city.x에서 아무 값도 저장되어 있지 않은 값 삭제
bike_full <- subset(bike_full, city.x != "")

View(bike_full)

#longtitude와 latitude 값 numeric으로 변경

bike_full$longitude <- as.numeric(bike_full$longitude)
bike_full$latitude <- as.numeric(bike_full$latitude)

#지하철역 위도 경도 csv 불러오기
seoul_metro <- read.csv("서울시지하철위도경도.csv", header = T, fileEncoding = "euc-kr")

View(seoul_metro)

#서울 지도 얻기
kor <- get_map("seoul", zoom = 11, maptype = "roadmap")

#theme_set 변경하여 geom_point() 사용 시 한글 보이게 하기
theme_set(theme_grey(base_family='NanumGothic'))

#지역별, 대여건수 별 구분하여 지도에 점 찍기

kor.map <- ggmap(kor) + geom_point(data=bike_full, mapping = aes(x = longitude, y = latitude, size = rent, color = city.x), alpha=0.5, na.rm = T) + geom_point(data = seoul_metro, mapping = aes(x = lon, y = lat), size = 0.1, color = 'black', alpha=0.5, na.rm = T)

kor.map


#강서구
Gangseogu <- get_map("Gangseogu", zoom = 14, maptype = "roadmap")
gangseo.map <- ggmap(Gangseogu) + geom_point(data=bike_full, mapping = aes(x = longitude, y = latitude, size = rent), color = 'skyblue', alpha=0.5, na.rm = T)+ geom_point(data = seoul_metro, mapping = aes(x = lon, y = lat), size = 2, color = 'yellow', alpha=1, na.rm = T) + geom_text(data = seoul_metro, mapping = aes(x = lon, y = lat, label = station), size = 3, na.rm = T, family = "NanumGothic")
gangseo.map

#시간 별로 나타내기
