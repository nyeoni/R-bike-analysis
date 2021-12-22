#구글 맵 패키지 install and vitalization
install.packages("devtools")
library(devtools)
devtools::install_github("dkahle/ggmap")

library(ggmap)

#구글 api 인증
register_google(key = 'AIzaSyBUDeKORQeXAM-BlkY3EA1Zwt8EWssIj74')

# 각 정류소 별 위도 경도와 대여건수를 같이 보기 위하여 bike1, bike3 join 후 null 값 삭제

bike_full <- left_join(bike1, bike3, by = 'rno')

#city.x에서 아무 값도 저장되어 있지 않은 값 삭제
bike_full <- subset(bike_full, city.x != "")

View(bike_full)

#longtitude와 latitude 값 numeric으로 변경

bike_full$longitude <- as.numeric(bike_full$longitude)
bike_full$latitude <- as.numeric(bike_full$latitude)

#서울 지도 얻기
kor <- get_map("seoul", zoom = 11, maptype = "roadmap")

#theme_set 변경하여 geom_point() 사용 시 한글 보이게 하기
theme_set(theme_grey(base_family='NanumGothic'))

#지역별, 대여건수 별 구분하여 지도에 점 찍기

kor.map <- ggmap(kor) + geom_point(data=bike_full, mapping = aes(x = longitude, y = latitude, size = rent, color = city.x), alpha=0.5, na.rm = T)

kor.map
