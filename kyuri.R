# 공공자전거 대여소정보
bike1 <- read.csv( "data/public_bicycle_rentalshop_2106.csv" )
# 공공자전거 이용정보( 시간대별 )
bike2 <- read.csv( "data/public_bicycle_using_time_2106.csv")
# 공공자전거 대여소별 이용정보
bike3 <- read.csv( "data/public_bicycle_rentalshop_using_2102_2106.csv" )

# 1. 데이터 전처리
# (1)  데이터의 컬럼명 변경
View( bike1 )
bike1_newcolname <- c("rno", "rname", "city", "address", "latitude", "longitude", "setup", "lcd", "qr", "operation")
names( bike1 ) <- bike1_newcolname

View( bike3 )
bike3_newcolname <- c("city","rname","rdate","rent")
names( bike3 ) <- bike3_newcolname

bike2_newcolname <- c("rdate", "rtime", "rno", "rname", "rcode", "sex", "age", "rent", "exercise","co2", "distance", "utime" )
names( bike2 ) <- bike2_newcolname


# (3) bike3 이용일 202106 데이터만 사용하기
bike3 <- subset( bike3, bike3$rdate == "202106" )
View( bike3 )

# (4) bike2 사용시간이 최대 사용시간인 12시간이 넘어가는 데이터 삭제하기
View( bike2 )
bike2 <- subset( bike2, bike2$utime < 12 )

# (5) 대여소번호가 na이거나 "정비센터"인 데이터 삭제
table( is.na( bike1$rno ) )
bike1 <- na.omit( bike1 )

# (6) bike2에서 성별이 F,f 를 모두 F로, M, m을 모두 M으로 변경
table( bike2$sex )
bike2$sex<-ifelse(bike2$sex=="m"|bike2$sex=="M", "M", ifelse( bike2$sex=="f"|bike2$sex=="F", "F",""))

############### 12/22
View( bike1 )
View( bike2 )

install.packages( "dplyr" )
library( dplyr )
# 대여소 정보와 inner join을 통해 존재하는 대여소에 대한 건수만 남기기
# 필요한 컬럼만 분리 ( 대여시간, 대여소번호, 이용건수)
index <- c(2,3,8)
bike2_time <- bike2[,index]
View( bike2_time )

bike2_using_pertime <- bike2_time %>%
  group_by( rno, rtime ) %>%
  summarise( rent = sum(rent))
View( bike2_using_pertime )

# 시간대별 대여소의 이용건수 비교하기
# 새벽( 00~06 ) dawn
# 오전( 06~10 ) morning
# 낮  ( 10~14 ) afternoon
# 오후( 14~18 ) evening
# 밤  ( 18~24 ) night

bike2_using_alltime <- bike2_using_pertime %>%
  group_by( rno ) %>%
  summarise( rent_allday = sum(rent) ) %>%
  arrange(desc(rent_allday))
bike2_using_alltime

bike2_dawn <- bike2_using_pertime %>%
  filter( rtime>=0 & rtime < 7 ) %>%
  group_by( rno ) %>%
  summarise( rent_dawn = sum(rent) )
bike2_morning <- bike2_using_pertime %>%
  filter( rtime>=6 & rtime < 11 ) %>%
  group_by( rno ) %>%
  summarise( rent_morning = sum(rent) )
bike2_afternoon <- bike2_using_pertime %>%
  filter( rtime>=10 & rtime < 15 ) %>%
  group_by( rno ) %>%
  summarise( rent_afternoon = sum(rent) )
bike2_evening <- bike2_using_pertime %>%
  filter( rtime>=14 & rtime < 19 ) %>%
  group_by( rno ) %>%
  summarise( rent_evening = sum(rent) )
bike2_night <- bike2_using_pertime %>%
  filter( rtime>=18 & rtime < 25 ) %>%
  group_by( rno ) %>%
  summarise( rent_night = sum(rent) )

bike2_day <- inner_join( bike2_dawn, bike2_morning, by="rno" )
bike2_day <- inner_join( bike2_day, bike2_afternoon, by="rno" )
bike2_day <- inner_join( bike2_day, bike2_evening, by="rno" )
bike2_day <- inner_join( bike2_day, bike2_night, by="rno" )
bike2_day <- inner_join( bike2_day, bike2_using_alltime, by="rno" )

# 대여소번호(rno), 자치구(city), 대여소명(rname), 주소(address), 위도(latitude), 경도(longitude)
bike1_esential <- bike1[,c(1:6)] 
bike_full <- inner_join( bike1_esential, bike2_day, by="rno" )
# 전체 이용건수가 많은 순으로 정렬
bike_full <- bike_full[order(-bike_full$rent_allday),]
View( bike_full )
View( bike2_day )

bike2_for_heatmap <- bike_full[,c(2,8,11)]
head(bike2_for_heatmap,10)

install.packages("ggplot2")
library( ggplot2 )

heatmap(as.matrix(bike2_for_heatmap[, -1]))
m <- as.matrix(bike2_for_heatmap[, -1])
rownames(m) <- bike2_for_heatmap$rname
heatmap(t(head(m,10)),scale = "none")


# 꺾은선그래프
bike2_head <- head(bike2_using_alltime,5)
bike2_graph <- inner_join(bike2_head,bike2_using_pertime,by="rno" )
bike2_graph <- inner_join( bike1_esential, bike2_graph, by="rno" )
bike2_graph <- bike2_graph[,c(2,8,9)]

ggplot(data=bike2_graph, aes(x=rtime, y=rent, colour=rname, group=rname)) + 
  geom_line() + 
  geom_point(size=3) +
  ggtitle("이용건수 Top5의 시간대별 이용현황")

View( bike2_graph )

# 자치구별 꺾은선 그래프
bike2_city_graph <- inner_join(bike2_using_alltime,bike2_using_pertime,by="rno" )
bike2_city_graph <- inner_join( bike1_esential, bike2_city_graph, by="rno" )
View(bike2_city_graph)

region <- bike2_city_graph %>%
  filter(!is.na(rent)) %>%
  group_by(city) %>%
  summarise(sum_rent = sum(rent)) %>%
  arrange(desc(sum_rent))
region <- head(region,10)
region

bike2_city_graph <- bike2_city_graph %>%
  group_by( city, rtime ) %>%
  summarise( rent = sum(rent) )

bike2_city_graph <- inner_join( bike2_city_graph, region, by="city")

# 전체 이용건수가 많은 순으로 정렬
bike2_city_graph <- bike2_city_graph[order(-bike2_city_graph$sum_rent,bike2_city_graph$rtime),]
View(bike2_city_graph)

ggplot(data=bike2_city_graph, aes(x=rtime, y=rent, colour=city, group=city)) + 
  geom_line() + 
  geom_point(size=3) +
  ggtitle("이용건수 Top10의 자치구별 + 시간대별 이용현황")
        