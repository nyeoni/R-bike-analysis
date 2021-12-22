setwd('/Users/chloek/Lectures/[3-2]/비즈니스모델링2/rproject/BM2_Bicycle_Analysis/')
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
View( bike1 )

# (2) bike3 데이터의 컬럼명 변경
View( bike3 )
bike3_newcolname <- c("자치구","대여소명","대여일자/월","대여건수")
names( bike3 ) <- bike3_newcolname

# (3) bike3 이용일 202106 데이터만 사용하기
bike3 <- subset( bike3, bike3$`대여일자/월` == "202106" )
View( bike3 )

# (4) bike2 사용시간이 최대 사용시간인 12시간이 넘어가는 데이터 삭제하기
View( bike2 )
bike2 <- subset( bike2, bike2$사용시간 < 12 )

# (5) 대여소번호가 na이거나 "정비센터"인 데이터 삭제
table( is.na( bike1$대여소번호 ) )
bike1 <- na.omit( bike1 )

# (6) bike2에서 성별이 F,f 를 모두 F로, M, m을 모두 M으로 변경
table( bike2$성별 )
bike2$성별<-ifelse(bike2$성별=="m"|bike2$성별=="M", "M", ifelse( bike2$성별=="f"|bike2$성별=="F", "F",""))