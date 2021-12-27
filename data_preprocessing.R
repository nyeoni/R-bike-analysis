CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(paste0(CURRENT_WORKING_DIR, "/data"))
getwd()

# 데이터 로드
# 공공자전거 대여소정보
bike1 <- read.csv( "public_bicycle_rentalshop_2106.csv", header = T, fileEncoding = "euc-kr" )
# 공공자전거 이용정보( 시간대별 )
bike2 <- read.csv( "public_bicycle_using_time_2106.csv", header = T, fileEncoding = "euc-kr")

View( bike1 )
View( bike2 )

# 필요한 컬럼 추출 및 컬럼명 변경
bike1 <- bike1[,c(1,2,3,5,6)]
names(bike1) <- c("rno","rname","city","latitude", "longitude")
bike2 <- bike2[,c(2,3,7,8)]
names(bike2) <- c("rtime","rno","age","rent")

# na 값 삭제
bike1 <- na.omit( bike1 )

