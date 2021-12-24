############################################# 나이브 베이즈 알고리즘 #######################################################
###################################################
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


bike.use.all <- bike.use[,c(2,4)]
View( bike.use.all )

bike.use.all <- bike.use.all %>%
  group_by( rno ) %>%
  dplyr::summarise( sum = sum(rent))
View( bike.use.all )

bike.use.all <- bike.use.all[order(bike.use.all$sum),]
head(bike.use.all)
View( bike.use.all )

rental.bike <- inner_join( rental.stop, bike.use.all, by="rno")
# 대여소 근처 지하철역의 인접 여부, 월 > 대여 건수가 임계치를 넘어설 확률 구하기. 임계치 = 대여건수가 가장 많은 대여소의 평균 대여 건수

rental.stop.subway <- rental.bike %>%
  mutate(subway = ifelse(str_detect(rname, '출구') == TRUE, 'yes', 'no'))
rental.stop.subway$sum <- ifelse(rental.stop.subway$sum> mean(rental.stop.subway$sum),1,0)

View(rental.stop.subway)
table(rental.stop.subway$subway)

# create training and validation sets
train.index <- sample(c(1:dim(rental.stop.subway)[1]), dim(rental.stop.subway)[1]*0.6)
train.df <- rental.stop.subway[train.index,-c(1,4,5)]
valid.df <- rental.stop.subway[-train.index,-c(1,4,5)]
train.df

# run naive bayes subway근처인가? 대다수가 no로 나옴
bike.nb <- e1071::naiveBayes(train.df, train.df$subway, laplace = 0)
bike.nb

pred.prob <- predict(bike.nb , newdata = valid.df, type = "raw")
pred.prob
pred.class <-predict(bike.nb, newdata = valid.df)



########### 이거 그냥 따라한 코드 ##########
df <- data.frame(actual = valid.df$subway, predicted = pred.class, pred.prob)
head(df)
df[valid.df$rname == "출구" & valid.df$city == "강서구" & valid.df$sum == 2360 & valid.df$subway == "yes",]
########### 이거 그냥 따라한 코드 ##########



# confusionMatirx for accurate performance

pred.class <- predict(bike.nb , newdata = train.df)
confusionMatrix(factor(pred.class), factor(train.df$subway))

pred.class <- predict(bike.nb , newdata = valid.df)
confusionMatrix(factor(pred.class), factor(valid.df$subway))

# lift chart for accurate performance
gain <- gains::gains(ifelse(valid.df$subway == "no", 1, 0), pred.prob[,1], groups = 100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$subway=="no"))~c(0,gain$cume.obs), xlab = "# cases", ylab="Cumulative", main="")
lines(c(0, sum(valid.df$subway=="no"))~c(0,dim(valid.df)[1]), lty=2)