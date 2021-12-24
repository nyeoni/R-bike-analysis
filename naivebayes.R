############################################# 나이브 베이즈 알고리즘 #######################################################
###################################################
library(gains)
library(tidyverse)
library(dplyr)
library(forecast)

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

rental.stop.subway <- rental.bike %>%
  mutate(subway = ifelse(str_detect(rname, '출구') == TRUE, 'yes', 'no'))

View(rental.stop.subway)
table(rental.stop.subway$subway)

# create training and validation sets
train.index <- sample(c(1:dim(rental.stop.subway)[1]), dim(rental.stop.subway)[1]*0.6)
train.df <- rental.stop.subway[train.index,-c(1,2,4,5)]
valid.df <- rental.stop.subway[-train.index,-c(1,2,4,5)]
train.df

# run naive bayes
bike.nb <- e1071::naiveBayes(train.df, train.df$subway, laplace = 0)
bike.nb

pred.prob <- predict(bike.nb , newdata = valid.df, type = "raw")
pred.prob
pred.class <-predict(bike.nb, newdata = valid.df)

# confusionMatirx for accurate performance

pred.class <- predict(bike.nb , newdata = train.df)
confusionMatrix(factor(pred.class), factor(train.df$subway))

pred.class <- predict(bike.nb , newdata = valid.df)
confusionMatrix(factor(pred.class), factor(valid.df$subway))

# lift chart for accurate performance
gain <- gains::gains(ifelse(valid.df$subway == "no", 1, 0), pred.prob[,1], groups = 100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$subway=="no"))~c(0,gain$cume.obs), xlab = "# cases", ylab="Cumulative", main="")
lines(c(0, sum(valid.df$subway=="no"))~c(0,dim(valid.df)[1]), lty=2)
