### Project

setwd('D:/Heechul/R_Project/Project03')
getwd()

# 패키지 준비
library(dplyr)
library(stringr)
library(ggplot2)

# 데이터 불러오기
data_raw <- read.csv('Data/sales.csv')

head(data_raw)
length(data_raw)
tail(data_raw)

# 데이터 스포츠,이온음료로 필터링
data <- data_raw %>%
  filter(CATEGORY == '스포츠,이온음료')
data <- data[,-c(2)]
head(data)
str(data)

# YM	판매년월
# ITEM_CNT	상품품목수
# QTY	판매량
# PRICE	가격
# MAXTEMP	기온
# SALEDAY	영업(판매)일수
# RAIN_DAY	강우일수
# HOLIDAY	휴일일수


# 산점도 그려보기
pairs(data)

# Linear Regression Model
(fit1 <- lm(QTY~., data = data))
summary(fit1)
anova(fit1)

# 분산분석표를 통해서 봤을때 SALEDAY와 RAIN_DAY는 유의미한 영향력이 없는 것으로 보인다.

# Stepwise regression
# 1. backward elimination
step(fit1, direction = 'backward')

# 분석결과 ITEM_CNT, SALEDAY, RAIN_DAY 변수가 제거되었다.

# 2. forward selection
fit2 <- lm(QTY~1, data = data)
step(fit2, 
     direction = "forward", 
     scope = ~YM + ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY + HOLIDAY)

# 분석결과 ITEM_CNT, SALEDAY, RAIN_DAY 변수가 제거되었다. backward랑 결과가 같음

# 3. stepwise regression
step(fit1, direction = 'both')

# 분석결과 ITEM_CNT, SALEDAY, RAIN_DAY 변수가 제거되었다. backward랑 결과 같음

# 4. All possible regression
library(leaps)
subsets1 <- regsubsets(QTY~., data = data,
                       method = 'seqrep', nbest = 7) 
plot(subsets1)

subsets2 <- regsubsets(QTY~., data = data,
                       method = 'exhaustive', nbest = 7) 
plot(subsets2)

# 2개 분석결과 ITEM_CNT, SALEDAY, RAIN_DAY 변수가 제거되었다.

## ITEM_CNT, SALEDAY, RAIN_DAY 변수를 뺀 모델을 fit3로 지정

(fit3 <- lm(QTY ~ MAXTEMP + YM + PRICE + HOLIDAY, data = data))
summary(fit3)

# 셜명변수가 모두 유의미한 변수이다.


# Checking Assumptions
par(mfrow = c(2, 2))
plot(fit3)
par(mfrow = c(1, 1))

# 1. 정규성 (normality) 
qqnorm(fit3$residuals) ; qqline(fit3$residuals)
shapiro.test(fit3$residuals)

# 유의수준 0.05보다 작아 정규성을 따르지 않는다.

# 2. 등분산성 (homoscedasticity) 
# 3. 선형성 (linearity) 
library(gvlma)
gvmodel <- gvlma(fit3)
summary(gvmodel)

# 유의수준 0.05보다 작아 등분산성이 아니다


# 4. 독립성 (indepandence) 
library(car)
durbinWatsonTest(fit3)

# # 유의수준 0.05보다 작아 독립성이 아니다

# 4. 독립성 (indepandence) 