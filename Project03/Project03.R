### Project

setwd('D:/Heechul/R_Project/Project03')
getwd()

# 패키지 준비
library(dplyr)
library(stringr)
library(ggplot2)

#################################### 큰 흐름
# 각 변수들이 QTY에 미치는 영향을 찾는 과정 
# 여러가지를 분석한 결과 
# ITEM_CNT, PRICE, MAXTEMP, RAIN_DAY 선택된 fit3 모델
# ITEM_CNT, PRICE, MAXTEMP 선택된 fit4 모델
# fit3와 fit4 모델을 가지고 정규성, 등분산성, 선형성, 독립성 검정한 결과

# fit3 모델은
# 결과적으로 fit3모델은 정규성을 따르고
# Global Stat(선형성), Heteroscedasticity (등분상성) 만족하지 못함
# 독립성 만족하지 못함

# fit4 모델은
# 결과적으로 fit3모델은 정규성을 따르고
# Global Stat(선형성), Heteroscedasticity (등분상성) 만족하지 못함
# 독립성 만족하지 못함

# fit3, fit4 AIC 값을 비교한 결과 fit4모델이 더 적합

# summary(fit4)이용해요 fit4모델의 다중회귀식을 만들고
# pre_QTY 컬럼 하나 추가했음
# 끝
############################################


# 데이터 불러오기
data_raw <- read.csv('Data/sales.csv')

head(data_raw)
length(data_raw)
tail(data_raw)
View(data_raw)
# 데이터 스포츠,이온음료로 필터링
data <- data_raw %>%
  filter(CATEGORY == '비타민음료')
data <- data[,-c(1,2)]
head(data)
str(data)

# ITEM_CNT	상품품목수
# QTY	판매량
# PRICE	가격
# MAXTEMP	기온
# SALEDAY	영업(판매)일수
# RAIN_DAY	강우일수
# HOLIDAY	휴일일수

# 해당 데이터의 정규성 검토를 함
shapiro.test(data$QTY)

# 상과관계 분석
cor(data)

# MAXTEMP, RAIN_DAY 는 0.3~0.7사이로 뚜렷한 양적 선형관계
# HOLIDAY 0.3 이하로 약한상관관계
# ITEM_CNT, PRICE, SALEDAY 강한상관관계를 보이고 있음

######################################################################################
# 산점도 그려보기
pairs(data)

# Linear Regression Model
(fit1 <- lm(QTY~., data = data))
summary(fit1)
anova(fit1)

# 분산분석표를 통해서 봤을때 SALEDAY, HOLIDAY 유의미한 영향력이 없는 것으로 보인다.

# Stepwise regression
# 1. backward elimination
step(fit1, direction = 'backward')

# 분석결과 HOLIDAY, SALEDAY 변수가 제거되었다.

# 2. forward selection
fit2 <- lm(QTY~1, data = data)
step(fit2, 
     direction = "forward", 
     scope = ~ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY + HOLIDAY)

# 분석결과 HOLIDAY, SALEDAY 변수가 제거되었다. backward랑 결과가 같음

# 3. stepwise regression
step(fit1, direction = 'both')

# 분석결과 HOLIDAY, SALEDAY 변수가 제거되었다. backward랑 결과 같음

# 4. All possible regression
library(leaps)
subsets1 <- regsubsets(QTY~., data = data,
                       method = 'seqrep', nbest = 6) 
plot(subsets1)

subsets2 <- regsubsets(QTY~., data = data,
                       method = 'exhaustive', nbest = 6) 
plot(subsets2)

# 2개 분석결과 HOLIDAY, SALEDAY, RAINDAY 변수가 제거되었다.

## HOLIDAY, SALEDAY 변수를 뺀 모델을 fit3로 지정
(fit3 <- lm(QTY ~ ITEM_CNT + PRICE + MAXTEMP + RAIN_DAY, data = data))
summary(fit3)

# 셜명변수가 모두 유의미한 변수이다.


# Checking Assumptions
par(mfrow = c(2, 2))
plot(fit3)
par(mfrow = c(1, 1))

# 1. 정규성 (normality) 
qqnorm(fit3$residuals) ; qqline(fit3$residuals)
shapiro.test(fit3$residuals)

# 유의수준 0.05보다 작아 정규성을 따른다

# 2. 등분산성 (homoscedasticity) 
# 3. 선형성 (linearity) 
library(gvlma)
gvmodel <- gvlma(fit3)
summary(gvmodel)

# Global Stat, Heteroscedasticity 선형성, 등분산성 둘다 만족하지 못함


# 4. 독립성 (indepandence) 
library(car)
durbinWatsonTest(fit3)

# # 유의수준 0.05보다 작아 독립성을 만족하지 못함

# 결과적으로 fit3모델은 정규성을 따르고
# Global Stat(선형성), Heteroscedasticity (등분상성) 만족하지 못함
# 독립성 만족하지 못함

## HOLIDAY, SALEDAY, RAINDAY 변수를 뺀 모델을 fit4로 지정
(fit4 <- lm(QTY ~ ITEM_CNT + PRICE + MAXTEMP, data = data))
summary(fit4)

# 셜명변수가 모두 유의미한 변수이다.


# Checking Assumptions
par(mfrow = c(2, 2))
plot(fit4)
par(mfrow = c(1, 1))

# 1. 정규성 (normality) 
qqnorm(fit4$residuals) ; qqline(fit4$residuals)
shapiro.test(fit4$residuals)

# 유의수준 0.05보다 높아 정규성을 따른다

# 2. 등분산성 (homoscedasticity) 
# 3. 선형성 (linearity) 
library(gvlma)
gvmodel <- gvlma(fit4)
summary(gvmodel)

# Global Stat, Heteroscedasticity 선형성, 등분산성 둘다 만족하지 못함


# 4. 독립성 (indepandence) 
library(car)
durbinWatsonTest(fit4)

# # 유의수준 0.05보다 작아 독립성을 만족하지 못함

# 결과적으로 fit3모델은 정규성을 따르고
# Global Stat(선형성), Heteroscedasticity (등분상성) 만족하지 못함
# 독립성 만족하지 못함


### 결론
# AIC값은
AIC(fit3, fit4)

# 결과적으로 fit3보다 fit4가 더 적합한모델

# fit4의 다중회귀식을 만들어서 QTY 예측
data['pre_QTY'] = round((-1188.2848) + (data['ITEM_CNT'] * (23.0316)) + (data['PRICE'] * (0.7479)) + (data['MAXTEMP'] * (13.6824)), 0)
data


# 데이터 처리
data1 <- data[,-c(2, 5, 6, 7)]
head(data1)
data1

## 모델 성능 평가
library(caret)

idx <- createDataPartition(data1$pre_QTY, p = 0.7, list = F)
data1_train <- data1[idx, ]     # 70%
data1_test <- data1[-idx, ]     # 30%

table(data1_train$pre_QTY)
table(data1_test$pre_QTY)

library(rpart)
library(e1071)
library(randomForest)

rpart.data1 <- rpart(pre_QTY~., data = data1_train)
bayes.data1 <- naiveBayes(pre_QTY~., data = data1_train)
rdf.data1 <- randomForest(pre_QTY~., data = data1_train, importance = T)

rpart.pred <- predict(rpart.data1, newdata = data1_test, type = 'class')
bayes.pred <- predict(bayes.data1, newdata = data1_test, type = 'class')
rdf.pred <- predict(rdf.data1, newdata = data1_test, type = 'response')

table(data1_test$pre_QTY, rpart.pred)
table(data1_test$pre_QTY, bayes.pred)
table(data1_test$pre_QTY, rdf.pred)


confusionMatrix(rdf.pred, data1_test$pre_QTY, )
