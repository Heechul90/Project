### 옥자평점 분석(naver)

setwd('D:/Heechul/project/project01')

## 필요패키지
library(KoNLP)
library(wordcloud)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
useSejongDic()

# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
windowsFonts(malgun=windowsFont("맑은 고딕"))      # 맑은고딕
windowsFonts(malgun=windowsFont("나눔고딕"))       # 나눔고딕
Arial : windowsFonts(malgun=windowsFont("Arial"))  # 영어폰트
windowsFonts(malgun=windowsFont("맑은 고딕"))


### 1. 일자별/시간대별 평점 분석
rawdata <- read.csv('data/review(naver).csv', header = T)
str(rawdata)
head(rawdata)

okja2 <- rawdata %>%
  select(date, score)
head(okja2)
str(okja2)

okja2$date <- as.vector(okja2$date)


date.time <- strsplit(okja2$date, ' ')
date.time <- unlist(date.time)
head(date.time)

date <- date.time[seq(from = 1, to = length(date.time), by = 2)]
length(date)
time <- date.time[seq(from = 2, to = length(date.time), by = 2)]
length(time)

df_date.time <- data.frame(date=date, time=time)
df_date.time

# 평점, 날짜, 시간 데이터 프레임 만들기
head(rawdata)
okja3 <- rawdata %>%
  select(score)
head(okja3)
length(okja3$score)

okja3 <- cbind.data.frame(okja3, df_date.time)
okja3
head(okja3)



Sys.setlocale("LC_TIME", "C")
okja3$date <- as.Date(okja3$date, '%Y-%m-%d')
okja3$month <- factor(format(okja3$date, "%B"), levels = month.name)
okja3$month <- factor(format(okja3$date, "%B"), levels = month.name)

# 월별 평점평균
month_mean <- okja3 %>%
  group_by(month) %>%
  summarise(month_mean = mean(score))
month_mean

ggplot(month_mean, aes(x = month, y = month_mean, fill = month)) +
  geom_bar(stat = 'identity')



