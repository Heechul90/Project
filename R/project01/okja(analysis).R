### 옥자평점 분석(naver)

setwd('D:/Heechul/Project/R/project01')

## 필요패키지
library(KoNLP)
library(wordcloud)
library(stringr)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
useSejongDic()
library(xlsx)
library(writexl)

# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
windowsFonts(malgun=windowsFont("맑은 고딕"))      # 맑은고딕
windowsFonts(malgun=windowsFont("나눔고딕"))       # 나눔고딕
windowsFonts(malgun=windowsFont("Arial"))          # 영어폰트
windowsFonts(malgun=windowsFont("배달의민족 연성"))


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
  select(-date)
okja3

okja3 <- cbind.data.frame(okja3, df_date.time)
okja3
head(okja3)


Sys.setlocale("LC_TIME", "C")
okja3$date <- as.Date(okja3$date, '%Y-%m-%d')
okja3$month <- factor(format(okja3$date, "%B"), levels = month.name)
okja3$day <- as.Date(okja3$date, '%Y-%m-%d')
okja3$day <-  weekdays(as.Date(okja3$day))
okja3$time <- as.POSIXlt(okja3$time, format = '%H') 
okja3$time <- format(okja3$time, '%H') 
okja3$time <- as.numeric(okja3$time)

okja3$time <- ifelse(okja3$time %in% c(0:5) , '나이트',
              ifelse(okja3$time %in% c(6:9), '모닝',
              ifelse(okja3$time %in% c(10:12), '브런치',
              ifelse(okja3$time %in% c(13:15), '데이라이트',
              ifelse(okja3$time %in% c(16:21), '프라임', '문라이트')))))
                                                 

okja3$fee <- ifelse(okja3$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') & okja3$time == '모닝', 6000,
             ifelse(okja3$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') & okja3$time == '브런치', 8000,
             ifelse(okja3$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') & okja3$time == '데이라이트', 9000,
             ifelse(okja3$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') & okja3$time == '프라임', 9000,
             ifelse(okja3$day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday') & okja3$time == '문라이트', 8000, 7000)))))

okja3$fee <- ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '모닝', 7000,
             ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '브런치', 10000,
             ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '데이라이트', 10000,
             ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '프라임', 10000,
             ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '문라이트', 10000,
             ifelse(okja3$day %in% c('Friday', 'Saturday', 'Sunday') & okja3$time == '나이트', 8000, okja3$fee))))))


okja3
str(okja3)
View(okja3)
head(okja3)
head(okja3)
write.csv(okja3, 'data/okja.csv',
          row.names = F)

## 그래프 그리기
# 가정1 : 영화를 본 시간에 평점을 작성.
# 가정2 : 재미가 있으면 평점이 높음.

rawokja <- read.csv('data/okja.csv', header = T)
head(rawokja)

okja <- rawokja %>%
  select(score, date, time, month, day, fee)
head(okja)

# 01. 월별에 따른 평점
month_score <- okja %>%
  select(score, month) %>%
  group_by(month) %>%
  summarise(month_score_mean = mean(score))
month_score

display.brewer.all()
palete <- brewer.pal(12, 'Paired')



ggplot(month_score, aes(x= month, y= month_score_mean, fill= month)) +
  geom_bar(stat = 'identity', color = 'black') +
  ggtitle('월별에 따른 평균평점') +
  xlab('월') +
  ylab('평균평점') +
  theme_classic() +
  theme_bw(base_family = '맑은 고딕') +
  theme(plot.title = element_text(size=20, color = 'red',
                                  hjust = 0.5, vjust=0)) +
  theme(axis.title.x = element_text(size = 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.title.y = element_text(size= 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.text.x = element_text(angle=90, size = 10, color='black')) +
  theme(axis.text.y = element_text(angle=0, size = 10, color='black')) +
  theme(legend.title = element_text(face="italic", colour="darkblue",size=10)) +
  geom_text(aes(y=month_score_mean - 0.2, label= paste(round(month_score_mean,1), '점')),
            color='black', size=5)

# reorder(month, desc(month_score_mean)
# aes(reorder(job, c('January','February','March','April','May','June','July','August','September','October','November','December'))
# scale_x_discrete(limits = c("trt1", "trt2", "ctrl"))

# 02. 요일에 따른 평점
day_score <- okja %>%
  select(day, score) %>%
  group_by(day) %>%
  summarise(day_score_mean = mean(score))
day_score

ggplot(day_score, aes(x= day, y= day_score_mean, fill= day)) +
  geom_bar(stat = 'identity')

# 03. 주말 시간에 따른 평점
okja
weekend_score <- okja %>%
  filter(day %in% c('Friday', 'Saturday', 'Sunday')) %>%
  select(time, score) %>%
  group_by(time) %>%
  summarise(weekend_score_mean = mean(score))
weekend_score

ggplot(weekend_score, aes(x= time, y= weekend_score_mean, fill= time)) +
  geom_bar(stat = 'identity')


