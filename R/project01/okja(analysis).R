### 옥자평점 분석(naver)

setwd('D:/Heechul/Project/R/project01')

## 필요패키지
library(KoNLP)
library(wordcloud)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(extrafont)
library(wordcloud2)
useSejongDic()
library(ggplot2)
library(xlsx)
library(write)
library(reshape2)
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

okja3$주 <- ifelse(okja3$요일 %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday'), '평일', '주말')
okja3 
str(okja3)
View(okja3)
head(okja3)
write.csv(okja3, 'data/okja.csv',
          row.names = F)

### 그래프 그리기

rawokja <- read.csv('data/okja.csv', header = T)
head(rawokja)

okja <- rawokja %>%
  select(평점, 시간대, 월, 요일, 요금, 주)
head(okja)

### 01. 월별에 따른 평점
month_score <- okja %>%
  select(평점, 월) %>%
  group_by(월) %>%
  summarise(month_score_mean = mean(평점)) %>%
  arrange(month_score_mean)
month_score

# 월별에 따른 평균평점
ggplot(month_score, aes(x= reorder(월, -month_score_mean), y=month_score_mean, fill = 월)) +
  geom_bar(stat = 'identity', color = 'black') +
  ggtitle('월별에 따른 평점') +
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
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none') + 
  geom_text(aes(y=month_score_mean - 0.2, label= paste(round(month_score_mean,1), '점')),
            color='black', size=4)

# 정규성 검토
shapiro.test(okja$평점[okja$월 == 'January'])
shapiro.test(okja$평점[okja$월 == 'February'])
shapiro.test(okja$평점[okja$월 == 'March'])
shapiro.test(okja$평점[okja$월 == 'April'])
shapiro.test(okja$평점[okja$월 == 'May'])
shapiro.test(okja$평점[okja$월 == 'June'])
shapiro.test(okja$평점[okja$월 == 'July'])
shapiro.test(okja$평점[okja$월 == 'August'])
shapiro.test(okja$평점[okja$월 == 'September'])
shapiro.test(okja$평점[okja$월 == 'Occtober'])
shapiro.test(okja$평점[okja$월 == 'November'])
shapiro.test(okja$평점[okja$월 == 'December'])
## 유의수준 0.05보다 작게 나왔으므로, 정규분포를 따르지 않는다.

## 정규분포를 따른다는 가정하에 검정
# 검정(일원분산분석)
# H0 : 차이가 없다
# H1 : 차이가 있다
ow <- lm(평점 ~ 월, data = okja)
anova(ow)

# 결론 : 월별에 따른 평점의 차이가 있는지 알아본 결과
#        p-value 값이 유의수준 0.05보다 적게 나와 영가설을 기각하고 대안가설을 채택합니다.
#        따라서 월별에 따른 평점의 차이는 적어도 하나 이상은 차이가 있는것으로 판단됩니다.


### 02. 평일과 주말에 따른 평점
week_score <- okja %>%
  select(주, 평점) %>%
  group_by(주) %>%
  summarise(week_score_mean = mean(평점))
week_score

ggplot(week_score, aes(reorder(주, -week_score_mean), y= week_score_mean, fill= 주)) +
  geom_bar(stat = 'identity') +
  ggtitle('평일과 주말에 따른 평점') +
  xlab('') +
  ylab('평균평점') +
  theme_classic() +
  theme_bw(base_family = '맑은 고딕') +
  theme(plot.title = element_text(size=20, color = 'red',
                                  hjust = 0.5, vjust=0)) +
  theme(axis.title.x = element_text(size = 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.title.y = element_text(size= 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.text.x = element_text(angle=0, size = 10, color='black')) +
  theme(axis.text.y = element_text(angle=0, size = 10, color='black')) +
  geom_text(aes(y=week_score_mean - 0.2, label= paste(round(week_score_mean,1), '점')),
            color='black', size=5)

# 정규성 검토
shapiro.test(okja$평점[okja$주 == '평일'])
shapiro.test(okja$평점[okja$주 == '주말'])

# 1. 분산 동일성 검점(2-sample T)
# H0 : 분산이 같다
# H1 : 분산이 다르다
var.test(okja$평점 ~ okja$주)

# 결론 : p-value 값이 유의 수준 0.05 보다 높은 0.07016로 나타나 영가설을 기각할 수 없습니다.
#        따라서 분산이 같다고 결론을 내릴 수 있습니다.

# 2. 평균 동일성 검정
# H0 : 평점의 차이가 없다(평균이 같다)
# H1 : 평점의 차이가 있다(평균이 다르다)
t.test(okja$평점 ~ okja$주, alternative = 'two.sided', var.equal = T)

# 결론 : p-value 값이 유의 수준 0.05 보다 높은 0.0668로 나타나 영가설을 기각할 수 없습니다.
#        따라서 평일과 주말에 따른 평점의 차이는 있다고 판단됩니다.



### 03. 요일에 따른 평점
day_score <- okja %>%
  select(평점, 요일) %>%
  group_by(요일) %>%
  summarise(day_score_mean = mean(평점)) %>%
  arrange(day_score_mean)
day_score

# 요일에 따른 평점
ggplot(day_score, aes(x= reorder(요일, -day_score_mean), y=day_score_mean, fill = 요일)) +
  geom_bar(stat = 'identity', color = 'black') +
  ggtitle('요일에 따른 평점') +
  xlab('요일') +
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
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none') + 
  geom_text(aes(y=day_score_mean - 0.2, label= paste(round(day_score_mean,1), '점')),
            color='black', size=4)

# 정규성 검토
shapiro.test(okja$평점[okja$요일 == 'Monday'])
shapiro.test(okja$평점[okja$요일 == 'Tuesday'])
shapiro.test(okja$평점[okja$요일 == 'Wednesday'])
shapiro.test(okja$평점[okja$요일 == 'Thursday'])
shapiro.test(okja$평점[okja$요일 == 'Friday'])
shapiro.test(okja$평점[okja$요일 == 'Saturday'])
shapiro.test(okja$평점[okja$요일 == 'Sunday'])

## 결론 : 유의수준 0.05보다 작게 나왔으므로, 정규분포를 따르지 않는다.

# 검정(일원분산분석)
# H0 : 차이가 없다
# H1 : 차이가 있다
ow <- lm(평점 ~ 요일, data = okja)
anova(ow)

# 결론 : 요일에 따른 평점의 차이가 있는지 알아본 결과
#        p-value 값이 유의수준 0.05보다 적게 나와 영가설을 기각하고 대안가설을 채택합니다.
#        따라서 요일에 따른 평점의 차이는 있는것으로 보입니다.


### 04. 요금에 따른 평점
fee_score <- okja %>%
  select(요금, 평점) %>%
  group_by(요금) %>%
  summarise(fee_score_mean = mean(평점))
fee_score$요금 <- as.factor(fee_score$요금) 

ggplot(fee_score, aes(reorder(x= 요금, -fee_score_mean), y= fee_score_mean, fill= 요금)) +
  geom_bar(stat = 'identity') +
  ggtitle('요금에 따른 평점') +
  xlab('요금') +
  ylab('평균평점') +
  theme_classic() +
  theme_bw(base_family = '맑은 고딕') +
  theme(plot.title = element_text(size=20, color = 'red',
                                  hjust = 0.5, vjust=0)) +
  theme(axis.title.x = element_text(size = 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.title.y = element_text(size= 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.text.x = element_text(angle=0, size = 15, color='black')) +
  theme(axis.text.y = element_text(angle=0, size = 10, color='black')) +
  theme(legend.title = element_text(size=10)) +
  geom_text(aes(y=fee_score_mean - 0.3, label= paste(round(fee_score_mean,1), '점')),color='black', size=5)

# 검정(일원 분산분석)
# H0 : 차이가 없다
# H1 : 차이가 있다
ow <- lm(평점 ~ 요금, data = okja)
anova(ow)

# 결론 : 요금에 따라 평점이 차이가 있는지 알아본 결과
#        p-value 값이 유의수준 0.05보다 적게 나와 영가설을 기각하고 대안가설을 채택합니다.
#        따라서 요금에 따라 평점의 차이가 있다고 판단됩니다.


### 05. 시간대에 따른 평점
time_score <- okja %>%
  select(시간대, 평점) %>%
  group_by(시간대) %>%
  summarise(time_score_mean = mean(평점))
time_score

ggplot(time_score, aes(x= 시간대, y= time_score_mean, fill= 시간대)) +
  geom_bar(stat = 'identity') +
  ggtitle('시간대에 따른 평점') +
  xlab('시간대') +
  ylab('평균평점') +
  theme_classic() +
  theme_bw(base_family = '맑은 고딕') +
  theme(plot.title = element_text(size=20, color = 'red',
                                  hjust = 0.5, vjust=0)) +
  theme(axis.title.x = element_text(size = 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.title.y = element_text(size= 15, color = 'red',
                                    hjust = 0.5, vjust = 0)) +
  theme(axis.text.x = element_text(angle=, size = 10, color='black')) +
  theme(axis.text.y = element_text(angle=0, size = 10, color='black')) +
  theme(legend.title = element_text(size=10)) +
  scale_fill_discrete(labels = c('나이트(24:00~)', '데이라이트(13:00~)', '모닝(06:00~)', '문라이트(22:00~)', '브런치(10:00~)', '프라임(16:00~)')) +
  geom_text(aes(y=time_score_mean - 0.3, label= paste(round(time_score_mean,1), '점')),color='black', size=5)

# 검정(일원 분산분석)
# H0 : 차이가 없다
# H1 : 차이가 있다
ow <- lm(평점 ~ 시간대, data = okja)
anova(ow)

# 결론 : 시간대에 따라 평점이 차이가 있는지 알아본 결과
#        p-value 값이 유의수준 0.05보다 적게 나와 영가설을 기각하고 대안가설을 채택합니다.
#        따라서 시간대에 따라 평점의 차이가 있다고 판단됩니다.


# scale_fill_discrete(labels = c('나이트(24:00~)', '모닝(06:00~)', '브런치(10:00~)','데이라이트(13:00~)', '프라임(16:00~)', '문라이트(22:00~)')) +

