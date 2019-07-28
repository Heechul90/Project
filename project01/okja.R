### 옥자평점 분석(naver) - 종합

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

# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
windowsFonts(malgun=windowsFont("맑은 고딕"))        # 맑은고딕
windowsFonts(malgun=windowsFont("나눔고딕"))         # 나눔고딕
windowsFonts(malgun=windowsFont("Arial"))            # 영어폰트
windowsFonts(malgun=windowsFont("배달의민족 연성"))

### 1. Word Cloud 
# 3. 저장후 테이블로 불러서 공백 지우기
write(unlist(okja), 'data/reple.txt')
rev <- read.table('data/reple.txt')
nrow(rev)
rev
wordcount <- table(rev)
wordcount <- head(sort(wordcount, decreasing = T), 400)
class(wordcount)

## 4. 워드 클라우드를 생성

wordcloud2(data = wordcount,
           fontFamily='맑은 고딕',
           color = 'random-light',
           backgroundColor='black',
           rotateRatio = 0.5)



## 분석 : 영화 관객의 후기를 살펴본 결과, 영화에 대한 평은 배우, 감동, 자본주의, 연기 등 여러개가 있었지만 
##        가장 큰 관심은 재미로 나타났다.
##        그렇다면 이 영화를 언제 봐야 가장 재미를 느낄 수 있는지 분석해보자.


## 그래프 그리기
# 가정1 : 영화를 본 시간에 평점을 작성.
# 가정2 : 평점이 높으면 재미가 있음.

rawokja <- read.csv('data/okja.csv', header = T)
head(rawokja)

okja <- rawokja %>%
  select(score, date, time, month, day, fee)
head(okja)

# 01. 월별에 따른 평점
month_score <- okja %>%
  select(score, month) %>%
  group_by(month) %>%
  summarise(month_score_mean = mean(score)) %>%
  arrange(month_score_mean)
month_score


ggplot(month_score, aes(x= reorder(month, -month_score_mean), y=month_score_mean, fill = month)) +
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
  theme(legend.title = element_blank()) +
  theme(legend.position = 'none') + 
  geom_text(aes(y=month_score_mean - 0.2, label= paste(round(month_score_mean,1), '점')),
            color='black', size=5)


# 02. 요일에 따른 평점
day_score <- okja %>%
  select(day, score) %>%
  group_by(day) %>%
  summarise(day_score_mean = mean(score))
day_score

ggplot(day_score, aes(reorder(day, -day_score_mean), y= day_score_mean, fill= day)) +
  geom_bar(stat = 'identity') +
  ggtitle('요일에 따른 평균평점') +
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
            color='black', size=5)

# 03. 주말 시간에 따른 평점
weekend_score <- okja %>%
  filter(day %in% c('Friday', 'Saturday', 'Sunday')) %>%
  select(time, score) %>%
  group_by(time) %>%
  summarise(weekend_score_mean = mean(score))
weekend_score

ggplot(weekend_score, aes(x= time, y= weekend_score_mean, fill= time)) +
  geom_bar(stat = 'identity') +
  ggtitle('요일에 따른 평균평점') +
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
  theme(axis.text.x = element_text(angle=0, size = 15, color='black')) +
  theme(axis.text.y = element_text(angle=0, size = 10, color='black')) +
  theme(legend.title = element_text(size=10)) +
  scale_fill_discrete(labels = c('나이트(24:00~)', '모닝(06:00~)', '브런치(10:00~)','데이라이트(13:00~)', '프라임(16:00~)', '문라이트(22:00~)')) +
  geom_text(aes(y=weekend_score_mean - 0.3, label= paste(round(weekend_score_mean,1), '점')),
            color='black', size=5)
