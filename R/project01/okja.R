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
library(lubridate)
library(ggplot2)


# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
windowsFonts(malgun=windowsFont("맑은 고딕"))        # 맑은고딕
windowsFonts(malgun=windowsFont("나눔고딕"))         # 나눔고딕
windowsFonts(malgun=windowsFont("Arial"))            # 영어폰트
windowsFonts(malgun=windowsFont("배달의민족 연성"))

### 1. Word Cloud 
## 파일 불러오기
rawdata <- read.csv('data/review(naver).csv', header = T)
data <- rawdata %>%
  select(reple)
str(data)
head(data)

write.csv(data,'data/reple.csv',
          row.names=F)
data <- readLines('data/reple.csv')

okja <- sapply(data, extractNoun, USE.NAMES = F)
okja <- unlist(okja)
head(okja)

## 1. 단어 바꾸기 & 삭제하기
# 1) 한글, 영어 외는 삭제
okja <- str_replace_all(okja,'[^[:alpha:]]','')     # 한글, 영어 외는 삭제
okja

# 2) gsub() 함수로 바꾸기
okja <- gsub('의미','주제', okja)
okja <- gsub('감성','감정', okja)
okja <- gsub('내용','주제', okja)
okja <- gsub('채식주의자','채식', okja)
okja <- gsub('채식주의','채식', okja)
okja <- gsub('재밌었어요','재미', okja)
okja <- gsub('재밌게','재미', okja)
okja <- gsub('재밌어요','재미', okja)
okja

# 3) 필터링으로 지우기
okja <- Filter(function(x){nchar(x) >=2 & nchar(x) <=5 }, okja)
okja

# 4) 지울 단어 불러와서 반복문으로 지우기
txt <- readLines('data/okjagsub.txt')
i <- 1
for(i in 1 : length(txt)) {
  okja <- gsub((txt[i]), '', okja)
}
okja

## 2. 저장후 테이블로 불러서 공백 지우기
write(unlist(okja), 'data/reple.txt')
rev <- read.table('data/reple.txt')
nrow(rev)
rev
wordcount <- table(rev)
wordcount <- head(sort(wordcount, decreasing = T), 300)
class(wordcount)

## 3. 워드 클라우드를 생성

wordcloud2(data = wordcount,
           size = 1.2,
           fontFamily='맑은 고딕',
           color = 'random-light',
           backgroundColor='black',
           rotateRatio = 0.5)

legend(0.2, 1, '영화 옥자 분석')

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
            color='black', size=5) +
  abline(h= 8)


## 분석 : 월별에 따른 평점을 분석한 결과 7~8월이 8.5점, 2~3월이 8.4점으로 나타났다. 
##        여름방학 시기나, 겨울방학에 영화를 관람하는것이 평점이 높은것으로 나타났다.

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


## 분석 : 토요일, 일요일이 8.57, 8.52로 가장 평점이 높게 나와
##        주말에 영화를 관람하는 것이 평점이 높게 나왔음을 알 수 있습니다.


# 03. 요금에 따른 평점
fee_score <- okja %>%
  select(fee, score) %>%
  group_by(fee) %>%
  summarise(fee_score_mean = mean(score))
fee_score$fee <- as.character(fee_score$fee)
fee_score

ggplot(fee_score, aes(x= fee, y= fee_score_mean, fill= fee)) +
  geom_bar(stat = 'identity')

## 분석 : 1만원 일때 평점이 8.57로 가장 높게 나타났습니다.
##        따라서 주말에 브런치, 데이라이트, 프라임, 문라이트 시간에 보는것이
##        평점이 높다는 결론을 낼 수있습니다.

### 결론 : 3가지 그래프를 분석한 결과, 7,8월에, 주말에, 오전 10시부터 24시 사이에
###        평점이 높으므로, 큰 재미를 느끼기 위해서
###        7,8월에, 주말에, 오전 10시부터 24시 사이에 영화를 봐야 합니다.