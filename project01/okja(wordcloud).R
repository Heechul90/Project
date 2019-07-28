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



# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
windowsFonts(malgun=windowsFont("맑은 고딕"))        # 맑은고딕
windowsFonts(malgun=windowsFont("나눔고딕"))         # 나눔고딕
windowsFonts(malgun=windowsFont("Arial"))            # 영어폰트
windowsFonts(malgun=windowsFont("맑은 고딕"))
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

# 1. 한글, 영어 외는 삭제
okja <- str_replace_all(okja,'[^[:alpha:]]','')     # 한글, 영어 외는 삭제
okja

# 2. gsub() 함수로 바꾸기
okja <- gsub('감성','감정', okja)
okja <- gsub('내용','주제', okja)
okja <- gsub('채식주의자','채식', okja)
okja <- gsub('채식주의','채식', okja)
okja <- gsub('재밌었어요','재미', okja)
okja <- gsub('재밌게','재미', okja)
okja <- gsub('재밌어요','재미', okja)
okja

# 필터링으로 지우기
okja <- Filter(function(x){nchar(x) >=2 & nchar(x) <=5 }, okja)
okja

# 지울 단어 불러와서 반복문으로 지우기
txt <- readLines('data/okjagsub.txt')
i <- 1
for(i in 1 : length(txt)) {
  okja <- gsub((txt[i]), '', okja)
}
okja

# 3. 저장후 테이블로 불러서 공백 지우기
write(unlist(okja), 'data/reple.txt')
rev <- read.table('data/reple.txt')
nrow(rev)
rev
wordcount <- table(rev)
wordcount <- head(sort(wordcount, decreasing = T), 400)
wordcount

## 4. 워드 클라우드를 생성

wordcloud2(data = wordcount,
           fontFamily='맑은 고딕',
           color = 'random-light',
           backgroundColor='black',
           rotateRatio = 0.5)


