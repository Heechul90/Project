### 옥자평점 분석(naver)

setwd('D:/Heechul/Project/R/project01/photo')

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
okja <- gsub(' ','', okja)
okja <- gsub('\\.','', okja)
okja <- gsub(' ','', okja)
okja <- gsub("\\'",'', okja)
okja <- gsub('의미','주제', okja)
okja <- gsub('감성','감정', okja)
okja <- gsub('내용','주제', okja)
okja <- gsub('채식주의자','채식', okja)
okja <- gsub('채식주의','채식', okja)
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
head(sort(wordcount, decreasing = T), 100)

## 4. 워드 클라우드를 생성

display.brewer.all()
palete <- brewer.pal(9, 'Paired')
wordcloud(names(wordcount), freq=wordcount, scale=c(1.8,0.1), rot.per = 0.5,
          min.freq = 10, random.order = F, random.color = T, colors = palete)
legend(0.2, 1, '영화 옥자 분석', cex=1, fill=NA, border=NA,
       bg='white', text.col='red', text.font=10, box.col='black')

wordcloud2(wordcount, size=2,
           col='random-dark', backgroundColor='white',
           rotateRatio=3,
           fontFamily='배달의민족 연성')

wordcloud2(wordcount, figPath = 'pig.png', size = 0.5, backgroundColor = 'black')


