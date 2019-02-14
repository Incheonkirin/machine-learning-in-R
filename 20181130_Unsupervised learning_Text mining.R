#정규표현식

library(stringr)
str_extract("abcd12aaa33", "[0-9]{2}") #all이 안붙으면, 처음 패턴만 출력하게 됨.
str_extract_all("abcd12aaa33","[0-9]{2}")
str<- 'hongikdong35lee45kang55안창호25'
(result <- str_extract_all(str, '[a-z]{3}'))
(result <- str_extract_all(str, '[0-9]{2}'))
(result <- str_extract_all(str, '[가-히]{3}'))
x<- "R Totorial"
gsub("ot", "ut", x) #gsub 쓸 일이 많다.
gsub("tot", "ut", x, ignore.case=T)
(y <- gsub("[[:lower:]]", "-",x)) #소문자를 대체시켜준다.



#install.packages("tm")
library(tm)

data("crude")
class(crude) #VCorpus #527 => inspect를 이용해 내부 확인.
str(crude)
crude[[1]]
(as.list(crude))
(f <- content_transformer(function(x, pattern) gsub(pattern, "", x)))#패턴에 해당 되는 것을 공백처리
tm_map(crude, f, "[[:digit:]]+")[[1]]

summary(crude)
inspect(crude[1:3])

#행 : 단어 / 열 : document
(tdm <- TermDocumentMatrix(crude)[1:10, 1:10])
#Sparsity 단어10개 문서10 면 100개. 그 중 91.
#sparce matrix 희소행렬
#가중치 : tf / tfidf 를 이용해서
Docs(tdm) #document 번호가 나옵니다.
nDocs(tdm) #document 수가 나옵니다.
nTerms(tdm) #
Terms(tdm)


tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation= TRUE,
                                         removeNumber= TRUE,
                                         stopwords = TRUE, wordLengths=c(2, Inf)))# 1자는 제외

meta(crude[[1]])
DublinCore(crude[[1]]) #Metadata Management
meta(crude[[1]], tag="topics")
meta(crude[[1]], tag="comment") <- "A short comment" #metadata 수정
meta(crude[[1]], tag="topic") <- NULL
DublinCore(crude[[1]], tag = "creator") <- "Ano Nymous"
DublinCore(crude[[1]], tag = "format") <- "XML"
DublinCore(crude[[1]])
meta(crude[[1]])

findFreqTerms(tdm, lowfreq=10) #하한경계선 설정
findFreqTerms(tdm, 2, 3)
findAssocs(tdm, 'dlrs', 0.25) #관련 terms 확인
findAssocs(tdm, c("oil", "opec", "xyz"), c(0.7, 0.75, 0.1)) #리스트로 결과 출력 ( 사이즈가 상이하기 때문)
findMostFreqTerms(tdm) #각 문서별로 6개씩 출력.
findMostFreqTerms(tdm, INDEX = rep(1:2, each=10L))

x <- as.matrix(tdm)
write.csv(x, "crude_words.csv")
dist(x, method = "euclidean") #단어별 거리값



#install.packages("wordcloud")
library(wordcloud) #칼러 패키지 자동설치.
(m <- as.matrix(tdm)) #단어가 행으로오고 document가 행으로 온다.
(wordFreq <- sort(rowSums(m), decreasing = T)) # 단어가 행으로 각 문서에 몇번 나타나는가. 내림차순.행으로 합계를 내라.
set.seed(375)
pal <- brewer.pal(8, "Dark2")
(words=names(wordFreq)) #단어만 나옴.

#                                                                            글자 회전   컬러=팔레트
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=10, random.order=F, rot.per=.1, colors=pal)
#                                               최소경계값.   순차적으로(중간이 많은것)



#clustering
myTdm2 <- removeSparseTerms(tdm, sparse=0.80) #희소도에 따른 필터링
(m2<- as.matrix(myTdm2))
(distMatrix<- dist(scale(m2))) #clustering을 위한 거리행렬값 계산

fit <- hclust(distMatrix, method="ward.D2") #워드행렬 = 거리의 제곱값
plot(fit)
rect.hclust(fit, k=10)
(groups<- cutree(fit,k=10))

#kmeans #4개의 군집으로 나눌 때
(m3 <- t(m2))
k <- 4
kmres <- kmeans(m3, k)
round(kmres$centers, digits=3) #센터점으로 그룹의 의미파악
for( i in 1:k){
  cat(paste("cluster", i,":", sep=""))
  s<- sort(kmres$centers[i, ], decreasing=T)
  cat(names(s)[1:3], "\n")
}

#차원 축소 시각화
td.mat <- as.matrix(TermDocumentMatrix(crude))
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat
#cmdscale 다차원 척도법( multidemensional scaling) 알고리즘 이용한 시각화를 위한 함수.
library(ggplot2)
fit <- cmdscale(dist.mat, eig=TRUE, k=2)#2차원 축소 => 시각화하기 위함.
points <- data.frame(x = fit$points[,1], y=fit$points[,2])
ggplot(points, aes(x=x, y=y)) + geom_point(aes(x=x, y=y))


###
#텍스트 마이닝은 뒷단을 하기위해 하는 것이다.
#~단어별 거리값 (텍스트마이닝)


#install.packages("testthat")
#install.packages("KoNLP")
library(testthat)
library(KoNLP)
options(encoding="UTF-8")
useSejongDic()
MorphAnalyzer("충북 청주시의 한 고등학교에서 고3 담임을 맡고 있는 강모(32) 교사는 수학능력평가시험(수능)이 끝난 후 고삐 풀린 제자들을 통제하는 일로 골머리를 앓고 있다.
")
extractNoun("내년 경제 전망이 올해보다 어두울 것으로 여겨지는 상황에서 미국은 계속해서 금리를 올릴 가능성이 있으므로 한은의 금리 셈법은 한결 복잡해질 것으로 보인다. ")
SimplePos09("내년 경제 전망이 올해보다 어두울 것으로 여겨지는 상황에서 미국은 계속해서 금리를 올릴 가능성이 있으므로 한은의 금리 셈법은 한결 복잡해질 것으로 보인다. ")
SimplePos22("내년 경제 전망이 올해보다 어두울 것으로 여겨지는 상황에서 미국은 계속해서 금리를 올릴 가능성이 있으므로 한은의 금리 셈법은 한결 복잡해질 것으로 보인다. ")

txt<- "중국에서 몰려온 황사 영향으로 부산의 공기 질은 30일 하루 내내 나쁠 것으로 예보됐다."

extractNoun(txt)

buildDictionary(ext_dic = c('sejong', 'woorimalsam'),
                user_dic = data.frame(term="몰려온", tag="ncn"),
                category_dic_nms=c('political'))

extractNoun(txt)
head(get_dictionary('user_dic'))


#띄어쓰기 잘 안되었다.

extractNoun("아버지가방에들어가셨다.")
extractNoun("아버지가방에들어가셨다.",autoSpacing=T)
MorphAnalyzer("아버지가방에들어가셨다.",autoSpacing=T)
SimplePos22("아버지가방에들어가셨다.",autoSpacing=T)

data <- "주인공 이명준은 대학 철학과 학생으로 동료 친구 50명과 함께 아버지의 친구 집에 얹혀 살고 있다."
str(data)
(data2 <- sapply(data, extractNoun, USE.NAMES=F))
head(unlist(data2), 30)
(data3<- unlist(data2) )
(data3<- Filter(function(x) {nchar(x) >= 2}, data3))
(data3 <- gsub("\\d+","",data3))
(data3 <- gsub("palpit","",data3))
(data3 <- gsub("log","",data3))
(data3 <- gsub(" ","",data3))
(data3 <- gsub("-","",data3))
(data3 <- gsub(")","",data3))
(data3 <- na.omit(data3))
write(unlist(data3), "exo_filtered.txt")

data4 <- read.table("exo_filtered.txt")
nrow(data4)
wordcount<- table(data4)
head(sort(wordcount, decreasing=T), 20)

library(RColorBrewer)
(palete <- brewer.pal(9, "Set3"))
wordcloud(names(wordcount), freq=wordcount, scale=c(5, 1), rot.per=0.25, min.freq=2,
          random.order=F, random.color=T, colors=palete)
legend(0.3, 1, "마이 블로그 랭킹 워드", cex=0.8, fill=NA, border=NA, bg="white",
       text.col="red", text.font=2, box.col="red")


install.packages("innstallr")
install.Rtools(TRUE)
##
reviews=read.csv("movie_reviews.csv", stringsAsFactors = F, row.names=1)
class(reviews)
str(reviews)
reviews_corpus=VCorpus(VectorSource(reviews$content)) # V corpus 반드시 앞에 V를 붙여야 한다.

tm_corpus<- tm_map(reviews_corpus, content_transformer(tolower))
tm_corpus<- tm_map(tm_corpus, removeNumbers)
tm_corpus<- tm_map(tm_corpus, removePunctuation)
tm_corpus<- tm_map(tm_corpus, removeWords, c("the", "and", stopwords("english")))
tm_corpus<- tm_map(tm_corpus, stripWhitespace)
tm_corpus
tm_corpus<- tm_map(tm_corpus, stemDocument)
inspect(tm_corpus[1])

review_dtm<-DocumentTermMatrix(tm_corpus) #가중치 기본 옵션 weightTf,
review_dtm
inspect(review_dtm[,])

review_dtm=removeSparseTerms(review_dtm, 0.95)
review_dtm
inspect(review_dtm[, ])
inspect(review_dtm[1, 1:20])

(findFreqTerms(review_dtm, 1000))
freq= data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE)) #colSums : Term이 열로 온다.
wordcloud(rownames(freq), freq[,1], max.words = 50, colors=brewer.pal(1, "Dark2"))
review_dtm_tfidf <- DocumentTermMatrix((tm_corpus, control=list(weighting=weightTfIdf)))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
darks<- brewer.pal(6, "Dark2")
wordcloud(rownames(freq), freq[,1], max.words=50, rot.per=0.2, colors=dark2)
review_dict <- findFreqTerms(review_dtm_tfidf, 5)
lengtth(review_dict)
review_dict


getwd()
library(plyr)
library(stringr)
#상담에서 감정분석 : 감정별 단어장이 필요(우울, 등...)
neg_words<- read.table("negative-words.txt", header=F, stringsAsFactors=F)[,1] #부정단어(domain knowledge가 고려)
pos_words<- read.table("positive-words.txt", header=F, stringsAsFactors=F)[,1] #긍정단어장
str(neg_words)
length(pos_words)
length(neg_words)
inspect(tm_corpus)

sent<- function(corpus, pos, neg){
  scores = lapply(corpus, function(corpus, neg, pos){ #copus에 있는 긍부정 단어를 카운팅하는 함수
    word.list=str_split(corpus, '\\s+')
    words = unlist(word.list)

    pos.matches = match(words, pos)
    neg.matches = match(words, neg)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos, neg)
}

comp<- sent(tm_corpus, pos_words, neg_words)
length(comp)
ll<- unlist(comp)
class(ll)
reviewsent <- data.frame(content=reviews$content, score=ll)
head(reviewsent)
str(reviewsent)

reviewsent$content <- as.character(reviewsent$content)
reviewsent$remark[reviewsent$score >= 1] = '긍정'
reviewsent$remark[reviewsent$score == 0] = '중립'
reviewsent$remark[reviewsent$score < 0] = '부정'
reviewsent$remark <- as.factor(reviewsent$remark)
result<- table(reviewsent$remark)
library(ggplot2)
plot(reviewsent$score,xlab="document", ylab="sent score", main="리뷰별 스코어")
pie(result, main="감정분석 결과", col=c("blue", "red", "green"), radius=0.8)
ggplot(reviewsent, aes(reviewsent$score, fill=reviewsent$remark)) + geom_dotplot()
ggplot(reviewsent, aes(reviewsent$score, fill=reviewsent$remark)) + geom_histogram()




#tax.txt 데이터를 로딩하여 text mining을 처리한 다음 wordcloud로 출력해보시오.

library(KoNLP)
library(RColorBrewer)
library(wordcloud)
useSejongDic()
mytext <- file("tax.txt", encoding="UTF-8")

myline <- readLines(mytext)
close(mytext)
#                                                                             지금까지 들어있던 usr dic 다 지워짐.
buildDictionary(ext_dic = "sejong", user_dic=data.frame("세액공제", "ncn"), replace_usr_dic = T)
buildDictionary(ext_dic = "sejong", user_dic=data.frame("환급금", "ncn"))
buildDictionary(ext_dic = "sejong", user_dic=data.frame("자기검증", "ncn"))
buildDictionary(ext_dic = "sejong", user_dic=data.frame(name<-c('워드클라우','전작권'), pos<-c('ncn','ncn')) )
get_dictionary('user_dic')
myword<-sapply(myline, extractNoun, USE.NAMES=F)
myword
result<-unlist(myword)
result2 <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 4}, result)
head(result2, 20)
#불필요한 단어 및 조사들 정리.
result2 <- gsub("것", "", result2) #tm_map함수제공, 한글에서ㅓ는 gsub함수를 사용하여 처리.
result2 <- gsub("저", "", result2)
result2 <- gsub("원","", result2)
result2 <- gsub("\\n", "", result2)
result2 <- gsub("\\d+", "", result2)
result2 <- gsub("\\.", "", result2)
head(result2, 20)
write(unlist(result2), "taxes.txt")

myword<- read.table("taxes.txt")
nrow(myword)
wordcont<-table(myword)
head(sort(wordcount, decreasing=T), 20)
palete<- brewer.pal(9, "Set1")
x11( )
wordcloud( names(wordcount), freq=wordcount, #scale=c(5, 1) gradient로 출력하라
           scale= c(5, 1), rot.per=0.5, min.freq=4, random.order = F, random.color=T,
           colors=palete
)
tax_count<- head(sort(wordcoun, decreasing=T), 20)
pie(tax_count, col=rainbow(10), radius=1)





#사회네트워크 분석
library(arules)

library(igraph) #연관분석에서 그라프 그릴때 썼었다. 패키지로도 존재함. graph interactive하게 작동
library(combinat)
f <- file("tax.txt",encoding = "UTF-8")
fl <- readLines()
close(f)
head(fl, 10)
tran<- Map(extractNoun, fl)
tran<- unique(tran)
tran<- sapply(tran, unique) #관계성 파악. 중복된 것 제거.
tran<- sapply(tran, function(x) {Filter(function(y) + {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)})
tran<- Filter(function(x){length(x) >=2}, tran)
library(combinat)
names(tran) <- paste("Tr", 1:length(tran), sep="")
names(tran)
wordtran<- as(tran, "transactions")
wordtran
inspect(wordtran)
wordtab<- crossTable(wordtran) #단어 상호관게 파악
wordtab
area <- apriori(wordtran, parameter=list(supp=0.07, conf=0.05))
