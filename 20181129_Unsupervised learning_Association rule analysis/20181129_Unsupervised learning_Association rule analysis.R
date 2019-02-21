#데이터 transaction 으로 변환
 #행렬 : 상관행렬 거리행렬 transaction행렬
 #transaction행렬 : 열 fix, 행 반복 => 중복성 파악해서 분석
#install.packages("arules")
library(arules)
a_list <- list(
  c("a", "b", "c"), #데이터 조합 => item구별 (5)
  c("a", "b"),
  c("a", "b", "d"),
  c("c", "e"),
  c("a", "b", "d", "e")
)
names(a_list) <- paste("Tr", c(1:5), sep="")
a_list
trans1 <- as(a_list, "transactions") #연관분석시 transaction 행렬로 변환
summary(trans1)
image(trans1)

#데이터 프레임으로부터 변환
a_df <- data.frame(
  age = as.factor(c(6, 8, NA, 9, 16)),
  grade = as.factor(c("A", "C", "F", NA, "C")),
  pass = c(TRUE, TRUE, FALSE, TRUE, TRUE))
a_df
trans3 <- as(a_df, "transactions")
inspect(trans3)             #내부를 보기 위해서,
as(trans3, "data.frame")    #전체적으로 보기위해서는 data.frame으로 변환해야 안에있는 데이터를 볼 수 있다.


#지지도(support): A와 B가 포함된 거래수 / 전체 거래수.
#신뢰도(confidence) : A와 B가 포함된 거래수 비율. / A를 포함한 거래수 비율.
#향상도(lift) : 신뢰도 / B가 포함될 거래수 비율.

#apriori알고리즘은 transaction 행렬에만 적용 가능.
tran <- read.transactions("tran.txt", format="basket", sep=",")
tran
inspect(tran)
rule <- apriori(tran, parameter = list(supp=0.3, conf=0.1))
# itemset(lhs-rhs)에 대한 support/confidence, lift, count값을 계산 하는 것이 apriori알고리즘이다.
inspect(rule)

#여기서 supp을 0.1로 바꿔보면, 달라지죠.
#경우의 수 조합으로 대량 데이터가 발생하게 됩니다. 0.1 => 10%
#경우의 수가 아까는 16개였고, 지금은 35개.

rule <- apriori(tran, parameter = list(supp=0.1, conf=0.1))



#경우의 수 조합을 줄여주는 방법 maxlen.
rule <- apriori(tran, parameter = list(supp=0.1, conf=0.8, maxlen=10))



data("Groceries") #transaction 9835 transactions (rows) / 169 items (colums)
                  #비정방행렬 => SVD(특이행렬분해) => l직교, value정방행렬(대각행렬), r직교행렬.
str(Groceries)
Groceries
summary(Groceries)
itemFrequencyPlot(Groceries, topN = 15)
groceries<- Groceries
gdf  <- as(groceries, 'data.frame')#데이터 프레임으로 바꿔줘야 안에를 볼 수 있습니다.
head(gdf)

inspect(groceries[1:10]) #트랜젝션일 경우에는 인스펙트로 봅니다.
inspect(subset(groceries[1:200], size(groceries[1:200])>4))
itemFrequencyPlot(groceries, support=0.1) #0.1이상의 지지도 가진 것만 보여줘라.

arules <- apriori(groceries, parameter = list(support=0.1, confidence=0.8, minlen=1)) #maxlen(아이템 조합이 minlen
#건수가 없지? support와 confidence 재조정.

arules <- apriori(groceries, parameter = list(support=0.01, confidence=0.08, minlen=1))
#472 rules가 나옵니다. 줄이려면, support값을 조절해봅니다. 0.01 -> 0.05

arules <- apriori(groceries, parameter = list(support=0.05, confidence=0.08, minlen=1))


#그 다음에 정렬해주는데 confidence 혹은 lift 값으로 정렬해줍니다.
rules<- sort(arules, decreasing=T, by="confidence")
rules
summary(arules)
inspect(arules)
inspect(sort(arules, by = 'lift')[1:10])
#lift 값이 높다는 것은 향상도가 높다는 것을 의미합니다.
# 높다는 것은 2개를 같이 구매하는 경우가 높다는 것을 의미.
yogurt <- subset(arules, lhs %in% 'yogurt')
yogurt
inspect(yogurt)


#그 다음에 시각화를 해 줍니다.
#install.packages("arulesViz")
library(arulesViz)
plot(arules[1:5])
#plot하게 되어지면, 밑의 축은 support y축은 confidence 값 나와서 대충 데이터가 어떻게 되어 있는지 확인가능.

plot(arules[1:5], method="graph", engine='interactive') #사회관계망분석 : graph
#데이터 보면, 서로간에 관계가 없습니다.

#15로 늘려보겠습니다. 2개는 관련성이 있네요.
plot(arules[1:15], method="graph", engine='interactive') #사회관계망분석 : graph
#whole milk를 샀을 때만, 관계가 있고, 나머지는 연관이 없는 것으로 나옵니다.

#위에 2번째 것을 다시 실행시켜보고(arules <- apriori(groceries, parameter = list(support=0.01, confidence=0.08, minlen=1)) )
# ,  50개로 늘려보겠습니다.
plot(arules[1:50], method="graph", engine='interactive') #사회관계망분석 : graph


#data("AdultUCI") #income(소득)데이터 (성별, 인종별)

# 연관분석 : 열(item)고정, 행은 중복 가능 => count
library(arulesViz)
library(arules)
data("AdultUCI") #income(소득)데이터 (성별, 인종별)
str(AdultUCI)
summary(AdultUCI)
head(AdultUCI)
class(AdultUCI) #data.frame => transaction.
#연속형 데이터 => 다량의 경우의 수 발생. (연속형 [너무 많은 데이터 나오기때문에] => 범주형 데이터로 변환 필요.(count중심 알고리즘 사용))
#범주형 => 신경망
#ordered는 순서형 팩터를 생성. cut은 범위를 분할하는 함수.
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)),
                             labels=c("young", "middel", "senior", "old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)),
                                        labels=c("part-time", "full-time", "over-time", "workaholic"))
AdultUCI[["capital-gain"]]<- ordered(cut(AdultUCI[["capital-gain"]],
                                         c( -Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0]),Inf)),
                                     labels=c("None", "Low", "High"))
AdultUCI[["capital-loss"]]<- ordered(cut(AdultUCI[["capital-loss"]],
                                         c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]]>0]), Inf)),
                                     labels= c("None", "Low", "High"))

#불필요한 변수 제거
library(arules)
AdultUCI[["fnlwgt"]]<- NULL
AdultUCI[["education-num"]]<- NULL
str(AdultUCI)
#연관분석의 데이터 포맷으로 변경(transactions 포맷)
# 연관분석은 범주형으로 바꾸는 것을 중요하게 생각해야 합니다.

Adult_new <- as(AdultUCI, "transactions")
str(Adult_new)
summary(Adult_new)
#support와 confidence의 역할이 중요(연관데이터 범위를 결정)
basket_rules<- apriori(Adult_new, parameter=list(sup = 0.08, conf=0.5, target="rules"))

inspect(basket_rules[1:10])

#건수가 많으면 보기가 불편하니까 web에서 보겠습니다.
#건수를 좀 줄여보겠습니다

basket_rules<- apriori(Adult_new, parameter=list(sup = 0.02, conf=0.8, target="rules"))

p<- inspectDT(basket_rules)
htmlwidgets::saveWidget(p, "arules_2.html", selfcontained =FALSE)
browseURL("arules_2.html")


#이중에 일부만 뽑아보겠습니다.
basket_rules<- apriori(Adult_new, parameter=list(sup = 0.08, conf=0.5, target="rules"))

small_income <- subset(basket_rules, subset=lhs %in% "income=small" & lift>1.1) #in을 사용해서 하면 여러개 데이터 가능/
length(small_income)
inspect(small_income)
p <- inspectDT(small_income)
htmlwidgets::saveWidget(p, "small_income_2.html", selfcontained = FALSE)
browseURL("small_income_2.html")



#문제
#1) '가족 관계' 및 '교육수준'의 '소득'과의 연관성을 확인해 보시오


basket_rules<- apriori(Adult_new, parameter=list(sup = 0.08, conf=0.5, target="rules"))

levels(AdultUCI$education)
total_income <- subset(basket_rules, subset=(lhs %in%  c("education=Preschool", "education=Doctorate"))
                       & rhs %in% "income=large" |rhs %in% "income=small")
total_income
tot<- inspect(head(sort(total_income, decreasing = TRUE, by="lift"), n=30))
plot( tot, method="graph", engine="interactive")

p<- inspectDT(total_income)
htmlwidgets::saveWidget(p, "total_income_2.html", selfcontained = FALSE)
browseURL("total_income_2.html")




#2) '주당 일하는 시간'과 '소득'과의 관계를 확인해보시오

levels(AdultUCI$'hours-per-week')
hours_income<- subset(basket_rules, subset=(lhs %in% c("hours-per-week=part-time","hours-per-week=full-time","hours-per-week=over-time","hours-per-week=workaholic"))
                                                       & rhs %in% "income=large" | rhs %in% "income=small")
hours_income
plot(hours_income, method="graph", engine="interactive")

#appearance로 출력을 제한 가능
#list(sup=0.08, conf=0.5)로 제한 lift>1.1로 정렬하거나 subset으로 출력.
basket_rules<- apriori(Adult_new, parameter=list(sup = 0.08, conf=0.5),
                       appearance = list(rhs=c("hours-per-week=part-time", "hours-per-week=full-time",
                                               "hours-per-week=over-tiem", "hours-per-week=workaholic") ))

basket_rules
basket<- inspect( head(sort(basket_reules, decreasing=T, by="lift"), n=30))
subrules <- subset(basket_rules, lift>1.1)
plot(subrules, method="graph", engine="interactive")



#3) 기타 위의 데이터로부터 자기가 주장하고자 하는 내용을 확인하고 의견을 제시해보시오 (3건이상)


basket_rules<- apriori(Adult_new, parameter=list(sup = 0.1, conf=0.2, minlen=1, target="rules"))

basket_rules
hour_work <- subset(basket_rules, subset = rhs %in% "income=small" & lhs %in% "hours-per-week=over-time")
plot(hour_work, method="graph", engine="interactive")
inspect(hour_work)






#흑인과 백인에 따른 임금 차이가 존재한다.

levels(AdultUCI$'race')
race_income1<- subset(basket_rules, subset=(lhs %in% c("race=Black", "race=White"))
                      & rhs %in% "income=small"| "income=large")
race_income1
plot(race_income1, method="graph", engine="interactive")






#
# Adult 데이터 셑을 대상으로 다음을 수행하시오(transaction)

# 1. 최소 support = 0.5 최소 confidence = 0.8을 지정하여 연관규칙을 생성하시오.
# 2. 연관 분석 결과를 연관어 네트워크 형태로 시각화 하시오.
# 3. lhs가 white인 규칙만 subset으로 작성하고 연관어를 시각화 하시오

#선생님답
white.lhs <- subset(rules, lhs %in% "race=White")
inspect(white.lhs)

plot(white.lhs, method="grouped")
plot(white.lhs, method="graph", control=list(layout=igraph::in_circle()))
plot(white.lhs, method="graph",
     control=list(nodeCol=grey.colors(10), edgeCol=grey(.7), alpha=1))

whiteORus.lhs <- subset(rules, lhs %in% "race=White", lhs %in% "native-country=United-States")
inspect(whiteORus.lhs)
plot(whiteORus.lhs, method="grouped")

rules <- apriori(Adult, parameter=list(support=0.3, confidence=0.5))
husband.rhs<- subset(rules, rhs %in% "relationship=Husband")
inspect(husband.rhs)
plot(husband.rhs, method="grouped")

inspect(head(sort(rules, decreasing = T, by=c("support", "confidence")), 3))




# 4. lhs가 백인이거나 미국인을 대상으로 subset을 작성하고 연관어를 시각화 하시오

# 5. rhs가 husband인 단어를 포함한 규칙을 서브셑으로 작성하고 연관어 시각화 하시오.
# 6. 결과를 support, confidence와 기준으로 정렬한 다음 상위 3개만 출력하시오.






#연관분석 : 열(fix)와 행(중복) count
#recommendation(추천) -유사도(벡터와 벡터)
# item기반 추천, user사용자기반 추천
#    열기반          행기반

#추천하는 방식이
#                 IBCF(item), UBCF(user) 협업필터 를 이용해서 데이터를 읽고.


#install.packages("recommenderlab")
library(recommenderlab)

movie <- read.csv(file.choose(), header = T)
movie
str(movie)
movie_t <- t(movie[, -c(1,5,6)]) #유저 행, 영화 열 => 유저 열, 영화 행
movie_t
class(movie_t)
colnames(movie_t) <- c("a", "b", "c", "d", "e")
movie_t
##
cor(movie_t)
library(reshape2)
movie_long<- melt(id=1, movie) #casting 하기 위해서 melt 사용.
movie_long
names(movie_long) <- c('user', 'movie', 'rating')
movie_long<- subset(movie_long, rating!=0)
movie_long
length(movie_long$rating)
#데이터를 멜트한 다음 realRatingMatrix 변환/ 리스트나, 매트릭스, 데이터프레임 => transaction
movie_real<- as(movie_long, "realRatingMatrix") #평점 행렬로 변환/ 연관분석의 transaction 행렬과 비슷.
dim(movie_real)
movie_real
as(movie_real, "matrix") #평점행렬이라 수치 고로 매트릭스 변환.          #직접볼수 없다. 변환해야 보인다.
# 열이 아이템/ 행이 유저.
#    열기반 추천하면 아이템 기반 추천
#    행기반 추천하면 유저 기반 추천하기 된다.

# 데이터를 나누었다는 말은, train데이터 (4명), test데이터(1명) 분리
trainSet <- movie_real[c(2:5),]
trainSet
as(trainSet, 'matrix')
recommTarget <- movie_real[1,]
recommTarget
as(recommTarget, 'matrix')

#UBCF : user base cooperative filter 사용자 기반 추천 예, 유사도를 Pearson상관계수 유사도 사용
recom_model<- Recommender(trainSet, method="UBCF", parameter="Pearson")
recom_model
recomm_list <- predict(recom_model, recommTarget, n=2) #2개 추천   #행을 기준으로 유사도를 재고
recom_result <- as(recomm_list, "list")
recom_result
as(recomm_list, 'matrix')





#문제
# titanic.raw.csv 데이터를 로딩한 다음 데이터로부터 유의미한 연관성 정보를 추출해보시오
# - 선실, 클래스별, 어린이 어른 별 생존관계를 분석하시오
# - 2가지 유의미한 정보를 추출하여 분석한 다음 시각화해보시오


ti <- read.csv(file.choose(), header = T, sep=";")
str(ti)
summary(ti)
class(ti)

ti_new <-as (ti, "transactions")
class(ti_new)
str(ti_new)
summary(ti_new)
ti_rules <- apriori(ti_new, parameter=list(sup= 0.05, conf=0.8, target="rules"))

rules<- sort(ti_rules, decreasing=T, by="lift")
inspect(rules[1:10])

plot(ti_rules[1:15], method="graph", engine='interactive')

class_survive <- subset(ti_rules, subset=(lhs %in%  c("Class=1st", "Class=2nd","Class=3rd","Class=Crew")
                                          & rhs %in% "Survived=Yes" |rhs %in% "Survived=No"))



plot(class_survive, method="graph", engine="interactive")


age_survive <- subset(ti_rules, subset=(lhs %in%  c("Age=Adult", "Age=Child"))
                        & rhs %in% "Survived=Yes" |rhs %in% "Survived=No")

plot(age_survive, method="graph", engine="interactive")
