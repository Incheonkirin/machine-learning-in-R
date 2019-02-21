#distance 함수들
#install.packages("lsa")
library(lsa)
vec1 = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c(0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )
cosine(vec1, vec2)
vec3 = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0 )
matrix = cbind(vec1, vec2, vec3)
cosine(matrix) #sin아닌 cos을 사용하는 이유는? sin 유사도 라는 말은 없어. cos 유사도.
               #유사도 크면, 사이즈도 커야한다. cos 0도 = 1 "가깝다"라는 의미이다.
               #cos유사도에서 가장 큰 값은 1이다.
               # 내적 / ||a||||b|| ,    norm 크기 : 대각선길이 ,    vec1=c(1,1) => 1 414
               #"내적" 이란 ? 두개의 요소값을 곱한다음 더해.  ||A|| ||B|| cos theta
               #공분산행렬은 정방행렬이고 대칭행렬이다.

#install.packages("proxy")#proxy는  distance와 similarity 함수를 제공합니다.
library("proxy")
(x<- matrix(1:16, nrow=4))

(dist <- dist(x, method="euclidean"))
(dist <- dist(x, method="minkowski", p=2))
(dist <- dist(x, method="binary"))
(dist <- dist(x, method="manhattan")) #거리의 절대값.

(simil <- simil(x, method="manhattan"))
(simil <- simil(x, method="euclidean"))

#탐색적 clustering : hierarchical clustering
idx = sample(1:dim(iris)[1], 40)
irisSample= iris[idx,]
Species <- irisSample$Species
irisSample$Species= NULL
#거리값을 기준으로 클러스터링 함.
hc = hclust(dist(irisSample), method="ave")#평균연결
plot(hc, hang= -1, labels = iris$Species[idx])

rect.hclust(hc, k=3)
(groups= cutree(hc, k=3))
table(groups, Species)




##변수 3,4번을 기준으로 클러스터링
clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)
library(ggplot2)
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)
#원래의 종과, 클러스터링 종을 오버랩해서 출력
ggplot(iris, aes(Petal.Length, Petal.Width, color=iris$Species))+
  geom_point(alpha=0.4, size=3.5) + geom_point(col=clusterCut) +
  scale_color_manual(values = c('black','red','green'))



##확인적 클러스터링 : kmeans, 자료구조를 파악하는 용도, 다른 분석전에 시행해서 작업 편리성 도모.

#에러가 많이 나지만, 이 작업을 하는 것만으로도 데이터 구조를 알 수 있다.

set.seed(1)
iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3, trace=TRUE)) #그룹수: 3, 알고리즘 "Hartigan-wong"일 때만 사용 가능.
table(iris$Species, kmeans.result$cluster)#오분류
#2차원

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)

points(kmeans.result$center[, c("Sepal.Length", "Sepal.Width")],
       col= 1:3, pch= 8, cex= 2)

names(kmeans.result)
kmeans.result$cluster #데이터별 군집번호
kmeans.result$centers #군집갯수마ㅏㄴ큼 센터, 중심값으로부터 멀다.
print(kmeans.result$totss) #중심값으로부터의 거리값
print(kmeans.result$withinss) #3개의 그룹내에서 거리
print(kmeans.result$betweenss) #군집간 거리값 .



##caret패키지
library(caret)
set.seed(123)
(inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE))
class(inTrain)

training<- iris[inTrain,]
testing<- iris[-inTrain,]

training.data<- scale(training[-5])
summary(training.data)
iris.kmeans<- kmeans(training.data[,-5], centers= 3, iter.max=10000) #iter.max는 중심점을 잡아서 테스트하는 한계. 중심점을 랜덤으로 주기 때문에, 재계산을 수행 한계값.
training
training$cluster<- as.factor(iris.kmeans$cluster) #원래 데이터를 그룹별로 나누기 위해서 Cluster
class(iris.kmeans$cluster)
length(iris.kmeans$cluster)
iris.kmeans$cluster==3
qplot(Petal.Width, Petal.Length, colour=cluster, data=training)
table(training$Species, training$cluster)
#데이터분류
clust3 <- training[iris.kmeans$cluster ==3, ]
clust2 <- training[iris.kmeans$cluster ==2, ]
clust1 <- training[iris.kmeans$cluster ==1, ]
#각 그룹간의 median값 확인
summary(clust3)[3,]
summary(clust2)[3,]
summary(clust1)[3,]


#install.packages("NbClust")
#자동으로 k값 결정.
library(NbClust)
nc <- NbClust(training.data, min.nc=2, max.nc=15, method="kmeans")
nc$Best.n[1,]
nc$Best.n[2,]
table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Clusters의 수", ylab="Criteria 수",
        main="클러스터 갯수 결정 그래프")
#sum of square값으로 판단, nc: number of cluster
wssplot <- function(data, nc=15, seed=1234){
  wss<- (nrow(data)-1)*sum(apply(data,2,var))
  for( i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} #kmeans 그룹수별 호출, 그룹내 ss값의 합계.
  plot(1:nc, wss, type="b", xlab="Cluster수",
       ylab="그룹내 sum of squares")}
wssplot(training.data)




#diamond 를 이용한 클러스터링, 군집분석, 상관분석, 군집분류의
library(ggplot2)
data(diamonds)
str(diamonds)
nrow(diamonds)
t <- sample(1: nrow(diamonds), 1000)
test<- diamonds[t,]
dim(test)
head(test)
mydia<- test[c("price","carat","depth","table")]
head(mydia)
result<- hclust(dist(mydia), method="average")
result
plot(result, hang=-1)
result2 <- kmeans(mydia, 3) #해석 무슨이유인지.
result2
names(result2)
result2$cluster
mydia$cluster <- result2$cluster
head(mydia)
plot(mydia[,-5])
cor(mydia[,-5], method="pearson")                   #상관계수를 해 보았더니, 뭐가 제일 높니?
#install.packages("corrgram")
library(corrgram)
corrgram(mydia[,-5])
corrgram(mydia[,-5], upper.panel=panel.conf)
plot(mydia$carat, mydia$price)                      #price carat이 제일 높지?

#캐럿과 가격간의 관게와 클러스터 중심점 출력
plot(mydia$carat, mydia$price, col=mydia$cluster)   #price carat 해보았지.
result2$centers
points(result2$centers[,c("carat", "price")],
       col=c(3,1,2), pch=8, cex=5)




## 군집의 의미를 해석

#SNS 계정을 가진 30,000명의 청소년을 대상으로 4개의 기본정보와 36개의 관심분야에 대해 수집된 정보
teens <- read.csv("snsdata.csv")
set.seed(100)
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age)

teens$age<- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA) #나이값에 해당하지 않는 것들은 NA로.
summary(teens$age)

table(teens$gender, useNA="ifany")
mean(teens$age, na.rm=T)
#졸업년도를 기준으로 => 청소년, 평균나이로 대체
(avg <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=T)))
teens$age <- ifelse(is.na(teens$age), avg, teens$age)
teens$age[1:6]
summary(teens$age)

interests<- teens[5:40]
summary(interests)
interests_n <- data.frame(lapply(interests, scale)) #군집분석하기 전에 scale 함. 이상치 영향 받지 않도록 하기 위해서. 표준치 내에 들어올 수 있도록.
summary(interests_n)

teen_clusters<- kmeans(interests_n, 5)#청소년을 위한 분석에서 고등학생들을 5개의 그룹으로 나누었다.
                                      #범죄성향, 운동성향, 외모지향, 무기력, 브레인
names(teen_clusters)
teen_clusters$cluster[1:5] #데이터별 그룹이 지정 : 일반성향
teen_clusters$size
table(teen_clusters$cluster)
#결과에 대하여 그룹별 명명식을 진행.

teen_clusters$centers  #각 그룹은 원형태를 그리는데, 그 가운데의 좌표를 나타냄.
teens$cluster <- teen_clusters$cluster
head(teens)
class(teens)
head(teens[, c("cluster", "gender", "age", "friends", "cute")])





aggregate(data=teens, age~cluster, mean) #클러스터별 나이의 평균을 구해라.
aggregate(data=teens, gender=='F' ~ cluster, mean)
aggregate(data=teens, softball + volleyball + hair + dress ~ gender == 'F', mean)

# 기준 : 외모지향, 운동성향, 범죄성향, 무기력, 브레인
#           1          2         3        4       5

# 5가지의 정보를 구해서 데이터의 특성을 파악해보시오.
aggregate(data=teens, age~cluster, mean)

str(teens)
#외모지향 clothes + hollister + abercrombie + shoppiing + mall + blonde + hair

aggregate(data = teens, sex + die + drunk ~ bible > 0, mean)


aggregate(data = teens, sex ~ hot , mean )
aggregate(data = teens, baseball ~ friends > 0, mean)
aggregate(data = teens, death ~ friends > 0, mean)
aggregate(data = teens, church+bible+jesus ~band, mean)
aggregate(data = teens, drugs~sports, mean)



#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
library(tidyverse)
library(cluster) #clustering 알고리즘
library(factoextra) #알고리즘 + 시각화
df<- USArrests
df<- na.omit(df)
df<- scale(df)
head(df)
distance<- get_dist(df) #거리값함수
#시각화
#gradient 점진적으로 변화해간다.
fviz_dist(distance, gradient=list(low="#00AFBB", mid="white", high="#FC4E07"))
k2<- kmeans(df, centers=2, nstart=25) #25번에 걸쳐 모델을 만들어 보시오. 25번에 걸쳐서 모델 만드는데, 좋은 건지 아닌지 무엇으로 판단하나?
                                      # 비교해서 좋은 것을 골라야 할텐데. 무엇을 골라야 하는가?
                                      # 그룹내 응집도 최대화, 그룹간 분리도 최대화. 를 기준으로 한다 !
str(k2)
names(k2)
k2
#dplyr에서 새로운 데이터 포맷, data.frame, matrix => tibble
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster, #mutate 데이터수정 transform
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color= factor(cluster), label=state))+
  geom_text()

k3 <- kmeans(df, centers=3, nstart=25)
k4 <- kmeans(df, centers=4, nstart=25)
k5 <- kmeans(df, centers=5, nstart=25)

p1 <- fviz_cluster(k2, geom="point", data=df) +ggtitle("k=2")
p2 <- fviz_cluster(k3, geom="point", data=df) +ggtitle("k=3")
p3 <- fviz_cluster(k4, geom="point", data=df) +ggtitle("k=4")
p4 <- fviz_cluster(k5, geom="point", data=df) +ggtitle("k=5")
#ggplot2의 그라프를 묶어주는 역할을 합니다. 하부에 ggplot가 있다.

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow= 2)
#convexhull
#sum of square : elbow기법
set.seed(123)
wss <- function(k) {
  kmeans(df, k, nstart = 10) $tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)  #map함수 ! 원래는 apply함수를 썼는데, 이제 map함수가 자바스크립트 등 모두에 들어와 있다.
plot(k.values, wss_values,
     type="b", pch=19, frame=FALSE,
     xlab="clusters K",
     ylab="sum of squares")

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")




###
#silhoutte : kmeans의 평가 지표 1에 가까우면
#군집내 응집도는 최대, 군집간 분리도 최대.

avg_sil <- function(k) {
  km.res<- kmeans(df, centers=k, nstart=25)
  ss<- silhouette(km.res$cluster, dist(df))
  mean(ss[,3])
}

k.values <- 2.15
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type="b", pch=19, frame=FALSE,
     xlab="Number of clusters K",
     yalab="Average Silhouetters")
fviz_nbclust(df, kmeans, method="silhoutte")



## DTW 알고리즘을 이용한 clustering
# DTW (Dynamic time wraping) #시간차 발행하는 주파수를 비교하는 알고리즘
# DTWCluster
# clustering 활용
#install.packages("dtwclust")
library(dtwclust)
data(uciCT)
str(CharTraj)
#선형보간법 : (아닌것은 B-Spline보간법, 등)
# 보간법이란, 중간에 값을 모를때 ,         시간을 일정하게 하기 위해서
series <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))
#NaN을 고려한 score : NaN값 zero 값으로
series <- zscore(series)
#ts "time series" : 파형. 클러스트링 =>
#pam알고리즘을 사용하고 있음.
pc.dtwlb <- tsclust( series, k = 20L,
                     distance = "dtw_lb", centroid = "pam", #pamk는 언제쓴다고 하였죠? k값을 결정하지 않을 때 사용한다고 하였죠?
                     seed=3247, trace=TRUE,
                     control=partitional_control(pam.precompute=FALSE),
                     args = tsclust_args(dist = list(window.size = 20L)))
plot(pc.dtwlb)






#문제 : 중1학년 신체검사 결과 군집분석을 하시오
# k값을 구하는 알고리즘을 다양하게 구현하여 비교해보시오
# 구해진 군집을 해석해보시오.

#install.packages("factoextra")
library(factoextra)
body <- read.csv("bodycheck.csv", header=TRUE)
class(body)
df <- body[,-1]
df <- na.omit(df)
colnames(df) <- c("handpw", "height", "weight", "glass")
df <- scale(df)
class(df)
df <- as.data.frame(df)
head(df)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low="#00AFBB", mid="white", high="#FC4E07"))
k2 <- kmeans(df, centers=2 , nstart=25)
fviz_cluster(k2, data=df)
cor(df, method="pearson")
library(corrgram)
corrgram(df, upper.panel=panel.conf)
attributes(df)
dim(df)
df<- as.data.frame(df)
colnames(body)<- c("handpw", "height", "weight", "glass")
df$cluster <- as.factor(k2$cluster)
plot(df$handpw, df$weight, col=df$cluster)
points(k2$centers[,c("handpw", "weight")], col=c(1,2), pch=8, cex=5)
plot(df$handpw, df$glass, col=df$cluster)
points(k2$centers[,c("handpw", "glass")], col=c(1,2), pch=8, cex=5)

k2$centers # "악력"  "신장"  "체중"  "안경"
#1그룹 :       큼      큼     큼       o
#2그룹 :       작      작     작       x




## seeds_dataset
#            V1    V2    V3    V4    V5     V6      V7       V8
#종자분류 : 면적, 둘레, 밀도, 길이, 가로, 대칭성, 홈길이, 분류번호
# c("area", "perimeter", "compactness", "length", "width", "asymmetery", "groovelength", "num" )
# clustering 하시오
# Nbclust로 클러스터링 갯수를 확인하시오
# hclust k 값을 결정
# 질문
#  클러스터 별 area의 평균?
#  클러스터 별 대칭성 정도?
#  군집분포 형태를 그리시오.

##
library(fpc)
library(factoextra)
seed_origin = read.table("seeds_dataset.txt")
str(seed_origin)
head(seed_origin)
colnames(seed_origin) = c("area", "perimeter", "compactness", "length", "width", "asymmetery", "groovelength")
seed = seed_origin[,1:7]
seed_sc <- scale(seed)
#hierachical cluster
seed.dist=dist(seed_sc)
seed.hclust=hclust(seed.dist, method="ward.D2")
plot(seed.hclust)
rect.hclust(seed.hclust, k=3)

seed.3clust = cutree(seed.hclust, k=3)
seed[seed.3clust==3,]
length(seed.3clust)
length(seed_origin[8])
table(seed.3clust, seed_origin[,8])
plotcluster(seed, seed.3clust)

library(NbClust)
nc <- NbClust(seed_sc, min.nc=2, max.nc=9, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Clusters의 수", ylab="Criterroia의 수",
        main="클러스터 갯수 결정 그래프")


wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed_sc, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="그룹수", ylab="그룹내 sum of square의 합")

fit<- kmeans(seed_sc, 3)
table(fit$cluster)
seed$cluster<- fit$cluster
p1 <- fviz_cluster(fit, geon="point", dat=seed) + ggtitle("종자분류 k=3")
print(p1)
str(seed)
aggregate(data=seed, area~cluster, mean)
aggregate(data=seed, asymmetery ~cluster, mean)
fit$centers

#Parallel Coordinates Plot : 변수간에 관계성( 혼잡도 판단)

#:: 를 사용하는 이유. 필요한 것만 로딩, 패키지 이름이 충동할 때 사용.
MASS:: parcoord(seed, col=seed$cluster, var.label=TRUE, lwd=2)
#어디서 데이터 혼동/겹치는지 보여주는 그래프
