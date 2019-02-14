library(dplyr)
weather = read.csv("weather.csv")
length(weather)
nrow(weather)
str(weather)

sum(is.na(weather))
weather<- na.omit(weather) #결측치 처리
sum(is.na(weather))
head(weather)
summary(weather)

weather$key=1:nrow(weather)
weather$key

outdata=select_if(weather, is.numeric)
outdata

str(weather)
str(outdata)

for(i in 1:(ncol(outdata)-1)){ #이상치제거
  uppercut=fivenum(outdata[,i])[4]+1.5*IQR(outdata[,i]) #[4] 에 3위수
  lowercut=fivenum(outdata[,i])[2]-1.5*IQR(outdata[,i])
  out<-filter(outdata, outdata[,i]<=uppercut, outdata[,i]>=lowercut)
}

str(out)
res_cor<-cor(out) #상관계수
res_eig<- eigen(res_cor) #고유값 분해 : 고유치와 고유벡터(직교벡터) -> 전치행렬이 역행렬이다.
res_eig
res_eig_t = t(res_eig$vectors) #전치시킴
class(res_eig$vectors)
dim(res_eig$vectors)
class(res_eig_t)
dim(res_eig_t)

res_eig$vectors %*% res_eig_t #전치행렬과 원래 직교 행렬 내적을 내었더니,
#대각선이 1이 나옴. 나머지는 거의 다 0에 가까움.  == 단위행렬로 이해할 수 있다.





#의사결정나무 기본적으로 함... 시각적으로 볼 수 있어.


##    PCA    ##

a<- c(4.0, 4.2, 3.9, 4.3, 4.1)
b<- c(2.0, 2.1, 2.0, 2.1, 2.2)
c<- c(0.60, 0.59, 0.58, 0.62, 0.63)

(mat <- matrix(c(a,b,c), nrow=5, byrow=F))
mat.pca<- princomp(mat, cor=T, scores=T) # principal components analysis (princomp) 주성분분석 함수
# 상관계수 -> 고유값분해 -> 고유값 + 고유벡터

names(mat.pca)        # cor=T 상관계수를 사용하고, scores=T 표준화하라.
mat.pca  # 제 1축이 분산이 가장 크다.

# 주성분분석 => 상관계수 구하고, 고유값분해 . 그 결과값이 mtc.pca에 들어와있다.

summary(mat.pca) # 각 축의 분산비율을 확인.
eig.val=mat.pca$sdev^2 #표준편차를 제곱하면 분산이 된다 !
eig.val
par(mfrow=c(1,2))
screeplot(mat.pca)
screeplot(mat.pca, type='line', pch=19, main='Scree Plot') # 85%
#나중에 눈으로 보면 몇번째 주성분까지 하면 좋겠다고, 눈으로 확인이 돼 !
mat.pca$loadings[,1:2] #각축에 각 변수가 어떻게 영향을 미치고 있나
(mat.pca$scores[,1:2]) #각행의 값이 각 주성분축에 어떤 영향을 미치고 있나.


##
(x<-1:10)
(y<-x+runif(10, min=-.5, max=.5))
(z<-x+y+runif(10, min=-10, max=10))
data<- data.frame(x,y,z)
pr<- princomp(data)
summary(pr)
screeplot(pr,type=c("barplot", "lines"), max(10, length(pr)))
fit<- princomp(data, cor=TRUE)
summary(fit)
loadings(fit)
fit$loadings[,1:2]
(fit$scores[,1:2])  #scores 는 아래 col.ind를 의미
plot(fit,type="lines")

#install.packages("factoextra")
library(factoextra)
fviz_eig(mat.pca)

#repel / 중복을 배제시켜라. overlapping 방지 - 지터

fviz_pca_biplot(fit, repel=TRUE, #개별데이터의 위치와 열변수의 방향이 같이 나타나 있습니다. 흩어져있는것은 각데이터(x,y,z에 들어있는 개별요소들의 위치)
                col.var="#2E9FDF", #열 변수
                col.ind="#696969") #행 변수

#제1주성분 입장에서 보았을때, 모두 비슷함. 제2주성분 입장에서 봅았을때는 z이 가장 위아래로 차이가 많이 나서, 영향을 많이 미친다고 보임.
#세개를 합쳐놓았을때 , 제 1주성분은 x,y,z비슷하니까.. 비슷한것일거고, 제 2주성분은 z가 가장 차이많이 나서..
#각 성분에 대해서 파악할 수도 있음. 1, 2, 3, ...



fviz_pca_ind(mat.pca,
             col.ind="cos2",
             gradient.cols=c("#00AFBB", "#E7B800","#FC4E07"),
             repel = TRUE
)
fviz_pca_var(mat.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE
)

# prcomp : SVD(singular value decomposition) : 특이행렬분해 (일반적인 경우)
#비정방행렬 => 고유값 분해는 특이행렬분해의 특수한 경우다.



#문제 : wine 데이터에 대하여 고유값 분해를 실시하시오

wine = read.csv("whitewines.csv")
install.packages("ggbiplot")

library(ggbiplot)
data(wine)
str(wine)

wine.pca <- prcomp(wine, scale. = TRUE)
screeplot(wine.pca, type='line', pch=19, main='wine')
ggscreeplot(wine.pca)
fviz_eig(wine.pca)
fviz_pca_biplot(wine.pca, repel=TRUE,
                col.var="#FF0000",
                col.ind="#00FF00"
)

## iris에 대한 주성분 분석

pcaCharts<- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("분산점유율:")
  print(x.pvar)

  par(mfrow=c(2,2))
  plot(x.pvar, xlab="주성분", ylab="분산점유율", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar), xlab="주성분", ylab="누적분산점유율", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x, type="l")
  par(mfrow=c(1,1))
}

head(iris)
str(iris)
iris.cntn <- iris[,-5]
iris.cat <- iris[,5]
# 5번을 뺀 이유. 주성분분석은 비지도학습이기 때문에, 종속변수를 제거해준다. 이미 알고 있으니까.

#install.packages("caret")
library(caret) #자동화 패키지, 노가다를 줄여주는 패키지
#machine learning : parameter 조절
iris.trans <- preProcess(x = iris.cntn, method=c("center", "scale")) #preProcess는 전치리를 줄여줍니다.
iris.trans
iris.preproc <- predict(iris.trans, newdata=iris.cntn) #predict는 예측하라는 소리가 아니라, preProcess 모델에 적용해서, iris.cntn을 iris.preproc으로 바꿔주라.
iris.pca <- prcomp(iris.preproc, center=FALSE)
iris.pca
summary(iris.pca)
pcaCharts(iris.pca)
biplot(iris.pca)


##
cereal <- read.csv('./serials.csv',sep="\t", header=T)
if (!require(MASS) ){
  install.packages("MASS")
  require(MASS) }
if (!require(knitr) ){
  install.packages("knitr")
  require(knitr) }
str(cereal)

names(cereal)[1] <- c("name")

#시리얼 종류별 구성요소, 변수가 구성요소 ()
cereal <- cereal[1:16]
cereal <- na.omit(cereal)
kable(summary(cereal), caption="", align="r")


cereal

normalize <- function(x) {
  return ((x - min(x)) / (max(x) -min(x))) }
cereal.normal <- as.data.frame(lapply(cereal[,4:16], normalize))
sapply(cereal.normal, sd)
cereal.scale <- as.data.frame(lapply(cereal[,4:16], scale))

par(mfrow=c(4,4))
for(i in 4:16){
  hist(cereal[,i], main = paste(i, '-', colnames(cereal)[i], sep='', 0 ) )
}


par(mfrow=c(4,4))
for (i in 4:16){
  plot(cereal[,i], main=paste(i, '-', colnames(cereal)[i], sep=''))
}
pairs(cereal[,4:16])
row.names(cereal) <- paste(round(cereal$rating), '-', cereal$name, sep = '')
cereal.pca <- prcomp(~rating+calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins+shelf+weight+cups,
                     data=cereal, scale= TRUE)
plot(cereal.pca, type= "l")
summary(cereal.pca)
biplot(cereal.pca, col=c("gray50", "black"))


dim(cereal)

#주성분분석을 하고 주성분분석, 다른분석하기 전
#변수 많으면 복잡, 시간도 많이 걸림 => 주성분분석 :: 다음 모델로 사용할 것:: 주성분분석에서 나온 변수(변수이름 모름, 제1주성분/2주성분 이런식으로 나옴.):: biplot으로 알 수 있음. 결정.
#


# 새로운 주성분 축의 의미를 설명하고 그 취지에 부합하는 제품을 선택
# 문제 : 제 1주성분하고 제 2주성분을 설명하고 그 의미에 부합하는 제품을 선택해보시오.





# secu_com_finanace_2007.csv 데이터를 이용하여 주성분 분석을 하고
#제1, 제2 주성분에 대한 설명을 하시오

#v1 : 총자본순이익율
#v2 : 자기자본순이익율
#v3 : 자기자본비율
#v4 : 부채비율
#v5 : 자기자본회전율

finance <- read.csv("secu_com_finance_2007.csv")
str(finance)
class(finance)
finance <- transform(finance,
                     V1_s = scale(V1),
                     V2_s = scale(V2),
                     V3_s = scale(V3),
                     V4_s = scale(V4),
                     V5_s = scale(V5))
finance <- transform(finance,V4_s2 = max(V4_s) - V4_s)

finance_2 <- finance[,c("company", "V1_s", "V2_s", "V3_s", "V4_s2", "V5_s")]
cor(finance_2[,-1])
round(cor(finance_2[,-1]), digits=3)
plot(finance_2[,-1])




## 문제 secu_com_finance_2007.csv 데이터를 이용하여 주성분 분석을 하고
## 제1, 제2 주성분에 대한 설명을 하시오

# v1 : 총자본순이익율
# v2 : 자기자본순이익률
# v3 : 자기자본비율
# v4 : 부채비율
# v5 : 자기자본회전율

secu<-read.csv('secu_com_finance_2007.csv', sep=",", header=T)
head(secu)
str(secu)
class(secu)

head(secu)
secu.cntn<-secu[,-1]
secu.cat<-secu[,1]

library(caret)
secu.trans<-preProcess(x=secu.cntn, method=c("center","scale"))
secu.preproc<-predict(secu.trans, newdata=secu.cntn)
secu.pca<-prcomp(secu.preproc, center=FALSE)
secu.pca
summary(secu.pca)
secu.pca[,c(1:5)]


pcaCharts(secu.pca)
biplot(secu.pca)

secu_pc1 <- predict(secu.pca)[,1]
secu_pc2 <- predict(secu.pca)[,2]


text(secu_pc1, secu_pc2, labels = secu$company, cex = 0.7, pos = 3, col = "blue")

## pc1에 영향을 미치는 변수(v3), 자기자본비율이 주성분1에 가장 큰 영향을 준다.
## pc2에 영향을 미치는 변수(v1), 총자본순이익율이 주성분2에 가장 큰 영향을 준다.
## pc1의 측면에서 부국증권, 브릿지 증권을 선택하는 것이 좋고 pc2의 측면에서 미래에셋증권을 선택하는 것이 좋다.
