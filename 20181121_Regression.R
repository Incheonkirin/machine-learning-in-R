#선형회귀 :
# 오차는 독립성, 등분산성, 정규성을 가정하나 실제는 계측이 불가함으로
# 잔차 분석을 통해 확인
# 다중 공선성
#  변수간의 높은 상관관계를 의미
#  다중 공선성이 높으면 회귀 계수의 추정치의 분산이 커져 추정치의 부호까지 바뀌는 문제가 발생

# 회귀모델의 평가
#  모델이 통계적으로 유의미한가 : F분포에 해당하는 p-value
#  계수의 유의미 정도 : 계수의 t-value와 p-value 확인
#  모형이 설명력이 있는가 : R-squared, Adjusted R-squared
#  모델이 데이터를 잘 fitting 하고 있는가 : 잔차 그래프로 확인
#  데이터에 적용된 선형회귀가 적절한가 : 상관분석

#종속변수
y = c(1, 5, 3, 8, 5, 3, 10, 7)
#독립변수
x1 = c(2, 4, 5, 6, 8, 10, 11, 13)
x2 = c(1, 2, 2, 4, 4, 4, 6, 6)
opar = par(mfrow=c(1,3))

plot(x1, y)
plot(x2, y)
plot(x1, x2)
#서로간에 별다른 영향력이 없어 보임.

summary(lm(y~x1))
#Residuals
#Min 1Q Median 3Q max
#Coefficients
#설명력이 없다는 결론.


summary(lm(y~x2))
#Residuals
#Min 1Q Median 3Q max
#Coefficients
#설명력이 없다는 결론.


summary(lm(y~x1+x2))
#Residuals
#Min 1Q Median 3Q max
#Coefficients
#유의미 & 설명력이 있다.
#Intercept 가 1.0355 y절편.
#1.2223 x절편

# 이게 회귀분석이다.
#1.0355 - 1.2223*x1 + 3.6493 * x2
1.0355 - 1.2223*5 + 3.6493 * 2
1.0355 - 1.2223*13 + 3.6493 * 6


yx1 = lm(y~x1)
yx2 <- lm(y~x2)
yx1x2 <- lm(y~x1+x2)
plot(yx1)
plot(yx2) #설명력 낮기 때문에 잔차가 크게 나타나고 있다.
plot(yx1x2)
par(opar)
cor(y, x1)
#이상치 찾는것 2가지. 레버리지-독립변수가 영향. 종속변수는 y.


#####
str(women)
fit=lm(weight~height, data=women)
summary(fit)
women.fitted_weight = -87.52 + 3.45*women$height
plot(weight~height, data=women)
abline(fit, col="blue")

#cor = 상관계수, cov = 공분산
cor(women$weight, women$height) #회귀분석은 상관분석에 의해 정당성 부여
cor(women$weight, women$height)


#문제
#1) cars 데이터를 선형회귀 해보시오
# 종속변수는 dist 로 합니다.

#정답
opar=par(mfrow=c(1,1))
data(cars)
head(cars)
summary(cars)
plot(cars$speed, cars$dist)
res.lm<- lm(dist~speed, data=cars)
#65% 설명력
summary(res.lm)

summary(cars$speed)
cof=coef(res.lm) #cofficient 계수
class(cof)
cof["speed"]
cof["(Intercept)"]
plot(cars$speed, cars$dist, xlab="속도", ylab="정지거리", xlib=c(0, 30), ylim=c(0, 125))
abline(coef(res.lm), col=2)
fitted(res.lm)[1:4] #실제 데이터를 가지고 예측
residuals(res.lm)[1:4] #잔차확인(예측값-관측값)
deviance(res.lm)
plot(res.lm, which=1)

library(lmtest)
dwtest(res.lm) #자기상관성 0-2:양적 자기상관성이 있고
               #           2-5:부적 자기상관성이 있다.
               #           2  :자기 상관성이 없다.


predict(res.lm, newdata=data.frame(speed=10)) #점추정
predict(res.lm, newdata=data.frame(speed=10), interval="confidence") #신뢰구간예측
predict(res.lm, newdata=data.frame(speed=10), interval="prediction") #실제상황의 random값까지 고려

predict(res.lm, newdata=data.frame(speed=seq(4.0, 25.0, .21)), interval="confidence")
(speed<-seq(min(cars$speed), max(cars$speed), .1))
(ys<-predict(res.lm, newdata=data.frame(speed=speed), interval="confidence"))

par(mfrow=c(2,2))
plot(res.lm)



#집값에 영향을 미치는 변수들과 집값예측
library(MASS)
data("Boston")
help("Boston")
str(Boston)
head(Boston, 3)
boston_df <- as.data.frame(scale(Boston)) #scale을 쓴 이유는, 표준화를 하기 위함.
head(boston_df, 3)
set.seed(123)
#모든 데이터의 참여 => 평가를 불가능.    평가 7:3
idx<- sample(1:nrow(boston_df), 300) #샘플 분리 이유 : 모델 생성에 참여하지 않은 데이터로 테스트
trainDF <- boston_df[idx,]
testDF <- boston_df[-idx,] #그 인덱스를 제외한 것은
dim(trainDF); dim(testDF)
form<- medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black+lstat
form2<- medv ~ . # 점을 찍으면 전부 다 를 의미한다.
#다항회귀

lm_model <- lm(formula = form, data = trainDF)
lm_model
names(lm_model)

#모델 참여 데이터에 대한 결과
lm_model$fitted.values[1:5] #모델을 평가하면서 미리 참여 데이터에 대한 피팅값 보유
lm_model$residuals[1:5]
trainDF$medv[1:5] #실제 데이터
summary(lm_model)
# 모델 유의성 검정:
# 모델 설명력:
# x변수의 유의성 검정:
## 회귀모델의 성능 평가

# 열은 같음. 열이 같아야 데이터를 집어넣을 수 있다.
pred<- predict(lm_model, testDF) #테스트 데이터로 모델 평가
# 결과값들이 부동소수점: 직접비교가 불가능.
# 이산적이면 직접비교가 가능.
# => cor를 사용
cor(pred, testDF$medv) #상관계수로 평가하는 이유: 연속적이기 때문에 카운트 안됨.
#하나의 데이터가 올라갈때 그 데이터가 맞느냐 하는것.




## 자기상관성은 이전 데이터가 그 다음데이터에 영향을 미치는 것.
## 다중 공선성은  변수간에 영향력이 있느냐.

## 선형회귀분석의 잔차검정과 모형진단
# 잔차 독립성 검정: Durbin-Watson검정 : 통상 1~3 정도의 값을 가짐.
library(lmtest)
dwtest(lm_model)

#다중 공선성 검정
#install.packages("car")
library(car)
sqrt(vif(lm_model)) > 2 #2보다 크면 다중 공선성이 있다.

cor(trainDF[c('nox', 'dis', 'rad', 'tax')])
#다중공선성이 있으면 있는 변수간의 상관성을 확인하여 해당 변수 제거
#nox와 tax를 제거
form <- medv ~ crim+zn+indus+rm+age+dis+rad+ptratio+black+lstat

lm_model <- lm(formula=form, data=trainDF)
sqrt(vif(lm_model))>2

pred <- predict(lm_model, testDF)
cor(pred, testDF$medv)





# 문제
# 1) mtcars에 대하여 다항 회귀 분석하시오
# 연비를 종속변수로 한다

str(mtcars)
head(mtcars, 3)
mtcars_df <- as.data.frame(scale(mtcars))
head(mtcars_df, 3)
set.seed(123)
idx<- sample(1:nrow(mtcars_df), 20)
trainDF <- mtcars_df[idx,]
testDF <- mtcars_df[-idx,]
dim(trainDF); dim(testDF)
form2<- mpg ~ . # 점을 찍으면 전부 다 를 의미한다.
#다항회귀
lm_model <- lm(formula = form2, data = trainDF)
lm_model
names(lm_model)
lm_model$fitted.values[1:5]
lm_model$residuals[1:5]
trainDF$mpg[1:5]
summary(lm_model)
pred<- predict(lm_model, testDF) #테스트 데이터로 모델 평가
cor(pred, testDF$mpg) #상관계수로 평가하는 이유: 연속적이기 때문에 카운트 안됨.

#정답
str(mtcars)
head(mtcars)
mydata  <- mtcars
sum(is.na(mydata))
str(mydata)
mycor <- round(cor(mydata), 2) #상관계수확인
mycor
#하나가 높게 나옴. 상관테스트를 해본다. 유의미하게 나온다. 귀무가설 기각하고 대립가설채택한다.
cor.test(mtcars$mpg, mtcars$cyl)
cor.test(mtcars$mpg, mtcars$disp)
cor.test(mtcars$mpg, mtcars$hp)
cor.test(mtcars$mpg, mtcars$drat)
cor.test(mtcars$mpg, mtcars$wt)
fit <- lm(mpg ~., data=mydata)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
step(fit) #변수 선택법 (전진선택법(하나씩 추가하면서),후진선택법(하나씩 제거하면서)))

#후진선택법

#AIC 복잡도 => 낮은 값이 좋음.
#step AIC 61.31
#mpg ~ wt + qsec+ am
#이 것을 쓰면 잘 예측해준다고 알려줌

fit2 <- lm(mpg~wt+qsec+am, data=mydata)
summary(fit2)
#변수 아래는 3개 위에는 전부 다 들어간 것임. 데이터 량이 많아지면 아래 것을 사용할 것이다.




#문제
# insurance 데이터를 로딩한 다음 charges 의료비를 종속변수로 하는 회귀분석을 실시하시오
# 분석에 불필요한 데이터 한개는 제거하시오
# 7:3으로 sampling하여 테스트 하시오

data <- read.csv("insurance.csv", header=T, encoding="UTF-8")
str(data)
head(data)
sum(is.na(data))

cor.test(data$charges, data$age)
#cor.test(data$charges, data$sex)
cor.test(data$charges, data$bmi)
cor.test(data$charges, data$children)
#cor.test(data$charges, data$smoker)
#cor.test(data$charges, data$region)

fit <- lm(charges ~., data=data)

summary(fit)
par(mfrow=c(2,2))
plot(fit)
step(fit)

fit2 <- lm(charges ~ age + bmi + children + smoker + region, data=data)
summary(fit2)


#정답
insurance <- read.csv("insurance.csv", header=T, encoding="UTF-8")
str(insurance)

dim(insurance)
insurance<- insurance[-6]
head(insurance)
summary(insurance$charges)

cor(insurance[c('age','bmi','children','charges')])
#install.packages("psych")
library(psych)
pairs.panels(insurance[c('age','bmi','children','charges')]) #한쪽은 수치로 한쪽은 그라프로 보여줍니다.

#여기까지는 데이터 탐색


#데이터를 확인했으니, 나누어 줍니다.
#분할

#데이터 셋 생성 : 모델 데이터와 테스트 데이터 분류
set.seed(123) #랜덤수의 시작위치를 가르쳐 주기때문에, 같은 값을 보여줌. 의사랜덤.
idx = sample(1:nrow(insurance), 0.7*nrow(insurance))
training_ins = insurance[idx, ]
testing_ins=insurance[-idx, ]
dim(training_ins)
dim(testing_ins)


#모델링을 해봅니다.
model_ins<- lm(charges ~age + children + bmi + sex + smoker, data = training_ins)
model_ins<- lm(charges ~., data=training_ins)#실제 현업에서는 데이터 변수를 넣고 빼고 하기 때문에 위에와 같이 사용합니다.
model_ins

#직접 볼 수 가 없어요. summary로 봐야 합니다.
names(model_ins)
model_ins$fitted.values[1:5]
model_ins$residuals[1:5]
training_ins$charges[1:5]
summary(model_ins)


#평가를 해봅니다.
pred <- predict(model_ins, testing_ins) #뭐와 뭐를 비교? 예측해서 나온 값과, 실제값(charge값) 비교.
pred

#그런데 charge값이 부동소수점. 비교할 수 없습니다. 그러면 상관계수를 이용해서 비교합니다.
cor(pred, testing_ins$charges)

sqrt(vif(model_ins)) >2

step(model_ins, direction="both") #both는 전진선택법 후진선택법 모두 사용하라는 의미
#중요한 판단기준이 되는 것 'AIC (Akaike Information Criterion)'/ 'BIC(Bayesian Information Criterion)'
# AIC, BIC 자주 나옵니다. 직접 계산하는 것보다, 뜻만 알고 있으면 됩니다.
# AIC는 모형의 적합도와 복잡도를 동시에 나타내는 지수로 작으면 좋다.
# Call은 추천하는 것. '맨 마지막것이 추천'
model_ins <- lm(charges ~ age + bmi + children + smoker, data=training_ins)
model_ins
pred <- predict(model_ins, testing_ins)
pred
cor(pred, testing_ins$charges)
#변수가 하나 (sex) 적어졌는데 확률이 같음. 아래것을 사용할 것입니다.

#파생변수를 추가한 모델  I :; Inhibit:금지
model_ins2 <- lm(charges ~ age + I(age**2) + children + bmi + smoker, data=training_ins)
model_ins2
summary(model_ins2)
#age를 제곱한 수를 파생변수를 추가해라/ 파생변수 쓸 때도 있습니다.



## 상관분석
cor.test(c(1, 2, 3, 4, 5), c(1, 0, 3, 4, 5), method="pearson") #연속적수치 #p-value가 0.02 == 상당히 관계가 있다
cor.test(c(1, 2, 3, 4, 5), c(1, 0, 3, 4, 5), method="spearman") #순위 #rho #p-value 0.08 관계없음
cor.test(c(1, 2, 3, 4, 5), c(1, 0, 3, 4, 5), method="kendall") #순서쌍 #tau #p-value 0.08 관계없음
####

# 주성분 분석의 원리를 이해해봅시다.

# vector가 3개 있습니다.
a<- c(4.0, 4.2, 3.9, 4.3, 4.1)
b<- c(2.0, 2.1, 2.0, 2.1, 2.2)
c<- c(0.60, 0.59, 0.58, 0.62, 0.63)
(mat<- matrix(c(a,b,c), nrow=5, byrow=F ))

# x, y 변수
# 공분산 구하는 수식= (xi-x평균)(yi-y평균) + (xi-x평균)(yi-y평균) + (xi-x평균)(yi-y평균) ... / (n-1)
# 공분산 / ( x의 표준편차 * y의 표준편차 ) ==> 상관계수

(avr<- colMeans(mat)) #변수마다 평균을 냄.
(acha<- a - avr[1]) #첫번째 열 - 첫번째 열에 대한 평균
(bcha<- b - avr[2]) #두번째 열 - 두번째 열에 대한 평균
(ccha<- c - avr[3]) #세번째 열 - 세번째 열에 대한 평균

(aa_var <- sum(acha*acha) / (length(a)-1)) #벡터의 사칙연산은 요소별로..
(ab_var <- sum(acha*bcha) / (length(a)-1))
(ac_var <- sum(acha*bcha) / (length(a)-1))

#공분산을 내는 함수 = cov()

cov(mat) #열변수 행변수를 같이 놓고 상호 상관도를 구해준다. # 정방행렬/ 대칭행렬

(aa_cor<- aa_var/(sd(a)*sd(a)))
(ab_cor<- ab_var)/(sd(a)*sd(b))
#상관계수가 구해진다.

cor(mat)
#공분산과 상관계수를 미리 만들어줌.
#공분산과 상관계수를 구하는 이유 ? 변수 상호간의 관계성을 알기 위해서 !!
#  회귀분석-독립변수상호간에 관련성 높으면 => '다중 공선성' 이라고 함.
#  '다중공선성' 이 있으면 ==> 중복변수를 제거

### 고유값분해 ==> 고유값과 고유벡터(상호직교) ### 그러면서 새로운 축이 만들어진다
# 고유값분해를 하는 이유?? 계산을 간단히 하기 위해서 !!
#  정방행렬 대칭행렬을 고유값분해를 하면, 상호직교를 한다.
#  상호직교를 하면 무슨 특성이 있다? 변수 상호간에 독립적이다 는 뜻이다.

(eigvector<-eigen(cor(mat)))#변수가 3개 => 벡터도 3개가 출력
#eigen 하면, 고유값과 고유벡터로 이루어진 값을 리턴함.
#상호직교하는가를 확인
#두개의 벡터씩 내적을 내 봅니다. 내적기호 %*%
eigvector$vectors[,1] %*% eigvector$vectors[,2] #0
eigvector$vectors[,2] %*% eigvector$vectors[,3] #0
eigvector$vectors[,1] %*% eigvector$vectors[,3] #0
#3번째 건 명확히 0이 나옴. 1번째/2번째건 0에 가까움.
#내적이 0이다 => 직교한다는
#직교하는 3개의 축과
#정직교하는 축이 나온다.



#(eigvector<-eigen(cor(mat)))에서 [1] 2.2592714 0.5202666 0.2204620 는 대각을 의미
# eigvector$vectors[,1] %*% eigvector$vectors[,2] #0  이 3개는 방향값. 정확히 x,y,z 축이 맞다.
# eigvector$vectors[,2] %*% eigvector$vectors[,3] #0
# eigvector$vectors[,1] %*% eigvector$vectors[,3] #0


#                                           ======>> 새로운 새 축에 맞춰서 '재해석' 해야 함.
#a<- c(4.0, 4.2, 3.9, 4.3, 4.1)              이건 정확히 x,y,z 축은 아니다. a가 각 축에 어떤 영향을 미쳤는지 알면 된다 !
#b<- c(2.0, 2.1, 2.0, 2.1, 2.2)              =========================================================================
#c<- c(0.60, 0.59, 0.58, 0.62, 0.63)          딥러닝 할때 이런 개념 알아야 함. 모든 개념은 특징 추출해야하고,
#                                                     1만차원도 2차원으로 차원 축소할 수 있어야 함.
#                                                          주성분 분석을 해야함.
#                                              "기여도를 따져서 새 좌표점에 새로 찍는다."




###
library(Hmisc)
rcorr(as.matrix(mtcars), type="pearson")
rcorr(as.matrix(mtcars))

symnum(cor(iris[, 1:4]))
###
d<-data.frame(x1=rnorm(10),
              x2=rnorm(10),
              x3=rnorm(10))
M<-cor(d)
#install.packages("corrplot")
library(corrplot)
corrplot(M, method="circle")

###
set.seed(1)
x<- matrix(rnorm(20), nrow=5, ncol=4)
y<- matrix(rnorm(15), nrow=5, ncol=3)
COR<- cor(x,y)
COR
image(x=seq(dim(x)[2]), y=seq(dim(y)[2]), z=COR, xlab="x column", ylab="y column")
text(expand.grid(x=seq(dim(x)[2]), y=seq(dim(y)[2])), labels=round(c(COR),2))

###
cor_4 <- cor(mtcars[1:6])
symnum(cor_4, abbr.colnames= FALSE)
corrplot(cor_4, method="circle")
M<- cor(mtcars)
head(round(M,2))
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot(M, method = "pie")
corrplot(M, method = "color")
corrplot(M, method = "number")
corrplot(M, type = "upper")
corrplot(M, type = "lower")
corrplot(M, order = "hclust") #hierachical clustering
corrplot(M, type = "upper", order = "hclust")
library(RColorBrewer) #컬러 팔레
corrplot(M, type="upper", order="hclust", col=brewer.pal(n=9, name="PuOr"), bg="darkgreen")
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data<-mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
col<-colorRamPalette(c("darkblue", "white", "darkorange"))(20)
M<-cor(mtcars[1:7])
heatmap(x=M, col=col, symm=TRUE) #전체 분포 보고 싶을 때 heatmap, 데이터 분포 datachart.

#문제 airquality에 대한 상관계수를 구해보시오

# 1) 숫자 데이터에 대해서만 처리합니다. (달하고 일은 제외합니다.(select_if))
# 2) na는 제거합니다.
# 3) 데이터의 상관정도를 확인합니다.(min, max)
# 4) corrplot을 이용하여 확인합니다.
# 5) 가장 강한 상관관게는 ?
# 6) 가장 약한 상관관계는 ?


#정답
library(dplyr)
(airquality)
str(airquality)
head(airquality)
airquality_1<-
  airquality %>%
  select_if(function(col) is.numeric(col))
(sum(is.na(airquality_1$Ozone)))
cor(airquality_1)
(airquality_2<- na.omit(airquality_1))
(airquality_cor<- cor(airquality_2))
library(corrplot)
plot(airquality_2)
corrplot(airquality_cor, method="shade")
col<-colorRampPalette(c("darkblue", "white", "darkorange"))(20)
heatmap(x=airquality_cor, col=col, symm=TRUE)



#숙제
# mtcars의 상관행렬에 대하여 eigen(고유값 분해를 실시하는 과정을 수식으로 풀어보시오)
# 아이젠 분해 후 생성되는 벡터에 대하여 정직교함을 수식으로 확인하시오.
str(mtcars)
cov(mtcars) #공분산 => 정방행렬/ 대칭행렬
cor(mtcars$mpg, mtcars$cyl, mtcars$disp, mtcars$hp, mtcars$drat, mtcars$wt, mtcars$qsec, mtcars$vs, mtcars$am, mtcars$gear,
    mtcars$crab)

(aa_cor<- aa_var/(sd(a)*sd(a)))
(ab_cor<- ab_var)/(sd(a)*sd(b))
#상관계수가 구해진다.


#고유값 분해#

(eigvector<-eigen(cor(mtcars)))
str(mtcars)

#eigen 하면, 고유값과 고유벡터로 이루어진 값을 리턴함.
#상호직교하는가를 확인
#두개의 벡터씩 내적을 내 봅니다. 내적기호 %*%

eigvector$vectors[,1] %*% eigvector$vectors[,2] #0
eigvector$vectors[,2] %*% eigvector$vectors[,3] #0
eigvector$vectors[,1] %*% eigvector$vectors[,3] #0



# 로지스틱 회귀분석(분류할때 사용한다.)
# 회귀분석 + sigmoid(시그모이드)함수 => 확률분포로 변화.
# 0~1 사이의 값을 출력하는 분포 함수 (0.5를 기준으로 분류)
# odd비 = 우도비 => exp => 승산비
# odd비 = yes/1-yes 이항분포

Dine <- matrix(c(0, 17, 0 , 19, 0, 20, 0, 20.5, 0, 21.5, 0, 22, 1, 24, 1, 1, 25, 0, 25, 0, 27,
                 1, 28, 1, 30, 0, 31.5, 1, 32, 1, 34, 1, 35, 36.5, 1, 39, 1, 40, 1, 44), nc=2, byrow=T)

Dine
colnames(Dine) <- c("Class", "Income")
(Dine<- as.data.frame(Dine))
Logit.model<- glm(Class~Income, family="binomial", data=Dine) #glm일반화된 선형회귀 함수
summary(Logit.model)
Logit.model$coeff[2]#결과값은 odd비로 이해해야
OR<-exp(Logit.model$coeff[2])
OR#승수비



#weather 내일 비가 올 것인가?
weather = read.csv("./weather.csv", header=TRUE, stringsAsFactors = F)
dim(weather)
head(weather)
str(weather)
#빼주어야 할 데이터 = Date, WindGustDir, RainToday필요없음.
weather_df<-weather[,c(-1,-6,-8,-14)]
str(weather_df)
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <-1
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <-0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)
weather_df<-na.omit(weather_df) #결측치 제거
#데이터 샘플링
set.seed(100)
idx<- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train<- weather_df[idx, ]
test<- weather_df[-idx, ]
dim(train)
dim(test)
weather_model <- glm(RainTomorrow ~ ., data = train, family= 'binomial')
weather_model
summary(weather_model)
step(weather_model)

#추천해준 것으로 다시 정의 해보겠습니다.
weather_model2 <- glm(RainTomorrow ~ MinTemp + MaxTemp + Sunshine + WindGustSpeed +
                        WindSpeed + Humidity + Pressure,  data = train, family='binomial')
weather_model2
summary(weather_model2)


#예측해보겠습니다.
pred <- predict(weather_model, newdat=test, type="response")
pred
str(pred)

#선형회귀의 결과는 절편과 기울기
cpred<- ifelse(pred>= 0.5, 1, 0) #sigmoid함수 0~1, 0.5를 기준으로 참과 거짓 분류
table(cpred)
(ret<-table(cpred, test$RainTomorrow)) #예측된 결과와 실제 데이터 비교


diag(ret)
sum(diag(ret))/ sum(ret) #85%   #대각선에 있는 값만 추출
#대각에 있는 값이 중요함 ! 대각에 있는 값이 '해'를 의미함. #벡터의 크기값을 결정하고, 대각행렬이라고 이야기 함.
#대각화 한다는 이야기는 해를 푼다는 의미.
#고유값분해는 대각행렬을 구해주는데, 벡터/직교하는 벡터 까지 같이 구해준다는 의미.

# !!!! 직교행렬은 전치행렬이 역행렬이다 !!!! ** 중요함 ** 행렬 바꿔주면 그게 바로 역행렬이 된다.
# 직교행렬은 어떻게 만드나? 직교행렬은 정방행렬이고 대칭행렬을 '고유값 분해'를 하면, 직교행렬이 만들어진다.
#직교행렬의 전치행렬은 역행렬이다 !!



#  문제 MASS 패키지의 birthwt에 대한 데이터에서 신생아의 체중에 영향을 미치는 요인에 대하여
#  선형회귀 분석 및 상관분석을 실시하시오.
#  어머니의 나이, 어머니의 체중, 인종, 흡연여부, 낙태여부, 고혈, 병, 임신방문여부, 출생체중
#  bwt가 출생체중이고, 이것이 종속변수
#  low age lwt race smoke ptl ht ui ftv bwt
