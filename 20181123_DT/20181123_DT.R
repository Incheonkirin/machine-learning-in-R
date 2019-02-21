library(rpart)
result <- sample(1: nrow(iris), nrow(iris) * 0.7)

train <- iris[result,]
test<- iris[-result,]
dim(train); dim(test)
formula <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width #종속변수 ~
formula
model <- rpart(formula = formula, data=train)
model
pred <- predict(model, test) #예측
pred
cpred <- ifelse(pred[,1] >= 0.5, "setosa", #지난번에 0.5기준으로 나눴던것 => logistic 회귀분석 // 0.5보다 크면 참// 회귀분석>sigmooid함수(0~1사이 확률값).
                ifelse(pred[,2] >= 0.5, "versicolor", "virginica")) #0.5보작 작으면 버지칼라, 그것도 아니면 버지니카
(tb=table(cpred, test$Species))
sum(diag(tb)) / nrow(test)
plot( model )
text(model, use.n=T, cex=0.6)
post(model, use.n = TRUE, file="")
formula <- Species ~ .
iris.df <- rpart(formula, data=iris)
iris.df
plot(iris.df)
text(iris.df, use.n=T, cex=0.6)
post(iris.df, file="")
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("rattle")
library('rattle')
prp(model)
rpart.plot(model)
#데이터 구조를 확인하기 위해서 사용함.


#install.packages("party")
library(party)
iris_ctree <- ctree(formula, data=train)
iris_ctree
plot(iris_ctree, type="simple")


#문제
#  wdbc_data.csv 를 이용하여 암진단 유무(diagnosis)를 결정하는 dicision_tree를 생성하시오
#  종속변수 : diagnosis 필드에 B->'양성', M->'음성'
#  rpart함수를 사용하시오
library(rpart)
library(rpart.plot)
library('rattle')


wdbc<- read.csv("wdbc_data.csv", stringsAsFactors = F)
str(wdbc)
wdbc <- wdbc[-1]
head(wdbc)
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
wdbc$diagnosis[1:10]

#숫자형 : min-max 정규화
normalize <- function(x){
  return((x-min(x))/ (max(x) -min(x)))
}
wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
wdbc_x
summary(wdbc_x)
class(wdbc_x)
wdbc_df<- data.frame (wdbc$diagnosis, wdbc_x) #다시 해주는 이유
dim(wdbc_df)
head(wdbc_df)
#모델 생성
idx=sample(nrow(wdbc_df), 0.7*nrow(wdbc_df))
wdbc_train=wdbc_df[idx, ]
wdbc_test=wdbc_df[-idx, ]
model2 <- rpart(wdbc.diagnosis ~., data=wdbc_train)
pred2 <- predict(model2, wdbc_test, type='class')
prp(model2)
rpart.plot(model2)
fancyRpartPlot(model2)

#평가
  1 2
  3 4

(tr<- table(pred2, wdbc_test$wdbc.diagnosis))
paste("정분류율", round(sum(diag(tr)) / nrow(wdbc_test)*100 ), '%')
paste("오분류율", round(sum(tr[1,2], tr[2,1])/ nrow(wdbc_test) *1000)/10, '%') #오분류

#정밀도, 특이도, 민감도를 구해보시오.
paste("정밀도", round(tr[1,1]/sum(tr[1,]) *100 )) #정밀도는 행으로 봄. (참+거짓 중) 포지티브로 예측한 것이 얼마나 맞았나.
paste("민감도", round(tr[1,1]/sum(tr[,1]) *100) ) #가로
paste("특이도", round(tr[2,2]/sum(tr[,2]) *100))


#평가매트릭(실제값과 예측값의 비교)

# 혼동행렬           실제positive    실제Negative    *오분류율
# 예측Positive            TF             FP          *정밀도
# 예측Negative            FN             FN

#                      *민감도       *특이도       *정분류율(정확도)
#                             *오분류율




#AdultUCI 데이터에 대하여 dicision tree 분석을 실시하시오
#주요변수 : age(나이), workclass(직업:4개), education(교육수준:16개),
#occupation(직업:12개), race(인종:아시아계, 백인), capital-gain(자본이득),
#capital-loss(자본손실), hours-per-week(주당 근무시간),
#native-country(국가), income(소득)

#install.packages("arules")
library(arules)
data("AdultUCI")

class(AdultUCI)
str(AdultUCI)

##income<- na.omit(income)
names(AdultUCI)[11] <- c("capital_gain")
names(AdultUCI)[12] <- c("capital_loss")
names(AdultUCI)[13] <- c("hours_per_week")
names(AdultUCI)[14] <- c("native_country")

result <- sample(1: nrow(AdultUCI), nrow(AdultUCI) * 0.7)

train <- AdultUCI[result,]
test<- AdultUCI[-result,]
dim(train); dim(test)

formula <- income~age+workclass+education+occupation+race+capital_gain+capital_loss+hours_per_week+native_country
formula
model <- rpart(formula = formula, data=train)
model
pred <- predict(model, test) #예측
pred

cpred <- ifelse(pred[,1] >= 0.5, "setosa", #지난번에 0.5기준으로 나눴던것 => logistic 회귀분석 // 0.5보다 크면 참// 회귀분석>sigmooid함수(0~1사이 확률값).
                ifelse(pred[,2] >= 0.5, "versicolor", "virginica")) #0.5보작 작으면 버지칼라, 그것도 아니면 버지니카
(tb=table(cpred, test$Species))
sum(diag(tb)) / nrow(test)
plot( model )
text(model, use.n=T, cex=0.6)
post(model, use.n = TRUE, file="")
formula <- Species ~ .
iris.df <- rpart(formula, data=iris)
iris.df
plot(iris.df)
text(iris.df, use.n=T, cex=0.6)
post(iris.df, file="")
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("rattle")
library('rattle')
prp(model)
rpart.plot(model)
#데이터 구조를 확인하기 위해서 사용함.

#install.packages("party")
library(party)
iris_ctree <- ctree(formula, data=train)
iris_ctree
plot(iris_ctree, type="simple")


# 새로운 함수 3가지
#createDataPartition       # trainControl        # confusionMatrix : 우리가 구했던 데이터가 한꺼번에 나온다.


library(caret) #노가다/잡일을 줄여주는 패키지
library(rpart.plot)

car_df<- read.csv("car.data", sep=',',header=FALSE)
#변수이름을 달아보자
title<-c("buying","maint","doors","persons","lug_boot","safety","Car_Evaluation")
#종속변수 = Car_Evaluation
names(car_df) <- title
str(car_df) # V7 은 자동차에 대한 품질 데이터
head(car_df)
set.seed(3033)
#createDataPartition 데이터 분할 함수.


intrain<- createDataPartition(y=car_df$buying, p=0.7, list=FALSE)
training<- car_df[intrain,]
testing<- car_df[-intrain,]
dim(training); dim(testing);
anyNA(car_df) #결측치가 한개라도 있는지 확인.
summary(car_df)
#dicision_tree는 이산적 연속적 데이터에 강건하다.
#install.packages("e1071")
#cross_validation = 데이터를 10등분으로 나누어라.

trctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(3333)
#train 훈련함수 : 일반함수 - 모든 모델에 공통
#formula : Car_Evaluation ~.
dtree_fit <- train(Car_Evaluation ~., data=training, method="rpart", #모델이름이 "rpart"
                   parms=list(split="information"),trControl=trctrl,tuneLength=10)
dtree_fit
prp(dtree_fit$finalModel, box.palette="Reds", tweak=1.2)
predict(dtree_fit, newdata=testing[1,])
test_pred<- predict(dtree_fit, newdata=testing)
confusionMatrix(test_pred, testing$Car_Evaluation)

#confusionMatrix을 사용하게 되면, 아래와 같이 하나하나 구해주었던 것을, 한꺼번에 구할 수 있게 된다.
#(tr<- table(pred2, wdbc_test$wdbc.diagnosis))
#paste("정분류율", round(sum(diag(tr)) / nrow(wdbc_test)*100 ), '%')
#paste("오분류율", round(sum(tr[1,2], tr[2,1])/ nrow(wdbc_test) *1000)/10, '%')
#paste("정밀도", round(tr[1,1]/sum(tr[1,]) *100 ))
#paste("특이도", round(tr[2,2]/sum(tr[,2]) *100))

# 1 2
# 3 4
# 전체중에 1 3 나오는게 A+C

# prevalance : (A+C)/(A+B+C+D)
# 민감도         A/(A+C)
# Detection Rate : A/(A+B+C+D) #TP
# Detection prevalance = (A+B) /(A+B+C+D)

#이해를 하기위해서 처음 것을 공부하고
#실무에서는 caret을 이용한 것을 사용한다.


set.seed(3333)
dtree_fit_gini <- train(Car_Evaluation ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl = trctrl, tuneLength = 10)
dtree_fit_gini

prp(dtree_fit_gini$finalModel, box.palette="Blues", tweak= 1.2)

test_pred_gini <- predict(dtree_fit_gini, newdata=testing)
confusionMatrix(test_pred_gini, testing$Car_Evaluation)

# RF (Random Forest) : disiontree의 문제점(변수의 순서바꾸면 결과가 달라진다.)

# Ensembling : 앙상블학습 - 여러 모델(열)을 생성하고 선거에 의해 선출.
# bootstrraping : 경우의 수를 많이 생성해서 학습
# Boosting : 강건한 학습기를 생성 : 약학습기에 가중치를 부여 개선책을 만들어 개선.
# baggingg : 'Boosting을' aggregation

# 특징:
  #변수기여도를 확인
  #병렬처리에 관심
  #caret패키지의 도움을 받아야 한다.

data(iris)
#install.packages("randomForest")
library(randomForest)
model = randomForest(Species~., data=iris)
model
model = randomForest(Species~., data=iris, ntree=300, mtry=4, na.action=na.omit)
model
ntree <- c(400, 500, 600) #3가지
mtry<- c(2:4) #3가지                 ==> 9가지 경우에 대하여 테스트
param <- data.frame(n=ntree, m=mtry)
param
for (i in param$n){
  cat('ntree = ', i, '\n')
  for (j in param$m){
    cat('mtry = ', j, '\n' )
    model=randomForest(Species~., data=iris,
                       ntree=i, mtry=j,
                       na.action=na.omit)
    print(model)
  }
}

model3 = randomForest(Species ~., data=iris, ntree=600, mtry=4,
                      importance=T, na.action=na.omit)
# mtry가 적을 수록 좋다

model3
names(model3)
importance(model3)
model3$importance
varImpPlot(model3) #큰 것이 가장 큰 영향을 미친다. Petal Length. Petal.Width.  ==> 변수선택


# 병렬처리 # CPU Core수. GPU 가 병렬처리를 하는데, 이것은 CPU Core수 만 ! 지원한다.

library(foreach)
m<-matrix(1:9, 3)
m
result<- 0
for (i in 1:ncol(m)){
  result[i] <- mean(m[,i])
}
result
#combine
foreach(i=1:nrow(m), .combine=c) %do% mean(m[,i])


model_iris<- foreach(i=rep(250, 4), .combine=combine) %do%
  randomForest(Species~., data=iris,
               ntree=i, mtry=2, na.actioon=na.omit)
model_iris
str(model_iris)
names(model_iris)
model_iris$predicted
model_iris$classes
importance(model_iris)
table(model_iris$predicted, iris$Species)


system.time(for(i in 1:100) mad(runif(1000)) )
Sys.time()
Sys.Date()

#install.packages("doParallel")
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
foreach(i=1:3) %dopar% sqrt(i)
registerDoParallel(cores=2)
foreach(i=1:3) %dopar% sqrt(i)


registerDoParallel(core=4)
getDoParWorkers()
system.time(
  rf_iris<- foreach(ntree=rep(250,8), .combine=combine,
                    .packages='randomForest', .multicombine=TRUE) %dopar%
    randomForest(Species~., data=iris,
                 ntree=ntree, mtry=2, na.action=na.omit) #4개의 변수 중에 2개를 선택
)
rf_iris
####
#데이터 필터링
x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000

ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime


##문제
# 1) weatherAUS를 100개의 트리와 2개의 분류변수를 파라미터로 하여 모델을 생성하고
# 분류정확도를 구하시오
# train:test = 7:3
# 변수중요도를 출력하시오

library(randomForest)
weatherAUS<- read.csv("weatherAUS.csv",stringsAsFactors = TRUE)
#weatherAUS=read.csv(file.choose()) #stringAsFactors = TRUE)str(weatherAUS)
str(weatherAUS)
weatherAUS=weatherAUS[,c(-1,-2,-22,-23)]
str(weatherAUS)

wAUS<- na.omit(weatherAUS)
sum(is.na(wAUS))

#결측치 처리
wAUS<-na.omit(wAUS)
sum(is.na(wAUS))

##sampling
set.seed(123)
result<- sample(1:nrow(wAUS), nrow(wAUS)*0.7)

train<- wAUS[result,]
test<- wAUS[-result,]
dim(train)
dim(test)
str(train)
sum(is.na(wAUS))

head(train,2)

#modeling과 prediction
rain=randomForest(RainTomorrow~., data=train, ntree=100, mtry=2, importance=TRUE, na.action=na.omit)
rain
rain1<- predict(rain, newdata=test)
#분류정확도
library(caret)
(c<- confusionMatrix(rainp1, test$RainTomorrow))
importance(rain)
varImpPlot(rain)



# 2) 최적의 트리와 분류변수 파라미터의 개수를 찾아보시오( 최적의 parameter조합 확인)
ntree <- c(200, 300, 400, 500, 600)
mtry <- c(6:10)
param<- data.frame(n=ntree, m=mtry)
param
for (i in param$n){
  cat('ntree= ', i, '\n')
  for(j in param$m){
    cat('mtry= ', j, '\n')
    rain = randomForest(RainTomorrow~., data=train,
                        ntree=i, mtry=j,
                        na.action=na.omit)
    print(rain)
  }
}

str(model)



# 3) 병렬처리를 적용하여 작업 속도를 개선해 보시오
registerDoParallel(cores=4)
getDoParWorkers()
system.time(
  rf_weather <- foreachh(ntree=c(200), .combine=combine,
                         .packages= 'randomForest', .multicombine=TRUE) %dopar%
    randomForest(RainTomorrow~., data=train,
                 ntree=ntree, mtry=8, importance=TRUE, na.action=na.omit)

)
rf_weather
str(rf_weather)
pred<- predict(rf_weather, newdata=test)
tb<- table(pred, test$RainTomorrow)
paste('정분류율 = ', round(sum(diag(tb))/sum(tb)*100), '%')


# 4)
#install.packages("cvTools")
library(cvTools)
library(foreach)
library(randomForest)

set.seed(719)
K=10
R=5
cv=cvFolds(NROW(wAUS), K=K, R=R) #cv 프로스벨리데이션.
cv$subsets[which(cv$which ==1), 1]
length(cv)
NCOL(cv)
grid3<- expand.grid(ntree=c(200, 3000, 400, 500), mtry=c(3,10))
result <- foreach(g=1:NROW(grid3), .combine=rbind) %do% {
  foreach(r=1:R, .combine=rbind) %do% {
    foreach(k=1:K, .combine=rbind) %do% {

      validation_idx<- cv$subsets[which(cv$which ==k), r]
      train<- wAUS[-validation_idx,]
      validation<-wAUS[validation_idx, ]
      m <- randomForest(RainTomorrow ~.,
                        data=train,
                        ntree=grid3[g, "ntree"],
                        mtry=grid3[g, "mtry"])

      predicted <- predict(m, newdata=test)

      precision <- sum(predicted == test$RainTomorrow)/NROW(predicted)
      return(data.frame(g=g, r=r, k=k, precision=precision))
    }
  }
}
result
head(result)
