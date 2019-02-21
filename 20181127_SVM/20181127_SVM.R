#SVM(support vector machine)
library(e1071)
data(cats, package = "MASS")
str(cats)
m <- svm(Sex~., data=cats)
plot(m, cats) #plot overloading해서
plot(m, cats, svSymbol = 1, dataSymbol=2, symbolPaleette=rainbow(4),
     color.palette=terrain.colors)
m$cost
m$gamma
## 범주형 데이터도 지원
data(iris)
m2 <- svm(Species~., data = iris)
plot(m2, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4))
## iris svm model 생성
data(iris)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training=iris[idx, ]
testing=iris[-idx, ]
training
testing
dim(training)
dim(testing)
model_svm=svm(Species ~., data= training, na.action=na.omit)
summary(model_svm)

#하이퍼 파라메터
# summary에서 중요한부분

#Parameters:
#  SVM-Type:  C-classification = 결과값이 0~ 무한대 = relu, nu-classification = 결과값 0~1 = sigmoid activation 함수    분류모델
#SVM-Kernel:  radial (방사형커널)
#cost:  1
#gamma:  0.25


pred <- predict(model_svm, testing)
(res=table(pred, testing$Species))
sum(res[1,1] + res[2,2] + res[3,3]) / nrow(testing)
sum(diag(res)) / sum(res)

plot(model_svm, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
plot(model_svm, iris, Sepal.Width ~ Sepal.Length)
plot(model_svm, iris, Petal.Width ~ Petal.Length)

#지수형
1e+01
1e-01
1e-05
10^(-5:1)
10^(-12:3)


tuning <- tune.svm(Species ~., data = training,    #cv : cross-validation :모든 데이터가 테스트에 참여할 수 있도록.- 최적화된 결과를 만들어 줄 수 있도록..
                   gamma = 10^(-5:1), cost =  10^(-10:3))
tuning
# -> best parameter를 출력해줌.

#gamma : 0.1 , cost:100
tuning
attributes(tuning)
tuning$best.parameters
tuning$best.parameters[,'gamma']
tuning$best.parameters[,'cost']

model_svm2 = svm(Species ~., data = training, gamma=tuning$best.parameters[,'gamma'],
                 cost=tuning$best.parameters[,'cost'], na.action=na.omit)
summary(model_svm2)
pred2<- predict(model_svm2, testing)
(res=table(pred2, testing$Species))
sum(res[1,1] + res[2,2] + res[3,3]) / nrow(testing) #정확도를 0.01percent 라도 올릴 수 있다면. 해야한다.


#svm 에서 regression (SVR)
#install.packages("mlbench")
library(mlbench)
library(rpart)
data(Ozone, package="mlbench")
str(Ozone)
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex, -3])
trainset<- na.omit(Ozone[-testindex, -3])
#종속변수는 연속형 => 자동회귀작동.
tuning <- tune.svm(V4 ~., data = trainset,
                   gamma = 10^(-5:1) , cost = 10^(-10:3))

tuning$best.parameters
tuning$best.parameters[,"gamma"]
tuning$best.parameters[,"cost"]

svm.model <- svm(V4 ~ ., data = trainset, cost = tuning$best.parameters[, "cost"],
                 gamma = tuning$best.parameters[,"gamma"])

svm.pred <- predict(svm.model, testset)
cor(svm.pred, testset[,3])
summary(svm.model) #eps-regression   SVM-Type : eps-regression

#SVM-Type:  eps-regression
#SVM-Kernel:  radial
#cost:  10
#gamma:  0.1
#epsilon:  0.1    #epsilon상수는 부동소수점 비교에서 0.1이하는 같은 것으로 본다. 0.13, 0.14 #잔차의 정밀도를 0.1까지 하라.


#rpart와 비교
rpart.model <- rpart(V4 ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset)
cor(rpart.pred, testset[, 3])


#문제 : mlbench의 Glass를 분류하는 분류기를 svm 모델을 사용하여 구현하시오.
# 조건 1, Glass의 Type이 종속변수

# 1)sampling
# 2)tuning
# 3)modeling
# 4)evaluation
# 5)rpart와 비교


library(e1071)
library(rpart)
data(Glass, package="mlbench")
str(Glass)
set.seed(1)
index <- 1:nrow(Glass)
testindex<- sample(index, trunc(length(index)/3))

testset <- Glass[testindex, ]
trainset <- Glass[-testindex, ]
# 2) tuning
tuning <- tune.svm(Type ~ ., data = trainset,
                   gamma = 10^(-5:1), cost = 10^(-10:3))
# 3) modeling
svm.model <- svm(Type ~., data = trainset, cost = tuning$best.parameters[,"cost"],
                 gamma = tuning$best.parameters[,"gamma"])
# 4) evaluation
svm.pred <- predict(svm.model, testset[,-10])
res<- table(pred=svm.pred, true=testset[,10])
sum(diag(res))/sum(res) # 0.6901408

# 5) rpart비교
rpart.model <- rpart(Type ~., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type="class")
tr<- table(pred=rpart.pred, true = testset[,10])
sum(diag(tr))/ sum(tr) # 0.6056338



#딥러닝은 문자자체를 인식하는 거고, 이건 중간단계가 하나 더 있어요
#OCR 캐릭터 인식
letters = read.csv("letterdata.csv") #scan데이터로부터 기장, 박스
str(letters)
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000,]
install.packages("kernlab")
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot") #vanilladot 선형 커널
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
caret::confusionMatrix(letter_predictions, letters_test$letter)
agreement<- letter_predictions ==letters_test$letter
prop.table(table(agreement)) #84%
letter_classifier_rbbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot") #방사형커널로
letter_predictions <- predict(letter_classifier_rbf, letters_test)
caret::confusionMatrix(letter_predictions, letters_test$letter)
agreement<- letter_predictions  == letters_test$letter
prop.table(table(agreement)) #91%


#kernlab 의 kernel은
#tanhdot n- sigmoid
#vanilladot - linear
#polydot - poly
#rbfdot - radial


library(kernlab) #분류와 커널을 이용한 비교
#C-svc: relu에 의한 분류
#nu-svc : sigmoid activation 함수

(iris.ksvm<- ksvm(Species ~., data=iris))
(iris.ksvm<- ksvm(Species ~., type="nu-svc", data=iris))
predict(iris.ksvm, newdata=iris)
iris.ksvm
(iris.ksvm<- ksvm(Species ~., data=iris, kernel="rbfdot"))
(iris.ksvm<- ksvm(Species ~., data=iris, kernel="vanilladot"))
(iris.ksvm<- ksvm(Species ~., data=iris, kernel="polydot", kpar=list(degree=3)))

(iris.ksvm<- ksvm(Species ~., data=iris, type="C-svc", kernel="rbfdot"))
(iris.ksvm<- ksvm(Species ~., data=iris, type="C-svc", kernel="vanilladot"))
(iris.ksvm<- ksvm(Species ~., data=iris, type="C-svc", kernel="polydot", kpar=list(degree=3)))

(iris.ksvm<- ksvm(Species ~., data=iris, type="nu-svc", kernel="rbfdot"))
(iris.ksvm<- ksvm(Species ~., data=iris, type="nu-svc", kernel="vanilladot"))
(iris.ksvm<- ksvm(Species ~., data=iris, type="nu-svc", kernel="polydot", kpar=list(degree=3)))



#이번에는 캐럿패키지를 해보겠습니다.
#caret 패키지 ; 심장병여부가 종속변수인 데이터.
library(caret) #기본적으로 병렬처리 진행.
heart_df <- read.csv("heart_tidy.csv", sep=',', header=FALSE)
str(heart_df) # V14 종속변수, 심장병여부
head(heart_df)
set.seed(3033)
#데이터 구성을 고려 - 분포
intrain<- createDataPartition(y=heart_df$V14, p=0.7, list=FALSE)
(training<- heart_df[intrain,])
(testing<- heart_df[-intrain,])
dim(training); dim(testing);
training$V14
testing#V14
summary(heart_df)
training[["V14"]] = factor(training[["V14"]])
testing[["V14"]] = factor(testing[["V14"]])

levels(training[["V14"]])
#cross-validation 데이터를 10개로 분할, 3개의 변수를 이용해서 #이전에는 70:30으로 나눠서 했어더니. 참여했던 넘은 참여하고 ... 이걸 다 참여시키도록 하는 방법이지.... 중요한 방법. 크로스 밸리데이션.
#또 데이터가 부족할때 이 방법을 사용하면 경우의 수가 많아진다. 이익이있다.
trctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(3233)
#train을 이용한 모든 모델 구성 가능  #svmRadial 방사형이 가장 정확했었다.  svmlinear로 확인해보고, svmRadial로 확인해보라.

#caret패키지가 원하는 것은 자동화. 정확도를 높이기 위한 파라미터 튜닝.
svm_Linear <- train(V14 ~., data=training, method="svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"), #전처리
                    tuneLength= 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata=testing)
class(test_pred)
levels(test_pred)
testing$V14
levels(testing$V14)
confusionMatrix(test_pred, testing$V14)



#12 10, 20, 3 조합
#svmRadial을 이용한 모델 생성
#여러 단계 - 처음 값을 범위를 넓게 하고고 .. =>정확하게 나오는 범위가 있으면 => 0.07, 0.08, 0.09 세밀하게 테스트
#
grid_radial<- expand.grid(sigma = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.5),
                   C=c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

set.seed(3233)

#160행 16* 열 10 => 고속으로 진행
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmRadial",
                         trControl=trctrl,
                         preProcess=c("center", "scale"),
                         tuneGrid = grid_radial, #parameter tuning (hyper parameter)
                         tuneLength=10)
svm_Linear_Grid
plot(svm_Linear_Grid)
test_pred_grid<- predict(svm_Linear_Grid, newdata=testing)
test_pred_grid

names(svm_Linear_Grid)
svm_Linear_Grid$bestTune #Sigma가 언제일때 가장 좋은 건지 알게된다.
svm_Linear_Grid$finalModel


#가장 많이 사용하는 방법(분류와 회귀). SVM이 항상 베스트는 아니다.단, 일반적으로 높은 정확도 보임.
#SVM support vector를 이용해서 optimization된 분할성을 찾는것이고,
# 커널함수 : 고차원으로 데이터를 매핑한 다음 평면분할
#중요한 파라메터 - gamma, cost에 대한 hyper parameter + kernel
# tune.svm함수 로 hyper parameter tuning하게 되는데
#tune.svm보다 좋은게, caret package가 tuning기능을 제공(병렬처리도 지원)
#cv, expand.grid 이용한 매개변수 조합을 다룬다.



#문제 letterdata.csv에 caret package를 적용해서 parameter tuning을 한 다음
# 최적의 svm 모델을 생성해보시오.


library(caret)
letterdata=read.csv("letterdata.csv")
str(letterdata)
head(letterdata)
set.seed(3033)
letterdata=na.omit(letterdata)
intrain <- createDataPartition(y= letterdata$letter, p=0.7, list=FALSE)
training<- letterdata[intrain,]
str(training)
testing<- letterdata[-intrain,]
dim(training); dim(testing);
summary(training)
trctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
grid<- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.25, 1.5, 1.75, 2.5 ))
set.seed(3233)
svm_Linear <- train(letter~., data=training, method="svmLinear",
                    trControl=trctrl,
                    preProcess=c("center", "scale"),
                    tuneGrid=grid,
                    gridtuneLength=10)
svm_Linear
plot(svm_Linear)
test_pred <- predict(svm_Linear, newdata=testing)
test_pred
#confusionMatrix는 결과가 이산치일때 사용함. 결과가 연속적일때는 상관계수 cor 사용.
confusionMatrix(test_pred, testing$letter)
table(test_pred, testing$letter)
result<- test_pred == testing$letter
prop.table(table(result))

trctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
grid_radial<- expand.grid(sigma =c(0.05, 0.06, 0.07, 0.08, 0.08, 0.1, 0.25, 0.5),
                          C= c(0, 0.01, 0.05, 0.1, 0.5, 1, 2))
set.seed(100)
svm_Radial_Grid <- train(letter~., data=training, method="svmRadial",
                         trControl=trctrl,
                         preProcess=c("center", "scale"),
                         tuneGrid=grid_radial,
                         tuneLength= 10)
plot(svm_Radial_Grid)
test_pred<- predict(svm_Radial_Grid, newdata=testing)
test_pred
confusionMatrix(test_pred, testing$letter)


#다음 3가지 방법으로 svm모델을 구성하고 비교테스트 해보시오.
#svm
#tune.svm
#foreach문으로
#현재 caret package tuning 으로 자동화 하고 있다.
#대용량 데이터인 경우 clustering SVM을 이용한 분류

library(doParallel)
(cl <- makeCluster(detectCores()))
registerDoParallel(cl)

library("e1071")
letterdata = read.csv("letterdata.csv")
str(letterdata)
head(letterdata)
length(letterdata)
nrow(letterdata)

letterdata <- na.omit(letterdata)
length(letterdata)
nrow(letterdata)

set.seed(415)

idx = sample(1:nrow(letterdata), 0.7*nrow(letterdata))
training_letter = letterdata[idx, ]
testing_letter= letterdata[-idx, ]

model_letter <- svm(letter~., data = training_letter)
training_dep <- letterdata[ , -1]
training_aply <- letterdata[,1]
pred_letter<- predict(model_letter, testing_letter[,-1])
pr=prop.table(table(pred_letter == teesting_letter$letter))
pr
pr["TRUE"]
#파라미터 조합 생성 (foreach문을 하나만으로 해결하도록
(cost<- rep(c(0.01, 0.1, 1, 10, 100), each=5))
(gamma <- rep(c(0.01, 0.1, 1, 10, 100), 5))
length(cost)
length(gamma)
#병렬처리 : 반드시 %dopar%사용
#r , rbind, cbind, + 등의 연산자로 묶음
#r에서는 함수가 명시적으로 리턴하지 않더라도 마지막에 게산한 값이 리턴값.
out<- foreach(i=1:length(cost), .combine=rbind, .inorder=FALSE, .packages='e1071', .verbose=TRUE) %dopar%{
  model_letter <- svm(letter~., data=raining_letter, kernel="radial",
                      gamma=gamma[i], cost=cost[i])
  pred_letter <- predict(model_letter, testing_letter[,-1])
  result<- pred_letter == testing_letter$letter
  table(result)
  pr=prop.table(table(result))
  data.frame(gamma=gamma[i], cost=cost[i], accuracy=pr["TRUE"])
}
#foreach는 디버깅도 안된다. 병렬처리이기 때문에

tune.svm_res<- out[which.max(out$accuracy),]
tune.svm_res$gamma
tune.svm_res$cost
stopCluster(cl) #병렬처리가 해제된다.

model_letter<- svm(letter~., data = training_letter,
                   gamma=tune.svm_res$gamma, cost=tune.svm_res$cost)

pred_letter <- predict(model_letter, testing_letter[,-1])
table(pred_letter, testing_letter$letter)
result<- pred_letter == testing_letter$letter
table(result)
prop.tablee(table(result)) 97%



#대용량 데이터인 경우 clustering SVM을 이용한 분류
#clustering 을 먼저합니다. 그리고 svm 적용하게되면, 빨리할 수 있다. clustering SVM 대용량 처리할 때 사용함.
#위의 데이터와 한끝 차이. 우리가 가진 데이터는 2번째 letter가 있다.

#문제
# 1번열에 있는 데이터를 없애도록 다음 코드를 정리해서 작동하도록 처리합니다.



library(swarmSVM)
help(swarmSVM)
library(caret)

letterdata = read.csv("letterdata.csv")
str(letterdata)
#클러스터는 군집분석뿐만 아니라, 다른 작업하기 전에 전처리용으로 많이 사용한다.
# chatbot -> 다양한 주제 비지도 학습
head(letterdata)
set.seed(3033)
letterdata = na.omit(letterdata)
intrain <- createDataPartition(y= letterdata$letter, p=0.7, list=FALSE)
training<- letterdata[intrain,]
str(training)
testing<- letterdata[-intrain,]
dim(training); dim(testing);
summary(training)
csvm.obj= clusterSVM(x = training[,-2], y=training[,2], lambda=1,#l2-norm : 과적합 방지용 (람다)
                     centers=2, seed=512, verbose=0,
                     valid.x=testing[,-2], valid.y=testing[,2])
names(csvm.obj)
csvm.obj$svm
csvm.pred=csvm.obj$valid.pred
