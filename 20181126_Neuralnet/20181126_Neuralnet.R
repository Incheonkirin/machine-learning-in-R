#install.packages('neuralnet')
library(neuralnet)

x1 <- runif(30, -1, 1)
x2 <- runif(30, -1, 1)
x <- cbind(x1, x2)
Y <- ifelse(x2>0.5+x1, +1, -1) #컴퓨터는 이 식을 찾아내는 일을 합니다 x2>0.5+x1

plot (x, pch=ifelse(Y>0, "+", "-"), xlim=c(-1,1), ylim=c(-1,1), cex=2)
abline(0.5, 1)
#사람 => perceptron을 이용해서 자동으로 구분선을 찾아가는 과정 simulation
#거리값을 구하는 함수

calculate_distance=function(x, w, b){ #가중치를 적용해서 공간변환.
  sum(x*w) + b
}

#분류기 - 예측기
linear_classifier = function(x, w, b) {
  distances = apply(x, 1, calculate_distance, w, b) #w, b가 아직 정해지지 않아서, 뒤에 달아줌.
  return(ifelse(distances<0, -1, +1)) # activation 함수
}

(linear_classifier( x, c(-1,1)/sqrt(2), -sqrt(2)/4)) #초기값을 해서 (-1,1) 여기다가 넣어본거죠.

second_norm = function(x) {sqrt(sum(x * x))} #얘는 뭐하는거에요? normalize하는 거죠. 제곱한다음 sqrt로 나눴으니까.


perceptron = function(x, y, learning_rate=1) { #x와 y를 집어넣어놓고 학습률 1로.
  #가중치 바이어스b 카운트k
  w = vector(length=ncol(x)) ; b = 0 ; k = 0 #nocl이 몇개에요? 2개죠. 위에 cbind로 합쳐놓은거. k 횟수카운트.
  R = max(apply(x, 1, second_norm)) #들어가서 각각 값을 노말라이즈해라.
  incorrect = TRUE #프로그램을 제어하기 위해서
  plot(x, cex=0.2)
  while (incorrect) { #횟수가 지정되지 않은 무한(반복)
    incorrect = FALSE #끝내라고
    yc<- linear_classifier(x, w, b) #순전파: 값의 예측 #yc 예측값.
    for (i in 1:nrow(x)) {
      if (y[i] != yc[i]) { #다르면 게속 작업(가중치를 수정) #같으면 끝내라는 의미값 #각행의 예측값이 같지 않으면,
        w <- w + learning_rate * y[i] * x[i,] #역전파 (back- propagation)           #y[i] * x[i,] 를 learning_rate에 곱해서 더해주라.
        #이전 가중치에 수정값을 더해줌
        b <- b + learning_rate * y[i] * R^2
        k <- k+1
        if(k%%5 ==0) { #5번마다 결과 보고
          intercept <- -b/ w[[2]]
          slope<- -w[[1]]/w[[2]]
          abline(intercept, slope,col="red")
          cat("반복 # ",k, "\n")
          cat("Press [enter] to continue")
          line<- readline()
        }
        incorrect = TRUE
      }
    } }
  s = second_norm(w) #결과값을 정규화하기 위하여 (정규화하지 않으면 값이 발산하거나 0으로 수렴)
  return(list(w=w/s, b=b/s, updates=k))
}

(p <- perceptron(x, Y))
(y <- linear_classifier(x, p$w, p$b)) #예측분류값
plot(x, cex=0.2)
points(subset(x, Y==1), col="black", pch="+", cex=2)
points(subset(x, Y==-1), col="red", pch="-", cex=2)

intercept <- -p$b/ p$w[[2]] #요소값 => 자료구조를 벗기고 그 요소가 가진 값으로
slope <- -p$w[[1]]/p$w[[2]] #계수의 비 - 기울기
abline(intercept, slope, col="green")


#이렇게 하면 복잡하잖아. 구조를 알 수 있는거고..
#실제 할때는 아래와 같이 한다는 말이지..

#install.packages("nnet")
library(nnet)
df = data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('n','n','n','y','y','y'))
)
df
model_net1 = nnet(y ~., df, size = 1) # nnet 신경망 모델 : 단일레이어 사용.
model_net1
#모델을 만든거지

summary(model_net1)
names(model_net1)

model_net1$fitted.values
predict(model_net1, df)
#가중치 + bias 를 가진 신경망 모델을 만들었다는 말이지.
p<- predict(model_net1, df, type="class")
#모델을 넣으면 자동으로 예측을 해주는거지


table(p, df$y)


#이번에는 실제 데이터 IRIS 데이터에 넣고 해보겠습니다.

#install.packages("devtools")
library(devtools)
library(nnet)
set.seed(123)
idx=sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
training
testing

nrow(training)
nrow(testing)

names(training)
nrow(training)
#hidden layer 사이즈를 수정 => hyper parameter
(model_net_iris1 = nnet(Species ~., training, size = 1) )
(model_net_iris3 = nnet(Species ~., training, size = 3) )
#1 이 정확하게 나왔다. 사이즈를 무조건 높이는게 좋은 건 아니다.

summary(model_net_iris1)
summary(model_net_iris3)

predict(model_net_iris1, testing, type="class")
predict(model_net_iris3, testing, type="class")

tb<-table(predict(model_net_iris1,testing, type="class"), testing$Species)
sum(diag(tb))/sum(tb)
tb<-table(predict(model_net_iris3,testing, type="class"), testing$Species)
sum(diag(tb))/sum(tb)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model_net_iris1)


#같은 내용을 neuralnetwork로 풀어보겠습니다
#nerualnet 패키지로( 다층레이어 - deep learning의 초기버전)

#install.packages("neuralnet")
library(neuralnet)

data("iris")
set.seed(1234)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training_iris=iris[idx,]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)
training_iris$Species <- ifelse(training_iris$Species =='setosa' , 1, #범주형 변수 사용불가 #바꾼 이유? nnet할때는 3개 그냥 썼지? 뉴럴은 범주형 변수 쓸수 없어요. 그래서 숫자형 변수로 바꿔줘야해..
                                ifelse(training_iris$Species == 'versicolor', 2, 3))
head(training_iris); tail(training_iris)
str(training_iris)

testing_iris$Species <- ifelse(testing_iris$Species =='setosa', 1,
                               ifelse(testing_iris$Species =='versicolor', 2, 3))
head(testing_iris); tail(testing_iris)
str(testing_iris)

normal <- function(x){ #min-max 정규화
  return(( x - min(x) / (max(x)- min(x) )))
}
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)
str(training_nor)

testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)
str(testing_iris)

model_net = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                      #미분방정식 y= ax^2 + x, y = ax^2 + bz^3 변수가 두개이상있을때는 미분불가. x에 대해 미분하면 나머지는 상수로 취급(봄).
                      data=training_nor, hidden=1, threshold=0.01, #threshold 는 편미분해서 종료값, 임계값 이 수준 넘어가면 더 자세히 계산하지말고 중지하라. 미분이 정확하게 하면 아주 정확하게 할 수 있잖아.
                      linear.output=FALSE, act.fct='logistic')

model_net
names(model_net)
model_net$err.fct #sum of square error #cost fuction 기본 디폴트
#act.fct : activation function : 활성화함수 => logistic 0~1 사이의 값을 가짐.
#히든레이어 하나 주고.
model_net$net.result
model_net$weights
model_net$algorithm #알고리듬도 없네??
model_net$learningrate #이게 없네?
plot(model_net)
model_result<- compute(model_net, testing_nor[-5]) #예측함수가 predict가 아닌 compute함수
model_result$net.result
cor(model_result$net.result, testing_nor$Species)  #수치적으로 나오기 때문에 상관비교가 불가능함.
# 81%가 나온다..?



#hyperparameter 수정 : hidden : 2, algorithm = backprop
#deep해서 히든레이어를 추가시 정교해진다.
model_net2 = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                       data=training_nor, hidden=3, learningrate=0.01,
                       linear.output = FALSE, act.fct='logistic',
                       algorithm="backprop")
model_result2 <- compute(model_net2, testing_nor[c(1:4)])
cor(model_result2$net.result, testing_nor$Species)


model_net3 = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                       data=training_nor, hidden=c(2,2), learningrate=0.01,
                       linear.output = FALSE, act.fct='logistic',
                       algorithm = "backprop")
model_net3
model_result3<- compute(model_net3, testing_nor[c(1:4)])
cor(model_result3$net.result, testing_nor$Species)
plot(model_net3)





#문제 boston 집값 예측을 신경망 모델로 구현해 보시오.
library(MASS)
data("Boston")

set.seed(123)
DataFrame<- Boston
str(DataFrame)

hist(DataFrame$medv)
head(DataFrame, 3)
apply(DataFrame, 2, range)

maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)
medv_maxValue <- maxValue["medv"]
medv_minValue <- minValue["medv"]
DataFrame<- as.data.frame(scale(DataFrame, center=minValue,
                                scale=maxValue))
summary(DataFrame)
idx<- sample(1:nrow(DataFrame), 400)
trainDF <- DataFrame[idx,]
testDF <- DataFrame[-idx,]

allVars <- colnames(DataFrame)
predictorVars <- allVars[!allVars%in%"medv"]
predictorVars <- paste(predictorVars, collapse = "+")
form <- as.formula(paste("medv~", predictorVars, collapse = "+"))
form




neuralModel <- neuralnet(formula =form, hidden= c(4, 3, 2), learningrate = 0.01 , linear.output = T, data=trainDF)
plot(neuralModel)
predictions <- compute(neuralModel, testDF[,1:13]) #행렬 계산.
str(predictions)
predictions$net.result
cor(predictions$net.result, testDF$medv)



#문제 :
# 예측값을 원래 사이즈로 계산하시오 ( 신경망 할 때는 정규화 => 예측 : 정규화된 => 원래 사이즈로 복원)


names(predictions)
#2개를 가지고 있어
predictions$net.result
#0. x 으로 값 가지고 있어. 원래 값으로 바꿔줘야 하죠
predictions$net.result * (medv_maxValue-medv_minValue) + medv_minValue


#그리고
class(predictions)
#하면 리스트입니다.

#리스트라서,

#4,3,2에 대한 가중치 값.
predictions$neurons[1]             # 이거 쳤을 때 나오는 것들 의미/ 어떻게 생겼나.
predictions$neurons[2]#  4 #바이어스가 전부 1로 들어가 있어서 5개.
predictions$neurons[3]# 3
predictions$neurons[4]# 2   바이어스 + 2개의 가중치        505* 3          [,1] [,2] [,3] 1은 왜있다고 했어요?
predictions$neurons
#히든레이어 4, 3, 2 라고 위에 설정해두었음.


cor(predictions$net.result, testDF$medv) #정규화해서 모두 소수점을 가니까

#범위수 곱하고 민값더해주면 원래값
predictions$net.result * (medv_maxValue-medv_minValue) + medv_minValue





#문제 wine.data를 로딩하고 와인의 label을 분류하는 모델을 신경망으로 구현하시오.
#종속변수 label
#class.ind : Generates Class Indicator Matrix from a Factor : dummy변수 / on-hot-encoding
#dummy변수화 한다. 범주형변수로 만들고..

require(neuralnet)
require(ggplot2)
set.seed(10)
wines<- read.csv("./wine.data")
names(wines)<- c("label",
                 "Alcohol",
                 "Malic_acid",
                 "Ash",
                 "Alcalinity_of_ash",
                 "Magnesium",
                 "Total_phenos",
                 "Flavanoids",
                 "Nonflavanoid_phenols",
                 "Paoanthocyanins",
                 "Color_intensity",
                 "Hue",
                 "OD280_OD315_of_diluted_wines",
                 "Proline")

head(wines)
str(wines)

#신경망은 예측과 분류 모두 사용됨. wine은 분류기. 집값은 예측.

as.factor(wines$label)
class.ind(as.factor(wines$label)) #dummy화 변수로 변환
train <- cbind(wines[, 2:14], class.ind(as.factor(wines$label)))
names(train) <- c(names(wines)[2:14], "l1", "l2", "l3")
# 3개로 출력되지요??

minmaxnorm<- function(x){ (x- min(x))/(max(x) - min(x)) }
train[, 1:13] <- data.frame(lapply(train[, 1:13], minmaxnorm))
n <- names(train)


(f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1", "l2", "l3")],
                                               collapse= " + "))))

nn <- neuralnet(f,
                data = train,
                hidden = c(13, 10, 3), # 13개 변수 => 13, 3 => 생략이 많아서 중간에 특성이 사라져, 잘못 생각 가능.
                act.fct = "logistic",   #0~1 사이 확률값.
                linear.output = FALSE, #activation 함수를 output에 적용할 것인가 말것인가?(FALSE는 적용)
                lifesign = "minimal") #none, minimal, full
plot(nn)
#위의 결과로 평가해보시오.

pr.nn <- compute(nn, train[, 1:13]) #평가는 뭘로 한다고요? compute
names(pr.nn)
dim(wines)
pr.nn$neurons[4]  # 177 * 4( 3 + 1 )

(pr.nn_ <- pr.nn$net.result)
(original_values<- max.col(train[, 14:16])) #max 값은 컬럼의 인덱스. 가장 큰 것의 인덱스를 가지고 온다.
# 14-> 1 15-> 2 16-> 3
(pr.nn_2 <- max.col(pr.nn_))
mean(pr.nn_2 == original_values)








###
library(class)
weatherAUS <- read.csv("weatherAUS.csv")
str(weatherAUS)
weatherAUS_df <- weatherAUS[-c(1,2,22,23)]
weatherAUS_df <- na.omit(weatherAUS_df)
str(weatherAUS_df)
idx <- sample(nrow(weatherAUS_df), 0.7*nrow(weatherAUS_df))
train_weather <- weatherAUS_df[idx, ]
test_weather <- weatherAUS_df[-idx, ]

k <- sqrt(nrow(train_weather))

str(train_weather)
#knn은 모델이 없음. 게으른 모델. : 훈련과 테스트가 동시에 실시됨.

model_weather <- knn(train_weather[-c(6, 8:9, 20)],
                     test_weather[-c(6,8:9,20)],
                     train_weather$RainTomorrow, k=111)

t <- table(model_weather, test_weather$RainTomorrow)
t

(3888+544) / nrow(test_weather)
(t[1,1]+t[2,2]) / nrow(test_weather)

## k값 결정이 문제
weather <- read.csv("weatherAUS.csv", stringsAsFactors = FALSE)
str(weather)
weather<- weather[,c(-1, -2, -8, -10, -11, -22)]
weather <- na.omit(weather)
table(weather$RainTomorrow)
weather$RainTomorrow <- factor(weather$RainTomorrow) #짝수를 사용하지 않습니다.

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
weather_n <- as.data.frame(lapply(weather[1:17], normalize))

idx <- sample(1:nrow(weather_n), nrow(weather_n) *0.7)
weather_train <- weather_n[idx, ]
weather_test<- weather_n[-idx, ]
weather_train_labels <- weather[idx, 18]
weather_test_labels <- weather[-idx, 18]
weather_test_pred<- NULL
error.rate<- NULL
#1:30:2
for (i in 1:30) {
  error.rate[i] <- 0
  if(i%% 2)
  {
    weather_test_pred <- knn(train = weather_train, test=weather_test, cl=weather_train_labels, k=i)
    error.rate[i] <- mean(weather_test_pred != weather_test_labels) #데이터를 모으고
  }
}


library(caret)

k.values <- 1:30
error.df <- data.frame(error.rate, k.values)
ggplot(error.df, aes(k.values, error.rate))+ geom_point() + geom_line(lty='dotted', color='red')

weather_test_pred <- knn(train=weather_train, test=weather_test, cl=weather_train_labels, k=29)
confusionMatrix(weather_test_labels, weather_test_pred)
# 88%
