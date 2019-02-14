library(stringr)
getwd()
data <- read.csv("input.csv", header=T, encoding="UTF-8")
colnames(data)[1] <-"id"
print(data)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))
sal <- max(data$salary)
print(sal)
# 조건식을 가지고 결과 boolean indexing을 처리하는 subset
retval <- subset(data, salary == max(salary)) #filtering
print(retval)
data
retval <- subset(data, dept == "IT ")
print(retval)
info <- subset(data, salary > 600 & str_trim(dept) == "IT") #공백 없애는 str_trim
print(info)

retval <- subset(data, as.Date(start_date) >as.Date("2014-01-01"))
print(retval)

write.csv(retval, "output.csv")
newdata<- read.csv("output.csv")
print(newdata)


# 문제1 : 다음 데이터를 데이터 프레임에 입력하시오
#        영어점수        등급
#퀴즈      67             "C"
#중간      92             "A"
#기말      89             "B"

score <- data.frame("영어점수"=c(67, 92, 89),"등급"=c("C","A","B"))
rownames(score) <- c("퀴즈","중간","기말")
score

mat<-data.frame("수학"=c(50,100,80))
score <- cbind(score,mat)
score


extra<- matrix(nrow=1, ncol=3)
extra

rbind(score, extra)
???????????????????????????????????


score[3,1]<- 88
score

#문제6 : 데이터를 저장하시오
write.csv(score, "output_test.csv")


#문제7 : 저장한 데이터를 로딩하시오.
newdata<- read.csv("output_test.csv")
newdata




#문제1 : 다음 데이터를 데이터 프레임에 입력하시오
#        영어점수        등급
#퀴즈      67             "C"
#중간      92             "A"
#기말      89             "B"

x<- c(67,92,89)
y<- c("C","A","B")
name<-c("퀴즈","중간","기말")
data<-data.frame("영어"=x, "등급"=y, row.names=name)
data
colnames(data)<-c("영어점수","등급")


#문제2 : 수학점수 50, 100, 80 점으로 입력하시오

data$"수학점수"<-c(50,100,80)
data


#문제3 : 보충이라는 이름으로 dataframe을 만들어 rbind 시키시오
da<-data.frame("영어점수"=c(90), "등급"=c("A"),"수학점수"=c(100), row.names="보충")
da
(data2<-rbind(data,da))


#문제4 : 일별합계를 "합계"라는 이름으로 추가하시오.
hab<-colSums(as.matrix(cbind(data2$"영어점수", data2$"수학점수")))
hab

da=data.frame("영어점수"=hab[1], "등급"=NA, "수학점수"=hab[2], row.names="합계")
da
(data2=rbind(data2, da))


#문제5 : 영어 기말 점수를 88점으로 수정하시오.
(data2[3,1]<-88)
data2


app=c(50,"C", 50)
app

data2=rbind(data2,app)
data2


length(rownames(data2))
rownames(data2)<-c("퀴즈","중간","기말","보충","합계","추가연습")
rownames(data2)[6]<-"추가연습2"
data2

kor<-c(90,80,100,77)
(kor<-c(kor,sum(kor),100))
data2<-cbind(data2, kor)
data2
#데이터 형태중 사각형 형태로
write.table(data2, file="sungjuk.csv", sep=",", col.names = NA) #분리자,
#첫줄 때문에, 헤더처리를 함.
(data3 <- read.table("sungjuk.csv", header= TRUE, sep="," , row.names = 1))
plot(c(data3$kor), main="그래픽", col="red", ylab="국어점수", ylim=c(0,100), type="b") #both(타입both= 포인트와 라인 모두 주라.)
hist(as.numeric(data3$kor[1:4]), main="국어점수", xlab="점수", ylab="도수", col="lightblue", breaks=5)
plot(c(data3$수학점수),main="그래픽",col="red",ylab="수학점수",ylim=c(0,100),type="b")

mean(data3$kor[1:4])#산술평균
sd(data3$kor[1:4]) #표준편차
median(data3$kor[1:4]) #중위수
var(data3$kor[1:4])#분산

#분산요구하는 곳도 있고 표준편차 요구하는 곳도 있기 때문에,모두 알아야 합니다.






# array- 행, 열, 면
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

column.names <- c("COL1", "COL2", "COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1", "Matrix2", "m3", "m4", "m5")
#                                                       (행,열,도면)
result <- array(c(1:9, 21:29, 31:39, 41:49, 51:59), dim = c(3,3,5), dimnames = list(row.names, column.names, matrix.names))

#result2 <- array(c(1:16, 21:36, 31:46), dim=c(2,8,3))
#result2

print(result)
print(result[3,,2])

#문제 - 추상 후 확인 [행,열,면] #나중에 데이터가 3차원으로 쌓이기 때문에, "얼마가 맞을 것이다 라고 추상하는 게 필요"
#2번에 있는 데이터를 모두 출력하시오
print(result[,,2])
#2번 매트릭스의 3행 2열에 있는 데이터를 추상해보고 확인하시오
print(result[3,2,2])
#1번 매트릭스의 2행 3열에 있는 데이터를 추상해보고 확인하시오
print(result[2,3,1])
#2번 매트릭스의 1행 3열에 있는 데이터를 추상해보고 확인하시오
print(result[1,3,2])
#3번 매트릭스의 2행 2열에 있는 데이터를 추상해보고 확인하시오
#3번 매트릭스의 2행 3열에 있는 데이터를 추상해보고 확인하시오
#4번 매트릭스의 3행 2열에 있는 데이터를 추상해보고 확인하시오
#5번 매트릭스의 3행 1열에 있는 데이터를 추상해보고 확인하시오


# data.frame
library(data.table) #키이개념 : 정렬(index사용가능)
install.packages("data.table")
titanic <- read.csv("titanic3.csv", fileEncoding = "UTF-8")
class(titanic)

titanic.dt <- data.table(titanic) #메모리를 조금 더 사용
class(titanic.dt)
str(titanic.dt)
head(titanic.dt)

# pclass !!

setkey(titanic.dt$pclass)
table() #현재 메모리에 있는 테이블 확인
setkeyv(titanic.dt, c("sex","pclass")) #여러키를 주고자 할 때 사용
tables()
titanic.dt[pclass==1,]
titanic.dt[class==2]
#좌석별 생존률을 구하시오

#(키를 달 수 있고, 쿼리를 달 수 있다 !!)

titanic.dt[, mean(servived), by="pclass"]
titanic.dt[, length(which(pclass=="1"))]
#남녀간의 생존확률
titanic.dt[,mean(survived),by=sex]
#1등급 클래스의 성비
titanic.dt[pclass=="1", .N, by="sex"]



#제어문
num = 5
if (num == 5){
  cat('출력 5')
}
if (num == 5){
  cat('숫자가 5임')
} else if (num == 7){
  cat('숫자가 7임')
} else{
  cat('해당하는 것이 없음')
}

mydat <- sample(1:4, 1000, rep=TRUE, prob=c(.2,.3,.2,.3))
#                                           20% 30% 20% 30% 가중치 부여 가느
table(mydat)

#분포별 랜덤수를 결정(의사난수)
N<- 100
qnorm(runif(N)) #runif(=random) 균등분포 qnorm은 정규분포의 분위수를 확인해 줌.

#정규분포의 x축은 표준편차를 나타냄.






name <- c("일번","이번","삼번","사번","오번","육번","칠번","팔번","구번","십번")
(L3 =c("M", "F"))
gender <- sample(L3, 10, replace=TRUE)
price <- floor(runif(10, min=50, max=100))
customer <- data.frame(name, gender, price)

(result = ifelse(customer$price > 65, "Good","Bad")) #3항 연산자
(customer$result<- result)
table(result)
(gender2 <- ifelse (customer$gender =="M", "Male",
          ifelse(customer$gender=="F", "Female", customer$gender)) )
(customer$gender2 <- gender2)
table(customer$gender2)



#for문

for (n in i){
  if(n%%2==0){
    next#continue와 같음.
  }else{
    print(n)
  }
}


i = 0
while(i<10){
  i <- i+1
  print(i)
}

i = 0
repeat(i<10){
  i<- i+1
  return(i)
}


# 평균 구하기
lifetime = c(15, 3, 46, 623, 126, 64)
total <- 0
for (i in 1:length(lifetime)) {
  total<-total+lifetime[i]
}

mean<- total/length(lifetime)
print(mean)

total<- sum(lifetime)
mean<- total/length(lifetime)
print(mean)


# foreach 빠른 반복문
install.packages("foreach") #병렬처리 ( CPU의 multicore, GPU가 병렬처리 인데, 그 중 멀티코아를 지원함.)
library(foreach)
foreach(i=1:5) %do% {
  return(i)
}

foreach( i=1:5, .combine=c) %do% {
  return(i)
}
foreach( i=1:5, .combine="+") %do% {
  return(i)
}
#이렇게 쓴다고 이해만 하면 된다.



#도수분포표
coin <- c(2,2,0,1,1,1,2,3,1,2,2,3,3,4,1,3,2,3,3,1,2,2,1,2,2)
frequency <- table(coin) #도수분포표
table(coin)/length(coin)#상대도수분포표
#table 속을 이해하는 부분
coin[coin==1] #1인 것만 나옴
length(coin[coin==1]) # 몇개인지 셈.
frequency<-rep(0,5) #저장공간만들어 놓고.
frequency
for(i in 1:5) {
  frequency[i] <- length(coin[coin=i-1])
}



frequency
(relative = frequency/length(coin)) #상대도수분포표
round(relative, 2)
cumulative = cumsum(relative) #누적 도수 분포표
round(cumulative,2)
(result=data.frame("도수"=frequency,"상대도수"=round(relative,2), "누적상대도수"=round(cumulative,2)))
rownames(result) <- c(0,1,2,3,4)
result


plot(result$"도수", type="b", col="red", main="도수분포표", frame.plot=F)
plot(result$"상대도수", type="b", col="red", main="도수분포표", frame.plot=F)
plot(result$"누적상대도수", type="b", col="red", main="도수분포표", frame.plot=F, panel.first=F)



# 함수
pow <- function(x,y){    #명시적으로 리턴값이 필요 없음. 명시적으로 리턴하지 않아도 마지막 계산한 값이 리턴값.
  result <- x^y
  print(paste(x,"의", y,"승은 ", result))
}
pow(8,2)
pow(x = 8, y= 2)
pow(y = 2, x= 8)
pow(x= 8, 2)
pow(2, x=8)
a = pow(2, x=8)
print(a)


pow <- function(x,y){    #명시적으로 리턴값이 필요 없음. 명시적으로 리턴하지 않아도 마지막 계산한 값이 리턴값.
  result <- x^y
}
a=pow(3,2)
a
#프로그래밍을 위한 언어가 아니고, 분석을 위한 언어이다.



multi_return <- function() {
  my_list <- list("color" = "red", "size" = 20, "shape" = "round")
  return(my_list)
}
a <- multi_return()
a$color
a$size



#문제 유클라디안 거리를 구하는 함수를 작성하시오. (유클라디안 거리를 내라 = 대각선 길이를 구하라) #call by reference로 받음.

ukl <- function(x,y){    #명시적으로 리턴값이 필요 없음. 명시적으로 리턴하지 않아도 마지막 계산한 값이 리턴값.
  ukl_result <- sqrt( x*x + y*y) #python 데이터를 주어야 초기화 : 변수타입이 없음.
  print(ukl_result)
}

ukl(3,4)

# 정답
square <- function(data){
  return (data^2)
}
point <- function(xval, yval){
  return (c(x=xval, y=yval))
}
p1 <- point(5,6)
p2 <- point(2,3)
p1
p2

euclidean.distance <- function(point1, point2, square.func){ #함수도 객체이기 때문에 매개변수로 전달가능하다.
  distance <- sqrt(
    as.integer(
      square.func(point1['x'] - point2['x'])
    ) +
      as.integer(
        square.func(point1['y'] - point2['y'])
      )
  )
  return (c(distance=distance))
}

euclidean.distance(point1 = p1, point2 = p2, square.func = square)
euclidean.distance(point1 = p2, point2 = p1, square.func = square)
euclidean.distance(point1 = point(10, 3), point2 = point(-4, 8), square.func = square)



#문제)  성적 프로그램을 작성하시오 (DataFrame을 사용)
name = c("소중한", "아임을", "사랑해")
kor = c(100, 90, 80)
eng = c(100, 98, 88)
mat = c(100, 97, 83)

data <- data.frame("국어"=kor, "영어"=eng, "수학"=mat, row.names=name)
data


#문제 1 ) 합계와 평균을 구하시오(for 문을 사용하시오)

total <- 0
for (i in 1:length(kor)) {
  total<-total+kor[i]
}

mean<- total/length(kor)
print(mean)


#문제 2 ) 합계를 내는 함수를 작성해서 이용하시오(colMeans, colsums)
#문제 3 ) 평균이 80점 이상이면 우수 아니면 보통으로 변수를 생성해서 추가하시오 (if문 사용)
#문제 4 ) 계산된 결과를 csv파일로 저ㅓ장하고 로딩해보시오
#문제 5 ) 국어 점수의 분포를 그래프로 출력하시오
#문제 6 ) 국어와 수학의 상관계수. (var,)
#문제 7 ) 국어와 수학 그리고 영어의 상관도.


# 정답
sungjuk = data.frame(name=name, kor=kor, mat=mat, eng=eng, stringsAsFactors=FALSE)
str(sugjuk)
sung_total <- function(kor, mat, eng){
  total <- kor+mat+eng
}
for (i in 1:NROW(sungjuk)){
  sungjuk$total[i] = sung_total(sungjuk$kor[i], sungjuk$mat[i], sungjuk$eng[i])
  sungjuk$average[i] = round(sungjuk$total[i]/3, 2)
  if (sungjuk$average[i] >= 80) {
    sungjuk$rank[i] = "우수"
  } else {
    sungjuk$rank[i] = "보통"}
}
sungjuk[4,]= c("합계", sum(sungjuk$kor), sum(sungjuk$mat), sum(sungjuk$eng), NA, NA, NA)
sungjuk
write.table(sungjuk, file = "sungjuk_last.csv", sep = "," , col.names = NA)
sungjuk_load <- read.table("sungjuk_last.csv", header = TRUE, sep = ",")
sungjuk_load
plot(sungjuk_load$kor, main = "국어점수 분포", col="red", ylab="국어점수", ylim=c(0,100), type="b")
cor(sungjuk_load$kor, sungjuk_load$mat) #상관계수
cov(sungjuk_load$kor, sungjuk_load$mat) #공분산
plot(sungjuk_load[,3:5])

#이건 모두 파이썬에서 해봤던 것들.. 조금 더 간단히 해보겠습니다!


###벡터화 연산
sungjuk = data.frame(name=name, kor=kor, mat=mat, eng=eng, stringsAsFactors = FALSE)
str(sungjuk)
(mat <- sungjuk[c("kor","eng","mat")])    #동질적 데이터 : matrix, rowSums, colSums, colMeans
csum <- rowSums(mat)
rsum <- colSums(mat)
cmean<- rowMeans(mat)


(sungjuk$total <- csum)
(sungjuk$average <- round(csum / 3,2))
(rank <- ifelse( sungjuk$average >= 80, "우수", "보통"))
(sungjuk$rank <- rank)


#   within 는 데이터 변경할 때   ##whith 데이터 변경 없을 때

data(airquality)
str(airquality)
aq <- within(airquality, { #R은 객체, 포인터, 실시간 추가, 삭제(변수,함수)
  lozone <- log(Ozone)     #로그변화는 정규분포가 아닌걸, 정규분포로 만들고자 할 때 사용함.
  Month <- factor(month.abb[Month])    #abb 줄임말. factor화 시킴.
  cTemp <- round((Temp -32) * 5/9, 1)
  S.cT <- Solar.R / cTemp
  rm(Day, Temp)    #변수 제거
})
head(aq)


x<- data.frame(name=c("a","b","c"),math=c(1,2,3))
y<- data.frame(name=c("a","b","c"),english=c(4,5,6))
merge(x,y)    #inner join 으로 있는 것만 나옴.
merge(x,y,all=TRUE)    #outer join

merge(x,y,all.x=TRUE)    #left outer join
merge(x,y,all.y=TRUE)     #right outer join


search() #메모리내 로딩된 패키지 확인
data(iris)
str(iris)
summary(iris)    #네가지 정보를 보면, 3가지 종류 중 어느 종류에 속하는 지 알 수 있다.

iris
#변수 5개/ 관측치 150개

head(iris,3)
tail(iris,3)
dim(iris)
length(iris)
NROW(iris)
names(iris)
class(iris)
sapply(iris,class)  # sapply는 변수마다 적용하라는 이야기.



# 리스트 도 $로 접근한다.

a = (split(iris, iris$Species)) #factor 형. factor가 기준점이 된다.
a
class(a)
a$versicolor
class(a$versicolor)  #데이터 형태를 유지
a$versicolor$Sepal.Length
class(a$versicolor$Sepal.Length)

lapply(split(iris$Sepal.Length, iris$Species), mean)



# 파생변수
str(airquality)
plot(airquality$Ozone, airquality$wind)
(abc=transform(airquality, airquality$Ozone<- -airquality$Ozone))
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)

# attach
search()
attach(airquality)
transform(Ozone, logOzone = log(Ozone))     #위에 airquality를 붙이고 사용했던것과 달리, 여기는 airquality안붙이고 사용.
search()
detach(airquality)
search()
ls()


(tf_0_1 <- (max(mtcars$mpg) - mtcars$mpg) / (max(mtcars$mpg) - min(mtcars$mpg)))
(tf_0_1_2 <- with(mtcars, (max(mpg)- mpg) / (max(mpg)- min(mpg))))

#subset 필터링.

subset(iris, subset=(Species=="setosa"))
subset(iris, subset=(Species=="setosa" & Sepal.Length>5.0))
subset(iris, select = c(Sepal.Length, Species))
subset(iris, select = -c(Sepal.Length, Species), subset=(Species=="setosa" & Sepal.Length>5.0))
iris[, !names(iris) %in% c("Sepal.Length", "Species")]


#문제 ) str(mtcars) 데이터에서 다음을 처리하시오

#실린더가 4~6개인 자동차들 중 연비, 실린더 수, 변속기 종류를 출력하시오.
subset(mtcars, subset=(cyl>=4.0 & 6.0>=cyl), select=c(mpg, cyl, vs, am))
#답: mtcars[which(mtcars$cyl == c(4,6)), c("mpg","cyl","am")]
#답: subset(mtcars,select=c(mpg,cyl,am),subset=(cyl==c(4,6)))
#답: subset(mtcars,select=c(mpg,cyl,am), subset=(cyl %in% c(4,5,6)))


#자동변속기인 자동차를 선택.
subset(mtcars, subset=(vs==1))
#답: subset(mtcars, select=c(mpg,cyl,am), subset=(am==1))


#연비(mpg)가 20보다 큰 자동차 선택
subset(mtcars, subset=(mpg>20))
#답: subset(mtcars, select=c(mpg,cyl,am),subset=(mpg>20))


#평균 마력(mean(mtcars$hp)) 이상인 자동차들을 출력.
subset(mtcars,subset=(hp>(mean(mtcars$hp))))
#답: subset(mtcars, select = c(mpg,cyl,am,hp),subset=(hp>=mean(mtcars$hp)))


str(mtcars)
# mpg연비, cyl실린더수, hp마력, drat, wt(weight), qsec가속능력, vs변속기, am변속기, gear자동/기, carb흡입력



# reshape2 (melting / dcast ) : pivoting =축을 바꿔줘서 자유자재로 쓰는것.
install.packages("reshape2")
library(reshape2)
names(airquality) <- tolower(names(airquality))
class(airquality)
head(airquality)
summary(airquality)
str(airquality)
nrow(airquality)

str(airquality) #wind, temp, ozone, solar.. 모두 녹아서 들어왔음.
(aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)) #기준점을 month와 day로 만들고..
head(aqm, 30)


acast(aqm, day ~ month ~ variable) #각 변수에 월별 데이터. R은 열로 데이터가 들어옴. 하나하나 확인이 안되니까. melt시켜서 확인함.
#day를 행으로 month를 열로.
           #행      열         값
acast(aqm, month ~ variable , mean) # ,와 ~의 버전별 차이가 있음.


install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

head(diamonds)
(df.diamonds_ideal <- filter(diamonds, cut=="Ideal")) #컷이 Ideal 것만 필터링.
#변수 받고, 변수 집어넣고.. 변수받고, 집어넣고...
head(df.diamonds_ideal)

(df.diamonds_ideal <- select(df.diamonds_ideal, carat, cut, color, price, clarity))
head(df.diamonds_ideal)

(df.diamonds_ideal <- mutate(df.diamonds_ideal, price_per_carat = price/carat))   #mutate 데이터 파생변수. 변수추가.
head(df.diamonds_ideal)



df.diamonds_ideal_chained <- diamonds %>%     #다이아몬드에 들어있는데이터를 파이프로 넘겼음. 아래로 자동으로 넘어감.
  filter(cut=="Ideal") %>%
  select(carat, cut, color, price, clarity) %>%
  mutate(price_per_carat = price/carat)

(df.disordered_data <- data.frame(num_var = c(2,3,5,1,4)))
head(df.disordered_data)
arrange(df.disordered_data, num_var) #sort
arrange(df.disordered_data, desc(num_var)) #거꾸로 하고 싶다면.
summarize(df.diamonds_ideal, avg_price = mean(price, na.rm=TRUE))#다이아몬드 Avr가 mean값과 같은것.


diamonds %>%
  filter(cut == "Ideal") %>%
  ggplot(aes(x=color, y=price)) +     ##ggplot은 레이어. #aes는 ggplot에서 쓰는 문법, 어떤 형상을 쓸것인가.
  geom_boxplot()
#boxplot 사용하는이유 => 이상치를 찾기 위해서.


diamonds %>%
  filter(cut =="Ideal") %>%
  ggplot(aes(price))+
  geom_histogram()+
  facet_wrap(~color) #다차원 표현이 가능하다. - ggplot
