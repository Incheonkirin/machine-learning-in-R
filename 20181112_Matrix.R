Sys.getenv("JAVA_HOME")
Sys.getlocale() #LC_COLLATE=Korean_Korea.949 윈도우용

options(encoding = 'UTF-8')
getOption("encoding")
Sys.setenv(LANG = "ko_KR.UTF-8")
ls()

#포인터
x <- 5
y <- x
y


(x <- 7)
y <- 16
x+y
x-y
y/x
y%/%x #연산자 [%% 사용자정의 연산자][몫연산자]
y%%x #나머지 연산자
y^x #제곱 연산자

#관계연산자 TRUE/ FALSE
x <- 5
y <- 16
x<y
x==5
y>= 20
y == 16
x != 5


x <- c(TRUE,FALSE,0,6) # combination : vector
y <- c(FALSE, TRUE, FALSE, TRUE)
x
y
!x #for문을 쓰지 않아도, 모두 계산해줌.

x&y #[다른언어에서는 비트연산자.] 논리연산자인 and와 같다. #목표가 빅데이터 처리로, 비트연산자 없음.
x|y  # or
x&&y #첫번째 요소간의 and
x||y

v <- 2:8 #series #마지막도 포함해서.
print(v)
v1 <- 8
v2 <- 12
t <- 1:10
print(v1 %in% t) #범위연산자
print(v2 %in% t)


num <- 2
num <- num ^2 * 5 + 10/3 #연산의 우선순위
num

c(1,3,5,7,9)* 2 #요소에 곱
c(1,3,5,7,9,10) * c(2, 4) #recycling 배수가 되어야 오류가 안남.
c(2, 4) * c(1, 3, 5, 7, 9, 10)

x <-10  #값 하나는,scalar !

#매개변수 => vector를 집어 넣을 수 있다. ( 1개를 계산하는게 목적이 아님)
#모든 데이터가 vector다 !
factorial(1:5)  # for => vector화 연산.
exp(2:10) #지수분포.

cos(c(0, pi/4))
sqrt(c(1, 4, 9, 16)) #루트
sum(1:10)

#   NaN : 수가 정의 되지 않음. Inf: Infinite . NA : 결측치
# R에서는 결측치가 하나라도있으면, 그 결과는 결측치.
#결측치 제거가 중요하다 :삭제, 0, 평균 => 결과상이.
1 / 0
0 / 0
Inf / NaN
Inf / Inf
log(Inf)
Inf + NA이


# 선형대수 내적, 외적. (R에서는 외적이 없다. 외적 값=>벡터.)
x <- c(5,6,7)
y <- c(1,2,3)

x%%y #나머지
x%*%y  #내적 [각 요소별로 곱해서 더한 값 ==> 숫자한개 => 사이각]
x%/%y # 요소 나누기
x%o%y  #outer곱
x%x%y # outer곱을 개별 데이터로 출력
x%in%y #범위

#타입 확인 함수
vec <- c(0, Inf, NaN, NA)
is.finite(vec)
is.nan(vec)
is.na(vec) #NaN도 NA로 확인.
is.infinite(vec)
sum(vec)
vec <- c(0, 10, 20, NA)
sum(vec)# NA가 하나라도 있으면, sum 값이 NA이 됨.

#사용자 정의 연산자.
`%divisible%`  <- function(x,y)
{
  if (x%%y == 0) return (TRUE) #   %% 나머지 연산자
  else           return (FALSE)
}
10 %divisible% 3
10 %divisible% 2
`%divisible%`(10,5)


getwd() #내가 읽어들일 파일 열 기준으로 읽어야 함.
setwd("C:/User...") # 여기를 내 작업 디렉토리로 만들겠다. 다음에 저장하면 여기 저장됨.
?getwd #모르는 것 있을 때는 '?' 를 앞에 붙이고 , 아니면 help함수를 앞에 사용.
help(getwd)
help(options)

options(digits=3) #3자리까지 표현하겠다.
View(x) #view함수를 사용하면, 안에 값이 나옴.

#내장 상수.
LETTERS
letters
month.abb
month.name
pi


int <- 100
string <- '대한민국'
boolean <- TRUE


#is함수와 mode 함수를 통해서 데이터 타입 확인 가능함.
mode(int);  mode(string);  mode(boolean)
is.numeric(int)
is.character(string)
is.logical(boolean)

score <- c(85, 95, NA, 75, 65)
score
mean(score, na.rm = TRUE)  #NA를 제외하고 계산.
sum(score, na.rm = T)  #TRUE라고 쓸 수 있고, T라고 쓸 수 있음.

x <- c(10, 20, 30, '40') # vector 는 동질적
mode(x)
(xx <- as.numeric(x))
#문자열이 더 높은 급으로, 문자열로 인식. as numeric을 통해서 데이터 타입을 변환해주어야 한다.
# '40'이 아니라, 문자가 들어가면, as numeric으로 해도 숫자로 안바뀜.

#factor는 mapping 된 결과.
gender <- c('M','F','M','F','M')
gender
# plot(gender) #문자열
fgender<- as.factor(gender)
fgender
fgender[2]
mode(fgender); class(fgender)
plot(fgender)


Sys.Date()
Sys.time()

today <- '2017-03-11 10:15:24' #입력은 문자열
today
mode(today)
class(today) #클래스 계층구조
today2 <- as.Date(today)
today2
mode(today2); class(today2)

ctoday <- strptime(today, '%Y-%m-%d %H:%M:%S')
ctoday
class(ctoday)

data()
Nile #나일강 강우량 데이터
length(Nile)
plot(Nile)

data(AirPassengers)
ls()

# 가끔 메모리에서 데이터 제거해야함. 전처리하고, 원본 제거해야함.
# rm(list=ls()) 하면 다 없어짐
rm(list=ls())
ls()

x<- 1:10
x
y<- 100:200
y
ls()

# vector : 1차원 배열[프로그래밍측면], 선형대수적으로 vector. 크기와 방향을 저장.
#  그 중에 가장 중요한게, '내적' 내적은 요소간의 곱을 다 더한것.
# 선형으로 문제를 해결한다.
#  2차원 3차원 축이 있다. 축간의 내적은 0 : 직교함 => 데이터간에 독립.
# 실제 데이터들의 축은 직교하지 않는다. "주성분분석"을 통해서 직교하는 축으로 데이터 변환.
# 주성분분석을 한다 => 데이터 추출을 한다.
# 데이터 분석 = 특성 추출


# 경영학적으로 내적은 매출액.
x <- c(10,20,30,40)
y <- c(40,50,20,30)
x%*%y


# 벡터가 나왔다 하면, 유사도와 디스턴스가 나온다.
# 유사도와 유클라디안 거리 를 재서, 데이터를 분석한다.

# 벡터는 ,데이터가 독립적이어야 한다.





a <- c('apple','orange','banana')
length(a)
NROW(a)

x<- c(1,5,4,9,0)
class(x)

typeof(x)
length(x)




# sequence, repeat
seq(1, 3.2, by=0.2) #간격
seq(1, 5, length.out=4) #갯수
seq(1, 6, length.out=4)
(c <- seq(1, 9, by=2))

(b<- rep(1:4, 2)) #2번 반복.
(d<- rep(1:3, each=3)) #each는 각각



# indexing
x<-c(1,2,3,4,5,6,7,8,9) #인덱스 0아닌 1부터 시작함.
x
x[3]
x[c(2, 4)] #index vector    # x[2, 4]라고 쓰는 것과는 다름.

x[-1] #제외하고
#c(2, -4) #같이 사용이 불가능함.
x[c(2.4, 3.54)] #소수점을 주면, 정수화해서 사용함.
x[c(TRUE, FALSE, FALSE, TRUE)] #boolean indexing : recycling 짝이 안맞아도 되나, 연산에서는 짝이(배수로) 꼭 맞아야함.
x[x < 3] #boolean indexing     #boolean연산 => 이상치 제거에 사용가능함.
x[x > 3] #boolean indexing

(x <- c("first"=3, "second"=0, "third"=9)) #열 이름 인덱싱
x["second"]
x[c("first", "third")]
x


# 값을 대입할 때도 Indexing으로 대입.
x = c(-3, -2, -1, 0, 1, 2)
x[2] <- 0;
x
(x[x<0] <- 5 )
x
(x<- x[1:4] )

x<- NULL # instance가 안되었다는 의미임. => (변수에 값이 없다)즉 포인터가 가르키는 곳이 없다는 뜻이다.
x
x[4]

x <- 1:5
all(x>2)
any(x>2)
identical(c(1,2,3),c(1,2,3)) #일치하느냐

x <- c(3,2,6,4,5,8,1)
x
sort(x) #외부함수  #하나의 열을 처리할때
sort(x, decreasing=TRUE) #함수에는 디폴트 매개변수

order(x) #순서를 줌  #이렇게하는 이유는? 하나의 데이터 정렬할때는 위와같이하면됨. 여러개의 열이 있을때는 어떤 것을 기준으로 정렬하고 싶다는 것이지.
order(x, decreasing=TRUE) #기준열을 중심으로하고 다른 데이터가 존재할때.
x[order(x)]

x = c(2, NA, 3, 1, 4) #NA가 하나라도 있으면 안된다. 그래서 R에서는 결측치 처리가 중요하다.
sum(x)
sum(x, na.rm=TRUE)
mean(x, na.rm=TRUE)  #산술평균
median(x, na.rm=TRUE)#위치적 중앙값을 평균으로 함(정렬) #학교에서는 mean많이 쓰지만, 통계에서는 median많이 씀.  :: 왜? 이상치를 제거하기 위해서. 이상치가 통계학에서 많은 영향을 미치기 때문에, mean보다 median을 사용하게 됨.
prod(x, na.rm=TRUE)

# 변수에 이름 적음.
vectorA <- c(1, 2, 3) #이미 들어간 데이터를 구분하기위해
names(vectorA) <- c('aaa','bbb','ccc') # 변수이름을 줌. names를 사용함.

vectorA['aaa']
vectorA[2]
vectorA[-1]    #여기서 - 의 의미는 제외하고 라는 의미.
vectorA[c(1,3)]
vectorA[vectorA > 5]
vectorA[c(FALSE, FALSE, TRUE)]
append(vectorA, c(3,4,5))
vectorA
(vectorB = append(vectorA, c(3,4,5)))


#집합연산
union(vectorA, vectorB)#합집합
intersect(vectorA, vectorB)#교집합
setdiff(vectorA, vectorB)#차집합 앞에서 뒤를 뺀다.
setdiff(vectorB, vectorA)#차집합 앞에서 뒤를 뺀다.
setequal(vectorA, vectorB)#같은걸 확인


# 원하는데이터만 골라내기 위해서, subset
(x <- c(3,2,6,4,5,8,1))
subset(x, x>3) #데이터
which(x*x > 8) #위치값


# 문제 vectorA에서 2보다 큰 값만 모으시오
subset(vectorA, vectorA>2)


# vectorA에서 요소를 제곱했을 때, 8보다 큰 위치값을 출력하시오.
which(vectorA * vectorA > 8)


nums <- c(5, 8, 10, NA, 3, 11)
nums
which.min(nums)
which.max(nums)
nums[which.min(nums)]
nums[which.max(nums)]


# 문제
#vector1 벡터 변수를 만들고, "R" 문자가 5회 반복되도록 하시오.
vector1<- rep("R", 5)
vector1
#vector2 벡터 변수에 1~20까지 3간격으로 연속된 정수를 만드시오.
vector2 <- seq(1, 20, by=3)
vector2
#vector3에는 1~10 까지 3간격으로 연속된 정수가 3회 반복되도록 만드시오.
vector3 <- rep(seq(1, 10, by=3),3)
vector3
#vector4에는 vector2~vector3가 모두 포함되는 벡터를 만드시오--append
vector4 <- append(vector2, vector3)
vector4
#25~15까지 5간격으로 벡터 생성 - seq()함수 이용
vector <- seq(25, 15, by=-5)
vector
#vector4에서 홀수번째 값들만 선택하여 vector5에 할당하시오(첨자이용)
vector5 <- vector4[1:length(vector4) %%2 == 1]
vector5 <- vector[c(TRUE,FALSE)]

vector5

#vector5에서 짝수번째 값들만 선택하여 vector6에 할당하시오(첨자이용)
vector6 <- vector5[1:length(vector5) %%2 == 0]
vector6 <- vector[c(FALSE, TRUE)]

vector6


# vector5의 데이터를 문자로 변경하여 vector6에 할당하시오.--as.character

#vector5와 vector6을 벡터 연산하여 더하시오. -- as.numeric



# Matrix(행렬)
  # 축과 요소
  # 변환을 하기 위해서 사용 === 벡터를 변환함.
                                    #   (sin(theta), -cos(theta))
                                    #   (cos(theta), sin(theta))   행렬을 만들어서 벡터를 곱하면, 회전을 함. MRS( Move, rotate, scale)
                                                                  # 동차 좌표계에 존재 해야 함.
  # 벡터 %*% 행렬==MRS, 행렬 %*% 행렬==변환의 합.
  # 데이터 분석 : 차원감소( 시각화) , 특징추출
  # 정방행렬, 대칭행렬, 영행렬, 대각행렬, 역행렬, 고유값분해, 특행렬분해.(이해 사용)
    #정방행렬: 정사각형행렬.
    #대칭행렬: 대각선을 중심으로하고, 데이터가 같은 행렬.

(a = matrix(1:9, nrow = 3, ncol = 3))    #byrow= FALSE 열로 먼저 돌아감.디폴트값이 FALSE.
# 앞뒤에 괄호를 붙이면 입력과 동시에 출력됨.
matrix(1:9, nrow = 3)
class(a)
attributes(a) #class의 속성을 확인함.
dim(a)

# 행렬은 변환이다. 변환- 공간을 맵핑해서 고차원을 저차원으로. 시각화하거나 특징을 뽑을때 사용한다.
# 매트릭스는 동질적인 요소로 구성된다.

# 축이름 지정 column(열)row(행) #열이름 지정하는것 == names.
x = matrix(1:9, nrow=3, byrow=TRUE) #옆으로 돌아감.
x
colnames(x)
colnames(x) <- c("C1", "C2", "c3")
rownames(x) <- c("R1", "R2", "R3")

x


# 이렇게도 사용 가능하다.
x = matrix(1:9, nrow=3, byrow=TRUE, dimnames = list(c("X","Y","Z"), c("A","B","C"))) # 짝이 안맞을때는 리스트를 사용한다.
colnames(x)
colnames(x) <- c("C1", "C2", "c3")
rownames(x) <- c("R1", "R2", "R3")
x

?matrix
# ncol은 디폴트 1로 되어있음



# 사이즈가 다른 것이 들어왔을때..

(x <- matrix(1:12, nrow = 3, ncol=4, dimnames =
              list(c("X","Y","Z"), c("A","B","C","D"))))
colnames


# 깡통행렬을 만들고, [행과 열]로 인덱싱
y <- matrix(nrow=2, ncol=2)
y[1,2]
y[1,1]<-1
y[2,1]<-2
y[1,2]<-3
y[2,2]<-4
mode(y)
class(y)
y

# cbind/ rbind
(x= matrix(1:9, nrow=3, byrow=TRUE))
(y= matrix(11:19, nrow=3, byrow=TRUE))
(c=cbind(x,y)) #컬럼으로, 옆으로 붙임. #3x6
(r=rbind(x,y)) # 로 으로, 아래로 붙임. #6x3
#함수는 원래의 값을 변경하지 않음.
x



##행렬인덱싱
# 1 2 3
# 4 5 6
# 7 8 9
(x = matrix(1:9, nrow=3, byrow=TRUE))
x[c(1,2),c(2,3)] #교차점이 나온다.
x[c(2,3),] #아무것도 없으면, 전부다 라는 의미다.
x[,]
x[-1,]#첫번째 행만 제거하고, 나머지는 다 라는 이야기다.
a=x[1,]#첫번째 행과 나머지 다.
a
class(a)
mode(a)
(a=x[1,,drop=FALSE]) #drop=FALSE, 요소가 아닌, 매트릭스로 표현하기위함. 특성을 없애버리지 말라고 한 것이다.
mode(a)
class(a)



#행렬인덱싱
# 1 2 3
# 4 5 6
# 7 8 9

#이차원 불린 인덱. 2차원에다가 1차원인덱싱 사용함.=> 데이터를 1차원으로 메모리 놓고.그대로 인덱싱하면 됨.
x[c(T,F,T),c(T,T,F)]
x[c(T,F),c(2,3)] #recycling
x[c(T,F)]
x[1:4]   #열로 내려감. 디폴트 byrow = FALSE
x[c(3,5,7)]



####
# 1 4 7
# 2 5 8
# 3 6 9
####
# 0 0 7
# 0 10 8
# 0 6 9

## 전치행렬 ( 행과 열을 변경) + 행렬곱(변환)은 전치행렬이 필요
############################### 행렬곱은 변환을 위해서 한다 ####
# 0 0 0
# 0 10 6
# 7 8 9
x = matrix(1:9, nrow = 3, ncol = 3)
x
(x[2,2] <- 10)
(x[x<5] <- 0)
print(x)
t(x)
cbind(x, c(1, 2, 3))
rbind(x, c(1, 2, 3))
x <- x[1:2,]




(x = matrix(1:12, nrow = 3, ncol = 4))
x %*% x          # (행,열) (행,열) = 행렬곱의 규칙은 앞에있는 열 과 뒤에있는 행 수가 일치 되어야 함.
                 #  3  4    3   4  ==> 뒤에 것을 *전치* 시킨다.
                 #  3  4    4   3 ==> 결과는 앞의 행수와 뒤의 열수로 결과가 나온다. (3x3)

x%*% t(x) #변환의 합.

####전치행렬은 행과 열을 바꾸는 것이고, 하는 이유는 행렬내적을 위해서이다.



(mdat <- matrix( seq(20,4, -2), nrow=3, ncol=3, byrow=TRUE,
                 dimnames=list(c("a","b","c"), c("x","y","z"))))

t(mdat)    #전치행렬
nrow(mdat)    #행 수
ncol(mdat)    #컬럼수
dim(mdat)    #차원 수

rowMeans(mdat)    #행의 평균값
rowSums(mdat)     #행의 합
colMeans(mdat)    #열의 평균값
colSums(mdat)
diag(mdat) #대각행 == 나중에 '평가'할때 대각에 있는 것이 맞는 것들이다.
#한 축에는 예측값, 한 축에는 실제값.



#연립방정식의 해를 구하시오
# y= 10x y=20일때
#solve(10,20) # => 2
#solve(20,10) # => 0.5

#2X + 3Y = 5,
#3X + 5Y = 6    => 갯수들을 행렬로 만든다.
(mdat <- matrix(c(2,4,3,5), nrow=2, ncol=2, byrow=TRUE))
(c= c(5,6))
solve(mdat, c)


#2X + Y + Z = 1
#4X + 3Y + 4Z = 2
#-4X + 2Y + 2Z = -6
(x = c(2,1,1,4,3,4,-4,2,2))
(mat = matrix(x, nrow=3, ncol=3, byrow=T))
(y=c(1,2,-6))
solve(mat,y)

# x + 3y + 5z = 10
# 2x + 5y + z = 8
# 2x + 3y + 8z = 3
(x = c(1,3,5,2,5,1,2,3,8))
(mat=matrix(x, nrow=3, ncol=3, byrow=T))
(y=c(10,8,3))
solve(mat,y)
#방정식의 해를 구하고 검산해보시오.



#문제
#Matrix Operations 문제

mat1 <- matrix(
  1:15,
  nrow=5,
  ncol=3,
  byrow=T,
  dimnames=list(
    c("M1.r1", "M1.r2", "M1.r3", "M1.r4", "M1.r5")
    ,c("M1.c1", "M1.c2", "M1.c3")
  )
)
mat2 <- matrix(
  16:30,
  nrow = 5,
  ncol = 3,
  byrow = T,
  dimnames = list(
    c("M2.r1", "M2.r2", "M2.r3", "M2.r4", "M2.r5"),
    c("M2.c1", "M2.c2", "M2.c3")
  )
)


dim(mat1)
dim(mat2)

# 다음 두 매트릭스를 rbind, cbind를 이용해 묶으시오

c = cbind(mat1, mat2)
c
r = rbind(mat1, mat2)
r



# 다음 두 매트릭스의 사칙연산을 구하시오

mat1 + mat2
mat1 - mat2
mat1 * mat2
mat1 / mat2


# 다음 두 매트릭스의 행렬내적(곱 : %*%)를 구하시오

( tmat2<-t(mat2) )
mat1 %*% t(mat2) #행렬내적 #행렬나누기는 없다 : 역행렬.


#더하기 항등원은 0, 곱하기의 항등원은 1, 행렬의 항등원은 단위행렬.
#역행렬을 구해라. 1/행렬을 구해라. 1/행렬 을 다시 행렬으로 만들어라.
m <- matrix(c(5, -3, 2, 4, 12, -1, 9, 14, 7), nrow=3, ncol=3)
inv.m <- solve(m) #m과 inv.m은 역행렬이다. 그걸 어떻게 아느냐, 곱하면 1이되면 (단위행렬)이 되면 역행렬이다.
#solve에 넣었다 빼면 역행렬이된다.
round(m %*% inv.m) #단위행렬


#'행렬식을 계산하는 것' : 역행렬의 여부를 결정해준다.
det(m)



# 행렬에 함수를 적용.
(m=matrix(c(1:9), nrow=3, ncol=3))

apply(m, 1, max) #'1' 이면 행방향.으로 max값이뭐냐 // max는 함수가 아니라, 매개변수로 전달하고 있기때문에 () 안씀.
apply(m, 1, min) #'
apply(m, 2, max) #'2' 열방향으로 max값이 뭐냐.



### list
x <- c(82, 95, 78)
y <- c(84,92)
z <- c(96,86,82,77)
(core <- list(수학=x, 영어=y, 국어=z)) #실제 업무에는 변수는 영어 !!



#R은 객체지향 프로그래밍이고, 실시간으로 변수를 추가하는 것이 가능하다.
# dataFrame
#요소 수가 같아야 함.
#열간 이질적 기능, 열내 동질적 기능.
x <- c(10,20,30,40)
y <- c(6, 7, 8, 9)
data <- data.frame(길이=x, 높이=y) #열이름.
data
str(data)
data$길이#하나의 열을 참조하고 싶다.
data[,1]#2차원 배열처럼 접근.. 행은 모두 보여주고, 1열만 보여줘라.
head(data[c("길이","높이")])#헤더를 보고 싶다. 헤더는 6개만 보여줌.
head(data[c("길이","높이")],2)#헤더 2개만 보여달라.
data$둘레 = c(30,40,50,60) #열이 하나 더 만들어짐. 갯수가 다르면 안된다.
data


d.f <- data.frame()
d.f <- edit(d.f) # 쓸 일은 없다..


L3 = LETTERS[1:3]
d<- data.frame(cbind(x=1, y=1:10), fac=sample(L3, 10, replace=TRUE))
#                   x가 recycling되어서 계속만들어짐.
#                                                  replace는 복원추출해라는 뜻.
d$fac
names(d)
(d$yhat <- d$fac) #파생변수.

rownames(d) = c("일","이","삼","사","오","육","칠","팔","구","십") #행렬과 같다.
d



x <- data.frame("SN"= 1:2, "Age"= c(21,15), "Name"= c("에이콘","삼성SDS"),stringsASFactors=FALSE)
x
str(x)
x["Name"] #이렇게 접근할 수 있지만.. data.frame이 나옴.
x$Name #보통 이렇게 접근한다.      vector가 나옴..
class(x)
#class(x$Name)
x[["Name"]]    # [[ ]] 이렇게 2개가 붙으면, 요소개별로 접근하겠다는 뜻.
class(x[["Name"]])
x[3] #열별로. Matrix에서는 전체를 1차원벡터로 보고 골라냈으나, 여기서는 열로 나온다. 여기서는 열 인덱스라는 이야기.
x[[3]]
class(x[[3]]) #요소별로.      ===>> 결과 '데이터 타입'이 다르다 !!



x$Name = as.character(x$Name)
names(x)
colnames(x) <- c("SS","AA","CC")
ncol(x)
nrow(x)
length(x) # ncol과 같음
str(x)
rbind(x, list(1, 16, "Paul"))
cbind(x, State=c("NY","FL"))
x$Stage <- c("NY","FL");
#  행   열
x["Age", 1] <- 20; #
x[1, "Age"] <- 20; #
x$Stage <- NULL #데이터 삭제 방법
x <- x[-1,]


library(help = "datasets")
str(trees)#나무 종류 별 둘레, 높이, 체적.
head(trees, n=3)
tail(trees, n=3)
trees[2:3,]
trees[trees$Height > 82,]#값 #filtering
trees[10:12, 2]
trees[10:12, 2, drop = FALSE]
data()
str(trees)
summary(trees) #최소값 4분위수 최대값.   #IQR 1.5배 벗어나는 녀석은 이상치. # boxplot은 이상치 여부를 확인하는데 사용함.
boxplot(trees)
pairs(trees)   # 두 변수간의 산포도를 출력. #자기자신은 직선이라서 이름만 나옴. #이와같이, 축만 다르고, 같은 것들을 대칭행렬이라고 한다.
