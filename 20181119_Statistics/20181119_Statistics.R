score4 <- c(3,3,6,7,7,10,10,10,11,13,30)
(n <- length(score4))
min(score4)
max(score4)
range(score4) #최소 최대를 돌려줌.
diff(range(score4))
mean(score4)
median(score4)
sum(score4)
sort(score4)
order(score4) #order는 위치값이 나옴. #정렬의 기준점으로 작동하기 위해 사용
rank(score4) #등수를 고려한 순서
summary(score4)  #최소, 1사분위수, 평균, 3사분위수, 최대값
var(score4) #분산. (차의 제곱의 합)/ 자유도(n-1)
sd(score4) #standard deviation 표준편차
#계산방법에 의한 분산과 표준편차
total <- sum((score4-mean(score4)) * (score4-mean(score4)))
total/length(score4) #length로 나눌때는 모집단인 경우.
var_val<- total/(length(score4)-1) # 표본인 경우.
var_val
sqrt(var_val)

#분산 = 면(관측치- 평균 ) ^2 /자유도
#분산을 루트씌우면
#*** 분산과 표준편차가 통계학에서 가장 중요하다 !

range(score4)
range(score4)[2]-range(score4)[1]
quantile(score4)
quantile(score4, 0.25, type=1)


fivenum(score4)
fivenum(score4)[4] = fivenum(score4)[2]
(iqr <- quantile(score4)[4] - quantile(score4)[2] )
IQR(score4)


table(score4)
(uppercut=fivenum(score4)[4]+1.5*IQR(score4)) #이상치 판단을 위한 상한값
(lowercut=fivenum(score4)[2]-1.5*IQR(score4)) #하한값.

boxplot(score4, xlab='점수')

#apply 함수는 데이터 함수 적용.
sapply(mtcars, mean, na.rm=TRUE) #행열지정이 없음, 열단위 작업. => 변수에 평균계산(=열)


#위 와 같은 변수들을 한꺼번에 구해주는 패키지.
install.packages("Hmisc")
library(Hmisc)
describe(mtcars)

install.packages("pastecs")
library(pastecs)
stat.desc(mtcars) #nbr(number of values), SE (standard error), CI (confidence interval) 신뢰구간[추정할때 씀]



#변동계수(표준편차의 크기비교: 평균)와 표준오차(표준편차/ sqrt(n)) : n 은 표본수
#표본수가 나왔다는 말은, '모집단'이 있다는 말이다. # 표본수가 나왔다는 건, '자유도'가 나와야 한다.
data(trees)
dim(trees)
head(trees)
summary(trees)
sd(trees$Volume) /mean(trees$Volume) #변동계수/표준화시킨것: 비교.

#표준오차와 신뢰구간
x = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) #표본
mean(x) #모집단이 평균과 같을 것이다. 추정하는 것은 , 점 추정.
(sem<-sd(x)/sqrt(length(x))) #표준오차
c(mean(x)-2*sem, mean(x)+2*sem) #신뢰구간 95%신뢰구간(2로 곱해주었기때문에) 97%신뢰구간(3으로 곱해주었기때문에)
c(mean(x)-3*sem, mean(x)+3*sem) #97% 신뢰구간.
# ==> 신뢰구간 구하는 이유? 점 추정하면 틀릴확률 많음. 정확성 떨어지기 때문에, 구간을 주기위해서 함.


#표준편차만 하면 되지, 왜 변동계수를 구하느냐? 크기에 영향을 받지 않도록 하기 위해서
#표준오차는 , '표본수'에 영향을 받지 않도록 하기 위해서. =>표본수가 많아지면 표준오차가 심해짐.

# 비교.  변수의 중요도를 같게 해줌.




#문제 : 표준오차를 구하는 함수를 만드시오. (함수에서 정의한 것을 대입해서 사용함.) #함수도 객체다.

stderr <- function(x) sqrt(var(x, na.rm=TRUE)/length(na.omit(x)))
stderr(x)

#범주화
library(ggplot2)
library(MASS)
str(Cars93)
hist(Cars93$MPG.highway) #고속도로에서의 연ㅂ
disc_1 <- Cars93[,c("Model", "MPG.highway")]
head(disc_1)
disc_1 <- within( disc_1, {#with와 within의 차이 : 수정여부부 / disc_1을 붙이지 않고 쓰겠다.
  MPG.highway_cd = character(0)다  #변수 추가.
  MPG.highway_cd[ MPG.highway >= 20 & MPG.highway <25] = "20~25"     #[] 안에는 boolean연산자. 범주호
  MPG.highway_cd[ MPG.highway >= 25 & MPG.highway <30] = "25~30"
  MPG.highway_cd[ MPG.highway >= 30 & MPG.highway <35] = "30~35"
  MPG.highway_cd[ MPG.highway >= 35 & MPG.highway <40] = "35~40"
  MPG.highway_cd[ MPG.highway >= 40 & MPG.highway <45] = "40~45"
  MPG.highway_cd[ MPG.highway >= 45 & MPG.highway <=50] ="45~50"
  MPG.highway_cd = factor(MPG.highway_cd,화                          # 범주화 다 시키고, 팩터화.
                          level = c("20~25", "25~30", "30~35", "35~40", "40~45", "45~50"))
})
attributes(disc_1$MPG.highway_cd) #변수의 속성을 확인하는 함수.
table(disc_1$MPG.highway_cd)
disc_1


#Model과 MPG.highway만 있을때는 분석하기가 막막하다/


#문제 : 범주화 된 구간값에 따라 MPG.highway의 함계, 평균, 표준편차를 구하시오.
#정답
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sum)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, mean)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sd)


#문제 : ggplot2를 이용해서 범주별로 그라프를 출력하시오
#정답
ggplot(disc_1, aes(x=MPG.highway_cd, fill=MPG.highway_cd)) + geom_dotplot(binwidth=0.5)
ggplot(disc_1, aes(x=MPG.highway_cd, fill=MPG.highway_cd)) + geom_bar()






#사분위수 범주화
summary(disc_1$MPG.highway)
disc_1 <- within( disc_1, {
  MPG.highway_cd2 = character(0)
  MPG.highway_cd2[ MPG.highway < quantile(MPG.highway, 0.25) ] = "1Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.25) & MPG.highway < quantile(MPG.highway, 0.50) ] = "2Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.50) & MPG.highway < quantile(MPG.highway, 0.75) ] = "3Q"
  MPG.highway_cd2[ MPG.highway >= quantile(MPG.highway, 0.75) ] = "4Q"
  MPG.highway_cd2 = factor(MPG.highway_cd2,
                           level = c("1Q", "2Q", "3Q", "4Q"))
})
head(disc_1)
(table(disc_1$MPG.highway_cd2))


# 문제) 위의 데이터를 MPG.highway 를 기준으로 오름차순으로 정렬하시오
#정답: 원래데이터는 sort 다른데이터 기준 order
(disc_1 <- disc_1[order(disc_1$MPG.highway),])
# 문제) 구간값에 따라 MPG.highway의 합계, 평균, 표준편차를 구하시오.
#정답
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd2, sum)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd2, mean)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd2, sd)
# 문제) ggplot2를 이용하여 범주별로 그라프를 출력하시오.
#정답
ggplot(disc_1, aes(x=MPG.highway, fill=MPG.highway_cd2)) + geom_dotplot(binwidth = 2)
qplot( disc_1$MPG.highway, data= disc_1,, color=disc_1$MPG.highway_cd2, binwidth=1)



#이산 범주화 - one-hot-encoding
Season <- c("E1", "E2", "E3", "E4", "E1", "E2", "E3", "E4") #factor 수만큼 변수화함.
SalesAmt <- c(300, 800, 400, 100, 280, 750, 390, 60)
TS <- data.frame(Season, SalesAmt, stringsAsFactors = F)
TS <- transform(TS,
                Season1 = ifelse(Season =="E1", 1, 0),      #season이 E1이냐, 맞으면 1, 아니면 0.
                Season2 = ifelse(Season =="E2", 1, 0),
                Season3 = ifelse(Season =="E3", 1, 0),
                Season4 = ifelse(Season =="E4", 1, 0)       #결과값은 0과 1만 들어간다. #그렇게 하기 위해서 팩터/변수 늘려야 한다.
)
TS


install.packages("prob")
library(prob)
tosscoin(1) # 표본공간 : 경우의 수 H:Head, T:Tail
tosscoin(4)
(S=tosscoin(3, makespace="TRUE")) #2가지 * 3번반복# 2^3 = 8가지 경우의 수
1/8

rolldie(1)
rolldie(2)
rolldie(2, nside=4) # 4면주사위
(rolldie(2, nside=4, makespace="TRUE"))

cards()
cards(jokers="TRUE")
(cards(makespace="TRUE")) #Spade, Heart, Diamond, Club, J:11 Q:12 K:13


head(cards())
head(1:50, 10)
tail(cards())
tail(1:50, 2)


roulette() #38


#복원기록을 추출하기 위해서
library(prob)
urnsamples(1:3, size=2, replace=TRUE, ordered=TRUE) #복원추출, 순서가 있다. #(1,2)(2,1) 다르게 취급(순서고려)
urnsamples(1:3, size=2, replace=FALSE, ordered=TRUE) #비복원추출
urnsamples(c("A","B","C"), size=2, replace=FALSE, ordered=TRUE)
urnsamples(c("A","B","C"), size=2, replace=FALSE, ordered=FALSE)
(s<- tosscoin(2, makespace=TRUE))
s[1:3]
s[c(2,4),]


#문제 :: cards에서 "Heart" 가 가 들어간 경우의 수를 확인.
c<-cards()
subset(c, suit="Heart")
subset(c, rank %in% 7:9)

rolldie(3)
subset(rolldie(30), x1+x2+x3 >16)

(s= cards(makespace=TRUE))    #25%
(a=subset(s, suit="Heart"))   #25% , spade, heart, diamond, club
(b=subset(s, rank %in% 7:9))  #52장, 12장. 0.231
12/52
prob(a) #s가 표본공간.
prob(s, suit=="Heart") #표본공간, suit는 사상.





prob(a, given=b) #조건부확률    12개 3개 0.25
prob(s, suit=="Heart", given=rank %in% 7:9)
prob(intersect(a,b))/prob(b) #교집합 / given

# 카드를 선택했는데, Heart이고, 이 중 7:9 나올 확률
Prob(b, given=a)


Prob(union(a,b)) #합집합은 공통부분이 제거됨.
Prob(a) + Prob(b) - Prob(intersect(a,b))


#문제
# 1) 숫자 카드를 뽑았는데, Spade, Heart이면서 5번 일 확률 (결합) == 교집합
#정답
(s=cards(makespace=TRUE))
(a=subset(s, suit %in% c("Heaert", "Spade")))
(b=subset(s,rank==5))
Prob(intersect(a,b))
# 2) 숫자 카드를 뽑았는데, 2,3이 선택되었고, 그 중 Diamond일 확률(조건부)
(a=subset(s,rank%in% 2:3))
(b=subset(s, suit=="Diamond"))
Prob(b, given=a)



조건부확률(conditional probability)
에이콘 대학교 졸업생 900명의 취업상태조사

           취업(E)          미취업(U)             합계
남학생M   460(0.51)         40 (0.04)           500(0.56)
여학생W   140(0.16)         260(0.29)           400(0.44)
합계      600(0.67)         300(0.33)           900(1.00)

P(E) <- 600/900
P(W) <- 400/900
P(M) <- 500/900
P(W&E) <- 140/900
P(W|E): [=P(W&E)/P(E)] <- 140/600


######################################################
#    확률 분포 중 정규분포에 대해서 배우겠습니다.    #
######################################################
# 정규분포 : 다양한 분포에서 표본수가 30개 이상인 경우 발생하는 분포
# 정규분포 확인 방법 : qq-norm, 왜도(skewness), 첨도(kurtosis)
#momennt 패키지

#적률 = 제곱의 의미.
 #1차 원점에 대한 적률은 : 평균
 #2차의 중심적률 : 분산
 #3차의 중심적률 : 왜도 (좌우 대칭 - 0 ) 0이 정규분포)
 #4차의 중심적률 : 첨도 (뾰족한 정도 - 3이 정규분포)


install.packages("moments")
library(moments)
set.seed(1234)
n.sample<- rnorm(n=10000, mean=55, sd=4.5)  #random + normal (정규분포)
skewness(n.sample)
kurtosis(n.sample)
skewness(rnorm(1000)) #평균/표준편차가 생략되어있음. 이런경우엔, 평균=0  표준편차=1 이다.=> 표준정규분포 =>표준화



#정규 분포 아니면 분석이 안됨-> 정규분포화 해야함 '변환'
install.packages("UsingR")
library(UsingR)
data(cfb)
head(cfb)
summary(cfb$INCOME)
hist(cfb$INCOME, breaks=500, freq=TRUE)

cfb<- transform(cfb, INCOME_log = log(INCOME + 1))
hist(cfb$INCOME_log, breaks=500, freq=TRUE) #정규분포에서 확인하는 것, qqnorm

cfb<- transform(cfb, INCOME_sqrt = sqrt(INCOME + 1))
hist(cfb$INCOME_sqrt, breaks=500, freq=TRUE)

#로그변화/ sqrt변화 하면 정규분포화




par(mfrow = c(1,3))
qqnorm(cfb$INCOME, main="INCOME의 Q-Q도")
qqline(cfb$INCOME)

qqnorm(cfb$INCOME_log, main="INCOME_log")
qqline(cfb$INCOME_log)

qqnorm(cfb$INCOME_sqrt, main="QINCOME_sqrt")
qqline(cfb$INCOME_sqrt)
par(mfrow = c(1,1))

skewness(cfb$INCOME)
kurtosis(cfb$INCOME)
skewness(cfb$INCME_log)
kurtosis(cfb$INCOME_log)
skewness(cfb$INCOME_sqrt)
kurtosis(cfb$INCOME_sqrt)




#분포 - 확률을 구하기 위함
#정규분포 : 중심극한원리 : 표본이 일정 수준 이상이면, 표본은 정규분포를 띈다.
(x <- rnorm(1000, 64.5, 2.5))
(x <- sort(x))
(d <- dnorm(x, 64.5, 2.5))
hist(x, probability=TRUE, main="한국 남자들 몸무게 분포")
plot(x, d, main="한국남자들 몸무게 분포", xlab="몸무게")
curve(dnorm(x), -3, 3)
plot(density(rnorm(10000, 0, 1)))


#누적확률( 모집단에서 표본을 선택하고 표본의 평균을 구하고 표준편차를 구해.  => z점수)
# z점수 => (관측치-평균) / 표준편차 = z점수 (표준정규분포에서 확률을 확인 가능)
pnorm(0) #평균0 #누적값
pnorm(1)
pnorm(2)
pnorm(3)
pnorm(1.64)
pnorm(1.96)
pnorm(2.58)
pnorm(1) -pnorm(-1) #구간값
pnorm(2) -pnorm(-2) #%95 신뢰구간에서 5%유의수준
pnorm(3) -pnorm(-3)

#분위수에 대한 확률값
dnorm(0) #평균0  #평균이 나올 한 지점에서의 확률
dnorm(1)
dnorm(2) #2배수.
dnorm(3) #3배수.
dnorm(1.64)
dnorm(1.96)
dnorm(2.58)






## 문제) 다음 표준 점수를 구하라
# 문제1) 평균이 100이고 분산이 10인 정규분포에서 50이 나올 확률은?

dnorm(50, mean=100, sd=sqrt(12))


# 문제2) 평균이 90이고 분산이 12인 정규분포에서 98이 나올 확률은
# 정규 분포로 구한 값과 표준점수로 구한 값이 다르다.

dnorm(98, mean=90, sd=sqrt(12))
