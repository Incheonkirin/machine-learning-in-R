par(mfrow=c(3,1))

curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)") #정규분포 곡선
z=seq(0,40,0.02)
lines(z,dnorm(z), type="h",col="grey")

curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq(-1.96, 1.96, 0.001)#95%신뢰구간
lines(z,dnorm(z),type="h",col="grey")

curve(dnorm(x,0,1),-4,4,xlab="z", ylab="f(z)")
z=seq(-2.58,2.58,0.001)#99%신뢰구간
lines(z,dnorm(z),type="h",col="grey")

par(mfrow=c(1,1))


#1) x~(300, 50^2) 인 정규분포에서 P(X>=370)일 확률을 구하여라
#이를 그라프 상에서 시각적으로 표현해 보시오.

#정답
1-pnorm(370,300,50)
#1-pnorm((370-300)/50)

curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq(((370-300)/50), 4, 0.01)
lines(z, dnorm(z), type="h", col="grey")
points(((370-300)/50), dnorm((370-300/50)))

# ? 표준편차로 나눠주는 이유 ?




#2) 브랜드의 백열전구의 수명이 1500시간의 평균값과 75시간의 표준편차로 정규적으로 분포되어 있다.
#2-1)백열 전구가 1410시간보다 덜 오래 갈 확률은 얼마인가?
#2-2)백열 전구가 1563과 1648시간 사이의 오래갈 확률은 얼마인가?

#정답
pnorm(1410, 1500, 75)

(pnorm(1648,1500,75)-pnorm(1563,1500,75))
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq((1563-1500)/75, (1648-1500)/75, 0.01)
lines(z, dnorm(z), type="h", col="grey")


#3)
#우리나라에서 사육하고 있는 생후 18개월 이상 된 황소 무게는 평균 500kg이고
#표준편차가 50kg인 정규분포라 한다. 이제 우량 한우를 집중 육성 관리하기
#위해 무게가 무거운 순서대로 5%에 해당하는 황소를 선발하고자 한다.
#그렇다면 무게가 몇 kg이상인 황소를 선발해야 하는가?

#정답

qnorm(1-0.05, 500, 50)
#(start<-qnorm(1-0.05, 500, 50))
curve(dnorm(x,0,1), -4, 4, xlab="z", ylab="f(z)")
z=seq( (start-500)/50, 4, 0.01)
lines(z, dnorm(z), type="h", col="grey")





# scale = 'z점수를 보여주는 것' => z점수 정규화

set.seed(100)
h_korean <- rnorm(n=1000, mean=170, sd=10)  #장키
h_bushman <- rnorm(n=1000, mean=140, sd=8)  #단키
height <- data.frame(h_korean, h_bushman)
rm(h_korean, h_bushman)

head(height)
attach(height)
par( mfrow=c(1,2) )
hist(h_korean, freq=TRUE, main="한국인 키 빈도 히스토그램")   #도수
hist(h_korean, freq=FALSE, main="한국인 키 확률밀도함수 그래프")   #상대도수

hist(h_bushman, freq=TRUE, main="부시맨 키 빈도 히스토그램")
hist(h_bushman, freq=FALSE, main="부시맨 키 확률밀도함수 그래프")

#아래는 z점수화
height<- transform(height, z2.h_korean=(h_korean - mean(h_korean))/sd(h_korean),
                                        z2.h_korean=(h_bushman-mean(h_bushman))/sd(h_bushman))

height<- transform(height, z.h_korean=scale(h_korean), z.h_bushman=scale(h_bushman))
hist(height$z.h_korean, freq=TRUE, main="한국인 표준")
hist(height$z.h_bushman, freq=TRUE, main="부시맨 표준")

#문제) 160cm의 키가 한국인과 부시맨에서 각각 어떤 위치인가 비교해보시오.

dnorm(   (160- mean(h_korean))/sd(h_korean))
dnorm(    (160- mean(h_bushman))/sd(h_bushman))




#****** 대부분 평균을 비교하는 건 이 녀석이다 *******************

# t-분포 (정규분포보다 조금 완만한 경사, 자유도에 영향을 많이 받음)
# 모집단의 표준편차를 모르기 때문에 표본의 표본편차를 사용해서 다시 작성한 확률 그라프 = 조금 원만하다.
# 평균을 비교
# t분포의 검정통계량 : 두 평균의 차이/ 차이의 표준편차

#****************************************************************


library(ggplot2)
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dt, args=list(df=3), colour="red", size=2) +
  stat_function(fun=dt, args=list(df=1), colour="yellow", size=3) +
  annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
  annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) +
  annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) +
  annotate("text", x=2.4, y=0.4, label="N(0,1)") +
  annotate("text", x=2.4, y=0.37, label="t(3)")+
  annotate("text", x=2.4, y=0.34, label="t(1)")+
  ggtitle("정규분포와 t분포")

#카이제곱분석 =교차표
#table , ftable, margin.table, prob.table, xtab, Crosstable
x=c(1,2,3,4,5,6)
y=c(7,8,9,10,11,12)

(z=table(x)) #한 변수의 factor기준 의한 도수 분포표
(z=table(x,y)) #마치 one-hot-encoding

#Cross Tanbulation - data.frame에 작동
(d <- data.frame(x=c("1","2","2","1"), y=c("A","B","A","B"), num=c(3,5,8,7)))
(xt = xtabs(num ~ x+y, data=d) )

margin.table(xt,1)
margin.table(xt,2)
margin.table(xt)
prop.table(xt,1)
prop.table(xt,2)
prop.table(xt)


#카이스퀘어 분포 함수
x<- seq(1, 10, .1)
plot(x, dchisq(x,6), type="l")
plot(x, dchisq(x,5), type="l")
plot(x, dchisq(x,4), type="l")
plot(x, dchisq(x,3), type="l")
plot(x, dchisq(x,2), type="l")
plot(x, dchisq(x,1), type="l")

#ftable : Flat Contingency Tables(인접행렬을 pivoting)
class(Titanic) #table
str(Titanic)
ftable(Titanic, row.vars=1:3) #factor형에 따른 분류
ftable(Titanic, row.vars=1:2, col.vars = "Survived")

# 1) 성별로 생존여부를 확인해보시오.
ftable(Titanic, row.vars = "Sex", col.vars = "Survived")


# 2) 객실과 나이별로 확인해보시오.
ftable(Titanic, row.vars="Age", col.vars="Class")


head(mtcars)
x<- ftable(mtcars[c("cyl", "vs", "am", "gear")])
x
x<- ftable(mtcars[c("cyl", "vs", "am")])
x

#dnn: 필드 이름을 새롭게 정의
ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear, row.vars =c(2,4),
       dnn=c("Cylinders", "V/S", "Transmission", "Gears"))

A<- mtcars$vs
B<- mtcars$am
c<- mtcars$gear

#2차원 테이블 , 3번째 범주에 따라 여러개의 테이블로 분리. => 한꺼번에 보기가 어려움. => ftable로 바꿔보자
mytable <- xtabs(~A+B+C, data=mtcars) #x,y 축으로 테이블 3차원으로 분리하는 역할
mytable

str(mytable)
ftable(mytable)
summary(mytable) #카이 스퀘어 테스트 chi-squared test 독립성검증(변수간 영향력이 있느냐) 0.05작음=>변수간에 영향력.?
# 독립성 검증의 귀무가설 : 서로 독립적이다. 대립가설: 서로 영향력이 있다.

#ftable


#이항테스트
binom.test(c(125,9), p=0.7) #(만족 수, 불만족 수) 134회를 시도횟수. 0.7 확률이 되는가. (비율이 0.7에 근사하냐.)
#p-value가 4.204e-11


## 비교의 문제.
#비율 검정 문제
# A회사 직장인 500명과 B회사 직장인 600명을 대상으로 조사를 한 결과,
# A회사 직장인의 흡연율은 33%, B회사 직장인의 흡연율은 41%로 나타났다.
#그러면 A회사와 B회사 직장인의 흡연율(proportion of smokers)에는 차이가
#있다고 할 수 있는지 유의수준(significance level) 5%에서 검정하시오.

# 귀무가설 : A회사와 B회사의 흡연율 차이가 없다.
# 대립가설 : A회사와 B회사의 흡연율 차이가 있다.

prop <- c(0.33, 0.41)
n <- c(500, 600)
#실제 인원을 구해보면
x <- prop*n

prop.test(x=x, n=n, alternative=c("two.sided"), conf.level=0.95)
#                               양쪽으로 확인

#귀무가설을 기각하고 대립가설을 채택한다.
#A회사와 B회사의 흡연율에는 차이가 있다. A회사는 전철역 인근에 있어 흡연자의 영향을 많이 받는다.
#B회사는 직장내 불협화음으로 스트레스가 심한 상태인 것 같다.




#다음은 사이다 선호도 조사결과이다. 선호도에 차이가 있는지 검정하시오
#귀무가설 : 사이다 종류에 따른 선호도에는 차이가 없다.
#대립가설 : 사이다 종류에 따른 선호도에는 차이가 있다.


data<- textConnection(
  "사이다 종류 관측도 수
  1 14
  2 32
  3 17
  4 9
  5 18")

(x<-read.table(data, header=T))
chisq.test(x$관측도수)
#결과 : 귀무가설을 기각하고 대립가설을 채택한다. 사이다에 대한 선호도에는 차이가 있다.
(기대도수=(14+32+17+9+18)/5)
dchisq(16.333, 4)



#귀무가설 : 성별에 따라 운동량의 차이가 없다.
#대립가설 : 성별에 따라 운동량의 차이가 있다.
library(MASS)
#install.packages("gmodels")
library(gmodels)
str(survey)
View(survey)
head(survey[c("Sex", "Exer")])
(xt=xtabs(~ Sex+Exer, data=survey))
CrossTable(survey$Sex, survey$Exer, expected=TRUE)
chisq.test( xtabs(~ Sex+Exer, data=survey))

#CrossTable
#N 관측치
#Expected N 기대치
#Chi-squared 값  (49-57)^2 /57
#Row Total
#Col Total
#Table Total

#자유도 == 2
#p-value = 0.05 로, 유의 수준 보다 크다. 얼마 차이 나지 않더라도, 기무가설을 기각하지 못한다. 남녀간에는 운동량의 차이가 없다. 없는 이유는 인간은 다 똑같으니까.



#동질성 검증 . 이미 데이터를 가지고 알고 있음. (왼손잡이와 오른손잡이의 비율이 0.3:0.7) 이때 표본이 이런 경향을 보이는가?

#동질성 검증에서 귀무가설은 = 비율이 동일하다
#대립가설 = 비율이 동일하지 않다.
table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd), p=c(.3, .7))

# p-value가 안된다 ?  == 무슨 말인지 잘 보자.
# 귀무가설을 기각하고, 대립가설을 채택한다.
# 비율이 동일하지 않다.



#1)
# 카이제곱 값을 구해보시오
# 다음은 맥주회사 선호도 조사 결과이다. 카이제곱 분석을 통해 맥주회사별 선호도에 차이가 있는지 검정하시오.

#BrandA     BrandB      BrandC
#18         23          29
beers <- c(18,23,19)
chisq.test(beers)

#2)
#다음 데이터의 Treatment, Improved 데이터를 이용하여 관절염 치료에 대해 처치에 개선 효과가 있었는지 검정해보시오.
#install.packages("vcd")
library(vcd)
str(Arthritis)
attach(Arthritis)

CrossTable(Treatment, Improved,
           expected = TRUE,
           chisq = TRUE)

chisq.test(Treatment, Improved)
detach(Arthritis)


#3)
#cleanDescriptive.csv 데이터를 로딩한 다음 부모학력수준(level2)과 자녀대학진학여부(pass)간에
# 관련이 있는지 검정하시오.

univ <- read.csv("cleanDescriptive.csv", stringsAsFactors =FALSE)
univ

attach(univ)
CrossTable(level2, pass, expected=TRUE, chisq=TRUE)



#t-test
pt(q=1, df=1, lower.tail=TRUE)
qt(p=0.75, df=1, lower.tail=TRUE)
rt<- rt(50, df=1)
rt
hist(rt, breaks=20)
#shapiro.test 귀무가설은 정규분포이다.
#대립가설은 정규분포가 아니다.

shapiro.test(rnorm(1000))
set.seed(450)
x<- runif(50, min=2, max=4) #균등분포  #균등분포도 일정(30) 이상 올라가면 정규분포를 띈다.
shapiro.test(x)



#일원 T-test
a = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81) #p-value가 0.4537 == 모집단에서 있을 수 있는 표본 추출
t.test( a, mu=75) #모집단의 평균이 75임을 알고 있음.
#단일 집단 평균검정 / 폐암 발병률이 20%
set.seed(200)
(lung<- runif(1000, min=10, max=25)) #p-value가 2.2e-16이 나왔다. 의미있는 집단. '유의미' #발병률이 20%인 집단과 다른 집단으로 판별된다.
t.test(lung, mu=20, conf.level=.95)


#수면제 - 대응 표본인지 아닌지에 따라서 많이 달라짐. 대응 표본 여부를 확인 해야 함.
str(sleep)
plot(extra ~ group, data = sleep)

t.test(extra ~ group, data = sleep)
with(sleep, t.test(extra[group==1], extra[group==2])) #p-value가 0.07939
with(sleep, t.test(extra[group==1], extra[group==2], paired=TRUE)) #동일한 집단 대상  #p-value가 0.002833 수면효과가 유의미함.

#등분산성 테스트(var.test)
#귀무가설은 분산이 동일하다 - 등분산성이다. => 귀무가설 기각하지 못한다. var.equal 을 달아 주어야 한다.
#대립가설은 분산이 동일하지 않다.
sleep
str(sleep2<- sleep[,-3])
tapply(sleep2$extra, sleep2$group, mean)
var.test(extra~group, sleep2)
t.test(extra~group, data=sleep2, paired=TRUE, var.equal=TRUE)



#문제 : iris의 Sepal.Width와 Sepal.Length의 등분산성 테스트를 하시오.

#정답
with(iris, var.test(Sepal.Width, Sepal.Length))
#등분산성이 아니다.
# var.test <0.05 분산이 동일하지 않다.


#새로운 당뇨병 치료제를 개발한 제약사의 예
#치료에 지대한 영향을 주는 외부요인을 통제하기 위해 10명의 당뇨병 환자를 선별하여 1달동안
#'위약(placebo)'을 투여한 기간의 혈압(Xi)와 '신약(new medicine)'을 투약한 1달 기간 동안의 혈당 수치(Yi)를
#측정하여 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시오

x1 <- c(51.4, 58.0, 45.5, 55.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

#귀무가설(위약이나 신약에 효과의 차이가 없다.) 대립가설(위약이나 신약에 효과의 차이가 있다.)

#정규분포 테스트
shapiro.test(x1)
shapiro.test(x2) #정규분포 아니다
#shapiro.text 실패하면 wilcox test로 가게 됨.

#대응표본
#정규분포 띄지 않기 때문에 var test는 하지 않는다.
wilcox.test(x1, x2,
            alternative = c("greater"),
            paired = TRUE,
            conf.int=FALSE,
            exact=FALSE, #정밀하게
            conf.level=0.95)

#p-value가 0.006201 == 0.05보다 작게 나왔으므로, 귀무가설을 기각하고, 대립가설을 채택한다.
#신약의 효과가 있다.




#문제
#MASS패키지에 내장된 Cars93 데이터프레임의 가격(Price)와 생산국가(Origin)
#데이터에서 생산국이 USA vs. non-USA 2개의 group에 대하여 차 가격의(Price)
#평균이 차이가 있는지를 검정해보시오
#다음과 같은 형식으로 보고서를 제출하시오

#1) 가설설정 : 연구가설(H1), 귀무가설(H0)
#2) 연구환경 : 미국내에서 판매되고 있는 자동차에 대하여 원산지별 가격차이가 발생하고 있다는
#              여론이 있어 이를 확인하고자 원산지별 자동차 가격을 수집하여 검정을 실시한다.
#3) 유의수준 : 0.05
#4) 분석방법 : 비모수검정
#5) 검정통계량 : W = 1024.5
#6) 유의확률 : p-value = 0.6724
#7) 결과 해석: 귀무가설을 기각하지 못한다. 그러므로 미국내 자동차는 원산지별 가격차이 없다.
#################################################################################################
#################################################################################################
#################################################################################################
library(MASS)

str(Cars93)
x1<-Cars93$Price[Cars93$Origin=="USA"]
x2<-Cars93$Price[Cars93$Origin!="USA"]

shapiro.test(x1)
shapiro.test(x2) #정규분포 따르지 않음.


wilcox.test(x1, x2,
            alternative = c("greater"),
            paired = FALSE,
            conf.int=FALSE,
            exact=FALSE,
            conf.level=0.95)

#p-value = 0.6666 귀무가설을 기각하지 않는다.
#################################################################################################
#################################################################################################
#################################################################################################


#정답
str(Cars93)
Cars93$PriceCars93$Origin
class(Cars93$Origin)
levels(Cars93$Origin)
table(Cars93$Origin)
with(Cars93, tapply(Price, Origin, summary))
boxplot(Price ~ Origin,
        data = Cars93,
        main = "원산지별 가격",
        xlab = "원산지",
        ylab = "가격")
with(Cars93, tapply(Price, Origin, shapiro.test))
var.test(Price ~ Origin, data = Cars93)
wilcox.test(Price ~ Origin,
            data = Cars93,
            alternative = c("two.sided"),
            var.equal=TRUE,
            exact=FALSE,
            conf.level=0.95)




# anova test : 집단이 2개 이상인 경우
# batlett.test : 등분산성 테스트
# aov(등분산성인 경우), kruskal.test(비모수테스트)
require(stats)
require(graphics)

data(InsectSprays)
attach(InsectSprays)
str(InsectSprays) #count(벌레수) / spray : 곤충박별 , 스프레이별 기능차이가 있는가.
InsectSprays
xtabs(InsectSprays)   # <<<===== 이 값이 유의미 한지.

with(InsectSprays, mean(count[spray=="A"]))
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, (tapply(count, spray, var)))
with(InsectSprays, (tapply(count, spray, length)))
with(InsectSprays, (boxplot(count ~ spray)))
photoperiod<- ordered(spray, levels=c("F", "B", "C", "D", "E", "A"))
tapply(count, photoperiod, mean)
with(InsectSprays, oneway.test(count~spray))
#귀무가설 차이가 없다.
#스프레이별 기능차이가 있다.
aov.out = aov(count~spray, data=InsectSprays) #oneway test와 aov test 모두 같음. 단, aov는 summary해야 보임.
summary(aov.out)
TukeyHSD(aov.out)  #0.05보다 크면 차이가 없고, 보다 작으면 차이가 있는 것이다.
summary.lm(aov.out) #위 TukeyHSD와 비슷함.
plot(aov.out)
#Q-Q도 정규분포를 보인다.

plot(count ~ spray, data = InsectSprays)
bartlett.test(InsectSprays$count, InsectSprays$spray) #bartlett.test 등분산성.  띄면 anov 띄지않으면 kruskal
bartlett.test(count ~ spray, data = InsectSprays)
#스프레이별 기능 차이가 있다.

kruskal.test(count~spray, data=InsectSprays)
