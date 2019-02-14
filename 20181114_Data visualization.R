library(MASS)
data("Boston")
help("Boston")
str(Boston)
Boston$tax
plot(density(Boston$tax, bw=5)) #메인
rug(Boston$tax + rnorm(length(Boston$tax), sd=5), col=2, lwd=3.5) #보조


#나만 알 수 있는 표
max.temp <- c(22, 27, 26, 24, 23, 26,28)
barplot(max.temp)
#설명 첨가
barplot(max.temp,
        main = "Maximum Temperatures in a week",
        xlab = "Degree celsius",
        ylab = "Day",
        names.arg = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        col = "darkred",
        horiz = TRUE) #horizontal /vertical

Titanic # Table
margin.table(Titanic,1) #margin 합계를 낸다는 뜻이다. #table 도수분포표 나타낼때 씀.
margin.table(Titanic,4)
barplot(margin.table(Titanic,1))
barplot(margin.table(Titanic,4))



honeymoon <- c(12,14,8,9)
names(honeymoon) <- c("하와이", "동남아", "유럽", "기타")
pie(honeymoon, col=rainbow(4))

per <- round(100*honeymoon/sum(honeymoon), 1)
lab <- paste(names(honeymoon), "\n", per, "%") #for문이 없는데, honeymoon안에 있는 게 하나씩 나오면서 출력.
lab
pie(honeymoon, labels=lab, col=rainbow(4))

text(-0.5, 0, "35%", col="black")
text(0.3, 0.3, "30%", col="black")
text(-0.1, -0.5, "20%", col="black")
text(0.5, -0.3, "15%", col="black")


weight <- rnorm(30, 10, 80)
stem(weight)
stem(weight, scale=1.0)
stem(weight, scale=0.5)

(c<- seq(0:100))
(d<- sort(round(runif(100)*100))) # 0~100까지의 정수
quantile(c,0.25) # 4사분위수 중 1사분위수
quantile(d,0.25)
(sum(d)/100*4)
range(d)/4



data(iris) #4개의 데이터 읽어들임.
head(iris)
x<- iris$Sepal.Length
hist(x, prob=TRUE, col="skyblue", border="white")
lines(density(x))
rug(iris$Sepal.Length)
rug(iris$Sepal.Length, side=3, lwd=3)
rug(jitter(iris$Sepal.Length))  #jitter

res=boxplot(x)
res


#문제1 airquality의 Temp(Temprature)을 hist를 이용하여 출력.
#      다양한 컬러 값을 적용하여 출력하고, x축은 50, 100 사이의 값으로 제한하시오
#      (break=5) 적용
airquality
str(airquality)
Temperature <- airquality$Temp
hist(Temperature)
colors= c("yellow","green","violet","orange","blue","pink","cyan")
res = hist(Temperature,
           main="La Guardia Airport의 최고온도",
           xlab="Fahrenheit로 측정한 온도",
           xlim=c(50,100),
           breaks=5,
           col=colors)

text(res$mids, res$counts, labels=res$counts, adj=c(0.5, -0.5))
hist(Temperature, breaks=20)




x<- rnorm(1000, mean=100, sd=1) #정규분포
qqnorm(x)
qqline(x, lty=2)

x<- runif(1000) #균등분포
qqnorm(x)
qqline(x)
#                => 갈수록 정규분포를 이루지 않는 것을 볼 수 있음.



# 디지털 회사의 직원 임금. histogram으로 출력.
# 임금별 도수를 확인하시오.
(salary = round(rnorm(100, 250, 100)))
hist(salary, main= "디지털 회사 직원 임금", xlab="임금", ylab="도수", col="lightblue", breaks=5)
hist(salary, main= "디지털 회사 직원 임금", xlab="임금", ylab="상대도수", col="lightblue", prob=TRUE)
lines(density(salary), col="blue", lwd=2)
lines(density(salary, adjust=2), lty="dotted", col="darkgreen", lwd=2)




#진폭과 주기  #고주파와 저주파   #음성 = sin + cos 분해가능.
(x = (0:100)*pi/ 10)
amp.1 <- 2 #진폭이 2, 5
amp.2 <- 2
amp.3 <- 5
amp.4 <- 5

wav.1 <- 1 #주파수 빠른것은 빽빽히 차있는것.
wav.2 <- 2 #주파수 원에서의 방향. 한자리에 놓고 계속 뺑뺑이로 돌려. 돌아가는 선을 받아서 시간순으로 찍으면. 반복되는 주기가 나옴.
wav.3 <- 3 # 짧으면 고주파(충격에 약해서 짧게 감), 길면 저주파(멀리, 장파). 고주파는 막 왔다갔다해서 특성파악이 힘듦.
wav.4 <- 7 #고주파를 저주파로 만들어야 함. '이동평균선' 12일 60일 120일 이동평균선. 주식에서 10년 단위 보려면, 하루종일 왔다갔다한건 없애고,마지막 것만 봄.
           # 주식 평균을 내면, 1개만 나옴. 그럼 주파수가 없어지고, 특성파악이 쉬워짐.
signal.1 <- amp.1*sin(wav.1*x)
signal.2 <- amp.2*sin(wav.2*x)
signal.3 <- amp.3*sin(wav.3*x)
signal.4 <- amp.4*sin(wav.4*x)

par(mfrow = c(1,4))
plot(x, signal.1, type="1",ylim=c(-5,5)); abline(h=0, v=0, lty=3)
plot(x, signal.2, type="1",ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.3, type="1",ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.4, type="1",ylim=c(-5,5)); abline(h=0, lty=3)
par(mfrow= c(1,1))



# 데이터 중복 확인  #주파수가 너무 왔다갔다해서 의미없으면, 스피드별로 그룹해서 평균을 내서 출력하면, 고주파/저주파로 바뀜.
plot(cars$dist, type="o", cex=0.5, xlab="speed", ylab="dist")
tapply(cars$dist, cars$speed, mean)
plot(tapply(cars$dist, cars$speed, mean), type="o", cex=0.5, xlab="speed", ylab="dist")


# 신뢰구간의 확인 #lm = linear regression 선형회귀. (formular ~) 물결이 formular다. 종속변수와 독립변수.
#차량의 브레이크 제동거리        linear regression은 어떤선인지 몰라. 모르는데 그선을 그렸을때, 선이 결정되었어. 실제 예측한 값과, 선과 차이가 존재함. +도 있고 -도 있음. 그래서 차에 제곱을 취함. 오차의 합. 오차의 합이 가장 작아지는 것을 구하는게 선형회귀 이다. 어디서만나느냐/인터셉트, 기울기/슬롭
str(cars)

(m <- lm(dist ~ speed, data=cars) )   # = y = -17.579 + 3.932 x     #선형회귀 모델이 되었음. #절편과 기울기 돌려주는게 선형회기이다.
plot(cars) #산포도 출력 먼저, x,
abline(m) #m값이 뭐가 나오나?
(p <- predict(m, interval="confidence"))   #예측이 가능해짐. 신뢰구간.
### 뭐를 predict한거야? 점들에 대해서 데이터를 가지고있는데, 점들에 대해서 예측을 해준것이다. '이'점일때 하한선과 상한선을 에측.

head(p)    #fit = 점추정, 신뢰구간(상한선/하한선)
cars$speed
x <- c(cars$speed, tail(cars$speed, 1), rev(cars$speed), cars$speed[1])
# 실제 관측된           car$speed 에서 하나 뺀 이유? 마지막 점을 하나 더 빼서,
y <- c(p[, "lwr"], tail(p[, "upr"],1), rev(p[, "upr"]), p[, "lwr"][1])

length(x)
length(y)

polygon(x, y, col=rgb(.7, .7, .7, 0.5)) #rgb( red, green, blue, alpha[투명값])


#  이런 원리로 선형회기가 이뤄져 있다는 것을 알면 된다. #

#파생변수 = Height를 제곱해준 것.

str(trees)
summary(trees)
Height2 <- trees$Height^2 #파생변수(로 만들었을때 데이터가 맞다고 하면 제곱하는것이다)
trees2 <- cbind(trees, Height2)
trees2
attach(trees2)
test2<- lm(Girth ~ Height + Height2) #선형회기를 해도 직선 아닌게 나올 수 있다. #물결 중심하고 왼쪽:종속변수,오른쪽:독립변수. / 다중회기(독립변수가 여러개 들어오는 것)
test2

#y = 8.276598 - 0.133717 * Height + 0.002602 * Height2

plot(Girth~Height)  #물결이 들어가면 포뮬러 왼쪽 종속변수 오른쪽 독립변수  y축=종속변수 x축= 독립변수
fitted(test2) #참여한 값들을 알아볼 수 있는 함수.
lines(sort(Height), fitted(test2)[order(Height)], col='red', type='l')
detach(trees2)


#문제 women데이터의 weight와 height간의 선형회귀선을 그리시오.

str(women)
res=lm(weight ~ height, data=women)
plot(women)
abline(res)



x<- seq(-2*pi, 2*pi, 0.01)
y<- matrix(c(cos(x), sin(x)), ncol=2)
matplot(x,y, lty=c("solid", "dashed"), cex=.2, type="l")
abline(h=0, v=0)
##
str(trees)
fix(trees)
summary(trees)
boxplot(trees)
pairs(trees) #두 변수간에 상관 관계(산포도)를 보여주는 것.
plot(trees)
matplot(trees)

## bquote 매크로 " " : mtext에 들어가려면 !
plot(1:10, type="n", xlab="", ylab="",, main="plot math & numbers")
theta <- 1.23 ; mtext(bquote(hat(theta) == .(theta)), line= .25)
for(i in 2:9)

    text(i, i+1, substitute(list(xi, eta) == group("(",list(x,y),")"),list(x=i, y=i+1)))
text(1, 9.6, expression(
  "            first: {f * minute}(x) " == {f * minute} (x)), adj = 0)

#축이름을 수학기호로 쓰고 싶을때. expression 안에서 hat쓰면 hat, alpha^beta는 그리스문자
par(mar = c(4, 4, 2, 0.1))
plot(rnorm(100), rnorm(100),
     xlab = expression(hat(mu)[0]), ylab = expression(alpha^beta),
     main = expression(paste("plot of", alpha^beta, "versus", hat(mu)[0])))



#데이터 저장

Temperature <- airquality$Temp
jpeg(file="saving_plot1.jpeg")
hist(Temperature, col="darkgreen")
dev.off()

png(file="sv1.png",
    width=600, height=350)
hist(Temperature, col="gold")
dev.off()



#3차원 그라프
x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, cone)
cone <- function(x, y){
  sqrt(x^2+y^2)
}
#matlab
persp(x, y, z)
persp(x, y, z,
      main="Cone",
      zlab="Height",
      theta=30, phi=15, #카메라 조절: 어떤 시점에서 확인하느냐냐, theta = 좌우, phi = 상하
      col="springgreen", shade=0.5)



#lattice 3차원 등 고차원으로 데이터 확인 가능.
# 파이프 지원, 창을 여러개 여는 것이 가능함.
#다차원 출력

library(lattice)
str(airquality)
xyplot(Ozone ~ Wind, data=airquality)
xyplot(Ozone ~ Wind | Month, data=airquality)
xyplot(Ozone ~ Wind | Month, data=airquality, layout=c(3,2))
convert <- transform(airquality, Month=factor(Month))
str(convert)
xyplot(Ozone ~ Wind | Month, data = convert, layout=c(5,1))

##
install.packages("mlmRev")
library(mlmRev)
data(Chem97)
str(Chem97)
head(Chem97, 30)
tail(Chem97, 10)

histogram(~gcsescore, data=Chem97)
histogram(~gcsescore|score, data=Chem97)
histogram(~gcsescore|factor(score), data=Chem97)
densityplot(~gcsescore|factor(score), data=Chem97, groups=gender)

#파이프 다음에는 factor형이 와야 함.


dice = c(1,2,4,3,4,6,5,6,4,5,3,4,3,2,1,6,4,6,2,4,5,1,1,6)
dotplot(table(dice), xlab="빈도", ylab="주사위")


#문제 다음 데이터의 dotplot으로 확인해 보시오
VADeaths
#데이터프레임으로 변환해야 표현하기 용이함.
dft <- as.data.frame.table(VADeaths)
dotplot(Var1 ~ Freq | Var2, dft)
dotplot(Var1 ~ Freq | Var2, dft, layout=c(4,1))

#바 차트로 해보자
barchart(Var1 ~Freq | Var2, data=dft, layout=c(4,1))


#위도 (lattitude):층으로 경도(longitude):세로로
head(quakes)
str(quakes)

xyplot(lat~long, data=quakes, pch=".")
dotplot(lat~long, data=quakes, pch=".")

tplot<-xyplot(lat~long, data=quakes, pch=".")
tplot2<-update(tplot, main="1964년 이후 태평양에서 발생한 지진위치")
print(tplot2)
##
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)
depthgroup
class(depthgroup)

xyplot(lat~long | depthgroup, data=quakes, main="Fiji Earthquakes(depthgroup)", ylab="latitude",
       xlab="longitude", pch="@", col="red")

magnitude<- equal.count(quakes$mag, number=2, overlap=0)
magnitude
xyplot(lat~long|depthgroup*magnitude, data=quakes, main="피산 지진", ylab="latitude",
       xlab="longitude", pch="@", col=c('red', 'blue'))


## ggplot2 : 데이터를 이해하는데 좋은 시각툴
# qplot, ggplot, ggsave
# 미적 매핑(asenthetic), 통계적 변환(stats), 기하학적 객체의 적용(geom), 위치조정(position adjustment)

library(ggplot2)
data(diamonds)
data(mtcars)
#displace : 배출량
qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv) #drive 사륜구동, 전륜구동, 후륜구동.


qplot(hwy, data=mpg, fill=drv)
qplot(hwy, data=mpg, fill=drv, binwidth=2)
qplot(hwy, data=mpg, fill=drv, facets=.~ drv, binwidth=2) #나눠서 종류별로 보여줌.
qplot(hwy, data=mpg, facets=drv~., binwidth=2)

qplot(wt, mpg, data=mtcars, color=factor(carb))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(cyl))

qplot(clarity, data=diamonds)
qplot(clarity, data=diamonds, fill=cut, geom="bar")

qplot(carat, price, data=diamonds)
qplot(carat, data=diamonds, geom="histogram") #qplot은 geom이라고 하는 매개변수로 들어옴.
qplot(carat, price, data=diamonds, geom=c("point", "smooth")) #선형회기, 포인터로 표현하고. 같이 출력해주어라.


# ggplot2

library(ggplot2)
data(mpg)
str(mpg)

#ggplot2 는 layer를 이용해서 다단계로 출력
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_bar()
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_bar(position="fill") #구성.
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_bar(position="dodge")
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_bar(position="stack")
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_dotplot(binwidth=2)
ggplot(mpg, aes(x=hwy) + geom_histogram(binwidth=2))
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_histogram(binwidth=2)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density()
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density(adjust=4)
ggplot(mpg, aes(x=hwy, fill=drv)) + geom_density(adjust=0.25)
ggplot(mpg, aes(x=hwy, y=cty)) + geom_boxplot()
ggplot(mpg, aes(x=hwy, y=cty)) + geom_boxplot() + coord_flip() #좌표계를 회전.


ggplot(mpg, aes(x=hwy, y=cty)) + geom_point() + ylab('cty') + xlab('hwy')
ggplot(mpg, aes(x=hwy, y=cty)) + geom_line() + ylab('cty') + xlab('hwy')
ggplot(mpg, aes(x=hwy, y=cty, color=factor(hwy))) + geom_line() +
  ylab('cty') + xlab('hwy')
ggplot(mpg, aes(x=hwy, y=cty)) + geom_point(position=position_jitter(w=0.2, h=0.2))+
  ylab('cty') + xlab('hwy')
ggplot(mpg, aes(x=hwy, y=cty)) +
  geom_point(position=position_jitter(w=0.2, h=0.2)) + geom_smooth(method="lm")+
  ylab('cty') + xlab('hwy')
ggsave(file="bar.png", width=10, height=5)



#문제
#데이터 coin<- c(2,2,0,1,1,1,2,2,3,4,1,3,2,3,3,1,2,2,1,2,2)
#도수분포표, 상대도수분포표, 누적도수분포표를 ggplot2를 이용하여 그리시오

library(ggplot2)
coin<- c(2,2,0,1,1,1,2,2,3,4,1,3,2,3,3,1,2,2,1,2,2)
coin.num = sort(unique(coin))

coin.freq = rep(0, 4)
for (i in 1: length(coin.num)) {
  coin.freq[i] <- length(coin[coin==i-1])
}
coin.rel = round(coin.freq/length(coin), 2)
coin.cum = cumsum(coin.rel)

coin.freq <- data.frame(coin.num, coin.freq)
names(coin.freq)[2] <- paste("val") #val 종류를 구분
coin.freq$type=rep("freq", length(coin.num))
coin.freq

coin.rel = data.frame(coin.num, coin.rel)
names(coin.rel)[2] <- paste("val")
coin.rel$type = rep("rel", length(coin.num))
coin.rel
coin.cum = data.frame(coin.num, coin.cum)
names(coin.cum)[2] <- paste("val")
coin.cum$type = rep("cum", length(coin.num))
coin.cum
(coin.graph = rbind(coin.freq, coin.rel, coin.cum))
ggplot(coin.graph, aes(coin.num, val, group=type, col=type)) + #그룹핑해서 출력
  geom_point() +
  geom_line()



#dplyr
#비행기의 이착륙 정보를 이용하여 연착에 대한 분석정보
flights <- read.csv("flights.csv", stringsAsFactors =FALSE) #스트링형을 팩터형으로 바꾸는 성질있음.
library(dplyr)

str(flights)
flights <- na.omit(flights)
attach(flights)
sum(is.na(flights))
#dest : destination
filter(flights, dest %in% c("SFO", "OAK")) #조건에 일치하는 것만 필터링
filter(flights, dest =="SFO" | dest =="OAK")
filter(flights, dep_delay >60)

filter(flights, arr_delay > 2 * dep_delay) #도착지연 > 2*출발지연

select(flights, arr_delay, dep_delay)
select(flights, arr_delay:dep_delay)
head(select(flights, ends_with("delay")),5)
select(flights, contains("delay"))



arrange(flights, date, hour, minute)
arrange(flights, desc(dep_delay)) #desc 내림차순
arrange(flights, desc(arr_delay))

arrange(flights, desc(dep_delay - arr_delay))

str(flights)
flights <- mutate(flights, speed=dist /((arr-dep)/60) ) #경과시간을 60으로 나누니까, 시간. "시간당 속도가 된다." 그걸 스피드에 집어 넣어라. == 파생변수
arrange(flights, desc(speed))

mutate(flights, delta = dep- arr)
by_date <- group_by(flights, date)
by_hour <- group_by(flights, date, hour)
mutate(flights, delta = dep - arr)

by_date <- group_by(flights, date)
by_hour <- group_by(flights, date, hour)

by_date <- group_by(flights, date)
delays <- summarise(by_date,
                    mean = mean(dep_delay),
                    median = median(dep_delay),
                    q75 = quantile(dep_delay, 0.75),
                    over_15 = mean(dep_delay > 15),
                    over_30 = mean(dep_delay > 30),
                    over_60 = mean(dep_delay > 60)
)
delays





#문제 : dep가 있는 데이터만 필터링해서 지연시간의 평균, 준위수, 15분 지연, 30분 지연, 60분 지연
#데이터로 요약하시오.

no_missing <- filter(flights, !is.na(dep))
delays <- summarise(no_missing,
                    mean=mean(dep_delay),
                    median = median(dep_delay),
                    q75 = quantile(dep_delay, 0.75),
                    over_15 = mean(dep_delay>15),
                    over_30 = mean(dep_delay>30),
                    over_60 = mean(dep_delay>60))


delays



#문제 : dep_delay 시간이 있는 것만 찾아서 date, hour로 그룹핑하고, 그룹별로 평균 지연시간과 갯수를 카운팅하여
#       갯수가 10개 이상인 것만 출력해 보시오.

hourly_delay <- filter(
  summarise(
    group_by(
      filter(
        flights,
        !is.na(dep_delay)
      ),
      date, hour #날짜와 시간별로 데이터 그룹핑
    ),
    delay = mean(dep_delay), #날짜와 시간별로 지연시간의 평균
    n = n() #날짜와 시간별로 지연된 비행기의 카운팅함수
  ),
  n>10
)
hourly_delay






#문제 : 위의 해를 파이프라인으로 표현 %>%

hourly_delay <- flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date, hour) %>%
  summarise(delay = mean(dep_delay), n=n()) %>%
  filter(n>10)





# 문제1
# 목적지별로 그룹핑하고 도착지연시간의 평균을 구하고 그 갯수를 카운팅하여 도착지연시간의 평균으로
# 내림차순으로 정렬하여 출력하시오

arr_delay_by_destination <- group_by(flights, dest) %>%
  summarise(delay = mean(arr_delay), n=n()) %>%
  arrange(desc(delay))

arr_delay_by_destination



#문제2
# carrier, flight, dest 별로 그룹핑한 다음 정렬하고 이중 갯수가 365개 이상인 것만 출력하시오.

group_by_cfd <- filter(
  summarise(
    group_by(
      filter(flights, !is.na(carrier), !is.na(flight), !is.na(dest)),carrier, flight, dest #그룹핑
    ),
    n = n() #날짜와 시간별로 지연된 비행기의 카운팅함수
  ),
  n>365
)

group_by_cfd

#문제3
# 취소되지 않은 (cancelled ==0 ) 비행기 편중을 필터링하여 time= hour+ minute/60 으로 파생변수를 생성하고
# time으로 그룹핑한 결과를 arr_delay = mean(arr_delay, na.rm=TRUE)를 구하고 갯수를 카운팅하시오

answer <- filter(flights, cancelled==0)
time = hour + minute/60
group_by(flights, time)



#문제4
# 위 3번에서 구한 결과에서 time과 arr_delay의 관계를 그라프로 시각화 하시오.

#문제5
# 위 3번 데이터를 이용하여 per_hour과 n>30보다 큰 것만 필터링 한 다음 scale_size_area()함수를 레이어로 추가하여
# 그라프로 표현하시오


#문제6
# 도착 지연시간이 있는 데이터만 필터링하고 plane 별로 그룹핑한 다음 30개 이상인 데이터만 필터링하여 출력하시오.


filter(summarise(group_by(filter(flights,!is.na(arr_delay)),flight),n=n() ),n>30)
