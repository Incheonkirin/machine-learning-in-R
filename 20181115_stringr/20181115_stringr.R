#문자열 처리함수
 #Base와 stringr
 #리턴값을 다른 함수의 매개변수로 전달하기 용이
 #matacharacter
  # . : 1문자와 일치, | : or, [] : 선택, () : group
  # * : 0회 이상, + : 1회 이상, ? : 0또는 1회
  # {m}, {m,}, {m,n}
 #nchar        str_length
 #paste        str_join
 #substr()     str_sub
 #strsplit     str_split
 #sub()        str_replace
 #grep         str_extract
 #regexp       str_locate
 #gregexp      str_locate_all      #이것까지만 이해하고, 다른 것들은 찾아서 쓰면 된다.

empty_str = ""
class(empty_str)
empty_chr = character(0)
class(empty_chr)
char_vector = character(5)
char_vector[1] = "우리나라"
char_vector
#문자열은 기본이 배열이고 1차원배열로 처리됨.
example = character(0)
example
length(example)
example[1] = "first"
example
example[2] = "second"
example
length(example)
example[4] = "fourth"
length(example)
is.character(example)
b = 8+9
b = as.character(b)
(PI = paste("pi의 값은 =", pi))
rbind(1:5, letters[1:5]) #모두 문자로 취급

head(USArrests) #주별 범죄현황.
(states = rownames(USArrests)) #주별 이름.

substr(x = states, start = 1, stop = 4) #주별 이름을 1번째-4번째 자리로 줄임.
(states2 = abbreviate(states)) #abbreviate 함수를 이용해서 주별 이름을 4글자로 줄임.
names(states2) = NULL #주별 이름을 지우고, abbreviate함수 결과만 출력하도록함.
states2
abbreviate(states, minlength = 5) #abbreviate 함수를 이용해서 주별 이름을 5글자로 줄임.
state_char = nchar(states) #주별 이름의 길이를 변수로 잡음.
states[which(state_char == max(state_char))] #주별 이름이 가장 긴 것을 찾음.

my_string = "programming with data is fun"
print(my_string)
print(my_string, quote = FALSE) # ""없이 출력
noquote(my_string)

cat(month.name[1:4], sep=" ") # cat함수는 개행을 하지않음(줄바꿈을 하지 않음)
format(c(6, 13.1), digit = 2)
format(c(6, 13.1), digit = 2, nsmall = 1)
format(c("A", "BB", "CCC"), width = 5, justify = "centre") #정렬.

sprintf("%f", pi) #f:float
sprintf("%.3f", pi) #소수점 자리수 3번째까지 출력.
sprintf("%-10f", pi) #왼쪽 정렬해서 출력.
sprintf("%e", pi) #지수형으로 출력.

#문자열 변환
toString((c("Bonjour", 123, TRUE, NA, log(exp(1))))) #문자형이 반환됨.
toString(c("one", "two", "3333333333"), width = 8)
tolower(c("President", "ABCDE")) #소문자로 변환.

#문자갯수를 세어줌.
nchar("How many characters?")

#문자열 추출
x=c("may","the","force","be","with","you")
substr(x, 2, 2)                    #추출 #2번째글자에서 2번째글자까지 추출해라.
substr(x, 2, 2) <- "#"             # 2번째 글자에서 2번째 글자까지 "#"으로 대체
x

#집합연산
set1 = c("오리", "하늘위", "문자열") #프로그램적 이해, 함수적 목적.
set2 = c("오리", "모두다", "아무도", "함께해")
set3 = c("문자열", "하늘위", "오리")
union(set1, set2) #합집합
intersect(set1, set2) #교집합
setequal(set1, set2) #동등
identical(set3, set3) #같은지 확인.
is.element('오리', set3) #요소확인
sort(set1)
paste(rep("x",4), collapese ="")


# 정규표현식을 이용하는 함수들.

head(USArrests)
(states = rownames(USArrests))
grep(pattern = "k", x = states) #k가 들어있는 주 이름의 인덱스 출력 #x=states 뒤에, "value=TRUE"없으면, 결과를 인덱스값으로 알려줌..
class(grep(pattern = "k", x = states))
grep(pattern = "k", x = states, value = TRUE)#k가 들어있는 주 이름을 출력. # "value =TRUE" 를 포함시키면, 찾은 값을 문자열로 알려준다.
grep(pattern = "w", x = states, value = TRUE)
grep(pattern = "[wW]", x = states, value = TRUE) #대소문자 다 찾기
grep(pattern = "w", x = tolower(states), value = TRUE) #소문자/대문자로 바꿔서 찾기.
grep(pattern = "W", x = toupper(states), value = TRUE)
grep(pattern = "w", x = states, value = TRUE, ignore.case = TRUE) #ignore.case == 대소문자 구문 무시.
hist(nchar(states), main = "문자수", xlab = "미국 주이름의 문자수")
(positions_a = gregexpr(pattern = "a", text = states, ignore.case = TRUE)) #개별적으로 설명 #그런데.. gregexpr은 잘 안씀.

num_a = sapply(positions_a, function(x) ifelse(x[1] >0, length(x), 0))
num_a

grepl(pattern= "w", x = states, ignore.case = TRUE) #logical 로 결과 출력


#install.packages("stringr")
library(stringr)

money = "$money" #문자열 $달러
sub(pattern = "$", replacement = "", x = money) #     문자열$를 찾으려고 하는데, $는 정규표현식에서 '마지막'이라는 의미로 쓰임. 즉 '$'만 쓰게되면 정규표현식으로 쓰여서, 문자열 $달러를 찾지 못함. 그렇기 때문에, \\ 를 붙여줘야지만, 문자$를 찾을 수 있다.
sub(pattern = "\$", replacement = "", x = money) #   \ 한개만 붙이면 에러가 난다. 두개 붙여줘야 함.
sub(pattern = "\\$", replacement = "", x = money) # 정규표현식에서 escape 문자로 처리.

#substritute 대체
sub("\\$", "", "$Peace-Love")
sub("\\.", "", "Peace.Love")
sub("\\+", "", "Peace+Love")
sub("\\^", "", "Peace^Love")
sub("\\|", "", "Peace|Love")
sub("\\(", "", "Peace(Love)")
sub("\\)", "", "Peace(Love)")
sub("\\[", "", "Peace[Love]")
sub("\\]", "", "Peace[Love]")
sub("\\{", "", "Peace{Love}")
sub("\\}", "", "Peace{Love}")
sub("\\\\", "", "Peace\\Love") #escape 문자들
sub("\\d", "_", "the dandelion war 2010") #숫자
gsub("\\d", "_", "the dandelion war 2010")
sub("\\D", "_", "the dandelion war 2010")#숫자아닌것(문자)
gsub("\\D", "_", "the dandelion war 2010")

transport = c("car", "bike", "plane", "boat")
grep(pattern = "[ei]", transport, value = TRUE) #transport 중 e와 i가 들어간 것을 출력하라.

numerics = c("123", "17-April", "I-II-III", "R 3.0.1")
grep(pattern = "[01]", numerics, value = TRUE)
grep(pattern = "[0-9]", numerics, value = TRUE)
grep(pattern = "[^0-9]", numerics, value = TRUE) #선택안에서의 ^는 부정 #숫자가 아닌게 있는 것을 출력하라.

la_vie = "그라프에서 컬러는 #FFC0CB (rose);\nCes't la vie! \ttres jolie"
print(la_vie)
gsub(pattern = "[[:blank:]]", replacement ="", la_vie) #gsub은 대체/ blank는 공백/  "" 는 아무것도 없음 / == 공백 찾아서 없애라
gsub(pattern = "[[:punct:]]", replacement ="", la_vie) # ! . " , % & () * + - / : ; 를 제거
gsub(pattern = "[[:xdigit:]]", replacement ="", la_vie) # hexa digit  를 제거
gsub(pattern = "[[:print:]]", replacement ="", la_vie) #alpha, punct, 공백    를 제거
gsub(pattern = "[^[:print:]]", replacement ="", la_vie)
gsub(pattern = "[[:graph:]]", replacement ="", la_vie) #alpha, punct,   를 제거
gsub(pattern = "[^[:graph:]]", replacement ="", la_vie)



#횟수를 조절
people = c("rori", "emilia", "matteo", "mehmet", "filipe", "anna", "tyler",
           "rasmus", "jacob", "youna", "flora", "adi")
grep(pattern = "m?", people, value = TRUE)  # 물음표는 0회 혹은 1회    # m이 0개 이상 나온건 다 출력.
grep(pattern = "m{1}", people, value = TRUE, perl = FALSE) # m이 1회 라도 온 것.
grep(pattern = "m*t", people, value = TRUE) # m이 1회 이상, 그리고 t.
grep(pattern = "t*m", people, value = TRUE) # t가 여러번 오고 m이 온 것. m이 안와도 됨.1
grep(pattern = "m+", people, value = TRUE) # 무조건 m이 들어오고, m이 10장있어도 됨.
grep(pattern = "m+.t", people, value = TRUE) # m이 있고, 뭔가하나 있고, t가 오는것
grep(pattern = "t{2}", people, value = TRUE) # T가 2회인 것.



#문제 :: 다음 데이터의 소수점을 제거하시오
data <- c("12.57869486", "12.57869582", "12.57870155")

##선생님 답
a <- gsub(".","",data)
a
# 위와 같이 하면, 정규표현식으로 인식되기 때문에 안된다.
#아래와 같이 역슬래쉬 2개를 앞에 찍어주어야 한다.
gsub("\\."," ",data)
gsub("[.]"," ",data)


library(stringr)
str_length(c("i", "like", "programming", NA))

str_c("May", "The", "Force", "Be", "With", "You")

lorem = "Lorem Ipsum"
str_sub(lorem, start = 1, end = 5)
str_sub(lorem, seq_len(str_length(lorem))) #길이를 잰 다음에 seq_len 해주었다. seq와 같은 방법으로 길이만큼 만들어준다.
str_sub(lorem, c(1,7), c(5,8)) <- c("Nullam", "Enim")
lorem
str_sub("adios", 1:3) #시작지점에 vector가 입력.

#공백제거
bad_text = c("This", "example ", "has several ", " whitespaces ")
str_trim(bad_text, side="left")
str_trim(bad_text, side="both")

#채우기
str_pad("hola", width=7)
str_pad("adios", width=7, side="both")
str_pad("hashtag", width=9, side="both", pad="-")

#첫단어 추출
change = c("Be the change", "you want to be")
word(change, 1)


#factor와 stringr
some_factor = factor(c(1,1,1,2,2,2), labels = c("good", "bad"))
some_factor
nchar(some_factor)                   #nchar로 하면 세어지지 않음.
str_length(some_factor)              #str_length로 해야지 정상적으로 세어짐

#stringr과 정규표현식
str_extract("abcd12aaa33", "[1-9]{2}")
str_extract_all("abcd12aaa33", "\\d")
str_extract_all("abcd12aaa33", "[1-9]{2}")

d<- c("에이콘", "종로구", "강남", "신촌")
str_replace(d, "에이콘", "종로구")
str_replace(d, "에+","종로구")

fruit <- c("apple", "banana", "pear", "pineapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")
str_count(fruit, c("a"))
str_count(fruit, c("a", "b"))
str_locate(fruit, "e")
str_locate(fruit, "na")





#문제 :: 다음 문자열에서 알파벳만 추출하시오
chr5 = "123ab456"
chr6 = "123한글나라456"

##정답
str_extract( chr5, "[a-z]+")
str_extract(chr6, "[가-흐]+")





#문제 :: 다음 문자열에서 정규표현식을 이용하여 전화번호를 추출하시오
chr6 = "abdbsdsa 010-1234-5678 dasdasd"

##정답
pat <- "[0-9]{3}-[0-9]{4}-[0-9]{4}"
str_extract( chr6, pat)







fruits1 <- c(" 사과 한 개", "배 두개", "바나나 세개 ")
str_replace(fruits1, "[개]", " 물건")
str_replace_all(fruits1, "나+", "nana")




#문제 :: 다음 전화번호 조합을 모두 추출해보시오
strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
             "239 923 8115", "842 566 4692", "work: 579-499-7527", "$1000",
             "Home: 543.355.3679")

##정답
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})" # 여기서 점 . 은 개행을 제외한 모든 것과 매칭
str_extract(strings, phone)

chr1 = "AB-CD"
str_split( chr1, "-")
str_replace( chr1, "-", "+" )
str_sub(chr1, 2,4)
paste( "AB", "CD")


#문제 :: 다음 문자열을 이용하여 문제에 답하시오.
shoppinglist <- c("2017-10-19 수입3000원","2017-10-20 수입4500원","2017-10-21 수입2500원")

# 1) 일자별 수입중에 다음과 같은 형태로 출력하시오
# 결과 ) "3000원" "4500원" "2500원"

# 2) 금액만 추출하시오

# 3) 수치를 제거하여 다음과 같이 출력하시오
# 결과 ) "-- 수입원" "-- 수입원" "--수입원"

# 4) 위 데이터 값 중에 -를 /로 치환하시오

# 5) 위의 데이터 중 날짜만 모두 추출하시오

# 6) 위 데이터 중에 다음과 같은 형식으로 출력하시오
# 결과 ) "수입3000원" "수입4500원" "수입2500원"


##정답
## 1) str_extract_all( shoppinglist, "[0-9]{4}원")
## 2) shoppinglist2 <- str_split(shoppinglist, " ", simplify = TRUE)
##    shoppinglist3 <- shoppinglist2[,2]
##    str_extract(shoppinglist3, "[0-9]{4}")
## 3) gsub(pattern = "\\d", replacement ="", shoppinglist)
##    str_replace_all(shoppinglist, pattern= "\\d", replacement= "")
## 4) str_replace_all(shoppinglist, "-", "/")
## 5) str_extract_all(shoppinglist, "[0-9]{4}[-][0-9]{2}[-][0-9]{2}", simplify=TRUE)
##    str_extract_all(shoppinglist, "\\d{4}[-]\\d{2}[-]\\d{2}", simplify = TRUE)
## 6) str_extract_all(shoppinglist, "\\w{2}[0-9]{4}원")



#mongolite
library(ggplot2)
library(dplyr)
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
install.packages("mongolite")
library(mongolite)

dmd <- mongo(url = "mongodb://192.168.0.46", collection="diamonds", db="testdb") #diamonds는 로컬에 있는 데이터프레임인데,
dmd$insert(ggplot2::diamonds)

# 자동으로 데이터 변환되어서 저장됨. R -> mongodb
dmd$count() == nrow(ggplot2::diamonds) #monngodb에 저장될 때 bson(binary json)으로 저장됨. R에서는 dataframe으로 작업함.

dmd$count('{}')
alldata <- dmd$find('{}')
print(alldata)
# 조건검색
premium_diamond <- dmd$find('{"cut": "Premium", "price":{"$lt":1000}}')
class(premium_diamond) # 다시 검색해서 가져올 때는 ,data.frame으로 바뀜.

print(premium_diamond)
nrow(premium_diamond)
test <- dmd$find(
  query ='{"cut" : "Premium", "price" : { "$lt" : 1000} } ',
  fields = '{"cut" : true, "clarity" : true}',
  limit = 5
)

print(test)


#문제 ::        cut : Premium인 데이터를 price로 정렬해서 7개만 가지고 오시오
dmd$find('{"cut" : "Premium"}', sort= '{"price": -1}', limit=7)
dmd$index(add = '{"price" :1}') #인덱스 정의

#개별적으로 데이터 출력
it <- dmd$iterate('{"cut" : "Premium"}', sort = '{"price": -1}', limit=7)
while(!is.null(x<- it$one())){
  cat(sprintf("Found %.2f carat diamond for $%d\n", x$carat, x$price))
}
dmd$export(file("dump.json")) # mongodb 3T 를 이용해서 import export했던 것. #백업.
dmd$drop() #드랍할때 써줌.
dmd$count()
dmd$import(file("dump.json"))
dmd$count()
# dump.json이 저장된 곳에 가서 , sublimetext3로 열어보기.
dmd$disconnect()



mydata <- jsonlite::stream_in(file("dump.json"), verbose=FALSE) #json파일을 R에서 바로 로딩하기.
print(mydata)

# 문제 iris 데이터를 isis collection으로 저장하고 다음 질문에 답하시오

# 1) iris 데이터 입력


# 2) 전체 데이터 쿼리 후 5개만 출력

# 3) 전체 데이터 갯수를 확인.
# 4) species가 setosa인 데이터만 모두 지우시오
# 5) Sepal length가 5보다 작은 데이터를 삭제하시오

# 6) 작업을 종료하기 위하여 session을 정리하시오.



## 정답

## 1) dmd <- mongo(url = "mongodb://192.168.0.46", collection="iris", db="testdb")
##    class(iris)
##    test$insert(iris)

## 2) test$find(limit=5)

## 3) test$count()

## 4) test$remove('{"species":"Setosa"}')

## 5) test$remove('{"Sepal_Length" : {"$lt" :5 }}', just_one = TRUE)

## 6)test$drop()
##   test$disconnect()






#R - thread 한개 ---> arregate(집게함수), map reduce(함수를 적용하고 집계)
#                     이걸 이용하면 작은 메모리로 큰 데이터 처리 가능하다 !


admin <- mongo(url = "mongodb://192.168.0.46", db="damin") #어드민 명령어 실행 할 수 있다.
admin$run('{"listDatabase":1}')
admin$disconnect()

# E-mail 확인 #해당 파일을 로딩해보아라.

flt <- mongo(url = "mongodb://192.168.0.46", collection = "flights", db="testdb")
#다음과 같이 읽어들이면 된다.
flt$import(file("flights.bson"), bson=TRUE)

#'디스크를 활용'해서 'carrier'를 기준으로 '그룹핑'하고, '그룹별로 갯수를 카운트'하고 '거리값의 평균'.
stats <- flt$aggregate(
  '[{"$group":{"_id":"$carrier","count":{"$sum":1},"average":{"$avg":"$distance"}}}]',
  options = '{"allowDiskUse":true}'
)
print(stats)
#이렇듯 대량으로 작업할 것을, 몽고디비에서 1차 가공하고, 데이터를 이용해서 R에서 작업. - Thread
# Hadoop 에서 mongoDB를 사용하는 추세. mongoDB도 sharding을 통해서 함. (sharding 시스템을 실시간으로 추가할 수 있도록 구성. 분산시스템)


names(stats) <- c("carrier", "count", "average")
print(stats)
library(ggplot2)
ggplot(aes(carrier, count), data=stats) + geom_col()


#어그리게이트를 이터게이트로 가지고 올 수 도 있다. 아래와 같이 제이슨포멧으로 볼 수 도 있다.
iter <- flt$aggregate(
  '[{"$group":{"_id":"$carrier", "count": {"$sum":1}, "average":{"$avg":"$distance"}}}]',
  options = '{"allowDiskUse":true}',
  iterate=TRUE
)
iter$json(10)
##map reduce : aggregation, 함수를 적용해서. javascrippt를 적용.
histdata <- flt$mapreduce(
  map = "function(){emit(Math.floor(this.distance/100)*100, 1)}",   #정수화하고 (키이, 데이터)형태로.
  reduce = "function(id, counts){return Array.sum(counts)}" #갯수를 카운팅
)

names(histdata) <- c("distance", "count")
print(histdata)
library(ggplot2)
ggplot(aes(distance, count), data=histdata)+geom_col()

flt$disconnect()

# GridFS 파일을 단위처리하는 데이터베이스 (파일 데이터베이스)

fs <- gridfs(url="mongodb://192.168.0.46", db="testdb", prefix="fs")
print(fs)
fs$upload(path="flights.bson", name=basename("flights.bosn"))
fs$find()
out <- fs$read('flights.bson', file('flight_mongo.bson'), progress = FALSE)
buf <- serialize(mtcars, NULL)
fs$write(buf, 'mtcars', progress = FALSE)
fs$find()
out<-fs$read('mtcars',con=NULL,progress=FALSE)  #con은 원격으로 조정. #progress는 진행상황 보여주는것.
df <- unserialize(out$data)
head(df)
fs$drop()
fs$disconnect()


install.packages("lubridate")
library(lubridate)
install.packages("gridExtra")
library(gridExtra)

crimes <- data.table::fread("testdata.csv")
names(crimes)
names(crimes) = gsub(" ","",names(crimes))
names(crimes)

my_collection <- mongo(url="mongodb://192.168.0.46", collection="crimes", db="testdb")
my_collection$insert(crimes)
my_collection$count()
my_collection$iterate()$one()
my_collection$count('{"PrimaryType":"ASSAULT","Domestic":"true"}')

query2= my_collection$find('{"PrimaryType":"ASSAULT", "Domestic":"true"}',
                           fields = '{"_id":0, "PrimaryType":1, "Domestic":1}')

ncol(query2)
head(query2)
#mongodb query + dplyr
#프로젝트 - 데이터 몽고디비에 싣고, 몽고디비에 싣으면, 대용량데이터도 처리할 수 있다.
my_collection$aggregate('[{"$group":{"_id":"$$LocationDescription", "Count": {"$sum":1}}}]') %>%
  na.omit() %>%
  arrange(desc(Count))%>% head(10)%>%
  ggplot(aes(x=reorder(`_id`,Count),y=Count))+
  geom_bar(stat="identity", color='skyblue', fill='#b35900') + geom_text(aes(label = Count), color ="blue") +
  coord_flip() + xlab("Location Description")
my_collection$disconnect()


#mysql-RMySQL      :: 이번에는 mySQL을 사용해보겠습니다.

install.packages("dbplyr")
library(dbplyr)
library(DBI)
install.packages("RMySQL")
library(RMySQL)
install.packages("rstudioapi")
library(rstudioapi)

#db는 utf8 이고 R은 cp949 코드로 불일치한다.
#엑셀은 cp-949코드이다.
mysqlconnection <- dbConnect(MySQL(), user='root', password="acorn1234", dbname="acorn", host="192.168.0.46", encoding="UTF8")
dbListTables(mysqlconnection)

(trial_read=dbReadTable(mysqlconnection, "student"))
trial_read[[2]] <- iconv(as.character(trial_read[[2]]), from='UTF-8') #[[2]]해주는 이유=> 요소로 접근해서 변경하기 위함.
trial_read
dbDisconnect(mysqlconnection)

result = dbSendQuery(mysqlconnection, "select * from student")
data.frame = fetch(result, n=5)
iconv(as.character(data.frame["name"]),from='UTF-8')
data.frame["name"] <- iconv(data.frame["name"], "UTF-8", "CP949")
print(data.frame)
dbDisconnect(mysqlconnection)




install.packages("nycflights13")
library(nycflights13)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname = "acorn",
                      host="192.168.0.46",
                      user="root",
                      password= rstudioapi::askForPassword("Database password")
)
copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE,
        indexes = list(
          c("year","month","day"),
          "carrier",
          "tailnum",
          "dest"
        )
)
flights_db <- tbl(con, "flights")
flights_db
flights_db %>% select(year:day, dep_delay, arr_delay) #dplyr과 연결해서 사용
flights_db %>% filter(dep_delay > 240)
flights_db %>% group_by(dest) %>% summarise(delay = AVG(dep_time))

dbDisconnect(con)



install.packages("XLConnect")
require(XLConnect)
wb = loadWorkbook("excel_sungjuk.xlsx")
(df = readWorksheet(wb, sheet = "Sheet1", header = TRUE))
(df2 = readWorksheet(wb, sheet = "Sheet2", header = TRUE))
sapply(df, class)

df$국어
df[1,]
df[,5] <- rowSums(df[,2:4])
df[,6] <- df[,5]/3
df[4,2:5] <- colSums(df[,2:5])
(round(df[,6] <- df[,5] /4, 2))
df
result = df
#워크시트를 생성 -> 데이터를 입력하고 -> 워크시트의 이름을 주고, 영역에 이름을 주고.
df3 = create_sheet= createSheet(wb, name= "result")
createName(wb, name = "result_data_pos", formula = "result!$C$5")
writeNamedRegion(wb, result, name="result_data_pos") #
writeWorksheetToFile(df3)

createSheet(wb, name = "mtcars")
writeWorksheet(wb, mtcars, sheet = "mtcars")
setActiveSheet(wb, sheet= "mtcars3")

saveWorkbook(wb) #전체를 저장

#워크북을 SB에 저장
con <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname="acorn",
                      host="192.168.0.46",
                      user="root",
                      password="acorn1234",
                      encoding="UFT8"
)

wb[[1]] <- iconv( wb[[1]], "CP949", "UTF-8") #엑셀 CP949코드를 사용 => DB : UFT8사용
name(wb)<- iconv( names(wb), "CP949", "UTF-8")
dbWriteTable(con, 'df', wb[,], overwirte= TRUE, fileEncoding="UTF-8")
result = dbSendQuery(mysqlconnection, "select*from df")
dbDisconnect(con)
