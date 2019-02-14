install.packages("rAmCharts")
library(rAmCharts)
library(dplyr)
data <- data.frame(label =c("A", "Z", "E", "R", "T"),
                   Product1 = c(1, 2, 3, 4, 2),
                   Product2 = c(2, 8, 1, 1, 0),
                   Product3 = c(1, 1, 2, 2, 4))
amRadar(data = data)
amRadar(data = data, backTransparency = 1)
amRadar(data = data, backTransparency = c(0, 0.4, 0.6))
amRadar(data = data, type = "circles")
amRadar(data = data, pch = "bubble", backTransparency = 0) %>%
  amOptions(legend = TRUE)

data("data_candleStick2")
head(data_candleStick2)
amOHLC(data = data_candleStick2, labelRotation = -45) #open/high/low/close
amCandlestick(data = data_candleStick2, labelRotation = -45)

data(data_stock_3)
amStockMultiset(data = data_stock_3)

# 문제 :
# 1) 다음 미국 범죄 현황에서 주별 이름 중 각 이름에서 모음이 몇개나 들어 있나.
# 확인해서 그라프로 출력하시오(파싱문제) (a,e,i,o,u 몇개 있는지 barplot)
head(USArrests)
(states = rownames(USArrests))

states <- tolower(states)



barplot(max.temp,
        main = "모음의 갯수",
        ylab = "갯수",
        names.arg = c("a", "e", "i", "o", "u"),
        col = "darkred",
        horiz = TRUE) #horizontal /vertical


# 2) 시각화 문제 다음 데이터를 rader를 이용하여 비교할 수 있도록 2행 3열로 시각화 해 보시오.

# 이름     국어  영어  수학  과학  기술
# 전공일    90    88    65    86    76
# 전공이    84    77    66    55    77
# 전공삼    74    66    54    34    55
# 전공사    95    55    99    99    56
# 전공오    56    44    75    86    92
# 전공육    63    33    45    56   100

data <- data.frame(label =c("전공일", "전공이", "전공삼", "전공사", "전공오","전공육"),
                   Kor = c(90,84,74,95,56,63),
                   Eng = c(88,77,66,55,44,33),
                   Mat = c(65,66,54,99,75,45),
                   Sci = c(86,55,34,99,86,56),
                   Tec = c(76,77,55,56,92,100))
amRadar(data = data)
amRadar(data = data, backTransparency = 1)
amRadar(data = data, backTransparency = c(0, 0.4, 0.6))
amRadar(data = data, type = "circles")
amRadar(data = data, pch = "bubble", backTransparency = 0) %>%
  amOptions(legend = TRUE)

data("data_candleStick2")
head(data_candleStick2)
amOHLC(data = data_candleStick2, labelRotation = -45) #open/high/low/close
amCandlestick(data = data_candleStick2, labelRotation = -45)

data(data_stock_3)
amStockMultiset(data = data_stock_3)


# 3) 영화 입장권 통합 전산망을 검색하고 주소로 입장하여 다음 처리 절차대로 처리한다음, (anaconda)
# 원하는 데이터를 파싱하여 mongoDB에 저장하시오.
#    1) 다음 주소에서 openAPI 사용법을 숙지하고 키를 획득하시오
#       http://www.kobis.or.kr/kobisopenapi/homepg/apiservice/searchserviceInfo.do
#    2) 획득한 키를 이용하여 다음과 같이 요청하면 데이터를 수집이 가능
#       http://www.kobis.or.kr/kobisopenapi/webservice/rest/movie/searchMovieInfo.json?
#        key=43015....
#    3) 원하는 데이터를 파싱한 다음 몽고 DB에 저장.


# 문제4 wetherAUS.csv 를 로딩하여 다음 과정을 처리하시오.
#    1) NA수를세고 결측치를 처리하시오

wether <- read.csv("weatherAUS.csv", stringsAsFactors =FALSE)
library(dplyr)



#    2) 이상치를 확인하고 처리하시오(boxplot)
#    3) 데이터를 7:3 으로 샘플링하시오(sampling)
#    4) 위의 데이터에서 각자3가지 정보(특징) 을 찾아내고 이를 시각화하시오.
#      예) 오후3시의 습도는 내일 비내림과 상관있음(pairs)
#      예) 장소별 최고온도는 다음과 같음


# 5) election_2012.csv 파일을 로딩한 다음, 다음 내용을 해결하시오.
#   0) election_2012.csv를 로딩하여 MongoD에 저장합니다.
#   1) 다음 데이터만 선택하여 작업하도록 mongoDB에 쿼리한 다음 조건을 만족하도록
#     data.frame을 준비하시오. -(mongoDB에서 처리하시오)-
#     1-1) 조건 : 다음 필드로만 구성합니다.
#     1. cand_id: 2. cand_nm: 대선 후보자 이름 3. contbr_nm: 후원자 이름
#     4. contbr_occupation: 후원자 직업군 10. contb_receipt_amt: 후원금
#     1-2) 'Romney, Mitt'와 'Obama, Barack' 대통령 후보자 별로 subset을 생성하시오.
#     1-3) romney와 obama로 저장.
#
#   2) 각 후보자의 데이터 수를 확인하고 그 내용을 head로 확인하시오.
#   3) romney와 obama 변수를 대상으로 후원금이 600달러 이상인 후원자를 추출하고, 다음을 처리하시오.
#   4) 추출된 결과를 다음 변수에 저장하시오 : romney_6000over, obama_6000over
#   5) 각 후보자 별로 후원자 수를 파악하시오 ( 중복 한 경우를 제거한 후)
#   6) 각 후보자 별로 가장 많은 후원금의 기부자의 이름과 직업군은?
#   7) romney와 obama변수를 대상으로 직업군이 공백인 관측치를 제거하여 서브셋을 생성하시오.
#     조건1 : romney2, obama2 변수 저장
#     조건2 : 공백 제거 전고ㅘ 후 관측치 차이 계산
#   8) romney2와 obama2 변수를 MongoDB에 'romney'와 'obama'이름 파일로 저장합니다. (GridFS)
#   9) romney, obama를 MongoDB에서 읽어 옵니다. 파일을 읽어와서 다음과 같이 처리하시오.
#     조건1 : 저장할 변수명 : romney3, obama3
#     조건2 : 후보지별 직업군이 "RETIRED"인 후원금만 추출하여 합계 계산
#     조건3 : 출력 결과 : ooo 후보자의 후원금 합계 : ooo원
#     조건4 : romney3, obama3 변수를 대상으로 각 후보자별 가장 많은 후원자의 직업군 3개씩 확인하시오.
#  10) 데이터로부터 한가지 정보씩을 추가로 확인하시오.
