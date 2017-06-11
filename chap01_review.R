# chap01_review

##########################
### Part-I 총정리 
##########################

# file.choose()함수를 이용하여 election_2012.csv 파일을 election 변수로 읽어온다.
election <- read.csv(file.choose()) # 2012년 미국 대선 후원금 현황 데이터 셋  , # stringsAsfactor=F 사용 가능 
str(election)
# 데이터 셋 설명 : 2012년 미국 대선자('Romney, Mitt'와 'Obama, Barack') 후원금 현황
# 'data.frame':	1001731 obs. of  16 variables:
# 2. cand_id : 대선 후보자 id
# 3. cand_nm : 대선 후보자 이름
# 4. contbr_nm : 후원자 이름 
# 9. contbr_occupation : 후원자 직업군 
# 10. contb_receipt_amt: 후원금 


## 필요한 칼럼 추출 ## 

# 문제 1) 위 5개 칼럼을 대상으로 election_df 이름으로 데이터 프레임을 생성하시오.
# (1) 칼럼 수를 이용하는 방법 
election_df <- election[c(2:4,9,10)]
str(election_df) # 'data.frame':	1001731 obs. of  5 variables:

# (2) 칼럼명을 이용하는 방법
election_df <- election[c('cand_id','cand_nm','contbr_nm','contbr_occupation','contb_receipt_amt')]
str(election_df) 
head(election_df)


## subset()함수 ##

# 문제 2) 'Romney, Mitt'와 'Obama, Barack' 대령통 후보자 별로 서브셋(subset)을 생성하시오.
# <조건1> romney와 obama 변수명으로 저장 
# <조건2> 각 후보자의 차원 보기 - dim() 함수 
# <조건3> 앞부분/뒷부분 6줄 관측치 확인 - head()/tail() 함수 
# 형식) 변수 <- subset(dataset, 조건식)
romney <- subset(election_df, cand_nm == 'Romney, Mitt')
obama <- subset(election_df, cand_nm == 'Obama, Barack')

dim(romney) # 107229      5
dim(obama) # 593746      5

head(romney); tail(romney)
head(obama); tail(obama)


# 문제 3) romney, obama 변수를 대상으로 후원금이 7000 달러 이상인 후원자들을 추출하여 
#   다음과 같이 처리하시오.
# <조건1> 추출된 결과 저장 변수 : romney_7000over, obama_7000over 
# <조건2> 각 후보자별로 후원자 수는 몇명인가 ?
# <조건3> 각 후보자별로 가장 많은 후원금의 기부자의 이름과 직업군은 ?

romney_7000over <- subset(romney, contb_receipt_amt >= 7000)
obama_7000over <- subset(obama, contb_receipt_amt >= 7000)
dim(romney_7000over) # 6  5
dim(obama_7000over) #  10  5
romney_7000over # NORPAC - 12,700
obama_7000over # OBAMA VICTORY FUND 2012 - UNITEMIZED, - 2,014,490.5


## 파일 입출력 ## 

# 문제 4) romney, obama 변수를 대상으로 직업군이 공백인 관측치를 제거하여 서브셋을 생성하시오.
# <조건1> romney2, obama2 변수 저장 
# <조건2> 공백 제거 전과 후 관측치 차이 계산  
# <조건3> romney2와 obama2 변수를 romney.csv와 obama.csv 파일 저장(행번호 제외)
#    파일 저장 경로 : c:/Rwork/output

# 공백 제거 전 관측치 
dim(romney) # 107229      5
dim(obama) # 593746      5

# 직업군 공백없는 관측치 추출 
romney2 <- subset(romney, contbr_occupation != '') # 직업군 공백 제외 
# 공백과 NA는 다르다 
obama2 <- subset(obama, contbr_occupation != '')
dim(romney2) # 106136      5
dim(obama2) # 589436      5
# 공백이 제거된 것을 알 수 있음

# 파일 저장 
setwd('c:\\Rwork-II\\data')
write.csv(romney2, 'romney.csv', row.names = F) # 행번호 없이 저장 
write.csv(obama2, 'obama.csv', row.names = F)


## 제어문 ##

# 문제 5) romney.csv, obama.csv 파일을 읽어와서 다음과 같이 처리하시오.
# <조건1> 저장할 변수명 : romney3, obama3
# <조건2> 후보자별 직업군이 'RETIRED'인 후원금만 추출하여 합계 계산
#    힌트) for()함수 이용 
# <조건3> 출력 결과 : OOO 후보자의 후원금 합계 : OOO 원 
#   힌트) cat()함수 이용 

romney3 <- read.csv('romney.csv', header = T)

romney3_retired_amt <- numeric() # 빈 vector 생성
idx = 1 # index 역할 
for(occ in romney3$contbr_occupation){ # 직업군을 vector로 사용 
  if(occ == 'RETIRED'){ # 직업군이 RETIRED인 경우 
    # 후원금을 빈 vector에 저장  
    romney3_retired_amt[idx] <- romney3$contb_receipt_amt[idx]
    idx <- idx + 1 # index 증가 
  }else{ #  RETIRED 아닌 경우 
    idx <- idx + 1 # index 증가
  }
}
# 결과 출력 
cat('romney 후보자의 RETIRED 후원금 합계 :',sum(romney3_retired_amt, na.rm=T), '원')
# romney 후보자의 RETIRED 후원금 합계 : 11266949 원

table(!is.na(romney3_retired_amt)) # 빈도분석표
# FALSE  TRUE 
# 79486 26537 (퇴직자)


obama3 <- read.csv('obama.csv', header = T)

obama3_retired_amt <- numeric()
idx = 1 # index 역할 
for(occ in obama3$contbr_occupation){ # 직업군을 vector로 사용 
  if(occ == 'RETIRED'){ # 직업군이 RETIRED인 경우 
    # 후원금을 빈 vector에 저장  
    obama3_retired_amt[idx] <- obama3$contb_receipt_amt[idx]
    idx <- idx + 1 # index 증가 
  }else{ #  RETIRED 아닌 경우 
    idx <- idx + 1 # index 증가
  }
}
# 결과 출력 
cat('obama 후보자의 RETIRED 후원금 합계 :',sum(obama3_retired_amt, na.rm=T), '원')
# obama 후보자의 RETIRED 후원금 합계 : 25270507 원

25270507 - 11266949 # 14003558(obama 후보자가 더 후원금이 많음)
table(!is.na(obama3_retired_amt)) # 빈도분석표


## 내장함수(sort함수) ##

# 문제 6) romney3, obama3 변수를 대상으로 각 후보자별 가장 많은 후원자의 직업군 3개씩 확인하시오. 
sort(table(romney3$contbr_occupation)) # 빈도수 -> 오름차순 
head(sort(table(obama3$contbr_occupation), decreasing = T),3) # 빈도수 -> 내림차순

head(sort(table(romney3$contbr_occupation), decreasing = T),3) # 빈도수 -> 내림차순




################################
### Part-II 총정리
################################
election <- read.csv(file.choose()) # election_2012.csv
str(election)
# 'data.frame':	1001731 obs. of  16 variables:
# 2. cand_id : 대선후보자 id
# 3. cand_nm : 대선후보자 이름
# 4. contbr_nm : 후원자 이름 
# 5. contbr_city : 후원자 도시 
# 9. contbr_occupation : 후원자 직업군 
# 10. contb_receipt_amt: 후원금 
# 11. contb_receipt_dt:후원날짜 


# 위 7개 칼럼을 대상으로 데이터 프레임을 생성한다.
election_df2 <- election[c(2:5,9:11)]
str(election_df2) # 'data.frame':	1001731 obs. of  7 variables:
dim(election_df2) # 1001731       7
summary(election_df2) # - contbr_occupation(128 NA) # na 를 발견할 때 사용 


## NA 관측치 제거 ##

# 문1) 직업군 칼럼을 대상으로 NA를 포함하는 관측치를 제외하여 clean_election 변수에 저장하시오.
# <조건> 전처리 전과 후 관측치 수의 차이? 

x <- election_df2
clean_election <- subset(x, !is.na(x$contbr_occupation) )
dim(clean_election) # 1001603       7
1001731 - 1001603 # 128


## 관측치 샘플링 ##

# 문2) clean_election 변수를 대상으로 10만개 관측치만 샘플링하여 clean_election2 변수에 저장하시오.

idx <- sample(nrow(clean_election), 100000) # 1:nrow(clean_election)
# 7:3 비율 dataset
idx <- sample(nrow(clean_election), nrow(clean_election)*0.7) # 1:nrow(clean_election)

dim(clean_election2) # 100000      7
nrow(clean_election2) # 100000  
head(clean_election2,6)

clean_election3 <- clean_election[-idx,]
dim(clean_election3)

## 문자열 처리 ##

# 문3) 직업군의 문자열 길이가 20개을 초과한 경우 끝에서 10개 문자열로 직업군의 칼럼을 수정하는 함수를 정의하시오.
#   힌트) stringr 패키지 : str_sub(), str_length() 함수 이용
# <조건1> 대상 변수 : clean_election2 
# <조건2> 함수명 : clean_data
# <조건3> 수정된 내용으로 직업군 칼럼 수정 

clean_election2$contbr_occupation[1:10]

install.packages('stringr')
# 함수 정의 
clean_data <- function(occupation){
  library(stringr)
  clean_occupation <- character() # vector 저장 변수 
  idx <- 1 # index 변수 
  
  for(data in occupation){ # 직업군 vector
    if(str_length(data) >= 20){ # 길이가 20보다 큰 경우 
      clean_occupation[idx] <- str_sub(data, str_length(data)-9, str_length(data))
      idx <- idx + 1
    }else{ # 20 미만인 경우 
      clean_occupation[idx] <- data
      idx <- idx + 1
    }
  }
  return(clean_occupation) # vector변수 반환
}

# 함수 호출 
clean_occupation <- clean_data(clean_election2$contbr_occupation)
clean_occupation[1:10]
names(clean_election2)

# 파생변수 추가
clean_election2$contrbr_occupation2 <- clean_occupation
clean_election2[1:10,]

# 리턴값으로 칼럼 수정 
clean_election2$contbr_occupation <- clean_occupation
clean_election2$contbr_occupation[1:10]


## 막대차트/파이차트 시각화 ## 

# 문4) romney와 obama의 후원자 직업군 빈도수가 상위 10위 해당하는 데이터를 이용하여 시각화 하시오
# <조건1> 작업 대상 변수 : clean_election2
# <조건2> 막대차트 시각화 - 무지개 색 적용, 후원자의 직업군을 x축 눈금 이름으로 표시
# <조건3> 파이 차트 시각화 : romney와 obama 후보자 동시 표현, cex=1.2 속성 지정 
romney <- subset(clean_election2, clean_election2$cand_nm == 'Romney, Mitt')
obama <- subset(clean_election2, clean_election2$cand_nm == 'Obama, Barack')

dim(romney) # 10757     7
dim(obama) # 59245     7

head(romney); tail(romney)
head(obama); tail(obama)

romney_table <- sort(table(romney$contbr_occupation), decreasing = T)
head(romney_table)
barplot(romney_table[1:10], main = 'romney 후원자 직업군', # top10 그리기 
        names.arg = names(romney_table[1:10]),
        col=rainbow(10))
# names(romney_table[1:10]) : 칼럼명 추출 

obama_table <- sort(table(obama$contbr_occupation), decreasing = T)

# 막대차트 
barplot(obama_table[1:10], main = 'obama 후원자 직업군',
        names.arg = names(obama_table[1:10]),
        col=rainbow(10))

# 파이차트 
par(mfrow=c(1,2)) # 두 화면으로 플로팅 
pie(romney_table[1:10], main = 'romney 후원자 직업군 현황', cex=1.2)
pie(obama_table[1:10], main = 'obama 후원자 직업군 현황', cex=1.2)


## 이상치 발견과 정제 ##

# 문5) romney와 obama 후보자 별로 다음과 같이 후원금 칼럼을 처리하시오.
# <조건1> 각 후보자별로 이상치(음수)를 발견하여 정제
# <조건2> 각 후보자별로 정제 전과 후 관측치의 차이 계산  
# <조건3> 각 후보자별로 가장 많은 후원금 찾기 

# 이상치 발견 
summary(romney$contb_receipt_amt)
summary(obama$contb_receipt_amt)
plot(romney$contb_receipt_amt)
plot(obama$contb_receipt_amt)

# 0이상 데이터 정제 
clean_romney <- subset(romney, romney$contb_receipt_amt > 0)
clean_obama <- subset(obama, obama$contb_receipt_amt > 0)

summary(clean_romney$contb_receipt_amt)
summary(clean_obama$contb_receipt_amt)

# 가장 많은 후원금 찾기 
max(clean_romney$contb_receipt_amt) # 10000
max(clean_obama$contb_receipt_amt) # 5000

