## -----------------------------------------------------------
## -- Practical Techniques in R 
##                                  -- 2017. 7. 22. 박민수
## -----------------------------------------------------------


# ------------------------------------------------------------
# - 제어문 사용
# ------------------------------------------------------------

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
length(romney3$contbr_occupation)
table(!is.na(romney3_retired_amt))
#FALSE  TRUE 
#79486 26537(퇴직자) 

summary(romney3$contbr_occupation)
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


## Using FOR Statement_2                                                           
for(i in k){
  cat('i=', i, '\n')
  pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=i)
  t <- table(pred, wdbc_test_y)
  acc[cnt] <- (t[1,1]+t[2,2]) / sum(t)
  cat('분류정확도', acc[cnt], '\n')
  cnt <- cnt +1 # 카운터 
}

acc # 21
sort(acc, decreasing = T) # 내림차순 
# [1] 0.9590643 0.9590643 0.9590643
# k = 6

first <- sort(acc, decreasing = T)[1]
first

for(i in k){
  if(acc[i-4] >= first) {
    cat('k=', i)
    cat(', 분류정확도', acc[i-4], '\n')
  }
}

}


## -------------------------------------------------------------------------
## 함수 정의와 함수 호출 연습
## -------------------------------------------------------------------------

# 문3) 직업군의 문자열 길이가 20개을 초과한 경우 끝에서 10개 문자열로 직업군의 칼럼을 수정하는 함수를 정의하시오.
#   힌트) stringr 패키지 : str_sub(), str_length() 함수 이용
# <조건1> 대상 변수 : clean_election2 
# <조건2> 함수명 : clean_data
# <조건3> 수정된 내용으로 직업군 칼럼 수정 

clean_election2$contbr_occupation[1:10]

# 패키지 설치 
install.packages('stringr')
library(stringr) # 메모리 로딩 

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
# 리턴값으로 칼럼 수정 
str(clean_election2)
names(clean_election2)
# 파생변수 추가
clean_election2$contbr_occupation <- clean_occupation
clean_election2$contbr_occupation[1:10]
names(clean_election2)

## ------------------------------------------------------------------------------
## 이상치 발견 및 정제 
## ------------------------------------------------------------------------------

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

# 가장 많은 후원금 찾기 
max(clean_romney$contb_receipt_amt) # 10000
max(clean_obama$contb_receipt_amt) # 5000

## ---------------------------------------------------------------------------------
## 막대차트, 파이차트 시각화 
## ---------------------------------------------------------------------------------
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
barplot(romney_table[1:10], main = 'romney 후원자 직업군',
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

