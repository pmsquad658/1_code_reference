####################################################
#### Data Manipulation in R ########################
#-------------------- v0.3 _ by 박민수. 2017. 5. 13


##### 초기 setting ########################################

# deletes all existing R objects in global space
rm(list = c(ls()))  
rm(xDims, yDims, xMin, xMax)

# set varname
varname <- c("tmin")
names(user.action.pca.base)
names(allMoVals) <- c("scenario","variable","year","month","model","Value","yrMon","seas")
names(df_data) <- tolower(names(df_data))  # 컬럼값을 소문자로 변환 전처리
colnames(perf_mat) <- c("Naive Bayes", "LR with all variables", "LR with selected variables")
rownames(perf_mat) <- c("TPR", "TNR", "ACC", "BCR")


# infile, outfile
dau <- read.csv("section3-dau.csv", header = T, stringsAsFactors = F)
outFileName = paste("d:/R/raster webinar/webinar_CMIP-A2-", dte,".csv", sep="")
write.csv(allMoVals, file = outFileName, row.names=FALSE)
write.csv(result_df, 'recommand_moview.csv', row.names=T)
read.csv(sprintf("%s/%s/%s/data.tsv", base.dir, app.name, day),
         header = T, sep = "\t", stringsAsFactors = F)
gloseri <- read.csv(file.choose(), header = T) # choose() 
save(mert, mert_rank, file="merts.RData")   #R Data로 저장 & 불러오기
load("merts.RData")

# ronames null
rownames(dau.user.info) <- NULL
row.names(google) <- 1:733
nrow(acs)

# detecting of NA
summary(teens$age) # NA's 5523,   
table(teens$gender, useNA = "ifany")
mean(teens$age, na.rm = T)

# 중복제거 
unique(Cars93[, c("Origin", "Type")])

dup <- duplicated(mert2$businessnumber) #False는 중복항들의 첫째항 또는 중복되지 않는 항
mert3 <- mert2[!dup,] #사업자 등록번호 기준으로 중복제거

# 정렬
ordered_val_perf <- val_perf[order(val_perf[,5], decreasing = TRUE),]

# print, str
print(nc)
str(nc)

##### Explore #######################################################
class(wdbc_x)
nrow(wdbc_x)
dim(wdbc_train)
nCar <- dim(corolla)[1]
nVar <- dim(corolla)[2]


##### 형변환 #########################################################

ploan_data <- data.frame(ploan_input, ploan_target)
RawPlant$Plant_Code <- as.character(RawPlant$Plant_Code)
ploan_target <- as.numeric(ploan_target)-1
movie_real <- as(movie_real, "matrix")
recom_result <- as(recomm_list,"list")
RawPlant$Plant_Code <- gsub(" ","",RawPlant$Plant_Code) # get rid of spaces
weight_bmi_df <- as.data.frame(weight_bmi_prop)
ploan_target <- as.factor(ploan[,target_idx])
number_3 <- as.character(number)
datetime_3 <- as.POSIXlt(number_3, # character
                         format = '%Y%m%d%H%M%S', 
                         origin = "1970-01-01", 
                         tz ="UTC") # UTC : universal time
datetime_3 # "2016-07-09 10:30:50 UTC"
[1] "2016-07-09 10:30:50 UTC"

# factor, labels
mtcars$tm=factor(mtcars$am,labels=c("automatic","manual"))
df_data$sex_name <- factor(df_data$sex, levels = c(1, 2), labels = c("Male", "Female"))
df_data$bmi_cate <- factor(df_data$bmi_cate, levels=c("체중부족", "정상", "과체중", "비만"), ordered=TRUE)
df_data$sex_name <- factor(df_data$sex, levels = c(1, 2), labels = c("Male", "Female"))

# format 변경 
dau.user.info.device.summary$log_date <- format(dau.user.info.device.summary$log_date, format="%m/%d/%Y")

# class
class(a2[ , "orderdate_2"]) <- "character"



##### 결측값 처리 ################################################

d.complete <- na.omit(d)


sum(is.na(Cars93))  # 결측값의 갯수를 확인(TRUE 가 논리값 1이므로)
sum(is.na(Cars93$Rear.seat.room))   #컬럼별 결측값 갯수를 확인
sum(is.na(Cars93$Luggage.room))
colSums(is.na(Cars93))              #한꺼번에 결측값 갯수 확인

sum(x, na.rm = TRUE)
mean(x, na.rm = TRUE)


# 특정컬럼에서 na 값 삭제
Cars93_2 <- Cars93[ complete.cases(Cars93[ , c("Rear.seat.room")]), ]
Cars93_3 <- Cars93[ complete.cases(Cars93[ , c(23:24)]), ]
sum(is.na(Cars93_3)) # 검사
dim(Cars93_3)

# 결측값을 다른 값으로 대체
Cars93_4$Luggage.room[is.na(Cars93_4$Luggage.room)] <- 0
Cars93_5$Luggage.room[is.na(Cars93_5$Luggage.room)] <- mean(Cars93_5$Luggage.room, na.rm
w[is.na(w)] <- 0 # list 일 경우 
                                                            
#
a1[!complete.cases(a1),]

# na.omit
acs1=na.omit(acs)
acs2=na.omit(acs[c("EF","BMI")])
barplot(na.count[na.count>0]) # 0이상인 데이터만 플로팅하기


##### 숫자처리 ########################################################
limits <- c(0, max(dau.user.info.device.summary$dau))                                                            




##### 문자처리 ########################################################

# substr, regexpr, paste, sub
Flag1 <- regexpr(" ",SPlist$SciName) # position of first space
Genus <- substr(SPlist$SciName,1,Flag1 - 1)
Remainder <- substr(SPlist$SciName,Flag1 + 1,nchar(SPlist$SciName))
Flag2 <-  regexpr(" ",Remainder) 
Species <- substr(Remainder,1,Flag2 - 1)
SPlist$GenSp <- paste(Genus, Species, sep=" ")
tmp_x <- paste(colnames(trn_data)[-1], collapse=" + ")                                                            
outFile <- substr(files[nF], 1, nchar(files[nF])-3)
outFileName = paste("d:/R/raster webinar/webinar_CMIP-A2-", dte,".csv", sep="")

# Strip out space from "Genus "
SPlist$GenSp <- sub(" $","",SPlist$GenSp)

# sub, gsub

x="Parks hates stats. He hates math, too."
sub("hat","lov",x)
gsub("hat","lov",x)


str <- c("Regular", "expression", "examples of R language")
x <- sub("x.ress","",str) # 점(.) <=> Any character, except \n or line terminator

# %in% 사용
SpeciesLookup[!as.character(SpeciesLookup$Plant_Code)%in%
                as.character(ROMO_SPlist$Plant_Code),]

SpeciesLookup[SpeciesLookup$SCINAME%in%Test$SCINAME,]

# grep함수 사용하기

grep("test", model.df$VALUE1)
grep("^ap+", c("apple", "Apple", "apple2", "bbapple"), value=TRUE) ## 첫 글자가 ap로 시작하는 값을 찾아라
grep("ap+", c("apple", "Apple", "apple2", "bbapple"), value=TRUE) ## ap라는 글자가 들어가는 값을 다 찾아라 
grep("ap$", c("apple", "AAAap", "apple2", "bbapple"), value=TRUE) ## 마지막 끝나는 문자가 ap인 값을 찾으라
grep("[1-3]", c("apple1", "apple2", "apple3", "apple4", "Apple1"), value=TRUE) ## 1-3을 포함하는 모든 데이터를 출력
grep("[[:digit:]]", c("apple1", "apple2", "apple3", "apple4", "Apple1"), value=TRUE) ## 숫자를 포함하는 데이터를 모두 출력 
grep("[[:lower:]]", c("apple1", "apple2", "apple3", "apple4", "Apple1"), value=TRUE) ## 소문자를 포함하는 데이터를 전부 출력 
grep("^[[:upper:]]", c("apple1", "apple2", "apple3", "apple4", "Apple1"), value=TRUE) ## 첫 글자가 대문자인 데이터를 전부 출력
grep("[[:upper:]]", c("apple1", "apple2", "apple3", "apple4", "Apple1"), value=TRUE) ## 대문자를 포함하고 있는 데이터를 전부 출력


##### 날짜 처리 ######################################################
year <- format(subway2[,"income_date"], "%Y") 
month <- format(subway2[,"income_date"], "%m") 
subway2 <- cbind(subway2, year, month)

a2$orderdate_2 <- substr(a2$orderdate, 5, 8)
class(a2[ , "orderdate_2"]) <- "character"
a2$orderdate_3 <- as.Date(a2[ , "orderdate_2"], format="%m%d")
a2$orderdate_4 <- format(a2$orderdate_3, format="%b/%d")



w$ts <- as.Date(w$V1, "%Y-%m-%d")

w$month <- factor(format(w$ts, "%B"), levels=month.name)  # 날짜형식에서 월만 추출하기
                                                            
##### Culculation ####################################################
perf_mat[4,2] <- mean(abs((val_data$Price-forward_haty)/val_data$Price))*100
                                                            



##### data handling ##################################################

# 추출_ subset

a1 <- subset(a1, select=c(mertid, mertname, businessnumber, month, monamount, moncnt))
a2 <- subset(a1, 사업자등록번호=='7808700034')
c2=subset(c1, !(Cult=="c52" & Date=="d21"))
movie_long <- subset(movie_long, rating != 0)
subset(prices, Date!=("2002-02-01")) 
dji.prices=subset(dji.prices, Date>"2001-12-31")   # 필요 기간만 잘라내기
dji.prices=subset(dji.prices, Date!="2002-02-01")
subway2 <- subset(subway, subset = format(income_date, "%Y") != "2014")
ww <- subset(w, month %in% c('June', 'July', 'August', 'September')) # sql 의 like 와 같은 것


# 각 분할별로 데이터를 추출하는 데 사용. subset() 에 조건을 지정하면 그룹별로 조건을 만족하는 행만 추출된다.
a2 <- subset(a1, select = c("transactionid", "amount","orderdate", "cardtype"))
b1 <- subset(a2, cardtype == "81000"|cardtype == "81100"|cardtype == "81200"
             |cardtype =="81300" |cardtype =="41000" |cardtype =="41100"|cardtype =="41200"|cardtype =="41300"|cardtype =="41800")

## IF ELSE 문 사용

ifelse (AA == BB, "XX", "YY")
AA$BB <- ifelse(AA$CC == AA$DD , "install" , "existing")
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
   



##### data.frame 사용 ################################################

# select value
c2 <- a1[a1$Cult=="c52" & a1$Date=="d21",]
c2 <- price[price$Date!="2002-02-01",] 
dji.prices <- dji.prices[dji.prices$Date>"2001-12-31",]   # 필요 기간만 잘라내기
subway2 <- subway2[format(subway2$income_date, "%Y") != "2014"),] # 되는지는 확인
# subway2 <- subset(subway, subset = format(income_date, "%Y") != "2014")
wdbc_train_y <- wdbc[idx, 1]                                                            
                                                            
result <- terms[str_length(terms) >= 5 & str_length(terms) <= 6]
ww <- w[w$month %in% c('June', 'July', 'August', 'September'),]
trainSet <- movie_real[c(2:5),]
training_ins = insurance[idx, ]
testing_ins = insurance[-idx, ]                                                            
ploan_input <- ploan[,input_idx]    
                                                            
model_ins$fitted.values[1:5]

# 정렬
t2 <- t1[order(-t1$q),]
mtcars <- mtcars[order(mtcars$mpg, -mtcars$hp), ]
a3 <- a2[order(-a2$businessnumber,a2$month), ]   # 두개 다 decreasing 됨
mtcars <- mtcars[order(rownames(mtcars)), ]


# insert value                                                           
allMoVals[,7] <- (allMoVals[,3] + (allMoVals[,4]*0.0833)-0.0416)   # decimal yr.month
allMoVals[which(allMoVals[,4]==1 | allMoVals[,4]==2 | allMoVals[,4]==12), 8] <- "Win"
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1                                                            
RawPlant$WithinPlot[RawPlant$WithinPlot==0] <- 0.1
xMin = nc$dim$longitude$vals[1]

# insert with ifelse
mtcars$tm=ifelse(mtcars$am==0,"automatic","manual")
dat$Cntr <- ifelse( dat$Country == "Canada", "CAN", "USA")
dau2$is.payment <- ifelse(is.na(dau2$payment), 0, 1)
dau.install.payment$log_month <-substr(dau.install.payment$log_date, 1, 7)

# 인덱싱

df[1:3,]     # df의 1~3행 추출
df[1:3, 2]   # df의 1~3행, 2열 추출
df[1:3, 2, drop=F]  # 차원축소 방지

class(df[1:3, 2])
class(df[1:3, 2, drop=F])
                                                            
# which statement
full_predicted[which(full_response >= 0.5)] <- 1
p_idx <- which(corolla$Fuel_Type == "Petrol")
                                                            
# lapply
interests_n <- as.data.frame(lapply(interests, scale))   # interests : data.frame
                                                            
                                                            
##### Using plyr ######################################################

# ddply 사용하여 집계하기
# user_id 를 freq 변수로 두고, is.goal 을 0,1 로 두어 sum 했을 때 1만 집계되도록 함
ddply(AA, .(test_case), summarize, cvr=sum(is.goal)/length(user_id))


mau.payment <- ddply(dau.install.payment,
                     .(log_month, user_id, install_month), # 그룹화
                     summarize, # 집계 명령
                     payment = sum(payment) # payment 합계
)

# 날짜별, 테스트 케이스별로 클릭율을 산출하기
ab.test.imp.summary <-
  ddply(ab.test.imp, .(log_date, test_case), summarize,
        imp=length(user_id),
        cv=sum(is.goal),
        cvr=sum(is.goal)/length(user_id))

# 기타 
mau <- ddply(dau2, .(log_month, user_id), summarize, payment = sum(payment),
             access_days = length(log_date))

ddply(user.action.kpi, .(cluster), summarize, arpu= round(mean(payment)), 
      access_days = round(mean(access_days)))

dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type),
                                      summarize, dau = length(user_id))

##### Merge ##########################################################

# Inner join
dau.user.info <- merge(dau, user.info, by = c("user_id", "app_name"))

# left join
dau2 <- merge(dau, dpu[, c("log_date", "user_id", "payment"),], 
              by = c("log_date", "user_id"), all.x = T)

ab.test.imp <- merge(ab.test.imp, ab.test.goal, by="transaction_id", all.x=T, suffixes=c("",".g"))
                                                            
# rbind, cbind
ploan.trn <- cbind(ploan.x[trn_idx,], ploanYN = ploan.y[trn_idx,])
ploan.val <- cbind(ploan.x[-trn_idx,], ploanYN = ploan.y[-trn_idx,])
ploan.all <- rbind(ploan.trn, ploan.val)   
mlr_data <- cbind(corolla[,-c(id_idx, category_idx)], Fuel)                                                            




##### 집계&분석 기본 ####################################################

table(user.action.km$cluster)
cm_full <- table(full_target, full_predicted)                                                            
summary(user.action.pca.base)
summary(wdbc[,c(2:31)])                                                            
table(dau.user.info[, c("log_month", "gender")])
table(sms_mt[1,])                                                            

library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id",
      length)

table(mert_rank$rank)
bmi_prop <- prop.table(table(df_data$bmi_cate)) * 100
mean(teens$age, na.rm = T)                                                            

# ave(기준변수, 집단변수, FUNC = mean())
ave(df$x, df$y, FUN = function(x) mean(x, na.rm = T))
# - 졸업년도 기준으로 나이 평균 계산 해서 NA 값을 채우기 
avg <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm =T))    
teens$age <- ifelse(is.na(teens$age), avg, teens$age)

# aggregate
           
# 군집별 평균 나이
aggregate(data = teens, age ~ cluster, mean)         
# 군집별 여성 비율 - 여성 비율이 월등이 높음 
aggregate(data = teens, gender=='F' ~ cluster, mean)           
           
           
#### Using FOR Statement ################################################
                                                           
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
