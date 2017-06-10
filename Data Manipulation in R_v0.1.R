####################################################
#### Data Manipulation in R ########################
#################### v0.1 _ by 박민수. 2017. 1. 22##
####################################################


#################################################################################
### dplyr #######################################################################

# 1.filter – It filters the data based on a condition
# 2.select – It is used to select columns of interest from a data set
# 3.arrange – It is used to arrange data set values on ascending or descending order
# 4.mutate – It is used to create new variables from existing variables
# 5.summarise (with group_by) – It is used to perform analysis by commonly used operations such as min, max, mean count etc


install.packages('dplyr')
library(dplyr)


filter(mynewdata, cyl > 4 & gear > 4 )
filter(mynewdata, cyl > 4)
filter(myirisdata, Species %in% c('setosa', 'virginica'))


select(mynewdata, cyl,mpg,hp)
select(mynewdata, -cyl, -mpg ) 
select(mynewdata, -c(cyl,mpg))
select(mynewdata, cyl:gear)

vars <- c("Manufacturer", "MAX.Price", "MPG.highway")
select(Cars93_1, one_of(vars))

#chaining or pipelining - a way to perform multiple operations

mynewdata %>%
  select(cyl, wt, gear)%>%
  filter(wt > 2)

#arrange can be used to reorder rows

mynewdata%>%
  select(cyl, wt, gear)%>%
  arrange(desc(wt))

# or

mynewdata%>%
  select(cyl, wt, gear)%>%
  arrange(desc(wt))


#mutate - create new variables

mynewdata %>%
  select(mpg, cyl)%>%
  mutate(newvariable = mpg*cyl)

#or

newvariable <- mynewdata %>% mutate(newvariable = mpg*cyl)

Cars93_2 <- mutate(Cars93_1, 
                     Price_range = Max.Price - Min.Price, 
                     Price_range_cd = ifelse(Price_range >= 5, 1, 0))



#summarise - this is used to find insights from data

myirisdata%>%
  group_by(Species)%>%
  summarise(Average = mean(Sepal.Length, na.rm = TRUE))


#or use summarise each
#You can create complex chain commands using these 5 verbs.

myirisdata%>%
  group_by(Species)%>%
  summarise_each(funs(mean, n()), Sepal.Length, Sepal.Width)



#you can rename the variables using rename command

mynewdata %>% rename(miles = mpg)

# rename(dataframe, new_var1 = old_var1, new_var2 = old_var2, ...)
Cars93_2 <- rename(Cars93_1, 
                     New_Manufacturer = Manufacturer,
                     New_Model = Model, 
                     New_Type = Type, 
                     New_Min.Price = Min.Price, 
                     New_Price = Price, 
                     New_Max.Price = Max.Price, 
                     New_MPG.city = MPG.city, 
                     New_MPG.highway = MPG.highway)
 
names(Cars93_2)



# distinct
distinct(Cars93, Type)
unique(Cars93[, c("Origin", "Type")])   # 위와 동일


############################################################################

# 특정 인덱스 값에 insert value
RawPlant$WithinPlot[RawPlant$WithinPlot==0] <- 0.1


############################################################################
### 데이터 전처리###########################################################
############################################################################


# factor, labels
mtcars$tm=factor(mtcars$am,labels=c("automatic","manual"))

# ifelse
mtcars$tm=ifelse(mtcars$am==0,"automatic","manual")
dat$Cntr <- ifelse( dat$Country == "Canada", "CAN", "USA")


# %in% 사용
SpeciesLookup[!as.character(SpeciesLookup$Plant_Code)%in%
                as.character(ROMO_SPlist$Plant_Code),]

SpeciesLookup[SpeciesLookup$SCINAME%in%Test$SCINAME,]

# as.character

RawPlant$Plant_Code <- as.character(RawPlant$Plant_Code)
RawPlant$Plant_Code <- gsub(" ","",RawPlant$Plant_Code) # get rid of spaces


# substr, regexpr, paste, sub
Flag1 <- regexpr(" ",SPlist$SciName) # position of first space
Genus <- substr(SPlist$SciName,1,Flag1 - 1)
Remainder <- substr(SPlist$SciName,Flag1 + 1,nchar(SPlist$SciName))
Flag2 <-  regexpr(" ",Remainder) 
Species <- substr(Remainder,1,Flag2 - 1)
SPlist$GenSp <- paste(Genus, Species, sep=" ")

# Strip out space from "Genus "
SPlist$GenSp <- sub(" $","",SPlist$GenSp)


# sub, gsub

x="Parks hates stats. He hates math, too."
sub("hat","lov",x)
gsub("hat","lov",x)


str <- c("Regular", "expression", "examples of R language")
x <- sub("x.ress","",str)

# 점(.) <=> Any character, except \n or line terminator


## 결측값 처리

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
                                                            
                                                            
#
# as.character
number_3 <- as.character(number)
datetime_3 <- as.POSIXlt(number_3, # character
                             format = '%Y%m%d%H%M%S', 
                             origin = "1970-01-01", 
                             tz ="UTC") # UTC : universal time
datetime_3 # "2016-07-09 10:30:50 UTC"
[1] "2016-07-09 10:30:50 UTC"



































