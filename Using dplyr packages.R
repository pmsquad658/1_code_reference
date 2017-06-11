####################################################
#### dplyr packages ########################
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