library(dplyr)
# 1번----------------
data<- read.csv('rtest.csv')
data

# 2번 -------------------
summary(data$resident)
table(data$resident)

list_resident <- data.frame(resident=c(1:5),resident2=c("서울","인천","대전","대구","시구군"))
list_resident
data <- left_join(data, list_resident, id = "resident")
data

# 3번-----------------------
summary(data$age)
data <- data %>%
  mutate(age2 = ifelse(age <= 30,'청년층',ifelse(age >= 31 & age <= 55,'중년층','장년층')))
data$age2

# 4번-------------------
table(data$gender)
data$gender2[data$gender==1]<-'남자'
data$gender2[data$gender==2]<-'여자'
class(data$gender2)
data$gender2 <- factor(data$gender2)
t1<-table(data$gender2)
t1
pie(t1, col = rainbow(2))

# 5번 ------------------------
resident_gender<- data %>%
  group_by(resident2, gender2) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 2))
resident_gender
library(ggplot2)
ggplot(data = resident_gender, aes(x = resident2, y = pct, fill = gender2))+geom_col()

# 6번 -------------------

ggplot(data = data, aes(x = age, y= price)) +geom_point()
cor.test(data$age, data$price) # 0.05 이상으로 상관관계가 없음

# 7번------------------
# install.packages('hflights')
library(hflights)
hflights
mean(hflights$AirTime, na.rm = T)

# 8번----------------
n <- hflights %>%
  filter(!is.na(AirTime)) %>%
  summarise(n())
n
sum <- hflights %>%
  filter(!is.na(AirTime)) %>%
  summarise(sum(AirTime))
sum
Airtime <- sum/n
Airtime

# 9번---------------

hflights$AirTime <- ifelse(is.na(hflights$AirTime),round(mean(hflights$AirTime, na.rm = T)),hflights$AirTime)
hflights$AirTime

# 10번----------------
var(hflights$ArrTime, na.rm = T)
sd(hflights$ArrTime, na.rm = T)







