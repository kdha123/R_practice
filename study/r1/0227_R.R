install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
data = read.csv('sales.csv')
head(data) # 판매량 가격, 광고량
tree1 = rpart(sales~price+advert, data=data)
tree1
rpart.plot(tree1)

data = read.csv('insurance.csv')
head(data)

tree2 = rpart(charges~age+sex+bmi+smoker+region, data = data)
rpart.plot(tree2)
