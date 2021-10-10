# Questions 1

data_reg <- read.csv('regressionDataSet.csv')

str(data_reg)
train <- data_reg[1:13000,]
test <- data_reg[13001:16382, ]

relation <- lm(formula = Area~Energy, data = train)
print(relation)
Energy <- data.frame(test$Energy)
predicted <- predict(relation, test)
predicted2  = predict(relation, test)
plot(data_reg$Area, data_reg$Energy, col="red", 
     main = "Regression", abline(relation), cex=1.3,
     pch=16, xlab = "Independent", ylab = "Dependent")
cor(predicted, test$Area)

# Root mean square
RMSE <-function(m, o){
  sqrt(mean((m-o)^2))
}

Accuracy <- RMSE(predicted, test$Area)

Accuracy

# Q2. Execution of the following 3 R commands will give us the data {(x(i), y(i), z(i), i = 1,2, .,100}.

x<-rpois(100, 50)
y<-rpois(100, 100)
z<-rpois(100, 150)


df <- data.frame(x,y,z)
lm <- lm(z~x+y, data = df)
summary(lm)

# a) Fit the linear regression model of the form z = a +b.x + c.y using,

df <- cbind(df, x)
model1 <- lm(z~x, df)
print(model1)



# b) Fit the 3 models of the form y = a + b.x, y = a + b.x + c.x2,
#   and y = a.bx  to this data  using
x2 <- x^2
model2 <- lm(z~x+x^2, df)
print(model2)

model3 <- lm(log(z)~x, df)
print(model3)
