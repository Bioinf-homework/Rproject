# 装包
# install.packages("asbio")
# 加载包
library("asbio")

x11 <- rnorm(10,10,1)
x12 <- rnorm(10,10.2,1)
x13 <- rnorm(10,10.4,1)
x14 <- rnorm(10,10.6,1)
x21 <- rnorm(10,8,1)
x22 <- rnorm(10,8.2,1)
x23 <- rnorm(10,8.4,1)
x24 <- rnorm(10,8.6,1)
x31 <- rnorm(10,9,2)
x32 <- rnorm(10,9.2,2)
x33 <- rnorm(10,9.4,2)
x34 <- rnorm(10,9.6,2)
x41 <- rnorm(10,13,1.5)
x42 <- rnorm(10,13.2,1.5)
x43 <- rnorm(10,13.4,1.5)
x44 <- rnorm(10,13.6,1.5)


X = c(x11,x12,x13,x14,x21,x22,x23,x24,x31,x32,x33,x34,x41,x42,x43,x44)

A = gl(4,40)
B = gl(4,10,160)

data = data.frame(X,A,B)


data.aov <- aov(X~A+B,data) 
summary(data.aov)

tukey.add.test(X,A,B)


op <- par(mfrow = c(1,2))
plot(X~A+B,data)

data.aov2 <- aov(X~A+B+A:B,data)
# data.aov2 <- aov(X~A*B,data)
summary(data.aov2)

# 交互检验？？