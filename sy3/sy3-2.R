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

y11 <- rnorm(1,10,1)
y12 <- rnorm(1,10.2,1)
y13 <- rnorm(1,10.4,1)
y14 <- rnorm(1,10.6,1)
y21 <- rnorm(1,8,1)
y22 <- rnorm(1,8.2,1)
y23 <- rnorm(1,8.4,1)
y24 <- rnorm(1,8.6,1)
y31 <- rnorm(1,9,2)
y32 <- rnorm(1,9.2,2)
y33 <- rnorm(1,9.4,2)
y34 <- rnorm(1,9.6,2)
y41 <- rnorm(1,13,1.5)
y42 <- rnorm(1,13.2,1.5)
y43 <- rnorm(1,13.4,1.5)
y44 <- rnorm(1,13.6,1.5)
# no repeat
Y = c(y11,y12,y13,y14,y21,y22,y23,y24,y31,y32,y33,y34,y41,y42,y43,y44)
A2 = gl(4,4)
B2 = gl(4,1,16)



data = data.frame(X,A,B)
data2 = data.frame(Y,A2,B2)

# no interaction
# data.aov <- aov(X~A+B,data) 
# summary(data.aov)

# draw..
# op <- par(mfrow = c(1,2))
# plot(X~A+B,data)
# op <- par(mfrow = c(1,2))
# plot(Y~A2+B2,data)


# data.aov2 <- aov(X~A+B+A:B,data)
data.aov <- aov(X~A*B,data)
summary(data.aov)


data2.aov <- aov(Y~A2*B2,data2)
summary(data2.aov)
# 交互检验
tukey.add.test(Y,A2,B2)
