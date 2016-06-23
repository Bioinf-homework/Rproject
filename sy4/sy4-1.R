x = c(2,4,9,12,19,19,29,35,39,41,49,50,59,59,69,71,77,79)
y = c(14.29,13.85,12.92,12.88,12.16,12.15,11.91,11.82,11.68,11.51,11.25,11.24,10.94,10.91,10.39,10.42,10.31,10.31)
n = length(x)
level = data.frame(x,y)
plot(level,main='散点图',xlab='年龄(单位：年)',ylab='端粒长度（单位：kb）')


# 点估计
reg <- lm(y~1+x)
# summary(reg)
a = reg$coefficients[1]
b = reg$coefficients[2]
print(a)
print(b)

# 自己的点估计、
Sxy = sum((x-mean(x))*(y-mean(y)))
Sxx = sum((x-mean(x))**2)
Syy = sum((y-mean(y))**2)
MSe = (Syy-b*Sxy)/(n-2)

myb = Sxy/Sxx
mya = mean(y)-myb*mean(x)


# 区间估计
confint(reg, level=0.99)

# 自己写的
# 双侧0.1
t = qt(0.995,df=n-2)

temp = t*sqrt(MSe/Sxx)
bb = c(b-temp,b+temp)

temp = t*sqrt(MSe*((1/n)+(mean(x)**2/Sxx)))
aa = c(a-temp,a+temp)

print(aa)
print(bb)


# 给自变量，估计因变量均值，区间
point = data.frame(x=40)

pr = predict(reg,point,interval="prediction",level=0.99)

print(pr[1])
print(c(pr[2],pr[3]))

myy = mya + myb*point

temp = t*sqrt(MSe*(1+(1/n)+((point-mean(x))**2/Sxx)))
myyy = c(myy-temp,myy+temp)