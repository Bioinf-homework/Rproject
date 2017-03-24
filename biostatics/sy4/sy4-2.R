# 对回归系数做点估计
# 判断模型的显著性
# 判断其中每一个自变量的显著性


y<-c(162, 120, 223, 131, 67, 169, 81, 192, 116, 55, 252, 232, 144, 103, 212)
x1<-c(274, 180, 375, 205, 86, 265, 98, 330, 195, 53, 430, 372, 236, 157, 370)
x2<-c(2450, 3250, 3802, 2838, 2347, 3782, 3008, 2450, 2137, 2560, 4020, 4427, 2660, 2088, 2605)

level<-data.frame(y, x1, x2)
lm.reg<-lm(y~x1+x2, data=level)
# lm.reg<-lm(y~1+x1+x2)
# 包含t
summary(lm.reg)
a = lm.reg$coefficients[1]
b1 = lm.reg$coefficients[2]
b2 = lm.reg$coefficients[3]


# 解正规方程，做点估计
# solve() 逆
# t() 转置
n = length(x1)

S11 = sum(x1**2)-sum(sum(x1)**2/n)
S12 = sum(x1*x2)-sum(sum(x1)*sum(x2)/n)
S21 = S12
S22 = sum(x2**2)-sum(sum(x2)**2/n)
myS = matrix(c(S11,S12,S21,S22), nrow = 2, ncol = 2, byrow=TRUE)
S1y = sum(x1*y)-sum(sum(x1)*sum(y)/n)
S2y = sum(x2*y)-sum(sum(x2)*sum(y)/n)
myY = matrix(c(S1y,S2y), nrow = 2, ncol = 1, byrow=TRUE)

myC = solve(t(myS)%*%myS)
myB = myC%*%t(myS)%*%myY
myA = mean(y)-myB[1]*mean(x1)-myB[2]*mean(x2)

# 方差分析，判断模型的显著性

SSt = sum((y-mean(y))**2)
vt = n-1

yHat = myA+myB[1]*x1+myB[2]*x2
SSe = sum((y - yHat)**2)
ve = n-1-2
MSe = SSe/ve

SSh = SSt - SSe
vh = 2
MSh = SSh/vh
myF = MSh/MSe

myp = pf(myF,vh,ve)

# 对每个回归系数检验

c11 = myC[2,2]
# c12 = myC[2]
c22 = myC[1,1]

t1 = myB[1]/sqrt(MSe*c11)
t2 = myB[2]/sqrt(MSe*c22)

pt(t1,df=n-1-2)
pt(t2,df=n-1-2)