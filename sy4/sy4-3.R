setwd("D:/this term/生统/实验/4")

x = read.table("step.data.txt")


y = x$y
x1 = x$x1
x2 = x$x2
x3 = x$x3
level<-data.frame(y, x1, x2, x3)

lm.reg<-lm(y~x1+x2+x3+x1*x2+x1*x3+x2*x3+x1**2+x2**2+x3**2, data=level)
# lm.reg<-lm(y~x1+x2+x3+x1*x2+x1**2+x2**2+x3**2, data=level)
summary(lm.reg)

tstep<-step(lm.reg)

summary(tstep)