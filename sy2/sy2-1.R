# chisq.test 要求正数?

x1<-rt(10000,1,0.5)
x2<-rpois(10000,1)

x4<-rexp(10000,1)



x1_1<-rnorm(100,10,1)
x1_2<-rnorm(100,10,5)
x1_3<-rnorm(100,10,10)

chisq.test(x1_1)

load(file='sy2-1.Rdata')