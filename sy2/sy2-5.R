# H0：均数为1
# x <- rnorm(100,1.1,10)

# H0：二者均数一致
# y1 <- rnorm(50,2,2)
# y2 <- rnorm(50,2.3,2)

# save(x,y1,y2,file="D:/data.Rdata")

load("D:/data.Rdata")

t.test(x,mu = 2)

t.test(y1,y2)
