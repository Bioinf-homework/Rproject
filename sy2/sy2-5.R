x <- rnorm(100,1.1,10)

y1 <- rnorm(50,2,2)
y2 <- rnorm(50,2.3,2)

save(x,y1,y2,file="./data.Rdata")

#load("D:/data.Rdata")