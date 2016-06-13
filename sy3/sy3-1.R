# setwd("D:/Programs/Rproject/sy3")
setwd("/media/tmn07/3AD6217DD6213A91/Programs/Rproject/sy3")

# x1 <- rnorm(10,10,1)
# x2 <- rnorm(10,10.2,1)
# x3 <- rnorm(10,12,1)
# x4 <- rnorm(10,9,1)
# save(x1,x2,x3,x4,file = "sy3-1.Rdata")

load(file = "sy3-1.Rdata")
X <- c(x1,x2,x3,x4)

A <- factor(rep(1:4,each=10))

data = data.frame(X,A)

# 方差检验
bartlett.test(data$X, data$A)

# 单因素分析
data.aov = aov(X~A,data)

# 输出方差分析表
summary(data.aov)

# 箱线图
plot(data$X~data$A)

# 多重t检验
pairwise.t.test(X,A,p.adjust = "bonf")

# 增大样本容量检测，300左右是易检测出。
x1 <- rnorm(300,10,1)
x2 <- rnorm(300,10.2,1)

t.test(x1,x2)

# 
myaov <- function(X,A){
  m <- mean(X)
  a1 <- X[1:10]
  a2 <- X[11:20]
  a3 <- X[21:30]
  a4 <- X[31:40]
  SSt <- sum((X-m)**2)
  vt <- length(X)-1
  a1m = mean(a1)
  a2m = mean(a2)
  a3m = mean(a3)
  a4m = mean(a4)
  am <- c(a1m,a2m,a3m,a4m)
  SSj <- 10*sum((am - mean(am))**2)
  vj <- length(levels(A))-1
  Mj <- SSj/vj
  SSn <- SSt - SSj
  vn <- vt-vj
  Mn <- SSn/vn
  F <- Mj/Mn
  
  p = pf(F,vj,vn)
  cat("F=")
  cat(F)
  cat("\nP=")
  cat(p)
}
