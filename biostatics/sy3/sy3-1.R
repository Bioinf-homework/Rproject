setwd("D:/Programs/Rproject/sy3")
# setwd("/media/tmn07/3AD6217DD6213A91/Programs/Rproject/sy3")

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




# 额，目前要每组样本容量一样大
myaov <- function(X,A){
  # 处理A带的信息
  len = length(A)
  les = length(levels(A))

  # 这两步，很难想到。。所以写起来很痛苦。。。
  func <- function(sp){
    return(mean(X[sp]))
  }
  sp = split(1:len,A)
  am <- lapply(sp,func)

  m <- mean(X)
  # 总
  SSt <- sum((X-m)**2)
  vt <- length(X)-1
  # 组间
  SSj <-  (len/les) *sum((as.numeric(am) - m)**2)
  vj <- les-1
  Mj <- SSj/vj
  # 组内
  SSn <- SSt - SSj
  vn <- vt-vj
  Mn <- SSn/vn
  # 计算统计量
  F <- Mj/Mn
  p = pf(F,vj,vn)
  # 输出
  cat("F=")
  cat(F)
  cat("\nP=")
  cat(1-p)
}
