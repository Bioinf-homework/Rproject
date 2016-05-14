# 控制布局函数
# attach(mtcars)
# layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))

f1 <- function(n){
  a <- c()
  for(i in 1:10000){
    #  x1<-rbinom(n,1,0.5) # 更改下这个函数生成不同的分布的随机数
    # x1 <- rpois(n,1)
    # x1 <- rnorm(n,0,1)
    x1<-rexp(n,1)
    a <-c(a,mean(x1))
  }
  return(a)
}
# 画出不同样本容量的直方图
draw_hist <- function(main_str){
   hist(f1(3),main = main_str)
   hist(f1(5),main = main_str)
   hist(f1(10),main = main_str)
   hist(f1(100),main = main_str)
   hist(f1(1000),main = main_str)
   hist(f1(10000),main = main_str)
}
# 画出不同样本容量的正态QQ图
draw_QQ <- function(main_str)
{
  qqnorm(f1(3),main = main_str)
  qqnorm(f1(5),main = main_str)
  qqnorm(f1(10),main = main_str)
  qqnorm(f1(100),main = main_str)
  qqnorm(f1(1000),main = main_str)
  qqnorm(f1(10000),main = main_str)
}
# 画出不同样本容量的经验分布图
draw_ecdf <- function(main_str)
{
  plot(ecdf(f1(3)),main = main_str)
  plot(ecdf(f1(5)),main = main_str)
  plot(ecdf(f1(10)),main = main_str)
  plot(ecdf(f1(100)),main = main_str)
  plot(ecdf(f1(1000)),main = main_str)
  plot(ecdf(f1(10000)),main = main_str)
}

#draw_hist('xxx')
#draw_QQ('binom')
draw_ecdf('exp')