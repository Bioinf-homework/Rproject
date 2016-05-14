# 偷懒不写出各种分布了。
# 控制布局函数
# attach(mtcars)
# layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))

f1 <- function(n){
  a <- c()
  for(i in 1:10000){
    x1<-rbinom(n,1,0.5) # 更改下这个函数生成不同的分布的随机数
    a <-c(a,mean(x1))
  }
  return(a)
}
# 画出不同样本容量的直方图
draw_hist <- function(){
   hist(f1(3))
   hist(f1(5))
   hist(f1(10))
   hist(f1(100))
   hist(f1(1000))
   hist(f1(10000))
}
draw_hist()
