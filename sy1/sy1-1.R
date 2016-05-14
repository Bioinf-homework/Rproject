# 该程序每执行一次画出一种分布的三个图。
# 控制 图的显示的函数只在console里运行？
# 例如
# attach(mtcars)
# layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))

# 绘图函数，直接画出三种图，直方图，经验分布图，箱线图
p <- function(x){
  hist(x)
  plot(ecdf(x))
  boxplot(x)
}

# 生成这4种分布的随机数各10000个
x1<-rbinom(10000,1,0.5)
x2<-rpois(10000,1)
x4<-rexp(10000,1)
x3<-rnorm(10000,0,1)

# 每一次执行其中一条
#p(x1)
#p(x2)
#p(x3)
p(x4)