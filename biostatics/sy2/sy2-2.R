# 方差越大越不好检出来，还以为我写错了233

# 计算P值，应该可以用pt吧。。
jisuan <- function(x,u){
	len = length(x)
	t = sqrt(len)*(mean(x) - u)/sd(x)
	if(t<0) {
		p = 2*pt(t,df = len-1)
	}
	else {
		p = 2*(1 - pt(t,df = len-1))
	}
	return(p)
}

# 对比手写的函数的和t.test的计算出来的。
test <- function()
{
	for(i in 2:10) {
		x <- rnorm(i,10.2,1)
		result = t.test(x,mu = 10)
		p1 = result$p.value
		p2 = jisuan(x,10)

		print(p1)
		print(p2)
		print("-----")
	}
}

test()

# t检验
analysis <- function(n) {
	u = 10
	x <- rnorm(n,10.2,1)
	result = t.test(x,mu = u,conf.level = 0.99,var.equal = FALSE)
	# print(result)
	return(result$p.value)
}

# 输出多少个样本时，会被检出
cycle <- function(){
	for(i in 2:1000) {
		p = analysis(i)
# 		p2 = analysis(i)
# 		p3 = analysis(i)
# 		p <- c(p1,p2,p3)
		if(p<0.1){
			return(i)
		}
	}
}
# 检验1000次
main <- function(){
	data <- c()
	for(i in 1:1000)
	{
		data <- c(data,cycle())
	}
	return(data)
}


data = main()
hist(data)
# hist(data,main="检测1000次拒绝的样本容量分布",xlab="样本容量")

