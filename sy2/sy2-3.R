analysis <- function(n) {
	x <- rnorm(n,50,11)
	X2 <- ((n-1)*var(x))/100
	p = 1 - pchisq(X2,n-1)
	return(p)
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