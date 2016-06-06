# 默认15的样本容量，如果有显著差异的，这返回样本容量
analysis <- function(n=15) {

	x<-rnorm(n,0,1)

	y<-rnorm(n,0,1)

	result1 = t.test(x,y)

	result2 = var.test(x,y)

	p1 = result1$p.value
	p2 = result2$p.value
	p  = c(p1,p2)
	# print(p)
	if(any(p<0.05))
	{
		# print(n)
		return(n)
	}
	return(0)
}
# 先是在15的容量下，做多次检验，在增大样本容量直到发现差异
main <- function(){
	record <- c()
	for(i in 1:100){
		if (analysis()){
			record<-c(record,i)
		}
	}
	cat("100次检查出")
	cat(length(record))
	cat("次\n")
	print("-------")
	# for(i in 1:10){
	# 	analysis(20)
	# }
	# print("-------")
	record2 <- c()

	for (j in 1:1000) {
		i = 16
		while(TRUE)
		{
			x = analysis(i)
			if(x){
				record2 <- c(record2,x)
				break
			}
			i = i+1	
		}
	}
	# print(record2)

}

main()