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
		print(n)
		return(n)
	}
	return(0)
}
# 先是在15的容量下，做多次检验，在增大样本容量直到发现差异
main <- function(){
	for(i in 1:10){
		analysis()
	}
	print("-------")
	# for(i in 1:10){
	# 	analysis(20)
	# }
	# print("-------")
	i = 16
	while(TRUE)
	{
		x = analysis(i)
		if(x){
			break
		}
		i = i+1	
	}
}



