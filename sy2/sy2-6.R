### 按理说要。随机区组设计的ANOVA。。。但是anova不是实验三的内容么。。
# 求某种基因的表达量变化。。不大会用数据框，勉强先这样做。。
# 设置路径
setwd("D:/programs/Rproject/sy2")
# 读取。
# data = read.csv("log2exp.quantiles.baseline.csv",nrows=100)
data = read.csv("log2exp.quantiles.baseline.csv")

len = length(data[,3])

main <- function(n) {
	d3 = unlist(data[3])
	d4 = unlist(data[4])
	d5 = unlist(data[5])
	d6 = unlist(data[6])
	d7 = unlist(data[7])
	d8 = unlist(data[8])
	d9 = unlist(data[9])
	d10 = unlist(data[10])
	d11 = unlist(data[11])
	d12 = unlist(data[12])
	d13 = unlist(data[13])
	d14 = unlist(data[14])
	d15 = unlist(data[15])
	d16 = unlist(data[16])
	d17 = unlist(data[17])
	d18 = unlist(data[18])

	xx = list()

	for (i in 1:n) {
		tt1 = c(d3[i],d4[i],d5[i],d6[i])
		tt2 = c(d7[i],d8[i],d9[i],d10[i])
		tt3 = c(d11[i],d12[i],d13[i],d14[i])
		tt4 = c(d15[i],d16[i],d17[i],d18[i])
		re1 = t.test(tt1,tt2)
		re2 = t.test(tt1,tt3)
		re3 = t.test(tt1,tt4)
		if(re1$p.value < 0.05)
		{
			xx$id = c(xx$id,i)
			xx$treatment = c(xx$treatment,"E2")
		}
		if(re2$p.value < 0.05)
		{
			xx$id = c(xx$id,i)
			xx$treatment = c(xx$treatment,"Akt knockout")
		}
		if(re3$p.value < 0.05)
		{
			xx$id = c(xx$id,i)
			xx$treatment = c(xx$treatment,"Both")
		}
	}
	return(xx)
}

xx = main(len)
write.table(xx,file='genedata.txt')