setwd("D:/programs/Rproject/sy2")
# 读取。
# data = read.csv("log2exp.quantiles.baseline.csv",nrows=100)
data = read.csv("log2exp.quantiles.baseline.csv")

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

len = length(data[,3])

A <- factor(gl(4,4))

xx = list()


for(i in 1:len){
	tt = c(d3[i],d4[i],d5[i],d6[i],d7[i],d8[i],d9[i],d10[i],d11[i],d12[i],d13[i],d14[i],d15[i],d16[i],d17[i],d18[i])
	# format_data = c(format_data,tt)
	data_frame = data.frame(tt,A)
	# bartlett.test(data$X, data$A)
	data_frame.aov = aov(tt~A,data_frame)
	y = summary(data_frame.aov)
	if(y[[1]][[5]][[1]] < 0.05)
	{
		pair = pairwise.t.test(tt,A,p.adjust = "bonf")
		for(j in 1:3){
			if(pair$p.value[j,1] < 0.05) {
				xx$id = c(xx$id,i)
				xx$treatment = c(xx$treatment,j)
			}
		}
	}

}

setwd("../sy3")
write.table(xx,file='genedata.txt')
