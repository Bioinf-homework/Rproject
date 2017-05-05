setwd("D://this term//大三下//基因组//实验课//experiment3")

data <- read.table("data.txt")

# print(data)
P <- c(0.3,0.2,0.2,0.3)

w <- c()

for (i in 1:ncol(data)) { 
	N <- sum(data[,i])
	wt <- c()
	for (j in 1:4) {
		new <- log( ( (data[j,i] + sqrt(N)/4)/(N+sqrt(N))) /P[j],2)
		wt <- c(wt, new)
	}
	w = cbind(w,wt)
}

w = matrix(w,nrow=4)

strand = "AGCTAGGGTCAGCATGGCCAGGTCAGCATGGCCC"

matrix = matrix(0,nrow=4,ncol=nchar(strand));

if (gregexpr("[aA]", strand)[[1]][1]>0){
	matrix[1,gregexpr("[aA]", strand)[[1]]] = 1;
}
if (gregexpr("[cC]", strand)[[1]][1]>0){
	matrix[2,gregexpr("[cC]", strand)[[1]]] = 1;
}
if (gregexpr("[gG]", strand)[[1]][1]>0){
	matrix[3,gregexpr("[gG]", strand)[[1]]] = 1;
}
if (gregexpr("[tT]", strand)[[1]][1]>0){
	matrix[4,gregexpr("[tT]", strand)[[1]]] = 1;
}

res <- c()

for (i in 1:(nchar(strand)-ncol(data)+1)) {
	res <- c(res,sum(matrix[,i:(i+14)]*w))
}
cat("最可能的位点起始：");cat(which.max(res));

# A	0.3
# C	0.2
# G	0.2
# T	0.3
