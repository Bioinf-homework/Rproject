setwd("C:/基因组信息/实验课/实验课/experiment1")


library("GeneR")

data <- read.table("refGene.txt")

data[1,]

n = length(data[,1])

setwd("chromFa")


now = "chr1"
readFasta("chr1.fa")
mydata = c()
for (i in 1:10000){

if(data[i,3] != now){
now = as.character(data[i,3])
filename <-  paste(as.character(data[i,3]),".fa",sep="")
readFasta(filename)	}

if(data[i,4] == "+"){ 
sq <- getSeq(from=(data[i,7]-1000),to=data[i,7],strand=0)}

else{ 
sq <- getSeq(from=data[i,8],to=data[i,8]+1000,strand=1)}

mydata = rbind(mydata,c(as.character(data[i,2]),as.character(data[i,3]),as.character(data[i,4]),data[i,5],data[i,6],sq))
print(i)	}




y=31788
mydata = c()
for (i in y:(y+10)){

if(data[i,4] == "+"){ 
sq <- getSeq(from=data[i,7]-1000,to=data[i,7],strand=0)}
else{ 
sq <- getSeq(from=data[i,8],to=data[i,8]+1000,strand=1)}
mydata = rbind(mydata,c(data[i,2],data[i,3],data[i,4],data[i,5],data[i,6],sq))	}




readFasta("chrY.fa")

y=31788
getSeq(from=1,to=10,strand=0)
getSeq(from=data[y,7],to=data[y,8])