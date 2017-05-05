
filei<- function(file){
    return (paste(filedir, file,sep = ""))
}

filedir = "C:\\基因组信息\\实验课\\实验课\\experiment4\\"

setwd(filedir)

library(affy);
library(simpleaffy);


# 读芯片数据
Data <- ReadAffy(filenames=c('AD01.CEL','AD02.CEL','AD03.CEL','AI01.CEL','AI02.CEL','AI03.CEL'), celfile.path=filedir );
eset <- call.exprs(Data,'mas5' , FALSE);
write.exprs(eset,file=filei('expressionlevels.txt'));
data.mas5calls <- mas5calls(Data);
write.exprs(data.mas5calls,file=filei('call.txt'));

# expression <- read.table(file=filei('expressionlevels.txt'));
# expression <- as.matrix(expression);
# new.expression <- cbind((expression[,1:3]), (expression[,4:6]));
# boxplot(as.data.frame(log(new.expression)));

# 组内normlize quantiles
library('preprocessCore')
expression <- read.table(file=filei('expressionlevels.txt'));
expression <- as.matrix(expression);
new.expression <- cbind(normalize.quantiles(expression[,1:3]), normalize.quantiles(expression[,4:6]));
boxplot(as.data.frame(log(new.expression)));

# 组间normlize k-scale
X <- mean(apply(new.expression, 2, median));
for(i in 1:6)
{
	new.expression[,i] <- new.expression[,i]*X/median(new.expression[,i]);
}

boxplot( as.data.frame(log(new.expression)));
write.csv(new.expression, file=filei('normalized.expressionlevels.csv'), row.names=FALSE);


# 对每一组基因在两个芯片上的表达进行t检验，计算fold_change
geneN <- length(new.expression[,1]);
result = matrix(0,nrow= geneN,ncol=2);
dif = c()
for (i in 1:geneN) {
tmp <- t.test(log10(new.expression[i,1:3]), log10(new.expression[1,4:6]));
pvalue <- tmp[[3]];
mean1 <- mean(log10(new.expression[i,1:3]));
mean2 <- mean(log10(new.expression[i,4:6]));
fold_change = mean1/mean2;
result[i,1] = pvalue;
result[i,2] = fold_change;
if(result[i,1]<0.05 && (result[i,2] <0.66 || result[i,2] >1.50)){
dif <- c(dif, i);
print(i);
}
}
write.csv(result,file="result.csv")
write.csv(dif,file="dif.csv")