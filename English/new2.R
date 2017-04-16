# setwd('D:/gitwork/Rproject/English/')
data = read.table("xxx.txt");

mdata = matrix(nrow=72,ncol=12)

check = function(data, i)
{
	if(data>0 && data<upperlimit[[i]])
	# if(data>0&&data<5000)
	{
		return (TRUE)
	}
	else{
		return (FALSE)
	  
	}
}
getrow = function(n)
{
	return (m[[n]][m[[n]]<=upperlimit[n]])
}
# outlier mark

m = list()
upperlimit = matrix(0,nrow=12,ncol=1)
lowerlimit = matrix(0,nrow=12,ncol=1)

m[13] = 0
for(j in seq(1,ncol(data), by = 2))
{
	for(i in (1:nrow(data)))
	{
		snum = 1+data[i,j]%%100
		if(data[i,j+1] > 0)
		{
			m[[snum]] =c(m[[snum]], data[i,j+1])
		}
	}
}
for (i in 1:12) {
	x = boxplot(abs( m[[i]]),plot = FALSE)
	# 不存在下界异常�?
	#upperlimit[i] = min(x$out)
	lowerlimit[i] = min(x$stats[1])
	upperlimit[i] = min(x$stats[5])
}


# g-mean
for(j in seq(1,ncol(data), by = 2))
{
	m1 = matrix(1,nrow=12,ncol=1)
	m2 = matrix(0,nrow=12,ncol=1)
	for(i in (1:nrow(data)))
	{
		snum = 1+data[i,j]%%100
		if(check(data[i,j+1], snum))
		{
			m1[snum] = m1[snum]*data[i,j+1];
			m2[snum] = m2[snum] + 1;
		}

	}
	mdata[ceiling(j/2),] = abs(m1)^(1/m2)
}

write.csv(mdata,file = "mdata.csv")


# t-test



# R1-base
N1R1 = getrow(2)
R2R1 = getrow(4)
N2R1 = getrow(6)
print("R1-base")
cat("sd(N1R1):")
print(sd(N1R1))
cat("sd(R2R1):")
print(sd(R2R1))
cat("sd(N2R1):")
print(sd(N2R1))
print("N1R1,R2R1")
print(var.test(N1R1,R2R1))
print(t.test(N1R1,R2R1,var.equal=FALSE))
print("N2R1,R2R1")
print(var.test(N2R1,R2R1))
print(t.test(N2R1,R2R1,var.equal=FALSE))

# R2-base
N1R2 = getrow(7)
N2R2 = getrow(12)
R1R2 = getrow(3)
print("R2-base")
cat("sd(N1R2):")
print(sd(N1R2))
cat("sd(N2R2):")
print(sd(N2R2))
cat("sd(R1R2):")
print(sd(R1R2))
print("N1R2,R1R2")
print(var.test(N1R2,R1R2))
print(t.test(N1R2,R1R2,var.equal=FALSE))
print("N2R2,R1R2")
print(var.test(N2R2,R1R2))
print(t.test(N2R2,R1R2,var.equal=FALSE))

# N1-base
N2N1 = getrow(10)
R1N1 = getrow(1)
R2N1 = getrow(8)
print("N1-base")
cat("sd(N2N1):")
print(sd(N2N1))
cat("sd(R1N1):")
print(sd(R1N1))
cat("sd(R2N1):")
print(sd(R2N1))
print("R1N1,N2N1")
print(var.test(R1N1,N2N1))
print(t.test(R1N1,N2N1,var.equal=FALSE))
print("R2N1,N2N1")
print(var.test(R2N1,N2N1))
print(t.test(R2N1,N2N1,var.equal=FALSE))

# N2-base
N1N2 = getrow(9)
R1N2 = getrow(5)
R2N2 = getrow(11)
print("N2-base")
cat("sd(N1N2):")
print(sd(N1N2))
cat("sd(R1N2):")
print(sd(R1N2))
cat("sd(R2N2):")
print(sd(R2N2))
print("R1N2,N1N2")
print(var.test(R1N2,N1N2))
print(t.test(R1N2,N1N2,var.equal=FALSE))
print("R2N2,N1N2")
print(var.test(R2N2,N2N1))
print(t.test(R2N2,N1N2,var.equal=FALSE))
