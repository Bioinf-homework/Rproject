# 随机生成0-100的10000个数字，并输出（以print 一个list是实现）
# 两份实现方法，一份大部分用系统自带的函数
# 另一份大多是自己写的函数，可能有部分不准

x<-runif(10000,0,100) # 生成10000个0-100的随机数

# 计算平均数
m1 <- function(x){
  sum = 0
  for(n in x){
    sum = sum +n
  }
  return (sum/length(x))
}

# 计算方差
var1 <- function(x){
  mean = m1(x)
  sum = 0
  for(n in x){
    sum = sum + (n-mean)**2
  }
  return(sum/length(x))
}

# 计算标准差
sd1<- function(x)
{
  return(var1(x)**0.5)
}
# 计算变异系数
cv1 <- function(x)
{
  return(sd1(x)/m1(x))
}
# 计算中位数
m2 <- function(x)
{
  x = sort(x)
  if(length(x)%%2==0){
    
    return((x[length(x)/2]+x[length(x)/2+1])/2)
  }else{
    return(x[ceiling(length(x)/2)])
  }
}
# 计算四分位数
q4 <-function(x){
  x = sort(x)
  return(list(x[length(x)/4],x[length(x)/4*3]))
}

# 自己的计算统计量函数
analysis_me <- function(x)
{
  mea_num = m1(x)
  med_num = m2(x)
  
  var_num = var1(x)
  sd_num = sd1(x)
  
  cv_num <- sd_num/mea_num
  q = q4(x)
  kur = kurtosis(x)
  ske = skewness(x)
  data <- list(mean=mea_num,var=var_num,sd=sd_num,cv=cv,median=med_num,q4=q,skewness=ske,kurtosis=kur)
  return(data)
}

# 集成R函数的计算统计量函数
analysis <- function(x){
  
  mea_num = mean(x)
  med_num = median(x)
  
  var_num = var(x)
  sd_num = sd(x)
  
  cv_num <- sd_num/mea_num
  q = quantile(x)
  q25 = q[2]
  q75 = q[4]
  kur = kurtosis(x)
  ske = skewness(x)
  data <- list(mean=mea_num,var=var_num,sd=sd_num,cv=cv,median=med_num,q4=q,skewness=ske,kurtosis=kur)
  return(data)
}

# 比较花费时间，结果在报告中
# print(analysis(x))
system.time(print(analysis(x)))
# print(analysis_me(x))
system.time(print(analysis_me(x)))

