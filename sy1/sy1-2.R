#
x<-runif(10000,0,100)

m1 <- function(x){
  sum = 0
  for(n in x){
    sum = sum +n
  }
  return (sum/length(x))
}
var1 <- function(x){
  mean = m1(x)
  sum = 0
  for(n in x){
    sum = sum + (n-mean)**2
  }
  return(sum/length(x))
}
sd1<- function(x)
{
  return(var1(x)**0.5)
}
cv1 <- function(x)
{
  return(sd1(x)/m1(x))
}
m2 <- function(x)
{
  x = sort(x)
  if(length(x)%%2==0){
    
    return((x[length(x)/2]+x[length(x)/2+1])/2)
  }else{
    return(x[ceiling(length(x)/2)])
  }
}
q4 <-function(x){
  x = sort(x)
  return(list(x[length(x)/4],x[length(x)/4*3]))
}
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

system.time(print(analysis(x)))
system.time(print(analysis_me(x)))

