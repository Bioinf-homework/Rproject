# chisq.test 要求正数?

setwd("/media/tmn07/3AD6217DD6213A91/Programs/Rproject/sy2")

#x1_1<-rnorm(100,100,1)
#x1_2<-rnorm(100,100,5)
#x1_3<-rnorm(100,100,10)
#save(x1_1,x1_2,x1_3,file = 'sy2-1-norm.Rdata')


#x2_1<-rexp(10,1)
#x2_2<-rexp(100,1)
#x2_3<-rexp(1000,1)
#save(x2_1,x2_2,x2_3,file = 'sy2-1-exp.Rdata')

#x3_1<-rpois(10000,1)
#x3_2<-rpois(1000,1)
#x3_3<-rpois(100,1)
#save(x3_1,x3_2,x3_3,file = 'sy2-1-pois.Rdata')
#x4_1 = 100+rt(100,df=100)
#x4_2 = 100+rt(100,df=200)
#x4_3 = 100+rt(200,df=500)
#save(x4_1,x4_2,x4_3,file = 'sy2-1-t.Rdata')

#load(file='sy2-1.Rdata')


test <- function(x,seq){
  count_list <- c()
  for (s in 2:length(seq)-1) {
    count = 0
    for (i in x) {
      if (i>=seq[s]&&i<seq[s+1])  
      {
        count = count+1
      }
    }
    #print(count)
    if(count < 5)
    {
      return(FALSE)
    }
    else{
      count_list <- c(count_list,count)
    }
  }
  return(count_list)
}



main <- function(x){
  m = mean(x)
  s = sd(x)
  ran = range(x)
  for (n in 9:2) {
    seq <- seq(ran[1],ran[2],(ran[2] - ran[1])/n)
    Oi <- test(x,seq)
    if(Oi!=FALSE){
      break
    }
    else{
      #print(n)
    }
  }
  #print(seq)
  u <- ((seq- m)/s)
  Ti <- c()
  for(i in 2:length(u)-1){
    Ti<-c(Ti,pnorm(u[i+1])-pnorm(u[i]))
  }
  #theoretical
  Ti = length(x)*Ti
  #pnorm(-2.2956)
  
  #print(Oi)
  #print(Ti)
  X2 <- sum((Oi - Ti)**2/Ti)
  #print(X2)
  return(X2)
}
