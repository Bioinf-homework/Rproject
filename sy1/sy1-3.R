#attach(mtcars)
#layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
f1 <- function(n){
  a <- c()
  for(i in 1:10000){
    x1<-rbinom(n,1,0.5)
    a <-c(a,mean(x1))
  }
  return(a)
}

draw_hist <- function(){
   hist(f1(3))
   hist(f1(5))
   hist(f1(10))
   hist(f1(100))
   hist(f1(1000))
   hist(f1(10000))
}
draw_hist()
