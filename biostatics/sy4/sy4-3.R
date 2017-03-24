setwd("D:/this term/生统/实验/4")

data = read.table("step.data.txt")

data$d1 = data$x1*data$x2
data$d2 = data$x1*data$x3
data$d3 = data$x2*data$x3
data$d4 = data$x1*data$x2*data$x3
data$s1 = data$x1^2
data$s2 = data$x2^2
data$s3 = data$x3^2
reg = lm(y~x1+x2+x3+d1+d2+d3+d4+s1+s2+s3,data = data)


summary(reg)

tstep<-step(reg)

summary(tstep)