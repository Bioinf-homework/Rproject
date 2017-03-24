x <- c(a1,a2,a3)
sum((x - mean(x))**2)
a1m = mean(a1)
a2m = mean(a2)
a3m = mean(a3)
10*sum(c((a1m-mean(x))**2,(a2m-mean(x))**2,(a3m-mean(x))**2))

sum(c((a1-a1m)**2,(a2-a2m)**2),(a3-a3m)**2)

qf(0.05, 2, 29, lower.tail = FALSE)#相当于查F分布临界值表

qf(0.01, 2, 29, lower.tail = FALSE)

qf(0.001, 2, 29, lower.tail = FALSE)

qf(0.0001, 2, 29, lower.tail = FALSE)

qf(0.00001, 2, 29, lower.tail = FALSE)