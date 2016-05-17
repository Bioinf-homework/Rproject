x<-rnorm(1000,10.2,2)

u = 10

# print(x)

result = t.test(x,mu = u,conf.level = 0.99,var.equal = FALSE)

print(result)