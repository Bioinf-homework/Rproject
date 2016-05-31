x1<-rnorm(15,10.2,2)
x2<-rnorm(15,10,2.1)
# F检验 验证总体方差差异性
var.test(x1,x2)

# t检验 验证总体均数差异性
t.test(x1,x2)
