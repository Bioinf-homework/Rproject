x11 <- c(1.0838,0.9425,0.3335)
x12 <- c(0.955,0.9215,0.8514)
x21 <- c(0.7069,0.7854,0.3581)
x22 <- c(0.1885,0.3403,0.2503)
# x11 <- c(16,25,18,23,21,14,13,22)

x = c(x11,x12,x21,x22)

c1 = c(x11,x21)
c2 = c(x12,x22)
SSb = 2*3*((mean(c1)-mean(x))**2+(mean(c2)-mean(x))**2)
r1 = c(x11,x12)
r2 = c(x21,x22)
SSa = 2*3*((mean(r1)-mean(x))**2+(mean(r2)-mean(x))**2)

# 3*((mean(c1)-mean(x))**2+(mean(c2)-mean(x))**2+(mean(r1)-mean(x))**2+(mean(r2)-mean(x))**2)
SStreat = 3*((mean(x11)-mean(x))**2+(mean(x12)-mean(x))**2+(mean(x21)-mean(x))**2+(mean(x22)-mean(x))**2)

SSt = sum((x-mean(x))**2)
SSab = SStreat - SSa - SSb
SSe = SSt - SStreat


# > MSe = SSe/8
# > SSa / MSe
# [1] 9.183943
# > SSb / MSe
# [1] 0.7515406
# > SSab / MSe

# > qf(0.05, 1, 11, lower.tail = FALSE) # 查临界F值
# [1] 4.844336

# > pf(0.7515406, 1, 11) #求P值
# [1] 0.5954971
# >  pf(9.183943, 1, 11)
# [1] 0.988561
# >  pf(3.148892, 1, 11)
# [1] 0.8963777