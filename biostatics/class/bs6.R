x = c(2,4,9,12,19,19,29,35,39,41,49,50,59,59,69,71,77,79)
y = c(14.29,13.85,12.92,12.88,12.16,12.15,11.91,11.82,11.68,11.51,11.25,11.24,10.94,10.91,10.39,10.42,10.31,10.31)

level = data.frame(x,y)

plot(level,main='散点图',xlab='年龄(单位：年)',ylab='端粒长度（单位：kb）')

# require(stats)
# lines(lowess(level))

reg <- lm(y~1+x)

summary(reg)

print(reg$coefficients)