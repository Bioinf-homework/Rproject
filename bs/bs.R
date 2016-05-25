#设置工作路径
setwd("D:/programs")

#稍微修改了一点那个文件 and data is list type
data = read.csv("gene expression in cancers.csv")

print(t.test(data$G1,data$G2))

print(t.test(data$G2,data$G3))

print(t.test(data$G3,data$G4))

