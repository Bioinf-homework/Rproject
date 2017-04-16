for(i in 1:12)
{
  wrongnum = 180 - length(m[[i]]);
  print(wrongnum)
  print(wrongnum/180)
}

for (i in 1:12) {
  x = boxplot(abs( m[[i]]),plot = FALSE)
  upperlimit[i] = min(x$out)
  print (length(x$out))
}
