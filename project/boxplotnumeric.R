boxplotnumeric<- function(data)
{
  
  data$over50k=ifelse(data$over50k==data$over50k[1],0,1)
  nums <- sapply(data, is.numeric)
  data=data[nums]
  
  df.m=melt(data,id.var="over50k")
  df.m$over50k=ifelse(df.m$over50k==1,">50k","<=50k")
  p <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=over50k)) + facet_wrap( ~ variable, scales="free")
  print(p)
  
  
}