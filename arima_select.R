arima_select<-function(X, p, q){
  m<-c()
  for (i in 0:p){
    aic1<-sapply(0:q, function(j) arima(X, order=c(i, 1, j), method="ML")$aic)
    m<-rbind(m, aic1)}
  rownames(m)<-0:p
  colnames(m)<-0:q
  print(m)
  return(m)
}
m<-arima_select(X, 5, 5)

arima_plot <- plot_ly(z = m, colorscale = "Greys", type = "heatmap")
arima_plot
plot(arima(X, c(3, 1, 2)))

model<-arima(X, c(3,1,2))