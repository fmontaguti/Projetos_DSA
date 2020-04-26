#Funções Auxiliares

#função para plotar gráficos de barra dos top N
plot.bars = function(df,column,N){
  df1 = df %>%
    filter(is_attributed == 0) %>%
    group_by_(column) %>%
    summarise(Freq=n()) %>%
    arrange(desc(Freq)) %>%
    head(N)
  
  df2 = df %>%
    filter(is_attributed == 1) %>%
    group_by_(column) %>%
    summarise(freq=n()) %>%
    arrange(desc(freq)) %>%
    head(N)
  
  data = data.frame(
    x = df1[column],
    y = df1['Freq'],
    x2 = df2[column],
    y2 = df2['freq']
  )
  
  names(data) = c('x','y','x2','y2')
  
  a = ggplot(data,aes(reorder(x,-y),y))+
    geom_bar(stat = "identity", fill = "steelblue", size = 1)+
    labs(title = paste('Top',N,'Clicks por',toupper(column)))+
    xlab('')+
    ylab('Frequência')
  
  b = ggplot(data,aes(reorder(x2,-y2),y2))+
    geom_bar(stat = "identity", fill = "steelblue", size = 1)+
    labs(title = paste('Top',N,'Downloads por',toupper(column)))+
    xlab('')+
    ylab('Frequência')
  
  return(grid.arrange(a,b))
}

#Pacote Utils_R para plotar curvas ROC

plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}