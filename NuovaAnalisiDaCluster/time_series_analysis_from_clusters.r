library(fpp)
library(lubridate)
library(plyr)
library(stlplus)
library(pander)
library(xtable)
library(grid)
library(gridExtra)
library(gtable)
library(fpp)
library(ggfortify)

printCluster <- function(kvoli_rotte, dataFrame) {
  # Plot del cluster a partire dal kmeans
  #
  # Args:
  #   kvoli_rotte: risultato dell'esecuzione del kmeans
  #   dataFrame: data frame sul quale visualizzare i cluster
  
  cluster <- factor(kvoli_rotte$cluster)
  centroids <- data.frame( kvoli_rotte$centers)
  ggplot() + 
    geom_point(data = as.data.frame(dataFrame), 
               aes(x = distanza, 
                   y = MTOW,
                   color = cluster),
               size = 3) + 
    geom_point(data=centroids, aes(x=distanza,y=MTOW, color="Center")) +
    geom_point(data=centroids, aes(x=distanza,y=MTOW, color="Center"), size=52, alpha=.3, show.legend=F) 
}


plotTimeseries <- function(tserie, title, save = TRUE) {
  # Plot della serie storica passata in input
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tittle: titolo da visualizzare nel plot
  
  if(save){
    File <- paste("./resultAnalysis/",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File,width = 1200, height = 800)
  }
  
  plot(tserie, col="darkblue", main=title)
  
  if(save){
    dev.off()
  }
  
  if(save){
    File <- paste("./resultAnalysis/","Boxplot ", title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File,width = 1200, height = 800)
  }
  
  boxplot(split(tserie, cycle(tserie)),main=paste("Boxplot ",title), names = month.abb, col = "gold")
  
  if(save){
    dev.off()
  }
}


plotForecastTrainingSet <- function(tserie, tWindow = 36, title, save = TRUE){
  # Training di diversi modelli di previsione a partire dal training set. Dopo il training
  # viene effettuato il plot delle previsioni di ogni modello a partire dalla fine del training set.
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   tittle: titolo da visualizzare nel plot
  #   save: salva il grafico invece di mostrarlo

    # training set
  sr <- window(tserie, start=c(2003,4), end=c(2014,4))
  # test set
  ser = window(tserie, start=c(2014,5), end=c(2017,3))
  
  # variabili che servono per calcolarmi il minimo e il massimo dei modelli (per il grafico)
  maxValue <- sr[which.max(sr)]
  minValue <- sr[which.min(sr)]
  
  meanMethod <- meanf(sr,h=tWindow)
  
  maxValue <- if(meanMethod$mean[which.max(meanMethod$mean)]>maxValue) meanMethod$mean[which.max(meanMethod$mean)] else maxValue
  minValue <- if(meanMethod$mean[which.min(meanMethod$mean)]<minValue) meanMethod$mean[which.min(meanMethod$mean)] else minValue
  
  naiveMethod <- rwf(sr,h=tWindow)
  
  maxValue <- if(naiveMethod$mean[which.max(naiveMethod$mean)]>maxValue) naiveMethod$mean[which.max(naiveMethod$mean)] else maxValue
  minValue <- if(naiveMethod$mean[which.min(naiveMethod$mean)]<minValue) naiveMethod$mean[which.min(naiveMethod$mean)] else minValue
  
  sNaiveMethod <- rwf(sr,drift=TRUE,h=tWindow)
  
  maxValue <- if(sNaiveMethod$mean[which.max(sNaiveMethod$mean)]>maxValue) sNaiveMethod$mean[which.max(sNaiveMethod$mean)] else maxValue
  minValue <- if(sNaiveMethod$mean[which.min(sNaiveMethod$mean)]<minValue) sNaiveMethod$mean[which.min(sNaiveMethod$mean)] else minValue
  
  driftMethod <- snaive(sr,h=tWindow)
  
  maxValue <- if(driftMethod$mean[which.max(driftMethod$mean)]>maxValue) driftMethod$mean[which.max(driftMethod$mean)] else maxValue
  minValue <- if(driftMethod$mean[which.min(driftMethod$mean)]<minValue) driftMethod$mean[which.min(driftMethod$mean)] else minValue
  
  if(save){
    File <- paste("./resultAnalysis/","Predizioni Dopo Training Set-",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
 
  
  # plot delle predizioni a partire dalla fine del training set
  plot(meanf(sr,h=tWindow, level=c(0,0)), col=4, main=paste("Predizioni Dopo Training Set-",title), ylab="", xlab="Months",ylim = c(minValue,maxValue+20), lwd=2)
  lines(naiveMethod$mean, col=2, lwd=2)
  lines(sNaiveMethod$mean, col=3, lwd=2)
  lines(driftMethod$mean, col=5, lwd=2)
  
  # the test set
  lines(ser, col="midnightblue", lwd = 3)
  
  legend("topleft", lty=1, col=c(4,2,3,5, "midnightblue"), lwd = c(2,2,2,2,3),    
         legend=c("Mean method","Naive method","Drift method", "Seasonal naive method", "Original test set"), bty="n")
  
  if(save){
    dev.off()
  }
  
  if(save){
    File <- paste("./resultAnalysis/","Confronto Predizioni su Test Set-",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File,  width = 1200, height = 800)
  }
 
  
  plot(ser, col="midnightblue",  main=paste("Confronto Predizioni su Test Set-",title), ylab="",ylim = c(minValue,maxValue+20), xlab="Months",  lwd = 2)
  
  lines(meanMethod$mean, col=4)
  lines(naiveMethod$mean, col=2)
  lines(sNaiveMethod$mean, col=3)
  lines(driftMethod$mean, col=5)
  
  legend("topleft", lty=1, col=c(4,2,3,5,"midnightblue"), lwd = c(1,1,1,1,2),
         legend=c("Mean method","Naive method","Drift method", "Seasonal naive method", "Original test set"),bty="n")
  
  if(save){
    dev.off()
  }

  
}


plotArimaModel <- function(tserie, tWindow, title, save = TRUE){
  # Training del modello arima. Dopo il training viene effettuato il plot
  # delle previsioni di ogni modello a partire dalla fine del training set.
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   tittle: titolo da visualizzare nel plot
  #   save: salva il grafico invece di mostrarlo
  
  trainData <- window(tserie, start=c(2003,4), end=c(2014,4))
  
  testData <- window(tserie, start=c(2014,5), end=c(2017,3))
  
  arimaMod <- auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  if(save){
    File <- paste("./resultAnalysis/",arimaMod.Fr$method,title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
 
  
  # plot delle predizioni a partire dalla fine del training set
  plot(forecast(arimaMod, h=tWindow, level=c(0,0)), main = paste(arimaMod.Fr$method,title), type = "l")
  lines(testData, col='red')
  legend("topleft",lty=1,bty = "n",col=c("blue","red"),c("testData","ARIMAPred"))
  
  if(save){
    dev.off()
  }
  
  AR.mean <-forecast(arimaMod,h=tWindow)$mean
  
  
  if(save){
    File <- paste("./resultAnalysis/","Confronto Arima su Test Set",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
 
  # plot delle predizioni a partire dalla fine del training set confrontate con il test set
  plot(testData, main= paste("Confronto Arima su Test Set",title), ylab="", xlab="Months", col="darkblue")  
  lines(AR.mean, col="red")
  legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("ARIMAPred","testData"))
  
  if(save){
    dev.off()
  }
  
  
}



evaluateBesModel <- function(tserie, tWindow, title, save = TRUE){
  # Calcola il modello migliore in base alle misure di accuracy dei modelli.
  # Dopo il calcolo viene visualizzata la tabella con tutte le misure di errore
  # 
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   title: titolo da visulizzare nella tabella delle misure di errore
  #   save: salva il grafico invece di mostrarlo
  #
  # Returns:
  #   Il modello migliore in base alle metriche "ME", "RMSE", "MAE", "MPE", "MAPE"
 
  sr <- window(tserie, start=c(2003,4), end=c(2014,4))
  ser <- window(tserie, start=c(2014,5), end=c(2017,3))
  
  meanMethod <- meanf(sr,h=tWindow)
  naiveMethod <- rwf(sr,h=tWindow)
  sNaiveMethod <- rwf(sr,drift=TRUE,h=tWindow)
  driftMethod <- snaive(sr,h=tWindow)
  arimaMod <- auto.arima(sr, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  a1 = accuracy(meanMethod, ser)
  a2 = accuracy(naiveMethod, ser)
  a3 = accuracy(sNaiveMethod, ser)
  a4 = accuracy(driftMethod, ser)
  a5 = accuracy(arimaMod.Fr, ser)
  
  keeps <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  a1 <- a1[1,keeps, drop = FALSE]
  a2 <- a2[1,keeps, drop = FALSE]
  a3 <- a3[1,keeps, drop = FALSE]
  a4 <- a4[1,keeps, drop = FALSE]
  a5 <- a5[1,keeps, drop = FALSE]
  
  temp.table <- as.data.frame(cbind(a1[1, ], a2[1, ], a3[1, ], a4[1, ], a5[1, ]))
  
  
  colnames(temp.table) <-
    c("Average Method",
      "Naive Methiod",
      "SNaive Method",
      "Drift method",
      "Arima")
  
  is.num <- sapply(temp.table, is.numeric)
  temp.table[is.num] <- lapply(temp.table[is.num], round, 10)
  
  t1 <- tableGrob(temp.table)
  titleTable <- textGrob(paste("Errori Test Set-", title), gp=gpar(fontsize=25))
  
  padding <- unit(5,"mm")
  
  table <- gtable_add_rows(
    t1, 
    heights = grobHeight(titleTable) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    titleTable, 
    1, 1, 1, ncol(table))
  
  if(save){
    File <- paste("./resultAnalysis/","Errori Test Set-",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File,width = 1200, height = 800)
  }
  
  grid.newpage()
  grid.draw(table)
  
  if(save){
    dev.off()
  }
  dfEvalColumn <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  dfEvalRow <- c(names(temp.table)[which.min(apply(temp.table[1,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[2,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[3,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[4,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[5,],MARGIN=2,min))])
  dfEval <- data.frame(dfEvalColumn, dfEvalRow)
  
  tableEval <-  table(dfEval$dfEvalRow)
  
  return (names(tableEval)[which.max(tableEval)])
  
}
