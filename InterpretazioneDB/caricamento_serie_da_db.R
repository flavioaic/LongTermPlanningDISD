source("./parametriConnessioniFile.R")

print("#### Esecuzione iniziata ####")

n <- readline(prompt="Vuoi Salvare i file (y/n): ")

if( n == 'y' || n == 'yes'){
  saveFile = TRUE
} else if ( n == 'n' || n == 'no') {
  saveFile = FALSE
} else {
  saveFile = FALSE;
}

sprintf("#### Salvataggio File: %s ####", saveFile)

# Questo script serve a valutare la correlazione tra la query effettuata sul db e le statistiche
# reperibili sul sito ufficiale dell'aeroporto, nello stesso periodo di tempo di quest'ultimo.
#
# Prerequisiti da verificare prima di lanciare lo script:
#  1- verificare che il file voli_sept_2017.csv sia nella stessa cartella del file corrente
#  2- verificare che il parametri di connessione a SQL 2016 sia corretto (server=DESKTOP-32KTKVV)

# CARICAMENTO TS DA STATISTICA SITO UFF -----------------------------------
library(fpp)
library(grid)
library(gridExtra)
library(gtable)
library(zoo)
library(ggplot2)
library(scales)



# ESTRAZIONE DATI DA DB JNB -----------------------------------------------
# Collegati al db di SQL 2016
library(RODBC)
dbhandle <-odbcDriverConnect(connection=param.connessione.db)

# Scarica le serie storiche a partire dalla stessa data di cui alle serie precedenti.
reg.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=1
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
reg.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=1
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
int.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=2
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
int.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=2
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
dom.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=3
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
dom.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=3
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")

# trasforma i dati in time series
startSeriesDB <- c(2003,4)
reg.arr.db <- ts(reg.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
reg.dep.db <- ts(reg.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
int.arr.db <- ts(int.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
int.dep.db <- ts(int.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.arr.db <- ts(dom.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.dep.db <- ts(dom.dep.aflight$CONTO, frequency=12, start=startSeriesDB)



#serie storiche sito
serie.storica <- read.csv2(param.file.voliStatisticheSito.csv, header= TRUE)
# Pulisci la serie da quello che non serve.
serie.storica <- serie.storica[1:71,]
startDateSito <- c(2012,4)
endDateSito <- c(2017,3)
# Trasforma in serie storiche con  start date e end date uguale al db
reg.arr.ss <- ts(serie.storica$Regional_ARR, frequency=12, start=startDateSito, end=endDateSito)
reg.dep.ss <- ts(serie.storica$Regional_DEP, frequency=12, start=startDateSito, end=endDateSito)
int.arr.ss <- ts(serie.storica$International_ARR, frequency=12, start=startDateSito, end=endDateSito)
int.dep.ss <- ts(serie.storica$International_DEP, frequency=12, start=startDateSito, end=endDateSito)
dom.arr.ss <- ts(serie.storica$Domestic_ARR, frequency=12, start=startDateSito, end=endDateSito)
dom.dep.ss <- ts(serie.storica$Domestic_DEP, frequency=12, start=startDateSito, end=endDateSito)


#Per confrontare con le serie storiche del sito, prendi le serie dal db che partono dal 2012
reg.arr.db.window <- window(reg.arr.db, start = startDateSito, end=endDateSito, extend = FALSE)
reg.dep.db.window <- window(reg.dep.db, start = startDateSito, end=endDateSito, extend = FALSE)
int.arr.db.window <- window(int.arr.db, start = startDateSito, end=endDateSito, extend = FALSE)
int.dep.db.window <- window(int.dep.db, start = startDateSito, end=endDateSito, extend = FALSE)
dom.arr.db.window <- window(dom.arr.db, start = startDateSito, end=endDateSito, extend = FALSE)
dom.dep.db.window <- window(dom.dep.db, start = startDateSito, end=endDateSito, extend = FALSE)



plotConfrontoSerie <- function(serieSito, serieDB, title, save = FALSE){
  if(save){
    File <- paste("./InterpretazioneDB/confrontoSerie/",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
  
  plot(serieSito, col=4, main=title, ylab="#Voli", xlab="Year")
  lines(serieDB, col=3, lwd=2)
  legend("topleft", lty=1, col=c(4,3,"midnightblue"), lwd = c(2,2),    
         legend=c("Serie Sito","Serie DB"), bty="n")
  
  if(save){
    dev.off()
  }
  
  if(save){
    File <- paste("./InterpretazioneDB/confrontoSerie/AreaChart",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
  
  df <- data.frame(Y=as.matrix(serieSito), date=as.Date(as.yearmon(time(serieSito))))
  df2 <- data.frame(Y=as.matrix(serieDB), date=as.Date(as.yearmon(time(serieDB))))
  min <- min(df2[,1], df[,1]) - 50
  max <- max(df2[,1], df[,1]) + 10
  
  ggplot() +
    geom_area(data=df, aes(date, Y, fill="Voli Sito"), alpha=0.3) + 
    geom_line(data=df, aes(date, Y), size = 0.5, color="blue") +
    geom_area(data=df2, aes(date, Y,fill="Voli DB"), alpha=0.3)+ 
    geom_line(data=df2, aes(date, Y), size = 0.5, color="red")+
    labs(title=title, 
         y="Numero voli", x = "Data")+
    scale_fill_manual("Serie:", values=c("red", "blue"))+
    scale_y_continuous(limits=c(min,max),oob = rescale_none)
  
  if(save){
    dev.off()
  }
  
}

showErrorConfrontoSerie <- function(serieSito, serieDB, title, dfDifferenze, columnDF){
  print(title)
  print(serieSito-serieDB)
  print(sum(serieSito-serieDB))
  
  de<-data.frame(columnDF, sum(serieSito),sum(serieDB),sum(serieSito-serieDB))
  names(de)<-c("Tipo Voli", "Valori Sito", "Valori DB", "Differenza")
  return (de)  
  # dfDifferenze[nrow(dfDifferenze) + 1,] <- list(columnDF, sum(serieSito),sum(serieDB),sum(serieSito-serieDB) )
  # dfDifferenze
}

printTableDifference <- function(dataFrame, title, save = FALSE){
  
  t1 <- tableGrob(dataFrame)
  titleTable <- textGrob(title, gp=gpar(fontsize=15))
  
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
    File <- paste("./InterpretazioneDB/confrontoSerie/",title,".jpg")
    if (!file.exists(File))  dir.create(dirname(File), showWarnings = FALSE)
    png(File, width = 1200, height = 800)
  }
  
  grid.newpage()
  grid.draw(table)
  
  if(save){
    dev.off()
  }
  
  
}

#Definisco le variabili per tenere traccia dei risultati
dfDifferenze <- data.frame()


# VALUTAZIONE QUERY SU DB - 1° ROUND --------------------------------------
# Realizza grafici comparativi tra i dati estratti dal db e quelli delle statistiche ufficiali

plotConfrontoSerie(reg.arr.ss,reg.arr.db.window,"Confronto Arrivi Reg Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(reg.arr.ss,reg.arr.db.window, "Errori Arrivi Reg Sito vs Arrivi DB", dfDifferenze, "Arrv Reg")
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(reg.dep.ss,reg.dep.db.window,"Confronto Dep Reg Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(reg.dep.ss,reg.dep.db.window, "Errori Dep Reg Sito vs Arrivi DB", dfDifferenze, "Dep Reg")
dfDifferenze <- rbind(dfDifferenze, result)

plotConfrontoSerie(int.arr.ss,int.arr.db.window, "Confronto Arr Int Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(int.arr.ss,int.arr.db.window, "Errori Arr Int Sito vs Arrivi DB", dfDifferenze, "Arr Int")
dfDifferenze <- rbind(dfDifferenze, result)

plotConfrontoSerie(int.dep.ss,int.dep.db.window, "Confronto Dep Int Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(int.dep.ss,int.dep.db.window, "Errori Dep Int Sito vs Arrivi DB", dfDifferenze, "Dep Int" )
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(dom.arr.ss,dom.arr.db.window, "Confronto Arr Dom Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(dom.arr.ss,dom.arr.db.window, "Errori Arr Dom Sito vs Arrivi DB", dfDifferenze, "Arr Dom"  )
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(dom.dep.ss,dom.dep.db.window, "Confronto Dep Dom Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(dom.dep.ss,dom.dep.db.window, "Errori Dep Dom Sito vs Arrivi DB", dfDifferenze, "Dep Dom"  )
dfDifferenze <- rbind(dfDifferenze, result)



printTableDifference(dfDifferenze, "Numero Voli Sito vs DB", saveFile)


# AGGIUSTAMENTO QUERY SU DB - 2° ROUND ------------------------------------
# I dati ottenuti nel round precedente indicano che i dati mensili sono sempre 
# inferiori e spesso di quantità costanti. Si ipotizza un errore sistematico
# che porti a sottostimare la serie, dimenticando qualche componente importante.
# Questa componente deve essere poco rilvante per i regionali (si vedano plot sopra)
#
# Aggiungi i voli transit:
### INTERNAZIONALI
int.dep.aflight.transit<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                                   FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                                   WHERE A_FLIGHT.ID in
                                   (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                                   FROM [AMS516].[dbo].[A_MOVEMENT] 
                                   INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                                   and S_ROUTE.FLIGHT_TYPE_ID=2
                                   and A_FLIGHT.STO between '20120401' and '20171231'
                                   and A_FLIGHT.QUALIFIER_ID=5
                                   and A_FLIGHT.EXCEPTION_ID is null
                                   GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                                   order by ANNO, MESE;")
int.dep.db.transit <- ts(int.dep.aflight.transit$CONTO, frequency=12, start=2012+3/12, end=endDateSito)
int.dep.db2 <- int.dep.db.window+int.dep.db.transit

plotConfrontoSerie(int.dep.ss,int.dep.db2,"Confronto Migliorato Dep Int Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(int.dep.ss,int.dep.db2, "Confronto Migliorato Dep Int Sito vs DB", dfDifferenze, "Dep Int Migli I")
dfDifferenze <- rbind(dfDifferenze, result)

int.arr.aflight.transit<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                                   FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                                   WHERE A_FLIGHT.ID in
                                   (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                                   FROM [AMS516].[dbo].[A_MOVEMENT] 
                                   INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                                   and S_ROUTE.FLIGHT_TYPE_ID=2
                                   --and A_FLIGHT.STO between '20120401' and '20171231'
                                   and A_FLIGHT.QUALIFIER_ID=5
                                   and A_FLIGHT.EXCEPTION_ID is null
                                   GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                                   order by ANNO, MESE;")

int.arr.db.transit <- ts(int.arr.aflight.transit$CONTO, frequency=12, start=2012+3/12, end=endDateSito)
int.arr.db2 <- int.arr.db+int.arr.db.transit

plotConfrontoSerie(int.arr.ss,int.arr.db2,"Confronto Migliorato Arr Int Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(int.arr.ss,int.arr.db2, "Confronto Migliorato Arr Int Sito vs DB", dfDifferenze, "Arr Int Migli I" )
dfDifferenze <- rbind(dfDifferenze, result)
### DOMESTICI
dom.dep.aflight.transit<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                            WHERE A_FLIGHT.ID in
                            (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                            FROM [AMS516].[dbo].[A_MOVEMENT] 
                            INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                            and S_ROUTE.FLIGHT_TYPE_ID=3
                            and A_FLIGHT.STO between '20120401' and '20171231'
                            and A_FLIGHT.QUALIFIER_ID=5
                            and A_FLIGHT.EXCEPTION_ID is null
                            GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                            order by ANNO, MESE;")
dom.dep.db.transit <- ts(dom.dep.aflight.transit$CONTO, frequency=12, start=2012+3/12, end=endDateSito)
dom.dep.db2 <- dom.dep.db+dom.dep.db.transit

plotConfrontoSerie(dom.dep.ss,dom.dep.db2,"Confronto Migliorato Dep Dom Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(dom.dep.ss,dom.dep.db2, "Confronto Migliorato Dep Dom Sito vs DB", dfDifferenze, "Dep Dom Migli I")
dfDifferenze <- rbind(dfDifferenze, result)

printTableDifference(dfDifferenze, "Numero Voli Sito vs DB Migliorato I", saveFile)


# PROVIAMO A MIGLIORARE ULTERIORMENTE - 3? ROUND --------------------------
dom.arr.aflight.transit<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                                   WHERE A_FLIGHT.ID in
                                   (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                                   FROM [AMS516].[dbo].[A_MOVEMENT] 
                                   INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                                   and S_ROUTE.FLIGHT_TYPE_ID=3
                                   and A_FLIGHT.STO between '20120401' and '20171231'
                                   and A_FLIGHT.QUALIFIER_ID=5
                                   and A_FLIGHT.EXCEPTION_ID is null
                                   GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                                   order by ANNO, MESE;")
dom.arr.db.transit <- ts(dom.arr.aflight.transit$CONTO, frequency=12, start=2012+2/12, end=endDateSito)
dom.arr.db2 <- dom.arr.db+dom.arr.db.transit

plotConfrontoSerie(dom.arr.ss,dom.arr.db2,"Confronto Migliorato II Dep Dom Sito vs DB", saveFile)
resutl <- showErrorConfrontoSerie(dom.arr.ss,dom.arr.db2, "Confronto Migliorato II Dep Dom Sito vs DB", dfDifferenze, "Dep Dom Migli II")
dfDifferenze <- rbind(dfDifferenze, result)
printTableDifference(dfDifferenze, "Numero Voli Sito vs DB Migliorato II", saveFile)



#Confronto serie storiche db (complete) con serie storiche sito

plotConfrontoSerie(reg.arr.ss,reg.arr.db,"Confronto Arrivi Reg Sito vs DB Completo", saveFile)
result <- showErrorConfrontoSerie(reg.arr.ss,reg.arr.db, "Errori Arrivi Reg Sito vs Arrivi DB Completo", dfDifferenze, "Comp Arrv Reg")
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(reg.dep.ss,reg.dep.db,"Confronto Dep Reg Sito vs DB Completo", saveFile)
result <- showErrorConfrontoSerie(reg.dep.ss,reg.dep.db, "Errori Dep Reg Sito vs Arrivi DB Completo", dfDifferenze, "Comp Dep Reg")
dfDifferenze <- rbind(dfDifferenze, result)

plotConfrontoSerie(int.arr.ss,int.arr.db, "Confronto Arr Int Sito vs DB Completo", saveFile)
result <- showErrorConfrontoSerie(int.arr.ss,int.arr.db, "Errori Arr Int Sito vs Arrivi DB Completo", dfDifferenze, "Comp Arr Int")
dfDifferenze <- rbind(dfDifferenze, result)

plotConfrontoSerie(int.dep.ss,int.dep.db, "Confronto Dep Int Sito vs DB Completo", saveFile)
result <- showErrorConfrontoSerie(int.dep.ss,int.dep.db, "Errori Dep Int Sito vs Arrivi DB Completo", dfDifferenze, "Comp Dep Int" )
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(dom.arr.ss,dom.arr.db, "Confronto Arr Dom Sito vs DB Completo", saveFile)
result <- showErrorConfrontoSerie(dom.arr.ss,dom.arr.db, "Errori Arr Dom Sito vs Arrivi DB Completo", dfDifferenze, "Comp Arr Dom"  )
dfDifferenze <- rbind(dfDifferenze, result)


plotConfrontoSerie(dom.dep.ss,dom.dep.db.window, "Confronto Dep Dom Sito vs DB", saveFile)
result <- showErrorConfrontoSerie(dom.dep.ss,dom.dep.db.window, "Errori Dep Dom Sito vs Arrivi DB", dfDifferenze, "Comp Dep Dom"  )
dfDifferenze <- rbind(dfDifferenze, result)


printTableDifference(dfDifferenze, "Numero Voli Sito vs DB", saveFile)

print("#### Esecuzione terminata. File salvati in InterpretazioneDB/confrontoSerie/ ####")

# PASSAGGIO DATI PER ANALISI E MODELLI PREVISIONALI TS --------------------
# In questa sezione, i dati estratti vengono inviati a script dedicati per l'applicazione
# dei modelli decisionali.
# NOTA BENE: per il momento ci concentriamo su una sola componente.
# source("serie_aeroporto.R")  #dom.arr 1
# source("serie_aeroporto.R")  #dom.dep 2
# source("serie_aeroporto.R")  #int.arr 3
# source("serie_aeroporto.R")  #int.dep 4
# source("serie_aeroporto.R")  #reg.arr 5
# source("serie_aeroporto.R")  #reg.dep 6
# 
# source("serie_aeroporto_arima.R")  #dom.arr 1
# source("serie_aeroporto_arima.R")  #dom.dep 2
# source("serie_aeroporto_arima.R")  #int.arr 3
# source("serie_aeroporto_arima.R")  #int.dep 4
# source("serie_aeroporto_arima.R")  #reg.arr 5
# source("serie_aeroporto_arima.R")  #reg.dep 6


