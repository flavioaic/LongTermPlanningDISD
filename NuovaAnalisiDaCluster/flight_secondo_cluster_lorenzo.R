# ESTRAZIONE DATI DA DB JNB -----------------------------------------------
# Collegati al db di SQL 2016
library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-HH30FVF;database=AMs516;trusted_connection=yes;")

# Scarica le serie storiche a partire dalla stessa data di cui alle serie precedenti.

#reg.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
#                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
#                           WHERE A_FLIGHT.ID in
#                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
#                           FROM [AMS516].[dbo].[A_MOVEMENT] 
#                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
#                           and S_ROUTE.FLIGHT_TYPE_ID=1
#                           and A_FLIGHT.STO between '20120401' and '20171231'
#                           and A_FLIGHT.QUALIFIER_ID=1
#                           and A_FLIGHT.EXCEPTION_ID is null
#                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
#                           order by ANNO, MESE;")


a_flight   <- sqlQuery(dbhandle, "SELECT * FROM A_FLIGHT")
a_movement <- sqlQuery(dbhandle, "SELECT * FROM A_MOVEMENT")
library(dplyr)
dep_joined <- left_join(a_movement,a_flight,by=c("DEP_FLIGHT_ID"="ID"))
dep_joined <- select(dep_joined, DEP_FLIGHT_ID, AIRCRAFT_ID, AIRCRAFT_ID, AIRCRAFT_TYPE_ID, ROUTE_ID, FLIGHT_TYPE_ID, STO, QUALIFIER_ID, EXCEPTION_ID)
# Query associate (ricavate da datamining originario)
dep_joined <-rbind(
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==1 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),], #Regionali 
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==2 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),], #Internazionali
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==2 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==5),],
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==3 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),]) #Domestici
#
dep_joined <- left_join(dep_joined,as.data.frame(mappa_cluster),by=c("DEP_FLIGHT_ID"="ID"))
#
head(dep_joined)

source("time_series_analysis_from_clusters.r")
printCluster(kvoli_rotte, voli_rotte_clean)

#itera sul numero di cluster
for (i in seq(1,n_cluster)) {
  #per ogni cluster estrai i voli associati
  nam <- paste("A", i, sep = "")
  temp <- dep_joined %>% filter(cluster == i)###### %>% filter(QUALIFIER_ID == 1)
  # format delle date
  x <-format(temp, format="%Y-%m")
  #crea il data frame che contiene le frequenze delle date (Data, numero voli)
  df <- as.data.frame(table(x$STO))
  #crea la serie storica
  tserie <- ts(df[,-1], frequency=12, start=c(2003,4), end=c(2017,3))
  #visualizza i plot della serie storica
  plotTimeseries(tserie, paste("#Voli Cluster ",i))
  #cerca il miglior modello tra "Mean method","Naive method","Drift method", "Seasonal naive method"
  models <- plotForecastTrainingSet(tserie, 36, paste("#Voli Cluster ",i))
  #visualizza arima
  arimaModel <- plotArimaModel(tserie, 36, paste("#Voli Cluster ",i))
  #prendi il miglior modello
  bestModel <- evaluateBesModel(tserie, 36, paste("#Voli Cluster ",i))
  #print del modello migliore
  print( paste("Il miglior modello in base alle misure di errore per", paste("#Voli Cluster",i),"e' ",bestModel ))
  #names(temp.table)[which.min(apply(temp.table[5,],MARGIN=2,min))]
  
}


