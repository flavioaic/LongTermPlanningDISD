# Questo script serve a valutare la correlazione tra la query effettuata sul db e le statistiche
# reperibili sul sito ufficiale dell'aeroporto, nello stesso periodo di tempo di quest'ultimo.
#
# Prerequisiti da verificare prima di lanciare lo script:
#  1- verificare che il file voli_sept_2017.csv sia nella stessa cartella del file corrente
#  2- verificare che il parametri di connessione a SQL 2016 sia corretto (server=DESKTOP-32KTKVV)

# CARICAMENTO TS DA STATISTICA SITO UFF -----------------------------------
library(fpp)
setwd("C:/Users/Flavio Aicardi/Documents/SERIE_AEROPORTO/aeroporto_ts/")
serie.storica <- read.csv2("voli_dec_2017.csv", header= TRUE)
# Pulisci la serie da quello che non serve.
serie.storica <- serie.storica[1:69,]
# Trasforma in serie storiche
reg.arr.ss <- ts(serie.storica$Regional_ARR, frequency=12, start=2012+3/12)
reg.dep.ss <- ts(serie.storica$Regional_DEP, frequency=12, start=2012+3/12)
int.arr.ss <- ts(serie.storica$International_ARR, frequency=12, start=2012+3/12)
int.dep.ss <- ts(serie.storica$International_DEP, frequency=12, start=2012+3/12)
dom.arr.ss <- ts(serie.storica$Domestic_ARR, frequency=12, start=2012+3/12)
dom.dep.ss <- ts(serie.storica$Domestic_DEP, frequency=12, start=2012+3/12)


# ESTRAZIONE DATI DA DB JNB -----------------------------------------------
# Collegati al db di SQL 2016
library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-32KTKVV;database=AMs516;trusted_connection=yes;")

# Scarica le serie storiche a partire dalla stessa data di cui alle serie precedenti.
reg.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
  FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=1
                           and A_FLIGHT.STO between '20120401' and '20171231'
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
                           and A_FLIGHT.STO between '20120401' and '20171231'
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
                           and A_FLIGHT.STO between '20120401' and '20171231'
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
                           and A_FLIGHT.STO between '20120401' and '20171231'
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
                           and A_FLIGHT.STO between '20120401' and '20171231'
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
                           and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")

# trasforma i dati in time series
reg.arr.db <- ts(reg.arr.aflight$CONTO, frequency=12, start=2012+3/12)
reg.dep.db <- ts(reg.dep.aflight$CONTO, frequency=12, start=2012+3/12)
int.arr.db <- ts(int.arr.aflight$CONTO, frequency=12, start=2012+3/12)
int.dep.db <- ts(int.dep.aflight$CONTO, frequency=12, start=2012+3/12)
dom.arr.db <- ts(dom.arr.aflight$CONTO, frequency=12, start=2012+3/12)
dom.dep.db <- ts(dom.dep.aflight$CONTO, frequency=12, start=2012+3/12)


# VALUTAZIONE QUERY SU DB - 1° ROUND --------------------------------------
# Realizza grafici comparativi tra i dati estratti dal db e quelli delle statistiche ufficiali
plot(reg.arr.ss, type = "l", col="red")
lines(reg.arr.db, col="blue")
# Valutazione errori
reg.arr.ss-reg.arr.db
sum(reg.arr.ss-reg.arr.db)


plot(reg.dep.ss, type = "l", col="red")
lines(reg.dep.db, col="blue")
# Valutazione errori
reg.dep.ss-reg.dep.db
sum(reg.dep.ss-reg.dep.db)

plot(int.arr.ss, type = "l", col="red")
lines(int.arr.db, col="blue")
# Valutazione errori
int.arr.ss-int.arr.db
sum(int.arr.ss-int.arr.db)

plot(int.dep.ss, type = "l", col="red")
lines(int.dep.db, col="blue")
# Valutazione errori
int.dep.ss-int.dep.db
sum(int.dep.ss-int.dep.db)

plot(dom.arr.ss, type = "l", col="red")
lines(dom.arr.db, col="blue")
# Valutazione errori
dom.arr.ss-dom.arr.db
sum(dom.arr.ss-dom.arr.db)


plot(dom.dep.ss, type = "l", col="red")
lines(dom.dep.db, col="blue")
# Valutazione errori
dom.dep.ss-dom.dep.db
sum(dom.dep.ss-dom.dep.db)

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
int.dep.db.transit <- ts(int.dep.aflight.transit$CONTO, frequency=12, start=2012+3/12)
int.dep.db.transit
int.dep.db2 <- int.dep.db+int.dep.db.transit
plot(int.dep.ss, type = "l", col="red")
lines(int.dep.db2, col="blue")
sum(int.dep.ss-int.dep.db2)
int.dep.ss-int.dep.db2

int.arr.aflight.transit<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                                   FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                                   WHERE A_FLIGHT.ID in
                                   (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                                   FROM [AMS516].[dbo].[A_MOVEMENT] 
                                   INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                                   and S_ROUTE.FLIGHT_TYPE_ID=2
                                   and A_FLIGHT.STO between '20120401' and '20171231'
                                   and A_FLIGHT.QUALIFIER_ID=5
                                   and A_FLIGHT.EXCEPTION_ID is null
                                   GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                                   order by ANNO, MESE;")

int.arr.db.transit <- ts(int.arr.aflight.transit$CONTO, frequency=12, start=2012+3/12)
int.arr.db2 <- int.arr.db+int.arr.db.transit
plot(int.arr.ss, type = "l", col="red")
lines(int.arr.db2, col="blue")
sum(int.arr.ss-int.arr.db2)

int.arr.ss-int.arr.db2
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
dom.dep.db.transit <- ts(dom.dep.aflight.transit$CONTO, frequency=12, start=2012+3/12)
dom.dep.db2 <- dom.dep.db+dom.dep.db.transit
plot(dom.dep.ss, type = "l", col="red")
lines(dom.dep.db2, col="blue")
sum(dom.dep.ss-dom.dep.db2)
dom.dep.ss-dom.dep.db2

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
dom.arr.db.transit <- ts(dom.arr.aflight.transit$CONTO, frequency=12, start=2012+3/12)
dom.arr.db2 <- dom.arr.db+dom.arr.db.transit
plot(dom.arr.ss, type = "l", col="red")
lines(dom.arr.db2, col="blue")
sum(dom.arr.ss-dom.arr.db2)
dom.arr.ss-dom.arr.db2


# PASSAGGIO DATI PER ANALISI E MODELLI PREVISIONALI TS --------------------
# In questa sezione, i dati estratti vengono inviati a script dedicati per l'applicazione
# dei modelli decisionali.
# NOTA BENE: per il momento ci concentriamo su una sola componente.
source("serie_aeroporto.R")  #dom.arr 1
source("serie_aeroporto.R")  #dom.dep 2
source("serie_aeroporto.R")  #int.arr 3
source("serie_aeroporto.R")  #int.dep 4
source("serie_aeroporto.R")  #reg.arr 5
source("serie_aeroporto.R")  #reg.dep 6

source("serie_aeroporto_arima.R")  #dom.arr 1
source("serie_aeroporto_arima.R")  #dom.dep 2
source("serie_aeroporto_arima.R")  #int.arr 3
source("serie_aeroporto_arima.R")  #int.dep 4
source("serie_aeroporto_arima.R")  #reg.arr 5
source("serie_aeroporto_arima.R")  #reg.dep 6


