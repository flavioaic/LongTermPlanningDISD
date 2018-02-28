library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-32KTKVV;database=AMs516;trusted_connection=yes;")

# a_flight <- sqlQuery(dbhandle,"select * from A_FLIGHT")
# colnames(a_flight)
# a_flight2 <- a_flight[-which(a_flight$FLIGHT_TYPE_ID!=7),]
# a_flight2 <- a_flight2[-which(a_flight2$FLIGHT_TYPE_ID!=4),]
# rm(a_flight, a_flight2)

flights <- sqlQuery(dbhandle,"
                                    SELECT A_FLIGHT.ID, A_FLIGHT.COMPANY_ID, A_FLIGHT.ROUTE_ID, A_FLIGHT.FLIGHT_TYPE_ID, STO, S_ROUTE.DESCRIPTION1, A_MOVEMENT.AIRCRAFT_TYPE_ID, S_AIRCRAFT_TYPE.DESCRIPTION1, S_AIRCRAFT_TYPE.MTOW
                                    FROM A_FLIGHT inner join S_ROUTE on S_ROUTE.ID=A_FLIGHT.ROUTE_ID
                                    inner join A_MOVEMENT on A_MOVEMENT.ARR_FLIGHT_ID=A_FLIGHT.ID or A_MOVEMENT.DEP_FLIGHT_ID=A_FLIGHT.ID
                                    inner join S_AIRCRAFT_TYPE on S_AIRCRAFT_TYPE.ID=A_MOVEMENT.AIRCRAFT_TYPE_ID
                                    where A_FLIGHT.ID in (SELECT ARR_FLIGHT_ID FROM A_MOVEMENT) or A_FLIGHT.ID in (SELECT DEP_FLIGHT_ID FROM A_MOVEMENT)
                                    and A_FLIGHT.EXCEPTION_ID is null  ")

tabella_class <- flights[which(flights$FLIGHT_TYPE_ID!=7),]
tabella_class <- tabella_class[which(tabella_class$FLIGHT_TYPE_ID!=4),]

# GRAFICI  ----------------------------------------------------------------
library(dplyr)
# Facciamo un join tra la tabella A_FLIGHT e quella ricavata per le "rotte"(che ora include le distanze)
voli_rotte <- inner_join(tabella_class,rotte.agg2,by=c("ROUTE_ID"="ID"))

# Rappresentazione grafica n.1
# plot(voli_rotte$distanza,voli_rotte$MTOW)

# Rappresentazione grafica n.2: proviamo a diversificare per regional, international e domestic
library(tidyverse) # Serve per caricare ggplot
FLIGHT_TYPE <- sqlQuery(dbhandle,"SELECT * FROM S_FLIGHT_TYPE") # scarichiamo le categorie...
voli_rotte2 <- left_join(voli_rotte,FLIGHT_TYPE,by=c("FLIGHT_TYPE_ID.x"="ID"))
# voli_rotte2 <- inner_join(FLIGHT_TYPE, voli_rotte,by=c("ID"="FLIGHT_TYPE_ID.x"))
voli_rotte2 <- subset(voli_rotte2, select=-c(CHARGE, FIDS, DESCRIPTION2, DESCRIPTION3, DESCRIPTION4, DESCRIPTION5))#facciamo un po' di pulizia...

# Pulizia
rm(FLIGHT_TYPE, flights, tabella_class, voli_rotte)

