# Questo script:
# 1- scarica il db delle rotte da e per JNB
# 2- converte le coordinate degli aeroporti dest/arr in decimali
# 3- recupera molte coordinate mancanti (ma non tutte)
# 4- calcola per ogni rotta la distanza da percorrere
# 5- esporta dei grafici, suddivisi per categorie (int,dom,reg), su destinazioni e provenienze
# N.B. Esistono alcune destinazioni che devono essere ricontrollate.
#
# 1 - SCARICA IL DB DELLE ROTTE -------------------------------------------
library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-32KTKVV;database=AMs516;trusted_connection=yes;")
rotte <- sqlQuery(dbhandle,"
 SELECT S_ROUTE.ID
  ,S_ROUTE.HOME_AIRPORT_ID
  ,PAR.LATTITUDE as LAT1
  ,PAR.LONGITUDE as LON1
  ,PAR.AEROPORTO_DESCR as PAESE1
  ,S_ROUTE.CODE
  ,S_ROUTE.FLIGHT_TYPE_ID
  ,S_ROUTE.FLIGHT_NATURE_ID
  ,S_ROUTE.PORT_AIRPORT_ID
  ,ARR.LATTITUDE as LAT2
  ,ARR.LONGITUDE as LON2
  ,ARR.AEROPORTO_DESCR as PAESE2
  ,ARR.IATA as IATA2
  ,S_ROUTE.DESCRIPTION1 as DESCRIZIONE_ROTTA
  ,S_ROUTE.FLYING_TIME
 FROM [AMS516].[dbo].[S_ROUTE] 
 join (SELECT S_AIRPORT.ID
        ,S_AIRPORT.IATA
        ,S_AIRPORT.ICAO
        ,S_AIRPORT.LATTITUDE
        ,S_AIRPORT.LONGITUDE
        ,S_AIRPORT.AIRPORT_TYPE_ID
        ,S_AIRPORT.CITY_NAME1
        ,S_COUNTRY.DESCRIPTION1 as AEROPORTO_DESCR
        ,S_COUNTRY.CODE_3 as AEROPORTO_CODICE
      FROM S_AIRPORT join S_COUNTRY on S_AIRPORT.COUNTRY_ID=S_COUNTRY.ID) 
 as ARR  on ARR.ID=S_ROUTE.PORT_AIRPORT_ID
 join (SELECT S_AIRPORT.ID
        ,S_AIRPORT.IATA
        ,S_AIRPORT.ICAO
        ,S_AIRPORT.LATTITUDE
        ,S_AIRPORT.LONGITUDE
        ,S_AIRPORT.AIRPORT_TYPE_ID
        ,S_AIRPORT.CITY_NAME1
        ,S_COUNTRY.DESCRIPTION1 as AEROPORTO_DESCR
        ,S_COUNTRY.CODE_3 as AEROPORTO_CODICE
       FROM S_AIRPORT join S_COUNTRY on S_AIRPORT.COUNTRY_ID=S_COUNTRY.ID) 
 as PAR on PAR.ID=S_ROUTE.HOME_AIRPORT_ID")




library(tidyverse)
# i dati importati contengono colonne corrispondenti a longitudine e latitudine che devono essere convertiti in decimali

library(sp) # per convertire i gradi gps a decimali
# Convertiamo prima l'aeroporto di provenienza (JNB), creando due nuove colonne (LAT1c,LON1c)
rotte <- mutate(rotte, 
  LAT1c=LAT1 %>%
  sub(':', 'd', .) %>%
  sub(':', '\'', .) %>%
  sub(':', '" ', .) %>%
  char2dms %>%
  as.numeric,
  LON1c=LON1 %>%
  sub(':', 'd', .) %>%
  sub(':', '\'', .) %>%
  sub(':', '" ', .) %>%
  char2dms %>%
  as.numeric)


# 2 - VALUTA I DATI PRESENTI E QUELLI MANCANTI ----------------------------
# Il db delle rotte contiene rispetto alle coordinate geografiche tre tipi di rotte:
# 1-rotte con geo coord indicate
# 2-rotte con geo coord tipo 0:0:0
# 3-rotte con geo coord tipo NA (cioè vuoto) -> queste mi hanno combinato un casino!
#
# Suddividiamo quindi il db nei gruppi indicati sopra...
#
# creiamo un dataframe contenente le destinazioni per cui sono riportate le coordinate geografiche
rotte.null      <- rotte[grep("0:0:0", rotte$LAT2),] # Totale: 122 nulli
rotte.na        <- rotte[which(is.na(rotte$LAT2)),]  # Totale:   2 nulli
rotte.not.null  <- rotte[-grep("0:0:0", rotte$LAT2),]# Totale: 623 nulli (provvisorio)
rotte.not.null  <- setdiff(rotte.not.null,rotte.na)  # Totale: 621 nulli (fa la differenza dei due dataset in argomento)
                                                     # Somma:  122+2+621=745 (di cui 3 unknown)
## Tanto per fare una verifica (ma comunque il numero di righe dei tre gruppi di sopra fa capire che ho coperto tutti i casi...)
# rotte.null2 <- rotte[grep("0:0:0", rotte$LON2),]
# setdiff(rotte.null, rotte.null2) # questo comando mi fa capire che rotte.null e rotte.null2 sono identici: ne elimino uno.
# rm(rotte.null2)                  # (vedi commento rigo superiore)

# Sistemiamo prima i dati che contengono coordinate valide e le convertiamo in decimali
rotte.not.null <- mutate(rotte.not.null, 
  LAT2c=LAT2 %>%
  sub(':', 'd', .) %>%
  sub(':', '\'', .) %>%
  sub(':', '" ', .) %>%
  char2dms %>%
  as.numeric,
  LON2c=LON2 %>%
  sub(':', 'd', .) %>%
  sub(':', '\'', .) %>%
  sub(':', '" ', .) %>%
  char2dms %>%
  as.numeric)       

# 2.1 USO UN DB RECUPERATO ONLINE -----------------------------------------
# Da ultimo vediamo se riusciamo a recuperare le coordinate per il gruppo delle NA e dei 50 che non sono stati reperiti al punto precedente.
if(file.exists("downloaded_airports.csv")){
  temp0 <- read.csv("downloaded_airports.csv",stringsAsFactors=F, header = TRUE) # carica downloaded
  temp0 <- temp0[,2:4] # perché devo eliminare la prima colonna a causa dell'importazione da csv
  }else {
  aeroporti <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = TRUE, stringsAsFactors=FALSE)
  temp0 <- as.data.frame(cbind(aeroporti[,5], as.numeric(aeroporti[,7]), as.numeric(aeroporti[,8])))
  colnames(temp0) <-  c("CODE", "LAT2c", "LON2c")
  write.csv(temp0, file = "downloaded_airports.csv")
  }

rotte.null <- left_join(rotte.null, temp0, by=c("IATA2"="CODE")) # incrocio sul codice IATA e vedo che trovo 28 coordinate
rotte.not.null <- rbind(rotte.not.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i non nulli
rotte.null <- setdiff(rotte.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i rimanenti nulli

# Al termine di questa procedura, ho recuperato 649-621= 28 rotte.

# 2.2 CORREGGO TRAMITE DATI GIà NEL DB ------------------------------------
# Adesso facciamo un join tra quanto presente nella rotte.not.null e rotte.null
incrocio.db <- inner_join(rotte.null, rotte.not.null, by=c("DESCRIZIONE_ROTTA"))[,c("ID.x","DESCRIZIONE_ROTTA", "FLIGHT_TYPE_ID.x", "FLIGHT_TYPE_ID.y", "PAESE2.x", "PAESE2.y", "LAT2c.y", "LON2c.y")]
# Si consiglia di visualizzare la tabella di cui sopra. Infatti i FLIGHT_TYPE_ID sono gli stessi: 
# si può concludere che è sbagliata la PAESE2.x

# Assegniamo i valori di incrocio.db a rotte.null
for (i in incrocio.db$ID.x) {
  rotte.null[which(rotte.null$ID==i),c("LAT2c")] <- incrocio.db[which(incrocio.db$ID.x==i),c("LAT2c.y")]
  rotte.null[which(rotte.null$ID==i),c("LON2c")] <- incrocio.db[which(incrocio.db$ID.x==i),c("LON2c.y")]
}

rotte.not.null <- rbind(rotte.not.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i non nulli
rotte.null <- setdiff(rotte.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i rimanenti nulli

rm(incrocio.db)

# Al termine di questa procedura, ho recuperato 669-649= 20 rotte.


# 2.2 CORREGGO TRAMITE DATI SU WIKIPEDIA---------------------------------
library(plotKML) # per importare file KML e GPX
# Scarica il file dei soli aeroporti in South Africa ricavato da: https://en.wikipedia.org/wiki/List_of_airports_in_South_Africa
if(file.exists("aeroporti.gpx")){
  south.africa.gpx <- readGPX("aeroporti.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)
  }else {
  download.file("http://tripgang.com/kml2gpx/http%3A%2F%2Ftools.wmflabs.org%2Fkmlexport%3Farticle%3DList_of_airports_in_South_Africa?gpx=1", destfile="aeroporti.gpx")
  south.africa.gpx <- readGPX("aeroporti.gpx", metadata = TRUE, bounds = TRUE, waypoints = TRUE, tracks = TRUE, routes = TRUE)
  }

south.africa.gpx.wp <- south.africa.gpx$waypoints # ci interessa solo un dataframe
rm(south.africa.gpx)

south.africa.gpx.wp$name <- gsub(" Airport", "\\1", south.africa.gpx.wp$name) # rimuovi la stringa non utile dalla colonna
south.africa.gpx.wp <- subset(south.africa.gpx.wp,select=c(lat, lon, name))   # riduci alle colonne utili
south.africa.gpx.wp$name <- toupper(south.africa.gpx.wp$name)                  # rendi maiuscola la colonna
names(south.africa.gpx.wp)[names(south.africa.gpx.wp)=="name"] <- "DESCRIZIONE_ROTTA"


# A noi interessano solo le rotte in South Africa, date da:
rotte.null.sa <- rotte.null[grepl("SOUTH AFRICA", rotte.null$PAESE2),]

incrocio.db2 <- inner_join(rotte.null.sa, south.africa.gpx.wp, by=c("DESCRIZIONE_ROTTA"))[,c("ID","DESCRIZIONE_ROTTA", "lat", "lon")]


# Assegniamo i valori di incrocio.db2 a rotte.null
for (i in incrocio.db2$ID) {
  rotte.null[which(rotte.null$ID==i),c("LAT2c")] <- incrocio.db2[which(incrocio.db2$ID==i),c("lat")]
  rotte.null[which(rotte.null$ID==i),c("LON2c")] <- incrocio.db2[which(incrocio.db2$ID==i),c("lon")]
}

rotte.not.null <- rbind(rotte.not.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i non nulli
rotte.null <- setdiff(rotte.null, rotte.null[-which(is.na(rotte.null$LAT2c)),]) # aggiorno i rimanenti nulli

rm(incrocio.db2)
rm(south.africa.gpx.wp)

# Al termine di questa procedura, ho recuperato 690-669= 21 rotte.


# 2.3.1 CORREZIONE A MANO -------------------------------------------------
# Correzione na:
rotte.na["LAT2c"] <- NA # Aggiungo colonna
rotte.na["LON2c"] <- NA # Aggiungo colonna
# https://tools.wmflabs.org/geohack/geohack.php?pagename=Lobatse_Airport&params=25_11_50_S_25_42_52_E_type:airport_region:BW
rotte.na[which(rotte.na$DESCRIZIONE_ROTTA=="Lobatse"),c("LAT2c", "LON2c")] <- c(-25.197222,25.714444)
# http://www.prokerala.com/travel/airports/south-africa/mzamba-airport.html
rotte.na[which(rotte.na$DESCRIZIONE_ROTTA=="MZAMBA"),c("LAT2c", "LON2c")] <- c(-30.85,29.7667)

# http://www.pilotnav.com/airport/FACC
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="ARATHUSA"),c("LAT2c", "LON2c")] <- c(-24.7442,31.5225)
# http://www.pilotnav.com/airport/FABP
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="BLACK ROCK"),c("LAT2c", "LON2c")] <- c(-27.1297, 22.8463)
# http://www.pilotnav.com/airport/FABS
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="BRITS"),c("LAT2c", "LON2c")] <- c(-25.5323, 27.7759)
# http://www.pilotnav.com/airport/FABD 
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="BURGERSDORP"),c("LAT2c", "LON2c")] <- c(-30.9775, 26.3081)
# http://www.pilotnav.com/airport/FADD
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="DUNDEE"),c("LAT2c", "LON2c")] <- c(-28.1831,30.2225)
# http://www.pilotnav.com/airport/FAJP
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="JOUBERTINA"),c("LAT2c", "LON2c")] <- c(-33.8312,23.8301)
# http://www.pilotnav.com/airport/FAKT
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="KITTY HAWK"),c("LAT2c", "LON2c")] <- c(-25.8600,28.4500)
# http://www.pilotnav.com/airport/FAGW
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="MAGWA"),c("LAT2c", "LON2c")] <- c(-31.3939,29.6935)
# http://www.pilotnav.com/airport/MWR
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="MOTSWARI"),c("LAT2c", "LON2c")] <- c(-24.1903,31.3864)
# http://www.pilotnav.com/airport/FANY
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="NYLSTROOM"),c("LAT2c", "LON2c")] <- c(-24.6861,28.4349)
# http://www.pilotnav.com/airport/OVG
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="OVERBERG"),c("LAT2c", "LON2c")] <- c(-34.5549,20.2507)
# http://www.pilotnav.com/airport/FAPC
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="PRINS ALBERT"),c("LAT2c", "LON2c")] <- c(-33.2025,22.0322)
# http://www.pilotnav.com/airport/FASK
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="SWARTKOP"),c("LAT2c", "LON2c")] <- c(-25.8097, 28.1646)
# http://www.pilotnav.com/airport/FATW
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="TSWALO"),c("LAT2c", "LON2c")] <- c(-27.2050, 22.4819)
# http://www.pilotnav.com/airport/airport-61042
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="PAFURI"),c("LAT2c", "LON2c")] <- c(-22.4151, 31.2130)
# https://airportguide.com/airport/info/FAGR
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="GRAAF REINET"),c("LAT2c", "LON2c")] <- c(-32.1936, 24.541401)
# https://tools.wmflabs.org/geohack/geohack.php?pagename=Air_Force_Base_Swartkop&params=25_48_25_S_28_09_52_E_region:ZA-GP_type:airport
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="SWARTKOP"),c("LAT2c", "LON2c")] <- c(-25.806944, 28.164444)
# http://tools.wmflabs.org/geohack/geohack.php?language=it&pagename=Aeroporto_di_Sant%27Elena&params=-15.959056_N_-5.645917_E_type:airport
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="SAINT HELENA"),c("LAT2c", "LON2c")] <- c(-15.959056, -5.645917)
#
# http://airportsbase.org/South_Africa/all/Londolozi/Londolozi 
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="LONDOLOZI (Redundant correct LDZ)"),c("LAT2c", "LON2c")] <- c(-24.8, 31.5)
# https://skyvector.com/airport/FAMU/Mkuzi-Airport
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="MKUZI"),c("LAT2c", "LON2c")] <- c(-27.633, 32.05)
# https://airportguide.com/airport/info/FAMX
rotte.null[which(rotte.null$DESCRIZIONE_ROTTA=="MBAZWANE"),c("LAT2c", "LON2c")] <- c(-27.481400,32.593899)



# 2.4 FINALIZZAZIONE DELLE CORREZIONI -------------------------------------
rotte.agg <-rbind(rotte.not.null, rotte.na, rotte.null) # 

rotte.agg$LAT1c <- as.numeric(rotte.agg$LAT1c)
rotte.agg$LON1c <- as.numeric(rotte.agg$LON1c)
rotte.agg$LAT2c <- as.numeric(rotte.agg$LAT2c)
rotte.agg$LON2c <- as.numeric(rotte.agg$LON2c) # DB SISTEMATO!

rm(aeroporti, rotte.na, rotte.not.null, rotte.null, temp0, temp)


# 3 - CALCOLO delle LUNGHEZZE DELLE ROTTE ---------------------------------
# 
# library(geosphere)
rotte.agg2 <- rotte.agg[-which(is.na(rotte.agg$LAT2c)),] # elimino gli NA...

deg2rad <- function(deg) {(deg * pi) / (180)} # convertire i gradi per funzioni trigonometriche usate sotto

#for(i in 1:nrow(rotte.agg4) ){
#  caio <- distGeo(c(rotte.agg4$LAT1c[i], rotte.agg4$LON1c[i]), c(rotte.agg4$LAT2c[i], rotte.agg4$LON2c[i]))
#  print(i)
#  print(caio)
#} # restituisce distanza in metri... per FCO vs JNB la distanza è corretta!

distanza <- numeric(nrow(rotte.agg2))

for(i in 1:nrow(rotte.agg2) ){
  deltaL=deg2rad(rotte.agg2$LON2c[i]-rotte.agg2$LON1c[i])
  fi1=deg2rad(rotte.agg2$LAT1c[i])
  fi2=deg2rad(rotte.agg2$LAT2c[i])
  cos(deltaL)
  A=(cos(fi2)*sin(deltaL))**2
  B=(cos(fi1)*sin(fi2)-sin(fi1)*cos(fi2)*cos(deltaL))**2
  C=sin(fi1)*sin(fi2)+cos(fi1)*cos(fi2)*cos(deltaL)
  DeltaSigma=atan2(sqrt(A+B),C)
  Raggio=6371
  lunghezza=Raggio*DeltaSigma
  print(lunghezza)
  rotte.agg2[i,"distanza"] <- lunghezza
} # restituisce distanza in km ... formula da https://en.wikipedia.org/wiki/Great-circle_distance#Computational_formulas
  

rm(lunghezza)


# 5 - GRAFICI SU MAPPAMONDO -----------------------------------------------
# Questa sezione rappresenta sul mappamondo le coordinate trovate. 
# I grafici vengono salvati nella cartella delle immagini (PICT)
cartella <- c('PICT')
ifelse(!dir.exists(file.path(getwd(), cartella)), dir.create(file.path(getwd(), cartella)), FALSE) 

#plot(rotte.agg2$distanza, rotte.agg2$FLIGHT_TYPE_ID, main='Route Distance v.s. Flight Type')

#colnames(head(left_join(rotte.agg2, sqlQuery(dbhandle, "SELECT S_FLIGHT_TYPE.ID, S_FLIGHT_TYPE.DESCRIPTION1 FROM [AMS516].[dbo].S_FLIGHT_TYPE"), by=c("FLIGHT_TYPE_ID"="ID"))))

#Eliminiamo le coordinate non affidabili per realizzare i grafici
rotte.agg3 <- rotte.agg2[!grepl("UNKNOWN INTERNATIONAL", rotte.agg2$DESCRIZIONE_ROTTA),] # elimina rotte UNKNOWN INTERNATIONAL
rotte.agg3 <- rotte.agg3[!grepl("UNKNOWN REGIONAL", rotte.agg3$DESCRIZIONE_ROTTA),]      # elimina rotte UNKNOWN REGIONAL
rotte.agg3 <- rotte.agg3[!grepl("UNKNOWN DOMESTIC", rotte.agg3$DESCRIZIONE_ROTTA),]      # elimina rotte UNKNOWN DOMESTIC

# Grafico 1: tutte le destinazioni/partenze
map_world <- map_data("world")
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3, aes(x = LON2c, y = LAT2c), color = 'red') 
dev.print(pdf, './PICT/mappa_citta_raggiungibili.pdf')

# Grafico 2: regional  
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3[which(rotte.agg3$FLIGHT_TYPE_ID==1),], aes(x = LON2c, y = LAT2c), color = 'yellow') +
  ggtitle("Reachable Cities by means of Regional Flights (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/mappa_citta_raggiungibili_regionali.pdf')

# Grafico 3: international
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3[which(rotte.agg3$FLIGHT_TYPE_ID==2),], aes(x = LON2c, y = LAT2c), color = 'blue') +
  ggtitle("Reachable Cities by means of International Flights (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/mappa_citta_raggiungibili_internazionali.pdf')

# Grafico 4: domestic
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3[which(rotte.agg3$FLIGHT_TYPE_ID==3),], aes(x = LON2c, y = LAT2c), color = 'green') +
  ggtitle("Reachable Cities by means of Domestic Flights (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/mappa_citta_raggiungibili_domestici.pdf')



#Nota bene: esistono rotte da verificare (come le UNKNOWNs, Buenos Aires che ha un doppione etc.)
#
# Pulizia
rm(map_world, rotte, rotte.agg, A, B, C, deltaL,DeltaSigma, fi1, fi2, i, Raggio, cartella, rotte.agg3)

write.csv(rotte.agg2, file = "rotte.agg2.csv") # Salvataggio dati su file csv



# Grafico 5: distanze per categoria
ggplot(data=left_join(rotte.agg2, sqlQuery(dbhandle, "SELECT S_FLIGHT_TYPE.ID, S_FLIGHT_TYPE.DESCRIPTION1 FROM [AMS516].[dbo].S_FLIGHT_TYPE"), by=c("FLIGHT_TYPE_ID"="ID")), 
       aes(x=DESCRIPTION1, y=distanza)) + 
  geom_point() +
  labs(y="Distance", x="") +
  ggtitle("Routes Distances by Categories defined as Flight Type (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/distanza_categorie_db_rotte.pdf')

dev.off() # rimuovi tutti i grafici

