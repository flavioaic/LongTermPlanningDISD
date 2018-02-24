# Questo script:
# 1- scarica il db delle rotte da e per JNB
# 2- converte le coordinate degli aeroporti dest/arr in decimali
# 3- recupera molte coordinate mancanti (ma non tutte)
# 4- calcola per ogni rotta la distanza da percorrere
# N.B. Esistono alcune destinazioni che devono essere ricontrollate.
#
library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-HH30FVF;database=AMs516;trusted_connection=yes;")
getwd()

rotte <- sqlQuery(dbhandle, 
"SELECT S_ROUTE.ID
      ,S_ROUTE.HOME_AIRPORT_ID
	  ,PAR.LATTITUDE as LAT1
	  ,PAR.LONGITUDE as LON1
      ,S_ROUTE.CODE
      ,S_ROUTE.FLIGHT_TYPE_ID
      ,S_ROUTE.FLIGHT_NATURE_ID
      ,S_ROUTE.PORT_AIRPORT_ID
	  ,ARR.LATTITUDE as LAT2
	  ,ARR.LONGITUDE as LON2
      ,S_ROUTE.DESCRIPTION1 
      ,S_ROUTE.FLYING_TIME
  FROM [AMS516].[dbo].[S_ROUTE] 
  join S_AIRPORT as ARR  on ARR.ID=S_ROUTE.PORT_AIRPORT_ID
  join S_AIRPORT as PAR on PAR.ID=S_ROUTE.HOME_AIRPORT_ID")

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

# Il db delle rotte contiene rispetto alle coordinate geografiche tre tipi di rotte:
# 1-rotte con geo coord indicate
# 2-rotte con geo coord tipo 0:0:0
# 3-rotte con geo coord tipo NA (cioè vuoto) -> queste mi hanno combinato un casino!
#
# Suddividiamo quindi il db nei gruppi indicati sopra...
#
# creiamo un dataframe contenente le destinazioni per cui sono riportate le coordinate geografiche
rotte.null      <- rotte[grep("0:0:0", rotte$LAT2),]
rotte.na        <- rotte[which(is.na(rotte$LAT2)),]
rotte.not.null  <- rotte[-grep("0:0:0", rotte$LAT2),]
rotte.not.null  <- setdiff(rotte.not.null,rotte.na) # fa la differenza dei due dataset in argomento

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

# Consideriamo poi le coordinate dei luoghi che non hanno coordinate (Buenos Aires etc...) e correggiamole tramite ggmap/geocode (Google)

library(ggmap) # carico ggmap per chiedere a Google le coordinate mancanti dei luoghi (http://stat405.had.co.nz/ggmap.pdf)
# lancio una query su google sui nomi degli aeroporti indicati in DESCRIPTION1, con l'aggiunta di 'airport' per raffinare la ricerca
temp           <- geocode(paste(rotte.null$DESCRIPTION1, "airport", sep = " ")) #occhio che lat e lon sono invertiti
colnames(temp) <- c("LON2c", "LAT2c")
pippo <- cbind(rotte.null, temp[,c(2,1)])        # aggiunto al db iniziale le due colonne con i nuovi geo points
rotte.null2 <- pippo[which(is.na(pippo$LAT2c)),] # valori ancora non trovati che rimandiamo al passo successivo!
rotte.null <- setdiff(pippo, rotte.null2)        # db dei valori trovati

rotte.na <- rbind(rotte.na, subset(rotte.null2, select=-c(LAT2c,LON2c)))
rm(temp, pippo, rotte.null2) #Eliminiamo quello che non serve più...

# Da ultimo vediamo se riusciamo a recuperare le coordinate per il gruppo delle NA e dei 50 che non sono stati reperiti al punto precedente.

aeroporti <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = FALSE, stringsAsFactors=FALSE)
temp2 <- as.data.frame(cbind(aeroporti[,5], as.numeric(aeroporti[,7]), as.numeric(aeroporti[,8])))
colnames(temp2) <-  c("CODE", "LAT2c", "LON2c")

rotte.na <- merge(x = rotte.na, y = temp2, by = "CODE", all.x = TRUE)

rotte.agg <- rbind(rotte.not.null, rotte.null, rotte.na) 

rotte.agg$LAT1c <- as.numeric(rotte.agg$LAT1c)
rotte.agg$LON1c <- as.numeric(rotte.agg$LON1c)
rotte.agg$LAT2c <- as.numeric(rotte.agg$LAT2c)
rotte.agg$LON2c <- as.numeric(rotte.agg$LON2c) # DB SISTEMATO!

rm(aeroporti, rotte.na, rotte.not.null, rotte.null, temp2)

# CALCOLO delle LUNGHEZZE DELLE ROTTE
# library(geosphere)

rotte.agg2 <- rotte.agg[-which(is.na(rotte.agg$LAT2c)),] #45

#rm(rotte.agg2, rotte.agg3)

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
  distanza[i]<-lunghezza
} # restituisce distanza in km ... formula da https://en.wikipedia.org/wiki/Great-circle_distance#Computational_formulas
  
rotte.agg2 <- cbind(rotte.agg2,distanza)

rm(lunghezza, distanza)

# mario <- subset(rotte.not.null, select=c("LAT2"))
# 
# mario[3,1]
# lat.conv(as.character(mario[3,1]))
# as.character(mario[3,1])
# class(as.character(mario[3,1]))
# 
# for(i in 1:nrow(mario) ){
#   lat.conv(as.character(mario[i,1]))
#   print(lat.conv(as.character(mario[i,1])))
#   mario[i,2]<-lat.conv(as.character(mario[i,1]))
# }
# 
# getAnywhere(char2dms)

#plot(rotte.agg2$distanza, rotte.agg2$FLIGHT_TYPE_ID)

# map_world <- map_data("world")
# ggplot() +
#   geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
#   geom_point(data = rotte.agg2, aes(x = LON2c, y = LAT2c), color = 'red')

#Nota bene: esistono rotte da verificare (come le UNKNOWNs, Buenos Aires che ha un doppione etc.)



