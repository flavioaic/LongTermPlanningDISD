# Questo script:
# 1- scarica il db delle rotte da e per JNB
# 2- converte le coordinate degli aeroporti dest/arr in decimali
# 3- recupera molte coordinate mancanti (ma non tutte)
# 4- calcola per ogni rotta la distanza da percorrere
# 5- esporta dei grafici, suddivisi per categorie (int,dom,reg), su destinazioni e provenienze
# N.B. Esistono alcune destinazioni che devono essere ricontrollate.
#
library(RODBC)
dbhandle <-odbcDriverConnect(connection="Driver={SQL Server};server=DESKTOP-32KTKVV;database=AMs516;trusted_connection=yes;")

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

###
#aeroporti2 <- read.csv("airports.csv", header = TRUE, stringsAsFactors=FALSE) #http://ourairports.com/data/airports.csv
#aeroporti2 <- as.data.frame(cbind(aeroporti2$iata_code, as.numeric(aeroporti2$latitude_deg), as.numeric(aeroporti2$longitude_deg)))
#colnames(aeroporti2) <-  c("CODE", "LAT2c", "LON2c")
#
#rotte.null.trovate <- merge(x = rotte.null, y = aeroporti2, by = "CODE", all.x = TRUE)
#rotte.null.trovate <- rotte.null.trovate[-which(is.na(rotte.null.trovate$LAT2c)),] # contiene solo quelli trovati (non NA)
#
#rotte.null <- setdiff(rotte.null,rotte.null.trovate[,c(1:14)]) # nuovo dataset dei rimanenti null (ho ridotto le due colonne)
###


# Consideriamo poi le coordinate dei luoghi che non hanno coordinate e correggiamole tramite ggmap/geocode (Google)
#
library(ggmap) # carico ggmap per chiedere a Google le coordinate mancanti dei luoghi (http://stat405.had.co.nz/ggmap.pdf)
# lancio una query su google sui nomi degli aeroporti indicati in DESCRIPTION1, con l'aggiunta di 'airport' per raffinare la ricerca
# 
temp1          <- geocode(paste(rotte.null$DESCRIPTION1, "airport", sep = " "))#occhio che lat e lon sono invertiti
temp2          <- geocode(paste(rotte.null$CODE, "airport", sep = " ")) # faccio la stessa cosa cercando i CODE
temp <- temp1
temp[which(is.na(temp1$lon)),]<-temp2[which(is.na(temp1$lon)),] # sostituisci agli na di temp1 i valori di temp2
colnames(temp) <- c("LON2c", "LAT2c")
#write.csv(temp, file = "downloaded.csv")
pippo <- cbind(rotte.null, temp[,c(2,1)])        # aggiunto al db iniziale le due colonne con i nuovi geo points
rotte.null2 <- pippo[which(is.na(pippo$LAT2c)),] # valori ancora non trovati che rimandiamo al passo successivo!
rotte.null <- setdiff(pippo, rotte.null2)        # db dei valori trovati

rotte.na <- rbind(rotte.na, subset(rotte.null2, select=-c(LAT2c,LON2c)))
rm(pippo, rotte.null2) #Eliminiamo quello che non serve più...

# Da ultimo vediamo se riusciamo a recuperare le coordinate per il gruppo delle NA e dei 50 che non sono stati reperiti al punto precedente.

aeroporti <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = TRUE, stringsAsFactors=FALSE)
temp0 <- as.data.frame(cbind(aeroporti[,5], as.numeric(aeroporti[,7]), as.numeric(aeroporti[,8])))
colnames(temp0) <-  c("CODE", "LAT2c", "LON2c")

rotte.na <- merge(x = rotte.na, y = temp0, by = "CODE", all.x = TRUE)

rotte.agg <- rbind(rotte.not.null, rotte.null, rotte.na) # , rotte.null.trovate
 
rotte.agg$LAT1c <- as.numeric(rotte.agg$LAT1c)
rotte.agg$LON1c <- as.numeric(rotte.agg$LON1c)
rotte.agg$LAT2c <- as.numeric(rotte.agg$LAT2c)
rotte.agg$LON2c <- as.numeric(rotte.agg$LON2c) # DB SISTEMATO!

rm(aeroporti, rotte.na, rotte.not.null, rotte.null, temp0)

# CALCOLO delle LUNGHEZZE DELLE ROTTE
# library(geosphere)

rotte.agg2 <- rotte.agg[-which(is.na(rotte.agg$LAT2c)),] #45 ################

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


# IMPUTAZIONE E CORREZIONE ------------------------------------------------
# Voli domestici: elimino quei voli il cui recupero di coordinate non è andato a buon fine.
prova <- rotte.agg2
prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083)  
prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611) 
# Per i valori dei domestici troppo a est:
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083) 
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611) 
# Per i valori dei domestici troppo a ovest:
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083)
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611)
#
prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)# Abbiamo assegnato 
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)
rotte.agg2 <- prova 
rm(prova)
# Voli regionali: elimino quei voli il cui recupero di coordinate non è andato a buon fine.
prova <- rotte.agg2
# Per i valori dei regionali troppo a ovest:
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LAT2c")] <- c(-27.86083)
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LON2c")] <- c(28.24611)
prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("distanza")] <- c(NA)
# Per i valori dei regionali troppo a est:
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LAT2c")] <- c(-27.86083)
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LON2c")] <- c(28.24611)
prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("distanza")] <- c(NA)
rotte.agg2 <- prova 
rm(prova)

# CORREZIONE DISTANZA PER "HANGAR MOVEMENT" (Flight type 4) ---------------
message('Il valore di distanza per la classe di rotte HANGAR MOVEMENT erroneamente valutato pari a ', 
        rotte.agg2[grep(4, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza, 
        ' ora è assegnato a 0 ')
rotte.agg2[grep(4, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza <- c(NA)
# 
# CORREZIONE DISTANZA PER "UNKOWN" (Flight type 7) ------------------------
message('Il valore di distanza per la classe di rotte UNKNOWN erroneamente valutato pari a ', 
        rotte.agg2[grep(7, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza, 
        ' ora è assegnato a 0 ')
rotte.agg2[grep(7, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza <- c(NA)

# GRAFICI SU MAPPAMONDO ---------------------------------------------------
cartella <- c('PICT')
ifelse(!dir.exists(file.path(getwd(), cartella)), dir.create(file.path(getwd(), cartella)), FALSE) 

#plot(rotte.agg2$distanza, rotte.agg2$FLIGHT_TYPE_ID, main='Route Distance v.s. Flight Type')

#colnames(head(left_join(rotte.agg2, sqlQuery(dbhandle, "SELECT S_FLIGHT_TYPE.ID, S_FLIGHT_TYPE.DESCRIPTION1 FROM [AMS516].[dbo].S_FLIGHT_TYPE"), by=c("FLIGHT_TYPE_ID"="ID"))))

#Eliminiamo le coordinate non affidabili per realizzare i grafici
rotte.agg3 <- rotte.agg2[-grep("UNKNOWN INTERNATIONAL", rotte.agg2$DESCRIPTION1),] # elimina rotte UNKNOWN INTERNATIONAL
rotte.agg3 <- rotte.agg3[-grep("UNKNOWN REGIONAL", rotte.agg3$DESCRIPTION1),]      # elimina rotte UNKNOWN REGIONAL
rotte.agg3 <- rotte.agg3[-grep("UNKNOWN DOMESTIC", rotte.agg3$DESCRIPTION1),]      # elimina rotte UNKNOWN DOMESTIC

# Grafico 1: tutte le destinazioni/partenze
map_world <- map_data("world")
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3, aes(x = LON2c, y = LAT2c), color = 'red') 
dev.print(pdf, './PICT/mappa_citta_raggiungibili.pdf')

# Grafico 2: tutte le destinazioni/partenze  
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3[which(rotte.agg3$FLIGHT_TYPE_ID==1),], aes(x = LON2c, y = LAT2c), color = 'yellow') +
  ggtitle("Reachable Cities by means of Regional Flights (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/mappa_citta_raggiungibili_regionali.pdf')

# Grafico 3: tutte le destinazioni/partenze
ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = rotte.agg3[which(rotte.agg3$FLIGHT_TYPE_ID==2),], aes(x = LON2c, y = LAT2c), color = 'blue') +
  ggtitle("Reachable Cities by means of International Flights (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/mappa_citta_raggiungibili_internazionali.pdf')

# Grafico 4: tutte le destinazioni/partenze
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

write.csv(rotte.agg2, file = "rotte.agg2.csv") # flag per esportare i dati

# # CORREZIONE DISTANZE PER ROTTE "UNKOWN" (dom, reg, int) ------------------
# #
# # Seguono alcune correzioni dovute alla query eseguite tramite i serivizi di Google.
# #
# # Decidiamo di assegnare alle rotte UNKNOWN i rispettivi valori medi
# message('Il valore di distanza per UNKNOWN REGIONAL erroneamente valutato pari a ', 
#         rotte.agg2[grep('UNKNOWN REGIONAL', rotte.agg2$DESCRIPTION1), "distanza"], 
#         ' ora è assegnato al valore medio delle altre rotte di omologa classe ',
#         mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==1), DESCRIPTION1 != 'UNKNOWN REGIONAL')$distanza))
# rotte.agg2[grep('UNKNOWN REGIONAL', rotte.agg2$DESCRIPTION1), "distanza"] <- mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==1), DESCRIPTION1 != 'UNKNOWN REGIONAL')$distanza)
# #
# message('Il valore di distanza per UNKNOWN INTERNATIONAL erroneamente valutato pari a ', 
#         rotte.agg2[grep('UNKNOWN INTERNATIONAL', rotte.agg2$DESCRIPTION1), "distanza"], 
#         ' ora è assegnato al valore medio delle altre rotte di omologa classe ',
#         mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==2), DESCRIPTION1 != 'UNKNOWN INTERNATIONAL')$distanza))
# rotte.agg2[grep('UNKNOWN INTERNATIONAL', rotte.agg2$DESCRIPTION1), "distanza"] <- mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==2), DESCRIPTION1 != 'UNKNOWN INTERNATIONAL')$distanza)
# #
# message('Il valore di distanza per UNKNOWN INTERNATIONAL erroneamente valutato pari a ', 
#         rotte.agg2[grep('UNKNOWN DOMESTIC', rotte.agg2$DESCRIPTION1), "distanza"], 
#         ' ora è assegnato al valore medio delle altre rotte di omologa classe ',
#         mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==3), DESCRIPTION1 != 'UNKNOWN DOMESTIC')$distanza))
# rotte.agg2[grep('UNKNOWN DOMESTIC', rotte.agg2$DESCRIPTION1), "distanza"] <- mean(subset(subset(rotte.agg2, FLIGHT_TYPE_ID==3), DESCRIPTION1 != 'UNKNOWN DOMESTIC')$distanza)
# 
# 
# Grafico 5: distanze per categoria
ggplot(data=left_join(rotte.agg2, sqlQuery(dbhandle, "SELECT S_FLIGHT_TYPE.ID, S_FLIGHT_TYPE.DESCRIPTION1 FROM [AMS516].[dbo].S_FLIGHT_TYPE"), by=c("FLIGHT_TYPE_ID"="ID")), 
       aes(x=DESCRIPTION1.y, y=distanza)) + 
  geom_point() +
  labs(y="Distance", x="") +
  ggtitle("Routes Distances by Categories defined as Flight Type (Routes DB)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.print(pdf, './PICT/distanza_categorie_db_rotte.pdf')

dev.off() # rimuovi tutti i grafici

