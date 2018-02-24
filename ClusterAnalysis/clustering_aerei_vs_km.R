# https://home.deib.polimi.it/matteucc/Clustering/tutorial_html/
#
#
# DATA DISCOVERY
# 1 - Valutazione di inefficacia classificazione db (dom, naz, int) --------
# Vogliamo dimostrare che la classificazione naurale dei voli non va bene rispetto alla distanza percorsa.
# Facciamo dei garfici dei valori medi MTOW
average <- data.frame(cbind(
  c(mean(voli_rotte2[grep("1", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW), 
    mean(voli_rotte2[grep("2", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW),
    mean(voli_rotte2[grep("3", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW)),
  c(mean(na.omit((voli_rotte2[grep("1", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))),
    mean(na.omit((voli_rotte2[grep("2", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))),
    mean(na.omit((voli_rotte2[grep("3", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))))),
  row.names = c("Regional","International","Domestic"))
colnames(average) <- c("MTOW","Distance")

# # In media, il MTOW tra regionali e domestici è molto simile:
# barplot(average$MTOW, main="Average MTOW [kg]", 
#         names.arg=rownames(average))
# # In media, la distanza percorsa tra regionali e domestici è la stessa:
# barplot(average$Distance, main="Average Distance [km]", 
#         names.arg=rownames(average))

rm(average) # ripuliamo

# 2 - Facciamo dei grafici per capirci qualcosa... ------------------------
# Rappresentiamo la nuvola di punti:
library(ggplot2)
# Tutti insieme...
# ggplot(data = voli_rotte2) +
#   geom_point(mapping = aes(x = distanza, y = MTOW, color=DESCRIPTION1))
# # Solo i Regional
# ggplot(data = voli_rotte2[grep("1", voli_rotte2$FLIGHT_TYPE_ID.x),]) +
#   geom_point(mapping = aes(x = distanza, y = MTOW, color=DESCRIPTION1))
# # Solo gli International
# ggplot(data = voli_rotte2[grep("2", voli_rotte2$FLIGHT_TYPE_ID.x),]) +
#   geom_point(mapping = aes(x = distanza, y = MTOW, color=DESCRIPTION1))
# # E infine solo i Domestic
# ggplot(data = voli_rotte2[grep("3", voli_rotte2$FLIGHT_TYPE_ID.x),]) +
#   geom_point(mapping = aes(x = distanza, y = MTOW, color=DESCRIPTION1))
#
#
# 3 - Realizziamo una cluster analysis ------------------------------------
# 3.1 - Primo approccio: tutto insieme.
# 
# Appliando kmeans, dobbiamo trovare un numero di centri ragionevole...
# Partiamo da un n=3 (pari a regionali+internazionali+domestico) e saliamo di quantità:
set.seed(20)
#
#
voli_rotte_clean <- na.omit(voli_rotte2[ , c("MTOW","distanza")])

# Standardizzazione esiste la funzione di scale

# voli_rotte_clean_sd <- scale(voli_rotte_clean)
# voli_rotte_clean_sd <- unique(voli_rotte_clean)
voli_rotte_clean <- voli_rotte_clean[!duplicated(voli_rotte_clean), ]
voli_rotte_clean_sd$MTOW <- (voli_rotte_clean$MTOW - mean(voli_rotte_clean$MTOW))/(2*sd(voli_rotte_clean$MTOW))
voli_rotte_clean_sd$distanza <- (voli_rotte_clean$distanza - mean(voli_rotte_clean$distanza))/(2*sd(voli_rotte_clean$distanza))
 # elimino i duplicati
#
kvoli_rotte2.3 <- kmeans(voli_rotte_clean_sd, 3, nstart = 30)
summary(kvoli_rotte2.3)
kvoli_rotte2.3$centers
# plot(kvoli_rotte2.3$centers[,c("distanza")], kvoli_rotte2.3$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")

#
kvoli_rotte2.4 <- kmeans(voli_rotte_clean_sd, 4, nstart = 20)
summary(kvoli_rotte2.4)
kvoli_rotte2.4$centers
# plot(kvoli_rotte2.4$centers[,c("distanza")], kvoli_rotte2.4$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
#
# kvoli_rotte2.5 <- kmeans(voli_rotte_clean_sd, 5, nstart = 20)
# summary(kvoli_rotte2.5)
# kvoli_rotte2.5$centers
# plot(kvoli_rotte2.5$centers[,c("distanza")], kvoli_rotte2.5$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# #
# kvoli_rotte2.6 <- kmeans(voli_rotte_clean_sd, 6, nstart = 20)
# summary(kvoli_rotte2.6)
# kvoli_rotte2.6$centers
# plot(kvoli_rotte2.6$centers[,c("distanza")], kvoli_rotte2.6$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# #
# kvoli_rotte2.7 <- kmeans(voli_rotte_clean_sd, 7, nstart = 20)
# summary(kvoli_rotte2.7)
# kvoli_rotte2.7$centers
# plot(kvoli_rotte2.7$centers[,c("distanza")], kvoli_rotte2.7$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# #
# kvoli_rotte2.8 <- kmeans(voli_rotte_clean_sd, 8, nstart = 20)
# summary(kvoli_rotte2.8)
# kvoli_rotte2.8$centers
# plot(kvoli_rotte2.8$centers[,c("distanza")], kvoli_rotte2.8$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# #
# kvoli_rotte2.9 <- kmeans(voli_rotte_clean_sd, 9, nstart = 20)
# summary(kvoli_rotte2.9)
# kvoli_rotte2.9$centers
# plot(kvoli_rotte2.9$centers[,c("distanza")], kvoli_rotte2.9$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# # dopo n=11 compare un centroide anche a basso MTOW e lunga distanza...
# kvoli_rotte2.11 <- kmeans(voli_rotte_clean_sd, 11, nstart = 20)
# summary(kvoli_rotte2.11)
# kvoli_rotte2.11$centers
# plot(kvoli_rotte2.11$centers[,c("distanza")], kvoli_rotte2.11$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# #
# kvoli_rotte2.12 <- kmeans(voli_rotte_clean_sd, 12, nstart = 20)
# summary(kvoli_rotte2.12)
# kvoli_rotte2.12$centers
# plot(kvoli_rotte2.12$centers[,c("distanza")], kvoli_rotte2.12$centers[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
#
# 4 - Scegliamo il cluster ------------------------------------------------
# Dei precedenti scegliamo un solo cluster:
###
###
kvoli_rotte <- kvoli_rotte2.3
###
###
# Destandardizziamo i centroidi
centroidi <- kvoli_rotte$centers
centroidi[,c("MTOW")]     <- 2*sd(voli_rotte_clean[,c("MTOW")])*centroidi[,c("MTOW")]         + mean(voli_rotte_clean$MTOW)*array(1, c(3,1))
centroidi[,c("distanza")] <- 2*sd(voli_rotte_clean[,c("distanza")])*centroidi[,c("distanza")] + mean(voli_rotte_clean[,c("distanza")])*array(1, c(3,1))
centroidi
###
###
###
### Visualizzazione Cluster
###
# Proviamo a mettere sul piano cartesiano i centroidi per n=...
##plot(kvoli_rotte2.12$centers[,c("MTOW")], kvoli_rotte2.12$centers[,c("distanza")])
# plot(centroidi[,c("distanza")], centroidi[,c("MTOW")], main="K-Means | Centroidi", 
#      xlab="distanza", ylab = "MTOW")
# 
# ggplot(data = voli_rotte2) +
#   geom_point(mapping = aes(x = centroidi[,c("distanza")], y = centroidi[,c("MTOW")]))

#
# Notiamo dal grafico appena plottato che sono abbastanza allineati:
# allora lanciamo una regressione lineare semplice!
# abline(lm( voli_rotte2$distanza ~ voli_rotte2$MTOW ))
# # che è molto vicina a quella fatta sui centroidi per n=11!
# abline(lm( kvoli_rotte2.20$centers[,c("distanza")] ~ kvoli_rotte2.20$centers[,c("MTOW")]))


# Eliminiamo le variabili non più necessarie
rm(list=ls(pattern="kvoli_rotte2"))


# 4 - Analisi del cluster ottenuto ----------------------------------------
#
# A questo punto abbiamo realizzato la nostra analisi sui cluster. 
# Vogliamo fare una analisi sommaria sui nostri cluster, in termini di max/min
attach(kvoli_rotte) # 
# Creiamo una nuova variabile: rappresenta i voli associati a ciascun cluster.
flight.clusterizzati <- cbind(voli_rotte_clean, cluster)
n_cluster <- max(as.numeric(flight.clusterizzati$cluster)) # definisci il numero di clusters
#
# Facciamo una verifica: scriviamo i limiti inferiori e superiori dei clusters.
classi_cluster <- data.frame(matrix(0,n_cluster,3))
for (i in seq(1,n_cluster)) {
  classi_cluster[i,1] <- min(flight.clusterizzati[grep(i, flight.clusterizzati$cluster),]$MTOW)
  classi_cluster[i,2] <- max(flight.clusterizzati[grep(i, flight.clusterizzati$cluster),]$MTOW)
  classi_cluster[i,3] <- i
}
colnames(classi_cluster) <- c("min","max","cluster")
classi_cluster <- classi_cluster[order(classi_cluster$min, classi_cluster$max),] 
classi_cluster
# classi_cluster ora mi indica, in maniera ordinata, i vari cluster.

# 5 - Riportiamo i cluster sulla tabella dei voli -------------------------
# 
# Adesso che abbiamo definito i cluster, riaggiustiamo la tabella flight, in modo da poter poi generare
# opportune serie storiche.
# mappa_cluster: questa mappa associa ad ogni id di volo il suo cluster.
mappa_cluster <- cbind(na.omit(voli_rotte2)$ID, cluster)
colnames(mappa_cluster) <- c("ID", "cluster")
# A questo punto non resta che reaggruppare i voli gia estratti secondo questo mapping.


