# Questo script ricava un modello lineare che correla le Rotte (ovvero la loro distanza) al MTOW del velivolo.
#
# The fit of the model can be summarized by sigma and by R-squared
# R-squared the fraction of variance “explained” by the model and the “unexplained” variance is sigma-squared
# Sigma is residual standard deviation.
# (from page 41 Gelman)
#
# Rif.: A Handbook of Statistical Anlyses Using R | B.S. Everitt, T. Hothorn 
#
library(arm)# Così possiamo standardizzare
attach(voli_rotte2)
#
# Facciamo un modello di regressione lineare:
# Scopriamo un pattern interessante (e atteso): il MTOW cresce con la distanza associata alla rotta!
# questo vuol dire che aerei più grandi sono impiegati per percorrere distanze più lunghe!
#
# 1- Primo modello di regressione lineare ---------------------------------
fit.1 <- lm(MTOW ~ distanza)
summary(fit.1) 
hist(fit.1$residuals, xlab="MTOW", ylab="Frequenza", nclass=50, main="Residuals fit.1")
# Dal comando precedente, notiamo che abbiamo un buon R-Squared (0.6684): questo attesta che la parte spiegata è significativa.
# Inoltre il coefficiente angolare stimato ha uno standard error basso: questo indica che è significativo.
# 
# 2- Secondo modello di regressione lineare -------------------------------
# Proviamo a realizzare un nuovo modello di regressione lineare: un predittore continuo e uno categorico.
# (Questo caso è riportato anche in Gelman pag. 33)
fit.2 <- lm(MTOW ~ distanza + FLIGHT_TYPE_ID.x)
summary(fit.2)
fit.2b <- standardize (fit.2)
summary(fit.2b)
# 
hist(fit.2$residuals, xlab="MTOW", ylab="Frequenza", nclass=50, main="Residuals fit.2")
#
# 3- Terzo modello di regressione lineare ---------------------------------
# Valutiamo l'interazione.
fit.3 <- lm(MTOW ~ distanza + FLIGHT_TYPE_ID.x + distanza:FLIGHT_TYPE_ID.x)
summary(fit.3)
hist(fit.3$residuals, xlab="MTOW", ylab="Frequenza", nclass=50, main="Residuals fit.3")
fit.3b <- standardize (fit.3)
summary(fit.3b)
# Quindi il risultato con l'R-squared più alto (cioè con la parte spiegata più ampia è il terzo),
# nonostante i risultati siano essenzialmente equivalenti.
#
cartella <- c('PICT')
ifelse(!dir.exists(file.path(getwd(), cartella)), dir.create(file.path(getwd(), cartella)), FALSE) 
# Grafico comparativo degli Squared ottenuti ------------------------------
barplot(cbind(summary(fit.1)$r.squared, summary(fit.2)$r.squared, summary(fit.3)$r.squared),
        main="Linear Regression Models | R-squared", 
        names.arg=c("fit.1", "fit.2", "fit.3")
        )

# ... che dimostra che i risultati sono sostanzialmente equivalenti.
#
# Grafico di MTOW vs ROTTE ------------------------------------------------
plot (distanza, MTOW, xlab="Route Distance [km]", ylab="MTOW [kg]", cex=.5)
# Sovrapponi al grafico fit.1
curve (cbind(1,x) %*% coef(fit.1), add=TRUE)
# Sovrappoini fit.2
curve (cbind (1, x, 1) %*% coef(fit.2), add=TRUE, col="gray")
curve (cbind (1, x, 2) %*% coef(fit.2), add=TRUE, col="red")
curve (cbind (1, x, 3) %*% coef(fit.2), add=TRUE, col="blue")


# Rimuovi i modelli
rm(fit.1, fit.2, fit.3, fit.2b, fit.3b)
detach(voli_rotte2)
dev.off() 

