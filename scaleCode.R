library(forecast)
library(MASS)
library(car)
library(gvlma)
library(ggplot2)
library(lubridate)

# Dati
dati<-read.table("C:/Users/F041/Downloads/weight.csv", header = TRUE, sep = ",")
dati$Date<-as.Date(ymd_hms(dati$Date))
dati$grasso_est<-ifelse(((dati$Fat.mass..kg./dati$Weight..kg.)*100)<12,1,0)


pacf(dati[,c(2,3)])
dati$Weight..kg.<-as.numeric(dati$Weight..kg.)
dati$Fat.mass..kg<-as.numeric(dati$Fat.mass..kg)
altezza=1.68
BMI=as.factor(dati$Weight..kg./(altezza^2));BMI

# Correlazioni
require(corrgram)
corrgram(dati, lower.panel = panel.cor, cex=1, cex.labels = 1)
cor.test(dati[,3],dati[,2])

# Controlo collinearità variabile con TOL sotto 0,3 vengolo tolte
target=dati[,3]
covariate=dati[,c(2,4:6)]
library(mctest)
imcdiag(covariate,target) #Tutto risulta collineare

# Tolgo qualche covariata per ritestare la collinearità DA VERIFICARE
dati$Date<-as.numeric(dati$Date)
covariate=dati[,c(1,2)]
target=dati[,3]
imcdiag(covariate,target)


# Primi grafici
plot(dati$Fat.mass..kg.~dati$Weight..kg., ylim=c(0,12))
hist(dati$Weight..kg., breaks=5)
hist(dati$Fat.mass..kg., breaks=5)

## Plotto la data
plot(dati$Date~dati$Fat.mass..kg.)
plot(dati$Date~dati$Weight..kg)



# Primo modello
model<-lm(Fat.mass..kg.~Weight..kg.+Date, data=dati)
summary(model) # 0.6907, 0.7208 con data
anova(model)


par(mfrow=c(2,2)) 
plot(model)
par(mfrow=c(1,1)) 

p<-predict(model,dati)
plot(p, dati$Fat.mass..kg.)

# Osservazione influenti?
influencePlot(model, main="Influence Plot")
stzed <- rstudent(model)
lever <- hat(model.matrix(model))
dffits1 <- dffits(model)
cooksd <- cooks.distance(model);cooksd
cutoff <- 4/((nrow(dati)-length(model$coefficients)-2))
plot(model, which=4, cook.levels=cutoff)
abline(h=cutoff)
influential <- dati[cooksd >= cutoff,];influential 
influ = dati[influential, ];influ                  
filtered <- dati[cooksd < cutoff, ]  ;filtered    
#Modello senza osservazioni influenti
model_r<-lm(Fat.mass..kg.~Weight..kg.+as.numeric(Date), data=filtered)
summary(model_r)  # 0.6931 senza trasformata, 0.6933 con trasformata, 0.7376 con data e trasformata, 0.7364 senza trasformata

par(mfrow=c(2,2)) 
plot(model_r)
par(mfrow=c(1,1)) 


# Vediamo se conviene una trasformata
boxcox1<-boxcox(model)
bc<-boxcox(model, plotit=T)
title("Lambda and log-likelihood")
which.max(bc$y)
lambda=bc$x[which.max(bc$y)] #Metterò 0.3 per comodità
model_t<-lm(I(Fat.mass..kg.^0.3)~Weight..kg.+Date, data=dati)
summary(model_t) #Non migliora, da 0.7376 a 0.7236


# Controllo ipotesi
#Prima
gvlma(model)

#Dopo
gvlma(model_r) 
#Non funziona su rmodel,  must be an lm object.

#Trasfromazioni covariate?
library(gam)
gam1<-gam(Fat.mass..kg.~s(Weight..kg.)+s(Date), data=filtered)
par(mfrow=c(2,2)) 
plot(gam1)
par(mfrow=c(1,1)) #la data sembra suggerire un log

# Modello robusto
library(robustbase) 
rmodel<-lmrob(Fat.mass..kg.~Weight..kg.+log(Date), data=filtered)
summary(rmodel) # R^2 0.7389

par(mfrow=c(2,2)) 
plot(rmodel)
par(mfrow=c(1,1)) 

shapiro.test(rmodel$residuals) #accetta normalità dei residui


## Logistico, non molto sensato in caso di collinearità

logistico<-glm(dati$grasso_est~Weight..kg.+Date, data=dati)
summary(logistico)
R<-1-(logistico$deviance/logistico$null.deviance);R 


# Correlazioni
require(corrgram)
corrgram(filtered, lower.panel = panel.cor, cex=1, cex.labels = 1)
cor.test(filtered[,3],filtered[,2])

#Grafico filtered
plot(filtered$Fat.mass..kg.~filtered$Weight..kg., ylim=c(0,12))


p2<-predict(rmodel,dati)
plot(p2, dati$Fat.mass..kg.)

  