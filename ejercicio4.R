load("/cloud/project/Repetir.RData")
summary(Repetir)
#Repetir$Repetir <- factor(as.vector(Repetir$Repetir),levels = c('Si','No'))
#Repetir$Sexo <- factor(as.vector(Repetir$Sexo),levels = c('Hombre','Mujer'))
#Repetir$Publico <- factor(as.vector(Repetir$Publico),levels = c('Si','No'))
attach(Repetir)

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(polycor)
library(corrplot)

### MATRIZ DE CORRELACIONES
cormat = hetcor(Repetir)
corrplot(cormat$correlations)

### DISTRIBUCIÓN DE ISEC
hist(ISEC,col = "Orange",breaks = 50)

smoothScatter(ISEC, MOTIVA, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

### MÁSCARAS
public = rep(FALSE,times=length(Sexo))
private = rep(FALSE,times=length(Sexo))
chicos = rep(FALSE,times=length(Sexo))
chicas = rep(FALSE,times=length(Sexo))
repetidor = rep(FALSE,times=length(Sexo))
no_repeti = rep(FALSE,times=length(Sexo))
public[Repetir$Publico == "Si"] = TRUE
private[Repetir$Publico == "No"] = TRUE
chicos[Repetir$Sexo == "Hombre"] = TRUE
chicas[Repetir$Sexo == "Mujer"] = TRUE
repetidor[Repetir$Repetir == "Si"] = TRUE
no_repeti[Repetir$Repetir == "No"] = TRUE


### COLORES
c1 = rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 = rgb(255,122,90, max = 255, alpha = 80, names = "lt.orange")

# MOTIVACIÓN SEGÚN TIPO COLEGIO
hgA = hist(MOTIVA, breaks = 40, plot = FALSE) # Save first histogram data
hgB = hist(MOTIVA[private], breaks = 40, plot = FALSE) # Save 2nd histogram data
plot(hgA, col = c1, main="HIST. PUBLIC VS PRIVATE", xlab="MOTIVA")
plot(hgB, col = c2, add = TRUE)

# MOTIVACIÓN SEGÚN SEXO
hgA = hist(MOTIVA, breaks = 40, plot = FALSE) # Save first histogram data
hgB = hist(MOTIVA[chicas], breaks = 40, plot = FALSE) # Save 2nd histogram data
plot(hgA, col = c1, main="HIST. diferente SEXO", xlab="MOTIVA")
plot(hgB, col = c2, add = TRUE)

### MOTIVACIÓN SEGÚN REPETIR O NO
hgA = hist(MOTIVA, breaks = 40, plot = FALSE) # Save first histogram data
hgB = hist(MOTIVA[repetidor], breaks = 40, plot = FALSE) # Save 2nd histogram data
plot(hgA, col = c1, main="HIST. REP. SÍ VS. NO", xlab="MOTIVA")
plot(hgB, col = c2, add = TRUE)

### MODELOS UNIVARIANTES
mod_sexo =  glm(Repetir$Repetir ~ Sexo,family=binomial(link='logit'))
summary(mod_sexo)

mod_ISEC =  glm(Repetir$Repetir ~ISEC,family=binomial(link='logit'))
summary(mod_ISEC)

mod_publico =  glm(Repetir$Repetir ~Publico,family=binomial(link='logit'))
summary(mod_publico)

mod_MOTIVA =  glm(Repetir$Repetir ~MOTIVA,family=binomial(link='logit'))
summary(mod_MOTIVA)


### MODELO TODAS VARIABLES
mod_todas =  glm(Repetir$Repetir ~ Sexo + ISEC + Publico + MOTIVA,family=binomial(link='logit'))
summary(mod_todas)

h = predict(mod_todas, data.frame(
  Sexo = "Mujer", ISEC = 0.3, Publico = "Si", MOTIVA = 1.0))
p = 1/(1-exp(h))
  