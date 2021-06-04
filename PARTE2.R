load("/cloud/project/V0.RData")
data = data.frame(V0,stringsAsFactors = FALSE)

## 1- DEPURACIÓN
data$f_destete <-as.Date(as.character(data$f_destete),format="%d/%m/%Y")
data$f_nacim <-as.Date(as.character(data$f_nacim),format="%d/%m/%Y")
data$edad_d = data$f_destete - data$f_nacim
data$edad_d[data$edad_d <90 | data$edad_d>220] = NA

data$alzada[data$alzada <80 | data$alzada>125] = NA
data$long_cruz[data$long_cruz <40 | data$long_cruz>80] = NA
data$ancho_muslos[data$ancho_muslos <30 | data$ancho_muslos>60] = NA
data$curva_nalga[data$curva_nalga <1 | data$curva_nalga>5] = NA
data$peri_torax[data$peri_torax <100 | data$peri_torax>175] = NA
data$peso[data$peso <90 | data$peso>390] = NA


data$tipo[data$tipo == "MC"] <- c("C ")
data$tipo[data$tipo == "A "] <- c("C ")
data$tipo[data$tipo == "AC"] <- c("C ")
data$tipo[data$tipo == " "] <- NA
data$tipo[data$tipo == ""] <- NA
data$tipo <- factor(as.vector(data$tipo),levels = c('C ','N '))
data$sexo[data$sexo == "C"] <- "M"
data$sexo <- factor(as.vector(data$sexo),levels = c('M','H'))

toros = subset(data,sexo=="M"); vacas = subset(data,sexo=="H");
culonas = subset(data,tipo=="C "); normales = subset(data,tipo=="N ")

## 2- DESCRIPCIÓN características
summary(data)

# valanceo sexo/tipo
sum(which(data$sexo=="M" & data$tipo=="C "))
sum(which(data$sexo=="H" & data$tipo=="C "))
sum(which(data$sexo=="M" & data$tipo=="N "))
sum(which(data$sexo=="H" & data$tipo=="N "))
smoothScatter(data$tipo, data$sexo, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# 10 gráficas 
# relación alzada/cruz
smoothScatter(data$alzada, data$long_cruz, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación alzada/ancho muslo
smoothScatter(data$alzada, data$ancho_muslos, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación alzada/peritorax
smoothScatter(data$alzada, data$peri_torax, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación alzada/peso
smoothScatter(data$alzada, data$peso, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación cruz/ancho muslo
smoothScatter(data$long_cruz, data$ancho_muslos, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación cruz/periTorax
smoothScatter(data$long_cruz, data$peri_torax, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación cruz/peso
smoothScatter(data$long_cruz, data$peso, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación ancho muslo/periTorax
smoothScatter(data$ancho_muslos, data$peri_torax, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación ancho muslo/peso
smoothScatter(data$ancho_muslos, data$peso, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación peritorax/peso
smoothScatter(data$peri_torax2, data$peso, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))
# relación edad/alzada
smoothScatter(data$alzada, data$edad_d, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación edad/cruz
smoothScatter(data$long_cruz, data$edad_d, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación edad/torax
smoothScatter(data$peri_torax, data$edad_d, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación edad/muslos
smoothScatter(data$ancho_muslos, data$edad_d, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# relación edad/peso
smoothScatter(data$edad_d, data$peso, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

## CREAR LA VARIABLE PERI2
data$peri_torax2 = data$peri_torax * data$peri_torax
## TRAIN TEST
N_tot = length(data$id)
train = sample(c(1:N_tot), round(0.75*N_tot))
validation = setdiff(c(1:N_tot), train)

## MODELO TODAS
reg_todas = lm(peso~alzada+long_cruz+ancho_muslos+peri_torax+peri_torax2+edad_d,data=data,subset=train)
summary(reg_todas)

## MODELO BEST
reg_best = lm(peso~long_cruz+ancho_muslos+peri_torax2,data=data,subset=train)
summary(reg_best)

## MODELOS UNIVARIENTES
reg_alzada = lm(peso~alzada,data=data,subset=train)
reg_longCruz = lm(peso~long_cruz,data=data,subset=train)
reg_anchoMuslos = lm(peso~ancho_muslos,data=data,subset=train)
reg_periTorax = lm(peso~peri_torax,data=data,subset=train)
reg_periTorax2 =lm(peso~peri_torax2,data=data,subset=train)
reg_edad = lm(peso~edad_d,data=data,subset=train)

## EVALUACION
predict = Predict(reg_best,data,subset=validation)
Y_val = data[validation,]$peso
error = Y_val - predict
R2=1-sum(error^2)/sum((Y_val- mean(Y_val))^2)
plot(reg_best,)

## CLASIFICACION CULONAS VS NORMALES
ggplot(data, aes(x = peri_torax2, y = curva_nalga, color = tipo)) + geom_point()
smoothScatter(data$ancho_muslos, data$tipo, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))
logit_todas = glm(tipo~alzada+long_cruz+ancho_muslos+peri_torax+peri_torax2+edad_d+curva_nalga,data=data,subset=train, family=binomial)
logit_nalga=glm(tipo~curva_nalga,data=data,subset=train, family=binomial)
logit_best=glm(tipo~curva_nalga+alzada+ancho_muslos+long_cruz,
                data=data,subset=train,family=binomial)
summary(logit_best)

pR2 (logit_best)

shapiro.test(logit_best$residuals[validation[1:5000]])

bptest(logit_best)

