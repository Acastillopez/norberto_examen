load("/cloud/project/Resistencia.RData")
#install.packages('corrplot')
summary(Resistencia)
attach(Resistencia)

y = resistencia

smoothScatter(longitud, resistencia, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

smoothScatter(cantidad, resistencia, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

smoothScatter('^'(cantidad-17,2), resistencia, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

smoothScatter(temperatura, resistencia, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

Resistencia$cantilde = '^'(cantidad-17,2)
attach(Resistencia)
N_tot = length(id)
spec = c(train = .6, test = .4)
g = sample(cut(seq(N_tot),N_tot*cumsum(c(0,spec)),labels = names(spec)))
full = split(Resistencia,g)
train = full$train; test = full$test

mod_todas = lm(resistencia ~  longitud+ cantilde+temperatura , data = train)
summary(mod_todas)

pred_max = predict(mod_todas, data.frame(
  longitud = 30, cantilde = 0, temperatura = 40))
intervalo = c ( pred_max - qt(0.995, pred_max) , pred_max + qt(0.995, pred_max) )
