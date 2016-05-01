wordProbPlot <- function(prbs, preds = 3) {
  yl <- max(c(ceiling(max(prbs) * 10)/10, 0.2))
  xl <- 0.2
  if(max(prbs) > 0.2) { xl <- 0.5 }
  par(mar = c(1,4.1,1,1))
  plot(prbs, type = "n"
       , ylim = c(-0.05, yl), ylab = "Probability", yaxt = "n"
       , xlim = c(1 - xl, preds + xl), xaxt = "n", xlab = "", bty = "n"
       , main = "Prediction Probablilities")
  text(1:preds, prbs
       , names(prbs)
       , cex = 1 + log(100 * prbs))
  text(1:preds, rep(-0.05, preds), round(prbs, 8))
  axis(side = 2, at = seq(0, yl, 0.1), line = 0.5)
}
