#test
load("D:/Documents/R/SignalCancellation/data/N1000_1.rda")
R <- N1000_1
N <-  nrow(R)
seuils = c(.001,.25)

library(devtools)
document()
load_all()

