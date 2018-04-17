library(Boom)
library(zoo)
library(bsts)     # load the bsts package
library(readr)
library('latex2exp')
options( warn = -1 )
options(repr.plot.width=7, repr.plot.height=3)
setwd('~/Desktop/STAT676/bayes-time-series/data/')
gilbralter <- read_csv("gilbralter_time_series_r.csv", 
                                     col_types = cols(startDate = col_date(format = "%Y-%m-%d"), 
                                                      timeIdx = col_skip()))

g = data.frame(gilbralter)
rownames(g) <- g$startDate


#sanity check on first depth
gilT <- ts(g$X0)
plot(gilT, main='SST of Gilbralter region',
     xlab='date',
     ylab='Temperature [C]', 
     ylim=c(15, 26))
nseasons = 12 #A monthly aggregate has 12 samples per period.
ss <- list()
ss <- AddLocalLinearTrend(ss, gilT)
ss <- AddSeasonal(ss, gilT, nseasons=nseasons)
model1 <- bsts(gilT,
               state.specification=ss,
               niter = 1000)
plot(model1,
     main=TeX('Conditional Expectation of $y_i$'),
     xlab='time idx',
     ylab='distribution', 
     ylim=c(15, 26))
par(xpd=TRUE)
legend("topleft", legend=c(TeX('$E(y_i|data)$'), "data"),
       col=c("black", "blue"), lty=c(1,NA), pch=c(NA, 1), cex=1)
plot(model1, "components")


model2 <- bsts(X0 ~ .,
               state.specification = ss,
               niter = 1000,
               data = g)
plot(model2)
plot(model2, "comp")
plot(model2, "coef")
