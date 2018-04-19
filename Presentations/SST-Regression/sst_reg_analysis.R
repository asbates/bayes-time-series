
library(readr)
library(bsts)

gib <- read_csv("data/gilbralter_time_series_r.csv", 
                       col_types = cols(startDate = col_skip(), 
                                        timeIdx = col_skip()))
names(gib) <- c('SST', '10', '20', '30', '40',
                      '50', '60', '70', '80', '90')
gib <- zooreg(gibraltar, start = c(2004, 1, 1), end = c(2017, 11, 29),
               frequency = 12)

plot(gib$SST, main='SST of Gilbralter region',
     xlab='date',
     ylab='Temperature [C]', 
     ylim=c(15, 26))

ss <- AddLocalLinearTrend(list(), gib$SST)
ss <- AddSeasonal(ss, gib$SST, nseasons = 12)
model1 <- bsts(SST ~., state.specification = ss,
               data = gib, niter = 1000)
