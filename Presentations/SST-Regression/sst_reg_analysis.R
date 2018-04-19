
library(readr)
library(bsts)

gibraltar <- read_csv("data/gilbralter_time_series_r.csv", 
                       col_types = cols(startDate = col_date(format = "%Y-%m-%d"), 
                                        timeIdx = col_skip()))

g <- data.frame(gibraltar)
rownames(g) <- g$startDate
gilt <- ts(g$X0, start = c(2004, 1, 1), end = c(2017, 11, 29), frequency = 12)
plot(gilt, main='SST of Gilbralter region',
     xlab='date',
     ylab='Temperature [C]', 
     ylim=c(15, 26))
