
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
               data = gib, niter = 1000, ping = 0)

plot(model1, 'components')
plot(model1, 'coefficients')

apply(model1$coefficients, MARGIN = 2, function(x) mean(x != 0))

newdata <- matrix(0, ncol = 9, nrow = 12)
newdata[1, ] <- colMeans(gib[, 2:10])
gib_sd <- apply(gib, 2, sd)
for(i in 2:12){
  for(j in 2:9)
  newdata[i, ] <- newdata[1, ] + rnorm(1, sd = gib_sd[j])
}

model1_pred <- predict(model1, newdata = newdata, horizon = 12)
plot(model1_pred, plot.original = 36)





