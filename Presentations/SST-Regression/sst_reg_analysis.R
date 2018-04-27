
library(readr)
library(bsts)

gib <- read_csv("data/gilbralter_time_series_r_2.csv", 
                       col_types = cols(startDate = col_skip(), 
                                        timeIdx = col_skip()))
names(gib) <- c('SST', '10m', '20m', '30m', '40m',
                      '50m', '60m', '70m', '80m', '90m')
gib_train <- gib[1:146, ]
gib_test <- gib[147:158, ]

ts.plot(gib_train$SST, main='SST of Gilbralter region',
     ylab='Temperature [C]', 
     ylim=c(15, 26))

ss <- AddLocalLinearTrend(list(), gib$SST)
ss <- AddSeasonal(ss, gib$SST, nseasons = 12)
model1 <- bsts(SST ~., state.specification = ss,
               data = gib_train, niter = 1000, ping = 0)

plot(model1, 'components')
plot(model1, 'coefficients')

# posterior inclusion probabilities
apply(model1$coefficients, MARGIN = 2, function(x) mean(x != 0))


model1_pred <- predict(model1, newdata = gib_test[, -1], horizon = 12)
plot(model1_pred, plot.original = 36)
points(147:158, gib_test$SST, col = 'red')


# ----- change prior from default ------

model2 <- bsts(SST ~., state.specification = ss,
               data = gib_train, niter = 1000, ping = 0,
               expected.model.size = 2)

plot(model2, 'coefficients')

# model2_pred <- predict(model2, newdata = gib_test[,-1], horizon = 12)
# plot(model2_pred, plot.original = 36)
# points(147:158, gib_test$SST)
bp = c(.01,.5,.3,.3,.1,.1,.1,.1,.1,.1)
model3 <- bsts(SST ~., state.specification = ss,
               data = gib_train, niter = 1000, ping = 0,
               prior.inclusion.probabilities = bp)
plot(model3, 'coefficients')

model3_pred <- predict(model3, newdata = gib_test[,-1], horizon = 12)
plot(model3_pred, plot.original = 36)
points(147:158, gib_test$SST, col = 'red')
