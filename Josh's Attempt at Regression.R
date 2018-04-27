library(bsts)

gilt = ts(gilbralter$tempMean, start = c(2004,1,13), end=c(2017,11,25), frequency=30)
plot(gilt)

#local level model
ll_ss = list()
ll_ss = AddLocalLevel(state.specification = ll_ss, y = gilt)
ll_fit = bsts(gilt, state.specification = ll_ss, niter = 1000)

plot(ll_fit)
plot(ll_fit, 'components')
plot(ll_fit, 'residuals')

ll_pred = predict(ll_fit, horizon = 30)
plot(ll_pred, plot.original = 90)

#local linear trend model
llt_ss = list()
llt_ss = AddLocalLinearTrend(state.specification = llt_ss, y = gilt)
llt_fit = bsts(gilt, state.specification = llt_ss, niter = 1000)

plot(llt_fit)
plot(llt_fit, 'components')
plot(llt_fit, 'residuals')

llt_pred = predict(llt_fit, horizon=30)
plot(llt_pred, plot.original = 90)

#seasonal model
lts_ss = list()
lts_ss = AddLocalLinearTrend(state.specification = lts_ss, y = gilt)
lts_ss = AddSeasonal(lts_ss, gilt, nseasons = 30)
lts_fit = bsts(gilt, state.specification = lts_ss, niter = 1000)

plot(lts_fit, 'components')

lts_pred = predict(lts_fit, horizon = 30)
plot(lts_pred, plot.original = 90)

CompareBstsModels(lwd = 4, model.list = list(level = ll_fit, trend = llt_fit, season = lts_fit), colors = c("green", "red", "blue"))

#Regression Stuffs

library(readr)
gib <- read_csv("Presentations/SST/data/gilbralter_time_series_r_recent.csv")
gib = gib[,-c(1,3)]
names(gib) = c("SST", "10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m")
gib_train = gib[1:130,]
gib_test = gib[131:158,]

ss = AddLocalLinearTrend(list(), y=gib_train$SST)
ss = AddSeasonal(ss, gib_train$SST, nseasons = 12)
model1 = bsts(SST ~ ., state.specification = ss, data = gib_train, niter = 1000)
plot(model1)
plot(model1, 'components')
plot(model1, 'coefficients')

ss_pred = predict(model1, newdata = gib_test, horizon = 28)
plot(ss_pred, plot.original = 90)

#expected model size = 2
model2 = bsts(SST ~ ., state.specification = ss, data = gib_train, niter = 1000, expected.model.size = 2)
plot(model2, 'components')
plot(model2, 'coefficients')

ss_pred2 = predict(model2, newdata = gib_test, horizon = 28)
plot(ss_pred2, plot.original = 90)

#specifying inclusion probabilities
bp = c(.01,.5,.3,.3,.1,.1,.1,.1,.1,.1)
model3 = bsts(SST ~ ., state.specification = ss, data = gib_train, niter = 1000, expected.model.size = 2, prior.inclusion.probabilities = bp)
plot(model3, 'components')
plot(model3, 'coefficients')

ss_pred3 = predict(model3, newdata = gib_test, horizon = 28)
plot(ss_pred3, plot.original = 90)

CompareBstsModels(lwd = 4, model.list = list(model1,model2,model3), colors = c("green", "red", "blue"))
