
library(readr)
library(bsts) # also loads BoomSpikeSlab, Boom, MASS, zoo, xts


gilbralter <- read_csv("data/gilbraltersimple.csv")
gilt <- ts(gilbralter$tempMean, start=c(2004,1,13),
           end=c(2017, 11, 25), frequency=30)
plot(gilt, main='SST of Gilbralter region',
     xlab='Date',
     ylab='Temperature (C)')


# ------- local level model ----------------

ll_ss <- list()
ll_ss <- AddLocalLevel(state.specification = ll_ss, y = gilt)
ll_fit <- bsts(formula = gilt, state.specification = ll_ss, niter = 1e4)
# probably want to reduce niter or cache results in slides
# so it doesn't take forever to compile
# make sure to mention we use small numbers to make it faster for presentation

# summary(ll_fit)
#burnin <- SuggestBurn(0.1, ll_fit)
# note: almost all the functions we use will use SuggestBurn(0.1, fit)
# to drop burn in

plot(ll_fit)  # posterior of conditional mean Z_t^T alpha_t given data
plot(ll_fit, 'components')  # level
plot(ll_fit, 'residuals')

ll_pred <- predict(ll_fit, horizon = 30)  # one year ahead
plot(ll_pred, plot.original = 90)  # with previous 3 years


# ------- local linear trend model -------
llt_ss <- list()
llt_ss <- AddLocalLinearTrend(state.specification = llt_ss, y = gilt)
llt_fit <- bsts(gilt, state.specification = llt_ss, niter = 1e4)

plot(llt_fit)
plot(llt_fit, 'components')
plot(llt_fit, 'residuals')

llt_pred <- predict(llt_fit, horizon = 30)
plot(llt_pred, plot.original = 90)


# ----------- linear trend with seasonal ----------

lts_ss <- list()
lts_ss <- AddLocalLinearTrend(lts_ss, y = gilt)
lts_ss <- AddSeasonal(lts_ss, gilt, nseasons = 30)
# warning: this fit takes a few minutes
lts_fit <- bsts(gilt, state.specification = lts_ss, niter = 1e4)


plot(lts_fit)
plot(lts_fit, 'components')
plot(lts_fit, 'residuals')

lts_pred <- predict(lts_fit, horizon = 30)
plot(lts_pred, plot.original = 90)


# -------- model comparison -------

# cumulative absolute one-step-ahead prediction error
# see paper for more info
CompareBstsModels(model.list = list(
  level = ll_fit, trend = llt_fit, season = lts_fit))






