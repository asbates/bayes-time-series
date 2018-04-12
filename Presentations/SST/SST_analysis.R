
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

# summary(ll_fit)
#burnin <- SuggestBurn(0.1, ll_fit)

plot(ll_fit)  # posterior of conditional mean Z_t^T alpha_t given data
plot(ll_fit, 'components')  # level
plot(ll_fit, 'residuals')

ll_pred <- predict(ll_fit, horizon = 30)  # one year ahead
plot(ll_pred, plot.original = 90)  # with previous 3 years


# ------- local linear trend model -------

llt_ss <- AddLocalLinearTrend(list(), y = gilt)
llt_fit <- bsts(gilt, state.specificatio = llt_ss, niter = 1e4)

plot(llt_fit)
plot(llt_fit, 'components')
plot(llt_fit, 'residuals')

llt_pred <- predict(llt_fit, horizon = 30)
plot(llt_pred, plot.original = 90)
