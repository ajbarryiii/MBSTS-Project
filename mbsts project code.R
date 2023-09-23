library(tidyverse)
library(mbsts)
library(rstan)
library(MARSS)
library(zoo)

trends<-read.csv("/Users/ajbarry/Dropbox/My Mac (AJ’s MacBook Pro)/Downloads/google_trends.csv")
stocks<-read.csv("/Users/ajbarry/Dropbox/My Mac (AJ’s MacBook Pro)/Downloads/tickers.csv")

trends <- trends %>%
  mutate_all(~ ifelse(. == "<1", 0, .))

trends<- trends %>%
  select_if(~ !all(. == 0))

#expand trends data to each date
alldates<- trends %>%
  mutate(date = as.Date(date)) %>%
  mutate(week_start = date - 6) %>%
  rowwise() %>%
  mutate(dates = list(seq(week_start, date, by = "day"))) %>%
  unnest(dates)



alldates <- alldates %>%
  mutate(dates = as.Date(dates))

column_names <- names(alldates)[2:12]

alldates <- alldates %>%
  mutate_at(.vars = column_names, .funs = as.numeric)

alldates <- alldates %>%
  dplyr::select(-date, -week_start)

alldates <- alldates %>%
  rename(date = dates)

stocks<-stocks%>%
  mutate(date = as.Date(date))
# filter data to use only contemporaneous dates
alldata <- stocks %>%
  inner_join(alldates, by = "date")


stocks.cl<- alldata[,1:10]
stocks.cl <- stocks.cl %>%
  dplyr::select(-CVG.Close)

trends.cl<- alldata[,c(1, 11:length(names(alldata)))]

## Plot the time series'
stocks.plot<- stocks.cl %>%
  pivot_longer(cols = -date, names_to = "series", values_to = "value")

ggplot(stocks.plot, aes(x = date, y = value, color = series)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "Value", title = "Plot of Stocks over Time", color = "Series")



## Plot the trends:


trends.plot<- trends.cl %>%
  pivot_longer(cols = -date, names_to = "series", values_to = "value")

ggplot(trends.plot, aes(x = date, y = value, color = series)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "Value", title = "Plot of Trends over time", color = "Series")

#normalize trends
trends.norm <- trends.cl %>%
  mutate_if(is.numeric, ~ . / mean(.))

#Y<-as.matrix(stocks.cl[,-1])
#Y<-Y[,-8]

#X<-as.matrix(trends.norm[, -1])

#split dataset into training set and test set
#n=dim(Y)[1]
#ntrain=n-5
#Ytrain<-Y[1:ntrain,]
#Xtrain<-X[1:ntrain,]
#Ytest<-Y[(ntrain+1):n,]
#Xtest<-X[(ntrain+1):n,]



## Model estimation:
#model_list <- list(
  #m = 8, # Number of time series
  #B = "diagonal and equal", # Local linear trend
  #A = "diagonal and equal", # Seasonal component
  #C = "diagonal and equal", # Cyclic component
  #R = "diagonal and equal", # Residuals
  #U = "zero", # No process errors
  #X = list(Xtrain) # Predictor variables
#)

#fit.test <- MARSS(Ytrain, control = list(maxit = 1000))



#specify Ts components
#Tmodel<-tsc.setting(Ytrain,mu=rep(1, dim(Y)[2]),rho=rep(0.5, dim(Y)[2]),S=rep(275, dim(Y)[2]),vrho = rep(0.95, dim(Y)[2]),lambda=rep(pi/dim(Y)[2], dim(Y)[2]))

#ki<- c(8,dim(Xtrain)[2])
#pii<- matrix(rep(0.5,dim(Xtrain)[2]),nrow=dim(Xtrain)[2])

#mbsts.model<-mbsts_function(Ytrain,Xtrain,Tmodel,ki,pii,v0=5,mc=100,burn=100)

## Stan model

model <- stan_model("/Users/ajbarry/Dropbox/My Mac (AJ’s MacBook Pro)/Downloads/mbsts.stan")

## Clean data for stan model on full dataset

stocks.cl$period = index(stocks.cl)
stocks.cl <- stocks.cl %>%
  dplyr::select(-date)

trends.cl <- trends.cl %>%
  dplyr::select(-date)

N<-dim(stocks.cl)[1]*dim(stocks.cl)[1]


assemble_stan_data <- function(stocks.cl, trends.cl) compose_data(
  # The data
  stocks.cl, 
  x=as.matrix(trends.cl[1:max(period),]), 
  N_periods = max(stocks.cl$period),
  N_features = ncol(trends.cl),
  # Parameters controlling the model 
  periods_to_predict = 5, 
  p = params$p, 
  q = params$q, 
  ar = params$ar, 
  period_scale = params$period_scale, 
  s = array(params$s, dim = length(params$s)),
  N_seasonality = length(params$s),
  cyclicality_prior = 52 * 5, 
  corr_prior = params$corr_prior,
  # Parameters controlling hierarchical shrinkage
  nu = params$nu, 
  m0 = params$m0,
  .n_name = n_prefix("N"))

test_len<-30

prices_gathered <- stocks.cl %>%
  gather(key="series", value="log_price", -period) %>%
  mutate(price = exp(log_price) - 1) %>%
  dplyr::select(-log_price) %>%
  arrange(period, series) %>%
  mutate(id = 1:n())

exact_stan_data <- prices_gathered %>%
  dplyr::filter(period < (max(period)-test_len)) %>% 
  rename(y=price) %>% 
  mutate(weight = 1) %>% 
  assemble_stan_data(trends.cl)

prices_gathered %<>% rename(y=price)


stan_data <<- exact_stan_data
price_set <<- prices_gathered


print(exact)

init_f <- function(chain_id=1) {
  list(alpha_ar="0")
}

samples <- sampling(model, data=stan_data, 
                    chains=3,
                    seed = 1,
                    #warmup=1000,
                    iter=1000, thin=1, init=init_f, 
                    cores=3, save_warmup=FALSE,
                    refresh=200,
) %>% recover_types(price_set)


save(samples, file="mbsts_fit.Rdata")

library(shinystan)
library(bayesplot)
library(tidybayes)



y_hat <- samples %>% gather_draws(log_y_hat[id]) %>%
  mutate(.value = expm1(.value))

y_hat %>% 
  dplyr::select(id, .value) %>%
  inner_join(price_set) %>% 
  mutate(
    error = .value - y
  ) %>%
  group_by(id, series) %>%
  summarize(
    mean_error = mean(error),
    rmse = sqrt(mean(error^2)), 
    sd_y_hat = sd(.value)
  ) %>% 
  dplyr::select(series, mean_error, rmse, sd_y_hat) %>%
  gather(key="metric", value="value", -series, -id) %>% 
  ggplot(aes(x = value, fill=series)) +
  geom_histogram(bins=40, position="identity", alpha = 1/params$S) + 
  facet_grid(series ~ metric, scales="free") +
  theme_minimal() +
  ggtitle("Residuals")


price_set %>% dplyr::select(id, y, series) %>%
  inner_join(y_hat %>% 
               group_by(id) %>%
               summarize(y_hat = mean(.value))) %>%
  ggplot(aes(x = y, y=y_hat)) +
  geom_point(size=0.2, alpha=0.2) +
  facet_wrap(~ series) +
  scale_x_continuous(limits=c(0, NA)) + 
  scale_y_continuous(limits=c(0, NA)) + 
  annotate("segment", x = 0, y = 0, xend=max(price_set$y), yend=max(price_set$y), color="red", size=0.5, alpha=0.5) +
  theme_minimal() + 
  ggtitle("Prices vs. Mean Predictions")

periodic_price_hat <- samples %>% gather_draws(log_prices[period, series])

mean_periodic_price_hat <- periodic_price_hat %>%
  dplyr::filter(is.finite(expm1(.value))) %>% 
  group_by(period, series) %>%
  summarize(
    y_hat = expm1(mean(.value))
  ) %>%
  mutate(.chain = as.character("mean_hat"))

periodic_price_hat %>%
  dplyr::filter(is.finite(.value)) %>% 
  group_by(period, series, .chain) %>%
  summarize(
    y_hat = expm1(mean(.value))
  ) %>%
  ungroup() %>%
  mutate(.chain = as.character(.chain)) %>%
  bind_rows(prices_gathered %>% mutate(.chain="gen") %>% rename(y_hat=y), mean_periodic_price_hat) %>% 
  mutate(
    #    alpha = case_when(
    #      .chain %in% c("gen", "mean_hat") ~ .8, 
    #      TRUE ~ .2
    #    ), 
    .chain = fct_relevel(factor(.chain), c("gen", "mean_hat"))
  ) %>% 
  ggplot(aes(x = period, y = y_hat, color=series, linetype=.chain)) +
  geom_line(size=0.3, alpha=0.6) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1000))
  ggtitle("Reconstruction of Time Series By Chain")

  
price_forecast <- samples %>% 
  gather_draws(log_prices_hat[period, series])


true_future <- prices_gathered %>%
  dplyr::filter(period > (max(stocks.cl$period)-test_len)) %>%
  mutate(period = period - (max(stocks.cl$period)))

price_forecast_summary <- price_forecast %>%
  dplyr::filter(is.finite(expm1(.value))) %>%
  ungroup() %>%
  group_by(series, period, .chain) %>%
  summarize(y = expm1(mean(.value)), .groups = "drop") %>%
  mutate(.chain = as.character(.chain))

ribbon_data <- price_forecast %>%
  dplyr::filter(is.finite(.value)) %>%
  mutate(y = expm1(.value), .chain = "median") %>%
  ungroup() %>%
  dplyr::select(series, period, y, .chain) %>% 
  group_by(period, series, .chain) %>% 
  median_qi(y, .width = c(0.05, 0.1, 0.2)) %>%
  dplyr::select(period, series, .chain, ymin = .lower, ymax = .upper)

bind_rows(true_future %>% dplyr::select(-id) %>% mutate(.chain = "gen"), price_forecast_summary) %>%
  ggplot(aes(x = period, y = y, color = series, linetype = .chain)) +
  geom_line() + 
  geom_lineribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.3, size = 0.5, data = ribbon_data) +
  scale_fill_brewer("Confidence", palette = "Blues") +
  theme_minimal() +
  scale_y_log10() +
  facet_grid(series ~ ., scales = "free") +
  ggtitle("Forecasted Prices")

