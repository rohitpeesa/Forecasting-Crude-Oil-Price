library(fpp3)
setwd("C:/Users/rohit/Downloads")
data<-read.csv("POILAPSPUSDM.csv")
data<- data %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%  
  as_tsibble()

data %>%
  autoplot()+
  labs(y="Price of Crude Oil ")
# Sudden jump and fall in 2008/9 period
# Sudden fall in 2015

data %>%
  gg_season()
# Seasonality cannot be inferred

data %>%
  gg_subseries()
# mean lies around same


data %>%
  ACF(lag_max = 36) %>%
  autoplot()
# Continously decaying ACF indicates no seasonality


data %>%
  model(stl = STL(POILAPSPUSDM)) %>%
  components() %>%
  autoplot()

data %>%
  features(POILAPSPUSDM, feat_stl)
# As observed before we have an increasing trend which is strong
# and seasonality is very low

lambda <- data %>%
  features(POILAPSPUSDM, features = guerrero) %>%
  pull(lambda_guerrero)
lambda

data<- data %>% mutate(Price_bc=box_cox(POILAPSPUSDM,lambda))

data %>% autoplot(Price_bc)+
  labs(y="Transformed Price of Crude Oil ")

# Splitting into train and test sets
# (~20% holdout i.e data from 2015 Nov to 2022 Apr)
data_train <- data %>%
  filter(yearmonth(Month) < yearmonth("2015 Nov"))

data_train %>% features(POILAPSPUSDM, unitroot_kpss)
# data is not stationary as p value is less than 0.01 so 
# we need to differentiate
data_train %>% features(POILAPSPUSDM, unitroot_ndiffs)
# it says differentiating once should be good.
data_train %>% features(POILAPSPUSDM, unitroot_nsdiffs)
# no need of seasonal differentiation

data_train %>% features(Price_bc %>%
                         difference(),
                       unitroot_kpss )
# p value is 0.1 which is larger than 0.05,
# we cannot reject null and accept data is stationary
data_train %>% features(Price_bc %>%
                         difference(),
                       unitroot_ndiffs )
# no seasonal differences are required which
# says 1 difference is good to go.


data_train <- data_train %>% mutate(Price_diff =Price_bc %>%
                                      difference()) 

data_train %>% 
  gg_tsdisplay(Price_diff, plot_type = "partial",lag=36)


data_train_fit <- data_train %>%
  model(# It is non seasonal so PDQ=0, 1 normal differencing so d=1
    #In PACF plot we notice that after lag 1, most significant 
    #is at lag 25 which appears confusing so let's automate it.
    autoARIMA = ARIMA(Price_bc, stepwise = FALSE, approx = FALSE),
    # Model has trend but no seasonality when checked before
    # so AAN(Holt's method) but as we know sometimes
    # this completely produces increasing or decreasing
    # results so we also need to try Damped trends method
    # AAdN
    AAN=ETS(Price_bc~error("A") + trend("A") + season("N")),
    AAdN=ETS(Price_bc~error("A") + trend("Ad") + season("N")),
    autoETS=ETS(Price_bc),
    # Sometimes benchmark model drift performs way better than 
    # advanced so trying to implement them
    drift=RW(Price_bc ~ drift())
  )

data_train_fit %>% pivot_longer(everything(), names_to = "Model name",
                               values_to = "Orders")
glance(data_train_fit) %>% arrange(AICc) %>% select(.model:BIC)
# Autoarima and AutoETS have the least AICc in respective methods

# A forecasting method works best when residuals represent white
# noise

# Residual diagnostics for ARIMA
data_train_fit  %>% select(autoARIMA) %>% 
  gg_tsresiduals(lag=36)

# Residuals perfectly represent a white noise but has few
# significance lags
augment(data_train_fit) %>%
  filter(.model=='autoARIMA') %>%
  features(.innov, ljung_box, lag = 12, dof=6)
# Pvalue being greater than 0.05 helps us confirm that
# residuals resemble white noise

# Residual diagnostics for ETS
data_train_fit  %>% select(autoETS) %>% 
  gg_tsresiduals(lag=36)

# Residuals perfectly represent a white noise but has few
# significance lags
augment(data_train_fit) %>%
  filter(.model=='autoETS') %>%
  features(.innov, ljung_box, lag = 12, dof=1)
# Pvalue being less than 0.05 helps us confirm that
# residuals don't resemble white noise

# Residual diagnostics for drift
data_train_fit %>%
  select(drift)%>%
  gg_tsresiduals()

augment(data_train_fit %>% 
          select(drift))%>%
  features(.innov, ljung_box,lag=12)
# Pvalue being less than 0.05 helps us confirm that
# residuals don't resemble white noise


data_train_fc<- data_train_fit %>%
  forecast(h = "78 months")

data_train_fc %>%
  filter(data_train_fc[1] != "AAdN" & data_train_fc[1] != "AAN") %>%
  autoplot(data)

data_train_fc %>%
  filter(data_train_fc[1] != "AAdN" & data_train_fc[1] != "AAN") %>%
  accuracy(data)
# Drift appears to be very good

# Evaluating distribution forecasts using
# scale free Continuous Ranked Probability Score
data_train_fc %>%
  filter(data_train_fc[1] == "drift" | data_train_fc[1] == "autoARIMA") %>%
  accuracy(data, list(skill = skill_score(CRPS)))
# This suggests drift model has to be improved a lot
# before being implemented to forecast
# in comparison to ARIMA(3,1,3)

