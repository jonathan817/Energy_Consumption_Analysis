pacman::p_load(fpp2)

auscafe


autoplot(auscafe)

autoplot(auscafe %>% stl  (s.window = "periodic"))

# To remove the variance we do.

ts_log <- log(auscafe)

autoplot(ts_log)

# To remove the trend,

seasonauscafe <- diff(ts_log)

autoplot(seasonauscafe)

# Descomposing the element.

auscafe_descomp <- seasonauscafe %>% stl(s.window = "periodic")

autoplot(auscafe_descomp)

autoplot(seasonauscafe %>% stl(s.window = "periodic"))


# To remove seasonality.

statiauscafe <- seasonauscafe %>% stl(s.window = "periodic") %>% seasadj()

autoplot (statiauscafe)

autoplot (statiauscafe %>% stl(s.window = "periodic"))

# to find ACF or PACF we could use:
# ggAcf(ts) or ggPacf(ts)  or ggtsdisplay(ts)

Diff=1

ggAcf(statiauscafe)

ggPacf(statiauscafe)

ggtsdisplay(statiauscafe)


auscafe_train <- window(statiauscafe, end = c(2016, 1))

auscafe_test <- window(statiauscafe, start = c(2016, 2))


mod1 <- auscafe_train %>% Arima(order=c(12,0,0))

# Other way to execute Arima:

mod1 <- Arima(auscafe_train, order=c(12,0,0)) # AICc= -1987.74


mod2 <- auscafe_train %>% Arima(order=c(2,0,0))

# Other way to execute Arima:

mod2 <- Arima(auscafe_train, order=c(2,0,0)) # AICc= -1963.70

mod3 <- auscafe_train %>% auto.arima() # # AICc= -1986.98

autoplot(auscafe_test, color="red") +
  autolayer(forecast(mod1, h= length(auscafe_test)),PI=FALSE,
            series = "ARIMA 12,0,0", color="black") +
  autolayer(forecast(mod2, h= length(auscafe_test)),PI=FALSE,
            series = "ARIMA 2,0,0", color="green") +
  autolayer(forecast(mod3, h= length(auscafe_test)),PI=FALSE,
            series = "AUTOARIMA", color="orange")


accuracy(forecast(mod3, h=20),auscafe_test)

checkresiduals (forecast(mod3, h=20))


# When figure out what is the best model it must be considered:
# AICc
# Accuracy error metrics ACF (ME, MAE, MAPE)
# Forecast (plot the forecast)
# Residuals.










