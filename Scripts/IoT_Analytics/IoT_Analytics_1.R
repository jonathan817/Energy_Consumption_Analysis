
library(Rtools)
library(DBI)
library(RMySQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(MASS)
library(reshape2)
library(reshape)
library(ggplot2)
library(plotly)
library(zoo)
library(forecast)
library(padr)
library(ggExtra)
library(imputeTS)
library(prophet)
library(forecastHybrid)
#library(ggfortify)

options(scipen=99)

pacman::p_load()

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con)

dbListFields(con,'iris')

irisALL <- dbGetQuery(con, "SELECT * FROM iris")

irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

yr_2006_all <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007_all <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008_all <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009_all <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010_all <- dbGetQuery(con, "SELECT * FROM yr_2010")

yr_todos <- bind_rows(yr_2007_all, yr_2008_all, yr_2009_all, yr_2010_all)

yr_todos <-cbind(yr_todos,paste(yr_todos$Date,yr_todos$Time), stringsAsFactors=FALSE)

colnames(yr_todos)[11] <-"DateTime"

yr_todos <- yr_todos[,c(ncol(yr_todos), 1:(ncol(yr_todos)-1))]

yr_todos$DateTime <- ymd_hms(yr_todos$DateTime)

# yr_todos$DateTime <- as.POSIXct(yr_todos$DateTime, "%Y/%m/%d %H:%M:%S")

# attr(yr_todos$DateTime, "tzone") <- "Europe/Paris"



# Relevant Metrics in energy comsuption data set (yr_todos)

yr_todos_ggplot_1<- yr_todos[,c(9:13)]

colnames(yr_todos_ggplot_1)[1] <-"Kitchen"
colnames(yr_todos_ggplot_1)[2] <-"Laundry_Room"
colnames(yr_todos_ggplot_1)[3] <-"Water_Air_Systems"

yr_todos_ggplot_1_grouped  <- yr_todos_ggplot_1 %>% group_by(year,month) %>% 
  summarise (Kitchen = sum(Kitchen), Laundry_Room = sum (Laundry_Room), Water_Air_Systems = sum(Water_Air_Systems)) %>% 
  arrange(year,month)

ggplot(yr_todos_ggplot_1_grouped, aes(x = month, group = 1)) +
  geom_line(aes(y = Kitchen, col = "Kitchen")) + geom_point((aes(y= Kitchen, col = "Kitchen"))) +
  geom_line(aes(y = Laundry_Room, col = "Laundry")) + geom_point((aes(y= Laundry_Room, col = "Laundry"))) +
  geom_line(aes(y = Water_Air_Systems, col = "Water_Air_Systems")) +
  geom_point((aes(y= Water_Air_Systems, col = "Water_Air_Systems"))) + facet_grid(~year) + theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title="Energy Consumption", subtitle= "Household Areas") + xlab("Month") + ylab("Watts")


# Plot all of sub-meter 1

plot(yr_todos$Sub_metering_1)


# Subset the second week of 2008 - All Observations

houseWeek <- filter(yr_todos, year == 2008 & week == 2)

# Plot subset houseWeek

plot(houseWeek$Sub_metering_1)


# Visualize a Single Day with Plotly ####

# Subset the 9th day of January 2008 - All observations

houseDay <- filter(yr_todos, year == 2008 & month == "enero" & day == 9)

# Plot sub-meter 1

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')


# Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Reducing Granularity ####

houseDay10 <- filter(yr_todos, year == 2008 & month == "enero" & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Visualize a Single Week with Plotly ####

# Subset the 7th week of the  2008 year- All observations.

houseWeek <- filter(yr_todos, year == 2008 & week == 7)

# Reducing Granularity ####

houseWeek30 <- filter(yr_todos, year == 2008 & week == 7 & (minute == 0 | minute == 30))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 30 Minutes frequency on week 7th.

plot_ly(houseWeek30, x = ~houseWeek30$DateTime, y = ~houseWeek30$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption the 7th week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



# Visualize a Single month with Plotly ####

# Subset the 12th month of the  2008 year- All observations.

houseMonth <- filter(yr_todos, year == 2008 & month == "diciembre")

# Reducing Granularity ####
# Average consumption per days of the month.

houseMonth30 <- filter(yr_todos, year == 2008 & month == "diciembre") %>% group_by(day) %>%
  summarise (Kitchen = mean(Sub_metering_1), Laundry_Room = mean(Sub_metering_2), Water_Air_Systems = mean(Sub_metering_3))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - daily frequency on week .

plot_ly(houseMonth30, x = ~houseMonth30$day, y = ~houseMonth30$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth30$Laundry_Room, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth30$Water_Air_Systems, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Daily Power Consumption during December 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))



# Prepare to analyze the data ####

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008, 2009 and 2010.

house07080910weekly <- filter(yr_todos, Weekday == "lunes" & hour == 20 & minute == 1)

## Create TS object with SubMeter3

tsSM3_07080910weekly <- ts(house07080910weekly$Sub_metering_3, frequency=52, start=c(2007,1))


## Plot sub-meter 3 with autoplot.

autoplot(tsSM3_07080910weekly)

## Plot sub-meter 3 with autoplot - add labels, color

autoplot(tsSM3_07080910weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts

plot.ts(tsSM3_07080910weekly)


# Dealing with missing values ####

yr_todos_ts <- yr_todos[,-c(2,3,4,12,13,14,15,16,17,18)]

yr_todos_ts <- pad(yr_todos_ts, by="DateTime", break_above=3)

yr_todos_ts <- ts(yr_todos_ts$Sub_metering_3, frequency=12, start=c(2007,1))

# yr_todos_ts_2 <- na.interp(yr_todos_ts)

autoplot(yr_todos_ts_2, series="Interpolated") +
  autolayer(yr_todos_ts, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="gray"))


# Forecasting Timeseries ####

# Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM3 <- tslm(tsSM3_07080910weekly ~ trend + season) 

summary(fitSM3)


# Create the forecast for sub-meter 3. Forecast ahead 20 time periods

forecastfitSM3 <- forecast(fitSM3, h=20)

# Plot the forecast for sub-meter 3.

plot(forecastfitSM3)


# Create sub-meter 3 forecast with confidence levels 80 and 90

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

# Plot sub-meter 3 forecast, limit y and add labels

plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")





# Checking the optimal granularity for each measurement of energy.####

yr_todos_ts <- yr_todos[,-c(2,3,4,12,13,14,15,16,17,18)]

yr_todos_ts <- pad(yr_todos_ts, by="DateTime", break_above=3)

# yr_todos_ts <- ts(yr_todos_ts$Sub_metering_3, frequency=12, start=c(2007,1))

# yr_todos_ts_2 <- yr_todos_ts

# yr_todos_ts_2[,-c(1)] <- sapply(yr_todos_ts[,-c(1)], function(x) { na.interp(x)})

yr_todos_ts <- na_interpolation(yr_todos_ts)

yr_todos_ts$year <- year(yr_todos_ts$DateTime)

yr_todos_ts$month <- month(yr_todos_ts$DateTime, label = TRUE, abbr = FALSE)

yr_todos_ts$day <- day(yr_todos_ts$DateTime)

yr_todos_ts$week <- week(yr_todos_ts$DateTime)

yr_todos_ts$minute <- minute(yr_todos_ts$DateTime)

yr_todos_ts$Weekday <- weekdays(yr_todos_ts$DateTime)

yr_todos_ts$hour <- hour(yr_todos_ts$DateTime)


#-------------------------

# Creation of Heat Calendar of NAs.

source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

calendarHeat(yr_todos_ts$DateTime,
             values= yr_todos_ts$Sub_metering_1,
             varname= "Missing values",
             color= "r2b")

#-------------------------


# Spike Sara-Ignacio ####


spike_1 <- yr_todos %>% group_by(year, month, day) %>%
  summarise (total_energy = sum(Global_active_power))

spike_2 <- yr_todos %>% group_by(year, month) %>%
  summarise (total_energy = sum(Global_active_power))

spike_1_ts <- ts(spike_1$total_energy, frequency = 365.25, start = c(2007,01))

spike_1_ts_strl <- spike_1_ts %>% stl(s.window = "periodic")

spike_2_ts <- ts(spike_2$total_energy, frequency = 12, start = c(2007,01))

spike_2_ts_strl <- spike_2_ts %>% stl(s.window = "periodic")


autoplot(spike_1_ts_strl)

autoplot(spike_2_ts_strl)


# Importance of each component.

apply(spike_1_ts_strl$time.series,2,var) / var(spike_1_ts)


apply(spike_2_ts_strl$time.series,2,var) / var(spike_2_ts)


# Splitting into Training and testing: training from 2007-01 to 2010-01 and testing from 2010-02 on
# Must: train size > forecast period

Activemonth_train <- window(spike_2_ts, end = c(2010, 01))

Activemonth_test <- window(spike_2_ts, start = c(2010, 02))

# Holtwinters model

Activemonth_HW <- HoltWinters(Activemonth_train)

Activemonth_forec <- forecast(Activemonth_HW, h= 10) # h = 10 (10 months forecast)

autoplot(Activemonth_test) + autolayer(Activemonth_forec$mean, series ="HW")

autoplot(spike_2_ts) + autolayer(Activemonth_forec$mean, series ="HW")

accuracy(Activemonth_forec,Activemonth_test) # Capitulo 12.8 del libro.



# Crear un for loop para mirar la granularity óptima mirando el "seasonal", "trend" and "remainder" o elemento Random.

# Crear un for loop para extraer el MAPE más bajo usando los resultados de la función "accuracy".


# List creation.

yr_todos_year <- yr_todos %>% group_by(year) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy), kitchen = sum(Sub_metering_1), laundry_room = sum (Sub_metering_2), water_air_systems = sum(Sub_metering_3)) 

yr_todos_month <- yr_todos %>% group_by(year,month) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy), kitchen = sum(Sub_metering_1), laundry_room = sum (Sub_metering_2), water_air_systems = sum(Sub_metering_3))

yr_todos_week <- yr_todos %>% group_by(year,month,week) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy), kitchen = sum(Sub_metering_1), laundry_room = sum (Sub_metering_2), water_air_systems = sum(Sub_metering_3))


list_1 <- list(yr_todos_year,yr_todos_month,yr_todos_week)


#--------------------

# Data set with energy prices.

yr_todos_ts_prices <- yr_todos_ts

yr_todos_ts_prices$ID <- floor_date(yr_todos_ts_prices$DateTime, unit = "month")

yr_todos_ts_prices <- yr_todos_ts_prices %>% group_by(year,month, ID) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy), kitchen = sum(Sub_metering_1), laundry_room = sum (Sub_metering_2), water_air_systems = sum(Sub_metering_3), Average_Global_intensity = mean(Global_intensity), Average_Voltage = mean(Voltage))

yr_todos_ts_prices$Month_year <- paste(yr_todos_ts_prices$year,"-",yr_todos_ts_prices$month)

yr_todos_ts_prices$Monthly_Bill <- round((yr_todos_ts_prices$total_energy/1000)*0.1799,2)

yr_todos_ts_prices$Kitchen_Bill <- round((yr_todos_ts_prices$kitchen/1000)*0.1799,2)

yr_todos_ts_prices$Laundry_Bill <- round((yr_todos_ts_prices$laundry_room/1000)*0.1799,2)

yr_todos_ts_prices$Water_Air_Bill <- round((yr_todos_ts_prices$water_air_systems/1000)*0.1799,2)

yr_todos_ts_prices$Unmeasured_Bill <- round((yr_todos_ts_prices$Monthly_Bill-yr_todos_ts_prices$Kitchen_Bill-yr_todos_ts_prices$Laundry_Bill-yr_todos_ts_prices$Water_Air_Bill),2)

yr_todos_ts_prices <- yr_todos_ts_prices[,c(1:2,4:15,3)]

write.csv(yr_todos_ts_prices, "power_bi_1.csv")


yr_todos_ts_prices_2 <- yr_todos_ts

yr_todos_ts_prices_2$ID <- floor_date(yr_todos_ts_prices_2$DateTime, unit = "day")

yr_todos_ts_prices_2 <- yr_todos_ts_prices_2 %>% group_by(ID) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy), kitchen = sum(Sub_metering_1), laundry_room = sum (Sub_metering_2), water_air_systems = sum(Sub_metering_3), Average_Global_intensity = mean(Global_intensity), Average_Voltage = mean(Voltage))

yr_todos_ts_prices_2$Daily_Bill <- round((yr_todos_ts_prices_2$total_energy/1000)*0.1799,2)

yr_todos_ts_prices_2$Kitchen_Bill <- round((yr_todos_ts_prices_2$kitchen/1000)*0.1799,2)

yr_todos_ts_prices_2$Laundry_Bill <- round((yr_todos_ts_prices_2$laundry_room/1000)*0.1799,2)

yr_todos_ts_prices_2$Water_Air_Bill <- round((yr_todos_ts_prices_2$water_air_systems/1000)*0.1799,2)

yr_todos_ts_prices_2$Unmeasured_Bill <- round((yr_todos_ts_prices_2$Daily_Bill-yr_todos_ts_prices_2$Kitchen_Bill-yr_todos_ts_prices_2$Laundry_Bill-yr_todos_ts_prices_2$Water_Air_Bill),2)

yr_todos_ts_prices_2$year <- year(yr_todos_ts_prices_2$ID)

yr_todos_ts_prices_2$month <- month(yr_todos_ts_prices_2$ID, label = TRUE, abbr = FALSE)

yr_todos_ts_prices_2$day <- day(yr_todos_ts_prices_2$ID)

yr_todos_ts_prices_2$Weekday <- weekdays(yr_todos_ts_prices_2$ID)

write.csv(yr_todos_ts_prices_2, "power_bi_2.csv")




#--------------------
#--------------------

total_energy_year <- yr_todos_ts %>% group_by(year) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy))



total_energy_month <- yr_todos_ts %>% group_by(year,month) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy))

total_energy_month_ts <- ts(total_energy_month$total_energy, frequency = 12, start = c(2007,01))

total_energy_month_ts_strl <- total_energy_month_ts %>% stl(s.window = "periodic")

apply(total_energy_month_ts_strl$time.series,2,var) / var(total_energy_month_ts)

total_energy_month_ts_train <- window(total_energy_month_ts, end = c(2010, 01))

total_energy_month_ts_test <- window(total_energy_month_ts, start = c(2010, 02))


# Without Seasonality.

total_energy_month_ts_no_season <- decompose(total_energy_month_ts)

total_energy_month_ts_no_season <- total_energy_month_ts - total_energy_month_ts_no_season$seasonal

total_energy_month_ts_train_no_season <- window(total_energy_month_ts_no_season, end = c(2010, 01))

total_energy_month_ts_test_no_season <- window(total_energy_month_ts_no_season, start = c(2010, 02))


# Holtwinters model

total_energy_month_HW <- HoltWinters(total_energy_month_ts_train)

total_energy_month_forec <- forecast(total_energy_month_HW, h= 10) # h = 10 (10 months forecast)

autoplot(total_energy_month_ts_test) + autolayer(total_energy_month_forec$mean, series ="HW")

accuracy(total_energy_month_forec,total_energy_month_ts_test) # Capitulo 12.8 del libro.

# Arima model.

total_energy_month_arima1 <- total_energy_month_ts_train %>% auto.arima()

total_energy_month_forec_2 <- forecast(total_energy_month_arima1, h= 10)

autoplot(total_energy_month_ts_test) + autolayer(total_energy_month_forec_2$mean, series ="Arima")

accuracy(total_energy_month_forec_2,total_energy_month_ts_test)


# ForecastHybrid model.

total_energy_month_hybrid_1 <- hybridModel(total_energy_month_ts_train, weights="equal")

total_energy_month_hybrid_2 <- hybridModel(total_energy_month_ts_train, weights="insample")

total_energy_month_hybrid_2_total <- hybridModel(total_energy_month_ts, weights="insample")

h <- length(total_energy_month_ts_test)

total_energy_month_hybrid_1_forecast <- forecast(total_energy_month_hybrid_1, h=h)

total_energy_month_hybrid_2_forecast <- forecast(total_energy_month_hybrid_2, h=h)

total_energy_month_hybrid_2_total_forecast <- forecast(total_energy_month_hybrid_2_total, h=h)

accuracy(total_energy_month_hybrid_1_forecast,total_energy_month_ts_test)

accuracy(total_energy_month_hybrid_2_forecast,total_energy_month_ts_test)

autoplot(total_energy_month_hybrid_1_forecast) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab("total Energy Consumption")

autoplot(total_energy_month_hybrid_2_forecast) + ggtitle("Hybrid - Insample mode") + xlab("Year") +
  ylab("total Energy Consumption")

autoplot(total_energy_month_hybrid_2_total_forecast) + ggtitle("Predicted Total Energy Consumptio - Hybrid Model") + xlab("Year") +
  ylab("Total Energy Consumption")

autoplot(total_energy_month_ts_test) + autolayer(total_energy_month_hybrid_1_forecast$mean, series ="Hybrid_1")

autoplot(total_energy_month_ts_test) + autolayer(total_energy_month_hybrid_2_forecast$mean, series ="hybrid_2")

autoplot(total_energy_month_ts) + autolayer(total_energy_month_hybrid_2_total_forecast$mean, series ="hybrid_2")



# Prophet model.

total_energy_month_2 <- yr_todos_ts %>% thicken("month") %>% group_by(DateTime_month) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (y = sum(total_energy))

names(total_energy_month_2)[1]<-"ds"

total_energy_month_2_train <- total_energy_month_2 %>% dplyr::filter (ds < as.Date("2010-02-01"))

total_energy_month_2_test <- total_energy_month_2 %>% dplyr::filter (ds >= as.Date("2010-02-01"))

total_energy_month_prophet <- prophet(total_energy_month_2_train)

total_energy_month_prophet_future <- make_future_dataframe(total_energy_month_prophet, periods = 10, freq = "month")

tail(total_energy_month_prophet_future)

total_energy_month_prophet_forecast <- predict(total_energy_month_prophet, total_energy_month_prophet_future)

plot(total_energy_month_prophet, total_energy_month_prophet_forecast)

prophet_plot_components(total_energy_month_prophet, total_energy_month_prophet_forecast)

dyplot.prophet(total_energy_month_prophet, total_energy_month_prophet_forecast)

accuracy(total_energy_month_prophet_forecast[38:47, "yhat"], total_energy_month_2_test$y)




total_energy_week <- yr_todos_ts %>% group_by(year,month,week) %>% mutate(total_energy = (Global_active_power)*1000/60) %>% 
  summarise (total_energy = sum(total_energy))

total_energy_week_ts <- ts(total_energy_week$total_energy, frequency = 52, start = c(2007,01))

total_energy_week_ts_strl <- total_energy_week_ts %>% stl(s.window = "periodic")

apply(total_energy_week_ts_strl$time.series,2,var) / var(total_energy_week_ts)

#--------------------

kitchen_energy_year <- yr_todos_ts %>% group_by(year) %>% 
  summarise (kitchen = sum(Sub_metering_1))


kitchen_energy_month <- yr_todos_ts %>% group_by(year,month) %>% 
  summarise (kitchen = sum(Sub_metering_1))

kitchen_energy_month_ts <- ts(kitchen_energy_month$kitchen, frequency = 12, start = c(2007,01))

kitchen_energy_month_ts_strl <- kitchen_energy_month_ts %>% stl(s.window = "periodic")

apply(kitchen_energy_month_ts_strl$time.series,2,var) / var(kitchen_energy_month_ts)


kitchen_energy_month_ts_train <- window(kitchen_energy_month_ts, end = c(2010, 01))

kitchen_energy_month_ts_test <- window(kitchen_energy_month_ts, start = c(2010, 02))


# Without Seasonality.

kitchen_energy_month_ts_no_season <- decompose(kitchen_energy_month_ts)

kitchen_energy_month_ts_no_season <- kitchen_energy_month_ts - kitchen_energy_month_ts_no_season$seasonal

kitchen_energy_month_ts_train_no_season <- window(kitchen_energy_month_ts_no_season, end = c(2010, 01))

kitchen_energy_month_ts_test_no_season <- window(kitchen_energy_month_ts_no_season, start = c(2010, 02))



# Holtwinters model

kitchen_energy_month_HW <- HoltWinters(kitchen_energy_month_ts_train)

kitchen_energy_month_forec <- forecast(kitchen_energy_month_HW, h= 10) # h = 10 (10 months forecast)

autoplot(kitchen_energy_month_ts_test) + autolayer(kitchen_energy_month_forec$mean, series ="HW")

accuracy(kitchen_energy_month_forec,kitchen_energy_month_ts_test) # Capitulo 12.8 del libro.


kitchen_energy_total_HW <- HoltWinters(kitchen_energy_month_ts)

kitchen_energy_month_forec_total <- forecast(kitchen_energy_total_HW, h= 10)

autoplot(kitchen_energy_month_ts) + autolayer(kitchen_energy_month_forec_total$mean, series ="HW")


# Arima model.

kitchen_energy_month_arima1 <- kitchen_energy_month_ts_train %>% auto.arima()

kitchen_energy_month_forec_2 <- forecast(kitchen_energy_month_arima1, h= 10)

autoplot(kitchen_energy_month_ts_test) + autolayer(kitchen_energy_month_forec_2$mean, series ="Arima")

accuracy(kitchen_energy_month_forec_2,kitchen_energy_month_ts_test)


# ForecastHybrid model.

kitchen_energy_month_hybrid_1 <- hybridModel(kitchen_energy_month_ts_train, weights="equal")

kitchen_energy_month_hybrid_2 <- hybridModel(kitchen_energy_month_ts_train, weights="insample")

h <- length(kitchen_energy_month_ts_test)

kitchen_energy_month_hybrid_1_forecast <- forecast(kitchen_energy_month_hybrid_1, h=h)

kitchen_energy_month_hybrid_2_forecast <- forecast(kitchen_energy_month_hybrid_2, h=h)

accuracy(kitchen_energy_month_hybrid_1_forecast,kitchen_energy_month_ts_test)

accuracy(kitchen_energy_month_hybrid_2_forecast,kitchen_energy_month_ts_test)

autoplot(kitchen_energy_month_hybrid_1_forecast) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab("kitchen Energy Consumption")

autoplot(kitchen_energy_month_hybrid_2_forecast) + ggtitle("Hybrid 2") + xlab("Year") +
  ylab("kitchen Energy Consumption")

autoplot(kitchen_energy_month_ts_test) + autolayer(kitchen_energy_month_hybrid_1_forecast$mean, series ="Hybrid_1")

autoplot(kitchen_energy_month_ts_test) + autolayer(kitchen_energy_month_hybrid_2_forecast$mean, series ="hybrid_2")



# Prophet model.

kitchen_energy_month_2 <- yr_todos_ts %>% thicken("month") %>% group_by(DateTime_month) %>%  summarise (y = sum(Sub_metering_1))

names(kitchen_energy_month_2)[1]<-"ds"

kitchen_energy_month_2_train <- kitchen_energy_month_2 %>% dplyr::filter (ds < as.Date("2010-02-01"))

kitchen_energy_month_2_test <- kitchen_energy_month_2 %>% dplyr::filter (ds >= as.Date("2010-02-01"))

kitchen_energy_month_prophet <- prophet(kitchen_energy_month_2_train)

kitchen_energy_month_prophet_future <- make_future_dataframe(kitchen_energy_month_prophet, periods = 10, freq = "month")

tail(kitchen_energy_month_prophet_future)

kitchen_energy_month_prophet_forecast <- predict(kitchen_energy_month_prophet, kitchen_energy_month_prophet_future)

plot(kitchen_energy_month_prophet, kitchen_energy_month_prophet_forecast)

prophet_plot_components(kitchen_energy_month_prophet, kitchen_energy_month_prophet_forecast)

dyplot.prophet(kitchen_energy_month_prophet, kitchen_energy_month_prophet_forecast)

accuracy(kitchen_energy_month_prophet_forecast[38:47, "yhat"], kitchen_energy_month_2_test$y)



kitchen_energy_week <- yr_todos_ts %>% group_by(year,month,week) %>% 
  summarise (kitchen = sum(Sub_metering_1))

kitchen_energy_week_ts <- ts(kitchen_energy_week$kitchen, frequency = 52, start = c(2007,01))

kitchen_energy_week_ts_strl <- kitchen_energy_week_ts %>% stl(s.window = "periodic")

apply(kitchen_energy_week_ts_strl$time.series,2,var) / var(kitchen_energy_week_ts)

#--------------------

laundry_energy_year <- yr_todos_ts %>% group_by(year) %>% 
  summarise (laundry_room = sum(Sub_metering_2))



laundry_energy_month <- yr_todos_ts %>% group_by(year,month) %>% 
  summarise (laundry_room = sum(Sub_metering_2))

laundry_energy_month_ts <- ts(laundry_energy_month$laundry_room, frequency = 12, start = c(2007,01))

laundry_energy_month_ts_strl <- laundry_energy_month_ts %>% stl(s.window = "periodic")

apply(laundry_energy_month_ts_strl$time.series,2,var) / var(laundry_energy_month_ts)


laundry_energy_month_ts_train <- window(laundry_energy_month_ts, end = c(2010, 01))

laundry_energy_month_ts_test <- window(laundry_energy_month_ts, start = c(2010, 02))


# Without Seasonality.

laundry_energy_month_ts_no_season <- decompose(laundry_energy_month_ts)

laundry_energy_month_ts_no_season <- laundry_energy_month_ts - laundry_energy_month_ts_no_season$seasonal

laundry_energy_month_ts_train_no_season <- window(laundry_energy_month_ts_no_season, end = c(2010, 01))

laundry_energy_month_ts_test_no_season <- window(laundry_energy_month_ts_no_season, start = c(2010, 02))



# Holtwinters model

laundry_energy_month_HW <- HoltWinters(laundry_energy_month_ts_train)

laundry_energy_month_forec <- forecast(laundry_energy_month_HW, h= 10) # h = 10 (10 months forecast)

autoplot(laundry_energy_month_ts_test) + autolayer(laundry_energy_month_forec$mean, series ="HW")

accuracy(laundry_energy_month_forec,laundry_energy_month_ts_test) # Capitulo 12.8 del libro.


# Arima model.

laundry_energy_month_arima1 <- laundry_energy_month_ts_train %>% auto.arima()

laundry_energy_month_forec_2 <- forecast(laundry_energy_month_arima1, h= 10)

autoplot(laundry_energy_month_ts_test) + autolayer(laundry_energy_month_forec_2$mean, series ="Arima")

accuracy(laundry_energy_month_forec_2,laundry_energy_month_ts_test)


# ForecastHybrid model.

laundry_energy_month_hybrid_1 <- hybridModel(laundry_energy_month_ts_train, weights="equal")

laundry_energy_month_hybrid_2 <- hybridModel(laundry_energy_month_ts_train, weights="insample")

laundry_energy_month_hybrid_2_total <- hybridModel(laundry_energy_month_ts, weights="insample")

h <- length(laundry_energy_month_ts_test)

laundry_energy_month_hybrid_1_forecast <- forecast(laundry_energy_month_hybrid_1, h=h)

laundry_energy_month_hybrid_2_forecast <- forecast(laundry_energy_month_hybrid_2, h=h)

laundry_energy_month_hybrid_2_total_forecast <- forecast(laundry_energy_month_hybrid_2_total, h=h)

accuracy(laundry_energy_month_hybrid_1_forecast,laundry_energy_month_ts_test)

accuracy(laundry_energy_month_hybrid_2_forecast,laundry_energy_month_ts_test)

autoplot(laundry_energy_month_hybrid_1_forecast) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab("Laundry Energy Consumption")

autoplot(laundry_energy_month_hybrid_2_forecast) + ggtitle("Hybrid 2") + xlab("Year") +
  ylab("Laundry Energy Consumption")

autoplot(laundry_energy_month_hybrid_2_total_forecast) + ggtitle("Predicted Laundry Energy Consumption - Hybrid Model") + xlab("Year") +
  ylab("Laundry Energy Consumption")

autoplot(laundry_energy_month_ts_test) + autolayer(laundry_energy_month_hybrid_1_forecast$mean, series ="Hybrid_1")

autoplot(laundry_energy_month_ts_test) + autolayer(laundry_energy_month_hybrid_2_forecast$mean, series ="hybrid_2")

autoplot(laundry_energy_month_ts) + autolayer(laundry_energy_month_hybrid_2_total_forecast$mean, series ="hybrid_2")


# Prophet model.

laundry_energy_month_2 <- yr_todos_ts %>% thicken("month") %>% group_by(DateTime_month) %>%  summarise (y = sum(Sub_metering_2))

names(laundry_energy_month_2)[1]<-"ds"

laundry_energy_month_2_train <- laundry_energy_month_2 %>% dplyr::filter (ds < as.Date("2010-02-01"))

laundry_energy_month_2_test <- laundry_energy_month_2 %>% dplyr::filter (ds >= as.Date("2010-02-01"))

laundry_energy_month_prophet <- prophet(laundry_energy_month_2_train)

laundry_energy_month_prophet_future <- make_future_dataframe(laundry_energy_month_prophet, periods = 10, freq = "month")

tail(laundry_energy_month_prophet_future)

laundry_energy_month_prophet_forecast <- predict(laundry_energy_month_prophet, laundry_energy_month_prophet_future)

plot(laundry_energy_month_prophet, laundry_energy_month_prophet_forecast)

prophet_plot_components(laundry_energy_month_prophet, laundry_energy_month_prophet_forecast)

dyplot.prophet(laundry_energy_month_prophet, laundry_energy_month_prophet_forecast)

accuracy(laundry_energy_month_prophet_forecast[38:47, "yhat"], laundry_energy_month_2_test$y)



laundry_energy_week <- yr_todos_ts %>% group_by(year,month,week) %>% 
  summarise (laundry_room = sum(Sub_metering_2))

laundry_energy_week_ts <- ts(laundry_energy_week$laundry_room, frequency = 52, start = c(2007,01))

laundry_energy_week_ts_strl <- laundry_energy_week_ts %>% stl(s.window = "periodic")

apply(laundry_energy_week_ts_strl$time.series,2,var) / var(laundry_energy_week_ts)


#--------------------

air_energy_year <- yr_todos_ts %>% group_by(year) %>% 
  summarise (water_air_systems = sum(Sub_metering_3))



air_energy_month <- yr_todos_ts %>% group_by(year,month) %>% 
  summarise (water_air_systems = sum(Sub_metering_3))

air_energy_month_ts <- ts(air_energy_month$water_air_systems, frequency = 12, start = c(2007,01))

air_energy_month_ts_strl <- air_energy_month_ts %>% stl(s.window = "periodic")

apply(air_energy_month_ts_strl$time.series,2,var) / var(air_energy_month_ts)


air_energy_month_ts_train <- window(air_energy_month_ts, end = c(2010, 01))

air_energy_month_ts_test <- window(air_energy_month_ts, start = c(2010, 02))


# Without Seasonality.

air_energy_month_ts_no_season <- decompose(air_energy_month_ts)

air_energy_month_ts_no_season <- air_energy_month_ts - air_energy_month_ts_no_season$seasonal

air_energy_month_ts_train_no_season <- window(air_energy_month_ts_no_season, end = c(2010, 01))

air_energy_month_ts_test_no_season <- window(air_energy_month_ts_no_season, start = c(2010, 02))




# Holtwinters model

air_energy_month_HW <- HoltWinters(air_energy_month_ts_train)

air_energy_month_forec <- forecast(air_energy_month_HW, h= 10) # h = 10 (10 months forecast)

autoplot(air_energy_month_ts_test) + autolayer(air_energy_month_forec$mean, series ="HW")

accuracy(air_energy_month_forec,air_energy_month_ts_test) # Capitulo 12.8 del libro.


# Arima model.

air_energy_month_arima1 <- air_energy_month_ts_train %>% auto.arima()

air_energy_month_forec_2 <- forecast(air_energy_month_arima1, h= 10)

autoplot(air_energy_month_ts_test) + autolayer(air_energy_month_forec_2$mean, series ="Arima")

accuracy(air_energy_month_forec_2,air_energy_month_ts_test)


air_energy_month_arima_total <- air_energy_month_ts %>% auto.arima()

air_energy_month_forec_2_total <- forecast(air_energy_month_arima_total, h= 10)

autoplot(air_energy_month_ts) + autolayer(air_energy_month_forec_2_total$mean, series ="Arima")


# ForecastHybrid model.

air_energy_month_hybrid_1 <- hybridModel(air_energy_month_ts_train, weights="equal")

air_energy_month_hybrid_2 <- hybridModel(air_energy_month_ts_train, weights="insample")

h <- length(air_energy_month_ts_test)

air_energy_month_hybrid_1_forecast <- forecast(air_energy_month_hybrid_1, h=h)

air_energy_month_hybrid_2_forecast <- forecast(air_energy_month_hybrid_2, h=h)

accuracy(air_energy_month_hybrid_1_forecast,air_energy_month_ts_test)

accuracy(air_energy_month_hybrid_2_forecast,air_energy_month_ts_test)

autoplot(air_energy_month_hybrid_1_forecast) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab("Air Energy Consumption")

autoplot(air_energy_month_hybrid_2_forecast) + ggtitle("Hybrid 2") + xlab("Year") +
  ylab("Air Energy Consumption")

autoplot(air_energy_month_ts_test) + autolayer(air_energy_month_hybrid_1_forecast$mean, series ="Hybrid_1")

autoplot(air_energy_month_ts_test) + autolayer(air_energy_month_hybrid_2_forecast$mean, series ="hybrid_2")



# Prophet model.

air_energy_month_2 <- yr_todos_ts %>% thicken("month") %>% group_by(DateTime_month) %>% summarise (y = sum(Sub_metering_3))

names(air_energy_month_2)[1]<-"ds"

air_energy_month_2_train <- air_energy_month_2 %>% dplyr::filter (ds < as.Date("2010-02-01"))

air_energy_month_2_test <- air_energy_month_2 %>% dplyr::filter (ds >= as.Date("2010-02-01"))

air_energy_month_prophet <- prophet(air_energy_month_2_train)

air_energy_month_prophet_future <- make_future_dataframe(air_energy_month_prophet, periods = 10, freq = "month")

tail(air_energy_month_prophet_future)

air_energy_month_prophet_forecast <- predict(air_energy_month_prophet, air_energy_month_prophet_future)

plot(air_energy_month_prophet, air_energy_month_prophet_forecast)

prophet_plot_components(air_energy_month_prophet, air_energy_month_prophet_forecast)

dyplot.prophet(air_energy_month_prophet, air_energy_month_prophet_forecast)

accuracy(air_energy_month_prophet_forecast[38:47, "yhat"], air_energy_month_2_test$y)


air_energy_week <- yr_todos_ts %>% group_by(year,month,week) %>% 
  summarise (water_air_systems = sum(Sub_metering_3))

air_energy_week_ts <- ts(air_energy_week$water_air_systems, frequency = 52, start = c(2007,01))

air_energy_week_ts_strl <- air_energy_week_ts %>% stl(s.window = "periodic")

apply(air_energy_week_ts_strl$time.series,2,var) / var(air_energy_week_ts)



#--------------------


# Prediction Exported.

predicted_total_energy <- as.data.frame(total_energy_month_hybrid_2_total_forecast$mean, row.names("predicciones"))

names(predicted_total_energy) [1] <- "Predicted_Total_Energy"

predicted_total_energy$ID <- total_energy_month_prophet_future[48:57, 1]

predicted_total_energy$Bill <- round((predicted_total_energy$Predicted_Total_Energy/1000)*0.1799,2) 

predicted_total_energy$kitchen_energy <- kitchen_energy_month_forec_total$mean

predicted_total_energy$Bill_kitchen_energy <- round((predicted_total_energy$kitchen_energy/1000)*0.1799,2)

predicted_total_energy$laundry_energy <- laundry_energy_month_hybrid_2_total_forecast$mean

predicted_total_energy$Bill_laundry_energy <- round((predicted_total_energy$laundry_energy/1000)*0.1799,2)

predicted_total_energy$air_energy <- air_energy_month_forec_2$mean

predicted_total_energy$Bill_air_energy <- round((predicted_total_energy$air_energy/1000)*0.1799,2)

predicted_total_energy$Unknown_Bill <- round(as.numeric(predicted_total_energy$Bill) - as.numeric(predicted_total_energy$Bill_kitchen_energy) - as.numeric(predicted_total_energy$Bill_laundry_energy) - as.numeric(predicted_total_energy$Bill_air_energy),2)

predicted_total_energy <- predicted_total_energy[2:10,]

write.csv(predicted_total_energy, "power_bi_3.csv")


#--------------------
