library(forecast)

setwd("/Users/tomnguyen/Desktop/Anne's files /Unemployment_Project")


#*****************************Part 1 : Data Exploration************************

#	Creating a time series data set for unemployment in R using the ts() function.
unemploy = read.csv("long_term_unemployment.csv",skip = 10)
unemploy.ts <- ts(unemploy$UEMP27OV, 
               start = c(2000, 1), end = c(2018, 12), freq = 12)

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
unemploy.stl <- stl(unemploy.ts, s.window = "periodic")
autoplot(unemploy.stl, main = "Long-term Unemployment Series Component")


#Use Acf() function to identify autocorrealtion and plot autocorrrelation
autocor <- Acf(unemploy.ts, lag.max = 12, main = "Autocorrelation for Unemployment")

## Use plot() to plot time series data  
plot(unemploy.ts, 
     xlab = "Time", ylab = "No of people unemployed",
     ylim = c(500, 7000), bty = "l",
     xlim = c(2000, 2020), main = "Monthly Unemployment", col = "blue")


#**************************Part 2 : Data Partition************************

#partitioning the data
nValid <- 68
nTrain <- length(unemploy.ts) - nValid
train.ts <- window(unemploy.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(unemploy.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))

# Use Arima() function to fit AR(1) model.
#test for predictability
unemploy.ar1 = Arima(unemploy.ts, order = c(1,0,0))
summary(unemploy.ar1)

# Create differenced unemployment data using (lag-1)
diff.sales = diff(unemploy.ts, lag = 1)
diff.sales
# Use Acf() function to identify autocorrealtion for the model residuals 
autocor <- Acf(diff.sales, lag.max = 12, main = "Autocorrelation for Residuals")




#********************* Part 3 : Run 3 models*******************************
# ************ MODEL 1 : simple exponenthial smoothing (SES) for unemployment data

ses.opt <- ets(train.ts, model = "AAN")
ses.opt

ses.opt.pred <- forecast(ses.opt, h = nValid , level = 0)
ses.opt.pred

# plot ts data, SES and forecast for validation period.
plot(ses.opt.pred, 
     xlab = "Time", ylab = "Unemployment", ylim = c(0, 8000), bty = "l",
     xlim = c(2000, 2019.25), main = "Original Data and SES Forecast, optimal alpha", 
     flty = 2) 
axis(1, at = seq(2000, 2019, 1), labels = format(seq(2000, 2019, 1)) )
lines(ses.opt.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,8000, legend = c("Unemployment Time Series", "SES for Training Period",
                             "SES for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(2016.25 - 3, 2016.25 - 3), c(0, 8000))
lines(c(2019, 2019), c(0, 8000))

#************* MODEL 2 : auto ARIMA

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Unemployment", ylim = c(0, 8000), bty = "l",
     xlim = c(2000, 2019.25), main = "Original Data and auto ARIMA", 
     flty = 2) 
axis(1, at = seq(2000, 2019, 1), labels = format(seq(2000, 2019, 1)) )
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,8000, legend = c("Unemployment Time Series", "Auto ARIMA Model for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2016.25 - 3, 2016.25 - 3), c(0, 8000))
lines(c(2019, 2019), c(0, 8000))

#************ Model 3: Holt-Winter model
## HOLT-WINTER'S EXPONENTIAL SMOOTHING WITH ORIGINAL DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA

# Create Holt-Winter's exponenthial smoothing (HW) for Unemployment data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.

hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot ses predictions for original data, optimal alpha.

plot(hw.ZZZ.pred, 
     xlab = "Time", ylab = "Unemployment", ylim = c(0, 8000), bty = "l",
     xlim = c(2000, 2019.25), main = "Original Data and Holt-Winter model", 
     flty = 2) 
axis(1, at = seq(2000, 2019, 1), labels = format(seq(2000, 2019, 1)) )
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
legend(2000,8000, legend = c("Unemployment Time Series", "H-W Model for Training Period",
                             "Holt-Winter Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2016.25 - 3, 2016.25 - 3), c(0, 8000))
lines(c(2019, 2019), c(0, 8000))

#*********************** PART 4: APPLY 3 models to Entire Dataset *********************

########### 1.SES

ses.all <- ets(unemploy.ts, model = "AAN")
ses.all

# Use forecast() function to make predictions using this ses model with optimal alpha
# and 12 periods into the future.
# Show predictions in tabular format
ses.all.pred <- forecast(ses.all, h = 12, level = 0)
ses.all.pred

# Plot ses predictions for original data and SES.
plot(ses.all.pred, 
     xlab = "Time", ylab = "Unemployment Counts (in 000s)", ylim = c(0, 8000), bty = "l",
     xaxt = "n", xlim = c(2000, 2019.25), lwd = 2,
     main = "Original Data and SES Forecast, Optimal Alpha", 
     flty = 5) 
axis(1, at = seq(2000, 2020, 1), labels = format(seq(2000, 2020, 1)))
lines(ses.all.pred$fitted, col = "red", lwd = 2)
legend(2000,8000, legend = c("Unemployment Series", 
                             "SES Model", "SES Forecast for 12 Future Periods"), 
       col = c("black", "red" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

#********* 2. Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
auto.arima <- auto.arima(unemploy.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

plot(auto.arima.pred, 
     xlab = "Time", ylab = "Unemployed People (in 000s)", ylim = c(0, 8000), bty = "l",
     xaxt = "n", xlim = c(2000, 2019.25), lwd = 2,
     main = "Original Data and Auto ARIMA", 
     flty = 5) 
axis(1, at = seq(2000, 2020, 1), labels = format(seq(2000, 2020, 1)))
lines(auto.arima.pred$fitted, col = "red", lwd = 2)
legend(2000,8000, legend = c("Unemployment Series", 
                             "Auto ARIMA Model", "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "red" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

#**************3. Holt-Winter
# Create Holt-Winter's exponenthial smoothing (hW) for full Amtrak dataset. 
# Use ets() function with model = "ZZZ", to identify the best hw option
# and optimal alpha, beta, & gamma to fit hw for the entire data period.
HW.ZZZ <- ets(unemploy.ts, model = "ZZZ")
HW.ZZZ 

HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = c(0, 95))
HW.ZZZ.pred

plot(HW.ZZZ.pred, 
     xlab = "Time", ylab = "Unemployed People (in 000s)", ylim = c(0, 8000), bty = "l",
     xaxt = "n", xlim = c(2000, 2020), lwd = 2,
     main = "Original Data and Holt-Winter Model", 
     flty = 5) 
axis(1, at = seq(2000, 2020, 1), labels = format(seq(2000, 2020, 1)))
lines(HW.ZZZ.pred$fitted, col = "red", lwd = 2)
legend(2000,8000, legend = c("Unemployment Series", "HW Forecast for 12 Future Periods",
                             "HW Model"), 
       col = c("black", "blue" , "red"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Model comparision
round(accuracy(ses.all.pred$fitted, unemploy.ts), 3)
round(accuracy(auto.arima.pred$fitted, unemploy.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, unemploy.ts), 3)



