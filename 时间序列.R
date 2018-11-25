library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(ggplot2)

getSymbols('UNRATENSA',src='FRED',return.class='xts')
ER<-100-UNRATENSA
ER
dimnames(ER)[2]<-'ER'
chartSeries(ER,theme='white')

#数据处理/销量数据处理也能这样
ER.data.frame <- as.data.frame(ER)
ER.data.frame$date <- ymd(rownames(ER.data.frame))

ER.time.series <- ts(ER.data.frame$ER, 
                     start = c(year(min(ER.data.frame$date)),month(min(ER.data.frame$date))),
                     end = c(year(max(ER.data.frame$date)),month(max(ER.data.frame$date))),
                     frequency=12)
ER.time.series

#另一个
getSymbols("DGORDER", src="FRED", return.class = "xts")
DGO <- DGORDER/1000 # convert to billions of dollars
dimnames(DGO)[2] <- "DGO" # use simple name for index
chartSeries(DGO, theme="white") 
DGO.data.frame <- as.data.frame(DGO)
DGO.data.frame$DGO <- DGO.data.frame$DGO
DGO.data.frame$date <- ymd(rownames(DGO.data.frame))
DGO.time.series <- ts(DGO.data.frame$DGO, 
                      start = c(year(min(DGO.data.frame$date)),month(min(DGO.data.frame$date))),
                      end = c(year(max(DGO.data.frame$date)),month(max(DGO.data.frame$date))),
                      frequency=12)

# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
getSymbols("UMCSENT", src="FRED", return.class = "xts")
ICS <- UMCSENT # use simple name for xts object
dimnames(ICS)[2] <- "ICS" # use simple name for index
chartSeries(ICS, theme="white")
ICS.data.frame <- as.data.frame(ICS)
ICS.data.frame$ICS <- ICS.data.frame$ICS
ICS.data.frame$date <- ymd(rownames(ICS.data.frame))
ICS.time.series <- ts(ICS.data.frame$ICS, 
                      start = c(year(min(ICS.data.frame$date)), month(min(ICS.data.frame$date))),
                      end = c(year(max(ICS.data.frame$date)),month(max(ICS.data.frame$date))),
                      frequency=12)

# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
NHS <- HSN1FNSA
dimnames(NHS)[2] <- "NHS" # use simple name for index
chartSeries(NHS, theme="white")
NHS.data.frame <- as.data.frame(NHS)
NHS.data.frame$NHS <- NHS.data.frame$NHS
NHS.data.frame$date <- ymd(rownames(NHS.data.frame))
NHS.time.series <- ts(NHS.data.frame$NHS, 
                      start = c(year(min(NHS.data.frame$date)),month(min(NHS.data.frame$date))),
                      end = c(year(max(NHS.data.frame$date)),month(max(NHS.data.frame$date))),
                      frequency=12)

# define multiple time series object
economic.mts <- cbind(ER.time.series, DGO.time.series, ICS.time.series,
                      NHS.time.series) 
dimnames(economic.mts)[[2]] <- c("ER","DGO","ICS","NHS") # keep simple names 
modeling.mts <- na.omit(economic.mts) # keep overlapping time intervals only

plot(modeling.mts,main="")

ER0 <- mean(as.numeric(window(ER.time.series,start=c(1997,3),end=c(1997,3))))
IER.time.series <- (ER.time.series/ER0) * 100  
IER.time.series

# create new indexed series IDGO using base date March 1997
DGO0 <- mean(as.numeric(window(DGO.time.series,start=c(1997,3),end=c(1997,3))))
IDGO.time.series <- (DGO.time.series/DGO0) * 100  

# create new indexed series INHS using base date March 1997
NHS0 <- mean(as.numeric(window(NHS.time.series,start=c(1997,3),end=c(1997,3))))
INHS.time.series <- (NHS.time.series/NHS0) * 100  

# create a multiple time series object from the index series
economic.mts <- cbind(IER.time.series,
                      IDGO.time.series,
                      ICS.time.series,
                      INHS.time.series) 
dimnames(economic.mts)[[2]] <- c("IER","IDGO","ICS","INHS")
working.economic.mts <- na.omit(economic.mts)
working.economic.mts

library(latticeExtra) # package used for horizon plot
print(horizonplot(working.economic.mts, colorkey = TRUE,
                  layout = c(1,4), strip.left = FALSE, origin = 100,
                  ylab = list(rev(colnames(working.economic.mts)), rot = 0, cex = 0.7)) +
        layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white")))

library(reshape2)
df22<-melt(working.economic.mts)
str(df22)

library(ggTimeSeries)
library(viridis)

ggplot_horizon(df22, 'Var1', 'value', vcGroupingColumnNames='Var2') +
  facet_grid(Var2 ~ .) +
  scale_fill_viridis(option="inferno") 

library(forecast) # functions for time series forecasting 

ER.auto.arima.fit <- auto.arima(ER.time.series, d=NA, D=NA, max.p=3, max.q=3,
                                max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
                                start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
                                ic=c("aic"), stepwise=TRUE, trace=FALSE,
                                approximation=FALSE, xreg=NULL,
                                test=c("kpss","adf","pp"), seasonal.test="ocsb",
                                allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(ER.auto.arima.fit))

ER.forecast <- forecast(ER.auto.arima.fit, h=24, level=c(90), 
                              fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot national employment rate time series with two-year forecast 
plot(ER.forecast,main="", ylab="Employment Rate (100 - Unemployment Rate)",
     xlab = "Time", las = 1, lwd = 1.5)


# ARIMA model fit to new home sales
NHS.auto.arima.fit <- auto.arima(NHS.time.series, d=NA, D=NA, max.p=3, max.q=3,
                                 max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
                                 start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
                                 ic=c("aic"), stepwise=TRUE, trace=FALSE,
                                 approximation=FALSE, xreg=NULL,
                                 test=c("kpss","adf","pp"), seasonal.test='ch',
                                 allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(NHS.auto.arima.fit))
# new home sales two-year forecast (horizon h = 24 months) 
NHS.forecast <- forecast(NHS.auto.arima.fit, h=24, level=c(90), 
                               fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot new home sales time series with two-year forecast 
plot(NHS.forecast,main="", ylab="New Homes Sold (millions)",
     xlab = "Time", las = 1, lwd = 1.5)

library(lmtest)
grangertest(ICS~ER,order=3,data=modeling.mts)
