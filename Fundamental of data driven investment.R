#---------------------------------------
# Fundamental of Data-driven investments
#---------------------------------------

#----------
# lecture 3
#----------

getwd()
setwd("C:/Users/julia/OneDrive/Desktop/Coursera/Fundamental of data-driven investments")

#data folder 
DIS <- read.csv(file = "C:/Users/julia/OneDrive/Desktop/Coursera/Fundamental of data-driven investments/DIS.csv")
# DIS <- read.csv(file = "./Data/DIS - Copy.csv")
#DIS <- read.csv(file = "./Data/DIS.csv", header = FALSE)


is(DIS$Date)
DIS$date.format <- as.Date(as.character(DIS$Date), format="%Y-%m-%d")
DIS$date.format <- as.Date(as.character(DIS$Date), format="%m/%d/%Y")

is(DIS$date.format)

save(DIS, file = "./RData/DIS.RData")

# lecture 2

getwd()
setwd("C:/Users/yjcla/Documents/R test")
load(file="./RData/DIS.RData")

#calculating daily return

price.t1 <- DIS$Adj.Close[-dim(DIS)[1]]
price.t2 <- DIS$Adj.Close[-1]
daily.return <- (price.t2-price.t1)/price.t1

dim(DIS)[1]
length(daily.return)

DIS$daily.return <- c(NaN, daily.return)

#calculating cumulative return

DIS$cumulative.return <- rep(NaN, dim(DIS)[1])

for (i in 2:dim(DIS)[1]) {
  DIS$cumulative.return[i] <- prod(1+DIS$daily.return[2:i])-1
}

#draw cumulative return
plot(DIS$cumulative.return[-1], main="Cumulative Return", xlab="date", ylab = "cumulative return")
plot(DIS$date.format[-1],DIS$cumulative.return[-1], main="Cumulative Return", xlab="date", ylab = "cumulative return")

#annualized return
(1+DIS$cumulative.return[dim(DIS)[1]])^(1/58)-1

#Partial data

DIS2000 <- DIS[DIS$date.format >= as.Date("2000-01-01"), ]
head(DIS2000)
tail(DIS2000)

save(DIS, file = "./RData/DIS.RData")

#----------
# lecture 4
#----------

setwd("C:/Users/julia/OneDrive/Desktop/Coursera/Fundamental of data-driven investments/")

SP500 <- read.csv(file = "C:/Users/julia/OneDrive/Desktop/Coursera/Fundamental of data-driven investments/SP500.csv")

SP500$annualReturn <- c(NaN, (SP500$SP500level[-1]-SP500$SP500level[-dim(SP500)[1]])/SP500$SP500level[-dim(SP500)[1]])

par(mar = c(5, 5, 3, 5))
plot(SP500$year[-1], SP500$annualReturn[-1],type="l" ,col="blue", main="SP500 dividend yield & annual return",xlab="year", ylab="Annual return")
par(new=TRUE)
plot(SP500$dividendYield[-dim(SP500)[1]],type="l",xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2) 
axis(side = 4)
mtext("Dividend yield", side = 4, line = 3)
legend("topleft", c("annual return", "dividend yield"),
       col = c("blue", "red"), lty = c(1, 2))

cor(SP500$annualReturn[-1], SP500$dividendYield[-dim(SP500)[1]])
cor(SP500$annualReturn[-1], SP500$Peratio[-dim(SP500)[1]])
cor(SP500$annualReturn[-1], SP500$ShillerPEratio[-dim(SP500)[1]])
cor(SP500$annualReturn[-1], SP500$X10yearTyield[-1])

#----------
# lecture 5
#----------

summary(SP500[, c("annualReturn", "dividendYield")])

summary(SP500$annualReturn[SP500$annualReturn > 0])
summary(SP500$annualReturn[SP500$annualReturn < 0])


hist(SP500$annualReturn, main="S&P500 Annual Return Histogram")

qqnorm(SP500$annualReturn, 
       ylab="S&P500 Annual Return", 
       xlab="Normal Scores", 
       main="S&P500 annual return normal probability plot") 
qqline(SP500$annualReturn)


DY.lm <- lm(SP500$annualReturn[-1] ~ SP500$dividendYield[-dim(SP500)[1]])
DY.lm.summary <- summary(DY.lm)

names(DY.lm.summary)
DY.lm.summary$coefficients
DY.lm.summary$coefficients[2,]
data.frame((DY.lm.summary$coefficients[2,]),DY.lm.summary$adj.r.squared) 
DY.output <- data.frame(t(DY.lm.summary$coefficients[2,]),DY.lm.summary$adj.r.squared) 
DY.output
colnames(DY.output) <- c("DY coefficient","std.error","t.value","p.value","adj.rsquared")
DY.output

save(SP500, file = "C:/Users/julia/OneDrive/Desktop/Coursera/Fundamental of data-driven investments/SP500.RData")

#----------
# lecture 6
#----------

#dividendYield > 0.03

SP500$port <- 0.6*SP500$annualReturn + 0.4*SP500$X10yearTyield
SP500$indicator <- SP500$dividendYield > 0.03

SP500$strat <- rep(0, dim(SP500)[1]) 

for (i in 1:(dim(SP500)[1]-1)) {
  if (SP500$dividendYield[i] > 0.03) {
    SP500$strat[i+1] <- 0.8*SP500$annualReturn[i+1]+0.2*SP500$X10yearTyield[i+1]
  } else {SP500$strat[i+1] <- 0.6*SP500$annualReturn[i+1]+0.4*SP500$X10yearTyield[i+1]
  }
}

summary(SP500[,c("port","strat")])

SP500$port.cr <- rep(NaN, dim(SP500)[1])
SP500$strat.cr <- rep(NaN, dim(SP500)[1])

for (i in 2:dim(SP500)[1]) {
  SP500$port.cr[i] <- prod(1+SP500$port[2:i])-1
  SP500$strat.cr[i] <- prod(1+SP500$strat[2:i])-1
}

plot(SP500$year[-1],SP500$port.cr[-1],type="l",col="blue", main="Cumulative Return", xlab="date", ylab = "cumulative return")
lines(SP500$year[-1],SP500$strat.cr[-1], type="l", col="red")

#----------
# lecture 7
#----------

installed.packages()
installed.packages()[,1]


if(!("quantmod" %in% as.character(installed.packages()[,1])))
{ install.packages("quantmod") } 

library(quantmod)

getSymbols(c("QQQ"),from = "2010-01-01",
           periodicity = "monthly",auto.assign=TRUE)
head(QQQ)
is(QQQ)

getSymbols(c("IVV","IDEV","IUSB","IEMG","IAGG","IJH","IJR")
           ,from = "2012-01-01", to = "2019-12-31", periodicity = "daily")


IVV

index(IVV)
is(index(IVV))

dailyReturn(IVV$IVV.Adjusted)

hist.return <- merge(dailyReturn(IVV$IVV.Adjusted),
                     dailyReturn(IDEV$IDEV.Adjusted),
                     dailyReturn(IUSB$IUSB.Adjusted),
                     dailyReturn(IEMG$IEMG.Adjusted),
                     dailyReturn(IAGG$IAGG.Adjusted),
                     dailyReturn(IJH$IJH.Adjusted),
                     dailyReturn(IJR$IJR.Adjusted)) 
colnames(hist.return) <- c("IVV","IDEV","IUSB","IEMG","IAGG","IJH","IJR")

#Daily cov and cor
hist.return.cov <- cov(hist.return,use=c("complete.obs"))
hist.return.cor <- cor(hist.return,use=c("complete.obs"))
hist.return.cor

#annual sd
sqrt(diag(hist.return.cov))*sqrt(250)
sd(hist.return$IDEV, na.rm=TRUE)*sqrt(250)


save(hist.return, file = "./RData/hist.return.RData")
