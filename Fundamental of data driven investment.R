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
