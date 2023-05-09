#---------------------------------------
# Fundamental of Data-driven investments
#---------------------------------------

# lecture 1

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