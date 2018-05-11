#loading required packages
library(tidyverse)
library(ggplot2)
library(lattice)
library(lubridate)
#reading the dataframes
setwd("/Users/jomr/Desktop/Coursera/EDA/Final Project/Data")
scc <- readRDS(list.files()[[1]])
summarysccpm25 <-readRDS(list.files()[[2]])
#plot #1
png("plot1.png")
plot(names(totalpm25), totalpm25, xlim=c(1999, 2008), xlab = "year", ylab="total amount of pm 2.5 (tons)")
dev.off()
# plot #2
png("plot2.png")
plot(names(baltimorepm25), baltimorepm25, xlim=c(1999, 2008), xlab = "year", ylab="total amount of pm 2.5 (tons)")
baltmodel <- lm(baltimorepm25~as.numeric(names(baltimorepm25))) #linear model
abline(baltmodel) #plotting the lm
dev.off()
#plot #3
#option1
g <- ggplot(data= baltimore, aes(x=as.factor(year)), y=Emissions)
g+geom_boxplot()+aes(color=type)+coord_cartesian(ylim=c(0,250))
#option2 (log (y))
f <- ggplot(data= baltimore, aes(x=as.factor(year), y=log10(Emissions)))
f+geom_boxplot()+aes(color=type)+labs(x="year", y="log(Emissions)")
#mirando missings por grupo
tapply(zeroobs$fips, zeroobs$type, length)
#plot #4
grepled <- grepl("coal",scc$SCC.Level.Three)
scccodes <- scc$SCC[grepled]
head(scccodes)
coal <- summarysccpm25[summarysccpm25$SCC %in% scccodes]
coal2 <- subset (summarysccpm25[summarysccpm25$SCC %in% scccodes])
