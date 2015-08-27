library(plyr)
library(lubridate)
library(reshape2)

setwd("F:/DATA/SLBE/R scripts/Shorebirds")
birds <- read.csv("F:/DATA/SLBE/R scripts/SLBE eBirdTool/NewData/ALLbeachebird.csv")
birdtype <- read.csv("BirdTypes.csv")
names(birds)
birds <- join(birds, birdtype, by="COMMON.NAME", type="left")

birds$OBSERVATION.DATE <- as.Date(birds$OBSERVATION.DATE, format = "%Y-%m-%d")

birds$Year <- year(birds$OBSERVATION.DATE)
birds$Month <- month(birds$OBSERVATION.DATE)
birds$DayNum <- yday(birds$OBSERVATION.DATE)

shorebirds <- birds[birds$BirdType=="Shorebird",]
shorebirds$OBSERVATION.COUNT <- as.numeric(as.character(shorebirds$OBSERVATION.COUNT))

shorebirds.count <- shorebirds

table(shorebirds.count$Year)
shorebirds.count <- shorebirds.count[shorebirds.count$Year >= 2012,]

sb.heat <- shorebirds.count[ , c("Month","OBSERVATION.COUNT","COMMON.NAME")]

aqw <- dcast(sb.heat, COMMON.NAME~ Month, fun.aggregate = mean, na.rm = TRUE, value.var = "OBSERVATION.COUNT")

row.names(aqw) <- aqw[,1]
aqw[,1] <- NULL
aqw <- as.matrix(aqw)

nba_heatmap <- heatmap(aqw, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

#just pres/abs
shorebirds$OBSERVATION.COUNT[is.na(shorebirds$OBSERVATION.COUNT)] <- 1
shorebirds$OBSERVATION.COUNT <- ifelse(shorebirds$OBSERVATION.COUNT > 0, 1, 0)

sb.heat <- shorebirds[ , c("Month","OBSERVATION.COUNT","COMMON.NAME")]

aqw <- dcast(sb.heat, COMMON.NAME~ Month, fun.aggregate = sum, na.rm = TRUE, value.var = "OBSERVATION.COUNT")

row.names(aqw) <- aqw[,1]
aqw[,1] <- NULL
aqw <- as.matrix(aqw)

nba_heatmap <- heatmap(aqw, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
