#### NASS Census of Agriculture Data

rm(list = ls())
# Load libraries
my.packages <- c("tidyr", "dplyr", "reshape2")
lapply(my.packages, require, character.only = TRUE)

#### Manipulating Ag Census data from UCB library data lab

# Import Ag Census data file
acdata <- read.csv("data/AG_CO_CA.csv", header = TRUE)
dim(acdata)
summary(acdata)

# Import County names file
coNames <- read.csv("data/AREANAME.csv", header = TRUE)
dim(coNames)
summary(coNames)

# Import Variable descriptions file
varsDesc <- read.csv("data/TEXT_CO.csv", header = TRUE)
summary(varsDesc)

# Import 1982 data
acdata82 <- read.csv("data/AG_CO_CA_1982.csv", header = TRUE)
summary(acdata82)

## Merge data sets together
caNames <- coNames[coNames$ST == 6,]
acdata2 <- merge(acdata, caNames, by = c("ST", "COU", "FIPS"))
acdata2 <- merge(acdata2, varsDesc[,-3], by = "ITEM")
# Difficult to merge 1982 data, just stick with 87 - 97 data
# acdata3 <- merge(acdata2, acdata82, by = c("ST", "COU", "ITEM"))
# acdata3[which(acdata3$D87.x != acdata3$D87.y), c("D87.x", "D87.y")]

## Reshape data set 
acdata3 <- separate(acdata2, AREANAME, c("County", "State"), sep = ", ", extra = "merge")
acdata3 <- separate(acdata3, County, c("County", "Co"), sep = " COUNTY", extra = "merge")
acdata3 <- select(acdata3, -c(Co, State))
acdata3 <- arrange(acdata3, County)
acCodes <- acdata3 %>% select(., c(County, TEXT, ITEM, F97, F92, F87))
acdata4 <- acdata3 %>% select(.,c(County, TEXT, ITEM, D97, D92, D87))
acdata4$County <- factor(acdata4$County)
# Save and read in Ag Census data in long format
# All data are in this data frame, and counties are in a single column, and the 3 years are in separate columns
save(acdata4, file = "Ag_Census_Data_1987-1997_long_version.Rdata")
acdata4 <- load("Ag_Census_Data_1987-1997_long_version.Rdata")

# Put all years in a single column
acdata5 <- gather(acdata4, "year", "value", D97:D87)

## Look into duplication of records
# Some of the variable descriptions are duplicated
acdatadupl <- acdata5[duplicated(acdata5[,c("County", "TEXT", "year")]),]
acdata5[grep("Crawfish", acdata5$TEXT),]
acdata5[acdata5$TEXT == "Hired farm labor (farms)" & acdata5$County == "CALIFORNIA",]
# And some of the duplicated rows don't have equal data
acdata4[acdata4$TEXT == "Cotton (bales)" & acdata4$County == "CALIFORNIA",]

## Finding the good data
# Select duplicated records that have equal values
acdata.goodDupl <- acdata5[duplicated(acdata5[,c("County", "TEXT", "year", "value")]),]
# Select records that aren't duplicated at all
acdata.noDupl <- acdata5[!duplicated(acdata5[,c("County", "TEXT", "year")]),]
acdata6 <- rbind(acdata.goodDupl, acdata.noDupl)
# Which observations are not included in acdata6?
not.incl <- setdiff(acdata5, acdata6)

## TO DO: Figure out what's going on with the duplicated data with discrepancies 

# Make data.frame that has county names as columns
countyData <- spread(acdata6, County, value)
# Modify year column entries to make more sense
countyData$year <- as.character(countyData$year)
countyData$year[countyData$year == "D97"] <- 1997
countyData$year[countyData$year == "D92"] <- 1992
countyData$year[countyData$year == "D87"] <- 1987
countyData$year <- as.numeric(countyData$year)

write.csv(countyData, file = "Ag_Census_Data_1987-1997.csv", row.names = FALSE)





#########################################################################
#### Figures for Bee Project
#### Farm Acreage Values
agdat <- read.csv("ca_ag_acreage.csv", header = TRUE)
summary(agdat)


# Plot total acreage per county
total.acreage <- agdat[agdat$Variable == "All.land.in.farms",]
cols <- c("orange", "purple", "orange", "purple", "orange", "purple", "purple")
ltys <- c(1,2,1,2,1,2,2)

tiff(filename = "Ag Census Acreage.tif")
plot.new()
matplot(x = total.acreage[,2], y = total.acreage[,4:10]/10000, 
        type = "l", lty = 1, lwd = 3, 
        col = cols, cex.axis = 1.3,
        ylim = c(0, 160), xlim = c(1900, 2020), ylab = "", xlab = "", bty = "n")
dev.off()

tiff(filename = "Ag Census Acreage Key.tif")
plot.new()
legend("topleft", c("Urban", "Natural"), lty = 1, lwd = 3, cex = 1.3,
       col = cols[1:2])
dev.off()

library(reshape)
ag2 <- reshape(agdat, direction = )