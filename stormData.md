# Severe Weather Events Impacts on Both Public Health and Economic Problems for Communities and Municipalities
# Synopsis
The report explores the U.S. National Oceanic and Atmospheric Administration's (NOAA) Storm data for the period  year 1950 and end in November 2011.
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data Processing

```r
library(plyr)
stormData <- read.csv("StormData.csv", header=TRUE)

names(stormData)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
dim(stormData)
```

```
## [1] 902297     37
```

```r
#(stormData$EVTYPE)

unique(stormData$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(stormData$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
# head(stormData, 100)
smallData <- stormData[,c(8,23,24)]

head(smallData)
```

```
##    EVTYPE FATALITIES INJURIES
## 1 TORNADO          0       15
## 2 TORNADO          0        0
## 3 TORNADO          0        2
## 4 TORNADO          0        2
## 5 TORNADO          0        2
## 6 TORNADO          0        6
```

```r
tail(smallData)
```

```
##                EVTYPE FATALITIES INJURIES
## 902292 WINTER WEATHER          0        0
## 902293      HIGH WIND          0        0
## 902294      HIGH WIND          0        0
## 902295      HIGH WIND          0        0
## 902296       BLIZZARD          0        0
## 902297     HEAVY SNOW          0        0
```

```r
sortHelper <- function(fieldName, top = 15, dataset = stormData) {

    index <- which(colnames(dataset) == fieldName)

    field <- aggregate(dataset[, index], by = list(dataset$EVTYPE), FUN = "sum")

    names(field) <- c("EVTYPE", fieldName)

    field <- arrange(field, field[, 2], decreasing = T)
    field <- head(field, n = top)
    field <- within(field, EVTYPE <- factor(x = EVTYPE, levels = field$EVTYPE))
    return(field)
}

fatalities <- sortHelper("FATALITIES", dataset = stormData)
fatalities
```

```
##               EVTYPE FATALITIES
## 1            TORNADO       5633
## 2     EXCESSIVE HEAT       1903
## 3        FLASH FLOOD        978
## 4               HEAT        937
## 5          LIGHTNING        816
## 6          TSTM WIND        504
## 7              FLOOD        470
## 8        RIP CURRENT        368
## 9          HIGH WIND        248
## 10         AVALANCHE        224
## 11      WINTER STORM        206
## 12      RIP CURRENTS        204
## 13         HEAT WAVE        172
## 14      EXTREME COLD        160
## 15 THUNDERSTORM WIND        133
```

```r
injuries <- sortHelper("INJURIES", dataset = stormData)
injuries
```

```
##               EVTYPE INJURIES
## 1            TORNADO    91346
## 2          TSTM WIND     6957
## 3              FLOOD     6789
## 4     EXCESSIVE HEAT     6525
## 5          LIGHTNING     5230
## 6               HEAT     2100
## 7          ICE STORM     1975
## 8        FLASH FLOOD     1777
## 9  THUNDERSTORM WIND     1488
## 10              HAIL     1361
## 11      WINTER STORM     1321
## 12 HURRICANE/TYPHOON     1275
## 13         HIGH WIND     1137
## 14        HEAVY SNOW     1021
## 15          WILDFIRE      911
```

```r
rm(stormData)
```


```r
stormData <- read.csv("StormData.csv", header=TRUE)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Transform PROPDMG multiplier # Vertical subset - limit the data to the required fields.
#stormData <- stormData[, c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
# Sorting the property exponent data
stormData$PROPEXP[stormData$PROPDMGEXP == "K"] <- 1000
stormData$PROPEXP[stormData$PROPDMGEXP == "M"] <- 10^6
stormData$PROPEXP[stormData$PROPDMGEXP == ""] <- 1
stormData$PROPEXP[stormData$PROPDMGEXP == "B"] <- 10^9
stormData$PROPEXP[stormData$PROPDMGEXP == "m"] <- 10^6
stormData$PROPEXP[stormData$PROPDMGEXP == "0"] <- 1
stormData$PROPEXP[stormData$PROPDMGEXP == "5"] <- 10^5
stormData$PROPEXP[stormData$PROPDMGEXP == "6"] <- 10^6
stormData$PROPEXP[stormData$PROPDMGEXP == "4"] <- 10000
stormData$PROPEXP[stormData$PROPDMGEXP == "2"] <- 100
stormData$PROPEXP[stormData$PROPDMGEXP == "3"] <- 1000
stormData$PROPEXP[stormData$PROPDMGEXP == "h"] <- 100
stormData$PROPEXP[stormData$PROPDMGEXP == "7"] <- 10^7
stormData$PROPEXP[stormData$PROPDMGEXP == "H"] <- 100
stormData$PROPEXP[stormData$PROPDMGEXP == "1"] <- 10
stormData$PROPEXP[stormData$PROPDMGEXP == "8"] <- 10^8
# give 0 to invalid exponent data, so they not count in
stormData$PROPEXP[stormData$PROPDMGEXP == "+"] <- 0
stormData$PROPEXP[stormData$PROPDMGEXP == "-"] <- 0
stormData$PROPEXP[stormData$PROPDMGEXP == "?"] <- 0
# compute the property damage value
stormData$PROPDMGVAL <- stormData$PROPDMG * stormData$PROPEXP


# Sorting the property exponent data
stormData$CROPEXP[stormData$CROPDMGEXP == "M"]  <- 10^6
stormData$CROPEXP[stormData$CROPDMGEXP == "K"]  <- 1000
stormData$CROPEXP[stormData$CROPDMGEXP == "m"]  <- 10^6
stormData$CROPEXP[stormData$CROPDMGEXP == "B"]  <- 10^9
stormData$CROPEXP[stormData$CROPDMGEXP == "0"]  <- 1
stormData$CROPEXP[stormData$CROPDMGEXP == "k"] <- 1000
stormData$CROPEXP[stormData$CROPDMGEXP == "2"]  <- 100
stormData$CROPEXP[stormData$CROPDMGEXP == ""]   <- 1

# give 0 to invalid exponent data, so they not count in
stormData$CROPEXP[stormData$CROPDMGEXP == "?"] <- 0

# compute the crop damage value
stormData$CROPDMGVAL <- stormData$CROPDMG * stormData$CROPEXP

# aggregate the data by event
fatal   <- aggregate(FATALITIES ~ EVTYPE, data = stormData, FUN = sum)
injury  <- aggregate(INJURIES ~ EVTYPE, data = stormData, FUN = sum)
propdmg <- aggregate(PROPDMGVAL ~ EVTYPE, data = stormData, FUN = sum)
cropdmg <- aggregate(CROPDMGVAL ~ EVTYPE, data = stormData, FUN = sum)



# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
library(xtable)
#packages <- c("xtable")
#sapply(packages, require, character.only = TRUE, quietly = TRUE)

# Subset Data into the columns of interrest
healthData <- stormData[, c("EVTYPE", "INJURIES", "FATALITIES")]
head(healthData)
```

   EVTYPE INJURIES FATALITIES
1 TORNADO       15          0
2 TORNADO        0          0
3 TORNADO        2          0
4 TORNADO        2          0
5 TORNADO        2          0
6 TORNADO        6          0

```r
#healthData <- aggregate.data.frame(x = healthData[, c(2, 3)], by = list(healthData$EVTYPE), FUN = sum)
healthData <- aggregate(x = healthData[, c(2, 3)], by = list(healthData$EVTYPE), FUN = sum)

# Rename the first coluns
colnames(healthData)[1] <- "EVTYPE"

# Add a column for totals
healthData$TOTAL <- healthData$FATALITIES + healthData$INJURIES

healthData <- healthData[order(healthData$TOTAL, decreasing = TRUE), ]

head(healthData)
```

            EVTYPE INJURIES FATALITIES TOTAL
834        TORNADO    91346       5633 96979
130 EXCESSIVE HEAT     6525       1903  8428
856      TSTM WIND     6957        504  7461
170          FLOOD     6789        470  7259
464      LIGHTNING     5230        816  6046
275           HEAT     2100        937  3037

```r
# Rename the columns to corresponding event types
row.names(healthData) <- healthData$EVTYPE

# Remove all rows with total of zero
healthData <- healthData[healthData$TOTAL != 0, ]

head(healthData)
```

                       EVTYPE INJURIES FATALITIES TOTAL
TORNADO               TORNADO    91346       5633 96979
EXCESSIVE HEAT EXCESSIVE HEAT     6525       1903  8428
TSTM WIND           TSTM WIND     6957        504  7461
FLOOD                   FLOOD     6789        470  7259
LIGHTNING           LIGHTNING     5230        816  6046
HEAT                     HEAT     2100        937  3037

```r
healthTable <- xtable(healthData[1:10, c("INJURIES","FATALITIES")], caption = "Table 1. Top Ten Events",digit=c(0,0,0))

#print(healthTable, floating=FALSE, comment=F,type="html",hline.after=c(0,nrow(healthTable)))
print(healthTable,type="html")
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Sun Jan 25 13:01:10 2015 -->
<table border=1>
<caption align="bottom"> Table 1. Top Ten Events </caption>
<tr> <th>  </th> <th> INJURIES </th> <th> FATALITIES </th>  </tr>
  <tr> <td align="right"> TORNADO </td> <td align="right"> 91346 </td> <td align="right"> 5633 </td> </tr>
  <tr> <td align="right"> EXCESSIVE HEAT </td> <td align="right"> 6525 </td> <td align="right"> 1903 </td> </tr>
  <tr> <td align="right"> TSTM WIND </td> <td align="right"> 6957 </td> <td align="right"> 504 </td> </tr>
  <tr> <td align="right"> FLOOD </td> <td align="right"> 6789 </td> <td align="right"> 470 </td> </tr>
  <tr> <td align="right"> LIGHTNING </td> <td align="right"> 5230 </td> <td align="right"> 816 </td> </tr>
  <tr> <td align="right"> HEAT </td> <td align="right"> 2100 </td> <td align="right"> 937 </td> </tr>
  <tr> <td align="right"> FLASH FLOOD </td> <td align="right"> 1777 </td> <td align="right"> 978 </td> </tr>
  <tr> <td align="right"> ICE STORM </td> <td align="right"> 1975 </td> <td align="right"> 89 </td> </tr>
  <tr> <td align="right"> THUNDERSTORM WIND </td> <td align="right"> 1488 </td> <td align="right"> 133 </td> </tr>
  <tr> <td align="right"> WINTER STORM </td> <td align="right"> 1321 </td> <td align="right"> 206 </td> </tr>
   </table>

```r
#healthTable

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
economyData <- stormData[, c("EVTYPE", "PROPDMG", "CROPDMG")]
head(economyData)
```

   EVTYPE PROPDMG CROPDMG
1 TORNADO    25.0       0
2 TORNADO     2.5       0
3 TORNADO    25.0       0
4 TORNADO     2.5       0
5 TORNADO     2.5       0
6 TORNADO     2.5       0

```r
economyData <- aggregate(x = economyData[, c(2, 3)], by = list(economyData$EVTYPE), FUN = sum)
colnames(economyData)[1] <- "EVTYPE"
economyData$TOTAL <- economyData$PROPDMG + economyData$CROPDMG
economyData <- economyData[order(economyData$TOTAL, decreasing = TRUE), ]
row.names(economyData) <- economyData$EVTYPE
economyData <- economyData[economyData$TOTAL != 0, ]
head(economyData)
```

                             EVTYPE   PROPDMG   CROPDMG     TOTAL
TORNADO                     TORNADO 3212258.2 100018.52 3312276.7
FLASH FLOOD             FLASH FLOOD 1420124.6 179200.46 1599325.1
TSTM WIND                 TSTM WIND 1335965.6 109202.60 1445168.2
HAIL                           HAIL  688693.4 579596.28 1268289.7
FLOOD                         FLOOD  899938.5 168037.88 1067976.4
THUNDERSTORM WIND THUNDERSTORM WIND  876844.2  66791.45  943635.6

```r
economyTable <- xtable(economyData[1:10, c("PROPDMG", "CROPDMG")], caption = "Table 2. crop damage",digit=c(0,0,0))
#names(economy.table) <- c("Property Damage", "Crop Damage")
print(economyTable, type = "html")
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Sun Jan 25 13:01:30 2015 -->
<table border=1>
<caption align="bottom"> Table 2. crop damage </caption>
<tr> <th>  </th> <th> PROPDMG </th> <th> CROPDMG </th>  </tr>
  <tr> <td align="right"> TORNADO </td> <td align="right"> 3212258 </td> <td align="right"> 100019 </td> </tr>
  <tr> <td align="right"> FLASH FLOOD </td> <td align="right"> 1420125 </td> <td align="right"> 179200 </td> </tr>
  <tr> <td align="right"> TSTM WIND </td> <td align="right"> 1335966 </td> <td align="right"> 109203 </td> </tr>
  <tr> <td align="right"> HAIL </td> <td align="right"> 688693 </td> <td align="right"> 579596 </td> </tr>
  <tr> <td align="right"> FLOOD </td> <td align="right"> 899938 </td> <td align="right"> 168038 </td> </tr>
  <tr> <td align="right"> THUNDERSTORM WIND </td> <td align="right"> 876844 </td> <td align="right"> 66791 </td> </tr>
  <tr> <td align="right"> LIGHTNING </td> <td align="right"> 603352 </td> <td align="right"> 3581 </td> </tr>
  <tr> <td align="right"> THUNDERSTORM WINDS </td> <td align="right"> 446293 </td> <td align="right"> 18685 </td> </tr>
  <tr> <td align="right"> HIGH WIND </td> <td align="right"> 324732 </td> <td align="right"> 17283 </td> </tr>
  <tr> <td align="right"> WINTER STORM </td> <td align="right"> 132721 </td> <td align="right"> 1979 </td> </tr>
   </table>

```r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(stormData)
```


## Results

```r
# get top10 event with highest fatalities
fatal10 <- fatal[order(-fatal$FATALITIES), ][1:10, ]
fatal10
```

```
##             EVTYPE FATALITIES
## 834        TORNADO       5633
## 130 EXCESSIVE HEAT       1903
## 153    FLASH FLOOD        978
## 275           HEAT        937
## 464      LIGHTNING        816
## 856      TSTM WIND        504
## 170          FLOOD        470
## 585    RIP CURRENT        368
## 359      HIGH WIND        248
## 19       AVALANCHE        224
```

```r
# get top10 event with highest injuries
injury10 <- injury[order(-injury$INJURIES), ][1:10, ]
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)

barplot(fatal10$FATALITIES, las = 3, names.arg = fatal10$EVTYPE, main = "Weather Events With The Top 10 Highest Fatalities", 
    ylab = "number of fatalities", col = "red")
barplot(injury10$INJURIES, las = 3, names.arg = injury10$EVTYPE, main = "Weather Events With the Top 10 Highest Injuries", 
    ylab = "number of injuries", col = "red")
```

![](./stormData_files/figure-html/unnamed-chunk-3-1.png) 

```r
########################################################
# get top 10 events with highest property damage
propdmg10 <- propdmg[order(-propdmg$PROPDMGVAL), ][1:10, ]
# get top 10 events with highest crop damage
cropdmg10 <- cropdmg[order(-cropdmg$CROPDMGVAL), ][1:10, ]
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(propdmg10$PROPDMGVAL/(10^9), las = 3, names.arg = propdmg10$EVTYPE, 
    main = "Top 10 Events with Greatest Property Damages", ylab = "Cost of damages ($ billions)", 
    col = "red")
barplot(cropdmg10$CROPDMGVAL/(10^9), las = 3, names.arg = cropdmg10$EVTYPE, 
    main = "Top 10 Events With Greatest Crop Damages", ylab = "Cost of damages ($ billions)", 
    col = "red")
```

![](./stormData_files/figure-html/unnamed-chunk-3-2.png) 
