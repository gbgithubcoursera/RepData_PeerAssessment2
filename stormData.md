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
library(xtable)
packages <- c("xtable")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
```

```
## xtable 
##   TRUE
```

```r
# Subset Data into the columns of interrest
healthData <- stormData[, c("EVTYPE", "INJURIES", "FATALITIES")]
#head(healthData)


#healthData <- aggregate.data.frame(x = healthData[, c(2, 3)], by = list(healthData$EVTYPE), FUN = sum)
healthData <- aggregate(x = healthData[, c(2, 3)], by = list(healthData$EVTYPE), FUN = sum)

# Rename the first coluns
colnames(healthData)[1] <- "EVTYPE"

# Add a column for totals
healthData$TOTAL <- healthData$FATALITIES + healthData$INJURIES

healthData <- healthData[order(healthData$TOTAL, decreasing = TRUE), ]

head(healthData)
```

```
##             EVTYPE INJURIES FATALITIES TOTAL
## 834        TORNADO    91346       5633 96979
## 130 EXCESSIVE HEAT     6525       1903  8428
## 856      TSTM WIND     6957        504  7461
## 170          FLOOD     6789        470  7259
## 464      LIGHTNING     5230        816  6046
## 275           HEAT     2100        937  3037
```

```r
# Rename the columns to corresponding event types
row.names(healthData) <- healthData$EVTYPE

# Remove all rows with total of zero
healthData <- healthData[healthData$TOTAL != 0, ]

head(healthData)
```

```
##                        EVTYPE INJURIES FATALITIES TOTAL
## TORNADO               TORNADO    91346       5633 96979
## EXCESSIVE HEAT EXCESSIVE HEAT     6525       1903  8428
## TSTM WIND           TSTM WIND     6957        504  7461
## FLOOD                   FLOOD     6789        470  7259
## LIGHTNING           LIGHTNING     5230        816  6046
## HEAT                     HEAT     2100        937  3037
```

```r
healthTable <- xtable(healthData[1:10, c("INJURIES","FATALITIES")], caption = "Table 1. Top Ten Events")

print(healthTable, floating=FALSE, comment=F,type="html",hline.after=c(0,nrow(healthTable)))
```

```
## <table border=1>
## <tr> <th>  </th> <th> INJURIES </th> <th> FATALITIES </th>  </tr>
##   <tr> <td align="right"> TORNADO </td> <td align="right"> 91346.00 </td> <td align="right"> 5633.00 </td> </tr>
##   <tr> <td align="right"> EXCESSIVE HEAT </td> <td align="right"> 6525.00 </td> <td align="right"> 1903.00 </td> </tr>
##   <tr> <td align="right"> TSTM WIND </td> <td align="right"> 6957.00 </td> <td align="right"> 504.00 </td> </tr>
##   <tr> <td align="right"> FLOOD </td> <td align="right"> 6789.00 </td> <td align="right"> 470.00 </td> </tr>
##   <tr> <td align="right"> LIGHTNING </td> <td align="right"> 5230.00 </td> <td align="right"> 816.00 </td> </tr>
##   <tr> <td align="right"> HEAT </td> <td align="right"> 2100.00 </td> <td align="right"> 937.00 </td> </tr>
##   <tr> <td align="right"> FLASH FLOOD </td> <td align="right"> 1777.00 </td> <td align="right"> 978.00 </td> </tr>
##   <tr> <td align="right"> ICE STORM </td> <td align="right"> 1975.00 </td> <td align="right"> 89.00 </td> </tr>
##   <tr> <td align="right"> THUNDERSTORM WIND </td> <td align="right"> 1488.00 </td> <td align="right"> 133.00 </td> </tr>
##   <tr> <td align="right"> WINTER STORM </td> <td align="right"> 1321.00 </td> <td align="right"> 206.00 </td> </tr>
##    </table>
```

```r
#healthTable

# http://rstudio-pubs-static.s3.amazonaws.com/18170_65d84748b47843c6a01d81b60b09d10c.html
# http://rstudio-pubs-static.s3.amazonaws.com/20314_2cc26c06d7334717acc71dae3bc4c05e.html
# http://rpubs.com/proudindiv/18069
# http://rstudio-pubs-static.s3.amazonaws.com/18276_dbaca171292d46088a5c766d548938f6.html
# http://rstudio-pubs-static.s3.amazonaws.com/25857_77d0955a90b24a4d80e15c6bdca441f9.html
# 
#####################################
### http://rstudio-pubs-static.s3.amazonaws.com/18170_65d84748b47843c6a01d81b60b09d10c.html
# http://www.inside-r.org/packages/cran/xtable/docs/print.xtable
```
## Results
