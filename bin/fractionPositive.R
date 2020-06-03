library(dplyr)
library(lubridate)
library(lattice)
library(latticeExtra)
library(ggplot2)

defaultArgs <- list (
  plotFile = NULL,
  outFile =  'positiveTestRate.csv',
  lag = 7,
  inFile = NULL,       ## by-pass download
  
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)

lag <- as.integer(args$lag)
dhsURL <- 'https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.csv'
censusURL <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"

### FIPS is an integer;
##  this should be from Census data file directly;
pop <- read.csv("data/processed/WiscPopulation.csv",stringsAsFactors = FALSE)
## 
if (!is.null(args$inFile)) {
  dataSource <- args$inFile
} else {
  dataSource <- dhsURL 
}

### read data and reformat: 
dhsData <- 
  read.csv(dataSource,stringsAsFactors = FALSE) %>% 
  select(2:7) %>% 
  filter(GEO == "County" | GEO == "State") %>% 
  mutate(LoadDttm = ymd(as.Date(.$LoadDttm))) %>% 
  rename(FIPS = "GEOID") %>% 
  rename(Date = "LoadDttm") %>% 
  rename(County = "NAME")

## enforce monotonicity in positive and negative test results;
dhsData <-  dhsData %>% 
  group_by(County) %>% 
  arrange(desc(Date)) %>% 
  mutate(NEGATIVE = cummin(NEGATIVE)) %>% 
  mutate(POSITIVE = cummin(POSITIVE)) %>% 
  mutate(Tests = NEGATIVE + POSITIVE) %>% 
  rename(Cases = "POSITIVE") %>% 
  select(-c("NEGATIVE")) %>% 
  arrange(Date) 

weeklySmoothed <- 
  dhsData %>% 
  mutate(newCases = Cases - lag(Cases, n=lag,default = NA)) %>% 
  mutate(newTests = Tests - lag(Tests,n = lag, default = NA)) %>% 
  mutate(posFraction = newCases/newTests)

## get population data
censusData <- read.csv(censusURL,stringsAsFactors = FALSE)
censusData <- 
  censusData %>% 
  filter(STNAME == "Wisconsin") %>% 
  select(c("CTYNAME","POPESTIMATE2019")) %>% 
  mutate(CTYNAME = sub(" County","",CTYNAME)) %>% 
  rename(County = "CTYNAME") %>% 
  rename(Population = "POPESTIMATE2019") %>% 
  mutate(County = sub("Wisconsin","WI",County))

weeklySmoothed <-  weeklySmoothed %>% 
  inner_join(censusData,by="County")

#xyplot(posFraction ~ Date,weeklySmoothed, groups=County, type="l")
#xyplot(posFraction ~ Date|ntile(Population,n=4),weeklySmoothed, groups=County, type="l")

#xyplot(posFraction ~ Date|ntile(Population,n=4),
#       weeklySmoothed %>% filter(Date > "2020-05-01"), groups=County, type="l")


dailyFraction <- weeklySmoothed %>% 
  mutate(dailyPos = Cases - lag(Cases,n=1)) %>% 
  mutate(dailyTests = Tests - lag(Tests,n=1)) %>% 
  mutate(dailyFractionPos = dailyPos/dailyTests)


x1 <- xyplot(posFraction ~ Date,
       dailyFraction %>% 
         filter(Date > "2020-05-01") %>% 
         filter(County == "WI"),  
       type=c("l","p")
)

x2 <- xyplot(dailyFractionPos ~ Date,
             
             dailyFraction %>% 
               filter(Date > "2020-05-01") %>% 
               filter(County == "WI"), 
             #type=c("l","p"),
             type="h", lwd = 10,
             col = "red"
)

b1 <- barchart(dailyFractionPos ~ Date,
         
         dailyFraction %>% 
           filter(Date > "2020-05-01") %>% 
           filter(County == "WI"), 
       horizontal = F,
       col = "red",
       y.same = TRUE,
       x.same = TRUE
       )

b2 <- barchart(posFraction ~ Date,
               
               dailyFraction %>% 
                 filter(Date > "2020-05-01") %>% 
                 filter(County == "WI"), 
               horizontal = F,
               col = "blue",
               fill  = F,
               y.same = TRUE,
               x.same = TRUE
)

x1+x2
x2+b1
b1+x2
b1 + b2

#write.csv(weeklySmoothed,"weeklySmoothFractionPositive.csv",quote = F, row.names = F)


d <- dailyFraction %>%
  filter(Date > "2020-03-30") %>%
  filter(County == "WI")

ggplot(d, aes(x=Date, y=posFraction)) + geom_point() +geom_line() 
ggplot(d, aes(x=Date, y=dailyFractionPos)) + geom_point() +geom_line() 

g0 <- ggplot(data=d, aes(x=Date, y=posFraction)) +
    geom_line(data=d,aes(Date,posFraction))
g1 <- ggplot(d, aes(x=Date, y=dailyFractionPos),col = "lightgrey") +
    geom_bar(stat="identity") 
g2 <- g1 +
    geom_point(data=d, aes(x=Date, y=posFraction)) +
    geom_line(data=d,aes(Date,posFraction) )
g0
g1
g2

g2+ annotate("text", x=range(d$Date)[1], y=range(d$posFraction)[2],label = "-- rolling 7 day window", col="red",hjust = 0, vjust = 1, size = 4)
g2+ annotate("text", x= min(d$Date)+1, y = max(d$posFraction), hjust=0,label = "-- rolling 7 day window", col="red", size = 4)

g2+ annotate("text", x= min(d$Date)+1, y = max(d$dailyFractionPos), hjust=0,
             label = "-- rolling 7 day window", col="red", size = 4)


