library(dplyr)
library(lubridate)

defaultArgs <- list (
  plotFile = NULL,
  outFile =  'positiveRates.csv',
  lag = 7,
  inFile = NULL,       ## by-pass download
  
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)
# source("lib/estimate.R")
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

xyplot(posFraction ~ Date,weeklySmoothed, groups=County, type="l")
xyplot(posFraction ~ Date|ntile(Population,n=4),weeklySmoothed, groups=County, type="l")

xyplot(posFraction ~ Date|ntile(Population,n=4),
       weeklySmoothed %>% filter(Date > "2020-05-01"), groups=County, type="l")

write.csv(weeklySmoothed,"weeklySmoothFractionPositive.csv",quote = F, row.names = F)

