library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

defaultArgs <- list (
  plotFile = NULL,
  outFile =  NULL,
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

dhsData <-  dhsData %>% 
  inner_join(censusData,by="County")

casesData <- 
  dhsData %>% 
  mutate(newCases = Cases - lag(Cases, n=lag,default = NA)) %>% 
  mutate(newTests = Tests - lag(Tests,n = lag, default = NA)) %>% 
  mutate(posFraction = newCases/newTests) %>% 
  mutate(newCases.per1000 = newCases/Population*1000/lag ) %>% 
  mutate(newTests.per1000 = newTests/Population*1000/lag )

casesData <- casesData %>% 
  mutate(dailyPos = Cases - lag(Cases,n=1)) %>% 
  mutate(dailyTests = Tests - lag(Tests,n=1)) %>% 
  mutate(dailyFractionPos = dailyPos/dailyTests) %>% 
  mutate(Cases.per1000 = dailyPos/Population*1000) %>% 
  mutate(Tests.per1000 = dailyTests/Population*1000)


if (!is.null(args$outFile)) {
    write.csv(casesData,args$outFile, quote = FALSE, row.names = FALSE)
}

if (!is.null(args$plotFile)) {
    pdf(args$plotFile)
} else {
    pdf("/dev/null")
}



plotData <- function(county = "WI") {
  d <- casesData %>%
    filter(Date > "2020-03-30") %>%
    filter(County == county)
  
  ### values for title
  lastDate <- max(d$Date)
  cases <- (d %>%  filter(Date == lastDate & County == county))$Cases
  tests <- (d %>%  filter(Date == lastDate & County == county))$Tests

  ### barchart with daily new cases
  casesPlot <- ggplot(d, aes(x=Date, y=Cases.per1000)) +
    geom_bar(stat="identity", fill = "lightgrey") 
  ## layer with weekly smoothing
  casesPlot <- casesPlot +
    geom_point(data=d, aes(x=Date, y=newCases.per1000), col = "red") +
    geom_line(data=d,aes(Date,newCases.per1000),col = "red" )
  
  ## annotate
  casesPlot <- casesPlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$Cases.per1000,na.rm = TRUE), hjust=0, vjust=1,
             label = "Confirmed Cases", size = 6) +
    annotate("text", x= max(d$Date)-1, y = max(d$Cases.per1000, na.rm = TRUE), hjust=1, vjust = 1,
             label = paste0("-- rolling ",args$lag ," day window"),
             col="red", size = 4) +
    labs(x = NULL, y ="New Cases per 1000") +
    ggtitle(paste0(county,
                  # " (", tests, " cumulative tests and ", cases," cases on ", lastDate,")"))
                  " (", lastDate, " totals: ", cases, " cases; ", tests, " tests)"
                  ))
    
  ### barchart with daily positive rate
  testingPlot <- ggplot(d, aes(x=Date, y=dailyFractionPos)) +
    geom_bar(stat="identity", fill = "lightgrey") 
  ## layer with weekly smoothing
  testingPlot <- testingPlot +
    geom_point(data=d, aes(x=Date, y=posFraction), col = "red") +
    geom_line(data=d,aes(Date,posFraction),col = "red" )
  testingPlot <- testingPlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$dailyFractionPos,na.rm = TRUE), hjust=0, vjust=1,
             label = "Positive Tests", size = 6) +
    labs(x = NULL, y ="Fraction positive") 
  
  ### barchart with daily testing rate
  testingVolumePlot <- ggplot(d, aes(x=Date, y=Tests.per1000)) +
    geom_bar(stat="identity", fill = "lightgrey") 
  ## layer with weekly smoothing
  testingVolumePlot <- testingVolumePlot +
    geom_point(data=d, aes(x=Date, y=newTests.per1000), col = "red") +
    geom_line(data=d,aes(Date,newTests.per1000),col = "red" )
  
  ### add annotations
  testingVolumePlot <- testingVolumePlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$Tests.per1000,na.rm = TRUE), hjust=0, vjust=1,
             label = "Testing Volume", size = 6) +
    labs(x = NULL, y ="Tests per 1000")
  
  plots <- grid.arrange(casesPlot,testingPlot, testingVolumePlot)
  return(plots)
}

#### Sort counties by population in decreasing order
wiCounties <- casesData %>% 
  arrange(desc(Population)) %>% 
  select(County) %>% distinct %>% unlist
lapply(wiCounties, function(cty) {plotData(cty)})
dev.off()

q()

## to do:
##   rename data structures;
##   cache census data?
##   all counties (facets or loop -- lapply?)

## title and text: just once;  add text for tests, cases;

