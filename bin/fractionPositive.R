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

smoothed <- 
  dhsData %>% 
  mutate(newCases = Cases - lag(Cases, n=lag,default = NA)) %>% 
  mutate(newTests = Tests - lag(Tests,n = lag, default = NA)) %>% 
  mutate(posFraction = newCases/newTests) %>% 
    mutate(newCases.per1000 = newCases/Population*1000/lag )



dailyFraction <- smoothed %>% 
  mutate(dailyPos = Cases - lag(Cases,n=1)) %>% 
  mutate(dailyTests = Tests - lag(Tests,n=1)) %>% 
  mutate(dailyFractionPos = dailyPos/dailyTests) %>% 
  mutate(Cases.per1000 = dailyPos/Population*1000)


if (!is.null(args$outFile)) {
    write.csv(dailyFraction,args$outFile, quote = FALSE, row.names = FALSE)
}

if (!is.null(args$plotFile)) {
    pdf(args$plotFile)
} else {
    pdf("/dev/null")
}


d <- dailyFraction %>%
  filter(Date > "2020-03-30") %>%
  filter(County == "WI")

### barchart with daily positive rate
g1 <- ggplot(d, aes(x=Date, y=dailyFractionPos)) +
    geom_bar(stat="identity", fill = "lightgrey") 
## layer with weekly smoothing
g2 <- g1 +
    geom_point(data=d, aes(x=Date, y=posFraction), col = "red") +
    geom_line(data=d,aes(Date,posFraction),col = "red" )

### add annotations
countyName <- "WI"
lastDate <- max(d$Date)
cases <- (d %>%  filter(Date == lastDate & County == countyName))$Cases
tests <- (d %>%  filter(Date == lastDate & County == countyName))$Tests
g3 <- g2+ 
  annotate("text", x= min(d$Date)+1, y = max(d$dailyFractionPos), hjust=0,
           label = paste0("-- rolling ",args$lag ," day window"),
                          col="red", size = 4) +
  ylab("Fraction positive") +
  ggtitle(paste0("Testing results for ",countyName,
                 " (", tests, " cumulative tests and ", cases," cases on ", lastDate,")"))

### barchart with daily new cases
h1 <- ggplot(d, aes(x=Date, y=Cases.per1000)) +
  geom_bar(stat="identity", fill = "lightgrey") 
## layer with weekly smoothing
h2 <- h1 +
  geom_point(data=d, aes(x=Date, y=newCases.per1000), col = "red") +
  geom_line(data=d,aes(Date,newCases.per1000),col = "red" )

## annotate
h3 <- h2+ 
  annotate("text", x= min(d$Date)+1, y = max(d$dailyFractionPos), hjust=0,
           label = paste0("-- rolling ",args$lag ," day window"),
           col="red", size = 4) +
  ylab("New Cases per 1000") +
  ggtitle(paste0("Confirmed cases for ",countyName,
                 " (", tests, " cumulative tests and ", cases," cases on ", lastDate,")"))
  #ggtitle(paste0(countyName," (", cases," cases on ", lastDate,")"))
grid.arrange(g3,h3)
dev.off()

q()

## to do:
##   rename data structures;
##   cache census data?
##   all counties (facets or loop -- lapply?)

## title and text: just once;  add text for tests, cases;

