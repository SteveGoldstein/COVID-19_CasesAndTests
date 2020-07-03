## Setup ---------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)

source("lib/plotCasesAndTests.R")
source("lib/processData.R")

defaultArgs <- list (
  plotFile = NULL,
  outFile =  NULL,
  lag = 7,              ## smoothing interval (days)
  posFractionMax = 0.5, ## threshold for truncating y axis on testing plot
  inFile = NULL,        ## hook to by-pass download for functional tests
  by = "County",         ## "County" or "Region"

  popGroupingSize = 70000,    ## group counties to get at least N in each group.
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

### read and format data -----------------------------------
dhsData <- getDHS_Data(dataSource)

## get population data
censusData <- getCensusData(censusURL)
dhsRegions <- getRegions("tmp/wi_dph_regions.csv")
adjCounties <- getAdjacentCounties()

## consolidate data into one data frame
dhsData <- dhsData %>%  
  inner_join(censusData,by="County") %>% 
  #inner_join(dhsRegions,by=c("FIPS", "County"))
  inner_join(adjCounties,by=c("FIPS", "County"))


##########  process data ---------------
if (args$by == "County") {
  casesData <- analyzeData(dhsData,lag)
  #### Sort counties by region then population in decreasing order  
  geoAreas <- casesData %>% 
    arrange(Region,desc(Population)) %>% 
    select(!!sym(args$by)) %>% distinct %>% 
    unlist
  
} else {
  casesData <- analyzeDataByRegion(dhsData,lag,geoGrouping=args$by)
  #### Sort regions by population in decreasing order
  geoAreas <- casesData %>% 
    ungroup %>% 
    arrange(desc(Population)) %>% 
    select(!!sym(args$by)) %>% distinct %>% 
    unlist
}

###  output csv and initailize plot -----------
if (!is.null(args$outFile)) {
    write.csv(casesData,args$outFile, quote = FALSE, row.names = FALSE)
}

if (!is.null(args$plotFile)) {
    pdf(args$plotFile)
} else {
    pdf("/dev/null")
}


###  Generate plots --------------------


## task 1:   plot the regions in separate pdfs;
## our just add label to county with region; sort by region then pop;


plotGrobList <- lapply(geoAreas, function(area) {plotData(casesData,area,args$by)})
numPlots <- length(geoAreas)

#plotGrobList <- lapply(wiRegions, function(region) {plotData(regionData,region)})
#numPlots <- length(wiRegions)

listIndices <- seq(1,numPlots, by = 2)
for (i in listIndices[-length(listIndices)]) {
    grid.arrange(grobs = plotGrobList[i:(i+1)], ncol=2, nrow=1)
}

## last page might not have ncol*nrow plots  
grid.arrange(grobs = plotGrobList[last(listIndices):numPlots], ncol=2, nrow=1)
        
dev.off()

q()

## to do:
##   cache census data?
##  
##  remove y axis label for other columns;
##  print annotations dependent on column in the layout.
##  indicate truncation of y axis with whitespace in a bar?

