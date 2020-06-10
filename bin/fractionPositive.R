## Setup ---------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)

defaultArgs <- list (
  plotFile = NULL,
  outFile =  NULL,
  lag = 7,              ## smoothing interval (days)
  posFractionMax = 0.5, ## threshold for truncating y axis on testing plot
  inFile = NULL,        ## hook to by-pass download for functional tests

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

##########  process data ---------------
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

###  output csv and initailize plot -----------
if (!is.null(args$outFile)) {
    write.csv(casesData,args$outFile, quote = FALSE, row.names = FALSE)
}

if (!is.null(args$plotFile)) {
    pdf(args$plotFile)
} else {
    pdf("/dev/null")
}

###  plot function  ----------------------
plotData <- function(county = "WI", 
                     textSize = c(3,8,6),  ## plot label, title, axis label
                     objSize = c(0.2,0.4), ## points and line sizes
                     yMax = as.numeric(args$posFractionMax)   ## upper limit for dailyFractionPos
                     ) {
  ##### subset data
  d <- casesData %>%
    filter(Date >= "2020-05-01") %>%
    filter(County == county)
  
  ### values for title
  firstDate <- min(d$Date)
  lastDate <- max(d$Date)
  cases <- (d %>%  filter(Date == lastDate & County == county))$Cases
  tests <- (d %>%  filter(Date == lastDate & County == county))$Tests
  
  ### Burden thresholds for DHS stages;
  burden14DayPer100k <- c(10,50,100)
  burdenPerDayPer1k <- burden14DayPer100k/14/100
  ##################################################################
  ## Top panel
  ### barchart with daily new cases
  casesPlot <- ggplot(d, aes(x=Date, y=Cases.per1000)) +
    geom_bar(stat="identity", fill = "lightgrey") 
  ## layer with weekly smoothing
  casesPlot <- casesPlot +
    geom_point(data=d, aes(x=Date, y=newCases.per1000), col = "red", size = objSize[1]) +
    geom_line(data=d,aes(Date,newCases.per1000),col = "red", size = objSize[2] )
  
  ## annotate
  casesPlot <- casesPlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$Cases.per1000,na.rm = TRUE), hjust=0, vjust=1,
             label = "Confirmed Cases", size = textSize[1]) +
    annotate("text", x= max(d$Date)-1, y = max(d$Cases.per1000, na.rm =TRUE), hjust=1, vjust = 1,
             label = paste0("-- rolling ",args$lag ," day window"),
             col="red", size = textSize[1]) +
    labs(x = NULL, y ="New Cases per 1000") +
    ggtitle(paste0(county,
                  " (", lastDate, " totals: ", cases, " cases; ", tests, " tests)"
                  )) +
    theme(plot.title = element_text(size = textSize[2]),
          axis.text.x = element_blank(),
          axis.title = element_text(size = textSize[3])
          )
  
  ### Annotate with DHS criteria
  yLimits <-  ggplot_build(casesPlot)$layout$panel_params[[1]]$y.range
  advisoryLevel <- c("Moderate", "Moderately High", "High")
  ncategories <- length(burdenPerDayPer1k)
  ## annotate lowest level
  casesPlot <-  casesPlot +
    annotate("text", x= min(d$Date), 
             #y = mean(c(yLimits[1], burdenPerDayPer1k[1])), 
             y = burdenPerDayPer1k[1], 
             hjust=0, vjust=1,
             label = "Low", size = textSize[1]*0.75,col = "blue")
  burdenPerDayPer1k <- c(burdenPerDayPer1k, yLimits[2]) ## facilitate placement of "High" label;
  
  for(b in 1:ncategories) {
    if (burdenPerDayPer1k[b] < yLimits[2]) {
      casesPlot <- casesPlot +
        geom_hline(yintercept = burdenPerDayPer1k[b], 
                   linetype=4-b, size = 0.2, col = "blue") +
        annotate("text", x= min(d$Date)+b, 
                 y = mean(c(burdenPerDayPer1k[b], burdenPerDayPer1k[b+1])), 
                 hjust=0, vjust= 0.5,
                 label = advisoryLevel[b], size = textSize[1]*0.75,col = "blue")
    }
  }
  casesPlot <- casesPlot +
    geom_vline(xintercept = lastDate-7,linetype = 3, size = 0.2) +
    geom_vline(xintercept = lastDate-14,linetype = 3, size = 0.2)

  ##################################################################
  ## Middle panel  
  ### barchart with daily positive rate
  testingPlot <- ggplot(d, aes(x=Date, y=dailyFractionPos)) +
    geom_bar(stat="identity", fill = "lightgrey")
  
  ### truncate y axis to yMax
  yLimits <-  ggplot_build(testingPlot)$layout$panel_params[[1]]$y.range
  if (yLimits[2] > yMax) {
    expansionFactor <- 1.05  ## determined empirically for this plot; might be incorrect if the plot is tweaked
    testingPlot <- testingPlot +
      coord_cartesian(xlim=c(firstDate,lastDate), 
                      ylim = c(yLimits[1]/expansionFactor, yMax),
                      expand = TRUE)
  } else {
    ## yMax determines placement of text annotation
    yMax <- max(d$dailyFractionPos,na.rm = TRUE)
  }

  ## layer with weekly smoothing
  testingPlot <- testingPlot +
    geom_point(data=d, aes(x=Date, y=posFraction), col = "red", size = objSize[1]) +
    geom_line(data=d,aes(Date,posFraction),col = "red", size = objSize[2] )
  testingPlot <- testingPlot + 
    annotate("text", x= min(d$Date)+1, y = yMax, hjust=0, vjust=0,
             label = "Positive Tests", size = textSize[1]) +
    labs(x = NULL, y ="Fraction positive") +
    theme(axis.title = element_text(size = textSize[3]))  + 
    theme(axis.text.x = element_blank()) +
    ## DHS criteria: 1 and 2 wks ago
    geom_vline(xintercept = lastDate-7,linetype = 3, size = 0.2) +
    geom_vline(xintercept = lastDate-14,linetype = 3, size = 0.2)
  
  
  ##################################################################
  ## Bottom panel
  ### barchart with daily testing rate
  testingVolumePlot <- ggplot(d, aes(x=Date, y=Tests.per1000)) +
    geom_bar(stat="identity", fill = "lightgrey")
    
  ## layer with weekly smoothing
  testingVolumePlot <- testingVolumePlot +
    geom_point(data=d, aes(x=Date, y=newTests.per1000), col = "red", size = objSize[1] ) +
    geom_line(data=d,aes(Date,newTests.per1000),col = "red", size = objSize[2] )
  
  ### add annotations
  testingVolumePlot <- testingVolumePlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$Tests.per1000,na.rm = TRUE), hjust=0, vjust=1,
             label = "Testing Volume", size = textSize[1]) +
    labs(x = NULL, y ="Tests per 1000") +
    theme(axis.title = element_text(size = textSize[3])) +
    ## DHS criteria: 1 and 2 wks ago
    geom_vline(xintercept = lastDate-7,linetype = 3, size = 0.2) +
    geom_vline(xintercept = lastDate-14,linetype = 3, size = 0.2)
  
  margin = theme(plot.margin = unit(c(1,0.5,0,0), "cm"))
  plots <- plot_grid(casesPlot,testingPlot, testingVolumePlot,ncol=1,align="v") +
    margin
  return(plots)
}


###  Generate plots --------------------

### hack:  just the counties in blue on the DHS map
dhsBlue <- c("Polk","Trempealeau","La Crosse","Lafayette","Rock",
             "Walworth","Racine","Kenosha","Jefferson","Waukesha",
             "Milwaukee","Dodge","Ozaukee","Fond du Lac","Sheboygan",
             "Winnebago","Portage","Waupaca","Forest"
             )

#### Sort counties by population in decreasing order
wiCounties <- casesData %>% 
  arrange(desc(Population)) %>% 
  select(County) %>% distinct %>% 
  filter(County %in% dhsBlue) %>% 
  unlist


plotGrobList <- lapply(wiCounties, function(cty) {plotData(cty)})


listIndices <- seq(1,length(wiCounties), by = 2)

for (i in listIndices[-length(listIndices)]) {
    grid.arrange(grobs = plotGrobList[i:(i+1)], ncol=2, nrow=1)
}

## last page might not have ncol*nrow plots  
grid.arrange(grobs = plotGrobList[last(listIndices):length(wiCounties)], ncol=2, nrow=1)
    
dev.off()

#q()

## to do:
##   cache census data?
##  
##  remove y axis label for other columns;
##  print annotations dependent on column in the layout.
##  indicate truncation of y axis with whitespace in a bar?

