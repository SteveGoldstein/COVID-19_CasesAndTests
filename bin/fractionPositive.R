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

plotData <- function(county = "WI", 
                     textSize = c(3,8,6),  ## plot label, title, axis label
                     objSize = c(0.2,0.4), ## points and line sizes
                     yMax = as.numeric(args$posFractionMax)   ## upper limit for dailyFractionPos
                     ) {
  ##### subset data
  d <- casesData %>%
    filter(Date > "2020-03-30") %>%
    filter(County == county)
  
  ### values for title
  firstDate <- min(d$Date)
  lastDate <- max(d$Date)
  cases <- (d %>%  filter(Date == lastDate & County == county))$Cases
  tests <- (d %>%  filter(Date == lastDate & County == county))$Tests

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
    annotate("text", x= min(d$Date)+1, y = yMax, hjust=0, vjust=1,
             label = "Positive Tests", size = textSize[1]) +
    labs(x = NULL, y ="Fraction positive") +
    theme(axis.title = element_text(size = textSize[3]))  + 
    theme(axis.text.x = element_blank())
  
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
    theme(axis.title = element_text(size = textSize[3]))   
  
  margin = theme(plot.margin = unit(c(1,0.5,0,0), "cm"))
  plots <- plot_grid(casesPlot,testingPlot, testingVolumePlot,ncol=1,align="v") +
    margin
  return(plots)
}



#### Sort counties by population in decreasing order
wiCounties <- casesData %>% 
  arrange(desc(Population)) %>% 
  select(County) %>% distinct %>% unlist
plotGrobList <- lapply(wiCounties, function(cty) {plotData(cty)})

## Plot state on first page;
grid.arrange(plotGrobList[[1]])

for (i in seq(2,length(wiCounties), by=4)) {
  grid.arrange(grobs = plotGrobList[i:(i+3)], ncol=2, nrow=2)
}


dev.off()

q()

## to do:
##   cache census data?
##  
##  remove y axis label for other columns;
##  print annotations dependent on column in the layout.
##  indicate truncation of y axis with whitespace in a bar?

