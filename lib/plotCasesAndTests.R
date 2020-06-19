###  plot function  ----------------------
plotData <- function(casesData,
                     county = "WI",
                     geoArea = "County",
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
  cases2wksAgo <- (d %>%  filter(Date == lastDate-14 & County == county))$Cases
  tests <- (d %>%  filter(Date == lastDate & County == county))$Tests
  tests2wksAgo <- (d %>%  filter(Date == lastDate-14 & County == county))$Tests
  recentCases <- cases - cases2wksAgo
  recentTests <- tests - tests2wksAgo
  
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
  
  geoAreaName <- d %>% select(!!sym(geoArea)) %>% head(1) %>% unlist
  if (geoArea == "County") {
    geoAreaName <- paste0(geoAreaName, " (", d$Region[1], ")")
  }
  totals <- paste0( lastDate, " totals: ",
                    cases, " cases; ", tests, " tests"
                  )
  recentTotals <- paste0( "Previous two weeks: ",
                    recentCases, " cases; ", recentTests, " tests"
                    )

  plotTitle <- paste(geoAreaName, totals,recentTotals,sep = "\n")

  casesPlot <- casesPlot + 
    annotate("text", x= min(d$Date)+1, y = max(d$Cases.per1000,na.rm = TRUE), hjust=0, vjust=1,
             label = "Confirmed Cases", size = textSize[1]) +
    annotate("text", x= max(d$Date)-1, y = max(d$Cases.per1000, na.rm =TRUE), hjust=1, vjust = 1,
             label = paste0("-- rolling ",args$lag ," day window"),
             col="red", size = textSize[1]) +
    labs(x = NULL, y ="New Cases per 1000") +
    ggtitle(plotTitle) +
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
    annotate("text", x= min(d$Date)+1, y = yMax, hjust=0, vjust=1,
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
}  ## plotData
