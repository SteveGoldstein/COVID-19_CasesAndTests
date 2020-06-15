## read and format data -----------------------------------

#############################################
##  fetch and reformat daily DHS data;
getDHS_Data <- function(dataSource) {
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

    return(dhsData)
} ## getDHS_Data

#############################################
## get population data
getCensusData <- function(censusURL) {
    censusData <- read.csv(censusURL,stringsAsFactors = FALSE)
    censusData <- 
        censusData %>% 
        filter(STNAME == "Wisconsin") %>% 
        select(c("CTYNAME","POPESTIMATE2019")) %>% 
        mutate(CTYNAME = sub(" County","",CTYNAME)) %>% 
        rename(County = "CTYNAME") %>% 
        rename(Population = "POPESTIMATE2019") %>% 
        mutate(County = sub("Wisconsin","WI",County))

    return(censusData)
}
#############################################
## partition the counties into 5 regions
getDHS_Regions <- function() {
    dhsRegions <- read.csv("data/raw/wi_dph_regions.csv", stringsAsFactors = FALSE)
    dhsRegions <- dhsRegions %>% 
        rename(FIPS = geoid) %>% 
        mutate(FIPS = as.character(FIPS))
    return(dhsRegions)
} ## getDHS_Regions

#############################################
##  process the data to prep for plotting;
analyzeData <- function(dhsData,lag) {
    casesData <-  dhsData %>% 
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

    return(casesData)
} ## analyzeData 

## aggregate one region -----------
## now a region is defined by DHS region file but
## later it could be adjacent counties.

## analyzeDataByRegion
analyzeDataByRegion <- function(dhsData,lag) {
  ## first aggregate data by region then
  ## hand off to analyzedData()
  d <- dhsData %>% 
    group_by(Region,Date) %>% 
    summarize_at(vars("Cases","Tests","Population"), sum) %>% 
    rename(County = Region)

  return(analyzeData(d,lag))
} ## analyzeDataByRegion
region <- "Southern"
aggregateRegionally <- function(casesData,
                                region = "Southern"
                                ) {
  d <- casesData %>% 
    filter(Region == region)
  return(d)
}

i=0
outFile <- paste("d",i,"csv",sep=".")
write.csv(d,outFile,quote=F,row.names = F)

