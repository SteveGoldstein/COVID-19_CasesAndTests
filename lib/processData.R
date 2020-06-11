### read and format data -----------------------------------

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
}
