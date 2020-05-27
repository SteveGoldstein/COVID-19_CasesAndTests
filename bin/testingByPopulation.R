library(dplyr)
library(lubridate)

### FIPS is an integer;
##  this should be from Census data file directly;
pop <- read.csv("data/processed/WiscPopulation.csv",stringsAsFactors = FALSE)
dhsData <- 
  read.csv("data/raw/DHS_data.csv",stringsAsFactors = FALSE) %>% 
  filter(GEO == "County" | GEO == "State")
names(dhsData)
dim(dhsData)
dhsData.save <- dhsData
d <-  dhsData[,1:7] %>% 
  mutate(LoadDttm = ymd(as.Date(.$LoadDttm))) %>% 
  rename(FIPS = "GEOID") %>% 
  rename(Date = "LoadDttm") %>% 
  rename(Cases = "POSITIVE")
  