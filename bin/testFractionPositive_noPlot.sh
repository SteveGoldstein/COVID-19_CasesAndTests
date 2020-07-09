#!/bin/bash

## this version writes csv but not pdf;
##  use it for faster testing.


## Warning: no warning if files are being overwritten;

date=$1
i=$2

mkdir $date

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byRegion.$i.csv -lag 7  -by Region \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	1> $date/testsAndCases.7DaySmoothing.byRegion.$i.out 2> $date/testsAndCases.7DaySmoothing.byRegion.$i.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byCounty.$i.csv -lag 7  \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	-by County 1> $date/testsAndCases.7DaySmoothing.byCounty.$i.out 2> $date/testsAndCases.7DaySmoothing.byCounty.$i.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.by_herc_region.$i.csv -lag 7  -by herc_region \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	1> $date/testsAndCases.7DaySmoothing.by_herc_region.$i.out 2> $date/testsAndCases.7DaySmoothing.by_herc_region.$i.err &
