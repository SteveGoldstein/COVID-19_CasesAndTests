#!/bin/bash

## update plots with latest version of DHS data;
##  usage:  bin/dailyUpdate.sh <DATE>

## Warning: no warning if files are being overwritten;

date=$1
i=$2

mkdir $date

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byRegion.$i.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.byRegion.$i.pdf -by Region \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	1> $date/testsAndCases.7DaySmoothing.byRegion.$i.out 2> $date/testsAndCases.7DaySmoothing.byRegion.$i.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byCounty.$i.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.byCounty.$i.pdf \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	-by County 1> $date/testsAndCases.7DaySmoothing.byCounty.$i.out 2> $date/testsAndCases.7DaySmoothing.byCounty.$i.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.by_herc_region.$i.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.by_herc_region.$i.pdf -by herc_region \
	-inFile data/raw/DHS_data.2020-06-16.csv \
	1> $date/testsAndCases.7DaySmoothing.by_herc_region.$i.out 2> $date/testsAndCases.7DaySmoothing.by_herc_region.$i.err &
