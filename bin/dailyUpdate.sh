#!/bin/bash

## update plots with latest version of DHS data;
##  usage:  bin/dailyUpdate.sh <DATE>

## Warning: no warning if files are being overwritten;

date=$1

mkdir $date

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byRegion.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.byRegion.pdf -by Region \
	1> $date/testsAndCases.7DaySmoothing.byRegion.out 2> $date/testsAndCases.7DaySmoothing.byRegion.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.byCounty.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.byCounty.pdf \
	-by County 1> $date/testsAndCases.7DaySmoothing.byCounty.out 2> $date/testsAndCases.7DaySmoothing.byCounty.err &

Rscript --vanilla bin/fractionPositive.R -outFile $date/testsAndCases.7DaySmoothing.by_herc_region.csv -lag 7 -plotFile $date/testsAndCases.7DaySmoothing.by_herc_region.pdf -by herc_region \
	1> $date/testsAndCases.7DaySmoothing.by_herc_region.out 2> $date/testsAndCases.7DaySmoothing.by_herc_region.err &
