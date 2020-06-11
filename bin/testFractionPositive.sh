#!/bin/sh

### run a single instance; 
i=$1

Rscript --vanilla bin/fractionPositive.R -outFile test/byRegion.$i.csv -plotFile test/byRegion.$i.pdf 1> test/o.$i 2> test/e.$i

## Validation:
## manually diff; 
##     for f in o e; do diff f.$i* f.$j*; done
##     diff test/byRegion.$i.csv test/byRegion.$j.csv
##                    ## where j=i-1;
## check size of pdf and visually inspect
##     ls -l test/byRegion.{$i,$j}.pdf
##     open test/byRegion.$i.pdf
