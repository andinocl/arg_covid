# arg_covid
 Hacks to help understand COVID in Argentina

 Major updates August 15-August 25 to use the full Ministry of Health Epi file

TO RUN ON A MAC: 
   1. Download RStudio from rstudio.com
   2. Create project folder in your Mac directory
   3. Download "Data" directory from GitHub to the project directory
   4. Download cron_scraper.R, icu_scraper.R, new_report.Rmd to project directory
   5. Install R plugins required by scripts

@TODO Add a shiny project to make the data easier to explore
@TODO Add plugin checker/installer to R scripts

## new_report.Rmd
R Markdown file to generate number of graphs for National, Buenos Aires, and Province of Buenos Aires, including 14-day trend, doubling time, and estimate of Rt.  Now includes better R estimator with potential for municipal-level tracking.

@TODO: add in vaccination rates
@TODO: consider adding in age/infection graphs (or save for shiny)

## cron_scraper.R
Checks for updates for the national Epi file, cut off at 16:45 local each day. From this data, updates some csv files that collect daily cumulative totals at the national, provincial, and (within CABA and Province of Buenos Aires) municipal/comuna level.

Also collects daily testing data and exports to tests.csv

Designed to be run as a cron task.

Exports data in a compressed data file (CovidEpiFile.RData) stored in a matrix raw_data

@TODO: scrape vaccination data

## icu_scraper.R
Scraper that searches the daily evening PDF reports from the ministry of health to obtain ICU usage.  Parses PDF to get raw number of national beds in use, national rate of use, and provincial rate of use. This differs from the ICU data taken from the Epi file, which only report ICU usage due to COVID, not overall utilization. It is possible that this data is drawn only from the public health system.

# Data files

All are stored in the /data directory

## cases.csv

Daily cumulative confirmed cases, by province, since March 2, 2020

## deaths.csv

Daily cumulative deaths due to COVID, by province, since August 14, 2020 

@TODO Add in data from Ministry of Health pdf reports

## recoveries.csv, icu_active.csv, icu_cumulative.csv

Taken from Epi file since August 14.  Data before that would not be particularly useful on the cumulative data, so these data sets will likely be peak/post-peak only.

## CABA and PBA data

### pba.csv
Collects cumulative case, deaths, recoveries, and ICU cases - as well as daily active ICU cases - at the municipal level for the municipalities in the greater Buenos Aires area (AMBA).  Useful for understanding where cases are growing in the Buenos Aires suburbs. 

### caba.csv

Collects cumulative case, deaths, recoveries, and ICU cases - as well as daily active ICU cases - at the Comuna level in the City of Buenos Aires.  Not particularly useful, as most cases have undefined Comunas.  Data since August 14, 2020.

## Population files

### PopulationCABA.csv, PopulationPBA.csv, PopulationNational.csv

The last Argentine census was taken in 2010.  These data are projections for the year 2020 from the national statistical entity - INDEC - for 1.) the City of Buenos Aires at the Comuna level, 2.) the municipalities of AMBA in Province of Buenos Aires, 3.) each individual province.  

Includes total population and population by gender.



