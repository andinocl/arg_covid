# arg_covid
 Hacks to help understand COVID in Argentina

## arg_covid.Rmd
R Markdown file to generate number of graphs for National, Buenos Aires, and Province of Buenos Aires, including 14-day trend, doubling time, and estimate of Rt

## data_scrape.R
Wrapper for scrapers on Argentine national ministry of health and Province of Buenos Aires ministry of health "sala de situacion" data.  Designed to be run as a cron job.  

@TODO Better error handling, add in MinSalud daily reports, wikipedia scrape, etc.

### national_sala_scrape.R
Depends on data_scrape for Selenium connection.  Takes 10x national datapoints and then total cases for each province.

### pba_sala_scrape.R
Depends on data_scrape for Selenium connection.  Gets 5x datapoints for full province, then each of the 40 municipalities in Greater Buenos Aires area.  Because the website is slow, takes approx 5 minutes to run.

