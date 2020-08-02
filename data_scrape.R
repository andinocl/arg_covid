#
# Argentina COVID data scraper
# Chris Andino

library(tidyverse)
library(rvest)
require(RSelenium) #ensure standalone server in same directory
# in terminal, run java -jar selenium-server-standalone.jar -port 5556
library(stringr)
library(gsubfn)

rD <- rsDriver(browser="chrome", port=5554L, chromever="85.0.4183.38")
remDr <- rD$client
#remDr$open()
num_tries = 5


#first scrape national data
i = 1
while (i < num_tries) {
  national <- tryCatch({ 
            source("national_sala_scrape.R") 
            },
    error = function(e) {
      e
      print(e)},
    warning = function(e) {NULL}
  )
  if(inherits(national, "error")) { i = i+1 
  } else { 
    i=num_tries
    national_success = TRUE # marker for future email messages
  }
} 

#next scrape PBA AMBA municipalities
i = 1
while (i < num_tries) {
  pba <- tryCatch({ 
    source("pba_sala_scrape.R") 
  },
  error = function(e) {e},
  warning = function(e) {NULL}
  )
  if(inherits(pba, "error")) { i = i+1 } else { i=num_tries }
} 

'''
Page Reload
 Error: 	 Summary: StaleElementReference
 	 Detail: An element command failed because the referenced element is no longer attached to the DOM.
 	 class: org.openqa.selenium.StaleElementReferenceException
	 Further Details: run errorDetails method 

Selenium Craps Out
Error in menu_element$clickElement() : attempt to apply non-function
'''
# @TODO add console log data

remDr$close()
