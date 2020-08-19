# First scrape national sala web page
# @TODO Prettify headers
# @TODO Check headers against where in csv
national_webpage = "https://coronavirus.msal.gov.ar/publico/d/20as/sala-de-situacion-coronavirus-acceso-publico/d/20as/sala-de-situacion-coronavirus-acceso-publico?orgId=3"
sala_file = "national_sala.csv"


remDr$navigate(national_webpage)
Sys.sleep(10) # let page load

ndata <- read_html(remDr$getPageSource()[[1]])
national_data <- ndata %>%
  html_nodes(".singlestat-panel-value") %>%
  html_text()
national_data_names <- ndata %>% 
  html_nodes("#panel-36 .panel-title-text, 
             #panel-58 .panel-title-text, 
             #panel-56 .panel-title-text, 
             #panel-54 .panel-title-text, 
             #panel-52 .panel-title-text, 
             #panel-50 .panel-title-text, 
             #panel-48 .panel-title-text, 
             #panel-42 .panel-title-text, 
             #panel-44 .panel-title-text, 
             #panel-94 .panel-title-text") %>%
  html_text()

province_headers <-ndata %>%
  html_nodes("#panel-75 td:nth-child(1)") %>%
  html_text()
province_headers[1] <- str_remove(province_headers[1],"ABREVIATURA") # hacky


province_cases <-ndata %>%
  html_nodes("#panel-75 td+ td") %>%
  html_text()
province_cases[1] <- str_remove(province_cases[1],"CONFIRMADOS") #ditto

scraped_data <- c(national_data,province_cases)
data_headers <- c(national_data_names,province_headers) #@TODO translate/prettify


## Open file for writing

if(file.exists(sala_file)) {
  csv_data <- read.csv2(sala_file, sep=",")
  data_check <- which(csv_data$Ultima.actualización.==scraped_data[1])
  if(length(data_check)==0) { #only add if not in
    table_data = as.data.frame(t(scraped_data)) # @TODO Check to make sure headers right
    write.table(table_data,sala_file, sep=",",
                append=TRUE,row.names = FALSE,col.names = FALSE)
  }
  
} else { # file doesn't exist, create a new one
  table_data = as.data.frame(t(scraped_data))
  colnames(table_data) <- data_headers
  write.csv(table_data,sala_file,row.names=FALSE)
}                    


