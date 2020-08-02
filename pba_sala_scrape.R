# Next hit PBA sala web page
pba_sala_file = "data/pba_sala.csv"
tmp_prefix = "data/tmp/pba_"
pba_webpage = "https://qlik3.ms.gba.gov.ar/sense/app/0a29a121-edef-4cd9-9ffd-fb5e298b5afd"

amba_search <- c("Almirante Brown", "Avellaneda", "Berazat", "Berisso", 
                 "Brandsen", "Campana", "Cañuelas", "Ensenada", "Escobar", 
                 "Esteban", "Exaltación", "Ezeiza", 
                 "Florencio", "Heras", "Rodr", 
                 "Martín", "Hurlingham", "Ituzaingó", "José", 
                 "Matanza", "Lanús", "Plata", "Zamora", 
                 "Luján", "Marcos", "Malvinas", "Moreno", "Merlo", 
                 "Morón", "Pilar", "Presidente", "Quilmes", "Fernando", 
                 "Isidro", "Miguel", "\"San Vicente\"", "Tigre", "Febrero", 
                 "López", "Zárate")
amba <- t(c("AlmiranteBrown", "Avellaneda", "Berazategui", "Berisso", "Brandsen",
            "Campana", "Canuelas", "Ensenada", "Escobar", "EstebanEcheverria", 
            "ExaltacionDeLaCruz", "Ezeiza", "FlorencioVarela", "GeneralLasHeras",
            "GeneralRodriguez", "GeneralSanMartin", "Hurlingham", "Ituzaingo", 
            "JoseCPaz", "LaMatanza", "Lanus", "LaPlata", "LomasDeZamora", 
            "Lujan", "MarcosPaz", "MalvinasArgentinas", "Moreno", "Merlo", 
            "Moron", "Pilar", "PresidentePeron", "Quilmes", "SanFernando", 
            "SanIsidro", "SanMiguel", "SanVicente", "Tigre", "TresDeFebrero", 
            "VicenteLopez", "Zarate"))

colnames(amba) <- amba_search

remDr$navigate(pba_webpage)
Sys.sleep(25) # this page is very slow to load
pbadata <- read_html(remDr$getPageSource()[[1]])

pba_total_data <- pbadata %>%
  html_nodes(".kpi-value .ng-binding") %>%
  html_text()

pba_total_data_headers <- pbadata %>%
  html_nodes(".measure-title .ng-binding") %>%
  html_text()

pba_update <- pbadata %>%
  html_nodes("#sheet-title .ng-binding") %>%
  html_text()

pba_update <- regmatches(pba_update, #extract date
                         regexpr("(\\d+/\\d+/\\d+ \\d+:\\d+:\\d+ \\w+)",
                                 pba_update))

# open csv file to see if date already captured

get_data = TRUE
if(file.exists(pba_sala_file)) {
  csv_data <- read.csv2(pba_sala_file, sep=",")
  data_check <- which(csv_data$LastUpdate==pba_update[1])
  if(length(data_check)!=0) { # Data exists, so exit
    get_data = FALSE
  }
} else { file.create(pba_sala_file) }

if (get_data) {
  muni_data <- character()
  muni_headers <- character()
  
  for (val in amba_search) {
    # wait variables for error catching
    menu_element <- NULL
    search_box <- NULL
    muni_check <- NULL
    map_reload[1] <- c(1)
    
    while(length(map_reload)>0) { # wait until map loads before trying to click menu
      map_reload <- tryCatch({remDr$findElements(using="xpath",
                                                 value="//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'progress', ' ' ))]")},
                             error=function(e){NULL})
    }
    menu_element <- tryCatch({remDr$findElement(using="xpath", 
                                                value="//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'qv-filterpane', ' ' ))]")},
                             error=function(e){NULL})
    Sys.sleep(1)
    menu_element$clickElement()
    
    #search box
    while(length(muni_check)==0) { ## wait until municipalities show up in js menu
      muni_check <- tryCatch({remDr$findElements(using="xpath",
                                                 value="//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'serverOptional', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'ng-scope', ' ' ))]")},
                             error=function(e){NULL})
    }
    
    while(is.null(search_box)){
      search_box <- tryCatch({remDr$findElement(using="xpath",
                                                value="//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'lui-search__input', ' '))]")},
                             error = function(e){NULL})
    }  
    search_box$sendKeysToElement(list(val, key = "enter"))
    Sys.sleep(3) # give data chance to load, annoyingly loads multiple times
    
    #run searches
    muni_totals = pba_total_data
    while (muni_totals[1]==pba_total_data[1] ||
           muni_totals[2]==pba_total_data[2] ||
           muni_totals[3]==pba_total_data[3] ||
           muni_totals[5]==pba_total_data[5]) { # loops until data reloaded
      muni_source <- read_html(remDr$getPageSource()[[1]])
      muni_totals <- muni_source %>%
        html_nodes(".kpi-value .ng-binding") %>%
        html_text()
    }
    
    muni_data_headers <- c(paste(amba[,val],"Cases",sep=""),
                           paste(amba[,val],"Recovered",sep=""),
                           paste(amba[,val],"Deaths",sep=""),
                           paste(amba[,val],"LethalityPct",sep=""),
                           paste(amba[,val],"Tests",sep=""))
    muni_data <- c(muni_data,muni_totals)
    muni_headers <- c(muni_headers,muni_data_headers)
    
    
    
    # clear selection
    clear_search <- remDr$findElement(using="xpath",
                                      value ="//*[contains(concat( ' ', @class, ' '), concat( ' ', 'lui-icon--remove', ' '))]")
    clear_search$clickElement()
    Sys.sleep(2) # give time for data reset before checking if map still exists
  }
  
  # clean data points
  scraped_data <- c(pba_total_data,muni_data)
  cleaned_data <- gsubfn(".",list("."="",","=".","%"=""),scraped_data)
  cleaned_data <- c(pba_update,cleaned_data)
  
  # add this scrape file to tmp folder to prevent/trace future error
  tmp_file_time <- gsubfn(".",list("/"="_",":"="_"),pba_update[1])
  tmp_file = paste(tmp_prefix,tmp_file_time,".csv", sep="")
  data_headers <- c("LastUpdate",pba_total_data_headers,muni_headers)
  table_data <- as.data.frame(t(cleaned_data))
  colnames(table_data) <- data_headers
  write.csv(table_data,tmp_file,row.names=FALSE)
  
  # write file table_data = as.data.frame(t(scraped_data)) # @TODO Check to make sure headers right
  data_headers[1:5] <- c("LastUpdate", "Cases","Recovered","Deaths","LeathalityPct","Tests")
  colnames(table_data) <- data_headers
  write.table(table_data,pba_sala_file, sep=",",
              append=TRUE,row.names = FALSE)
}