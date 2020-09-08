library(pdftools)
library(tidyverse)
library(stringr)
library(lubridate)

rate_file="data/icu_rates.csv"
if(file.exists(rate_file)) {
  existing_data <- read.csv2(rate_file,sep=",")
  last_val <- tail(existing_data$ICUBeds[!is.na(existing_data$ICUBeds)],1)
  last_row <- which(existing_data$ICUBeds==last_val)
  last_date <- as.Date(existing_data[last_row,1]) + 1
  back_icu <- existing_data[1:last_row,]
} else {
  back_icu <- character()
  last_date <- as.Date("2020-06-25")
}
today <- Sys.Date()
dates <- as.factor(seq(from=as.Date(last_date),to=as.Date(today),by=1))

for(this_day in dates) {
  file_date <- paste(str_pad(day(this_day),2,pad="0"),
                     str_pad(month(this_day),2,pad="0"),
                     str_sub(year(this_day),start=-2),sep="-")
  file_prefix="https://www.argentina.gob.ar/sites/default/files/"
  file_suffix=c("-reporte-vespertino-covid-19.pdf","_reporte_vespertino_covid_19.pdf",
                "_reporte_vespertino_covid_19_0.pdf","-reporte-vespertino-covid-19_0.pdf",
                "_reporte_vespertino_covid-19_1.pdf")

  pdf_exists<-FALSE
  for(this_suffix in file_suffix) {
    minsalud_file_test <- paste (file_prefix,file_date,this_suffix,sep="")
    hd <- httr::HEAD(minsalud_file_test)
    if(hd$all_headers[[1]]$status == 200) {
      minsalud_file <- minsalud_file_test
      pdf_exists <- TRUE
    }
  }

  if(pdf_exists) {
    UC_text <- pdf_text(minsalud_file) %>%
      readr::read_lines()
    
    nation_find = str_detect(UC_text,"Nación:")
    nation_row = which(nation_find == TRUE)
    nation_pct = str_replace(str_extract(UC_text[nation_row],"(\\d+,\\d)"),",",".")
    if(is.na(nation_pct)) {
      nation_pct = str_replace(str_extract(UC_text[nation_row],"(\\d{2})%"),"%","")
    }
    amba_find = str_detect(UC_text,"AMBA:")
    amba_row = which(amba_find == TRUE)
    amba_pct = str_replace(str_extract(UC_text[amba_row],"(\\d+,\\d)"),",",".")
    if(is.na(amba_pct)) {
      amba_pct =str_replace(str_extract(UC_text[amba_row],"(\\d{2})%"),"%","") 
    }
    uti_total_find = str_detect(UC_text,"Casos confirmados COVID-19 internados en UTI")
    uti_total_row = which(uti_total_find == TRUE)
    ## will collect whether there's a medial period or not
    icu_total = str_replace(
      str_replace(str_extract(UC_text[uti_total_row],": (\\d+\\.\\d+|\\d+)"),": ",""),
      "\\.","")
    this_row = c(this_day,icu_total,nation_pct,amba_pct)
  }
  else { 
    this_row = c(this_day,"NA","NA","NA") 
  }
  
  back_icu = rbind(back_icu,this_row)
}

colnames(back_icu) <- c("Date","ICUBeds","ICUPctNation","ICUPctAMBA")
write.table(back_icu,col.names=TRUE,row.names = FALSE,
            sep=",",file=rate_file,append=FALSE)




# Was going to use the below on covidstats.com.ar data, but there would be too many errors

# national_prefix = "data/backdate/national/2020-08-17 - DsNación - "
# pba_prefix = "data/backdate/pba/2020-08-17 - DsNación - Buenos Aires - "
# caba_prefix = "data/backdate/pba/2020-08-17 - DsNación - CABA - "
# 
# 
# provinces <- c("CABA", "Buenos Aires", "Catamarca","Chaco","Chubut",
#                "Córdoba","Corrientes","Entre Ríos", "Formosa","Jujuy",
#                "La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
#                "Río Negro","Salta","San Juan","San Luis","Santa Cruz",
#                "Santa Fe","Santiago del Estero","Tierra del Fuego",
#                "Tucumán")
# total_population <- character()
# male_population <- character()
# female_population <- character()
# daily_deaths <- character()
# for (this_province in provinces) {
#   this_file <- paste(national_prefix,this_province,".csv",sep="")
#   raw_data <- read.csv2(this_file,header=FALSE,sep=",")
#   total_population[this_province] <- raw_data[1,2]
#   male_population[this_province] <- raw_data[1,3]
#   female_population[this_province] <- raw_data[1,4]
#   daily_deaths <- cbind(daily_deaths,raw_data[5:234,2])
#   
#   
# }
# 
# amba <- c("Almirante Brown","Avellaneda","Berazategui","Berisso","Brandsen",
#           "Campana","Cañuelas","Ensenada","Escobar","Esteban Echeverría",
#           "Exaltación de la Cruz","Ezeiza","Florencio Varela","General Las Heras",
#           "General Rodríguez","General San Martín","Hurlingham","Ituzaingó",
#           "José C. Paz","La Matanza","La Plata","Lomas de Zamora",
#           "Luján","Marcos Paz","Malvinas Argentinas","Moreno","Merlo",
#           "Morón","Pilar","Presidente Perón","Quilmes","San Fernando",
#           "San Isidro","San Miguel","San Vicente","Tigre","Tres de Febrero",
#           "Vicente López","Zárate","SIN ESPECIFICAR")
# 
# caba_search_vector <- c("01","02","03","04","05","06","07","08",
#                         "09","10","11","12","13","14","15")
# 

