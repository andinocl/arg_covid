setwd("~/arg_covid")
library(data.table)
library(incidence)
library(imputeTS)
library(zoo)
library(anytime)

# @TODO Add currently in hospital?
# @TODO Add vaccine scrape datatable


# initiate logs and controls
log_file = "data/log_file.csv"
control_file = "data/data_control.csv"
if(file.exists(control_file)) {
  control_table <- read.csv2(control_file,header=TRUE,sep=",")
} else {
  control_table <- c()
  control_table["content_length"] <- 0
  control_table["epi_rows"] <- 0
  control_table["test_rows"] <- 0
  control_table["last_date"] <- "2020-01-01"
  control_table["zip_last_modified"] <- "2020-01-01"
}


# Set variables
get_zips <- TRUE  # Set to true if downloading, false if using downloaded unzipped files
erase_zips <- FALSE # No need to edit
download_dir <- "~/Downloads"


if(get_zips) {
  cases_zip = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip"
  tests_zip = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Determinaciones.zip"
  vaccines_zip = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19VacunasAgrupadas.csv.zip"
  file_response = httr::HEAD(cases_zip)
  zip_status = httr::headers(file_response)
  if(zip_status$`content-type`=="application/zip") {
    if(as.POSIXct(anytime(zip_status$`last-modified`)) > as.POSIXct(control_table$last_run)) {
      temp_cases <- tempfile()
      download.file(cases_zip,temp_cases)
      unzip(temp_cases,exdir=download_dir,overwrite=TRUE)
      unlink(temp_cases)
      minsalud_file = "~/Downloads/Covid19Casos.csv"
      csv_info <- file.info(minsalud_file)
      file_length <- csv_info["size"]
      temp_tests <- tempfile()
      download.file(tests_zip,temp_tests)
      unzip(temp_tests,exdir=download_dir,overwrite=TRUE)
      unlink(temp_tests)
      tests_data_file = "~/Downloads/Covid19Determinaciones.csv"
      temp_vaccines <- tempfile()
      download.file(vaccines_zip,temp_vaccines)
      unzip(temp_vaccines,exdir=download_dir,overwrite=TRUE)
      unlink(temp_vaccines)
      vaccine_data_file = "~/Downloads/Covid19VacunasAgrupadas.csv"
      zip_okay <- TRUE
      erase_zips <- TRUE
      control_table["zip_last_modified"] <- anytime(zip_status$`last-modified`)
    } else {
      control_table["log_message"] <- "Zip file not updated since last run"
      zip_okay <- FALSE
    }
  } else {
    control_table["log_message"] <- "Cases zip file not found."
    zip_okay <- FALSE
  }
} else { # old download/filechooser

    minsalud_file = file.choose()
    chosen_file_info <- file.info(minsalud_file)
    file_length <- chosen_file_info["size"]
    tests_data_file = file.choose()
    vaccine_data_file = file.choose()
  
}

cases_file = "data/cases.csv"
deaths_file = "data/deaths.csv"
recoveries_file = "data/recoveries.csv"
icu_cumulative_file ="data/icu_cumulative.csv"
icu_active_file = "data/icu_active.csv"
tests_file = "data/tests.csv"
pba_file ="data/conurbano.csv"
caba_file = "data/caba.csv"
vaccine_file = "data/vaccines.csv"
vaccine_df_file = "data/vaccines_df.csv"

deltas_case_file = "data/cases_deltas.csv"
deltas_deaths_file = "data/deaths_deltas.csv"
deltas_pba_file = "data/conurbano_deltas.csv"
deltas_test_file = "data/test_deltas.csv"
deltas_vaccine_file = "data/vaccines_deltas.csv"

update_time = "18:45"

covid_deltas <- function(orig_df, nas = "avg", start_date = "2020-03-02") {
  # return a new dataframe containing the deltas from cumulative data
  
  #DEBUG
  #orig_df <- case_data
  #start_date <- "2020-03-02"
  #nas <- "avg"
  
  ## Trim frame for the start_date
  if(start_date %in% orig_df$Date) { 
    start_frame <- which(orig_df$Date == start_date)
    orig_df <- orig_df[start_frame:nrow(orig_df),]
  }
  
  ## Ensure all dates in range have a row
  z <- read.zoo(orig_df,row.names=orig_df$Date)
  gz <- zoo(,seq(as.Date(min(time(z)),"%Y-%m-%d"),as.Date(max(time(z)),"%Y-%m-%d"),by="day")) # get time series
  #gz <- zoo(as.Date(seq(min(time(z)),max(time(z)),by="day"),"%Y-%m-%d"))
  cumulative_df <- merge(z,gz)
  
  ## Fill missing data
  # Average the NAs based on trailing data
  if(nas == "avg") {

    for(i in 1:ncol(cumulative_df)) {
      # cumulative_df <- floor((na.locf(cumulative_df) + rev(na.locf(rev(cumulative_df))))/2)
      cumulative_df <- round(na_interpolation(cumulative_df,option="linear"),digits=0)
    }
  }
  
  # Replace NAs with zeros 
  if(nas == "zero") {
    cumulative_df[is.na(cumulative_df)] <- 0
  }
  
  ## Get deltas
  delta_zoo <- diff(cumulative_df)
  delta_frame <- data.frame("Date"=index(cumulative_df[-1,]),as.data.frame(delta_zoo))
  return(delta_frame)
}

# only run if data needs updating
needs_update = FALSE

if(zip_okay) {
  if(!file.exists(cases_file)) { 
    needs_update = TRUE # Automatically run first time installed
  } else {
    csv_data <- read.csv2(cases_file, sep=",")
    last_line <- tail(csv_data,1)
    now_date <- Sys.Date() - 1  
    
    now_time <- format(as.POSIXct(Sys.time()),"%H:%M")
  
    # check if file updated yesterday before update time
    if ((as.Date(last_line$Date) == now_date && now_time > update_time) || 
        (as.Date(last_line$Date) < now_date)) { # Don't read file unless it should have been updated
      if (control_table["content_length"] < file_length) {
        raw_data <- fread(minsalud_file) # don't read data if the headers say it's smaller
        if((max(raw_data$ultima_actualizacion) != last_line$Date) && 
          nrow(raw_data[clasificacion_resumen %in% c("Confirmado")]) != last_line$CasesNational){ #see if file actually updated
          
          control_table["last_run"] <- format(as.POSIXct(Sys.time(),tz="America/Buenos_Aires"))
          control_table["content_length"] <- file_length
          control_table["epi_rows"] <- nrow(raw_data)
          control_table["last_date"] <- format(as.Date(max(raw_data$ultima_actualizacion)))
          needs_update = TRUE
          
        }
      } else {
        control_table["log_message"] <- paste("Update file is smaller than previous day's file.",
                                          file_length,sep=" ")
      }
    } else {
      control_table["log_message"] <- "Update already run on current data."
    }
  } 
  
  if(needs_update) { # only run downloads if we must
    vaccine_file_try <- try(read.csv2(vaccine_data_file,sep=","),silent=TRUE)
    if(class(vaccine_file_try) != "try-error") {
      vaccine_data_file_exists <- TRUE
    } else {
      vaccine_data_file_exists <- FALSE
      control_table["log_message"] <- "Vaccine file did not exist."
    }
    if(vaccine_data_file_exists) {
      vaccine_df_data <- read.csv2(vaccine_data_file, sep=",")
      # @TODO add better error correction for backloading old datafiles
      #now_date <- "2020-02-12" # comment this out for normal run
      vaccine_df_data <- cbind(now_date,vaccine_df_data) 
      colnames(vaccine_df_data) <- c("date","indec_code","province","vaccine","first_dose_qty","second_dose_qty")
    }
    
    raw_data$residencia_provincia_nombre[(raw_data$residencia_provincia_nombre == "SIN ESPECIFICAR")] <- raw_data$carga_provincia_nombre[(raw_data$residencia_provincia_nombre == "SIN ESPECIFICAR")]
    record_date <- toString(max(as.Date(raw_data$ultima_actualizacion)))
    raw_tests <- fread(tests_data_file)
    raw_tests$positivos[is.na(raw_tests$positivos)] <- 0
    raw_tests$provincia[(raw_tests$localidad) == "CIUDAD DE BUENOS AIRES"] <- "CABA"
    control_table["test_rows"] <- nrow(raw_tests)
    
    cases <- raw_data[clasificacion_resumen %in% c("Confirmado")]
    deaths <- cases[fallecido %in% c("SI")]
    recoveries <- raw_data[clasificacion %in% c("Caso confirmado - No activo (por tiempo de evolución)",
                                                "Caso confirmado por laboratorio - No Activo por criterio de laboratorio")]
    icu_cases <- cases[cuidado_intensivo %in% c("SI")]
    icu_active <- icu_cases[clasificacion %in% c("Caso confirmado - Activo",
                                                 "Caso confirmado por laboratorio - Activo Internado",
                                                 "Caso confirmado por criterio clínico - epidemiológico -  Activo internado"
                                                 )]
    
    
    case_total <- c(record_date,nrow(cases))
    death_total <- c(record_date,nrow(deaths))
    recoveries_total <- c(record_date,nrow(recoveries))
    icu_cases_total <- c(record_date,nrow(icu_cases))
    icu_active_total <- c(record_date,nrow(icu_active))
    tests_total <- c(record_date,sum(raw_tests$total),sum(raw_tests$positivos))
    
    case_header_vector <- c("Date","CasesNational")
    death_header_vector <- c("Date","DeathsNational")
    recoveries_header_vector <- c("Date","RecoveriesNational")
    icu_cases_header_vector <- c("Date","ICUTotal")
    icu_active_header_vector <- c("Date","ICUActive")
    tests_header_vector <- c("Date","TotalTests","PositiveTests")
    
    #
    # NATIONAL DATA
    #
  
    
    provinces <- c("CABA", "Buenos Aires", "Catamarca","Chaco","Chubut",
                   "Córdoba","Corrientes","Entre Ríos", "Formosa","Jujuy",
                   "La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
                   "Río Negro","Salta","San Juan","San Luis","Santa Cruz",
                   "Santa Fe","Santiago del Estero","Tierra del Fuego",
                   "Tucumán")
    provinces_code <- c("CABA","BuenosAires","Catamarca","Chaco","Chubut",
                        "Cordoba","Corrientes","EntreRios","Formosa","Jujuy",
                        "LaPampa","LaRioja","Mendoza","Misiones","Neuquen",
                        "RioNegro","Salta","SanJuan","SanLuis","SantaCruz",
                        "SantaFe","SantiagoDelEstero","TierraDelFuego",
                        "Tucuman")
    
    i=1
    for (this_province in provinces) {
      tmp_cases <- cases[residencia_provincia_nombre %in% this_province]
      tmp_deaths <- deaths[residencia_provincia_nombre %in% this_province]
      tmp_recoveries <- recoveries[residencia_provincia_nombre %in% this_province]
      tmp_icu_cases <- icu_cases[residencia_provincia_nombre %in% this_province]
      tmp_icu_active <- icu_active[residencia_provincia_nombre %in% this_province]
      tmp_tests <- raw_tests[provincia %in% this_province]
      
      case_header_vector <- c(case_header_vector,
                         paste("Cases",provinces_code[i],sep=""))
      death_header_vector <- c(death_header_vector,
                               paste("Deaths",provinces_code[i],sep=""))
      recoveries_header_vector <-c(recoveries_header_vector,
                                   paste("Recoveries",provinces_code[i],sep=""))
      icu_cases_header_vector <- c(icu_cases_header_vector,
                                   paste("ICU_Cases",provinces_code[i],sep=""))
      icu_active_header_vector <- c(icu_active_header_vector,
                                    paste("ICU_Active",provinces_code[i],sep=""))
      tests_header_vector <- c(tests_header_vector,
                               paste("TotalTests",provinces_code[i],sep=""),
                               paste("PositiveTests",provinces_code[i],sep=""))
      
      case_total <- c(case_total,nrow(tmp_cases))
      death_total <- c(death_total,nrow(tmp_deaths))
      recoveries_total <- c(recoveries_total,nrow(tmp_recoveries))
      icu_cases_total <- c(icu_cases_total,nrow(tmp_icu_cases))
      icu_active_total <- c(icu_active_total,nrow(tmp_icu_active))
      tests_total <- c(tests_total,sum(tmp_tests$total),sum(tmp_tests$positivos))
      
      i=i+1
    }
    
    #
    # PBA Data
    #
    
    amba <- c("Almirante Brown","Avellaneda","Berazategui","Berisso","Brandsen",
              "Campana","Cañuelas","Ensenada","Escobar","Esteban Echeverría",
              "Exaltación de la Cruz","Ezeiza","Florencio Varela","General Las Heras",
              "General Rodríguez","General San Martín","Hurlingham","Ituzaingó",
              "José C. Paz","La Matanza","La Plata","Lomas de Zamora",
              "Luján","Marcos Paz","Malvinas Argentinas","Moreno","Merlo",
              "Morón","Pilar","Presidente Perón","Quilmes","San Fernando",
              "San Isidro","San Miguel","San Vicente","Tigre","Tres de Febrero",
              "Vicente López","Zárate","SIN ESPECIFICAR")
    amba_headers <- c("AlmiranteBrown", "Avellaneda", "Berazategui", "Berisso", "Brandsen",
                "Campana", "Canuelas", "Ensenada", "Escobar", "EstebanEcheverria", 
                "ExaltacionDeLaCruz", "Ezeiza", "FlorencioVarela", "GeneralLasHeras",
                "GeneralRodriguez", "GeneralSanMartin", "Hurlingham", "Ituzaingo", 
                "JoseCPaz", "LaMatanza", "Lanus", "LaPlata", "LomasDeZamora", 
                "Lujan", "MarcosPaz", "MalvinasArgentinas", "Moreno", "Merlo", 
                "Moron", "Pilar", "PresidentePeron", "Quilmes", "SanFernando", 
                "SanIsidro", "SanMiguel", "SanVicente", "Tigre", "TresDeFebrero", 
                "VicenteLopez", "Zarate","Unspecified")
    
    i = 1
    amba_cases <- cases[residencia_provincia_nombre %in% c("Buenos Aires")]
    amba_deaths <- deaths[residencia_provincia_nombre %in% c("Buenos Aires")]
    amba_recoveries <- recoveries[residencia_provincia_nombre %in% c("Buenos Aires")]
    amba_icu_cases <- icu_cases[residencia_provincia_nombre %in% c("Buenos Aires")]
    amba_icu_active <- icu_active[residencia_provincia_nombre  %in% c("Buenos Aires")]
    amba_tests <- raw_tests[provincia %in% c("Buenos Aires")]
    
    conurbano_cases <- 0
    conurbano_deaths <- 0
    conurbano_recoveries <- 0
    conurbano_icu_cases <- 0
    conurbano_icu_active <- 0
    conurbano_tests_total <- 0
    conurbano_tests_pos <- 0
    
    pba_data <- c(record_date)
    
    pba_headers <- c("Date")
    
    for (this_municipality in amba) { # create AMBA totals sheet
      tmp_cases <- amba_cases[residencia_departamento_nombre %in% this_municipality]
      tmp_deaths <- amba_deaths[residencia_departamento_nombre %in% this_municipality]
      tmp_recoveries <- amba_recoveries[residencia_departamento_nombre %in% this_municipality]
      tmp_icu_cases <- amba_icu_cases[residencia_departamento_nombre %in% this_municipality]
      tmp_icu_active <- amba_icu_active[residencia_departamento_nombre %in% this_municipality]
      tmp_tests <- amba_tests[departamento %in% this_municipality]
      
      pba_headers <- c(pba_headers,
                              paste(amba_headers[i],"Cases",sep=""),
                              paste(amba_headers[i],"Deaths",sep=""),
                              paste(amba_headers[i],"Recoveries",sep=""),
                              paste(amba_headers[i],"ICUActive",sep=""),
                              paste(amba_headers[i],"ICUCases",sep=""),
                              paste(amba_headers[i],"TotalTests",sep=""),
                              paste(amba_headers[i],"PositiveTests",sep=""))
      
      pba_data <- c(pba_data,nrow(tmp_cases),nrow(tmp_deaths),nrow(tmp_recoveries),
                     nrow(tmp_icu_active),nrow(tmp_icu_cases),sum(tmp_tests$total),
                    sum(tmp_tests$positivos))
      
      conurbano_cases <- conurbano_cases + nrow(tmp_cases)
      conurbano_deaths <- conurbano_deaths + nrow(tmp_deaths)
      conurbano_recoveries <- conurbano_recoveries + nrow(tmp_recoveries)
      conurbano_icu_active <- conurbano_icu_active + nrow(tmp_icu_active)
      conurbano_icu_cases <- conurbano_icu_cases + nrow(tmp_icu_cases)
      conurbano_tests_total <- conurbano_tests_total + sum(tmp_tests$total)
      conurbano_tests_pos <- conurbano_tests_pos + sum(tmp_tests$positivos)
      
      i=i+1
    }
    pba_data <- c(pba_data[1],conurbano_cases,conurbano_deaths,conurbano_recoveries,
                  conurbano_icu_active,conurbano_cases,conurbano_tests_total,
                  conurbano_tests_pos,pba_data[2:length(pba_data)])
    pba_headers <- c(pba_headers[1],"ConurbanoCases","ConurbanoDeaths","ConurbanoRecoveries",
                     "ConurbanoICUActive","ConurbanoICUCases","ConurbanoTotalTests",
                     "ConurbanoPositiveTests",pba_headers[2:length(pba_headers)])
    #
    # CABA Cases
    # CABA is COMUNA 1-15 and SIN ESPECIFICAR
    #
    
    caba_headers <- character()
    caba_search_vector <- c("01","02","03","04","05","06","07","08",
                            "09","10","11","12","13","14","15")
    
    caba_cases <- cases[residencia_provincia_nombre %in% c("CABA")]
    caba_deaths <- deaths[residencia_provincia_nombre %in% c("CABA")]
    caba_recoveries <- recoveries[carga_provincia_nombre %in% c("CABA")]
    caba_icu <- icu_cases[carga_provincia_nombre %in% c("CABA")]
    caba_icu_active <- icu_active[carga_provincia_nombre %in% c("CABA")]
    caba_tests <- raw_tests[provincia %in% c("CABA")]
    
    caba_data <- c(record_date)
    caba_headers <- c("Date")
    caba_search <- character()
    caba_paste <- character()
    
    for(comuna in 1:15) {
      caba_search <- c(caba_search,paste("COMUNA ",caba_search_vector[comuna],sep=""))
      caba_paste <- c(caba_paste,paste("Comuna",comuna,sep=""))
    }
    caba_search <- c(caba_search,"SIN ESPECIFICAR")
    caba_paste <- c(caba_paste,"Unspecified")
    
    for(comuna in 1:16) {
      tmp_cases <- caba_cases[residencia_departamento_nombre %in% caba_search[comuna]]
      tmp_deaths <- caba_deaths[residencia_departamento_nombre %in% caba_search[comuna]]
      tmp_recoveries <- caba_recoveries[residencia_departamento_nombre %in% caba_search[comuna]]
      tmp_icu_cases <- caba_icu[residencia_departamento_nombre %in% caba_search[comuna]]
      tmp_icu_active <- caba_icu_active[residencia_departamento_nombre %in% caba_search[comuna]]
      tmp_tests <- raw_tests[departamento %in% caba_search[comuna]]
      
      caba_headers <- c(caba_headers,
                     paste(caba_paste[comuna],"Cases",sep=""),
                     paste(caba_paste[comuna],"Deaths",sep=""),
                     paste(caba_paste[comuna],"Recoveries",sep=""),
                     paste(caba_paste[comuna],"ICU_Cases",sep=""),
                     paste(caba_paste[comuna],"ICU_Active",sep=""),
                     paste(caba_paste[comuna],"TotalTests",sep=""),
                     paste(caba_paste[comuna],"PositiveTests",sep=""))
      
      caba_data <- c(caba_data,nrow(tmp_cases),nrow(tmp_deaths),nrow(tmp_recoveries),
                     nrow(tmp_icu_cases),nrow(tmp_icu_active),sum(tmp_tests$total),
                     sum(tmp_tests$positivos))
    }
    
    # 
    # write files
    #
    if(file.exists(cases_file)) {
      yesterday_cases <- read.csv2(cases_file,sep=",")
      if(as.numeric(last(yesterday_cases$CasesNational)) > as.numeric(case_total[2])) {
        process_writes <- FALSE
        control_table["log_message"] <- "File size smaller than last run."
        write_deltas <- FALSE
      } else {
        process_writes <- TRUE
      }
    }
    if(process_writes) {
      write_file = matrix(c(cases_file,deaths_file,recoveries_file,icu_cumulative_file,
                            icu_active_file),nrow=5)
      write_vector = matrix(rbind(case_total,death_total,recoveries_total,icu_cases_total,
                                  icu_active_total),nrow=5)
      headers_vector = matrix(rbind(case_header_vector,death_header_vector,recoveries_header_vector,
                                    icu_cases_header_vector,icu_active_header_vector),nrow=5)
      for (i in 1:length(write_file)) { #write national files
        table_data = as.data.frame(t(write_vector[i,]))
        colnames(table_data) <- headers_vector[i,]
        if(file.exists(write_file[i])) {
          write.table(table_data,write_file[i], sep=",",
                      append=TRUE,row.names = FALSE,col.names = FALSE)
          write_deltas = TRUE
        } else { 
          write.csv(table_data,write_file[i],row.names=FALSE)
        }
      }
      #
      # @Todo fix this, for whatever reason "ultima_actualizacion" no longer current date
      #
      tests_table = as.data.frame(t(tests_total))
      colnames(tests_table) <- tests_header_vector
      if(file.exists(tests_file)) {
        if(tests_total[1] %in% raw_tests$fecha) { #
        }else{
          write.table(tests_table,tests_file,sep=",",append=TRUE,row.names=FALSE,col.names=FALSE) 
        }
      }else{
        write.csv(tests_table,tests_file,row.names=FALSE)
      }
    
      
      pba_table = as.data.frame(t(pba_data))
      colnames(pba_table) <- pba_headers
      if(file.exists(pba_file)) {
        write.table(pba_table,pba_file,sep=",",append=TRUE,row.names=FALSE,col.names=FALSE) 
      } else {
        write.csv(pba_table,pba_file,row.names=FALSE)
      }
      
      caba_table = as.data.frame(t(caba_data))
      colnames(caba_table) <- caba_headers
      if(file.exists(caba_file)) {
        write.table(caba_table,caba_file,sep=",",append=TRUE,row.names=FALSE,col.names=FALSE) 
      } else {
        write.csv(caba_table,caba_file,row.names=FALSE)
      }
      
      control_data_table = as.data.frame(control_table)
      write.csv(control_data_table,control_file,row.names=FALSE)
      
      if(vaccine_data_file_exists) {
        if(file.exists(vaccine_df_file)) {
          write.table(vaccine_df_data,vaccine_df_file,sep=",",append=TRUE,row.names = FALSE,col.names = FALSE)
        } else {
          write.csv(vaccine_df_data,vaccine_df_file,row.names = FALSE)
        }
      }  
    }
    
    
    #
    #   
    # Add daily delta tables
    #
    if(write_deltas) {
      ## national case deltas
      case_data <- read.table(cases_file,sep=",",header=TRUE)
      case_deltas <- covid_deltas(case_data)
      write.table(case_deltas,deltas_case_file,sep=",",append=FALSE,row.names=FALSE,col.names=TRUE)
      
      ## write deaths deltas
      death_data <- read.table(deaths_file,sep=",",header=TRUE)
      death_deltas <- covid_deltas(death_data)
      write.table(death_deltas,deltas_deaths_file,sep=",",append=FALSE,row.names=FALSE,col.names=TRUE)
      
      ## write PBA deltas
      prov_data <- read.table(pba_file,sep=",",header=TRUE)
      pba_deltas <- covid_deltas(prov_data)
      write.table(pba_deltas,deltas_pba_file,sep=",",append=FALSE,row.names=FALSE,col.names=TRUE)
      
      ## write Test deltas
      test_data <- read.table(tests_file,sep=",",header=TRUE)
      test_deltas <- covid_deltas(test_data)
      write.table(test_deltas,deltas_test_file,sep=",",append=FALSE,row.names=FALSE,col.names=TRUE)
    } ## End write_deltas subroutine
    
    ## 
    # 
    # Save raw_data as an RData object for use in other files doing 
    # incidence-based analysis
    #
    rm(list=setdiff(ls(),c("raw_data","control_file","control_table","log_file","erase_zips")))  ##clear all variables that aren't the raw data file
    save(raw_data,file="CovidEpiFile.RData")
    control_table["log_message"] <- "Success."
    ## Load incidence object
    # set blank diagnostic days to date case opened
    #
    #cases$fecha_diagnostico[(cases$fecha_diagnostico %in% c("")) == TRUE] <- cases$fecha_apertura[cases$fecha_diagnostico %in% c("")]
    #
    #nat_incidence <- incidence(as.Date(cases$fecha_diagnostico),
    #                           interval=1,
    #                           groups = cases$clasificacion_resumen)
                               
    
  }
} else {control_table["log_message"] <- "Unknown Error "}
## Write results to log file
log_data <- as.data.frame(control_table)
log_data$runtime <- Sys.time()
if(file.exists(log_file)) {
  write.table(log_data,log_file,sep=",",append=TRUE,row.names=FALSE,col.names=FALSE) 
} else {
  write.csv(log_data,log_file,row.names=FALSE)
}

if(erase_zips) {
  unlink("~/Downloads/Covid19Casos.csv")
  unlink("~/Downloads/Covid19Determinaciones.csv")
  unlink("~/Downloads/Covid19VacunasAgrupadas.csv")
}
  
