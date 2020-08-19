library(data.table)
library(incidence)

# Set variables
minsalud_file = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv"
tests_file = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Determinaciones.csv"
cases_file = "data/cases.csv"
deaths_file = "data/deaths.csv"
recoveries_file = "data/recoveries.csv"
update_time = "16:45"

# only run if data needs updating
needs_update = FALSE
if(!file.exists(cases_file)) { 
  needs_update = TRUE
} else {
  csv_data <- read.csv2(cases_file, sep=",")
  last_line <- tail(csv_data,1)
  now_date <- Sys.Date() - 1  
  now_time <- format(as.POSIXct(Sys.time()),"%H:%M")
  # check if file updated yesterday before update time
  if(as.Date(last_line$Date) < now_date) { needs_update = TRUE}
  if (as.Date(last_line$Date) == now_date && now_time < update_time) {
    needs_update = TRUE
  }
} 

if(needs_update) { # only run downloads if we must
  raw_data <- fread(minsalud_file)
  safe_data <- raw_data
  raw_data$fecha_apertura <- as.Date(raw_data$fecha_apertura)
  nat_incidence <- incidence(raw_data$fecha_inicio_sintomas,
                             interval=1,
                             groups = raw_data$clasificacion_resumen)
  
  
  confirmed_cases <- raw_data[clasificacion_resumen %in% c("Confirmado")]
  deaths <- raw_data[fallecido %in% c("SI")]
  today_date <- toString(max(as.Date(raw_data$ultima_actualizacion)))
  recoveries <- raw_data[clasificacion %in% c("Caso confirmado - No activo (por tiempo de evolución)")]
  case_total <- c(today_date,nrow(confirmed_cases))
  death_total <- c(today_date,nrow(deaths))
  recoveries_total <- c(today_date,nrow(recoveries))
  case_header_vector <- c("Date","Cases_National")
  death_header_vector <- c("Date","Deaths_National")
  recoveries_header_vector <- c("Date","Recoveries_National")
  
  provinces <- c("CABA", "Buenos Aires", "Catamarca","Chaco","Chubut",
                 "Córdoba","Corrientes","Entre Ríos", "Formosa","Jujuy",
                 "La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
                 "Río Nego","Salta","San Juan","San Luis","Santa Cruz",
                 "Santa Fe","Santiago del Estero","Tierra del Fuego",
                 "Tucumán","SIN ESPECIFICAR")
  provinces_code <- c("CABA","BuenosAires","Catamarca","Chaco","Chubut",
                      "Cordoba","Corrientes","EntreRios","Formosa","Jujuy",
                      "LaPampa","LaRioja","Mendoza","Misiones","Neuquen",
                      "RioNegro","Salta","SanJuan","SanLuis","SantaCruz",
                      "SantaFe","SantiagoDelEstero","TierraDelFuego",
                      "Tucuman","SinEspecificar")
  
  i=1
  for (this_province in provinces) {
    tmp_cases <- confirmed_cases[carga_provincia_nombre %in% this_province]
    tmp_deaths <- deaths[carga_provincia_nombre %in% this_province]
    tmp_recoveries <- recoveries[carga_provincia_nombre %in% this_province]
    case_header_vector <- c(case_header_vector,
                       paste("Cases_",provinces_code[i],sep=""))
    death_header_vector <- c(death_header_vector,
                             paste("Deaths_",provinces_code[i],sep=""))
    recoveries_header_vector <-c(recoveries_header_vector,
                                 paste("Recoveries_",provinces_code[i],sep=""))
    case_total <- c(case_total,nrow(tmp_cases))
    death_total <- c(death_total,nrow(tmp_deaths))
    recoveries_total <- c(recoveries_total,nrow(tmp_recoveries))
    i=i+1
  }
  
  # quick write
  write_file = matrix(c(cases_file,deaths_file,recoveries_file),nrow=3)
  write_vector = matrix(rbind(case_total,death_total,recoveries_total),nrow=3)
  headers_vector = matrix(rbind(case_header_vector,death_header_vector,recoveries_header_vector),nrow=3)
  for (i in 1:length(write_file)) {
    table_data = as.data.frame(t(write_vector[i,]))
    colnames(table_data) <- headers_vector[i,]
    if(file.exists(write_file[i])) {
      write.table(table_data,write_file[i], sep=",",
                  append=TRUE,row.names = FALSE,col.names = FALSE)
    } else { 
    write.csv(table_data,write_file[i],row.names=FALSE)
    }
  }
}