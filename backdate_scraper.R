national_prefix = "data/backdate/national/2020-08-17 - DsNación - "
pba_prefix = "data/backdate/pba/2020-08-17 - DsNación - Buenos Aires - "
caba_prefix = "data/backdate/pba/2020-08-17 - DsNación - CABA - "


provinces <- c("CABA", "Buenos Aires", "Catamarca","Chaco","Chubut",
               "Córdoba","Corrientes","Entre Ríos", "Formosa","Jujuy",
               "La Pampa","La Rioja","Mendoza","Misiones","Neuquén",
               "Río Negro","Salta","San Juan","San Luis","Santa Cruz",
               "Santa Fe","Santiago del Estero","Tierra del Fuego",
               "Tucumán")
total_population <- character()
male_population <- character()
female_population <- character()
daily_deaths <- character()
for (this_province in provinces) {
  this_file <- paste(national_prefix,this_province,".csv",sep="")
  raw_data <- read.csv2(this_file,header=FALSE,sep=",")
  total_population[this_province] <- raw_data[1,2]
  male_population[this_province] <- raw_data[1,3]
  female_population[this_province] <- raw_data[1,4]
  daily_deaths <- cbind(daily_deaths,raw_data[5:234,2])
  
  
}

amba <- c("Almirante Brown","Avellaneda","Berazategui","Berisso","Brandsen",
          "Campana","Cañuelas","Ensenada","Escobar","Esteban Echeverría",
          "Exaltación de la Cruz","Ezeiza","Florencio Varela","General Las Heras",
          "General Rodríguez","General San Martín","Hurlingham","Ituzaingó",
          "José C. Paz","La Matanza","La Plata","Lomas de Zamora",
          "Luján","Marcos Paz","Malvinas Argentinas","Moreno","Merlo",
          "Morón","Pilar","Presidente Perón","Quilmes","San Fernando",
          "San Isidro","San Miguel","San Vicente","Tigre","Tres de Febrero",
          "Vicente López","Zárate","SIN ESPECIFICAR")

caba_search_vector <- c("01","02","03","04","05","06","07","08",
                        "09","10","11","12","13","14","15")

}