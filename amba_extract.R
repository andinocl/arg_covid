library("zoo")
amba_file = "data/pba.csv"
provinces_file = "data/cases.csv"

amba_headers <- c("AlmiranteBrown", "Avellaneda", "Berazategui", "Berisso", "Brandsen",
                  "Campana", "Canuelas", "Ensenada", "Escobar", "EstebanEcheverria", 
                  "ExaltacionDeLaCruz", "Ezeiza", "FlorencioVarela", "GeneralLasHeras",
                  "GeneralRodriguez", "GeneralSanMartin", "Hurlingham", "Ituzaingo", 
                  "JoseCPaz", "LaMatanza", "Lanus", "LaPlata", "LomasDeZamora", 
                  "Lujan", "MarcosPaz", "MalvinasArgentinas", "Moreno", "Merlo", 
                  "Moron", "Pilar", "PresidentePeron", "Quilmes", "SanFernando", 
                  "SanIsidro", "SanMiguel", "SanVicente", "Tigre", "TresDeFebrero", 
                  "VicenteLopez", "Zarate")

province_data <- read.csv(provinces_file,header=TRUE,sep=",")
amba_data <- read.csv(amba_file,header=TRUE,sep=",")
end_date <- length(province_data$Date)

new_csv_data <- character()
for(i in 166:end_date) { # Start on August 14
  pba_case_data <- province_data$CasesBuenosAires[i]
  amba_case_data <- 0
  amba_tmp <- 0
  if(province_data$Date[i] %in% amba_data$Date) {
    date_row <- which(grepl(province_data$Date[i],amba_data$Date))
    for(m in 1:length(amba_headers)){
      muni_string <- paste(amba_headers[m],"Cases",sep="")
      
      amba_tmp <- amba_tmp + amba_data[date_row,muni_string]
    }
    pba_case_data <- province_data$CasesBuenosAires[i] - amba_tmp
    amba_case_data <- amba_tmp
  }
  new_csv_data <- rbind(new_csv_data,c(province_data$Date[i],province_data$CasesNational[i],
                         province_data$CasesCABA[i],province_data$CasesBuenosAires[i],
                         pba_case_data,amba_case_data))
   
}
colnames(new_csv_data) <- c("Date","National","CABA","PBA","PBA (exlcuding AMBA)","AMBA")

write.table(new_csv_data,"data/amba_pba_sep.csv", sep=",",
            append=FALSE,row.names = FALSE,col.names = TRUE)

## Write Deltas
save_data <- new_csv_data
new_csv_data[new_csv_data == 0] <- NA
new_csv_data[,"AMBA"] <- (na.locf(as.numeric(new_csv_data[,"AMBA"])) + 
                        rev(na.locf(rev(as.numeric(new_csv_data[,"AMBA"])))))/2  
new_csv_data[,5] <- as.numeric(new_csv_data[,4]) - as.numeric(new_csv_data[,6])
delta_csv <- character()
for(i in 2:length(new_csv_data[,1])) {
  delta_csv <- rbind(delta_csv,c(as.numeric(new_csv_data[i,2])-as.numeric(new_csv_data[i-1,2]),
                                 as.numeric(new_csv_data[i,3])-as.numeric(new_csv_data[i-1,3]),
                                 as.numeric(new_csv_data[i,4])-as.numeric(new_csv_data[i-1,4]),
                                 as.numeric(new_csv_data[i,5])-as.numeric(new_csv_data[i-1,5]),
                                 as.numeric(new_csv_data[i,6])-as.numeric(new_csv_data[i-1,6])))
}
