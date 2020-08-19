## Wikipedia Scrape

wiki_url = "https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/Argentina_medical_cases"
wiki_edit_url = "https://en.wikipedia.org/w/index.php?title=COVID-19_pandemic_in_Argentina&action=edit"
wiki_age_url = "https://en.wikipedia.org/w/index.php?title=COVID-19_pandemic_in_Argentina"
wiki_csv = "data/wiki.csv"
wiki_age_sex_csv = "data/wiki_age_sex.csv"
wiki_age_sex_table_prefix = "data/wiki_age_sex_"
tmp_wiki_prefix = "data/tmp/wiki_"
first_date = "2020-03-03"
first_icu_date = "2020-06-24"
number_graphs = 3
number_aggregate_cols = 4

remDr$navigate(wiki_url)
Sys.sleep(4)

# table_data
wiki_source <- read_html(remDr$getPageSource()[[1]])
wiki_data <- wiki_source %>%
  html_nodes(".wikitable th+ td , td+ td") %>%
  html_text()
wiki_data <- str_replace_all(wiki_data,"\\[.*\\]","") # remove wiki footnotes
wiki_data <- str_replace_all(wiki_data,"—","0") # remove nil days
wiki_data <- str_replace_all(wiki_data,"\\n","")

#get headers
wiki_provinces <- wiki_source %>%
  html_nodes(".wikitable tr:nth-child(2) th") %>%
  html_text()
wiki_provinces <- str_replace_all(wiki_provinces,"\\[.\\]","") # remove wiki footnotes
wiki_provinces <- str_replace_all(wiki_provinces,"\\n","")
cut <- length(wiki_provinces) - 2
end_col_heads <- c("Total","D","NC","ND")
wiki_provinces <- c(wiki_provinces[1:cut],end_col_heads)


i=1
new_cases_df <- NULL
new_deaths_df <- NULL
end_col_data_df <- NULL
num_cols <- length(wiki_provinces)
num_rows <- length(wiki_data)/num_cols
for (i in 1:num_rows) {
  start = (i-1)*num_cols + 1
  end = i*num_cols-4
  end_data <- end+1
  end_end_data <- end_data + 3
  new_deaths <- str_match(wiki_data[start:end],"\\((\\d*)\\)")
  new_cases <- trimws(str_replace(wiki_data[start:end],"\\((\\d*)\\)",""))
  new_cases_df<- rbind(new_cases_df,new_cases)
  new_deaths_df <- rbind(new_deaths_df, new_deaths[,2])
  end_col_data_df <- rbind(end_col_data_df,wiki_data[end_data:end_end_data])
  next
}

# interweave dfs
df <- NULL
bind_end <- num_cols-number_aggregate_cols
i=1
col_heads <- NULL
for (i in 1:bind_end) {
  df <- cbind(df, new_cases_df[,i], new_deaths_df[,i])
  col_heads <- c(col_heads,
                 paste(wiki_provinces[i],"Cases", sep=""),
                 paste(wiki_provinces[i],"NewDeaths", sep="")
  )
}
df <- cbind(df,end_col_data_df) # add in trailing columns


date_col <- seq(as.Date("2020/03/03"),by="day",length.out=nrow(df))
df <- cbind(as.character(date_col),df)
colnames(df) <- c("Date",col_heads,end_col_heads)
row.names(df) <- 1:num_rows




# add in the ICU data rates from tables
remDr$navigate(wiki_edit_url)

# @TODO add error handling around this button press
button_value = ".oo-ui-flaggedElement-primary .oo-ui-labelElement-label"
menu_element <- tryCatch({remDr$findElement(using="css selector", 
                                            value=button_value)},
                         error=function(e){NULL})
menu_element$clickElement()
#Sys.sleep(4)

wiki_source <- read_html(remDr$getPageSource()[[1]])

# load edit window
icu_col_heads = c("NationalICURate","AMBAICURate","ICUPatients")
wiki_data <- wiki_source %>%
  html_nodes("#wpTextbox1") %>%
  html_text()
wiki_data <- str_split(wiki_data,"\\=\\=\\= Medical care \\=\\=\\=",simplify=TRUE)
wiki_data <- gsub("\\n","",wiki_data[2])
wiki_dates <- regmatches(wiki_data,
                         gregexpr("(\\d+\\/\\d+\\/\\d+)",
                                  wiki_data))
half_dates <- length(wiki_dates[[1]])/2
wiki_icu_dates <- wiki_dates[[1]][1:half_dates]

wiki_icu_y_data <- str_split(wiki_data,"y1\\=",simplify=TRUE)
wiki_icu_y_titles <- str_split(wiki_icu_y_data,"y1Title\\=",simplify=TRUE)
wiki_icu_y_data <- wiki_icu_y_titles[2:4,1]
wiki_icu_y_data <- gsub("\\|","",wiki_icu_y_data)
wiki_icu_y_data <- str_split(wiki_icu_y_data,",")
wiki_icu_y_data <- as.numeric(unlist(wiki_icu_y_data))
y_data_points = length(wiki_icu_dates)
num_icu=num_rows - y_data_points
icu_graph_data <- character()
for (i in 1:number_graphs) {
  i_start <- (i-1)*y_data_points+1
  i_end <- as.numeric(i*y_data_points)
  new_col <- c(rep(NA,num_icu),wiki_icu_y_data[i_start:i_end])
  icu_graph_data <- cbind(icu_graph_data,new_col)
}



#@TODO add in age data table
remDr$navigate(wiki_age_url)
Sys.sleep(4)

wiki_source <- read_html(remDr$getPageSource()[[1]])
wiki_data <- wiki_source %>%
  html_nodes("caption+ tbody td") %>%
  html_text()
wiki_heads <- wiki_source %>%
  html_nodes("caption+ tbody th") %>%
  html_text()
wiki_data <- str_replace_all(wiki_data,"\\n","")
wiki_sa_rows <- c("All","Male","Female","Other","80+","70-79","60-69","50-59","40-49",
                  "30-39","20-29","10-19","0-9","Source")
wiki_sa_cols <- c("NumCases","PctCases","NumDeaths","PctDeaths","Lethality")
wiki_sa_data <- character()
wiki_data = wiki_data[!(wiki_data %in% "N/A")]

line_headers <- character()
# as single line
for (i_rows in 1:length(wiki_sa_rows)) {
  for (i_col in 1:length(wiki_sa_cols)) {
    line_headers <- c(line_headers,paste(wiki_sa_cols[i_col],
                                         wiki_sa_rows[i_rows],
                                         sep=""))
  }
}
wiki_sa_data <- character()
wiki_sa_line <- character()
write_age_sex <- FALSE
# as_table
for(i in 1:length(wiki_sa_rows)) {
  if(i<5) {
    i_start <- (i-1)*5+1
    i_end <- i*5
    wiki_sa_data <- rbind(wiki_sa_data,wiki_data[i_start:i_end])
    wiki_sa_line <- c(wiki_sa_line,wiki_data[i_start:i_end])
  } else {
    i_start <- i_end + 1
    i_end <- i_start + 1
    new_line <- c(wiki_data[i_start:i_end],NA,NA,NA)
    wiki_sa_data <- rbind(wiki_sa_data,new_line)
    wiki_sa_line <- c(wiki_sa_line,new_line)
  }
}

colnames(wiki_sa_data) <- wiki_sa_cols
rownames(wiki_sa_data) <- wiki_sa_rows

if(file.exists(wiki_age_sex_csv)) {
  csv_headers <- as.vector(t(read.csv(wiki_age_sex_csv,header=FALSE,colClasses='character',nrows=1)))
  num_lines <- countLines(wiki_age_sex_csv) 
  csv_last_line <- read.csv2(wiki_age_sex_csv, 
                        header=FALSE, 
                        col.names=csv_headers, 
                        skip=num_lines - 1,  
                        sep=",")
  if(csv_last_line[1,1]!=table_data[1,1]) { # only write if new data
    write_age_sex = TRUE
  }
} else {
  file.create(wiki_age_sex_csv)
  write_age_sex = TRUE
}
if (write_age_sex) {
  table_data <- as.data.frame(t(wiki_sa_line))
  colnames(table_data) <- line_headers
  write.csv(table_data,wiki_age_sex_csv,row.names=FALSE)
  day_file <- paste(wiki_age_sex_table_prefix,Sys.Date(),".csv",sep="")
  write.csv(wiki_sa_data,day_file,row.names=TRUE)
}




# Combine columns
#date_col <- seq(as.Date("2020/03/03"),by="day",length.out=nrow(df))
#df <- cbind(df,icu_graph_data)
#colnames(df) <- c("Date",col_heads,icu_col_heads)
#row.names(df) <- 1:num_rows

## write file (overwrites previous data, considering this query is inexpesnive)
write.table(df,wiki_csv, sep=",",
            append=FALSE,row.names = FALSE,col.names=TRUE)


