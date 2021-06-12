library(pdftools)
library(tidyverse)
library(stringr)
library(lubridate)
library(tesseract)
library(magick)
library(animation)

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
  # It's a simple thing, but you'd think they could at least standardize their naming convention.
  # All of these for loops are added because, well, the people doing this have no 
  # rhyme or reason, which really instills confidence. (as does the quality of my
  # scripting, I know, I know)
  
  file_date <- c(paste(str_pad(day(this_day),2,pad="0"),
                     str_pad(month(this_day),2,pad="0"),
                     year(this_day),sep="-"),
                 paste(day(this_day),month(this_day),year(this_day),sep="-"))
  file_prefix <- c(paste("https://www.argentina.gob.ar/sites/default/files/",
                       year(this_day),
                       "/",str_pad(month(this_day),2,pad="0"),
                       "/",sep=""),
                   "https://www.argentina.gob.ar/sites/default/files/")
  file_middle <- c("sala-situacion-covid-19_",
                   "sala-situacion-covid19",
                   "sala-situacion-covid19-",
                   "sala-situacion-covid-19-",
                   "sala-situacion-covid19_")
  file_suffix <- c("","_0")

  
  pdf_exists<-FALSE
  for(this_suffix in file_suffix) {
    for(this_file_prefix in file_prefix) {
      for(this_file_date in file_date) {
        for(this_file_middle in file_middle) {
          minsalud_file_test <- paste (this_file_prefix,this_file_middle,this_file_date,this_suffix,".pdf",sep="")
          hd <- httr::HEAD(minsalud_file_test)
          if(hd$all_headers[[1]]$status == 200) {
            minsalud_file <- minsalud_file_test
            pdf_exists <- TRUE
            break
          }
        }
      }
    }
  }
  if(pdf_exists) {

    full_ocr <- first(pdf_ocr_data(minsalud_file,language="spa",dpi=150))
    raw_img <- image_read_pdf(minsalud_file, density=150)
    
    # first construct a search string to make this a bit quicker
    for(row_num in 1:nrow(full_ocr[1])) {
      search_fin <- row_num+4
      full_ocr[row_num,'search'] <- paste(unlist(full_ocr[row_num:search_fin,1]),collapse=" ")
    }
    start_row <- which(grepl("Confirmados Covid internados UTI %",full_ocr$search))
    end_row <- first(which(grepl("Todas las patologías, sector público",full_ocr$search)))
    row_num <- start_row
    y_bottom <- as.numeric(first(str_split(full_ocr[end_row,'bbox'],","))[2])

    # Get ICU Beds from OCR box
    box_start <- as.numeric(first(str_split(full_ocr[row_num,'bbox'],",")))
    box_end <- as.numeric(first(str_split(full_ocr[row_num+4,'bbox'],",")))
    x_offset <- box_start[1]
    y_offset <- box_end[4]  
    x_dist <- (box_end[3]-x_offset) * .6
    y_dist <- if(is.na(y_bottom)) {  #if OCR can't find the end string, guess a height
        as.numeric(image_info(raw_img)['height'])/18 
      } else {
        y_bottom - y_offset + (box_end[4] - box_end[2])*2
      }
    icu_bounding <- c(x_dist,y_dist,x_offset,y_offset)
    
    # This looks complicated, but is there to adjust the image so it's easier for OCR to read
    beds_box <- raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
      image_negate() %>% image_convert(colorspace="gray") %>% image_negate() %>% ocr()
    # line below is for debugging what OCR range was
    #image_ggplot(raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
    #               image_negate() %>% image_convert(colorspace="gray") %>% image_negate())
  
    # Get ICU National %
    box_start <- as.numeric(first(str_split(full_ocr[row_num+5,'bbox'],",")))
    box_end <- as.numeric(first(str_split(full_ocr[row_num+9,'bbox'],",")))
    x_offset <- box_start[1]
    y_offset <- box_end[4] 
    x_dist <- (box_end[3]-x_offset) * .85
    y_dist <- if(is.na(y_bottom)) {
      as.numeric(image_info(raw_img)['height'])/22
      } else {
        y_bottom - y_offset + (box_end[4] - box_end[2])/2
      }
    icu_bounding <- c(x_dist,y_dist,x_offset,y_offset)
    nation_bounding <- c(x_dist,y_dist,x_offset,y_offset)
    nation_box <- raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
      image_negate() %>% image_convert(colorspace="gray") %>% image_negate() %>% image_contrast() %>% image_blur(sigma=1,radius=10) %>% image_level(mid_point = 1) %>% ocr()
    # line below is for debugging what OCR range was
    #image_ggplot(raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
    #               image_negate() %>% image_convert(colorspace="gray") %>% image_negate() %>% image_contrast() %>% image_blur(sigma=1,radius=10) %>% image_level(mid_point = 1))
    
    # Get AMBA ICU%
    box_start <- as.numeric(first(str_split(full_ocr[row_num+11,'bbox'],",")))
    box_end <- as.numeric(first(str_split(full_ocr[row_num+15,'bbox'],",")))
    x_offset <- box_start[1]
    y_offset <- box_end[4] 
    x_dist <- (box_end[3]-x_offset) * .85
    y_dist <- if(is.na(y_bottom)) {
      as.numeric(image_info(raw_img)['height'])/22
    } else {
      y_bottom - y_offset + (box_end[4] - box_end[2])/2
    }
    amba_bounding <- c(x_dist,y_dist,x_offset,y_offset)
    amba_box <- raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
      image_negate() %>% image_convert(colorspace="gray") %>% image_negate() %>% image_contrast() %>% image_blur(sigma=1,radius=10) %>% image_level(mid_point = 1) %>% ocr()
    # line below is for debugging what OCR range was
    #image_ggplot(raw_img %>% image_crop(geometry_area(x_dist,y_dist,x_offset,y_offset)) %>% image_quantize(max=2) %>%
    #               image_negate() %>% image_convert(colorspace="gray") %>% image_negate() %>% image_contrast() %>% image_blur(sigma=1,radius=10) %>% image_level(mid_point = 1))

    unlink(tempfile())

    # Extract percentages out of OCR'd strings
    
    icu_total <- str_extract(beds_box,"(\\d+)")
    nation_pct = str_replace(str_extract(nation_box,"(\\d+.\\d)"),",",".")
    if(is.na(nation_pct)) {
      nation_pct = str_replace(str_extract(nation_box,"(\\d{2})%"),"%","")
    }
    amba_pct = str_replace(str_extract(amba_box,"(\\d+.\\d)"),",",".")
    if(is.na(amba_pct)) {
      amba_pct =str_replace(str_extract(amba_box,"(\\d{2})%"),"%","") 
    }

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




