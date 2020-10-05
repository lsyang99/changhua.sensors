#' A Self-made function for downloading hourly monitoring data from Taiwan EPA.
#'
#' A preferred time interval can be selected by defining the starting and ending time. The function setwd() is recommended to use beforhand to specify the path to the preferred folder.
#'
#' @param start.time The starting time of data downloaded. The date format follows "YYYY-mm-dd HH:MM". Nevertheless, formats including "YYYY", "YYYY-mm", and "YYYY-mm-dd" are also feasible. Note that data is only available since July, 1982.
#' @param end.time The ending time of data downloaded. The date format follows "YYYY-mm-dd HH:MM". Nevertheless, formats including "YYYY", "YYYY-mm" are also feasible. The default is current local time.
#' @param epa.site One or more than one EPA site can be selected by inputting its Chinese name. The default is all sites.
#' @param outputname The name of the output file. The default is "EPA".

EPAdownload <- function(start.time , end.time = NULL , epa.site = NULL , outputname = 'EPA'){

  require(magrittr) ; require(zoo) ; require(data.table) ; require(plyr) ; require(tidyr) ; require(lubridate) ; require(httr) ; require(dplyr) ; library(devtools)

  # 時間定義：先篩到月份，最後再篩到日期/時間

  if(substr(start.time , 5 , 6) == ''){  # 只輸入年份
    t1 <- as.Date(paste0(start.time , '-01-01'))
  }else if(substr(start.time , 9 , 10) == ''){
    t1 <- as.Date(paste0(start.time , '-01'))
  }else t1 <- as.Date(start.time)

  if(is.null(end.time)){
    end.time <- Sys.Date()
  }else end.time <- end.time
  if(substr(end.time , 9 , 10) == ''){
    t2 <- as.Date(paste0(end.time , '-01'))
  }else t2 <- as.Date(end.time)

  if(t1 < as.Date('1982-07-01')){
    return('Error. There is no data available prior to 1982-07.')
  }else if(t1 < t2){
    interval <- format(seq(t1 , t2 , by = 'day'), '%Y-%m') %>% unique
  }else return('Error. Please reset the starting time or the ending time.')


  url <- paste0('http://opendata2.epa.gov.tw/data/aqx_p_15/aqx_p_15_' , interval , '.zip')

  ### 資料下載
  dir.create('temporary')
  destfile <- paste0('temporary/' , interval , '.zip')
  download.file(url , destfile)

  lapply(1:length(destfile) , function(i) unzip(destfile[i] , exdir = 'temporary/'))
  raw <- ldply(dir('temporary' , pattern = glob2rx(paste0('*.csv')) , full.name = T) , .fun = fread)
  unlink('temporary' , recursive = TRUE)  # 整個原始資料夾移除


  colnames(raw) <- c("SiteId", "site", "ItemId", "ItemName", "測項",
                     "ItemUnit", "date" , 0:23)
  raw <- raw[ , c('date' , 'site' , '測項' , 0:23)]  # 只保留必要的變項，後續做gather & spread才無問題

  x <- gather(raw , key = 'hour' , value = 'value' , '0':'23')
  x$value <- suppressWarnings(as.numeric(x$value))
  epa <- suppressWarnings(spread(x , key = '測項' , value = value))

  epa$date <- parse_date_time(paste(as.Date(epa$date) , epa$hour) , orders = c('%Y/%m/%d %H' , '%Y-%m-%d %H') , tz = Sys.timezone(location = TRUE))  # ?strptime 可辨識其兩種時間格式
  epa <- epa[ , -3]  # remove column: hour

  f.list <- c('site')
  for(i in 1:length(f.list)){
    epa[ , f.list[i]] <- factor(epa[ , f.list[i]])
  }
  colnames(epa) <- c("date", "site", "temp", "ch4", "co", "co2", "nmhc",
                     "no", "no2", "nox", "o3", "ph_rain", "pm10", "pm25", "rain_cond",
                     "rain_int", "rh", "so2", "thc", "wd_hr", "wd", "ws",
                     "ws_hr")
  epa <- epa[which(epa$date >= start.time & epa$date <= end.time) , ]
  epa <- epa[order(epa$date) , ]
  epa <- subset(epa , site %in% epa.site)
  write.csv(epa , paste0(outputname , '.csv'))
  return(epa)
}
