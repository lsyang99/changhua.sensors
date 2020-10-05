#' A Self-made function to create a table of descriptive statistics
#'
#' Input data from the "ens" database and Taiwan EPA to export to a xlsx file. The function setwd() is recommended to use beforhand to specify the path to the preferred folder.
#' @param variable Variable inputs are the classification of each sheet in the output file. They could be divided into two types. (1) Pollutants-driven: pollutants and factors including "pm25" , "co2" , "voc" , "temp" , and "humidity" could be assigned to the output. The duration wil be fixed following the raw data or the specific date definde by start.time and end.time. (2) Time-driven: Numerous durations are available as needed. The format of variable inputs should be, for example, c("1 days" , "2 days" , "3 days") or c("1 week" , "3 weeks") . Note that when a set of time-driven variables are used, pollutants will be restricted to PM2.5 only.
#' @param outputname The name of output file is required: default is "Summary". Note that the name of the output file should not be repeated otherwise the function will fail.
#' @param data_path The path of the raw "ens" data downloaded.
#' @param epa_path The path of the raw EPA data downloaded.
#' @param standard.id The device ID in the "ens" data that has been adjused. The default is "s26245".
#' @param digits The number of digits after the decimal point. The default is 4.
#' @param start.time It is an option for a preferred starting time. The date format should be "YYYY-mm-dd HH:MM".
#' @param end.time It is an option for a preferred ending time. The date format should be "YYYY-mm-dd HH:MM".

SummaryEns <- function(variable = c('pm25' , 'co2' , 'voc' , 'temp' , 'humidity')  , outputname = 'Summary' , data_path , epa_path , standard.id = 's26245' , digits = 4 , start.time = NULL , end.time = NULL){

  require(openair) ; require(magrittr) ; require(tidyr) ; require(openxlsx) ; require(data.table) ; require(lubridate)

  #### 資料整理 ####
  # ens
  ensraw <- fread(data_path)
  ensraw$imei <- paste0('s' , substr(ensraw$id , 11 , 15)) %>% factor
  ensraw$date <- parse_date_time(ensraw$time , orders = c('%Y/%m/%d %H:%M' , '%Y-%m-%d %H:%M:%S') , tz = Sys.timezone(location = TRUE))  # ?strptime 可辨識其兩種時間格式
  ens <- subset(ensraw , select = c(date , imei , pm2_5 , voc , CO2 , temperature , humidity))  # 若未來有增加測項由此新增
  colnames(ens) <- c('date' , 'imei' , 'pm25' , 'voc' , 'co2' , 'temp' , 'humidity')

  # 若無指定日期則制定成ens資料的日期範圍
  if(is.null(start.time)){
    start.time <- min(ens$date)
  }else{
    start.time <- start.time
  }
  if(is.null(end.time)){
    end.time <- max(ens$date)
  }else end.time <- end.time

  ens_hr <- timeAverage(ens , avg.time = 'hour' , type = 'imei')  # 小時平均計算
  ens_min <- timeAverage(ens , avg.time = 'min' , type = 'imei')   # 分鐘平均計算
  ens_hr <- ens_hr[which(ens_hr$date >= start.time & ens_hr$date <= end.time) , ]
  ens_min <- ens_min[which(ens_min$date >= start.time & ens_min$date <= end.time) , ]

  # EPA
  epa <- fread(epa_path)
  epa$date <- parse_date_time(epa$date , orders = c('%Y/%m/%d %H:%M:%S' , '%Y-%m-%d %H:%M:%S') , tz = Sys.timezone(location = TRUE))  # ?strptime 可辨識其兩種時間格式
  epa <- epa[which(epa$date >= start.time & epa$date <= end.time) , ]




  #### 選擇輸出格式 ####
  if(sum(levels(ens$imei) %in% standard.id) > 0){
    imei_list <- c('EPA' , standard.id , levels(ens$imei)[-which(levels(ens$imei) == standard.id)] )
  }else imei_list <- c('EPA' , levels(ens$imei))  # 若無辨識到標準id則一般排序

  if(  sum(grepl('pm25' , variable)) > 0 ){      # 如果指定變項有 pm25 則視為需輸出以污染物分隔的表格
    target <- variable
  }else{                                   # 若無，則輸出以天數分隔的表格，只會針對污染物 pm25
    target <- rep('pm25' , length(variable))
  }

  table <- data.frame(row.names = c('min' , 'max' , 'avg' , 'std' , 'LCL' , 'HCL' ,
                                    make.names(rep(c('RSQ' , 'slope' , 'intercept') , length(imei_list)) , unique = TRUE)  ))
  wb <- createWorkbook()

  #### 逐一建立sheet ####
  for(j in 1:length(variable)){
    if(  sum(grepl('pm25' , variable)) > 0 ){      # 如果指定變項有 pm25 則視為需輸出以污染物分隔的表格
      epa2 <- epa
      ens_hr2 <- ens_hr
    }else{                                   # 若無，則輸出以天數分隔的表格，只會針對污染物 pm25
      ds <- strsplit(variable[j] , ' ') %>% unlist  # 根據每個指定天數限制時間範圍
      Date <- as.POSIXct(start.time) + duration(as.numeric(ds[1]) , ds[2])
      epa2 <- epa[epa$date <= Date , ]
      ens_hr2 <- ens_hr[ens_hr$date <= Date , ]
    }

    ens_spread_min <-
      ens_min[ , c('date' , 'imei' , target[j])] %>%
      spread(. , key = 'imei' ,  value = target[j]) %>%
      na.omit
    colnames(ens_spread_min) <- c('date' , levels(ens$imei))

    ens_spread_hr <-
      ens_hr2[ , c('date' , 'imei' , target[j])] %>%
      spread(. , key = 'imei' ,  value = target[j])
    ens_spread_hr <- Reduce(
      function(x,y) merge(x , y , by = c('date') , all = TRUE) ,
      list(subset(epa2 , select = c('date' , 'pm25')) , ens_spread_hr)
    ) %>% na.omit
    colnames(ens_spread_hr) <- c('date' , 'EPA' , levels(ens$imei))


    for(i in 1:length(imei_list)){

      X <- subset(ens_spread_hr , select = imei_list[i]) %>% unlist %>% as.numeric

      table[1 , i] <- min(X , na.rm = T)
      table[2 , i] <- max(X , na.rm = T)
      table[3 , i] <- mean(X , na.rm = T)
      table[4 , i] <- sd(X , na.rm = T)
      table[5 , i] <- table[3 , i] - 2 * table[4 , i]
      table[6 , i] <- table[3 , i] + 2 * table[4 , i]


      for(p in 1:i){  # two-two linear regression

        if(p < i){

          Y <- subset(ens_spread_hr , select = imei_list[p]) %>% unlist %>% as.numeric
          lm <- lm(Y ~ X)
          table[(6+1+3*(p-1)) , i] <- summary(lm)$r.squared
          table[(6+2+3*(p-1)) , i] <- lm$coefficients[2]  # slope
          table[(6+3+3*(p-1)) , i] <- lm$coefficients[1]  # intercept

        }else{

          table[(6+1+3*(p-1)) , i] <- 1
          table[(6+2+3*(p-1)) , i] <- 1
          table[(6+3+3*(p-1)) , i] <- 0

        }
      }
    }
    table <- round(table , digits)
    colnames(table) <- imei_list

    addWorksheet(wb , variable[j])
    writeData(wb , variable[j] , table , rowNames = T , startCol = 1 , startRow = 1)
  }

  saveWorkbook(wb , paste0(outputname , '.xlsx'))



}
