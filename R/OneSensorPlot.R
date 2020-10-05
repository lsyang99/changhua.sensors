#' A function for plotting with one sensor per plot
#'
#' It is used for exporting multiple graphs with one sensor on each graph. The raw "ens" data is applied to the function directly.The function setwd() is recommended to use beforhand to specify the file path of these plots to a preferred folder.
#'
#' @param data_path Required. The path of the raw "ens" data downloaded.
#' @param imeiID Required. Note that the order of the "imeiID" input should be paired with the order of "sitenames" input.
#' @param sitenames Required. Note that the order of the "sitenames" input should be paired with the order of "imeiID" input.
#' @param mode The output could be either time series("ts"), hourly average("havg"), or both time series and hourly average plots("both"). The default is "both".
#' @param variable There are five variables available: "pm25", "voc", "co2", "temperature", and "humidity". The default is c("pm25" , "voc" , "temperature" , "humidity").
#' @param x_breaks It defines the X axis intervals for time series plots only. The format of x_breaks input could be, for example, "1 hour", "12 hours", "1 day", "2 weeks", and etc. The default is 3 hours. For hourly average plots, the X axis intervals is fixed to 2 hours.
#' @param x_labels  It defines the X axis labels for time series plots only. The format of x_breaks input could be, for example, \code{"\%d \%H"} , \code{"\%m/\%d \%H"} , \code{"\%Y/\%m/\%d"} , and etc. The default is \code{"\%H"} .
#' @param N_cols The number of the columns of the plot. The default is 1.
#' @param font The font family of the plot. The default is "STKaiti". Please be awared that the default is for iOS users. For windows users, please change the font to, for example, "KaiTi".
#' @param xlab The title of X axis for time series plots. The default is "date". For hourly average plots, it is fixed to "Hour".
#' @param ylab An additional title of Y axis.
#' @param subtitle An additional subtitle of the plot.
#' @param width The width of the plot output. The default is 20 cm.
#' @param height The height of the plot output. The default is 30 cm.
#' @export


OneSensorPlot <- function(data_path , imeiID , sitenames , variable = c('pm25' , 'voc' , 'temperature' , 'humidity') , mode = 'both' , x_breaks = '3 hours' , x_labels = '%H' , N_cols = 1 , font = 'STKaiti' , xlab = 'date' , ylab = NULL , subtitle = NULL , width = 20 , height = 30){

  require(dplyr) ; require(tidyr) ; require(plyr) ; require(lubridate) ; require(ggplot2) ; require(openair) ; require(scales) ; require(magrittr) ; require(readr) ; require(data.table) ; require(ggpubr) ; require(mgsub)

  variable <- sub('pm25' , 'pm2_5' , variable)
  variable2 <- mgsub(variable , c('pm2_5' , 'voc' , 'co2' , 'temperature' , 'humidity') , c('PM2.5' , 'VOC' , 'CO2' , 'Temperature' , 'Humidity'))
  labels <- mgsub(variable2 , c('PM2.5' , 'VOC' , 'CO2' , 'Temperature' , 'Humidity') , c('PM[2.5]' , 'VOC' , 'CO[2]' , 'Temperature' , 'Humidity'))

  data <- fread(data_path)
  # 資料整理
  # 根據手動輸入的imei編號與測站名稱，將原始資料依據imei代入測站(type)
  id.table <- data.frame(
    id = imeiID ,
    type = sitenames
  )
  data$id <- factor(data$id , levels = imeiID)
  data$type <- sapply(1:dim(data)[1] , function(i) id.table[id.table$id == data$id[i] , ]$type)

  data$type <- factor(data$type , levels = sitenames)
  data$date <- as.POSIXct(data$time , format = '%Y-%m-%d %H:%M:%S')
  data <- subset(data , select = c('date' , 'id' , variable , 'type'))
  colnames(data) <- c('date' , 'id' , variable2 , 'type')

  # 小時平均計算
  data$hour <- hour(data$date) %>% as.numeric

  avg <- pivot_longer(data , cols = variable2 , names_to = 'Var' , values_to = 'Value') %>%
    aggregate(Value ~ hour + type + Var , . , mean) %>%
    pivot_wider(names_from = 'Var' , values_from = 'Value')


  # 作圖，依據指定mode分成逐分或小時平均
  if(mode == 'ts'){  # 逐分

    sample_list <- data %>% group_split(type)

    for(i in 1:length(sample_list)){
      plot.data1 <- sample_list[[i]] %>%  # 變動數字以選取感測器
        pivot_longer(. , cols = variable2 , names_to = 'Var' , values_to = 'Value')
      plot.data1$Var <- factor(plot.data1$Var , levels = variable2 , labels = labels)

      pp1 <- ggplot(plot.data1 , aes(x = date , y = Value)) +
        geom_line(size = 1.5) +
        scale_x_datetime(breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
        theme_bw() +
        labs(title = as.character(plot.data1$type[1]) , subtitle = subtitle , x = xlab , y = ylab) +
        theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
        theme(text = element_text(family = font , size = 25) ,
              plot.subtitle = element_text(size = 21 , hjust = 0.5)) +
        facet_wrap( ~ Var , scales = 'free_y' , ncol = N_cols , labeller =  label_parsed)
      ggsave(pp1 , file = paste0(sitenames[i] , '_逐分.png') , width = width , height = height , units = 'cm')
    }

  }else if(mode == 'havg'){  # 小時平均

    avg_list <- avg %>% group_split(type)  # 照id(地點)分組
    for(i in 1:length(avg_list)){
      plot.data2 <- avg_list[[i]] %>%  # 變動數字以選取感測器
        pivot_longer(. , cols = variable2 , names_to = 'Var' , values_to = 'Value')
      plot.data2$Var <- factor(plot.data2$Var , levels = variable2)

      pp2 <- ggplot(plot.data2 , aes(x = hour , y = Value)) +
        geom_line(size = 1.5) +
        scale_x_continuous(name = 'Hour' , breaks = seq(0,24,2)) +
        theme_bw() +
        labs(title = as.character(plot.data2$type[1]) , subtitle = subtitle , y = ylab) +
        theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
        theme(text = element_text(family = font , size = 25) ,
              plot.subtitle = element_text(size = 21 , hjust = 0.5)) +
        facet_wrap( ~ Var , scales = 'free_y' , ncol = N_cols , labeller =  label_parsed)
      ggsave(pp2 , file = paste0(sitenames[i] , '_小時平均.png') , width = 20 , height = 30 , units = 'cm')

      write.csv(avg , file = '小時平均數據檔.csv')
    }

  }else if(mode == 'both'){

    sample_list <- data %>% group_split(type)

    for(i in 1:length(sample_list)){
      plot.data1 <- sample_list[[i]] %>%  # 變動數字以選取感測器
        pivot_longer(. , cols = variable2 , names_to = 'Var' , values_to = 'Value')
      plot.data1$Var <- factor(plot.data1$Var , levels = variable2)

      pp1 <- ggplot(plot.data1 , aes(x = date , y = Value)) +
        geom_line(size = 1.5) +
        scale_x_datetime(breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
        theme_bw() +
        labs(title = as.character(plot.data1$type[1]) , subtitle = subtitle , x = xlab , y = ylab) +
        theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
        theme(text = element_text(family = font , size = 25) ,
              plot.subtitle = element_text(size = 21 , hjust = 0.5)) +
        facet_wrap( ~ Var , scales = 'free_y' , ncol = N_cols , labeller =  label_parsed)
      ggsave(pp1 , file = paste0(sitenames[i] , '_逐分.png') , width = width , height = height , units = 'cm')
    }
    avg_list <- avg %>% group_split(type)  # 照type(地點)分組

    for(i in 1:length(avg_list)){
      plot.data2 <- avg_list[[i]] %>%  # 變動數字以選取感測器
        pivot_longer(. , cols = variable2 , names_to = 'Var' , values_to = 'Value')
      plot.data2$Var <- factor(plot.data2$Var , levels = variable2)

      pp2 <- ggplot(plot.data2 , aes(x = hour , y = Value)) +
        geom_line(size = 1.5) +
        scale_x_continuous(name = 'Hour' , breaks = seq(0,24,2)) +
        theme_bw() +
        labs(title = as.character(plot.data2$type[1]) , subtitle = subtitle , y = ylab) +
        theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
        theme(text = element_text(family = font , size = 25) ,
              plot.subtitle = element_text(size = 21 , hjust = 0.5)) +
        facet_wrap( ~ Var , scales = 'free_y' , ncol = N_cols , labeller =  label_parsed)
      ggsave(pp2 , file = paste0(sitenames[i] , '_小時平均.png') , width = 20 , height = 30 , units = 'cm')

      write.csv(avg , file = '小時平均數據檔.csv')

    }

  }else print('Error. Please recheck the variable "mode".')


}
