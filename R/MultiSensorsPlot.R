#' A function for plotting with one variable (multiple sensors) per plot.
#'
#' It is used for exporting multiple graphs with one variable (multiple sensors) on each graph, and a total of five variables("pm25" , "voc" , "co2" , "temperature" , "humidity") is included. It is particularly used for the sensors located at the nursing centers. The raw "ens" data is applied to the function directly. The function setwd() is recommended to use beforhand to specify the file path of these plots to a preferred folder.
#' @param data_path Required. The path of the raw "ens" data downloaded.
#' @param imeiID Required. Note that the order of the imeiID input should be paired with the order of sitenames input.
#' @param sitenames Required. Note that the order of the sitenames input should be paired with the order of imeiID input.
#' @param mode The output could be either time series("ts"), hourly average("havg"), or both time series and hourly average plots("both"). The default is "both".
#' @param x_breaks It define the X axis intervals for time series plots only. The format of x_breaks input could be, for example, "1 hour" , "12 hours" , "1 day" , "2 weeks", and etc. The default is 2 days. For hourly average plots, the X axis intervals is fixed to 2 hours.
#' @param x_labels It define the X axis labels for time series plots only. The format of x_breaks input could be, for example, \code{"\%d \%H"} , \code{"\%m/\%d \%H"} , \code{"\%Y/\%m/\%d"} , and etc. The default is \code{"\%H"} .
#' @param N_cols The number of the columns of the plot. The default is 1.
#' @param font The font family of the plot. The default is "STKaiti". Please be awared that the default is for iOS users. For windows users, please change the font to, for example, "KaiTi".
#' @param width The width of the plot output. The default is 40 cm.
#' @param height The height of the plot output. The default is 30 cm.


MultiSensorsPlot <- function(data_path , imeiID , sitenames , mode = 'both' , x_breaks = '2 days' , x_labels = '%d' , N_cols = 2 , font = 'STKaiti' , width = 40 , height = 30){


  require(dplyr) ; require(tidyr) ; require(plyr) ; require(lubridate) ; require(ggplot2) ; require(openair) ; require(scales) ; require(magrittr) ; require(readr) ; require(data.table) ; require(ggpubr)

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
  data <- subset(data , select = c(date , id , pm2_5 , voc , CO2 , temperature , humidity , type))
  colnames(data) <- c('date' , 'id' , 'pm25' , 'voc' , 'co2' , 'temp' , 'humidity' , 'type')

  # 小時平均計算
  data$hour <- hour(data$date) %>% as.numeric

  avg <- cbind.data.frame(
    aggregate(pm25 ~ hour + type , data , mean),
    aggregate(voc ~ hour + type , data , mean)[3],
    aggregate(co2 ~ hour + type , data , mean)[3],
    aggregate(temp ~ hour + type , data , mean)[3],
    aggregate(humidity ~ hour + type , data , mean)[3]
  )


  if(mode == 'ts'){  # 逐分

    # time series
    pp1 <- data[data$pm25 != 0 , ] %>%  # 作圖前先移除0值
      ggplot(. , aes(x = date , y = pm25)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression(PM[2.5] * ' (' * mu *'g/' * m^3 * ')')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)

    ggsave(pp1 , file = 'pm2.5逐分.png' , width = width , height = height , units = 'cm')

    pp2 <- data[data$co2 != 0 , ] %>%
      ggplot(. , aes(x = date , y = co2)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression(CO[2] * ' (ppm)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp2 , file = 'co2逐分.png' , width = width , height = height , units = 'cm')

    pp3 <- data[data$voc != 0 , ] %>%
      ggplot(. , aes(x = date , y = voc)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression( 'VOC (ppb)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp3 , file = 'voc逐分.png' , width = width , height = height , units = 'cm')

    pp4 <- data %>%
      ggplot(. , aes(x = date , y = temp)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = '%d' , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression('Temperature ('* ~ degree * C * ')') , breaks = seq(20 , 40 , 5)) +
      coord_cartesian(ylim = c(20 , 40)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp4 , file = 'temp逐分.png' , width = width , height = height , units = 'cm')

    pp5 <- data %>%
      ggplot(. , aes(x = date , y = humidity)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = 'Humidity (%)' , breaks = seq(30 , 90 , 10)) +
      coord_cartesian(ylim = c(30 , 90)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp5 , file = 'humidity逐分.png' , width = width , height = height , units = 'cm')

  }else if(mode == 'havg'){

    # hourly average
    p1 <- ggplot(avg , aes(x = hour , y = pm25)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 35 , col = 'red' , size = 1.2, linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0,24,2)) +
      scale_y_continuous(name = expression(PM[2.5] * ' (' * mu *'g/' * m^3 * ')')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font, size = 34)) +
      geom_text(x = 12 , y = 40 , label = expression('環保署室內空氣品質日平均標準 35 (' * mu * 'g/' * m^3 * ')') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)

    ggsave(p1 , file = 'pm2.5小時平均.png' , width = width , height = height , units = 'cm')

    p2 <- ggplot(avg , aes(x = hour , y = co2)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 1000 , col = 'red' , size = 1.2 , linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression(CO[2] * ' (ppm)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      geom_text(x = 12 , y = 1105 , label = expression('環保署室內空氣品質八小時平均標準 1000 (ppm)') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p2 , file = 'co2小時平均.png' , width = width , height = height , units = 'cm')

    p3 <- ggplot(avg , aes(x = hour , y = voc)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 560 , col = 'red' , size = 1.2, linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression('VOC (ppb)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      geom_text(x = 12, y = 700 , label = expression('環保署室內空氣品質日平均標準 560 (ppb)') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p3 , file = 'voc小時平均.png' , width = width , height = height , units = 'cm')

    p4 <- ggplot(avg , aes(x = hour , y = temp)) +
      geom_line(size = 1.5) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression('Temperature ('* ~ degree * C * ')') , breaks = seq(20 , 40 , 5)) +
      coord_cartesian(ylim = c(20 , 40)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p4 , file = 'temp小時平均.png' , width = width , height = height , units = 'cm')

    p5 <- ggplot(avg , aes(x = hour , y = humidity)) +
      geom_line(size = 1.5) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = 'Humidity (%)' , breaks = seq(30 , 90 , 10)) +
      coord_cartesian(ylim = c(30 , 90)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p5 , file = 'humidity小時平均.png' , width = width , height = height , units = 'cm')

    write.csv(avg , file = '小時平均數據檔.csv')

  }else if(mode == 'both'){

    # hourly average
    p1 <- ggplot(avg , aes(x = hour , y = pm25)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 35 , col = 'red' , size = 1.2, linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0,24,2)) +
      scale_y_continuous(name = expression(PM[2.5] * ' (' * mu *'g/' * m^3 * ')')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font, size = 34)) +
      geom_text(x = 12 , y = 40 , label = expression('環保署室內空氣品質日平均標準 35 (' * mu * 'g/' * m^3 * ')') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)

    ggsave(p1 , file = 'pm2.5小時平均.png' , width = width , height = height , units = 'cm')

    p2 <- ggplot(avg , aes(x = hour , y = co2)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 1000 , col = 'red' , size = 1.2 , linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression(CO[2] * ' (ppm)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      geom_text(x = 12 , y = 1105 , label = expression('環保署室內空氣品質八小時平均標準 1000 (ppm)') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p2 , file = 'co2小時平均.png' , width = width , height = height , units = 'cm')

    p3 <- ggplot(avg , aes(x = hour , y = voc)) +
      geom_line(size = 1.5) +
      geom_hline(yintercept = 560 , col = 'red' , size = 1.2, linetype = 2) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression('VOC (ppb)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      geom_text(x = 12, y = 700 , label = expression('環保署室內空氣品質日平均標準 560 (ppb)') , col = 'red' , size = 6 , family = font) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p3 , file = 'voc小時平均.png' , width = width , height = height , units = 'cm')

    p4 <- ggplot(avg , aes(x = hour , y = temp)) +
      geom_line(size = 1.5) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = expression('Temperature ('* ~ degree * C * ')') , breaks = seq(20 , 40 , 5)) +
      coord_cartesian(ylim = c(20 , 40)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p4 , file = 'temp小時平均.png' , width = width , height = height , units = 'cm')

    p5 <- ggplot(avg , aes(x = hour , y = humidity)) +
      geom_line(size = 1.5) +
      scale_x_continuous(name = '小時' , breaks = seq(0 , 24 , 2)) +
      scale_y_continuous(name = 'Humidity (%)' , breaks = seq(30 , 90 , 10)) +
      coord_cartesian(ylim = c(30 , 90)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text(family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(p5 , file = 'humidity小時平均.png' , width = width , height = height , units = 'cm')


    # time series
    pp1 <- data[data$pm25 != 0 , ] %>%  # 作圖前先移除0值
      ggplot(. , aes(x = date , y = pm25)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression(PM[2.5] * ' (' * mu *'g/' * m^3 * ')')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)

    ggsave(pp1 , file = 'pm2.5逐分.png' , width = width , height = height , units = 'cm')

    pp2 <- data[data$co2 != 0 , ] %>%
      ggplot(. , aes(x = date , y = co2)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression(CO[2] * ' (ppm)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp2 , file = 'co2逐分.png' , width = width , height = height , units = 'cm')

    pp3 <- data[data$voc != 0 , ] %>%
      ggplot(. , aes(x = date , y = voc)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression( 'VOC (ppb)')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp3 , file = 'voc逐分.png' , width = width , height = height , units = 'cm')

    pp4 <- data %>%
      ggplot(. , aes(x = date , y = temp)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = '%d' , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = expression('Temperature ('* ~ degree * C * ')') , breaks = seq(20 , 40 , 5)) +
      coord_cartesian(ylim = c(20 , 40)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp4 , file = 'temp逐分.png' , width = width , height = height , units = 'cm')

    pp5 <- data %>%
      ggplot(. , aes(x = date , y = humidity)) +
      geom_line(size = 1.5) +
      scale_x_datetime(name = '日期' , breaks = date_breaks(x_breaks) , date_labels = x_labels , timezone = Sys.timezone(location = TRUE)) +
      scale_y_continuous(name = 'Humidity (%)' , breaks = seq(30 , 90 , 10)) +
      coord_cartesian(ylim = c(30 , 90)) +  # 保持原值不受影響
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5 , face = 'bold' , size = 38)) +
      theme(text = element_text( family = font , size = 34)) +
      facet_wrap( ~ type , scales = 'fixed' , ncol = N_cols)
    ggsave(pp5 , file = 'humidity逐分.png' , width = width , height = height , units = 'cm')

    write.csv(avg , file = '小時平均數據檔.csv')


  }else print('Error. Please recheck the variable "mode".')


}
