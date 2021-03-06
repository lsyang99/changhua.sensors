% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OneSensorPlot.R
\name{OneSensorPlot}
\alias{OneSensorPlot}
\title{A function for plotting with one sensor per plot}
\usage{
OneSensorPlot(
  data_path,
  imeiID,
  sitenames,
  variable = c("pm25", "voc", "temperature", "humidity"),
  mode = "both",
  x_breaks = "3 hours",
  x_labels = "\%H",
  N_cols = 1,
  font = "STKaiti",
  xlab = "date",
  ylab = NULL,
  subtitle = NULL,
  width = 20,
  height = 30
)
}
\arguments{
\item{data_path}{Required. The path of the raw "ens" data downloaded.}

\item{imeiID}{Required. Note that the order of the "imeiID" input should be paired with the order of "sitenames" input.}

\item{sitenames}{Required. Note that the order of the "sitenames" input should be paired with the order of "imeiID" input.}

\item{variable}{There are five variables available: "pm25", "voc", "co2", "temperature", and "humidity". The default is c("pm25" , "voc" , "temperature" , "humidity").}

\item{mode}{The output could be either time series("ts"), hourly average("havg"), or both time series and hourly average plots("both"). The default is "both".}

\item{x_breaks}{It defines the X axis intervals for time series plots only. The format of x_breaks input could be, for example, "1 hour", "12 hours", "1 day", "2 weeks", and etc. The default is 3 hours. For hourly average plots, the X axis intervals is fixed to 2 hours.}

\item{x_labels}{It defines the X axis labels for time series plots only. The format of x_breaks input could be, for example, \code{"\%d \%H"} , \code{"\%m/\%d \%H"} , \code{"\%Y/\%m/\%d"} , and etc. The default is \code{"\%H"} .}

\item{N_cols}{The number of the columns of the plot. The default is 1.}

\item{font}{The font family of the plot. The default is "STKaiti". Please be awared that the default is for iOS users. For windows users, please change the font to, for example, "KaiTi".}

\item{xlab}{The title of X axis for time series plots. The default is "date". For hourly average plots, it is fixed to "Hour".}

\item{ylab}{An additional title of Y axis.}

\item{subtitle}{An additional subtitle of the plot.}

\item{width}{The width of the plot output. The default is 20 cm.}

\item{height}{The height of the plot output. The default is 30 cm.}
}
\description{
It is used for exporting multiple graphs with one sensor on each graph. The raw "ens" data is applied to the function directly.The function setwd() is recommended to use beforhand to specify the file path of these plots to a preferred folder.
}
