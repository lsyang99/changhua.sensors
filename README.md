# changhua.sensors


changhua.sensors is a R package integrating and analyzing data from indoor low-cost PM2.5 sensors distributed at health care centers and preschools in Changhua County, Taiwan. It provides summary statistics and time-dependent plots for the monthly report.

```{r}
# install the package
install.packages("devtools")
devtools::install_github("lsyang99/changhua.sensors")
```

Functions include:                   
`EPAdownload`: Download air monitoing station data from Taiwan EPA.                 
`SummaryEns`: Perform regression analysis and compare results between our low-cost sensors and Taiwan EPA data.           
`OneSensorPlot`: Return a  monthly report with multple variables per sensor.             
`MultiSensorPlot`: Return a monthly report with one single variable for multiple sensors.            
