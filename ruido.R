setwd("C:/Users/pedro moises/Documents/R/project/ruido")
rm(list = ls())


#proceso de datos 
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
n <- lapply(list.files(pattern = "json"), function(x) {fromJSON(x)[[1]][3:5]}) #leyendo todos los archivos disponibles
noi <- do.call(rbind, n) #uniendo todo en una sola DB
noi$db <- as.numeric(noi$db)
noi$date <- ymd_hms(noi$date, tz = "America/Caracas")

#selet only the hour and minutes on the posixct
noi$time <- as.POSIXct(strftime(noi$date, format="%H:%M"), format="%H:%M")


#pick how to aggregate the numbers
noi$min <- floor_date(noi$time, unit = "15 mins")



#line
no1 <- noi %>% group_by(min) %>% summarise(mean = mean(db), max = max(db), mini = min(db), median = median(db)) %>% arrange(desc(median))
ggplot(no1, aes(min)) +
 geom_line(aes(y = mean, color = "Mean"))+
 geom_line(aes(y = max, color = "Max"))+
 geom_line(aes(y = mini, color = "Minimum"))

#boxplot
boxplot( data = noi, col = "green")

number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(noi, aes(min, db))+
        geom_boxplot(aes(group = min))+
        labs(x = "Hora", y = "decibeles")+
        ggtitle("Distribución decibeles en las Caobas, agrupados cada 20 minutos") 


#data processing by getting the data from internal storage 
library(XML)
a <- choose.files()
b <- xmlParse(a)
c <- xmlRoot(c)

#geting data from the API
all <- "http://www.noisetube.net/api/search.json?key=19959fbdd1ead1fedd03bf54d9488d5445e174c7&user=pedromoisescamacho"
all2 <- "http://www.noisetube.net/api/traces?key=19959fbdd1ead1fedd03bf54d9488d5445e174c7&traceid=8237"

