##Preparations...
#library the necessary packages 
library(dplyr)
library(purrr)
library(tidyr)

#create a folder in computer to store the data 
if(!file.exists("data")){ 
  dir.create("data")
}

#download the data from the internet 
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl, destfile = "./data/EPA.zip", method = "curl")
list.files("./data")

#unzip the data 
dir.create("./data/epa") #we create an empty folder for the files 
out_dir <- "./data/epa" #the directory we want to extract our files to 
unzip("./data/epa.zip", exdir = out_dir) #unzipping and extracting......
list.files("./data/epa")

#download the data and set the correct working directory 
pm2.5_emission <- readRDS("./data/epa/summarySCC_PM25.rds")
source_classification <- readRDS("./data/epa/Source_Classification_Code.rds")
setwd("./data/epa")

names(pm2.5_emission)


#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#   Using the base plotting system, make a plot showing the total PM2.5 emission from all 
#   sources for each of the years 1999, 2002, 2005, and 2008.
plot_1 <- matrix(NA, nrow = 4, ncol = 2)
plot_1[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_1[1, 2] <- pm2.5_emission %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_1[2, 2] <- pm2.5_emission %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_1[3, 2] <- pm2.5_emission %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_1[4, 2] <- pm2.5_emission %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_1 <- as.data.frame(plot_1)
colnames(plot_1) <- c("year", "total pm2.5 Emission")

plot(plot_1$year, plot_1$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")

for (i in 1:3){
  text(plot_1$year[i]+0.7, 
       plot_1$`total pm2.5 Emission`[i] + 50000, 
       labels = as.integer(plot_1$`total pm2.5 Emission`[i]))
}
text(plot_1$year[4]-0.7, plot_1$`total pm2.5 Emission`[4] + 50000, 
     labels = as.integer(plot_1$`total pm2.5 Emission`[4]))