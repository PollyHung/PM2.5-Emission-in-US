##Preparations...
#library the necessary packages 
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

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


par(mfrow = c(2, 2)) 
#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
mv_sn <- select(source_classification, SCC, Short.Name)
mv_sn <- mv_sn[grep(pattern = "([Mm][Oo][Tt][Oo][Rr])", 
                    mv_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let mv match with pm2.5emission 
mv <- as.list(mv_sn[1]) #data frame to list 
mv <- unlist(mv) #list to vector 
table_2 <- pm2.5_emission[pm2.5_emission$SCC %in% mv, ]
table_2 <- filter(table_2, fips == 24510)


plot_5 <- matrix(NA, nrow = 4, ncol = 2)
plot_5[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_5[1, 2] <- table_2 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_5[2, 2] <- table_2 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_5[3, 2] <- table_2 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_5[4, 2] <- table_2 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_5 <- as.data.frame(plot_5)
colnames(plot_5) <- c("year", "total pm2.5 Emission")

plot(plot_5$year, plot_5$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 
     from motor vehicle-related sources",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")
text(2004, 6, 
     labels = "Motor Vehicle = 
     search by the word 'motor'")

for (i in 1:3){
  text(plot_5$year[i]+0.5, 
       plot_5$`total pm2.5 Emission`[i], 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}
text(plot_5$year[4] - 0.5, plot_5$`total pm2.5 Emission`[4], 
     labels = as.integer(plot_5$`total pm2.5 Emission`[4]))



#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
mv_sn <- select(source_classification, SCC, Short.Name)
mv_sn <- mv_sn[grep(pattern = "([Mm]otor [Vv]ehicles)", 
                    mv_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let mv match with pm2.5emission 
mv <- as.list(mv_sn[1]) #data frame to list 
mv <- unlist(mv) #list to vector 
table_2 <- pm2.5_emission[pm2.5_emission$SCC %in% mv, ]
table_2 <- filter(table_2, fips == 24510)


plot_5 <- matrix(NA, nrow = 4, ncol = 2)
plot_5[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_5[1, 2] <- table_2 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_5[2, 2] <- table_2 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_5[3, 2] <- table_2 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_5[4, 2] <- table_2 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_5 <- as.data.frame(plot_5)
colnames(plot_5) <- c("year", "total pm2.5 Emission")

plot(plot_5$year, plot_5$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 
     from motor vehicle-related sources",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")
text(2006, -0.5, 
     labels = "Motor Vehicle = 
     search by the word 'Motor Vehicle'")

for (i in 1:4){
  text(plot_5$year[i], 
       plot_5$`total pm2.5 Emission`[i] +0.1, 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}


#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
mv_sn <- select(source_classification, SCC, Short.Name)
mv_sn <- mv_sn[grep(pattern = "([Vv]ehicles)", 
                    mv_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let mv match with pm2.5emission 
mv <- as.list(mv_sn[1]) #data frame to list 
mv <- unlist(mv) #list to vector 
table_2 <- pm2.5_emission[pm2.5_emission$SCC %in% mv, ]
table_2 <- filter(table_2, fips == 24510)


plot_5 <- matrix(NA, nrow = 4, ncol = 2)
plot_5[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_5[1, 2] <- table_2 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_5[2, 2] <- table_2 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_5[3, 2] <- table_2 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_5[4, 2] <- table_2 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_5 <- as.data.frame(plot_5)
colnames(plot_5) <- c("year", "total pm2.5 Emission")

plot(plot_5$year, plot_5$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 
     from motor vehicle-related sources",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")
text(2006, 60, 
     labels = "Motor Vehicle = 
     search by the word 'Vehicle'")

for (i in 1:3){
  text(plot_5$year[i] +0.3, 
       plot_5$`total pm2.5 Emission`[i], 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}
text(plot_5$year[4] -0.3, 
     plot_5$`total pm2.5 Emission`[i], 
     labels = as.integer(plot_5$`total pm2.5 Emission`[4]))

