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

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#   vehicle sources in Los Angeles County, California 
#   (\color{red}{\verb|fips == "06037"|}fips == "06037"). 
#   Which city has seen greater changes over time in motor vehicle emissions?
#   baltimore: fips == "24510", los angeles: fips == "06037"

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
     main = "PM2.5 Emission 
     (1999-2008) from 
     motor vehicle-related 
     sources in Baltimore",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b", 
     cex.main=1)
text(2003, 60, 
     labels = "Motor Vehicle = 
     search by the 
     word 'Vehicle'")

for (i in 1:3){
  text(plot_5$year[i] +0.5, 
       plot_5$`total pm2.5 Emission`[i] + 1, 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}
text(plot_5$year[4] -0.3, 
     plot_5$`total pm2.5 Emission`[i], 
     labels = as.integer(plot_5$`total pm2.5 Emission`[4]))


mv_sn <- select(source_classification, SCC, Short.Name)
mv_sn <- mv_sn[grep(pattern = "([Vv]ehicles)", 
                    mv_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let mv match with pm2.5emission 
mv <- as.list(mv_sn[1]) #data frame to list 
mv <- unlist(mv) #list to vector 
table_2 <- pm2.5_emission[pm2.5_emission$SCC %in% mv, ]
table_2 <- filter(table_2, fips == "06037")


plot_5 <- matrix(NA, nrow = 4, ncol = 2)
plot_5[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_5[1, 2] <- table_2 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_5[2, 2] <- table_2 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_5[3, 2] <- table_2 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_5[4, 2] <- table_2 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_5 <- as.data.frame(plot_5)
colnames(plot_5) <- c("year", "total pm2.5 Emission")

plot(plot_5$year, plot_5$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission 
     (1999-2008) from 
     motor vehicle-related 
     sources in LA",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b",
     cex.main=1)
text(2004, 1300, 
     labels = "Motor Vehicle = 
     search by the 
     word 'Vehicle'")

for (i in 1:3){
  text(plot_5$year[i] +1, 
       plot_5$`total pm2.5 Emission`[i], 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}
text(plot_5$year[4]-1, 
     plot_5$`total pm2.5 Emission`[4], 
     labels = as.integer(plot_5$`total pm2.5 Emission`[4]))


#as 1 graph 
baltimore <- select(source_classification, SCC, Short.Name)
baltimore <- baltimore[grep(pattern = "([Vv]ehicles)", 
                            baltimore$Short.Name), ]
baltimore <- as.list(baltimore[1])
baltimore <- unlist(baltimore)
baltimore <- pm2.5_emission[pm2.5_emission$SCC %in% baltimore, ]
baltimore <- filter(baltimore, fips == 24510)

LA <- select(source_classification, SCC, Short.Name)
LA <- LA[grep(pattern = "([Vv]ehicles)", 
              LA$Short.Name), ]
LA <- as.list(LA[1])
LA <- unlist(LA)
LA <- pm2.5_emission[pm2.5_emission$SCC %in% LA, ]
LA <- filter(LA, fips == "06037")


plot_6 <- matrix(NA, nrow = 8, ncol = 2)
plot_6[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_6[1, 2] <- baltimore %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_6[2, 2] <- baltimore %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_6[3, 2] <- baltimore %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_6[4, 2] <- baltimore %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_6[5, 2] <- LA %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_6[6, 2] <- LA %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_6[7, 2] <- LA %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_6[8, 2] <- LA %>% filter(year == 2008) %>% select(Emissions) %>% sum()



plot_6 <- as.data.frame(plot_6)
plot_6$city <- c("baltimore", "baltimore", "baltimore", "baltimore", "LA", "LA", "LA", "LA")
colnames(plot_6) <- c("year", "total pm2.5 Emission", "city")
bal <- filter(plot_6, city == "baltimore")
L.A <- filter(plot_6, city == "LA")

plot(plot_6$year, plot_6$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission (1999-2008) from motor vehicle-related sources in LA and baltimore",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "p",
     cex.main=1, 
     color = "city")+
  lines(bal$year, bal$`total pm2.5 Emission`, col = "blue") +
  lines(L.A$year, L.A$`total pm2.5 Emission`, col = "red")

text(2006, 800, 
     label = 'red = Los Angelos 
              blue = Baltimore')

