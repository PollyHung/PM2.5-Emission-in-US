#The overall goal of this assignment is to explore the National Emissions Inventory 
#database and see what it say about fine particulate matter pollution in the United states 
#over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

##Questions
#You must address the following questions and tasks in your exploratory analysis. 
#For each question/task you will need to make a single plot. Unless specified, 
#you can use any plotting system in R to make your plot.

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#   (\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.

#3. Of the four types of sources indicated by the 
#   \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999–2008 for 
#   Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 
#   plotting system to make a plot answer this question.

#4. Across the United States, how have emissions from coal combustion-related sources 
#   changed from 1999–2008?

#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
  
#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#   vehicle sources in Los Angeles County, California 
#   (\color{red}{\verb|fips == "06037"|}fips == "06037"). 
#   Which city has seen greater changes over time in motor vehicle emissions?
  
  



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

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#   (\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.
maryland <- filter(pm2.5_emission, fips == "24510")
plot_2 <- matrix(NA, nrow = 4, ncol = 2)
plot_2[ , 1] <- as.numeric(unique(maryland$year))
plot_2[1, 2] <- maryland %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_2[2, 2] <- maryland %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_2[3, 2] <- maryland %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_2[4, 2] <- maryland %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_2 <- as.data.frame(plot_2)
colnames(plot_2) <- c("year", "total pm2.5 Emission")

plot(plot_2$year, plot_2$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 in Maryland",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")
for (i in 1:3) {
  text(plot_2$year[i] + 0.7, 
       plot_2$`total pm2.5 Emission`[i] + 50000, 
       labels = as.integer(plot_2$`total pm2.5 Emission`[i]))
}

text(plot_2$year[4]-0.7, plot_2$`total pm2.5 Emission`[4] + 50000, 
     labels = as.integer(plot_2$`total pm2.5 Emission`[4]))

#3. Of the four types of sources indicated by the 
#   \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999–2008 for 
#   Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 
#   plotting system to make a plot answer this question.
types <- as.list(unique(pm2.5_emission$type))
#first plot model 
plot_3 <- pm2.5_emission %>%
  group_by(type, year) %>%
  summarise(total_pm2.5 = sum(Emissions), na.rm = TRUE)
plot_3 <- plot_3[1:3] #trim the data frame, remove the na.rm 
main <-  c("Total PM2.5 Emission from Point", "Total PM2.5 Emission from Non-Point",
           "Total PM2.5 Emission from Road", "Total PM2.5 Emission from Non-Road")
#this is the base plotting system 
for (i in 1:4) {
  df_1 <- filter(plot_3, type == types[i])
  plot(df_1$year, df_1$total_pm2.5,
       main = main[i],
       xlab = "years",
       ylab = types[i], 
       type = "b")
}


#this is the ggplot2 system 
plot_3$type <- as.factor(plot_3$type)
class(plot_3$type)

normal <- ggplot(plot_3, aes(year, total_pm2.5)) +
  geom_point(alpha = 1/2, size = 2, colour = "blue") +
  geom_line() +
  facet_wrap(plot_3$type, nrow = 2, ncol = 2) +
  labs(title = "Baltimore city pm2.5 emission from 1998 to 2008 grouped by types of sources")
print(normal)

logged <- ggplot(plot_3, aes(year, log(total_pm2.5))) +
  geom_point(alpha = 1/2, size = 2, colour = "blue") +
  geom_line() +
  facet_wrap(plot_3$type, nrow = 2, ncol = 2) +
  labs(title = "Baltimore city pm2.5 emission from 1998 to 2008 grouped by types of sources")
print(logged)

#for some reason I can't add labels on to the facets individually 
dat_text <- data.frame(
  label = c("increase, then decrease since 2002", "decrease since 1998", 
            "decrease since 1998", "overall decrease with slight fluctuation"),
  group = unique(plot_3$type),
  x = c(2003, 2003, 2003, 2003), 
  y = c(14, 14, 14, 15)
)
dat_text

logged + 
  geom_text(data = dat_text, 
            mapping = aes(x = 2003, 
                          y = 14, 
                          label = label))



#4. Across the United States, how have emissions from coal combustion-related sources 
#   changed from 1999–2008?
scc_sn <- select(source_classification, SCC, Short.Name)
coal_sn <- as.data.frame(grep(pattern = "([Cc][Oo][Aa][Ll])", 
          scc_sn$Short.Name, value = TRUE))
scc_sn <- scc_sn[grep(pattern = "([Cc][Oo][Aa][Ll])", 
                      scc_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let scc match with pm2.5emission 
scc <- as.list(scc_sn[1])
scc <- unlist(scc)
table_1 <- pm2.5_emission[pm2.5_emission$SCC %in% scc, ]
table_1 <- table_1[order(plot_4$year, plot_4$type), ]


plot_4 <- matrix(NA, nrow = 4, ncol = 2)
plot_4[ , 1] <- as.numeric(unique(pm2.5_emission$year))
plot_4[1, 2] <- table_1 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_4[2, 2] <- table_1 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_4[3, 2] <- table_1 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_4[4, 2] <- table_1 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_4 <- as.data.frame(plot_4)
colnames(plot_4) <- c("year", "total pm2.5 Emission")

plot(plot_4$year, plot_4$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 from coal combustion-related sources",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")

for (i in 1:3){
  text(plot_4$year[i]+0.7, 
       plot_4$`total pm2.5 Emission`[i]-20000, 
       labels = as.integer(plot_4$`total pm2.5 Emission`[i]))
}
text(plot_4$year[4]-0.7, plot_4$`total pm2.5 Emission`[4], 
     labels = as.integer(plot_4$`total pm2.5 Emission`[4]))







#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
mv_sn <- select(source_classification, SCC, Short.Name)
mv_sn <- mv_sn[grep(pattern = "([Mm]otor [Vv]ehicles)", 
                    mv_sn$Short.Name), ] #selects out the rows that have "coal" in short.name
#let mv match with pm2.5emission 
mv <- as.list(mv_sn[1]) #data frame to list 
mv <- unlist(mv) #list to vector 
table_2 <- pm2.5_emission[pm2.5_emission$SCC %in% mv, ]
table_2 <- filter(table_2, fips == 24510) #select out baltimore city's data 


plot_5 <- matrix(NA, nrow = 4, ncol = 2)
plot_5[ , 1] <- as.numeric(unique(pm2.5_emission$year))
n <- as.numeric(unique(pm2.5_emission$year))
plot_5[1, 2] <- table_2 %>% filter(year == 1999) %>% select(Emissions) %>% sum() 
plot_5[2, 2] <- table_2 %>% filter(year == 2002) %>% select(Emissions) %>% sum()
plot_5[3, 2] <- table_2 %>% filter(year == 2005) %>% select(Emissions) %>% sum()
plot_5[4, 2] <- table_2 %>% filter(year == 2008) %>% select(Emissions) %>% sum()
plot_5 <- as.data.frame(plot_5)
colnames(plot_5) <- c("year", "total pm2.5 Emission")

plot(plot_5$year, plot_5$`total pm2.5 Emission`, 
     main = "Total PM2.5 Emission from 1999 to 2008 from motor vehicle-related sources in baltimore city",
     xlab="years", ylab = "total PM2.5 Emission", 
     type = "b")

for (i in 1:3){
  text(plot_5$year[i]+0.5, 
       plot_5$`total pm2.5 Emission`[i]-80, 
       labels = as.integer(plot_5$`total pm2.5 Emission`[i]))
}
text(plot_5$year[4], plot_5$`total pm2.5 Emission`[4] - 150, 
     labels = as.integer(plot_5$`total pm2.5 Emission`[4]))





#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#   vehicle sources in Los Angeles County, California 
#   (\color{red}{\verb|fips == "06037"|}fips == "06037"). 
#   Which city has seen greater changes over time in motor vehicle emissions?
#   baltimore: fips == "24510", los angeles: fips == "06037"
par(mfcol = c(1,2))

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







