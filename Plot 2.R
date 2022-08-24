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