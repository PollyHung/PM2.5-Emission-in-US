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
table_1 <- plot_4[order(plot_4$year, plot_4$type), ]


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