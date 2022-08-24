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

#this plots the graph without altering y-axis values 
normal <- ggplot(plot_3, aes(year, total_pm2.5)) +
  geom_point(alpha = 1/2, size = 2, colour = "blue") +
  geom_line() +
  facet_wrap(plot_3$type, nrow = 2, ncol = 2) +
  labs(title = "Baltimore city pm2.5 emission from 1998 to 2008 grouped by types of sources")
print(normal)

#because the values differs too much, I decide to use log to show the changes 
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