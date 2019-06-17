library(ggplot2)
library(ggthemes)

df <- read.csv("D:\\Programming\\DA\\Lab 2\\2014\\2014.csv", header = T)


ggplot(df) + theme_fivethirtyeight() +scale_shape_manual(values=1:nlevels(df$sn)) +
  geom_bar(aes(y = df$Live.births, x = Year, fill = Year), 
           stat = "identity", position = "dodge") 

