library(ggplot2)
library(ggthemes)
library(pdftools)
library(cowplot)


df <- read.csv("D:\\Programming\\DA\\Lab 2\\2011\\2011.csv", header = T)

summary(df)

var(df$Birth.Registered)
sd(df$Birth.Registered)

head(df)
tail(df)

a <- ggplot(df, aes(Birth.Rate, Birth.Registered, colour = "Birth"))+ 
  geom_point()+ ggtitle("Birth registered VS rate")+theme_bw()+
  theme(legend.position = c(0.8, 0.8))+geom_line()
b <- ggplot(df, aes(Death.Rate, Death.Regesterd, colour = "Death"))+ 
  geom_point()+ ggtitle("Death registered VS rate")+theme_bw()+
  theme(legend.position = c(0.8, 0.8))+geom_line()
c <- ggplot(df, aes(Still.Birth.Rate, Still.Birth.Registered, colour = "Still Birth")) +
  geom_point()+ ggtitle("Still Birth registered VS rate")+theme_bw()+
  theme(legend.position = c(0.3, 0.8))+geom_line()
d <- ggplot(df, aes(Sr..No, Registered.Infant.Death, colour = "Infant"))+ 
  geom_point()+ ggtitle("Still Infant registered VS rate") + theme_bw()+
  theme(legend.position = c(0.3, 0.8))+geom_line()

plot_grid(a, b,c,d, labels = "AUTO")

p <- ggplot(df, aes(Birth.Registered)) + geom_histogram(colour="green") + 
  ggtitle("Birth Registered") 
q <- ggplot(df, aes(Birth.Rate)) + geom_histogram(colour="green") + 
  ggtitle("Birth Rate") 
r <- ggplot(df, aes(Death.Regesterd)) + geom_histogram(colour="green") + 
  ggtitle("Death Registered") 
s <- ggplot(df, aes(Death.Rate)) + geom_histogram(colour="green") + 
  ggtitle("Death Rate") 
t <- ggplot(df, aes(Registered.Infant.Death)) + geom_histogram(colour="green")+
  ggtitle("Registered Infant Death") 
u <- ggplot(df, aes(Still.Birth.Registered)) + geom_histogram(colour="green") +
  ggtitle("Still Birth Registered") 
v <- ggplot(df, aes(Still.Birth.Rate)) + 
  geom_histogram(colour="green") + ggtitle("Still Birth Rate") 

plot_grid(p,q,r,s, labels = "AUTO")
plot_grid(t,u,v, labels = "AUTO")

g <- ggplot(df, aes(Birth.Registered, Birth.Rate)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", 
               outlier.shape = 1)+  geom_jitter(width = 0.2)
y <- ggplot(df, aes(Death.Regesterd, Death.Rate)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", 
               outlier.shape = 1)+geom_jitter(width = 0.2)
z <- ggplot(df, aes(Still.Birth.Registered, Still.Birth.Rate)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red", 
               outlier.shape = 1)+geom_jitter(width = 0.2)

plot_grid(g,y,z, labels = "AUTO")


aa <- qqplot(df$Birth.Registered, df$Birth.Rate, col = "green")
bb <- qqplot(df$Death.Regesterd,df$Death.Rate, col = "red")
cc <- qqplot(df$Still.Birth.Registered, df$Still.Birth.Rate, col = "blue")



ggplot(df) + theme_fivethirtyeight(base_size = 12, base_family = "sans") +
  scale_shape_manual(values=1:nlevels(df$sn)) +
  geom_bar(aes(y = Birth.Registered, x = Sr..No, fill = District), 
           stat = "identity", position = "dodge") 

ggplot(df, aes(District ,Birth.Registered)) +
  theme(axis.text.x = element_text( angle = 45, hjust = 1))+
  geom_point(colour="black", size = 3.75) +
  geom_point(colour="blue", size = 2.5) 


