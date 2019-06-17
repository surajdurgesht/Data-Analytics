library(ggplot2)
library(ggthemes)

df <- read.csv("D:\\Programming\\DA\\2015\\2015_2.csv")
View(df)
summary(df)

head(df)
tail(df)

ggplot(df) + theme_fivethirtyeight(base_size = 12, base_family = "sans") +scale_shape_manual(values=1:nlevels(df$sn)) +
  geom_bar(aes(y = Live.births, x = Years, fill = Years), 
           stat = "identity", position = "dodge") 

df <- data.frame(
  gp = factor(rep(letters[1:9], each = 10)),
  y = rnorm(30)
)
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))


ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
ggplot(df) +
  geom_point(aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3)
ggplot() +
  geom_point(data = df, aes(gp, y)) +
  geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
  geom_errorbar(
    data = ds,
    aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
    colour = 'red',
    width = 0.4
  )


