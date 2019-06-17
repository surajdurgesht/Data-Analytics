odibatting2007 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2007odibattingrating.csv")
odibatting2008 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2008odibattingrating.csv")
odibatting2009 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2009odibattingrating.csv")
odibatting2010 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2010odibattingrating.csv")
odibatting2011 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2011odibattingrating.csv")
odibatting2012 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2012odibattingrating.csv")
odibatting2013 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2013odibattingrating.csv")
odibatting2014 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2014odibattingrating.csv")
odibatting2015 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2015odibattingrating.csv")
odibatting2016 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2016odibattingrating.csv")

testbatting2007 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2007testbattingrating.csv")
testbatting2008 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2008testbattingrating.csv")
testbatting2009 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2009testbattingrating.csv")
testbatting2010 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2010testbattingrating.csv")
testbatting2011 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2011testbattingrating.csv")
testbatting2012 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2012testbattingrating.csv")
testbatting2013 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2013testbattingrating.csv")
testbatting2014 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2014testbattingrating.csv")
testbatting2015 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2015testbattingrating.csv")
testbatting2016 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2016testbattingrating.csv")

twentybatting2007 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2007twenty20battingrating.csv")
twentybatting2008 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2008twenty20battingrating.csv")
twentybatting2009 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2009twenty20battingrating.csv")
twentybatting2010 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2010twenty20battingrating.csv")
twentybatting2011 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2011twenty20battingrating.csv")
twentybatting2012 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2012twenty20battingrating.csv")
twentybatting2013 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2013twenty20battingrating.csv")
twentybatting2014 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2014twenty20battingrating.csv")
twentybatting2015 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2015twenty20battingrating.csv")
twentybatting2016 <- read.csv("D:\\Programming\\DA\\Lab 4\\Player Ratings\\2016twenty20battingrating.csv")


library(dplyr)
dataBatting <- bind_rows(odibatting2007, odibatting2008, odibatting2009, odibatting2010,
                            odibatting2011, odibatting2012, odibatting2013, odibatting2014,
                            odibatting2015, odibatting2016,
                            testbatting2007, testbatting2008, testbatting2009, testbatting2010,
                            testbatting2011, testbatting2012, testbatting2013, testbatting2014,
                            testbatting2015, testbatting2016,
                         twentybatting2007, twentybatting2008, twentybatting2009,twentybatting2010,
                         twentybatting2011,twentybatting2012,twentybatting2013,twentybatting2014,
                         twentybatting2015,twentybatting2016)

summary(dataBatting)

library(VIM)
aggr(dataBatting)

dataBatting <- dataBatting %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

batcluster <- kmeans(dataBatting[, 2], 5)

batcluster$cluster <- as.factor(batcluster$cluster)

str(batcluster)

library(ggplot2)
ggplot(dataBatting, aes(dataBatting$Name, avg, color = batcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Good", "Avarage", "Useless", "Best", "Better")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(" Batting Ratings(2007-2016)")



