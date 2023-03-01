library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)
library(VIM)
library(gridExtra)
library(car)
library(knitr)
library(gmodels)
library(readxl)

dat <- read_csv("C:/Users/s1224442/Downloads/Forbes Richest Atheletes (1990-2020).csv")
describe(dat[,1:5])

dat <- dat %>% select(name, nationality, sport, earnings, current_rank)

## boxplot by group
dat %>% select(name, nationality, sport, 
               earnings, current_rank) %>%
  reshape2::melt(id="name") %>%
  ggplot(aes(x=variable, y=value, fill=variable))  + 
  geom_boxplot() +
  labs(x="nationality", y="earnings", 
       title="Earnings in different countries") +
  theme(plot.title=element_text(hjust=1.0))
