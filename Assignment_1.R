#Sys.setenv(LANG = "en")
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
library(readr)
library(reshape2)
library(tidyverse)

### Descriptive statistics

dat <- read_csv("C:/Users/Administrator/Downloads/Forbes Richest Atheletes (1990-2020).csv")
describe(dat[,1:5])

dat <- dat %>% select(name, nationality, sport, earnings, current_rank)

## First boxplot by group to study the relationship between 'nationality' and 'earnings'
dat %>% select(name, nationality, sport, 
               earnings, current_rank) %>%
  ggplot(data = dat, mapping = aes(x = nationality, y = earnings))+ 
  geom_point() + geom_smooth(method="gam") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))
  labs(x="Characteristics", y="Value", 
       title="Earnings in different nationalities") +
  theme(plot.title=element_text(hjust=0.5))

## Second boxplot by group to study the relationship between 'sport' and 'earnings'
dat %>% select(name, nationality, sport, 
                earnings, current_rank) %>%
  ggplot(data = dat, mapping = aes(x = sport, y = earnings))+ 
  geom_point() + geom_smooth(method="gam") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))
  labs(x="Characteristics", y="Value", 
       title="Earnings in different sports") +
  theme(plot.title=element_text(hjust=0.5))
  
## Second boxplot by group to study the relationship between 'sport' and 'earnings'
dat %>% select(name, nationality, sport, 
                earnings, current_rank) %>%
  ggplot(data = dat, mapping = aes(x = sport, y = earnings))+ 
  geom_point() + geom_smooth(method="gam") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))
  labs(x="Characteristics", y="Value", 
       title="Earnings in current rankings") +
  theme(plot.title=element_text(hjust=0.5))
  
### Missing data analysis
