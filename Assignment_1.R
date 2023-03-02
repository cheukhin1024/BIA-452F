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
library(r2r)

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
  
## Missing by count
cat("Missing by count\n")
sapply(dat[,1:5], function(x) round(sum(is.na(x)),2))

## Missing by percentage
cat("Missing by percentage\n")
sapply(dat[,1:5], function(x) round(sum(is.na(x))/nrow(dat),2))

aggr(dat, prop=FALSE, numbers=TRUE)
aggr(dat, prop=TRUE, numbers=FALSE)

matrixplot(dat, interactive=FALSE) # without sorting
dat_comp <- dat[complete.cases(dat),]

## Convert string english character factors into integers

# Convert 'nationality' character strings into integers
dat$nationality <- as.character(dat$nationality)
dat$nationality[which(dat$nationality=="Argentina")] <- "1"
dat$nationality[which(dat$nationality=="Australia")] <- "2"
dat$nationality[which(dat$nationality=="Austria")] <- "3"
dat$nationality[which(dat$nationality=="Brazil")] <- "4"
dat$nationality[which(dat$nationality=="Canada")] <- "5"
dat$nationality[which(dat$nationality=="Dominican")] <- "6"
dat$nationality[which(dat$nationality=="Filipino")] <- "7"
dat$nationality[which(dat$nationality=="Finland")] <- "8"
dat$nationality[which(dat$nationality=="France")] <- "9"
dat$nationality[which(dat$nationality=="Germany")] <- "10"
dat$nationality[which(dat$nationality=="Ireland")] <- "11"
dat$nationality[which(dat$nationality=="Italy")] <- "12"
dat$nationality[which(dat$nationality=="Mexico")] <- "13"
dat$nationality[which(dat$nationality=="Northern Ireland")] <- "14"
dat$nationality[which(dat$nationality=="Philippines")] <- "15"
dat$nationality[which(dat$nationality=="Portugal")] <- "16"
dat$nationality[which(dat$nationality=="Russia")] <- "17"
dat$nationality[which(dat$nationality=="Serbia")] <- "18"
dat$nationality[which(dat$nationality=="Spain")] <- "19"
dat$nationality[which(dat$nationality=="Switzerland")] <- "20"
dat$nationality[which(dat$nationality=="UK")] <- "21"
dat$nationality[which(dat$nationality=="USA")] <- "22"
dat$nationality <- as.numeric(dat$nationality)

# Convert 'sport' character strings into integers
dat$sport <- as.character(dat$sport)
dat$sport[which(dat$sport=="American Football")] <- "1"
dat$sport[which(dat$sport=="American Football / Baseball")] <- "2"
dat$sport[which(dat$sport=="Auto Racing")] <- "3"
dat$sport[which(dat$sport=="Baseball")] <- "4"
dat$sport[which(dat$sport=="Basketball")] <- "5"
dat$sport[which(dat$sport=="Boxing")] <- "6"
dat$sport[which(dat$sport=="Cycling")] <- "7"
dat$sport[which(dat$sport=="F1 Motorsports")] <- "8"
dat$sport[which(dat$sport=="F1 racing")] <- "9"
dat$sport[which(dat$sport=="Golf")] <- "10"
dat$sport[which(dat$sport=="Hockey")] <- "11"
dat$sport[which(dat$sport=="ice hockey")] <- "12"
dat$sport[which(dat$sport=="")] <- "13"
dat$sport[which(dat$sport=="Northern Ireland")] <- "14"
dat$sport[which(dat$sport=="Philippines")] <- "15"
dat$sport[which(dat$sport=="Portugal")] <- "16"
dat$sport[which(dat$sport=="Russia")] <- "17"
dat$sport[which(dat$sport=="Serbia")] <- "18"
dat$sport[which(dat$sport=="Spain")] <- "19"
dat$sport[which(dat$sport=="Switzerland")] <- "20"
dat$sport[which(dat$sport=="UK")] <- "21"
dat$sport[which(dat$sport=="USA")] <- "22"
dat$sport <- as.numeric(dat$sport)

## Correlation analysis
dat_comp %>% 
  select(name, nationality, sport, 
         earnings, current_rank) %>%
  cor() %>%
  round(3) %>%
  corrplot(method = "color", addCoef.col="white", type = "upper", 
           title="Correlation Matrix Among Factors",
           mar=c(0,0,2,0),
           tl.cex=0.5, number.cex = 0.4)
