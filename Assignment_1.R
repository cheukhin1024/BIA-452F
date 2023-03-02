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
library(ltm)

### Descriptive statistics

dat <- read_csv("C:/Users/Administrator/Downloads/Forbes Richest Atheletes (1990-2020).csv")
describe(dat[,1:5])

## Fixing the raw data input error
dat$sport[which(dat$sport=="ice hockey")] <- "Ice Hockey"


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
  
## Third boxplot by group to study the relationship between 'sport' and 'earnings'
dat %>% select(name, nationality, sport, 
                earnings, current_rank) %>%
  ggplot(data = dat, mapping = aes(x = current_rank, y = earnings))+ 
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

"""
## Kruskal-Wallis test is usesd because there are some categorical variables and they are not dichotomous.

set.seed(123)
kruskal.test(x~y)
"""

## Convert string english character factors into integers

# Convert 'nationality' character strings into integers
dat_comp$nationality <- as.character(dat_comp$nationality)
dat_comp$nationality[which(dat_comp$nationality=="Argentina")] <- "1"
dat_comp$nationality[which(dat_comp$nationality=="Australia")] <- "2"
dat_comp$nationality[which(dat_comp$nationality=="Austria")] <- "3"
dat_comp$nationality[which(dat_comp$nationality=="Brazil")] <- "4"
dat_comp$nationality[which(dat_comp$nationality=="Canada")] <- "5"
dat_comp$nationality[which(dat_comp$nationality=="Dominican")] <- "6"
dat_comp$nationality[which(dat_comp$nationality=="Filipino")] <- "7"
dat_comp$nationality[which(dat_comp$nationality=="Finland")] <- "8"
dat_comp$nationality[which(dat_comp$nationality=="France")] <- "9"
dat_comp$nationality[which(dat_comp$nationality=="Germany")] <- "10"
dat_comp$nationality[which(dat_comp$nationality=="Ireland")] <- "11"
dat_comp$nationality[which(dat_comp$nationality=="Italy")] <- "12"
dat_comp$nationality[which(dat_comp$nationality=="Mexico")] <- "13"
dat_comp$nationality[which(dat_comp$nationality=="Northern Ireland")] <- "14"
dat_comp$nationality[which(dat_comp$nationality=="Philippines")] <- "15"
dat_comp$nationality[which(dat_comp$nationality=="Portugal")] <- "16"
dat_comp$nationality[which(dat_comp$nationality=="Russia")] <- "17"
dat_comp$nationality[which(dat_comp$nationality=="Serbia")] <- "18"
dat_comp$nationality[which(dat_comp$nationality=="Spain")] <- "19"
dat_comp$nationality[which(dat_comp$nationality=="Switzerland")] <- "20"
dat_comp$nationality[which(dat_comp$nationality=="UK")] <- "21"
dat_comp$nationality[which(dat_comp$nationality=="USA")] <- "22"
dat_comp$nationality <- as.numeric(dat_comp$nationality)

# Convert 'sport' character strings into integers
dat_comp$sport <- as.character(dat_comp$sport)
dat_comp$sport[which(dat_comp$sport=="American Football")] <- "1"
dat_comp$sport[which(dat_comp$sport=="American Football / Baseball")] <- "2"
dat_comp$sport[which(dat_comp$sport=="Auto Racing")] <- "3"
dat_comp$sport[which(dat_comp$sport=="Baseball")] <- "4"
dat_comp$sport[which(dat_comp$sport=="Basketball")] <- "5"
dat_comp$sport[which(dat_comp$sport=="Boxing")] <- "6"
dat_comp$sport[which(dat_comp$sport=="Cycling")] <- "7"
dat_comp$sport[which(dat_comp$sport=="F1 Motorsports")] <- "8"
dat_comp$sport[which(dat_comp$sport=="F1 racing")] <- "9"
dat_comp$sport[which(dat_comp$sport=="Golf")] <- "10"
dat_comp$sport[which(dat_comp$sport=="Hockey")] <- "11"
dat_comp$sport[which(dat_comp$sport=="Ice Hockey")] <- "12"
dat_comp$sport[which(dat_comp$sport=="MMA")] <- "13"
dat_comp$sport[which(dat_comp$sport=="motorcycle gp")] <- "14"
dat_comp$sport[which(dat_comp$sport=="NASCAR")] <- "15"
dat_comp$sport[which(dat_comp$sport=="NBA")] <- "16"
dat_comp$sport[which(dat_comp$sport=="NFL")] <- "17"
dat_comp$sport[which(dat_comp$sport=="Soccer")] <- "18"
dat_comp$sport[which(dat_comp$sport=="Tennis")] <- "19"
dat_comp$sport <- as.numeric(dat_comp$sport)

## Correlation analysis: Heat Map
dat_comp %>% 
  select(nationality, sport, 
         earnings, current_rank) %>%
  cor() %>%
  round(3) %>%
  corrplot(method = "color", addCoef.col="white", type = "upper", 
           title="Correlation Matrix Among Factors",
           mar=c(0,0,2,0),
           tl.cex=0.5, number.cex = 0.4)

## Scatter plot between related variables
x<-dat_comp %>% select(nationality, sport, 
                       earnings, current_rank) %>%
  cor() 
  round(x,3)
  
dat_comp %>% select(nationality, sport, earnings, current_rank) %>%
  scatterplotMatrix()

### Hypothesis and evaluation

dat_comp %>% select(nationality, sport, earnings, current_rank) %>% sapply(quantile)

dat_new <- dat_comp %>% mutate(p_group=ifelse(nationality>=22, "High", 
                                              ifelse(nationality<22 & nationality>=20, "Medium", "Low")),
                               pf_group=ifelse(sport>=10, "High", 
                                               ifelse(sport<10 & sport>5, "Medium", "Low")),
                               e_group=ifelse(earnings>=59.4, "High", 
                                               ifelse(earnings<59.4 & earnings>24.0, "Medium", "Low")),
                               b_group=ifelse(current_rank>=8, "High", 
                                              ifelse(current_rank<=8 & current_rank>3, "Medium", "Low")))

# reorder the levels
dat_new$p_group <- factor(dat_new$p_group, levels=c("High", "Medium", "Low"))
dat_new$pf_group <- factor(dat_new$pf_group, levels=c("High", "Medium", "Low"))
dat_new$e_group <- factor(dat_new$e_group, levels=c("High", "Medium", "Low"))
dat_new$b_group <- factor(dat_new$b_group, levels=c("High", "Medium", "Low"))

## count the # for each group

## nationality:
cat("nationality:")
table(dat_new$p_group)

## sport:
cat("sport:")
table(dat_new$pf_group)

## earnings:
cat("earnings:")
table(dat_new$e_group)

## current_rank:
cat("current_rank:")
table(dat_new$b_group)

### H1 - installment and installment frequency
