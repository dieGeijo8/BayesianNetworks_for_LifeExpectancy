#required libraries
library(ggcorrplot)
library(dagitty)
library(dplyr)
library(dlookr)
library(ggplot2)
library(visdat)
library(lavaan)


# load the dataset
setwd('./Desktop/bayesian/assignment1/')
lexp <- read.csv("who_life_exp.csv", header = TRUE, stringsAsFactors = TRUE)

# checks 
str(lexp)
nrow(lexp)
ncol(lexp)
# 3111 rows, 32 variables

# variable subsetting 
columns <- c("life_expect", "adult_mortality", "infant_mort", "alcohol", "bmi", "polio", "gghe.d")
lexp2 <- lexp[columns]

# summary analysis
str(lexp2)
summary(lexp2)

# missing values analysis
vis_miss(lexp2)
lexp3 <- lexp2

vis_miss(lexp3)
diagnose(lexp3)
print(nrow(na.omit(lexp3)) / nrow(lexp3))

# remove the missing values
lexp4 <- na.omit(lexp3)
str(lexp4)
nrow(lexp4)
ncol(lexp4)


# outlier analysis
diagnose_outlier(lexp4)
# no measure applied

# 2950 rows, 9 variables 

# plot the distributions
My_theme_2 <- function() {
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))
}
p1 <- ggplot(lexp4, aes(x=life_expect)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p2 <- ggplot(lexp4, aes(x=adult_mortality)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p3 <- ggplot(lexp4, aes(x=infant_mort)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p4 <- ggplot(lexp4, aes(x=alcohol)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p5 <- ggplot(lexp4, aes(x=bmi)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p6 <- ggplot(lexp4, aes(x=polio)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p7 <- ggplot(lexp4, aes(x=basic_water)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
p8 <- ggplot(lexp4, aes(x=gghe.d)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + My_theme_2()
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=3)

# correlation matrix
m1<-lavCor(lexp4)
m1
varTable(lexp4)


### BUILDING NET      

g1 <- dagitty('dag {
  adult_mortality [pos="-1.555,-1.249"]
  alcohol [pos="0.462,-0.383"]
  bmi [pos="0.300,0.281"]
  gghe.d [pos="-1.988,0.231"]
  infant_mort [pos="0.138,-1.061"]
  life_expect [outcome,pos="-0.786,1.193"]
  polio [pos="-0.785,-1.446"]
  adult_mortality -> life_expect
  adult_mortality <-> gghe.d
  alcohol -> adult_mortality
  alcohol -> bmi
  alcohol -> gghe.d
  alcohol -> life_expect
  bmi -> adult_mortality
  bmi <-> infant_mort
  bmi <-> polio
  gghe.d -> bmi
  gghe.d -> life_expect
  gghe.d -> polio
  gghe.d <-> infant_mort
  infant_mort -> life_expect
  polio -> adult_mortality
  polio -> infant_mort
  }
')
res1 <- localTests(g1, lexp4, type='cis')
print(res1)
plot(g1)

# add alcoh<-> polio
g2 <- dagitty('dag {
adult_mortality [pos="-1.555,-1.249"]
alcohol [pos="0.462,-0.383"]
bmi [pos="0.300,0.281"]
gghe.d [pos="-1.750,0.139"]
infant_mort [pos="0.202,-1.357"]
life_expect [outcome,pos="-0.786,1.193"]
polio [pos="-0.785,-1.446"]
adult_mortality -> life_expect
adult_mortality <-> gghe.d
alcohol -> adult_mortality
alcohol -> bmi
alcohol <-> polio
alcohol -> gghe.d
alcohol -> life_expect
bmi -> adult_mortality
bmi <-> infant_mort
bmi <-> polio
gghe.d -> bmi
gghe.d -> life_expect
gghe.d -> polio
gghe.d <-> infant_mort
infant_mort -> life_expect
polio -> adult_mortality
polio -> infant_mort
}
')
res2 <- localTests(g2, lexp4, type='cis')
print(res2)

# FINAL NET
# remove alcoh->bmi
g3 <- dagitty('dag {
  adult_mortality [pos="-1.555,-1.249"]
  alcohol [pos="0.462,-0.383"]
  bmi [pos="0.300,0.281"]
  gghe.d [pos="-1.750,0.139"]
  infant_mort [pos="0.202,-1.357"]
  life_expect [outcome,pos="-0.786,1.193"]
  polio [pos="-0.785,-1.446"]
  adult_mortality -> life_expect
  adult_mortality <-> gghe.d
  alcohol -> adult_mortality
  alcohol -> gghe.d
  alcohol <-> polio
  alcohol -> life_expect
  bmi -> adult_mortality
  bmi <-> infant_mort
  bmi <-> polio 
  gghe.d -> bmi
  gghe.d -> life_expect
  gghe.d -> polio
  gghe.d <-> infant_mort
  infant_mort -> life_expect
  polio -> adult_mortality
  polio -> infant_mort
  }')
res3 <- localTests(g3, lexp4, type='cis')
print(res3)

plot(g3)

# FITTING

fit<-sem(toString(g3,'lavaan'), sample.cov = m1, sample.nobs = nrow(lexp4))
summary(fit)
fg <- lavaanToGraph(fit, digits = 2)
plot(fg, show.coefficients = T)


plot(lexp4$life_expect,lexp4$alcohol,col='blue')
abline(lm(lexp4$alcohol ~ lexp4$life_expect))
lm(lexp4$alcohol ~ lexp4$life_expect)
abline(65.1151, 0.8763, col='red')
