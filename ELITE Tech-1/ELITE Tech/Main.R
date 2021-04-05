#Shreeniket Bendre
#07/29/2020
#Avocado Data

#Library / Package Inclusion
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("insight")

library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(splines)

#Importing Data
data <- read.csv("~/workspace/ELITE Tech/avocado.csv", header = TRUE)
View (data)

#Raw Plot 
ggplot(data = data, aes(Date, AveragePrice, colour = "blue")) + geom_point()
ggplot(data = data, aes(Date, AveragePrice, colour = "blue")) + geom_line()
ggplot(data = data, aes(Date, AveragePrice)) + geom_bar(stat='identity')
#Remove NA
#Summary to count num of NA in each column
summary(data)
#Find location of NA in rev
data[is.na(data$AveragePrice),]
data <- data[!is.na(data$AveragePrice),]
View(data)

#Mean Plot
means_rev <- aggregate(AveragePrice ~ Date, data, mean)

ggplot(data = data, aes(x=Date, y=AveragePrice, fill=AveragePrice)) + geom_point() + stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 3, show.legend = FALSE) + geom_text(data = means_rev, aes(label = AveragePrice, y = AveragePrice + 0.08), label=sprintf("%0.1f", round(means_rev[,2]), digit = 2), size = 3, nudge_y = 0.5)

view(data)

#Crop -- data to same year ranges
#Ranges 2013-2018


#Hypothesis Gen
null <- lmer(AveragePrice ~ (1|Date), data, REML = FALSE)
alt1 <- lmer(AveragePrice ~ (1|Date) + region, data, REML = FALSE)
alt2 <- lmer(AveragePrice ~ (1|Date) + type, data, REML = FALSE)
alt3 <- lmer(AveragePrice ~ (1|Date) + region + type, data, REML = FALSE)


#data <- dplyr::filter(data, Year >= 2013, Year <= 2018)
View(data)

#Anova-Test

anova(null, alt1)
#alt1>null
anova(null, alt2)
#alt2>null
anova(null, alt3)
#alt3>null
anova(alt1, alt2)
#alt2==alt1
anova(alt1, alt3)
#alt3>alt1
anova(alt2, alt3)
#alt3>alt2

#alt 3 is best

#Residual Plot
avocado.lm = lm(AveragePrice ~ Date, data=data) 
avocado.res = resid(avocado.lm)

plot(data$AveragePrice, avocado.res, ylab="Price", xlab="Date", main="Avocado Price to Date") 
abline(0, 0)

#Coefficient of Determination
summary(avocado.lm)$r.squared 


