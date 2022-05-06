#Otis Idlebird
#Final Project R Script 

library(dslabs)
library(dplyr)
library(ggplot2)
library(tidyverse) 
#The code below shows the raw data from the 'fatality' data set I recovered.
urlfile <- 'https://raw.githubusercontent.com/idlebird03/IS4300-Final-Project/main/Fatality.csv'

RC <- read.csv(urlfile)

#Code below changes the Column name of data set to make it a bit more understanding.
names(RC) <- c("ID" , "State", "Year", "FatalRate" , "BeerTax" , "MinDA" , "Jaild" ,
                  "Comserd" , "Vmiles" , "UnRate" , "PerInc")
DUI <- as.data.frame(RC)

#To only select the columns I need, I entered the code below.
DUI <- DUI %>% select(3, 10, 11)
head(DUI)

#The code for the Unemployment Rate histogram.
DUI1 <- DUI$UnRate
hist(DUI1, col = "Red", main = "Unemployment Rate" , labels = TRUE)

#The code for the Personal Income Histogram. 
DUI2 <- DUI$PerInc
hist(DUI2, col = "darkgreen", main = "Personal Income", labels = TRUE)

#The code for the relationship between UnRate and PerInc scatterplot.
RatesPlot <- DUI %>% ggplot()
RatesPlot <- RatesPlot + geom_point(aes(UnRate, PerInc, col = "Orange"), size = 2.5) +
  geom_text(aes(UnRate, PerInc), label = DUI$Year, size = 2.0) + ggtitle("Relationship of Unemployment and Personal Income")
RatesPlot

#Box plot for the first two histograms which were unsuccsessful. (Not included in the RMD file)
boxplot(DUI$UnRate, DUI$PerInc, main = "Unemployment Rate vs Personal Income", horizontal = TRUE ,
        at = c(1,2), names =c("Unemployment Rate", "Personal Income"), col = c("red", "darkgreen"))

#T Test code for the Unemployment rate and Personal Income
DUITest <- t.test(DUI$UnRate, DUI$PerInc)
DUITest

