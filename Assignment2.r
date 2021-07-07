# Introduction to Data Science 11372

# Raavin Ashwath Sundar Rajan - u3189852

# Part A, B, C of Assignment 2 - Data Wrangling and Modelling.

setwd("C:/Users/ashus/Desktop/u3189852_DataScience")

# Libraries

library(tidyverse)
library(modelr)
library(caret)
library(tidyverse)
library(lubridate)
library(readr)
library(plyr)
library(tidyr)
library(dplyr)
library(GGally)


                    ##################### Part A #####################


#Question1:
#data files (you need to specify the paths of the CSV files (e.g. relative or absolute) )
#Load the participants and the diet groups data into your working directory
group1<- read_csv("data/Group1.csv")
group2<- read_csv("data/Group2.csv")
group3<- read_csv("data/Group3.csv")
group4<- read_csv("data/Group4.csv")
group5<- read_csv("data/Group5.csv")
group6<- read_csv("data/Group6.csv")
group7<- read_csv("data/Group7.csv")
group8<- read_csv("data/Group8.csv")
group9<- read_csv("data/Group9.csv")
group10<-read_csv("data/Group10.csv")

group_files <- bind_rows(group1,group2,group3,group4,group5,group6,group7,group8,group9,group10)

participants_file <- read_csv("data/participants.csv")
test_participant_file <- read_csv("data/test_participants.csv" )

#Question2
G1<- sum(group1$calories)
G1

G2<- sum(group2$calories)
G2

G3<- sum(group3$calories)
G3

G4<- sum(group4$calories)
G4

G5<- sum(group5$calories)
G5

G6<- sum(group6$calories)
G6

G7<- sum(group7$calories)
G7

G8<- sum(group8$calories)
G8

G9<- sum(group9$calories)
G9

G10<- sum(group10$calories)
G10


#Question3
group <- data.frame(
  Total_calories =c(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10),
  Group =c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10"))

x<- merge(participants_file,group, by.x ="Groups", by.y = "Group")


#Question4 & Question5
participants_file<- mutate(x, Comsumed_Calories = x$Total_calories +x$ExtraCalories)



                        ##################### Part B ####################


#Question1
gender_subject_freq <-table(participants_file$Gender, participants_file$Groups)
gender_subject_freq

#Question2
proportions <- prop.table(gender_subject_freq)
proportions

#Question3
proportions["F", "G5"] /margin.table(proportions, 2)["G5"] *100

#Question4
proportions["M", "G5"] / margin.table(proportions, 1)["M"] *100

#Question5
ggplot(participants_file, aes(x = participants_file$Groups, fill = participants_file$Gender)) +
  geom_bar(position = "dodge")

#Question6
#GainedWeights
GW_quartiles <- quantile(participants_file$GW)
GW_quartiles

GW_lower_q <- GW_quartiles[2]
GW_lower_q

GW_upper_q <- GW_quartiles[4]
GW_upper_q

IQR <- GW_upper_q - GW_lower_q
IQR

GW_lowest <- GW_lower_q - 1.5 * IQR
GW_lowest

GW_highest <- GW_upper_q + 1.5 * IQR
GW_highest


#Consumed Calories
Consumed_Calories_quartiles <- quantile(participants_file$Comsumed_Calories)
Consumed_Calories_quartiles

Consumed_Calories_lower_q <- Consumed_Calories_quartiles[2]
Consumed_Calories_lower_q

Consumed_Calories_upper_q <- Consumed_Calories_quartiles[4]
Consumed_Calories_upper_q

IQR <- Consumed_Calories_upper_q - Consumed_Calories_lower_q
IQR

Consumed_Calories_lowest <- Consumed_Calories_lower_q - 1.5 * IQR
Consumed_Calories_lowest

Consumed_Calories_highest <- Consumed_Calories_upper_q + 1.5 * IQR
Consumed_Calories_highest


#Question7
df_male <- subset(participants_file, Gender == "M")
df_female <- subset(participants_file, Gender == "F")
mean(df_male$GW)
mean(df_female$GW)

#Question8
ggplot() +
  geom_density(data= participants_file, aes(x=GW, group= participants_file$Gender, color=Gender), adjust=2) +
  xlab("Gender") +
  ylab("Density")+
  theme_classic()

#Histogram
ggplot(participants_file, aes(participants_file$GW, group = participants_file$Gender)) +
  geom_histogram(aes(y= ..density..), bins = 30, fill = "aquamarine3") +
  geom_density(color = "red")

#Question9
gg<- ggplot(participants_file, aes(x = Gender, y = participants_file$GW)) +
  geom_boxplot(outlier.color = "red",outlier.size = 1, outlier.shape = 20)+
  xlab("Gender") +
  ylab("Gained Weight")
gg

#Question10
GW_Boxplot<- boxplot(participants_file$GW)
outliers<- participants_file[participants_file$GW < GW_Boxplot$stats[1] | participants_file$GW > GW_Boxplot$stats[5],]
outliers


                          
                          ##################### Part C #####################



#Question1
cor(participants_file$GW,participants_file$Comsumed_Calories)

#Question2
ggplot(participants_file, aes(x = participants_file$Comsumed_Calories , y = participants_file$GW ))+
  geom_point() +stat_smooth(method = lm)

#Question3
lmModel1 <- lm(GW~Comsumed_Calories , data = participants_file)
print(lmModel1)
summary(lmModel1)

#Question4
lmModel1$coefficients

#Question5
#R-squared and adjacent R-squared values indicate that the intercept of the explanatory variable can explain ~48% of the variance in the response variable.

#Question6
ggplot(lmModel1, aes(lmModel1$residuals)) +
  geom_histogram(aes(y = ..density..), fill = "#C99800") +
  geom_density(color = "red")

#Question7
x1<- merge(test_participant_file,group, by.x ="Groups", by.y = "Group")
test_participant_file<- mutate(x1, Comsumed_Calories = x1$Total_calories +x1$ExtraCalories)
test_participant_file$PredictedGW <- predict(lmModel1, test_participant_file)
view(test_participant_file)

#Question8
predictionInterval<- predict(lmModel1, test_participant_file, interval = 'prediction')
data <- cbind(test_participant_file, predictionInterval)

pi <- ggplot(data, aes(GW,PredictedGW)) + geom_point() + stat_smooth(method = lm)
pi + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

