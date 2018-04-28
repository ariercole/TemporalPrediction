###    Load data and set up dataSet       ###
###          CM 2018 - 01 - 19            ###

library(readr)
library(readxl)
library(dplyr)

rawData = read_csv("dataset/dataSet.csv")

# reconstruct data set
dataSet <- mutate(rawData, 
                  alive = factor(alive_dead_icu, levels = c('D', 'A'), labels = c(0,1)),
                  sex = as.factor(sex),
                  day1Ventilated = as.factor(day1Ventilated),
                  day2Ventilated = as.factor(day2Ventilated),
                  day3Ventilated = as.factor(day3Ventilated),
                  day4Ventilated = as.factor(day4Ventilated),
                  day5Ventilated = as.factor(day5Ventilated),
                  day1AdrenalineTotal = as.numeric(day1AdrenalineTotal),
                  day2AdrenalineTotal = as.numeric(day2AdrenalineTotal),
                  day3AdrenalineTotal = as.numeric(day3AdrenalineTotal),
                  day4AdrenalineTotal = as.numeric(day4AdrenalineTotal),
                  day5AdrenalineTotal = as.numeric(day5AdrenalineTotal),
                  day1VasopressinTotal = as.numeric(day1VasopressinTotal),
                  day2VasopressinTotal = as.numeric(day2VasopressinTotal),
                  day3VasopressinTotal = as.numeric(day3VasopressinTotal),
                  day4VasopressinTotal = as.numeric(day4VasopressinTotal),
                  day5VasopressinTotal = as.numeric(day5VasopressinTotal),
                  day1minLactate = as.numeric(day1minLactate),
                  day2minLactate = as.numeric(day2minLactate),
                  day3minLactate = as.numeric(day3minLactate),
                  day4minLactate = as.numeric(day4minLactate),
                  day5minLactate = as.numeric(day5minLactate),
                  day1maxLactate = as.numeric(day1maxLactate),
                  day2maxLactate = as.numeric(day2maxLactate),
                  day3maxLactate = as.numeric(day3maxLactate),
                  day4maxLactate = as.numeric(day4maxLactate),
                  day5maxLactate = as.numeric(day5maxLactate))


### load list of variables
allVariablesDF <- read_excel("dataset/allVariableList.xlsx")
allVariablesDF$day <- as.integer(allVariablesDF$day)


