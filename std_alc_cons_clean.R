
#
library(readxl)
library(tidyverse)

#import the data
data1 <- read.csv("student-mat.csv")
data2 <- read.csv("student-por.csv")

#new variables to specify the class before I join the datasets
data1$class = "Math"
data2$class = "Portuguese"

merged <- data1 %>%
  full_join(data2)

str(merged)

#a look at students in both classes (by attributes specified in data description)
data3 <- merge(data1, data2, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(data3)) #382 students in both Math and Portuguese class

#lets look at some numeric variables of interest
#starting with family relationship (by sex)

by(merged$famrel, merged$sex, summary)

#creating a numeric variable only dataset

numeric_df <- select_if(merged, is.numeric)

#explorations of numeric variables
library(pastecs)

numeric_df %>%  stat.desc(norm = TRUE) %>%
  round(2)

#correlation coefficients and tests

  library(correlation)

  correlation::correlation(numeric_df,
                           include_factors = TRUE, method = "auto")


  corrplot2( #function created in another script...figure out the best way to save this function for future use
    data = numeric_df,
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
  )


