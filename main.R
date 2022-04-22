# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS - COURSEWORK
#
# ************************************************

rm(list=ls()) #Clearing objects


training_data<-'Data/Training Data.csv'

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "xgboost",
               "PerformanceAnalytics")
library(pacman)
library(ggplot2)

pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


source("pre_process.R") #Loading the functions defined in other R scripts

main<-function(){
  
  df<-read.csv(training_data) #Reading the dataset into the dataframe
  
  df<-handle_null_values(df) #Handling null values
  
  outlier_analysis(df) #Analysing the presence of outliers
  
  #Encoding
  encoded_data<-encoding_cat(df)
  
  featured_data<-feature_selection(encoded_data) #Checking for correlation of the dataset
  
  transformed_data<-transforming_data(featured_data) #Normalizing the dataset using min-max formula
  
  #balancing the imbalance in the data
  balanced_data<-my.ovun.sample(transformed_data)
  
  # Splitting the transformed data into train and test set
  split_data <- splitting_data(balanced_data)
  
  #Decision tree
  decision_tree(split_data$train, split_data$test)
  
  #XGBOOST
  xgb_modelling(training_data = split_data$train, testing_data = split_data$test)
  
  #Neural network using Keras
  neuralnetw1(split_data$train,split_data$test)
}
main()