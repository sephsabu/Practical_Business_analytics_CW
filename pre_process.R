#Functions
library(ggplot2)
library(lattice)
library(caret)
library(dummies)
library(dplyr)
library(Lahman)
library(ggcorrplot)
library(ROSE)
library(pROC)
library(ROCR)
library(keras)
library(rpart)

#Plotting density and boxplot graphs
outlier_plot<-function(num_df){
  
  for(x in 1:ncol(num_df)){
    
    print(ggplot(data=num_df,aes(x = num_df[,x])) + geom_density(fill = 'cyan', color = 'cyan') + xlab(colnames(num_df[x]))
          +theme(text = element_text(family = 'Gill Sans', color = "#444444"),panel.background = element_rect(fill = '#444B5A')
                 ,panel.grid.minor = element_line(color = '#4d5566'),panel.grid.major = element_line(color = '#586174')))
    
    print(ggplot(data = num_df, aes(x=num_df[,x]))+geom_boxplot(fill='slateblue')+xlab(colnames(num_df[x])))
  }
}

#Replacing null values with mean of the feature
handle_null_values<-function(df){
  for(i in 1:ncol(df)){
    print(c(colnames(df[i]),sum(is.na(df[i]))))
    if(sum(is.na(df[i]))>0){
      
      cat(colnames(df[i]),"column has",sum(is.na(df[i]))," values \n")
      cat("The standard deviation before handling null values is",sd(df[,i],na.rm=TRUE))
      df[is.na(df[,i]),i] <- mean(df[,i], na.rm = TRUE)
      cat("\n The standard deviation after handling null values is",sd(df[,i],na.rm=TRUE),"\n")
    }
  }

  return(df)  
}

outlier_analysis<-function(df){
   
  nums <- unlist(lapply(df, is.numeric)) #Capturing only numeric columns to check the data distribution and outlier presence
  num_df<-df[,nums]
  
  outlier_plot(num_df)
    
}


encoding_cat<-function(df){
  
  #One hot encoding
  one_hot_df <- dummy.data.frame(df, names=c("Married.Single","Car_Ownership","House_Ownership"), sep="_")

  #Mean encoding Profession
  temp_df<-select(one_hot_df,Profession,Risk_Flag)
  lookup = temp_df %>%
    group_by(Profession) %>%
    summarise(profession_enc = mean(Risk_Flag))
  temp_df = left_join(temp_df, lookup)
  #View(temp_df)

  #Mean encoding city
  temp_df1<-select(one_hot_df,CITY,Risk_Flag)
  lookup1 = temp_df1 %>%
    group_by(CITY) %>%
    summarise(city_enc = mean(Risk_Flag))
  temp_df1 = left_join(temp_df1, lookup1)
  #View(temp_df1)

  #Mean encoding state
  temp_df2<-select(one_hot_df,STATE,Risk_Flag)
  lookup2 = temp_df2 %>%
    group_by(STATE) %>%
    summarise(state_enc = mean(Risk_Flag))
  temp_df2 = left_join(temp_df2, lookup2)
  #View(temp_df2)
  
  one_hot_df <- cbind(one_hot_df, profession = temp_df$profession_enc, city = temp_df1$city_enc, state= temp_df2$state_enc)
  keeps <- c("Profession", "CITY",'STATE')
  one_hot_df<-one_hot_df[,!(names(one_hot_df) %in% keeps)]
  return (one_hot_df)
}

feature_selection<-function(encoded_data){
  #Dropping the Id column as it is unique and does not contribute to he variance of the target attribute
  dims<- c('Id')
  encoded_data<-encoded_data[,!(names(encoded_data)) %in% dims]
  correlation_matrix <- round(cor(encoded_data),4)
  print(ggcorrplot(correlation_matrix, method ="square",colors=c("#cf6b4a","#32a850","#423737"),lab=TRUE)) #Plotting heatmap correlation matrix
  return (encoded_data)
}

transforming_data<-function(featured_data){
  for (var in 1:16){
    featured_data[,var]<-normalize(featured_data[,var]) #Normalizing the dataset
  }
  return(featured_data)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Balancing the dataset
my.ovun.sample <- function(dataset) {
  my_data <- dataset
  curr_frame <<- sys.nframe()
  data_balanced_both<-ovun.sample(Risk_Flag ~ ., data = get("my_data", sys.frame(curr_frame)), method="both", N=252000, seed=1)$data
  print(table(data_balanced_both$Risk_Flag))
  return (data_balanced_both)
}

# Randomise and split the data into train and test
splitting_data<-function(transformed_data){
  
  transformed_data<-transformed_data[sample(nrow(transformed_data)),]
  # Split the dataset into train and test
  training_records<-round(nrow(transformed_data)*(70/100))
  training_data <- transformed_data[1:training_records,]
  testing_data = transformed_data[-(1:training_records),]
  
  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}


#Training using Decision Tree

decision_tree<-function(training,testing){
  
  test_y <- testing$Risk_Flag
  
  tree<- rpart(Risk_Flag~.,data=training)
  predtree <- predict(tree,testing)
  decision_predictions<-ifelse(as.numeric(predtree)<0.5,0,1)
  #print(decision_predictions)
  cftree <- confusionMatrix(table(test_y,decision_predictions))
  treeTP<-cftree$table[1,1]
  treeFN<-cftree$table[1,2]
  treeFP<-cftree$table[2,1]
  treeTN<-cftree$table[2,2]
  
  treeaccuracy<-100.0*((treeTP+treeTN)/(treeTP+treeFP+treeFN+treeTN))
  treeprecision<-treeTP/(treeTP+treeFP)
  treerecall<-treeTP/(treeTP+treeFN)
  treeaucscore<-auc(test_y,decision_predictions)
  
  print(cftree)
  print(paste("The accuracy of the decision tree model is ",treeaccuracy))
  print(paste("The precision of the decision tree model is ",treeprecision))
  print(paste("The recall of the decision tree model is ",treerecall))
  print(paste("The auc score of the decision tree model is ",treeaucscore))
  
  roc_score=roc(test_y, decision_predictions) #AUC score
  plot(roc_score ,main ="ROC curve -- Decision Tree")
}


#Training using XGBOOST algorithm
xgb_modelling <- function(training_data,testing_data){
  
  # Separate X and Y for train and test data 
  train_x <- training_data %>% select(-"Risk_Flag")
  train_y <- training_data$Risk_Flag
  
  test_x <- testing_data %>% select(-"Risk_Flag")
  test_y <- testing_data$Risk_Flag
  
  # Define the parameters for the xgb model
  parameters <- list(eta = 0.3,
                     max_depth = 6,
                     subsample = 1,
                     colsample_bytree = 1,
                     min_child_weight = 1,
                     gamma = 0,
                     set.seed = 1234,
                     eval_metric = "auc",
                     objective = "binary:logistic",
                     booster = "gbtree")
  
  # Train the xgb model
  xgbModel <- xgboost(data = as.matrix(train_x),
                      label = train_y,
                      set.seed(1234),
                      nrounds = 100,
                      params = parameters,
                      verbose = 1)
  
  # Predicting the probabilities of test data
  xgb_predictions <- predict(xgbModel, newdata = as.matrix(test_x))
  
  
  # Converting probs to determine class
  xgb_predictions<-ifelse(as.numeric(xgb_predictions)<0.5,0,1)
  expectedClass<-test_y
  
  # Evaluating the model using a confusion matrix 
  cf <- confusionMatrix(table(Expected = expectedClass, Predicted = xgb_predictions))
  TP<-cf$table[1,1]
  FN<-cf$table[1,2]
  FP<-cf$table[2,1]
  TN<-cf$table[2,2]
  
  accuracy1<-100.0*((TP+TN)/(TP+FP+FN+TN))
  precision1<-TP/(TP+FP)
  recall1<-TP/(TP+FN)
  aucscore<-auc(expectedClass,xgb_predictions)
  
  print(cf)
  print(paste("The accuracy of the xgboost model is",accuracy1))
  print(paste("The precision of the xgboost model is ",precision1))
  print(paste("The recall of the xgboost model is ",recall1))
  print(paste("The auc score of the xgboost model is ",aucscore))
  
  roc_score=roc(expectedClass, xgb_predictions) #AUC score
  plot(roc_score ,main ="ROC curve -- XGBoost")
  
  xgb.save(xgbModel,'./Model/xgb.model') #Saving the model
  
}

#Training using neural network model
neuralnetw1<-function(traindata,testdata){
    positionClassOutput<-which(names(traindata)=='Risk_Flag')
    train_x<-traindata[-positionClassOutput]
    train_y<-traindata[,positionClassOutput]
    
    test_x<-testdata[,-positionClassOutput]
    test_y<-testdata[,positionClassOutput]
    
    x<-as.matrix(train_x)
    y<-keras::to_categorical(train_y,num_classes = 2)

    
    model<-keras_model_sequential()
    
    model %>%
      layer_dense(units=8, activation='relu',input_shape = ncol(x)) %>%
      layer_dense(units=2,activation = 'softmax')
    
    #Compile
    model %>%
      compile(loss='binary_crossentropy',optimizer='adam',metrics='accuracy')
    
    #Fit model
    history<-model %>%
                fit(x,y,epoch=100,batch_size=32,validation_split=0.2)
    
    #Confusion Matrix
    predq<- predict(model,as.matrix(test_x))
    predq<-predq[,1]
    predq<-ifelse(predq>0.5,0,1)
    cfnn<-confusionMatrix(table(test_y,predq))
    
    nnTP<-cfnn$table[1,1]
    nnFN<-cfnn$table[1,2]
    nnFP<-cfnn$table[2,1]
    nnTN<-cfnn$table[2,2]
    
    nnaccuracy<-100.0*((nnTP+nnTN)/(nnTP+nnFP+nnFN+nnTN))
    nnprecision<-nnTP/(nnTP+nnFP)
    nnrecall<-nnTP/(nnTP+nnFN)
    nnaucscore<-auc(test_y,predq)
    
    print(paste("The precision of the neural network model is ",nnprecision))
    print(paste("The recall of the neural network model is ",nnrecall))
    print(paste("The auc score of the neural network model is ",nnaucscore))
    
    roc_score=roc(test_y, predq) #AUC score
    plot(roc_score ,main ="ROC curve -- Neural Network")
    
  }
