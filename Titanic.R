#### DATA INPUT
library('dplyr') # data manipulation
library(kernlab)
library(ggplot2)
library('ggthemes') # visualization
library('scales') # visualization
library('mice') # imputation
library('randomForest') # classification algorithm
library(kknn)


          test_data <- read.csv("titanic_test.csv", stringsAsFactors = TRUE, header = TRUE)
          train_data <- read.csv("titanic_train.csv", stringsAsFactors = TRUE, header = TRUE)
          full <- bind_rows(test_data, train_data)
          full$Embarked <- as.factor(full$Embarked)

          str(full)
          
          
####   REVIEW DATA
          # Grab title from passenger names
          full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
          
          # Show title counts by sex
          table(full$Sex, full$Title)
          
          # Titles with very low cell counts to be combined to "rare" level
          rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                          'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
          
          # Also reassign mlle, ms, and mme accordingly
          full$Title[full$Title == 'Mlle']        <- 'Miss' 
          full$Title[full$Title == 'Ms']          <- 'Miss'
          full$Title[full$Title == 'Mme']         <- 'Mrs' 
          full$Title[full$Title %in% rare_title]  <- 'Rare Title'
          
          # Show title counts by sex again
          table(full$Sex, full$Title)
          
          # Finally, grab surname from passenger name
          full$Surname <- sapply(full$Name,  
                                 function(x) strsplit(x, split = '[,.]')[[1]][1])
          
          # Create a family size variable including the passenger themselves
          full$Fsize <- full$SibSp + full$Parch + 1
          
          # Create a family variable 
          full$Family <- paste(full$Surname, full$Fsize, sep='_')
          
          # Use ggplot2 to visualize the relationship between family size & survival
          ggplot(full[1:nrow(full),], aes(x = Fsize, fill = factor(Survived))) +
            geom_bar(stat='count', position='dodge') +
            scale_x_continuous(breaks=c(1:11)) +
            labs(x = 'Family Size') +
            theme_few()
          
          ggplot(full, aes(x = Age, y=Title, colour = factor(Survived))) +
            geom_point() +
            #scale_x_continuous(breaks=c(1:11)) +
            labs(x = 'Age') +
            theme_few()
          
          ggplot(full, aes(x = Age, y=Embarked, colour = factor(Survived))) +
            geom_point() +
            #scale_x_continuous(breaks=c(1:11)) +
            labs(x = 'Age') +
            theme_few()
          
          ggplot(full, aes(x = Age, y=Pclass, colour = factor(Survived))) +
            geom_point() +
            #scale_x_continuous(breaks=c(1:11)) +
            labs(x = 'Age') +
            theme_few()
          
          ggplot(data, aes(x=Age, y=Pclass, color=Survived)) + 
            geom_jitter(position = position_jitter(height = .1)) +
            scale_color_manual(values=c("red", "blue")) + facet_grid(Sex ~ .) +
            ggtitle("Age, Sex, and Class as Survival Factors") + ylab("Pclass")
          
          # Process Age Column
          
          # create a new data set age
          set.seed(123)
          age <- full$Age
          n = length(age)
          # replace missing value with a random sample from raw data
          set.seed(123)
          for(i in 1:n){
            if(is.na(age[i])){
              age[i] = sample(na.omit(full$Age),1)
            }
          }
          # check effect
          par(mfrow=c(1,2))
          hist(full$Age, freq=F, main='Before Replacement', 
               col='lightblue', ylim=c(0,0.04),xlab = "age")
          hist(age, freq=F, main='After Replacement', 
               col='darkblue', ylim=c(0,0.04))
          
          full$Age <- age
          
          # Process Cabin Column to show number of cabins passenger has
          cabin <- full$Cabin
          n = length(cabin)
          for(i in 1:n){
            if(nchar(cabin[i]) == 0){
              cabin[i] = 0
            } else{
              s = strsplit(cabin[i]," ")
              cabin[i] = length(s[[1]])
            }
          } 
          table(cabin)
          
          full$PassengerId[is.na(full$Fare)]
          full[1044,]
          
          ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
                 aes(x = Fare)) +
            geom_density(fill = '#99d6ff', alpha=0.4) + 
            geom_vline(aes(xintercept=median(Fare, na.rm=T)),
                       colour='red', linetype='dashed', lwd=1)
          
          full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
          
          # process embarked column
          embarked <- full$Embarked
          n = length(embarked)
          for(i in 1:n){
            if(embarked[i] != "S" && embarked[i] != "C" && embarked[i] != "Q"){
              embarked[i] = "S"
            }
          }
          table(embarked)
          
      full[which(full$Embarked == ""),]
      
      
##  Data Exploration
      # number of survivals and nonsurvivals across different age
      d <- data.frame(Age = age[1:891], Survived = train_data$Survived)
      ggplot(d, aes(Age,fill = factor(Survived))) +
        geom_histogram()
      
      # create bar chart to show relationship between survival rate and age intervals
      cuts <- cut(d$Age,hist(d$Age,10,plot = F)$breaks)
      rate <- tapply(d$Survived,cuts,mean)
      d2 <- data.frame(age = names(rate),rate)
      barplot(d2$rate, xlab = "age",ylab = "survival rate")
      
      # create histgram to show effect of Sex on survival
      ggplot(train_data, aes(Sex,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      tapply(train_data$Survived,train_data$Sex,mean)
      
      # make a histogram
      ggplot(train_data, aes(Pclass,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      # histogram of Parch
      ggplot(train_data, aes(Parch,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      # combine SibSp and Parch 
      family <- full$SibSp + full$Parch
      d <- data.frame(family = family[1:891],Survived = train_data$Survived)
      ggplot(d, aes(family,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      # create histogram
      d <- data.frame(Cabin = cabin[1:891],Survived = train_data$Survived)
      ggplot(d, aes(Cabin,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      # make a histogram
      ggplot(train_data, aes(Fare,fill = factor(Survived))) +
        geom_histogram()
      
      cuts <- cut(train_data$Fare,hist(train_data$Fare,10,plot = F)$breaks)
      rate <- tapply(train_data$Survived,cuts,mean)
      d <- data.frame(fare = names(rate),rate)
      barplot(d$rate, xlab = "fare",ylab = "survival rate")
      
      # make histogram
      d <- data.frame(Embarked = embarked[1:891], Survived = train_data$Survived)
      ggplot(d, aes(Embarked,fill = factor(Survived))) +
        geom_histogram(stat = "count")
      
      tapply(train_data$Survived,train_data$Embarked,mean)
          
###  Support Vector Machines (SVM)

# ---------------------------- Data manipulation -------------------------------------
factor_vars <- c('Pclass','Sex','Embarked', 'Survived',
                 'Title','Surname','Family')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
          
          
svm_data_train <- full[which(is.na(full$Survived) == FALSE),]
# svm_data_train <- svm_data_train %>% 
#                       select(-c(Name, Ticket, Cabin, Family, Surname))
#svm_data_train <- svm_data_train[complete.cases(svm_data_train), ]
#svm_data_train <- as.matrix(svm_data_train)
str(svm_data_train)


#svm_data_test <- test_data
 svm_data_test <- full[which(is.na(full$Survived) == TRUE),]
# svm_data_test <- svm_data_test %>% 
#   select(-c(Name, Ticket, Cabin, Family, Surname))


# Setting the random number generator seed so that our results are reproducible
 set.seed(123)

# -------------------------- Creating the models ------------------------------------

# -------------------------- Scaled=TRUE model ------------------------------------

svm_model_scaled <- ksvm(Survived~Pclass + Sex + Age + Title + Fsize + Embarked,data=svm_data_train,
                     type = "C-svc", # Use C-classification method
                     kernel = "vanilladot", # Use simple linear kernel
                     C = 100,
                     scaled=TRUE) # have ksvm scale the data for you

 # svm
 library(e1071)
 
 fit_svm <- svm(factor(Survived) ~  Age + Fare + Sex + Embarked + Family 
                + Title + Cabin + Pclass, data = svm_data_train)
 
summary(fit_svm)
 # predicted result of regression
 svm.fitted = predict(fit_svm)
 ans_svm = rep(NA,891)
 svm.fitted
 for(i in 1:891){
   ans_svm[i] = as.integer(svm.fitted[[i]]) - 1
 }
 # check result
 mean(ans_svm == svm_data_train$Survived)
 table(ans_svm)
 
# fit_svm <- svm(factor(survived) ~ age + fare + sex + embarked + family 
#                + title + cabin + pclass,data = new_train)

#Attributes model show what the data structure model has to reference
#For example, we use model@b to get the intercept and model@coef to get the coefficients
#Those references (b and coef) can be found listed in the console by using attributes(model)

attributes(svm_model_scaled)

# Console output for attributes(model_scaled) is left out since it is a long output

#model lists some high level information about the model data structure

svm_model_scaled


# -------------------------- Calculating the a coefficients ------------------------------------
#
#Classification is done using linear kernel, a*scaled(x) + a0. 
# Unfortunately, the model does not output a directly, but we can use the model output to find a.
# calculate a1 to am using the stored data point values in the model data structure and corresponding coefficients
# multiplying the xmatrix by the coef gives the linear combination of data points that define a1,...,am
# we use the xmatrix attribute since the model stores these data points as scaled

a_scaled <- colSums(svm_model_scaled@xmatrix[[1]] * svm_model_scaled@coef[[1]])

#
# a0 is just -model_scaled@b

a0_scaled<- -svm_model_scaled@b

#

a_scaled
a0_scaled

# -------------------------- Calculating the predicted values ------------------------------------
#
#The ksvm package provides a predict() function that implements this for us, but we also
#show how to get the predicted values using the a coefficients

# Calculate the predicted values using the a's we got above and our data set.
# The coefficients for this model are based on the SCALED data points, so we need to 
# scale our data points to get the correct predictions. We do this by using the scaled
# mean and standard deviation values for V1 to V10 stored in the model data structure as:
# model@scaling$x.scale$`scaled:center` (means for V1 to V10)
# model@scaling$x.scale$`scaled:scale` (standard deviation for V1 to V10)
# Then we transform the data points into their scaled equivalent by using the function:
# scaled data point[i,1:10] = (data point[i,1:10] - model@scaling$x.scale$`scaled:center`)/model@scaling$x.scale$`scaled:scale`
#
#Create predicted vector (to hold our calculated predicted values)

predicted_scaled<-rep(0,nrow(svm_data_train))



# Get prediction from ksvm model we created, model_scaled
# Note that we could also get the predicted values of the model using model_scaled@fitted
#

pred_scaled <- predict(svm_model_scaled,svm_data_train[,-9])
pred_scaled


# typing "pred_scaled" will give the sequence of 1s and 0s showing the model's classification
# As you can see in the outputs, pred and predicted have the same predicted values
# so we know that our a coefficients are correct for the SCALED data version of the model

# -------------------------- Calculating the model's accuracy ------------------------------------
#
# I will use a simple accuracy measure that outputs the
# percent of testing observations that are correctly classified.

sum(pred_scaled == svm_data_train$Survived) / nrow(svm_data_train)


# Test prediction on Test data

pred_scaled2 <- predict(svm_model_scaled, svm_data_test[,-9])
pred_scaled2

write.csv(cbind(svm_data_test$PassengerId, pred_scaled2), "titanic_svm_submission.csv")


###  K-means Clustering



knn_data_test <- svm_test_data
#knn_data_test <- knn_data_test[complete.cases(knn_data_test),]
knn_data_train <- svm_train_data
knn_data_train <- knn_data_train[complete.cases(knn_data_train),]
nrow(knn_data_train)
# optional check to make sure the data is read correctly
#
str(knn_data_train)

check_accuracy = function(X){
  predicted <- rep(0,(nrow(knn_data_train))) # predictions: start with a vector of all zeros
  
  # for each row, estimate its response based on the other rows

  for (i in 1:nrow(knn_data_train)){
    # knn_data_test[-i] means we remove row i of the knn_data_test when finding nearest neighbors...
    #...otherwise, it'll be its own nearest neighbor!
    model=kknn(Survived~.,knn_data_train[-i,],knn_data_train[i,],k=12, scale = TRUE) # use scaled knn_data_test
    
    # record whether the prediction is at least 0.5 (round to one) or less than 0.5 (round to zero)
    
    predicted[i] <- as.integer(fitted(model)+0.5) # round off to 0 or 1
  }
  
  # calculate fraction of correct predictions
  predicted
  accuracy = sum(predicted == knn_data_train[,'Survived']) / nrow(knn_data_train)
  return(accuracy)
}
accuracy
#
# Now call the function for values of k from 1 to 20 (you could try higher values of k too)
#

acc <- rep(0,20) # set up a vector of 20 zeros to start
for (X in 1:20){
  acc[X] = check_accuracy(X) # test knn with X neighbors
}

#
# report accuracies
#

acc
str(knn_data_test)
knn_model_test <-  kknn(Survived~.,knn_data_train[-i,],knn_data_train[i,],k=12, scale = TRUE) # use scaled knn_data_test
knn_model_test
knn_data_test$Survived <- NA
#knn_pred_scaled2 <- predict(knn_model_test, knn_data_test[,-12])
#knn_pred_scaled2

#write.csv(cbind(svm_data_test$PassengerId, pred_scaled2), "titanic_svm_submission.csv")


###  Random Forest Trees

###  Linear Regression