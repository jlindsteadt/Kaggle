#### DATA INPUT
library('dplyr') # data manipulation
library(kernlab)
library(ggplot2)
library('ggthemes') # visualization
library('scales') # visualization


#Load Test and Train data sets & combine to create 'full' data set
test_data <- read.csv("titanic_test.csv", stringsAsFactors = TRUE, header = TRUE)
train_data <- read.csv("titanic_train.csv", stringsAsFactors = TRUE, header = TRUE)
full <- bind_rows(test_data, train_data)
set.seed(123)

str(full)


############# Data Engineering   ################
#Find NA values for each variable
na_matrix <- as.matrix(rep(0, ncol(full)))
rownames(na_matrix) <- colnames(full)
for(i in 1:ncol(full)){
  na_matrix[i] <- length(full[,i][which(is.na(full[,i])==TRUE)])
}
na_matrix


#Expolore Age data
# create a new data set age
age <- full$Age
n = length(age)
# replace missing value with a random sample from raw data

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

#Insert sample data into Age data
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

full$n_cabin <- as.numeric(cabin)

ggplot(data=full, aes(y = n_cabin, x=Age, colour=as.factor(Survived))) +
  geom_point()


### Explore Fare data
full[which(is.na(full$Fare)==TRUE),]

#Plot densitty for fares from passensgers in Class 3 that embarked from S
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)


#Set missing fare for Passenger 1044 to $8.05
full$Fare[which(full$PassengerId == 1044)] = median(full[full$Pclass == '3' & full$Embarked == 'S', 'Fare'], na.rm=TRUE)


# process embarked column
embarked <- full$Embarked
n = length(embarked)
for(i in 1:n){
  if(embarked[i] != "S" && embarked[i] != "C" && embarked[i] != "Q"){
    embarked[i] = "S"
  }
}
table(embarked)

full$Embarked <- embarked

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1




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


## Data Analysis
#visualize the relationship between Sex & Age & survival
ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat='count') +
  theme_few()

ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(x = Sex, fill = factor(Survived))) +
  geom_histogram(stat='count') +
  theme_few()

ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(x = Sex, y=Age, colour = factor(Survived))) +
  geom_point() +
   theme_few()

ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(Age,fill = factor(Survived))) +
  geom_histogram()


#visualize the relationship between family size & survival
ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()



# calculate survival rate by Sex
tapply(train_data$Survived,train_data$Sex,mean)


# make a histogram of title v.s survival
ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(Title,fill = factor(Survived))) +
  geom_histogram(stat = "count")


# survival rate by Title
train <- full[which(is.na(full$Survived) ==FALSE),]
tapply(train$Survived,train$Title,mean)

# make a histogram
ggplot(train, aes(Pclass,fill = factor(Survived))) +
  geom_histogram(stat = "count")


# calculate survival rate
tapply(train$Survived,train$Pclass,mean)


# histogram of Fsize
ggplot(train, aes(Fsize,fill = factor(Survived))) +
  geom_histogram(stat = "count")

tapply(train$Survived,train$Fsize,mean)


# create histogram
ggplot(full[which(is.na(full$Survived) ==FALSE),], aes(n_cabin,fill = factor(Survived))) +
  geom_histogram(stat = "count")


# calculate survival rate
tapply(train$Survived,train$n_cabin,mean)

# make a histogram
ggplot(train, aes(Fare,fill = factor(Survived))) +
  geom_histogram()


####   MODEL BUILDING ######
new_train <- full[which(is.na(full$Survived) ==FALSE),]
fit_svm <- ksvm(Survived~ Age + Fare + Sex + Embarked + Fsize + Title
                 + Pclass, data=new_train,
                type = "C-svc", # Use C-classification method
                kernel = "vanilladot", # Use simple linear kernel
                C = 100,
                scaled=TRUE) # have ksvm scale the data for you


# predicted result of regression
svm.fitted = predict(fit_svm, new_train[,-1])

ans_svm = rep(NA, length(svm.fitted))
for(i in 1:length(ans_svm)){
  ans_svm[i] = as.integer(svm.fitted[[i]]) 
}
# check result
mean(ans_svm == train$Survived)
table(ans_svm)


# SVM
a = sum(ans_svm ==1 & train$Survived == 1)
b = sum(ans_svm ==1 & train$Survived == 0)
c = sum(ans_svm ==0 & train$Survived == 1)
d = sum(ans_svm ==0 & train$Survived == 0)
data.frame(a,b,c,d)



#####  CONSTRUCT TESTING DATA FRAME
new_test <- full[which(is.na(full$Survived) ==TRUE),] %>%
  select(-c(Name, SibSp, Parch, Ticket, Cabin, ,n_cabinSurname, Survived))
  #select(-c(Name, SibSp, Parch, Ticket, Survived))

# make prediction
svm_predict = predict(fit_svm,newdata = new_test[,-1])
ans_svm_predict = rep(NA,nrow(new_test))
for(i in 1:nrow(new_test)){
  #ans_svm_predict[i] = as.integer(svm_predict[[i]]) - 1
  ans_svm_predict[i] = as.integer(svm_predict[[i]])
}
table(ans_svm_predict)


d<-data.frame(PassengerId = new_test$PassengerId, Survived = ans_svm_predict) 
write.csv(d,file = "TitanicResult.csv",row.names = F)


