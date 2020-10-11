# The MNIST database of handwritten digits has a training set of 60,000 examples, 
# and a test set of 10,000 examples. It is a subset of a larger set available from MNIST.
# The 784 columns apart from the label consist of  28*28 matrix describing the scanned image 
# of the digits
# The digits have been size-normalized and centered in a fixed-size image


#################################### Installing libraries if not installed ##########################
rm(list=ls())
packages <- c("caret", "kernlab", "tidyverse", "reshape2", 
              "rstudioapi", "doParallel", "parallel", 
              "data.table", "gridExtra", "e1071")
install.lib <- packages[!packages %in% installed.packages()]

for(libs in install.lib) install.packages(libs)
sapply(packages, require, character=TRUE)

# physical cores and clusters
phy_cores <- detectCores(logical = FALSE)
phy_clust <- makeCluster(phy_cores)

doParallel::registerDoParallel(cl = phy_clust,cores = phy_cores)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################  Objective ###########################

# To succesfully classify handwritten digits (0-9) using pixel values 
# Support Vector Machines will be applied

############### Loading data #######################

train <- fread(input = "mnist_train.csv", stringsAsFactors = F, 
                     data.table = FALSE, header = FALSE)
test <- fread(input = "mnist_test.csv", stringsAsFactors = F, 
                     data.table = FALSE, header = FALSE)


############# Data cleaning, preparation & understanding #######################

#-------------Data cleaning-----------------

# Duplicated rows

sum(anyDuplicated(test)) # no duplicate rows
# sum(anyDuplicated(train)) # no duplicate rows

sum(sapply(train, function(x) sum(is.na(x)))) # There are no NA values
sum(sapply(test, function(x) sum(is.na(x)))) # There are no NA values

#--------------Data understanding---------------------


# Data has system generated column names
# View(train)
# View(test)

#Understanding Dimensions

dim(train) #60000   785

dim(test) #10000   785

#Structure of train and test dataset

str(train) # all dependant variables are integers, 60000 observations, 785 variables
str(test) # all dependant variables integers, 10000 observations, 785 variables

#printing first few rows of train and test dataset

head(train)
head(test)

#Exploring train and test dataset

summary(train[,2:100]) # but some only go up to ~100, data needs to be scaled
summary(test[,2:100]) # some columns seem to be containing only zeros, Pixel values go upto 255,


#---------------Data Preparation-------------------------------------------

# As we have a lot of columns(785). we will use PCA to reduce the columns
# The way to do it like, merge train and test dataset, apply PCA , post that again 
# split the merged sheet into train and test data set 
merged_dataset <- rbind(train,test) # merging train and test data 
dim(merged_dataset)  ##70000   785
# Removing target variable(digit)
digit_features <- merged_dataset[,-1]
# Checking for variance
near_zero_var <- nearZeroVar(digit_features, saveMetrics = T)
# Removing those columns having zeroVar value as False and creating a new dataframe
digit_features2 <- digit_features[,near_zero_var$zeroVar==F]
dim(digit_features2) ##70000   719

# Scaling the features
# Normalize: X = (X - min) / (max - min) => X = (X - 0) / (255 - 0) => X = X / 255.
max(digit_features2[ ,1:ncol(digit_features2)]) # max pixel value is 255, lets use this to scale data
digit_features2[ , 1:ncol(digit_features2)] <- digit_features2[ , 1:ncol(digit_features2)]/255

# Dimensionality reduction using PCA

# Taking a threshold of 70%, applying PCA to get relevant columns 
digit_features3 <- preProcess(digit_features2, method=c("pca"), thresh = 0.70) %>%
                      predict(digit_features2)

dim(digit_features3)   # 70000    99
digit_features3 <- cbind(merged_dataset$V1,digit_features3)
digit_features3 <- data.frame(digit_features3)

# Naming first column as digit and converting to factor
colnames(digit_features3)[1] <- "digit"
digit_features3$digit <- factor(digit_features3$digit)

# Separating train and test dataset
processed_train <- head(digit_features3,60000)
dim(processed_train) # 60000   100

processed_test <- tail(digit_features3,10000)
dim(processed_test) # 10000   100

#---------- Exploratory Data Analysis -----------------------#

colnames(train)[1] <- "digit"
colnames(test)[1] <- "digit"

# converting the factor to character as melt fucntions is based on factors
# Convert label variable into factor

train$digit <- factor(train$digit)
summary(train$digit)

show.digit <- function(digit, col=gray(12:1/12), ...) {
  opa <- par(mar = rep(0, 4))
  image(matrix(digit, nrow=28)[,28:1], col=col, axes = FALSE, ...)
  par(mar = opa)
}

show.digit(matrix(unlist(train[floor(runif(1, min=1,max = 60000)),-1])))

#---------------Model building and evaluation----------------------

# Sampling training dataset

dim(train) # computation time would be unnaceptable for such a large dataset

set.seed(100)
sample_indices <- sample(1: nrow(processed_train), 5000) 
# extracting subset of 5000 samples for modelling
sample_train <- processed_train[sample_indices, ]

# Linear kernel using default parameters
model1_linear <- ksvm(digit ~ ., data = sample_train, scaled = FALSE, kernel = "vanilladot", C = 1)
print(model1_linear) 

# Taking whole test data set for evaluation
eval1_linear <- predict(model1_linear, newdata = processed_test, type = "response")
confusionMatrix(eval1_linear, processed_test$digit) 

# Observations:
# Overall accuracy of 89.53%

## Linear kernel using stricter C

model2_linear <- ksvm(digit ~ ., data = sample_train, scaled = FALSE, kernel = "vanilladot", C = 10)
print(model2_linear) 

eval2_linear <- predict(model2_linear, newdata = processed_test, type = "response")
confusionMatrix(eval2_linear, processed_test$digit) 

# Observations:
# Overall accuracy of 89.36%
# Model performance has slightly decreased, model may be overfitting


## Using cross validation to optimise C

grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) # defining range of C

fit.linear <- train(digit ~ ., data = sample_train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 5))

# printing results of 5 cross validation
print(fit.linear) 
plot(fit.linear)

# Observations:
# Best accuracy of 92% at C = 0.1
# Higher values of C are overfitting and lower values are giving simple models

eval_cv_linear <- predict(fit.linear, newdata = processed_test)
confusionMatrix(eval_cv_linear, processed_test$digit)

# Observations:
# Overall accuracy of 91.85%, slightly imporved
# improved from model1 by making model more generic i.e. lower C 


#--------------------------------------------- Radial Kernel ----------------------------------------------#

# Radial kernel using default parameters

model1_rbf <- ksvm(digit ~ ., data = sample_train, scaled = FALSE, 
                   kernel = "rbfdot", C = 1, kpar = "automatic")
print(model1_rbf) 

eval1_rbf <- predict(model1_rbf, newdata = processed_test, type = "response")
confusionMatrix(eval1_rbf, processed_test$digit) 

# Observations:
# Overall accuracy of 93.67%
# Increase in overall accuracy and sensitivty from linear kernel using C = 1, sigma = 0.0107
# data seems to have non linearity to it


## Radial kernel with higher sigma

model2_rbf <- ksvm(digit ~ ., data = sample_train, scaled = FALSE, kernel = "rbfdot",
                   C = 1, kpar = list(sigma = 1))
print(model2_rbf) 

eval2_rbf <- predict(model2_rbf, newdata = processed_test, type = "response")
confusionMatrix(eval2_rbf, processed_test$digit) 

# Observations:
# Accuracy drops to 11.35% and class wise results are very poor
# sigma = 1 is too much non linearity and the model is overfitting


# Using cross validation to optimise C and sigma

# defining ranges of C and sigma
grid_rbf <- expand.grid(C= c(0.01, 0.1, 1, 5, 10), sigma = c(0.001, 0.01, 0.1, 1, 5)) 

# Using only 2 folds to optimise run time
fit.rbf <- train(digit ~ ., data = sample_train, metric = "Accuracy", 
                 method = "svmRadial",tuneGrid = grid_rbf,
                 trControl = trainControl(method = "cv", number = 2), preProcess = NULL)

# printing results of 2 cross validation
print(fit.rbf) 
plot(fit.rbf)

# Observations:
# Best sigma value is ~ 0.01
# Higher sigma values are overfitting and lower sigma values are not capturing 
# non linearity adequately
# Accuracy increases with C until 5 and then decreases again, can be further optimised

# Optimising C further
grid_rbf2 = expand.grid(C= c(1,2, 3, 4, 5, 6 ,7, 8, 9, 10), sigma = 0.01)

fit.rbf2 <- train(digit ~ ., data = sample_train, metric = "Accuracy", 
                  method = "svmRadial",tuneGrid = grid_rbf2,
                  trControl = trainControl(method = "cv", number = 5), preProcess = NULL)

# printing results of cross validation
print(fit.rbf2) 
plot(fit.rbf2)

eval_cv_rbf <- predict(fit.rbf2, newdata = processed_test)
confusionMatrix(eval_cv_rbf, processed_test$digit)

# Observations:
# Accuracy is highest at C = 3 and sigma = 0.01ss
# Higher C values are overfitting and lower C values have too much bias
# Accuracy of 93.8%




