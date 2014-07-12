##################################
# R code to generate
#a submission All State insurance
#kaggle competition
#Namratha Sreekanta
##################################

read_training_data <- function() {
  cat("\n\n* Reading File ... ")
  allState <- read.table( file = "C:\\UCSC courses\\Winter2014\\Data Analytics\\train.csv",
                          header = TRUE,
                          sep=",",
                          colClasses = c( "character", # customer_ID
                                          "integer", # shopping_pt
                                          "factor", # record_type
                                          "NULL", # day
                                          "NULL", # time
                                          "NULL", # state
                                          "NULL", # location
                                          "factor", # group size
                                          "factor", # homeowner
                                          "integer", # car age
                                          "factor", # car value
                                          "factor", # risk factor
                                          "integer", # age oldest
                                          "integer", # age youngest
                                          "factor", # married couple
                                          "factor", # previous C coverage option
                                          "integer", # duration of previous coverage
                                          "integer", # option A
                                          "integer", # option B
                                          "integer", # option C
                                          "integer", # option D
                                          "integer", # option E
                                          "integer", # option F
                                          "integer", # option G
                                          "NULL" # cost
                          ))
  allState[,2:20] <- na.roughfix(allState[,2:20])
  
  # Set the test variable
  names(allState)[14:20] <- c("yA", "yB", "yC", "yD", "yE", "yF", "yG")
  # Set the previous entries -- derived feature
  nam <- c("A", "B", "C", "D", "E", "F", "G")
  for( i in 1:length(nam)) {
    allState[, nam[i]] <- c(NA, (allState[, 13+i])[1:(nrow(allState)-1)])
  }
  
  # Take only the purchased ones
  allState <- subset(allState, record_type == "1")
  
  # Set things as factors
  for( i in 14:27) {
    allState[, i] <- as.factor( allState[, i])
  }
    
  cat(" [done]\n")
  return(allState)
}

split_train_test <- function(tryState) {
  cat("* Splitting given training set into training and testing ... ")
  # Prepare the training and testing data
  customers <- unique(tryState$customer_ID)
  x <- length(customers)
  
  # Let's take 70% as training data
  train <- sample(x, round(0.70 * x))
  
  # Split into testing and training customer IDs
  train <- customers[train]
  test <- setdiff(customers, train)
  
  cat(" [done]\n")
  return(list(train = train, test = test))
}

features <- function(tryState, customers = unique(tryState$customer_ID)) {
  tryState <- subset(tryState,
                     customer_ID %in% customers)
  x <- data.frame( A = as.factor(tryState$A))
  x$B = tryState$B
  x$C = tryState$C
  x$D = tryState$D
  x$E = tryState$E
  x$F = tryState$F
  x$G = tryState$G
  x$group_size = tryState$group_size
  x$homeowner = tryState$homeowner
  x$car_age = tryState$car_age
  x$car_value = tryState$car_value
  x$risk_factor = tryState$risk_factor
  x$age_oldest = tryState$age_oldest
  x$age_youngest = tryState$age_youngest
  x$married_couple = tryState$married_couple
  x$C_previous = tryState$C_previous
  x$duration_previous = tryState$duration_previous
  return(x)
}

y_data <- function(tryState, customers = unique(tryState$customer_ID)) {
  tryState <- subset(tryState,
                     customer_ID %in% customers)
  y <- data.frame( yA = as.factor(tryState$yA))
  y$yB <- tryState$yB
  y$yC <- tryState$yC
  y$yD <- tryState$yD
  y$yE <- tryState$yE
  y$yF <- tryState$yF
  y$yG <- tryState$yG
  return(y)
}

train_model <- function(train.x, train.y) {
  model <- list()
  for( i in 1:length(train.y)) {
    n <- colnames(train.y)[i]
    train.data$y <- train.y[, n]    
    model[[i]] <- randomForest( y ~ ., data = train.data, ntree = 100)
    #model[[i]] <- rpart(y ~ ., data = train.data)
  }
  return(model)
}

predict_tryState <- function(data, models) {
  pred.y <- list()
  for (i in 1:length(models)) {
    pred.y[[i]] <- predict( models[[i]], data)
  }
  return(pred.y)
}

accuracy <- function(preds, tests) {
  acc <- vector()
  for (i in 1:length(preds)) {
    x <- pred.y[[i]] == test.y[, i]
    x <- x[ x == "TRUE"]
    acc[i] <- length(x) / length(pred.y[[i]])
  }
  return(acc)
}

read_submission_data <- function() {
  cat("\n\n* Reading File ... ")
  subm <- read.table( file = "~/learning/2013 - 2014 UC Santa Cruz/q2/tim209 jimi/final/test_v2.csv",
                      header = TRUE,
                      sep=",",
                      colClasses = c( "character", # customer_ID
                                      "integer", # shopping_pt
                                      "NULL", # record_type
                                      "NULL", # day
                                      "NULL", # time
                                      "NULL", # state
                                      "NULL", # location
                                      "factor", # group size
                                      "factor", # homeowner
                                      "integer", # car age
                                      "factor", # car value
                                      "factor", # risk factor
                                      "integer", # age oldest
                                      "integer", # age youngest
                                      "factor", # married couple
                                      "factor", # previous C coverage option
                                      "integer", # duration of previous coverage
                                      "factor", # option A
                                      "factor", # option B
                                      "factor", # option C
                                      "factor", # option D
                                      "factor", # option E
                                      "factor", # option F
                                      "factor", # option G
                                      "NULL" # cost
                      ))
  subm[, 2:19] <- na.roughfix(subm[, 2:19])
  i <- which(subm$shopping_pt == 1) - 1
  i <- c(i[2:length(i)], length(subm$customer_ID))
  subm <- subm[i, ]
  return(subm)
}

tryState <- read_training_data()
tryID <- split_train_test(tryState)

train.data <- features(tryState, tryID$train)
train.y <- y_data(tryState, tryID$train)

library("randomForest")
library("rpart")
models <- train_model(train.x, train.y)

test.data <- features(tryState, tryID$test)
pred.y <- predict_tryState(test.data, models)
test.y <- y_data(tryState, tryID$test)
print( accuracy <- accuracy(pred.y, test.y) )

subm.data <- read_submission_data()
subm.x <- features(subm.data)
subm.pred.y <- predict_tryState(subm.x, models)
subm.df <- cbind( customer_ID = subm.data$customer_ID,
                  plan = paste( subm.pred.y[[1]],
                       subm.pred.y[[2]],
                       subm.pred.y[[3]],
                       subm.pred.y[[4]],
                       subm.pred.y[[5]],
                       subm.pred.y[[6]],
                       subm.pred.y[[7]],
                       sep=""))