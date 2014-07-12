##################################
# R code to generate
#a submission to HHP challenge
#Namratha Sreekanta
##################################

#Function to load, train and generate 
# a submission - loadAgain = TRUE
#if data has to be loaded again


runExperiments <- function(loadAgain=TRUE, numOfTrees){
  
  
  #If loadAgain is true, load the data
  
  if(loadAgain){
    
    #load all the train data Y2, Y3
    print("Start Load data process...")
    datasets <- loadGeneratedDatasets()
    print("Finish Load data process...")
    
  }
  
  numOfTrees <- 10
  
  print("Start building trees...")  
  
  model <- trainRandomForestModel( datasets$train, numOfTrees)
  
  print("Built the model...")
  
  #Predict...
  print ("Start Predicting...")  
  logPredictions <- predict(object = model, newdata =  datasets$test)
  
  #Generate the score
  not.na <- !is.na(logPredictions)
  n <-length(logPredictions[not.na])
  score <- HPPScore_m(logPredictions[not.na] ,log(datasets$test$DaysInHospital[not.na]),n)
  print("Error of prediction with selected trees")
  print(numOfTrees)
  print(score)
  
  print("Verify and generate submission...")
  
  #Verify Predictions and generate output to submission files
  verifyPred(model,logPredictions,paste("submission-TIM209_NAMRATHA_SREEKANTA_",numOfTrees)) 
  
  
  return(score)
  
}

HPPScore_m <- function (p, a,n) {
  score <- (p - a + 1)
  score <- sum( score^2 )
  score <- sqrt( score /n )
  return (score)
}

verifyPred <- function(model,logPredictions,submissionTag="submission-TIM209_NAMRATHA_SREEKANTA"){
  # rescale DIH
  DaysInHospital = exp(logPredictions)-1
  
  # Predicting within scale of 0-15 days
  DaysInHospital=ifelse(DaysInHospital<0, 0, DaysInHospital)
  DaysInHospital=ifelse(DaysInHospital>15, 15, DaysInHospital)
  
  debug=FALSE # Setting the DEBUG
  # If debug is set, generate a summary
  if (debug) { 
    hist(logPredictions, breaks = 100);  grid()
    hist(DaysInHospital, breaks = 100); grid()
    summary(DaysInHospital)
  }
  par(mfrow=c(2,2))
  print("Generating Histogram for Days in Hospital and Member Id")
  hist(datasets$trainY3$DaysInHospital, breaks = 400, main = paste("Actual Days in Hospital in Y3"),xlab="DaysInHospital"); grid()
  hist(DaysInHospital, breaks = 400, main = paste("Predicted Days in Hospital in Y3")); grid()
  
  
  #generate the submission file columns
  print("Generating the submission file...")
  submissionDF = cbind(MemberID=datasets$trainY3$MemberID, ClaimsTruncated=datasets$trainY3$ClaimsTruncated, DaysInHospital)
  print("a CSV file")
  submissionFile = paste(dataDirOut, submissionTag, ".csv", sep="")
  write.csv(submissionDF, submissionFile, row.names=FALSE)
  cat(date(), "finished writing training file for ",submissionFile, " \n")
  #verify the submission file
  print("printing first few lines of submission file...")
  head(read.csv(submissionFile))  #verify submission
  print(head(read.csv(submissionFile)))
  print("printing last few lines of submission file...")
  print(tail(read.csv(submissionFile)))
  tail(read.csv(submissionFile))  #verify submission
  cat("submission row, column count is:",dim(read.csv(submissionFile)))  #verify submission	
  summary(read.csv(submissionFile))  #verify submission
  plot(model)
  
}

loadGeneratedDatasets = function( nrow = 10000 ) {
 
  datasets=list() 
  
  datasets$train = read.csv(paste(dataDirOut, "trainY2", ".csv", sep=""),nrows=nrow)
  print("done3");
  datasets$test = read.csv(paste(dataDirOut, "trainY3", ".csv", sep=""),nrows=nrow)
  
  # convert discrete variables
  sex.levels <- c("", "F", "M")
  return(datasets)
}


trainRandomForestModel = function(train, numOfTrees=10, droppedFeatures=c()) { 
  print("Start training...")
  droppedFeatures=20:109
  trainData=train
  targindex =   which(names(trainData)=="DaysInHospital")
  memberIndex =   which(names(trainData)=="MemberID")
  
  ntree <- numOfTrees
  trainData <- datasets$train[, -c(memberIndex, droppedFeatures)]  
  #build  the  model  
  #impute <- rfImpute( DaysInHospital ~ ., data = trainData, ntree = ntree)
  model <- randomForest( DaysInHospital ~ ., data = trainData, ntree = ntree, na.action = na.roughfix)
  print("after build....")  
  
  plot(model)
  cat(date(), "Finished training Random Forest model\n")
  return(model)
}


test91 <- function(){
  #Run on CV and select the best optimal tree 
  baseDir="C:\\UCSC courses\\Winter2014\\Data Analytics\\HHPDataSet\\"
  dataDir=baseDir
  #install.packages("randomForest") library(randomForest)
  
  #output dir
  dataDirOut=baseDir
 
  
  #score1 <- runExperiments(TRUE,1)
  score10 <- runExperiments(TRUE,10)
  #score50 <- runExperiments(FALSE,50)
 
  
  
  
  
  # score100 <- runExperiments(FALSE,100)
  #score300 <- runExperiments(FALSE,300)
  # score500 <- runExperiments(FALSE,500)
  #score1000 <- runExperiments(FALSE,1000)
  # cat("Score for 1 tree ", score1)
  cat("Score for 10 tree ", score10)
  # cat("Score for 50 tree ", score50)
  #cat("Score for 100 tree ", score100)
  #cat("Score for 300 tree ", score300)
  
  
  
} 