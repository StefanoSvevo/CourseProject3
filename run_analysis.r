##
##
## run_analysis.R
##
##

## The working directory must contain this file (run_analysis.R) and the zipped data file (getdata_projectfiles_UCI HAR Dataset.zip)
  
  library(stringr)
  
  ### Unzip the data and go to the data folder
                                                                    cat("Unzipping and creating data directory...")
  unzip("getdata_projectfiles_UCI HAR Dataset.zip")
  setwd("./UCI HAR Dataset")
                                                                    cat("\nDone.\n")
  
  
  ### For the test data, get three dataframes:
                                                                    cat("\nConsolidating the test data...")
  # 1) a dataframe for the values (561 columns)
  testX_df <- read.table(file="./test/X_test.txt", sep="",header = FALSE)
  # 2) a dtaframe for the subjects (1 column)
  testSubject_df <- read.table(file="./test/subject_test.txt", sep="",header = FALSE)
  # 3) a dataframe for the activity (1 column)
  testY_df <- read.table(file="./test/y_test.txt", sep="",header = FALSE)
  
  ### Get a single test dataframe from the juxtaposition of the three dfs
  test_df <- cbind(testX_df,testSubject_df,testY_df) 
                                                                    cat("\nDone.\n")
  
  
  ### For the training data, get three dataframes:
                                                                    cat("\nConsolidating the training data...")
  # 1) a dataframe for the values (561 columns)
  trainX_df <- read.table(file="./train/X_train.txt", sep="",header = FALSE,)
  # 2) a dtaframe for the subjects (1 column)
  trainSubject_df <- read.table(file="./train/subject_train.txt", sep="",header = FALSE)
  # 3) a dataframe for the activity (1 column)
  trainY_df <- read.table(file="./train/y_train.txt", sep="",header = FALSE)
  
  ### Get a single train dataframe from the juxtaposition of the three dfs
  train_df <- cbind(trainX_df,trainSubject_df,trainY_df) 
                                                                    cat("\nDone.\n")
  
  
  ### Get a single dataframe from the training and test dataset
                                                                    cat("\nMerging the two datasets...")
  df <- rbind(test_df,train_df)
                                                                    cat("\nDone.\n")
  
  
  
  ### From the features.txt file extract only those features that have "std(" or "mean(" in their name
                                                                    cat("\nSelecting features of interest...")
  # First of all read the features' names in a dataframe
  features_df <- read.table("./features.txt",header = FALSE, col.names = c("id","measurement"))
  # Then select only those features that have either std( or mean( in their name
  condition <- grep("mean[(]|std[(]", features_df$measurement)
  selectedFeatures_df <- features_df[condition,]

 
  ### From the total dataframe select the columns defined in selectedFeatures_df plus the two last columns
  selected_df <- df[,c(selectedFeatures_df$id, dim(df)[2]-1, dim(df)[2])]
 
  ### Appropriately name the columns and the activities
  names(selected_df)<- c(as.character(selectedFeatures_df$measurement),"Subject","Activity")
  activity <- read.table("./activity_labels.txt", col.names = c("id","activityName"))
  selected_df$Activity <- factor(selected_df$Activity, labels=activity$activityName)
                                                                    cat("\nDone\n")
  ### Split the datafame by subject and activity
  split_li <- split (selected_df, list(selected_df$Subject, selected_df$Activity))
  
  ### Compute the column means
  numericMeans <- lapply(split_li , function(ARG) { colMeans(ARG[,1:66])  })
  
  ### Create a vector with all the means
  vector <- sapply(numericMeans,c)
  
  ### Create a matrix with the vector
  m <- matrix(vector,byrow = TRUE, nrow = 180)
  
  ### Turn the matrix into a dataframe
  tidy_df <- data.frame((m))
 
 
  ### Recover subject number
  subject <- lapply(names(split_li), FUN =str_extract_all,pattern="[[:digit:]]")
  sub <- sapply (subject, as.vector)
  sub <- sapply (sub, as.vector)
  sub <- sapply (sub, paste, collapse="")
  
  ### Recover activity name
  name <- lapply(names(split_li), FUN =str_extract_all,pattern="[[:alpha:]]")
  na <- sapply (name, as.vector)
  na <- sapply (na, as.vector)
  na <- sapply (na, paste, collapse="")
  
  ### Add the subject and activity columns
  tidy_df <- cbind(tidy_df,sub,na)
  
  ### Add the column names
  names(tidy_df) <-  names(split_li[[1]])
  
  # Go back to the main directory and write the resulting dataset
  cat("\nWriting the tidy data set...")
  setwd("..")
  write.table(x=tidy_df,row.names = FALSE,file = "tidyDataSet.txt")
  cat("\nDone.\n")
 
 
 
 
