#README

## Introduction
This file illustrates the steps by which the script *run_analysis.R* performs the transformation on the provided data.

## Environment and preparation
The script should be loaded in a directory alongside with the zip file containing the original data (*getdata_projectfiles_UCI HAR Dataset.zip*). Once it's launched, the script unzips the data file and sets the working directory to *./UCI HAR Dataset*, the main data directory. It then starts to process the data.

## Preprocessing
The data is split in two subdirectories:test and train. In each of these subdirectories, the data is split in three different files. For the *test* subdirectory:
 
 1. X_test.txt (containing the 561 features measured from the phones9
 2. y_test.txt (containing the activities performed)
 3. subject_test.txt (containing the subject id)
 
For the *train* subdirectory:

1. X_train.txt (containing the 561 features measured from the phones9
2. y_train.txt (containing the activities performed)
3. subject_train.txt (containing the subject id)

Before we can merge the training and test sets we need to consolidate three files into a single data frame. So, for the test data, we will first  load the three txt files in three different data frames, then bind together the three data frames into a single test data frame (__test_df__)

The same operation will be performed with the train data: first the three txt files are loaded in three data frames, then the data frames are consolidated in a single train data frame (__train_df__).

Once we have these two dataframes, we can merge them to get a single dataframe, named __df__. 
This dataframe has 10299 rows and 563 columns. It has no column names, and the activities are numerical (from 1 to 6).

## Feature selection
The only variables we're interested in are those that have the string *mean()* or *std()* in the description, so we need to identify and extract them from the data frame.
In order to do so, we will first load the *features.txt* file in a data frame and then, using the __grep-- command, we find the indices of the variables that contain either the string *mean(* or the string *std(*. 
The grep command finds in all 66 variables that satisfy this requirements: these are stored in the __selectedFeatures_df__ data frame, which will be used to subset the main data frame.

The new data frame (__selected_df__) will contain the 66 variables indicated in the __selectedFeatures_df__ data frame, plus the subject and activity variables (which were at the end of the original data frame, and are thus identified by the indices__dim(df)[2]-1__ and __dim(df)[2]__ (that is the last and second to last columns of the original data frame).

Once the data frame is complete we need to name the columns: this is accomplished by assigning to the first 66 columns of the data frame the names of the selected variables, plus the names 'Subject' and 'Activity' for the last two columns.
The values of the activity, is now factored according to the list in the *activity_labels.txt* file.

## Data processing
In order to compute the average of every variable for each activity and each subject, we first split the data frame using Subject and Activity as factors for the splitting. The resulting list ( __split_li__ ) is then passed to an lapply function, in which I have used an inline function ,in order to compute the mean of the variables. This was necessary because the data frame (and hence the resulting split list) has 66 numeric variables but the last two columns are not numeric. So we need to tell the meanCol function that it must work only to the first 66 columns.

Once all the numeric averages have been computed they are put into a matrix that is subsequently transformed into a data frame (__tidy_df__).

The last step is to recover from the __split_li__ list the values of subject and activity.
These are present in the format "1.WALKING" (subject 1, action WALKING", and so must be recovered using the __str_extract_all__ string functions, in conjunction with the __paste__ funcion. Once they're recovered, they are column binded to the __tidy_df__.
