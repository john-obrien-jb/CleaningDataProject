################## run_analysis.R
## Getting & Cleaning Data Course Project
##   created: November 2014, John O'Brien
## (add description here)
####################

## Project instructions:
# create one R script called run_analysis.R that does the following.
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#     with the average of each variable for each activity and each subject.

library(dplyr)
library(reshape2)

# link to source data in zip file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Function for downloading the dataset from the web
get_data <- function(source_url = url) {

  ## check to see if the directory exists already or create one
  if (!file.exists("./data/dataset.zip")) {
    dir.create("./data")
    print("Created dir for data.")
  }
  #print(list.files("."))

  ## check to see if the zip file exists already or download it
  if (!file.exists("./data/dataset.zip")) {
    download.file(url = url, destfile = "./data/dataset.zip", method = "auto")
    dateDownloaded <- date()
    print(c("==== Downloaded data on:  ",dateDownloaded))
  }
  #print(list.files("./data"))

  ####################################################################
  ## STEP 1 - Merges the training and the test sets to create one data set.
  ##

  ## extract and read the subject data from the zip file
  con_subject_train <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/train/subject_train.txt")
  subject_train <- read.table(con_subject_train)
  #print(str(subject_train))
  #close(con_subject_train)

  con_subject_test <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/test/subject_test.txt")
  subject_test <- read.table(con_subject_test)
  #print(str(subject_test))
  #close(con_subject_test)

  subject_combined <- rbind(subject_train, subject_test)
  #print(str(subject_combined))
  # add names to the subject table
  names(subject_combined) <- "subject_id"


  ## extract or read the activity data needed from the zip file
  con_activity_train <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/train/y_train.txt")
  activity_train <- read.table(con_activity_train)
  #print(str(activity_train))
  #close(con_activity_train)

  con_activity_test <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/test/y_test.txt")
  activity_test <- read.table(con_activity_test)
  #print(str(activity_test))
  #close(con_activity_test)

  activity_combined <- rbind(activity_train, activity_test)
  #print(str(activity_combined))
  # add names to the activity table
  names(activity_combined) <- "activity"
#   print("==========  activity_combined  ===================================== ")
#   print(str(activity_combined))


  ## extract or read the measurements data needed from the zip file
  con_measure_train <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/train/X_train.txt")
  measure_train <- read.table(con_measure_train)
  #print(str(measure_train))
  #close(con_measure_train)

  con_measure_test <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/test/X_test.txt")
  measure_test <- read.table(con_measure_test)
  #print(str(measure_test))
  #close(con_measure_test)

  measure_combined <- rbind(measure_train, measure_test)



  con_feat <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/features.txt")
  feat <- read.table(con_feat)
  #print(str(feat))
  #close(con_feat)

  # add names to the feat table
  names(feat) <- c("measure_num","measure_name")

 ####################################################################
 ## STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.
 ##

  #feat_keep <- feat[,feat$measure_num < 4 ]
#   feat_tbl <- tbl_df(feat)
#   filter(feat_tbl, measure_name %in% c("mean", "std") )
#  print("feat_tbl ================ ")
#  print(str(feat_tbl))

  mnames <- feat$measure_name
  keep_names <- mnames %in% grep("mean|std",mnames, value = TRUE)
#  print("===  keep_names ================ ")
#  print(str(keep_names))
# print(sum(keep_names))

#dtbl <- tbl_df(dframe)
# print("===  dtbl ================ ")
# print(str(dtbl))
#
# print("===  measure nums to keep ================ ")
# print(feat$measure_num[keep_names])

measure_dtbl <- tbl_df(measure_combined)
dtbl_small <- select(measure_dtbl, feat$measure_num[keep_names]) # select columns by position

# print("===  dtbl_small ================ ")
# print(str(dtbl_small))

dframe <- cbind(subject_combined, activity_combined, dtbl_small)
# print("data frame ================ ")
# print(str(dframe))

 ####################################################################
 ## STEP 3 Goal: Use descriptive activity names to name the activities in the data set
 ##

 ## extract or read the activity labels needed from the zip file
 con_al <- unz(description="./data/dataset.zip", filename="UCI HAR Dataset/activity_labels.txt")
 al <- read.table(con_al)

 # add names to the activity table
 names(al) <- c("activity_id","activity_label")
 #print(str(al))
 #close(con_al)

 # Now replace the activity codes (integers  1 to 6) with the text labels from the file activity_labels.txt
 dframe$activity <-  al[dframe$activity, 2]



####################################################################
## STEP 4 Goal: Appropriately label the data set with descriptive variable names.
##

## remove unhelpful parts of feature names using  make.names(names, unique = FALSE, allow_ = TRUE)
clean_names <- make.names(feat$measure_name[keep_names], unique = TRUE)
cleaner_names <- gsub('\\.+','',clean_names)  # take out the dots put in by make.names()

## replace the variable names on the data frame with the clean ones
names(dframe) <- c("subject_id","activity", cleaner_names)

#print(names(dframe))

####################################################################
## STEP 5 From the data set in step 4, creates a second, independent tidy data set
#     with the average of each variable for each activity and each subject.
#

# Example
## reshape2  does its thing:
# library(reshape2)
# melted <- melt(data, id.vars=c("sex", "treatment"))

## This part is new:
# library(dplyr)
# grouped <- group_by(melted, sex, treatment)
# summarise(grouped, mean=mean(value), sd=sd(value))



dframe_melted <- melt(dframe, id.vars=c("activity", "subject_id"))

dframe_grouped <-
dframe_melted %>% group_by(activity, subject_id) %>%
   summarise(mean=mean(value))

# dframe_grouped <- group_by(dframe_melted, activity, subject_id)
#                   summarize(dframe_grouped,mean=mean(value))

# print("data frame grouped  ================ ")
print(dframe_grouped)
print(dim(dframe_grouped))


write.table(dframe_grouped, file = "activity_subject_summary.txt", row.name=FALSE)
}

