# FILE run_analysis.R
# AUTHOR: BISIMWA MUGISHO Pacifique  
# OVERVIEW  
# Using data collected from the accelerometers and gyroscope from the Samsung Galaxy S 
# smartphone, the R codes below work with the data and make a clean data set, outputting the
# resulting tidy data to a file named "tidydata.txt". See README.md for details. 
#==============================================================================   

library(dplyr)

# STEP I - Getting the data
#===============================================================================

# download zip file containing data if it hasn't already been downloaded
rawdataziped <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
rawdatafile <- "UCI HAR Dataset.zip"

if (!file.exists(rawdatafile)) {
  download.file(rawdataziped, rawdatafile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(rawdatafile)
}

# STEP II - Reading the data
#===============================================================================

# reading training data
subjectsTrained <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingSet <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingLabels <- read.table(file.path(dataPath, "train", "y_train.txt"))

# reading test data
subjectsTested <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testSet <- read.table(file.path(dataPath, "test", "X_test.txt"))
testLabels <- read.table(file.path(dataPath, "test", "y_test.txt"))

# reading features avoiding conversion of text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
  ## note: feature names (in features[, 2]) are not unique
  ##       e.g. fBodyAcc-bandsEnergy()-1,8

# reading activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# STEP III - Merging the training and the test sets to create one data set
#================================================================================

# concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(subjectsTrained, trainingSet, trainingLabels),
  cbind(subjectsTested, testSet, testLabels)
)

# remove individual data tables to save memory
rm(subjectsTrained, trainingSet, trainingLabels, 
   subjectsTested, testSet, testLabels)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# STEP IV - Extract only the measurements on the mean and standard deviation
#          for each measurement
#=================================================================================

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]

# STEP V - Using descriptive activity names to name the activities in the data
#          set
#=================================================================================

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
  levels = activities[, 1], labels = activities[, 2])

# STEP VI - Appropriately label the data set with descriptive variable names
#=================================================================================

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

# STEP VII - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
#===========================================================================

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidydata.txt"
write.table(humanActivityMeans, "tidydata.txt", row.names = FALSE, 
            quote = FALSE)
