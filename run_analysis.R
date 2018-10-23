library(plyr)

## You should create one R script called run_analysis.R that does the following.

##############################################################################
## 01. Merges the training and the test sets to create one data set.
##############################################################################


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "Getting_and_Cleaning_Data_Project_dataset.zip"

# download data file if it does not exist
if (!file.exists(fileName)) {
        download.file(fileUrl, fileName, mode = "wb")
}

# unzip data file if data directory doesn't exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(zipFile)
}

# read data files from different directories
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features file
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
##colnames(activities) <- c("activityId", "activityLabel")

# merge train and test data and then merge all the data
trainMerge <- cbind(trainingSubjects, trainingValues, trainingActivity)
testMerge <- cbind(testSubjects, testValues, testActivity)
allDataMerged <- rbind(trainMerge, testMerge)
colnames(allDataMerged) <- c("subject", features[, 2], "activity")



##############################################################################
## 02. Extracts only the measurements on the mean and standard deviation for each measurement.
##############################################################################

# subset the data (name of Features, measurements on the mean and standard deviation)
selectedMeasurements <- grepl("subject|activity|mean|std", colnames(allDataMerged))
subsetData <- allDataMerged[, selectedMeasurements]

##############################################################################
## 03. Use descriptive activity names to name the activities in the data set
##############################################################################

# replace values matching activities with named factor levels (activities <- activity_labels.txt)
subsetData$activity <- factor(subsetData$activity, labels = activities[, 2])

##############################################################################
## 04. Appropriately labels the data set with descriptive variable names.
##############################################################################

# get the column names and make them more understandable
subsetDataColNames <- colnames(subsetData)

subsetDataColNames <- gsub("^t", "time", subsetDataColNames)
subsetDataColNames <- gsub("^f", "frequency", subsetDataColNames)
subsetDataColNames <- gsub("Acc", "Accelerometer", subsetDataColNames)
subsetDataColNames <- gsub("Gyro", "Gyroscope", subsetDataColNames)
subsetDataColNames <- gsub("Mag", "Magnitude", subsetDataColNames)

colnames(subsetData) <- subsetDataColNames

##############################################################################
## 05. From the data set in step 4, creates a second, independent tidy data 
##      set with the average of each variable for each activity and each subject.
##############################################################################

# Create a new set for the averages
meanData <- ddply(subsetData, .(subject, activity), function(x) colMeans(x[, 2:67]))

# Write a new file from the averages table
write.table(meanData, file = "tidydata.txt",row.name=FALSE)


