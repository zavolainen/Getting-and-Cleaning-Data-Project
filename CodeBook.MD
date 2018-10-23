title: "Getting and Cleaning Data Peer-graded Assignment"
author: "Jani Savolainen"
date: "23.10.2018"
output: "tidydata.txt""

# Code Book

### Overall

Tidy data, tidydata.txt, is created by R script called run_analysis.R.Data contains 180 rows and 68 columns. The rows contain means and standard deviations calculated from the original dataset provided (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). Original data set is called Human Activity Recognition Using Smartphones.

### Script

The script behaves as the per instructions attached below.

You should create one R script called run_analysis.R that does the following.
- Merges the training and the test sets to create one data set.
- Extracts only the measurements on the mean and standard deviation for each measurement.
- Uses descriptive activity names to name the activities in the data set
- Appropriately labels the data set with descriptive variable names.
- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Values and columns

Activity column can have six different values. Subject column has integer values between 1 and 30. Other columns have values between -1 and 1.

Activity column values:
WALKING
WALKING_UPSTAIRS
WALKING_DOWNSTAIRS
SITTING
STANDING
LAYING

Columns 
1                                                 subject
2                          timeBodyAccelerometer-mean()-X
3                          timeBodyAccelerometer-mean()-Y
4                          timeBodyAccelerometer-mean()-Z
5                           timeBodyAccelerometer-std()-X
6                           timeBodyAccelerometer-std()-Y
7                           timeBodyAccelerometer-std()-Z
8                       timeGravityAccelerometer-mean()-X
9                       timeGravityAccelerometer-mean()-Y
10                      timeGravityAccelerometer-mean()-Z
11                       timeGravityAccelerometer-std()-X
12                       timeGravityAccelerometer-std()-Y
13                       timeGravityAccelerometer-std()-Z
14                     timeBodyAccelerometerJerk-mean()-X
15                     timeBodyAccelerometerJerk-mean()-Y
16                     timeBodyAccelerometerJerk-mean()-Z
17                      timeBodyAccelerometerJerk-std()-X
18                      timeBodyAccelerometerJerk-std()-Y
19                      timeBodyAccelerometerJerk-std()-Z
20                             timeBodyGyroscope-mean()-X
21                             timeBodyGyroscope-mean()-Y
22                             timeBodyGyroscope-mean()-Z
23                              timeBodyGyroscope-std()-X
24                              timeBodyGyroscope-std()-Y
25                              timeBodyGyroscope-std()-Z
26                         timeBodyGyroscopeJerk-mean()-X
27                         timeBodyGyroscopeJerk-mean()-Y
28                         timeBodyGyroscopeJerk-mean()-Z
29                          timeBodyGyroscopeJerk-std()-X
30                          timeBodyGyroscopeJerk-std()-Y
31                          timeBodyGyroscopeJerk-std()-Z
32                  timeBodyAccelerometerMagnitude-mean()
33                   timeBodyAccelerometerMagnitude-std()
34               timeGravityAccelerometerMagnitude-mean()
35                timeGravityAccelerometerMagnitude-std()
36              timeBodyAccelerometerJerkMagnitude-mean()
37               timeBodyAccelerometerJerkMagnitude-std()
38                      timeBodyGyroscopeMagnitude-mean()
39                       timeBodyGyroscopeMagnitude-std()
40                  timeBodyGyroscopeJerkMagnitude-mean()
41                   timeBodyGyroscopeJerkMagnitude-std()
42                    frequencyBodyAccelerometer-mean()-X
43                    frequencyBodyAccelerometer-mean()-Y
44                    frequencyBodyAccelerometer-mean()-Z
45                     frequencyBodyAccelerometer-std()-X
46                     frequencyBodyAccelerometer-std()-Y
47                     frequencyBodyAccelerometer-std()-Z
48                frequencyBodyAccelerometer-meanFreq()-X
49                frequencyBodyAccelerometer-meanFreq()-Y
50                frequencyBodyAccelerometer-meanFreq()-Z
51                frequencyBodyAccelerometerJerk-mean()-X
52                frequencyBodyAccelerometerJerk-mean()-Y
53                frequencyBodyAccelerometerJerk-mean()-Z
54                 frequencyBodyAccelerometerJerk-std()-X
55                 frequencyBodyAccelerometerJerk-std()-Y
56                 frequencyBodyAccelerometerJerk-std()-Z
57            frequencyBodyAccelerometerJerk-meanFreq()-X
58            frequencyBodyAccelerometerJerk-meanFreq()-Y
59            frequencyBodyAccelerometerJerk-meanFreq()-Z
60                        frequencyBodyGyroscope-mean()-X
61                        frequencyBodyGyroscope-mean()-Y
62                        frequencyBodyGyroscope-mean()-Z
63                         frequencyBodyGyroscope-std()-X
64                         frequencyBodyGyroscope-std()-Y
65                         frequencyBodyGyroscope-std()-Z
66                    frequencyBodyGyroscope-meanFreq()-X
67                    frequencyBodyGyroscope-meanFreq()-Y
68                    frequencyBodyGyroscope-meanFreq()-Z
69             frequencyBodyAccelerometerMagnitude-mean()
70              frequencyBodyAccelerometerMagnitude-std()
71         frequencyBodyAccelerometerMagnitude-meanFreq()
72     frequencyBodyBodyAccelerometerJerkMagnitude-mean()
73      frequencyBodyBodyAccelerometerJerkMagnitude-std()
74 frequencyBodyBodyAccelerometerJerkMagnitude-meanFreq()
75             frequencyBodyBodyGyroscopeMagnitude-mean()
76              frequencyBodyBodyGyroscopeMagnitude-std()
77         frequencyBodyBodyGyroscopeMagnitude-meanFreq()
78         frequencyBodyBodyGyroscopeJerkMagnitude-mean()
79          frequencyBodyBodyGyroscopeJerkMagnitude-std()
80     frequencyBodyBodyGyroscopeJerkMagnitude-meanFreq()
81                                               activity
