#Course Project

setwd("/Users/Cellist/Documents/Personal/Professional/CleaningData")

install.packages("dplyr")
library(dplyr)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fitnessFilename <- "./data/FitnessData.zip"
if (!file.exists(fitnessFilename))
{
  download.file(fileUrl, destfile = fitnessFilename, method = "curl")
}
list.files("./data")
dateDownloaded <- date()

unzip(fitnessFilename)

featuresFrame <- read.table("./UCI HAR Dataset/features.txt")
#Create the training frame and the test frame. Name the frames' columns based on the names in features.txt
trainFrame <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = featuresFrame[,2])
testFrame <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = featuresFrame[,2])

#Add the subjects' IDs
subjectTrainIDs <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "SubjectID")
subjectTestIDs <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "SubjectID")

trainFrame$subjectID <- subjectTrainIDs[,1]
testFrame$subjectID <- subjectTestIDs[,1]

#1. Adds the test set to the training set to create one data set.
combinedFrame <- rbind(trainFrame, testFrame)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#Use dplyr to get the columns containing mean and standard deviation
meanMeasurements <- select(combinedFrame, contains("mean"))
stdMeasurements <- select(combinedFrame, contains("std"))

meansAndStdFrame <- cbind(meanMeasurements, stdMeasurements, combinedFrame$subjectID)


#3. Uses descriptive activity names to name the activities in the data set
#Retrieve the activities for each sample
trainActivities <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "ID")
testActivities <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "ID")
combinedActivities <- rbind(trainActivities, testActivities)

#Get the names of the activities
namesOfActivities <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("ID", "Name"))

#Merge the names into the activities
descriptiveActivityNames <- merge(combinedActivities, namesOfActivities, sort = FALSE)

#Add the activity names into the combined data set
meansAndStdAndActivitiesFrame <- meansAndStdFrame
meansAndStdAndActivitiesFrame$Activity <- descriptiveActivityNames$Name

#4. Appropriately labels the data set with descriptive variable names. 
# This was already done in lines 21-22 by reading in the column names when reading in the original training and test sets


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Creates mean of tBodyAcc.mean...X by activity
tidyFrame <- by(meansAndStdAndActivitiesFrame$tBodyAcc.mean...X, 
                meansAndStdAndActivitiesFrame$Activity,
                mean, na.rm = TRUE)

#Doesn't work. Creates [1:6, 1:30], all NA
tidyFrame <- by(meansAndStdAndActivitiesFrame[,c("Activity", "tBodyAcc.mean...X", "tGravityAcc.std...X")],
                INDICES = list(meansAndStdAndActivitiesFrame$Activity,
                               meansAndStdAndActivitiesFrame$`combinedFrame$subjectID`),
                mean, na.rm = TRUE)

#Seems to work. Creates a mean of one measure (only) for each subject and for each activity
#Need to do this for all measures.
tidyFrame <- by(meansAndStdAndActivitiesFrame$tBodyAcc.mean...X, 
                INDICES = list(meansAndStdAndActivitiesFrame$Activity,
                               meansAndStdAndActivitiesFrame$`combinedFrame$subjectID`),
                mean, na.rm = TRUE)
