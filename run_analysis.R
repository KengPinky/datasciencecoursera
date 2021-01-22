# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Load the data

path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip"))
unzip("data.zip")

# extract the data for measurements and labels
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                 , col.names = c("id", "feature"))

activitylabel <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                  , col.names = c("class", "activity"))

neededmeasures <- grep("(mean|std)\\(\\)", features[, feature])
measure <- features[neededmeasures, feature]
measurement <- gsub('[()]', '', measure)

# load test data
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))
testmeasure <- test[, neededmeasures, with = FALSE]
data.table::setnames(testmeasure, colnames(testmeasure), measurement)
tactivity <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                   ,col.names = c("Activity"))
tsub <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
              ,col.names = c("SubNum"))
cTest <- cbind(tsub, tactivity, testmeasure)

#load train data
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))
traindata <- train[, neededmeasures, with = FALSE]
data.table::setnames(traindata, colnames(traindata), measurement)
trainAct <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSub <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubNum"))
cTrain <- cbind(trainSub, trainAct, traindata)

#merge data and use labels
bothsets <- rbind(cTrain, cTest)
bothsets[["Activity"]] <- factor(bothsets[, Activity]
                                 ,levels = activitylabel[["class"]]
                                 ,labels = activitylabel[["activity"]])

#create second tidy set
bothsets[["SubNum"]] <- as.factor(bothsets[, SubNum])
bothsets2 <- reshape2::melt(data = bothsets, id = c("SubNum", "Activity"))
bothsets2 <- reshape2::dcast(data = bothsets2, SubNum + Activity ~ variable, fun.aggregate = mean)
