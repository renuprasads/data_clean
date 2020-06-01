

# Load the needed packages
packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
#set path to the UCI HAR Dataset folder in the github repo
projectDataPath  <- getwd()

fileCount <- length(list.files(projectDataPath, recursive=TRUE))


dtTrainingSubjects <- fread(file.path(projectDataPath, "train", "subject_train.txt"))
dtTestSubjects  <- fread(file.path(projectDataPath, "test" , "subject_test.txt" ))

# Read in the 'Activity' data
dtTrainingActivity <- fread(file.path(projectDataPath, "train", "Y_train.txt"))
dtTestActivity  <- fread(file.path(projectDataPath, "test" , "Y_test.txt" ))


dtTrainingMeasures <- data.table(read.table(file.path(projectDataPath, "train", "X_train.txt")))
dtTestMeasures  <- data.table(read.table(file.path(projectDataPath, "test" , "X_test.txt")))


dtSubjects <- rbind(dtTrainingSubjects, dtTestSubjects)
setnames(dtSubjects, "V1", "subject")

# Row merge the Training and Test Activities
dtActivities <- rbind(dtTrainingActivity, dtTestActivity)
setnames(dtActivities, "V1", "activityNumber")

# Merge the Training and Test 'Measurements' data
dtMeasures <- rbind(dtTrainingMeasures, dtTestMeasures)

# Column merge the subjects to activities
dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dtSubjectAtvitiesWithMeasures <- cbind(dtSubjectActivities, dtMeasures)

# Order all of the combined data by, subject and activity
setkey(dtSubjectAtvitiesWithMeasures, subject, activityNumber)

## Read in the 'features.txt' 
## with the features/measures.
dtAllFeatures <- fread(file.path(projectDataPath, "features.txt"))
setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)]

dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)]

# Build up the columns to select from the data.table,
# dtSubjectActivitiesWithMeasures
columnsToSelect <- c(key(dtSubjectAtvitiesWithMeasures), dtMeanStdMeasures$measureCode)
# Just take the rows with the columns of interest ( std() and mean() )
dtSubjectActivitesWithMeasuresMeanStd <- subset(dtSubjectAtvitiesWithMeasures, 
                                                select = columnsToSelect)

# Read in the activity names and give them more meaningful names
dtActivityNames <- fread(file.path(projectDataPath, "activity_labels.txt"))
setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName"))

# Merge the 'meaningful activity names' with the 
# dtSubjectActiitiesWithMeasuresMeanStd
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtActivityNames, by = "activityNumber", 
                                               all.x = TRUE)

# Sort the data.table, dtSubjectActivitesWithMeasuresMeanStd
setkey(dtSubjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
dtSubjectActivitesWithMeasuresMeanStd <- data.table(melt(dtSubjectActivitesWithMeasuresMeanStd, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureCode", 
                                                         value.name="measureValue"))

# Merge measure codes
dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd, 
                                               dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)], 
                                               by="measureCode", all.x=TRUE)

# Convert activityName and measureName to factors
dtSubjectActivitesWithMeasuresMeanStd$activityName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$activityName)
dtSubjectActivitesWithMeasuresMeanStd$measureName <- 
  factor(dtSubjectActivitesWithMeasuresMeanStd$measureName)

# Reshape the data to get the averages 
measureAvgerages <- dcast(dtSubjectActivitesWithMeasuresMeanStd, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")

