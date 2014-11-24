
# Necessary libraries
library(data.table)
library(plyr)
# What I can consider constants, at least in this script
root = "./UCI HAR Dataset/"
activity_labels = read.table(file.path(root,"activity_labels.txt"))[,2]
features = read.table(file.path(root,"features.txt"))[,2]
# Since we are not interesting in all 561 features, we take only
# the one containing  means and standard deviations
# The outcoming type is "logical" (T/F)
proper_features = grepl("(mean|std)\\(\\)",features)
# Here we have the actual descriptive names
features_names = as.character(features[grep("(mean|std)\\(\\)", features)])
## 1. Merging the training and the test sets to creat one data set.
###############################################################################################
X_test <- read.table(file.path(root,"test","X_test.txt"))
Y_test <- read.table(file.path(root,"test","y_test.txt")) # activities
subject_test <- read.table(file.path(root,"test","subject_test.txt"))
X_train <- read.table(file.path(root,"train","X_train.txt"))
Y_train <- read.table(file.path(root,"train","y_train.txt")) # activities
subject_train <- read.table(file.path(root,"train","subject_train.txt"))
total_X <- rbind(X_test,X_train)
total_Y <- rbind(Y_test,Y_train) # activities
subjects <- rbind(subject_test,subject_train)
## 2. Extracting only the measurements on the mean and standard deviation for each measurement. 
###############################################################################################
X <- total_X[,proper_features]
## 3. Using descriptive activity names to name the activities in the data set.
###############################################################################################
data <- cbind(X, total_Y)
data <- cbind(data,subjects)
# This should be point #4, but since I prefer already using clear names, I add all the 
# descriptitve variable names here.
names(data) <- c(features_names,"Activity","Subject")
activitiesIds <- as.factor(data[,"Activity"])
# using plyr here ... I haven't found a way to use activity_labels properly for the vector
# so I had to write it by hand... not that happy about it, because I know it should be possible
activitiesDescribed = revalue(activitiesIds,c("1"="WALKING", "2"="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
data[,"Activity"] = activitiesDescribed
## 4. Appropriately labelling the data set with descriptive variable names.
###############################################################################################
# Done at passage #3, redoing it here to explain it better 
# if it were a script I would use, I wouldn't repeat myself
# but for peer review, I assume I should add it again, everybody has their own opinions on how
# properly fulfill the requirements 
names(data) <- c(features_names,"Activity","Subject") # extremely redundant
## 5. From the data set in step 4, createing a second, independent tidy data set with the 
## average of each variable for each activity and each subject
###############################################################################################
tidy <- data.table(data)
mean_and_tidy <- tidy[, lapply(.SD,mean), by=c("Activity","Subject")]
write.table(mean_and_tidy, file = "tidy_data.txt", sep = " ", row.names = FALSE)