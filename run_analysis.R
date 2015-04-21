#Cleaning Human activity recognition dataset for data science course
#created with R version 3.1.2, R studio version 0.98.1091.

#Import all tables
features <- read.table("./UCI HAR Dataset/features.txt", colClasses = c(NULL,"character"))
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_trains <- read.table("./UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Revise column names (I chose lower case period separated)
library("stringr")
rev_features <- str_replace_all(features[,2],",|-", "PERIOD")
rev_features <- str_replace_all(rev_features, "[[:punct:]]", "")
rev_features <- str_replace_all(rev_features, "PERIOD", ".")
rev_features <- str_replace_all(rev_features, "([A-Za-z])([A-Z])([a-z])", "\\1.\\2\\3")
rev_features <- tolower(rev_features)
activity_labels$V2 <- tolower(activity_labels$V2)
colnames(activity_labels) <- c("activity.code","activity.desc")

#Prepare for merging
colnames(x_test) <- rev_features
colnames(x_train) <- rev_features
colnames(y_test) <-"activity.code"
library("dplyr")
y_test <- left_join(y_test, activity_labels, by="activity.code")
y_test$test_train <- "test"
colnames(y_train) <- "activity.code"
y_train <- left_join(y_train, activity_labels, by="activity.code")
y_train$test_train <- "train"
colnames(subject_test) <- "subject"
colnames(subject_trains) <- "subject"

#Merge datasets
test_combine <- cbind(subject_test,y_test,x_test)
train_combine <- cbind(subject_trains,y_train,x_train)

train_test_set <- rbind(test_combine,train_combine)

#Final correction for column names, as per http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name
rev_column_names <- make.names(names=names(train_test_set), unique=TRUE, allow_ = TRUE)
colnames(train_test_set) <- rev_column_names

#Restrict dataset to mean and sd of each measurement - excluding the weighted average of the frequency components and 'angle' variables
train_test_set <- select(train_test_set, subject:test_train, contains("mean"), contains("std"), 
                         -contains("mean.freq"), -contains("angle"))

#Average varables by subject and activity
train_test_summarise <- summarise_each(group_by(train_test_set,subject, activity.code, activity.desc),  
                                  funs(mean), -test_train)
colnames(train_test_summarise) <- c(names(train_test_summarise[1:3]),
                                    paste("mean.", names(train_test_summarise[4:69]),sep=""))

#Export summarised dataset
write.table(train_test_summarise, "tidy_data_set.txt", row.name=FALSE)

write.table(names(train_test_summarise), "tidy_names.txt", row.names=FALSE)
