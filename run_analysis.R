# file: run_analysis.R
# author jetliner 
# version 0.1 Date: March 19 2015
# SEE README.md for instructins on how to run the file
# SEE CODEBOOK.md for details on the tidy data set, raw data and transformations.
# Description:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#    Any variable with mean() or std() in its name is included.
# 3. Uses descriptive activity names (activities_labels.txt) to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names using features.txt
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.



# get the train data 
# y values contain the activity
# x values contain the variable metrics
activity_train_data<-read.table("train/y_train.txt")
var_train_data<-read.table("train/x_train.txt")
subject_train_data<-read.table("train/subject_train.txt")

# get test data
activity_test_data<-read.table("test/y_test.txt")
var_test_data<-read.table("test/x_test.txt")
subject_test_data<-read.table("test/subject_test.txt")

# merge the train and tst sets
var_total_data <- rbind(var_test_data, var_train_data)
activity_total_data <- rbind(activity_test_data, activity_train_data)
subject_total_data <- rbind(subject_test_data, subject_train_data)


# add the variable descriptions to column names
# get feature data
feature_test_data<-read.table("features.txt",stringsAsFactors=FALSE)
names<-feature_test_data[,2]
colnames(var_total_data) <- names

#add the column names for acitvity and subject
colnames(activity_total_data) <-c("Activity_ID")
colnames(subject_total_data) <-c("Subject_ID")

# insert the activity name and merge
activity_labels_lookup<-read.table("activity_labels.txt",stringsAsFactors=FALSE)
colnames(activity_labels_lookup)<-c("Activity_ID","Activity_Desc")
activity_desc_data = merge(activity_total_data,activity_labels_lookup,by='Activity_ID',sort=FALSE)


# now subset the variable measures to only include variables that represent mean or std dev
var_total_mean_data<-var_total_data[ , grepl( "mean()" , names( var_total_data ),ignore.case = TRUE) ]
var_total_stddev_data<-var_total_data[ , grepl( "std()" , names( var_total_data ),ignore.case = TRUE) ]


# now create a consolidated dataset including the subject,the activity and the variable observations
# ensuring strings come in as chr rather than factors
# use subtotal variable to build up the full dataset
subtotal <- cbind(subject_total_data, activity_desc_data,stringsAsFactors=FALSE)
# drop the activity id whilst retaining the activity description
subtotal<-subset(subtotal,select = c(Subject_ID,Activity_Desc))
subtotal <- cbind(subtotal, var_total_mean_data,stringsAsFactors=FALSE)
total <- cbind(subtotal, var_total_stddev_data)
dateDownloaded <- date()

## now need to compute the average of each variable for each activity and each subject.
# first group by subject and activty_desc

tidy_data_set <-aggregate(total[3:ncol(total)], 
                          by=list(ACTIVITY_DESC=total$Activity_Desc,SUBJECT_ID=total$Subject_ID), 
                          FUN=mean, na.rm=TRUE)

# write out the file
write.table(tidy_data_set,file="tidy_data_set.txt",row.names=FALSE,col.names=TRUE)

# test the data file
# data <- read.table("tidy_data_set.txt", header = TRUE)
# View(data)

