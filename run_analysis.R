library(dplyr)
library(tidyr)
## 1. Merges the training and the test sets to create one data set.
data_dir<-"UCI HAR Dataset"

## common file reading
get_course_data<-function(subdir,file,labels)
{
  file<-file.path(".",data_dir,subdir,file,fsep="\\")
  data<-read.table(file, col.names=labels)
  data
}

run_it <- function()
{
## Get lables for data frames
label_file<-file.path(".",data_dir, "features.txt")
labels<-read.table(label_file)[["V2"]]

## load data from files
test_data<-get_course_data("test","x_test.txt", labels)
train_data<-get_course_data("train","x_train.txt", labels)

total_data<-bind_rows(test_data,train_data)

## clean up a bit
rm(test_data)
rm(train_data)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

only_mean_stddev=select(total_data, contains("std"), contains("mean")) %>%
  mutate(id=1:n())
rm(total_data) ## clean up

## 3. Uses descriptive activity names to name the activities in the data set
by_id_activity<-gather(only_mean_stddev, activity, value,-id)
with_functions<-separate(by_id_activity, activity, c("action", "func", "dim"), sep="[.]+", extra="drop")

## 4. Appropriately labels the data set with descriptive variable names. 
by_std_or_mean<-spread(with_functions, func, value) %>%
  select(id, action,dim,mean,std) %>%
  filter(!is.na(std) & !is.na(mean))
## 5. From the data set in step 4, creates a second,
##      independent tidy data set with the average of 
##      each variable for each activity and each subject.
summarize(group_by(by_std_or_mean, action), mean(mean), mean(std))
}

ret<-run_it()
write.table(ret, "answer.txt", row.name=FALSE)
