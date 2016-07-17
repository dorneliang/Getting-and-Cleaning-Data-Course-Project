#####Getting and Cleaning Data Course Project
####Chengdong Liang
###2015-07-17


## Download and unzip original dataset
filename <-"project_dataset.zip"
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("/Users/chengdong/Get_Clean_Data/Project")) {
  dir.create("/Users/chengdong/Get_Clean_Data/Project")
}
getwd()
setwd("/Users/chengdong/Get_Clean_Data/Project")
if(!file.exists("project_dataset.zip")){
  download.file(fileUrl,destfile="project_dataset.zip", method= "curl")
}

if(!file.exists("UCI HAR Dataset")){
  unzip("project_dataset.zip")
}
##Read the data from files
features <-read.table("./UCI HAR Dataset/features.txt",header=FALSE)
activity_Type <-read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)

#Training Data
subject_train <-read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
x_train <-read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
y_train <-read.table("./UCI HAR Dataset/train/Y_train.txt",header=FALSE)

#Testing Data
subject_test <-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
x_test <-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
y_test <-read.table("./UCI HAR Dataset/test/Y_test.txt",header=FALSE)

##Assign column names to dataset imported
colnames(activity_Type)= c("activityID","activityType")
colnames(subject_train)= "subjectID"
colnames(x_train)= features[,2]
colnames(y_train)= "activityID"
colnames(subject_test)= "subjectID"
colnames(x_test)= features[,2]
colnames(y_test)= "activityID"

##Create the final training dataset by merging y_train, subject_train and x_train
train_dataset <-cbind(y_train, subject_train, x_train)

##Create the final testing dataset by merging y_test, subject_test and x_test
test_dataset <-cbind(y_test, subject_test, x_test)

##Combine training and testing dataset to creat final dataset
final_dataset <-rbind(train_dataset, test_dataset)

##Extract only the measurements on the mean and standard deviation for each measurement
col_names <-colnames(final_dataset)
logicalVector <-(grepl("activity..", col_names)|grepl("subject..", col_names)|grepl("-mean..", col_names)&!grepl("-meanFreq..", col_names)&!grepl("mean..-", col_names)|grepl("-std..", col_names)&!grepl("-std()..-", col_names))
final_dataset=final_dataset[logicalVector==TRUE]

##Use descriptive activity nams to name activities in the dataset
head(final_dataset)
final_dataset<- merge(final_dataset, activity_Type, by='activityID', all.x=TRUE)
col_names <-colnames(final_dataset)
for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("^(t)","time",col_names[i])
  col_names[i] = gsub("^(f)","freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
};
colnames(final_dataset)= col_names
head(final_dataset)
names(final_dataset)
##Create a second, independent tidy dataset with the average of each variable for eah activity and each subject
final_dataset2 <-final_dataset[,names(final_dataset)!="activityType"]
tidy_data <-aggregate(final_dataset2[,names(final_dataset2)!=c("activityID", "subjectID")], by=list(activityID= final_dataset2$activityID,subjectID= final_dataset2$subjectID), mean)
tidy_data <-merge(tidy_data, activity_Type, by="activityID", all.x=TRUE)
write.table(tidy_data, "./tidy.text", row.names=FALSE,quote=FALSE)
