## This function was written by Daniel Bondeson on 4 October 2018 for the Getting and
## Cleaning Data final project. The  user should navigate to the UCI HAR Dataset 
## directory and then run the function. 

##  The function caches certain variables in the global environment: 
##  - The Test and Training datasets if the program needs to be rerun. 
##  - Total_Data set, which contains observations for all 30 subjects across 
##    the mean and standard deviation for 79 different measurements. 
##  - Summary_Table, which contains the average of each measurement recorded in 
##    Total_Data

## In addition, the function writes Total_Data, Summary_Table, and the variable 
## names of each to table files.



run.analysis <- function (){
        library(dplyr)
        
        start_dir <- getwd()
        print("Processing...")
        setwd("/Users/dbondeso/Desktop/Coursera/Cleaning Data/Course Project/UCI HAR Dataset")
        
        # Read the test and train datasets, inlcuding the subject and activites,
        # and changing variable names. 
        
        # Because the training and test sets are large, the program will 
        # cache them in memory and only reread the files if they haven't been 
        # read already. 
        
        features <- read.table("features.txt", stringsAsFactors = F)
        
        if (!exists("Train_Set")) {
                print("Getting Training Set")
                setwd("./train")
                train <- read.table("X_train.txt", col.names = features[,2])
                labels <- read.table("y_train.txt", col.names = "Activity")
                subjects <- read.table("subject_train.txt", col.names = "Subject")
                labels[,1] <- as.factor(labels[,1])
                
                train <- cbind(subjects, labels, train)
                Train_Set <<- train
                setwd("..")
                print("Set Train")
        } else {train <- Train_Set
                print("Retrieved Train from cache")}
        
        # Same for the test set. 
        
        if (!exists("Test_Set")) {
                print("Getting Test Set")
                setwd("./test")
                test <- read.table("X_test.txt", col.names = features[,2])
                labels <- read.table("y_test.txt", col.names = "Activity")
                labels[,1] <- as.factor(labels[,1])
                subjects <- read.table("subject_test.txt", col.names = "Subject")

                test <- cbind(subjects, labels, test)
                Test_Set <<- test
                setwd("..")
                print("Set Test")
        } else {test <- Test_Set
                print("Retrieved Test from cache")}
        
        # Bind the test and train into "combined", and then select only columns
        # containing the words "mean" or "std"
        
        combined <- rbind(test, train)
        sel <- c(1, 2, grep("std", names(combined)), grep("mean", names(combined)))
        sel <- sort(sel)
        combined <- combined[,sel]
        
        # Rename the activities using descriptive labels. This is done by 
        # converting the factor levels, but the "activity" column is converted 
        # back to char at the end to aid in downstream analysis. 
        
        activities <- factor(read.table("activity_labels.txt")[,2],
                             ordered = T)
        levels(combined$Activity) <- levels(activities)
        combined$Activity <- as.character(combined$Activity)
        
        # Reorder the data frame by subject and then activity for ease of view.
        combined <- combined[order(combined$Subject, combined$Activity),]
        
        # Total_Data is the resultant tidy dataset.
        for (i in 3:ncol(combined)) {
                combined[,i] <- as.numeric(combined[,i])
        }
        
        Total_Data <<- combined
        write.table(combined, "/Users/dbondeso/Desktop/Coursera/Cleaning Data/Course Project/UCI HAR Dataset/Total_Data.txt")
        print("The combined, tidy dataset is stored as Total_Data")
        
        
        # From Total_Data, create a second, independent tidy 
        # data set with the average of each variable for each activity 
        # and each subject.
        
        summary <- Total_Data %>% 
                        group_by(Subject, Activity) %>% 
                        summarize_at(3:81, funs(mean))
        
        # Coerce summary back into a data frame with numeric values
        summary <- data.frame(summary)
        for (i in 3:ncol(summary)) {
                summary[,i] <- as.numeric(summary[,i])
        }
        Summary_Table <<- summary
        write.table(summary, "/Users/dbondeso/Desktop/Coursera/Cleaning Data/Course Project/UCI HAR Dataset/Summary_Table.txt")
        write.table(names(summary), "/Users/dbondeso/Desktop/Coursera/Cleaning Data/Course Project/UCI HAR Dataset/Tidy_Var_Names.txt")
        #Go back to the original directory and tell the user. 
        print("Averages for each subject/activity pair is stored as Summary_Table")
        setwd(start_dir)
}