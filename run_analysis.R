library(dplyr)

#Load train data set
y_train <- read.table("train/y_train.txt", sep =  "", stringsAsFactors = F)
subject_train <- read.table("train/subject_train.txt", sep =  "", stringsAsFactors = F)
x_train <- read.table("train/x_train.txt", sep =  "", stringsAsFactors = F)
colIndexes <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503 , 504, 516, 517, 526, 529:530, 539, 542:543, 552)
x_train <- tbl_df(x_train[,colIndexes])
names(subject_train) <- "Subject"
names(y_train) <- "Activity"
finaltrain <- tbl_df(cbind(subject_train, y_train, x_train))

#Load test data set
subject_test <- read.table("test/subject_test.txt", sep =  "", stringsAsFactors = F)
x_test <- read.table("test/x_test.txt", sep =  "", stringsAsFactors = F)
colIndexes <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503 , 504, 516, 517, 526, 529:530, 539, 542:543, 552)
#I did this manually, could have been done by grep() function as well.
x_test <- tbl_df(x_test[,colIndexes])
names(subject_test) <- "Subject"
names(y_test) <- "Activity"
finaltest <- tbl_df(cbind(subject_test, y_test, x_test))

# Bind and arrange by 1 - Subject, 2 - Activity
dataset <- rbind(finaltrain, finaltest)
dataset <- arrange(dataset, Subject, Activity)

# Making the set readable and changing variables and activity names
features <- read.csv("features.txt", sep = "", stringsAsFactors = F, header = F)
features <- features[,2]
features <- features[colIndexes]
names(dataset) <- c("Subject", "Activity", features)

#The below transformation could have been done just by converting to factors levels-labels, but if - else is also an option.
activitynames <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
for (i in 1:10299) {
  if(dataset$Activity[i] == 1) {dataset$Activity[i] = activitynames[1]
    } else if(dataset$Activity[i] == 2) {dataset$Activity[i] = activitynames[2]
     }  else if(dataset$Activity[i] == 3) {dataset$Activity[i] = activitynames[3]
      }   else if(dataset$Activity[i] == 4) {dataset$Activity[i] = activitynames[4]
       }    else if(dataset$Activity[i] == 5) {dataset$Activity[i] = activitynames[5]
        }     else if(dataset$Activity[i] == 6) {dataset$Activity[i] = activitynames[6]}
                    }
  

# New data set with averaged values
dataset2 <- dataset %>% group_by(Subject, Activity) %>% summarise_all(funs(mean))
write.table(dataset2, "tidydataset.txt", row.names = FALSE)

