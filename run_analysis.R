##Create and merge training and test sets
features <- read.table('UCI HAR Dataset/features.txt',
                       col.names = c('Index_Feature','Feature'))
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt',
                              col.names = c('Index_Activity','Activity'))

X_train <- read.table('UCI HAR Dataset/train/X_train.txt',
                      col.names = features$Feature,check.names = FALSE)
y_train <- read.table('UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('UCI HAR Dataset/train/subject_train.txt')

X_test <- read.table('UCI HAR Dataset/test/X_test.txt',
          col.names = features$Feature,check.names = FALSE)
y_test <- read.table('UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('UCI HAR Dataset/test/subject_test.txt')

X_train_p = cbind(y_train, subject_train,X_train)
X_test_p = cbind(y_test, subject_test,X_test)

names(X_train_p)[c(1,2)] <- c('Activity','Subject')
names(X_test_p)[c(1,2)] <- c('Activity','Subject')

write.csv(x=X_train_p,file = 'X_train.csv',sep = ',',row.names = F)
write.csv(x=X_test_p,file = 'X_test.csv',sep = ',',row.names = F)

X_data <- rbind(X_train,X_test)
y_data <- rbind(y_train,y_test)
subject_data <- rbind(subject_train,subject_test)

##Extract mean and std dev
col_names <- colnames(X_data)
col_mean  <- grep('mean\\(\\)',col_names)
col_std   <- grep('std\\(\\)',col_names)

X_mean_std <- X_data[,c(col_mean,col_std)]


##Using descriptive activity names
X_clean <- cbind(y_data,X_mean_std)
colnames(X_clean)[1] <- 'Index_Activity'
X_clean <- cbind(subject_data,X_clean)
colnames(X_clean)[1] <- 'Subject'

X_clean <- merge(activity_labels,X_clean)
X_clean$Index_Activity <- NULL


##Labeling data set with descriptive variable names
col_names <- colnames(X_clean)

col_names <- sub(x = col_names,pattern = '^t',replacement = 'Time domain signal: ')
col_names <- sub(x = col_names,pattern = '^f',replacement = 'Frequency domain signal: ')
col_names <- sub(x = col_names,pattern = '-',replacement = ', ')
col_names <- sub(x = col_names,pattern = 'mean\\(\\)',replacement = ' mean value ')
col_names <- sub(x = col_names,pattern = 'std\\(\\)',replacement = ' standart deviation ')
col_names <- sub(x = col_names,pattern = '-X',replacement = 'in X direction')
col_names <- sub(x = col_names,pattern = '-Y',replacement = 'in Y direction')
col_names <- sub(x = col_names,pattern = '-Z',replacement = 'in Z direction')
col_names <- sub(x = col_names,pattern = 'AccJerk',replacement = ' acceleration jerk')
col_names <- sub(x = col_names,pattern = 'Acc',replacement = ' acceleration')
col_names <- sub(x = col_names,pattern = 'GyroJerk',replacement = ' angular velocity jerk')
col_names <- sub(x = col_names,pattern = 'Gyro',replacement = ' angular velocity')
col_names <- sub(x = col_names,pattern = 'Mag',replacement = ' magnitude')

colnames(X_clean) <- col_names


##Tidy data set
X_tidy <- aggregate(X_clean[,3:68],by=list(X_clean$Activity,X_clean$Subject),FUN=mean)
colnames(X_tidy)[1] <- 'Activity'
colnames(X_tidy)[2] <- 'Subject'

##Storing tidy data set
write.table(X_tidy,file = 'final_tidy_set.txt',row.names = F)