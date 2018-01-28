library(dplyr)

######################################################################################
# 1. Merging the training and the test sets.                                         #

# Getting train data set.
subject <- read.table ("./UCI HAR Dataset/train/subject_train.txt")
activity <- read.table ("./UCI HAR Dataset/train/y_train.txt")
feature_vector <- read.table ("./UCI HAR Dataset/train/X_train.txt")

train_data_set <- bind_cols (subject, activity, feature_vector)

# Getting test data set.
subject <- read.table ("./UCI HAR Dataset/test/subject_test.txt")
activity <- read.table ("./UCI HAR Dataset/test/y_test.txt")
feature_vector <- read.table ("./UCI HAR Dataset/test/X_test.txt")

test_data_set <- bind_cols (subject, activity, feature_vector)

# Now both training and test data frames have to be joined.
complete_data <- bind_rows (train_data_set, test_data_set)

######################################################################################
# 2. Extracting the measures on the mean and standard deviation for each measurement.#

# Obtaining the feature vector labels.
features <- read.table ("./UCI HAR Dataset/features.txt")

# Finding out the positions of the desired columns on the features vector.
mean_sd_columns <- grep ("mean|std", features[,2])

# Subseting the "complete_data" data frame by the desired columns.
# Note that on "complete_data" data frame the feature columns begin on column 3.
# That is why adding 2 to the "mean_sd_columns" is needed.
desired_data <- complete_data [, c(1, 2, 2+mean_sd_columns)]

######################################################################################
# 3. Changing activity column labels to have a more descriptive activity names.      #

# Reading the activity labels.
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Changing the activity names.
desired_data [, 2] <- factor (desired_data [, 2],
                              labels = as.character (activity_labels [, 2]))

######################################################################################
# 4. Labeling the data set with descriptive variabla names.                          #

names(desired_data) <- c("Subject", "Activity",
                         as.character (features [mean_sd_columns, 2]))

######################################################################################
# 5. Creating an independent tidy data set with the average of each variable for     #
# each activity and each subject.                                                    #

means_data_set <- group_by (desired_data, Subject, Activity) %>% summarize_all (mean)

write.table (means_data_set, file = "Tidy_data.txt", row.names = FALSE)