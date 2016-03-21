# Final Project to tidy data.  This script does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.

# The data files should be in the same places relative to where they were when the 
# zip file was unzipped.  Do not move the files.  Run this script in the same directory
# where the data file was unzipped.

library(dplyr)
# First step, load the data
# 561 rows of feature strings like tBodyAcc-mean()-X and tBodyAcc-mean()-Y
features_table <- read.table("features.txt")
features <- as.vector(features_table[,2])

# list of the 6 activities like walking, sitting, etc.
activity_labels_table <- read.table("activity_labels.txt")
acivitiy_labels <- activity_labels_table[,2]

# 2,947 rows of the 561 "features" data listed above
x_test_table <- read.table("X_test.txt")

# 2,947 rows of the activity ID for the x_test_table data above
y_test_table <- read.table("y_test.txt")
colnames(y_test_table) <- "activity"

# 2,947 rows of the subject's (participant's) ID # for the feature data
subject_test_table <- read.table("subject_test.txt")
#colnames(subject_test_table) <- "subject_id"

# Load the training data
# 2,947 rows of the 561 "features" data listed above
x_train_table <- read.table("X_train.txt")

# 2,947 rows of the activity ID for the x_train_table data above
y_train_table <- read.table("y_train.txt")
colnames(y_train_table) <- "activity"

# 2,947 rows of the subject's (participant's) ID # for the feature data
subject_train_table <- read.table("subject_train.txt")
#colnames(subject_train_table) <- "subject_id"

# Clean up the column names
tf <- features;
tf <- gsub("[(][])]", "", tf)
tf <- sub("^t", "time", tf)
tf <- sub("^f", "frequency", tf)
tf <- sub("Mag", "Magnitude", tf)
tf <- sub("Acc", "Acceleration", tf)
tf <- sub("-min", "-minimum", tf)
tf <- sub("-maxInds", "-MaximumIndex", tf)
tf <- sub("-max", "-maximum", tf)
tf <- sub("-mad", "-medianAbsoluteDeviation", tf)
tf <- sub("-iqr", "-interquartile", tf)
tf <- sub("-sma", "-signalMagnitudeArea", tf)
tf <- sub("-arCoeff", "-autoregresionCoefficients", tf)
tf <- sub("-std", "-standardDeviation", tf)
tf <- sub("Freq", "Frequency", tf)

colnames(x_test_table) <- tf
colnames(x_train_table) <- tf

# mutate features to add columns for subject_id (subject_test_table) and activity.  Need to substitute the 
# activity ID with the activity name.
y_test_table <- mutate(y_test_table, activity = activity_labels[as.numeric(activity)])
x_test_table <- cbind(y_test_table, x_test_table)

y_train_table <- mutate(y_train_table, activity = activity_labels[as.numeric(activity)])
x_train_table <- cbind(y_train_table, x_train_table)

# Need to add the subject ID variable to the test and train data frames.
#x_test_table <- cbind(subject_test_table, x_test_table)
#x_train_table <- cbind(subject_train_table, x_train_table)
x_test_table["subject_id"] <- as.vector(subject_test_table)
x_train_table["subject_id"] <- as.vector(subject_train_table)

# Merge the 2 tables, test & train
combined <- rbind( x_test_table, x_train_table)

# Write out the combined tidy data set
write.table(combined, file = "combined_tidy.txt", append = FALSE, sep = " ", eol = "\n", row.names = FALSE)

# Select the mean and std columns from the combined table
df_out <- combined %>%
    select(matches('subject|activity|-mean|-standardDeviation')) %>%
    group_by(subject_id, activity) %>%
    summarise_each(funs(mean))
    
write.table(df_out, file = "subject_activity_mean.txt", append = FALSE, sep = " ", eol = "\n", row.names = FALSE)

cat("View df_out to examine the output data.frame or look at the output file subject_activity_mean.txt\n")
cat("The combined test and training data set is stored in combined_tidy.txt\n")
