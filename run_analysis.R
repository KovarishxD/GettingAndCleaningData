# Coursera: Getting and Cleaning Data
# Author: KovarishxD (Renato Angrisani)

library(reshape2)

# Print function for debugging purposes
print <- function(text) {
    cat(text, "\n")
}

# Loads and combines the data (train and test data).
loadData <- function(source) {
	data = read.csv(paste(source, "/X_", source, ".txt", sep=""), sep="", header=FALSE)
	data[,562] = read.csv(paste(source, "/Y_", source, ".txt", sep=""), sep="", header=FALSE)
	data[,563] = read.csv(paste(source, "/subject_", source, ".txt", sep=""), sep="", header=FALSE)

	return (data)
}

# Loads the feature vector descriptors and the activity labels.
loadParams <- function(source) {
	param = read.csv(paste(source, ".txt", sep=""), sep="", header=FALSE)

	return (param)
}

# Main function. Loads, cleans, and analyzes the data.
GetAndCleanData <- function() {

	print("Loading data...")
	trainData <- loadData("train") # Train data set.
	testData <- loadData("test")   # Test data set.

	activities <- loadParams("activity_labels") # Activity labels.
	features <- loadParams("features")		  # Feature vetor descriptors.

	# Renaming feature vector descriptor, so they get "more readable".
	features[,2] = gsub('-mean', 'Mean', features[,2])
	features[,2] = gsub('-std', 'Std', features[,2])
	features[,2] = gsub('[-()]', '', features[,2])
	
	print("Merging data...")
	# A single data set containing both train and test data sets.
	bindedData <- rbind(trainData, testData)
	
	print("Filtering data...")
	# The indexes of the measurements on the mean and 
	# standard deviation for each measurement. 
	indixes <- grep(".*Std.*|.*Mean.*", features[,2])

	features <- features[indixes,]			 # Selection of the features required.
	bindedData <- bindedData[, c(indixes, 562, 563)] # Selection of the data required.

	print("Preparing data...")
	# Adding the names of the "Activity" and "Subject" columns.
	colnames(bindedData) <- c(features$V2, "Activity", "Subject")

	# Replaces the IDs of the activities by their labels.
	labels <- activities$V2
	for (i in 1:6) {
  		bindedData$Activity <- gsub(i, labels[i], bindedData$Activity)
	}

	print("Melting data...")
	# Reshaping data, like done in the "Reshaping data" video.
	tidyData <- melt(bindedData, id.vars = c("Activity", "Subject"), measure.vars=colnames(bindedData[,1:86]))

	print("Calculating parameters...")
	# Calculating the mean, like done in the "Reshaping data" video.
	tidyData <- dcast(tidyData, Subject + Activity ~ variable, mean)

	print("Saving file...")
	# Saving the file.
	write.table(tidyData, "cleaned.txt", row.names = FALSE, quote = FALSE)
}
GetAndCleanData()
