complete <- function(directory, id = 1:332) {
	        ## 'directory' is a character vector of length 1 indicating
	        ## the location of the CSV files

	        ## 'id' is an integer vector indicating the monitor ID numbers
	        ## to be used
	        
	        ## Return a data frame of the form:
	        ## id nobs
	        ## 1  117
	        ## 2  1041
	        ## ...
	        ## where 'id' is the monitor ID number and 'nobs' is the
	        ## number of complete cases

	#preallocate the vectors for the ids and nobs,
	#we will combine them into a data fram later...
	idVector <- integer(length(id))
	nobsVector <- integer(length(id))
	#Then, loop over all of the files in "id"in
	for (i in seq(1,length(id))){
		currentID <- id[i]
		# convert the id to a string
		# if the id is small, we will have to add leading zeros
		# 	we could do an ifthen, be we can use the function:
		# 	formatC(a, width=3, flag="0")
		currentFileID <- c(formatC(currentID, width=3, flag="0"))
		# create a filename to open
		currentFile <- paste(c("./", directory,"/", currentFileID, ".csv"), collapse="")
		# open the csv
		currentData <- read.csv(currentFile)
		# now, subsection just the complete cases
		completeData <- currentData[complete.cases(currentData),]	
		# count the number of complete observations
		currentNobs <- dim(completeData)[1]
		
		#copy the current ID and Nobs to the vector
		idVector[i] <- currentID
		nobsVector[i] <- currentNobs
	}
	returnFrame <- data.frame("id"=idVector, "nobs"=nobsVector)
	returnFrame
}

