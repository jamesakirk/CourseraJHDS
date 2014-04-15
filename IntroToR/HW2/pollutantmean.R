pollutantmean <- function(directory, pollutant, id = 1:332) {
	        ## 'directory' is a character vector of length 1 indicating
	        ## the location of the CSV files

	        ## 'pollutant' is a character vector of length 1 indicating
	        ## the name of the pollutant for which we will calculate the
	        ## mean; either "sulfate" or "nitrate".

	        ## 'id' is an integer vector indicating the monitor ID numbers
	        ## to be used

	        ## Return the mean of the pollutant across all monitors list
	        ## in the 'id' vector (ignoring NA values)
	#first, inilize an empty vector 
	allData <- vector()
	#Then, loop over all of the files in "id"
	for (i in id){
		# convert the id to a string
		# if the id is small, we will have to add leading zeros
		# 	we could do an ifthen, be we can use the function:
		# 	formatC(a, width=3, flag="0")
		currentFileID <- c(formatC(i, width=3, flag="0"))
		# create a filename to open
		currentFile <- paste(c("./", directory,"/", currentFileID, ".csv"), collapse="")
		# open the csv
		currentData <- read.csv(currentFile)
		# subsection the desired polutant
		dataVect <- currentData[, pollutant]
		# clean up the data by removing NAs
		dataVect <- na.omit(dataVect)
		# merge the polutant data from the current file with allData
		allData <- c(allData, dataVect)
	}
	#When we have gone through all of the files, return the mean
	mean(allData)
}
