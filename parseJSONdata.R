parseJSONdata <- function(fileName, numComponents=1, fileOutSuffixes=c("output")) {
  # Inputs:
  # fileName: (string) path to a single txt file containing the data downloaded from JATOS (JSON format)
  # numComponents: (num) number of JATOS components that the data corresponds to. This will be the same as the number of lines
  #  in the file to be parsed (1 line on JSON per JATOS component).
  #  If not specified, then the expected number of components (JSON lines) in the file is 1. 
  # fileOutSuffixes: (array of strings) suffix(es) to be added to the file name to form the new file name for the 
  #  resulting CSV file for each component. This should be an array with length equal to numComponents. Suffixes are assigned in 
  #  the same order as the components in the file.
  
  # Output:
  #  A file containing the JSON data in csv format, saved in the current directory, named 
  #  as the original file name + the suffix, e.g. "my_data.txt" -> my_data_output.csv"
  
  # Becky Gilbert, Oct 2017
  
  require(jsonlite)
  
  dataFileCon <- file(paste(fileName, ".txt", sep=""), open = "r")
  rawData <- readLines(dataFileCon, warn = FALSE)
  close(dataFileCon)
  
  # expecting one line of JSON for each component
  if (length(rawData) != numComponents) {
    cat(paste("\nWarning: file", fileName, "does not contain the expected number of components. Skipping this file.\n"))
    cat(paste("\nExpected",numComponents,"components, found",length(rawData),"JSON lines.\n"))
    return() 
  }
  
  # expecting one file out suffix for each component
  if (numComponents != length(fileOutSuffixes)) {
    cat(paste("\nWarning: file ", fileName, ", number of components specified not equal to number of file out suffixes.\n"))
    cat(paste("\nnumComponents = ",numComponents,", fileOutSuffixes = ",length(fileOutSuffixes),".\n"))
    return() 
  }
  
  # for each line (set of component results)
  for (i in 1:length(rawData)) {
    
    jsonLineIsArray <- FALSE
    
    currResults <- rawData[i]
    
    # is it valid JSON?
    isStringValid <- validate(currResults)
    
    if (!isStringValid) {
      
      # if not valid JSON, it may need brackets at the start/end
      # combine all elements into array to make it valid JSON
      currResultsArray <- paste("[",currResults,"]",sep="")
      isArrayValid <- validate(currResultsArray)
      
      if (!isArrayValid) {
        # can't get the string to parse into valid JSON
        cat(paste('\nWarning: file ',fileName,' is not valid JSON.',sep=""))
        cat(paste('\n',attr(isArrayValid,"err"),'\n',sep=""))
        return() 
      } else {
        jsonLineIsArray <- TRUE
        currResults <- currResultsArray
      }
    }
    
    # convert JSON string to data frame list
    currResultsList <- fromJSON(currResults, flatten = TRUE)
    
    # convert data frame list to data frame
    if (!jsonLineIsArray) {
      # if this line of JSON is not an array then convert it to a data frame
      currResultsCSV <- as.data.frame(currResultsList)
      # check to see if any of the columns are lists, and if so, convert them to character (otherwise write.csv will fail)
      currResultsClasses<-sapply(currResultsCSV,class)
      if (length(which(currResultsClasses=="list")) == 0) {
        # write the csv file
        currFileSuffix <- fileOutSuffixes[i]
        write.csv(currResultsCSV, paste(fileName, "_", currFileSuffix, ".csv", sep=""), row.names=FALSE)
      } else {
        for (k in 1:length(which(currResultsClasses=="list"))) {
          colIndex <- as.integer(which(currResultsClasses=="list")[k])
          currResultsCSV[,colIndex]<-vapply(currResultsCSV[,colIndex], paste, collapse=",", character(1L)) 
        }
        write.csv(currResultsCSV, paste(fileName, "_", currFileSuffix, ".csv", sep=""), row.names=FALSE)
      }
    } else {
      # if this line of JSON is an array, then need to parse each item in the array
      for (j in 1:length(currResultsList)) {
        # get the JSON from this index
        currResultsCSV <- as.data.frame(currResultsList[[j]])
        # check to see if any of the columns are lists, and if so, convert them to character (otherwise write.csv will fail)
        currResultsClasses<-sapply(currResultsCSV,class)
        if (length(which(currResultsClasses=="list")) == 0) {
          # write the csv file
          currFileSuffix <- fileOutSuffixes[i]
          write.csv(currResultsCSV, paste(fileName,"_",currFileSuffix,"_",j,".csv",sep=""),row.names=FALSE)
        } else {
          # convert each column of class 'list' to character
          for (k in 1:length(which(currResultsClasses=="list"))) {
            colIndex <- as.integer(which(currResultsClasses=="list")[k])
            currResultsCSV[,colIndex]<-vapply(currResultsCSV[,colIndex], paste, collapse=",", character(1L)) 
          }
          # write the csv file
          currFileSuffix <- fileOutSuffixes[i]
          write.csv(currResultsCSV, paste(fileName,"_",currFileSuffix,"_",j,".csv",sep=""),row.names=FALSE)
        }
      }
    }
  }
}
