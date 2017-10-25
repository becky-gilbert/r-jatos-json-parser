parseJSONdataCattell <- function(fileName, fileOutSuffixes=c("responses","interaction"), isJsonStr=FALSE, id=NULL) {
  
  # Inputs:
  # fileName: (string) either
  #  1. valid JSON string containing data from a single Cattell component (line of JSON)
  #  2. path to a txt file containing data from a single Cattell component (line of JSON)
  # fileOutSuffixes: (array of strings) suffix(es) to be added to the file name to form the new file names for the 
  #  resulting CSV files. Suffixes must be given in the order: responses, interaction
  # isJsonStr: (boolean) is 'fileName' a JSON string rather than a file name? (default is FALSE)
  # id: (string) if isJsonStr = TRUE, then what should the identifier be for the output files? (no default value)
  
  # Output:
  #  A set of files containing the data in csv format, saved in the current directory, named 
  #  as the original file name + the suffix, e.g. "my_data.txt" -> my_data_responses.csv" etc.
  # 1 file per participant for:
  # - responses
  # - response box interactions
  
  # Becky Gilbert, Oct 2017
  
  require(jsonlite)

  expectedLines <- 1
  expectedSuffixes <- 2
  metaDataColumns <- c("test_number","test_segment","trial_type","trial_index","time_elapsed","internal_node_id","rt")
  
  if (!(isJsonStr)) {
    dataFileCon <- file(paste(fileName, ".txt", sep=""), open = "r")
    rawData <- readLines(dataFileCon, warn = FALSE)
    close(dataFileCon)
  } else {
    if (is.null(id) || (!(is.character(id)))) {
      cat("\nWarning: when isJsonStr is TRUE, a valid string must be given for the 'id' argument.")
      return()
    }
    rawData <- fileName
    fileName <- id
  }
  
  # expecting one line of JSON
  if (length(rawData) != 1) {
    cat(paste("\nWarning: file", fileName, "does not contain the expected number of lines.\n"))
    cat(paste("\nExpected",expectedLines,"line(s), found",length(rawData),"JSON lines.\n"))
    return() 
  }
  
  # expecting one file out suffix for each component
  if (expectedSuffixes != length(fileOutSuffixes)) {
    cat(paste("\nWarning: file", fileName, ", unexpected number of file out suffixes.\n"))
    cat(paste("\nExpected = ",expectedSuffixes,", fileOutSuffixes = ",length(fileOutSuffixes),".\n"))
    return() 
  }
  
  # is it valid JSON?
  isStringValid <- validate(rawData)
  
  if (!isStringValid) {
    # if not valid JSON
    cat(paste("\nWarning: file,", fileName ,"is not valid JSON.\n"))
    return()
  }
  
  # convert JSON string to data frame list
  resultsList <- fromJSON(rawData)
  # get the non-nested JSON results from the data frame
  resultsDF <- as.data.frame(resultsList)
  resultsDFkeep <- resultsDF[,(names(resultsDF) %in% metaDataColumns)]
  
  # parse viewing history
  resultsViewHist <- resultsList[["view_history"]]
  parsedViewHist <- fromJSON(resultsViewHist)
  viewHistMerged <- merge(parsedViewHist, resultsDFkeep)
  
  # parse responses
  resultsResponses <- resultsList[["responses"]]
  parsedResponses <- fromJSON(resultsResponses)
  responsesMerged <- cbind(viewHistMerged,parsedResponses)
  write.csv(responsesMerged, paste(fileName, "_", fileOutSuffixes[1], ".csv", sep=""), row.names = FALSE)
  
  # parse response box interactions
  resultsInteractions <- resultsList[["resp_box_interaction"]]
  interactions <- fromJSON(resultsInteractions)
  interactionsMerged <- merge(interactions, resultsDFkeep)
  write.csv(interactionsMerged, paste(fileName, "_", fileOutSuffixes[2], ".csv", sep=""), row.names = FALSE)
}
