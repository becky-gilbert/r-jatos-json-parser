parseCattellAll_JD <- function(fileName, 
                            fileOutSuffixes = c("start",
                                                "participant_id",
                                                "welcome",
                                                "test1_practice_responses",
                                                "test1_practice_interactions",
                                                "test1_responses",
                                                "test1_interactions",
                                                "test2_practice_responses",
                                                "test2_practice_interactions",
                                                "test2_responses",
                                                "test2_interactions",
                                                "test3_practice_responses",
                                                "test3_practice_interactions",
                                                "test3_responses",
                                                "test3_interactions",
                                                "test4_practice_responses",
                                                "test4_practice_interactions",
                                                "test4_responses",
                                                "test4_interactions",
                                                "end_study")) {
  # inputs:
  # - fileName (string): file name to parse, without the '.txt' extension
  # - fileOutSuffixes (vector of strings): file name endings for the output. These
  #   must be given in the same order as the components in the task (see defaults). 
  #   All task components that contain Cattell trials (practice and main tasks) will
  #   result in two files: one containing the responses, and one containing the timing
  #   of interactions with response boxes.
  
  require('jsonlite')
  source('parseJSONdataCattell.R')
  source('parseJSONdata.R')
  
  # get the lines of JSON from the results file
  dataFileCon <- file(paste(fileName, ".txt", sep=""), open = "r")
  rawData <- readLines(dataFileCon, warn = FALSE)
  close(dataFileCon)
  
  # get ID for file naming
  idData <- rawData[2]
  # is it valid JSON?
  isStringValid <- validate(idData)
  if (!isStringValid) {
    cat("/nError: participant ID data (component 2) is not valid JSON")
    return()
  }
  idDataList <- fromJSON(idData, flatten = TRUE)
  idDataDF <- as.data.frame(idDataList)
  ppt_id_split <- strsplit(idDataDF$responses, "{\"Q0\":\"", fixed=TRUE)[[1]][2]
  ppt_id <- strsplit(ppt_id_split, "\"}", fixed=TRUE)[[1]]
  
  # 1st line: start study (browser/device check)
  parseJSONdata(rawData[1], numComponents=1, fileOutSuffixes=fileOutSuffixes[1], isJsonStr = TRUE, id = ppt_id)
  # 2nd line: participant ID
  parseJSONdata(rawData[2], numComponents=1, fileOutSuffixes=fileOutSuffixes[2], isJsonStr = TRUE, id = ppt_id)
  # 3rd line: welcome page
  parseJSONdata(rawData[3], numComponents=1, fileOutSuffixes=fileOutSuffixes[3], isJsonStr = TRUE, id = ppt_id)
  # 4th line: test 1 practice
  parseJSONdataCattell(rawData[4], fileOutSuffixes[4:5], isJsonStr = TRUE, id = ppt_id)
  # 5th line: test 1 
  parseJSONdataCattell(rawData[5], fileOutSuffixes[6:7], isJsonStr = TRUE, id = ppt_id)
  # 6th line: test 2 practice
  parseJSONdataCattell(rawData[6], fileOutSuffixes[8:9], isJsonStr = TRUE, id = ppt_id)
  # 7th line: test 2
  parseJSONdataCattell(rawData[7], fileOutSuffixes[10:11], isJsonStr = TRUE, id = ppt_id)
  # 8th line: test 3 practice
  parseJSONdataCattell(rawData[8], fileOutSuffixes[12:13], isJsonStr = TRUE, id = ppt_id)
  # 9th line: test 3
  parseJSONdataCattell(rawData[9], fileOutSuffixes[14:15], isJsonStr = TRUE, id = ppt_id)
  # 10th line: test 4 practice
  parseJSONdataCattell(rawData[10], fileOutSuffixes[16:17], isJsonStr = TRUE, id = ppt_id)
  # 11th line: test 4
  parseJSONdataCattell(rawData[11], fileOutSuffixes[18:19], isJsonStr = TRUE, id = ppt_id)
  # 12th line: end study
  parseJSONdata(rawData[12], numComponents=1, fileOutSuffixes=fileOutSuffixes[20], isJsonStr = TRUE, id = ppt_id)
  
}