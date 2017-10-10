## examples for using the parseJSONdata function to convert the data in JATOS results files to CSV
#  Becky Gilbert, Oct 2017

# if necessary, install the jsonlite library: 
# in RStudio, go to Tools -> Install Packages..., then type jsonlite

# load the jsonlite library
library(jsonlite)

# load the parsing function (use full path to this file if it's not in the current working directory)
source(parseJSONdata.R)

## Example 1

# parse a file containing data from 1 JATOS component
# here we can use the default parameter values (numComponents = 1, output file suffix = "output")

# use the full file path if this file is not in the current working directory
fileToParse1 <- "results_20170904164415" 

parseJSONdata(fileToParse1)

## Example 2

# parse a file containing data from mulitple JATOS components
# now we need to specify the number of components it contains (numComponents) and output file suffixes (fileOutSuffixes)

fileToParse2 <- "results_20170811141734"
numComponents <- 8
fileOutSuffixes <- c("mobile","id","info_consent","audio_calibration","headphones_test",
                     "tone_test","shift_test","end")
parseJSONdata(fileToParse2, numComponents, fileOutSuffixes)


