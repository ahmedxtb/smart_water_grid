### Analyze Water Quality Measurements

# Central question: What are the correlations between changes in water quality measued by Eventlab sensors (vitnor)
# and other real-time measurements, statuses and alarm values?
# See "./doc/Vitens Data Challenge 07122015.pdf" file for details about data



## PREPARE

# Libraries
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(caret)

# Print session info
sessionInfo()

# Variables
dir.data <- "~/git/smart_water_grid/data"
dir.results <- "~/git/smart_water_grid/data"
#url.data <- "https://db.tt/wf2nir4S"

# Workdir
setwd(dir.data)



## FUNCTIONS

readfiles <- function(fileslist) {
    listofdataframes <- lapply(fileslist, function(file) read.csv(file, 
                                                                  comment.char = "#", 
                                                                  col.names = c("Time", file), 
                                                                  na.strings = c("", "Calc Failed")))

    merged <- Reduce(function(x, y) merge(x, y, all=TRUE), listofdataframes)
    
    # Convert to POSIXct/POSIXt time format
    merged$Time <- ymd_hms(merged$Time)
    
    return(merged)
}
# test
#head(readfiles(list.files()[1:3]))


selecttimerange <- function(dataframe, begintime = -Inf, endtime = Inf) {
    subset(dataframe, Time >= begintime & Time <= endtime)
}
# test
#selecttimerange(head(readfiles(list.files()[1:3])), ymd("2014-01-01"), ymd("2015-03-01"))



## EXPLORE DATA AND DATA QUALITY

# There are 885 files in dir.data, all of which are csv files
# Example: Dump_PE-FR-Deelbalansgebied-Westeinde-levering.csv
# We are interested in vitnor data as response variables
# File name seems to contain variable info that is being measured
# There are 186 files whose filename contains vitnor

files.select <- list.files(path=dir.data, pattern="vitnor.*csv$")[1:9]
data.select <- readfiles(files.select)
summary(data.select)

# Measurements seem to be done in time range Januari 2014 to December 2015 (only checked sample of files)
# We observe that some variables in this subset contain many missing values (after being merged with other variables)
# A few exploratory plots make clear that for some variables data is only available in specific time ranges

ggplot(data.select, aes_string(x="Time", y=names(data.select)[2])) + geom_point()
ggplot(data.select, aes_string(x="Time", y=names(data.select)[10])) + geom_point()


# We first want to find a high quality subset that is suitable for analysis (of correlations, etc)
# Questions:
# 1. Which of the vitnor variable(s) should we choose as response variables?
# 2. Which of the non-vitnor variables should we choose as interesting predictor variables?
# 3. Which time range should we choose in which both predictors and response variables are very well available?


# 1. Investigating suitable vitnor variables

# Since availability of vitnor1, vitnor2 and vitnor3 variables seems to be related (when one is available
# the others are often too), we only look at the vitnor1 variables for now (62 files).
files.vitnor.select <- list.files(path=dir.data, pattern="vitnor1.*csv$")[1:20]
data.vitnor.select <- readfiles(files.vitnor.select)
hist(data.vitnor.select[!is.na(data.vitnor.select[,15]), "Time"], breaks = "weeks")


