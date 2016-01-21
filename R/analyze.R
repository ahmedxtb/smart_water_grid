### Analyze Water Quality Measurements


## Central question:
# Welke correlaties zijn er tussen waterkwaliteitsschommelingen die worden geregistreerd door
# Eventlab sensoren en de andere real-time meet-, status- en alarmwaarden?

## Background:
# Naast veel real-time debiet (flow) en drukmetingen, experimenteren we ook met diverse
# waterkwaliteitssensors. Een daarvan is de generieke waterkwaliteitsmeting van de Eventlab sensor.



## PREPARE

# Print session info
sessionInfo()

# Libraries
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(caret)

# Variables
dir.data <- "~/git/smart_water_grid/data"
dir.results <- "~/git/smart_water_grid/data"
#url.data <- "https://db.tt/wf2nir4S"

# Workdir
setwd(dir.data)



## EXPLORE DATA AND DATA QUALITY

list.files(path=dir.data)
list.files(path=dir.data, pattern=".csv$")

# 885 files in dir.data, all of them are csv files
# Example: Dump_PE-FR-Deelbalansgebied-Westeinde-levering.csv
# See "./doc/Vitens Data Challenge 07122015.pdf" file for details about measured data
# We are interested in vitnor data (response variables)

files.vitnor <- list.files(path=dir.data, pattern="vitnor")
files.vitnor
# 186 files whose filename contains vitnor

list.files(path=dir.data, pattern="TM")
# File name seems to contain variable info that is being measured


# Example file (first file in files.vitnor)
file.select <- files.vitnor[1]
file.select

# Read data (ignore comment lines)
df.vitnor.1 <- read.csv(file.select, comment.char = "#")
head(df.vitnor.1)

# Convert Timestamp
df.vitnor.1$Timestamp <- ymd_hms(df.vitnor.1$Timestamp)
# Rename Value to filename
varname <- make.names(file.select)
names(df.vitnor.1) <- c("Time", varname)

# Check for NA values
sum(is.na(df.vitnor.1))

# Exploratory Plot
plot1 <- ggplot(df.vitnor.1, aes_string(x="Time", y=varname)) + geom_point()
plot1

