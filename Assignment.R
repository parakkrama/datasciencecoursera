setwd('C:/Users/Para/Documents/Airpollution')

setwd("~/Desktop/Online Coursera/Coursera-R-Programming/week2/")
#getwd()
#list.files()

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # initialize a vector to hold the pollutant data
  mean_vector <- c()
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
    mean_vector <- c(mean_vector, na_removed)
  }
  result <- mean(mean_vector)
  return(round(result, 3)) 
}
setwd("~/Desktop/Online Coursera/Coursera-R-Programming/week2/")
#getwd()
#list.files()

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # initialize a vector to hold the pollutant data
  mean_vector <- c()
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
    mean_vector <- c(mean_vector, na_removed)
  }
  result <- mean(mean_vector)
  return(round(result, 3)) 
}





#Andrew Hawker
#Computing for Data Analysis (Winter 2013)
#Assignment #2 - Part 2
#https://class.coursera.org/compdata-002/assignment/view?assignment_id=2

#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file 
#and the second column is the number of complete cases.
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
  
  nobs <- function(id) {
    path <- file.path(directory, paste(sprintf("%03d", as.numeric(id)), ".csv", sep=""))
    return (sum(complete.cases(read.csv(path))))
  }
  return (data.frame(id=id, nobs=sapply(id, nobs)))
}


