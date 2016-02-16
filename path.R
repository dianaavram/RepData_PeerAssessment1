## Set the working directory and create other necessary folders
##  =============================================================
path.dir <- getwd()

if (!is.null(path.dir)){
    path.dir <- paste(path.dir, "RepData_PeerAssessment1", sep = "/");
    setwd(path.dir)	
}


## Check if data directory exists 
## - for reading and writing data files
## --------------------------------------------------------------
if (!file.exists("data")) {
    dir.create("data")
}
