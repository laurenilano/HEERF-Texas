setwd("~/Dropbox/EdTrust/HEERF")

library(tidyverse)
# Adapted from Ben Skinner's Code
# https://github.com/btskinner/downloadipeds/blob/main/downloadipeds.R

## PURPOSE ---------------------------------------------------------------------
##
## Use this script to batch download IPEDS files. Only those files listed
## in `ipeds_file_list.txt` will be downloaded. The default behavior is to
## download each of the following files into their own subdirectories:
##
## (1) Data file
## (2) Dictionary file
##
## You can also choose to download other data versions and/or program files:
##
## (1) Data file (STATA version)
## (2) STATA program file (default if you ask for DTA version data)
## (3) SPSS program file
## (4) SAS program file
##
## The default behavior is download ALL OF IPEDS. If you don't want everything,
## modify `ipeds_file_list.txt` to only include those files that you want.
## Simply erase those you don't want, keeping one file name per row, or
## comment them out using a hash symbol (#).
##
## You also have the option of whether you wish to overwrite existing files.
## If you do, change the -overwrite- option to TRUE. The default behavior is
## to only download files listed in `ipeds_file_list.txt` that have not already
## been downloaded.
## -----------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## CHOOSE WHAT YOU WANT (TRUE == Yes, FALSE == No)
## -----------------------------------------------------------------------------

## default
primary_data = TRUE
dictionary = TRUE

## STATA version
## (NB: downloading Stata version of data will also get Stata program files)
stata_data = FALSE

## other program files
prog_spss = FALSE
prog_sas  = FALSE

## overwrite already downloaded files
overwrite = FALSE

## -----------------------------------------------------------------------------
## CHOOSE OUTPUT DIRECTORY (DEFAULT == '.', which is current directory)
## -----------------------------------------------------------------------------

out_dir = './Raw Data/IPEDS'

## =============================================================================
## FUNCTIONS
## =============================================================================

## message
mess <- function(to_screen) {
  message(rep('-',80))
  message(to_screen)
  message(rep('-',80))
}

## create subdirectories
make_dir <- function(opt, dir_name) {
  if (opt & dir.exists(dir_name)) {
    message(paste0('Already have directory: ', dir_name))
  } else if (opt & !dir.exists(dir_name)) {
    message(paste0('Creating directory: ', dir_name))
    dir.create(dir_name)
  }
}

## download file
get_file <- function(opt, dir_name, url, file, suffix, overwrite) {
  if (opt) {
    dest <- file.path(dir_name, paste0(file, suffix))
    if (file.exists(dest) & !overwrite) {
      message(paste0('Already have file: ', dest))
      return(0)
    } else {
      download.file(paste0(url, file, suffix), dest)
      Sys.sleep(1)
      return(1)
    }
  }
}

## countdown
countdown <- function(pause, text) {
  cat('\n')
  for (i in pause:0) {
    cat('\r', text, i)
    Sys.sleep(1)
    if (i == 0) { cat('\n\n') }
  }
}

## =============================================================================
## RUN
## =============================================================================

## read in files; remove blank lines & lines starting with #
ipeds <- readLines('./Code/ipeds_file_list.txt')
ipeds <- ipeds[ipeds != '' & !grepl('^#', ipeds)]

## data url
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'

## init potential file paths
data_dir <- file.path(out_dir, 'data')
dictionary_dir <- file.path(out_dir, 'dictionary')


## create folders if they don't exist
mess('Creating directories for downloaded files')
make_dir(TRUE, out_dir)
make_dir(primary_data, data_dir)
#make_dir(stata_data, stata_data_dir)
make_dir(dictionary, dictionary_dir)
#make_dir(stata_data, stata_prog_dir)
#make_dir(prog_spss, spss_prog_dir)
#make_dir(prog_sas, sas_prog_dir)

## get timer (pause == max(# of options, 3))
opts <- c(primary_data, stata_data, dictionary, prog_spss, prog_sas)

## loop through files
for(i in 1:length(ipeds)) {
  
  ow <- overwrite
  f <- ipeds[i]
  mess(paste0('Now downloading: ', f))
  
  ## data
  d1 <- get_file(primary_data, data_dir, url, f, '.zip', ow)
  
  ## dictionary
  d2 <- get_file(dictionary, dictionary_dir, url, f, '_Dict.zip', ow)
  
  ## Stata data and program (optional)
  #d3 <- get_file(stata_data, stata_data_dir, url, f, '_Data_Stata.zip', ow)
  #d4 <- get_file(stata_data, stata_prog_dir, url, f, '_Stata.zip', ow)
  
  ## SPSS program (optional)
  #d5 <- get_file(prog_spss, spss_prog_dir, url, f, '_SPS.zip', ow)
  
  ## SAS program (optional)
  #d6 <- get_file(prog_sas, sas_prog_dir, url, f, '_SAS.zip', ow)
  
  ## get number of download requests
  #dls <- sum(d1, d2, d3, d4, d5, d6)
  dls <- sum(d1, d2)
  
  if (dls > 0) {
    ## set pause based on number of download requests
    pause <- max(dls, 3)
    ## pause and countdown
    countdown(pause, 'Give IPEDS a little break ...')
  } else {
    message('No downloads necessary; moving to next file')
  }
}

mess('Finished!')

## =============================================================================
## UNZIP
## =============================================================================

read.zip <- function(zipfile) {
  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(zipfile, exdir=zipdir)
  # Get the files into the dir
  files <- list.files(zipdir, recursive = TRUE)
  # Chose rv file if more than two
  if(length(files)>1) {
    file <- grep("*_rv.csv", files, value = TRUE)
  } else {
    file <- files[1]
  }
  # Get the full name of the file
  file <- paste(zipdir, file, sep="/")
  # Read the file
  read.csv(file, header=TRUE)
}

# Read Unzipped Data
Retention_2017 <- read.zip("Raw Data/IPEDS/data/EF2017D.zip") %>% mutate(Year = 2017)
Retention_2018 <- read.zip("Raw Data/IPEDS/data/EF2018D.zip") %>% mutate(Year = 2018)
Retention_2019 <- read.zip("Raw Data/IPEDS/data/EF2019D.zip") %>% mutate(Year = 2019)
Retention_2020 <- read.zip("Raw Data/IPEDS/data/EF2020D.zip") %>% mutate(Year = 2020)
Retention_2021 <- read.zip("Raw Data/IPEDS/data/EF2021D.zip") %>% mutate(Year = 2021)
IC_2020 <- read.zip("Raw Data/IPEDS/data/HD2020.zip")


# Combine all retention dataframes into one DF
Retention <- rbind(Retention_2017, Retention_2018, Retention_2019, Retention_2020, Retention_2021)

# Write retention dataframe to CSV in /Raw Data
write.csv(Retention, "Raw Data/IPEDS_Retention_2017_2021.csv")


## END
################################################################################