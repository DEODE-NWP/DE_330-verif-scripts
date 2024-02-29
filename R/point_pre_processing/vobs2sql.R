#!/usr/bin/env Rscript
# Read vobs data and save it in sqlite format
#renv::load(getwd())

library(harp)
library(argparse)
library(here)

### 
source(Sys.getenv('CONFIG_R'))


###
parser <- ArgumentParser()

parser$add_argument("-start_date", type="character",
    default=NULL, 
    help="First date to process [default %(default)s]",
    metavar="Date in format YYYYMMDDHH")

parser$add_argument("-end_date", type="character",
    default=NULL,
    help="Final date to process [default %(default)s]",
    metavar="Date in format YYYYMMDDHH")

args <- parser$parse_args()	


###
CONFIG <- conf_get_config()

 
 
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)
vobs_path       <- CONFIG$pre$vobs_path
obs_path        <- CONFIG$verif$obs_path
by_step         <- CONFIG$verif$by_step
by_vobs_step    <- CONFIG$pre$by_vobs_step
fcst_model      <- CONFIG$verif$fcst_model
lead_time       <- CONFIG$verif$lead_time
fclen           <- CONFIG$pre$fclen

# Function to add hours to a date string in YYYYMMDD, YYYYMMDDhh, or YYYYMMDDhhmm format
add_hours_to_date <- function(date_string, hours_to_add) {
  # Convert date_string to character if it's not already
  if (!is.character(date_string)) {
    date_string <- as.character(date_string)
  }
  
  # Define format based on the length of the input date string
  if (nchar(date_string) == 8) {
    format <- "%Y%m%d"
  } else if (nchar(date_string) == 10) {
    format <- "%Y%m%d%H"
  } else if (nchar(date_string) == 12) {
    format <- "%Y%m%d%H%M"
  } else {
    stop("Invalid date string format. Please provide a string in YYYYMMDD, YYYYMMDDhh, or YYYYMMDDhhmm format.")
  }
  
  # Convert date string to POSIXct object
  date <- as.POSIXct(date_string, format = format)
  
  # Convert hours_to_add to numeric if it's provided as a string
  if (is.character(hours_to_add)) {
    hours_to_add <- as.numeric(hours_to_add)
  }
  
  # Add hours
  date_with_hours_added <- date + hours_to_add * 3600  # 3600 seconds in an hour
  
  # Convert back to desired format
  final_date_string <- format(date_with_hours_added, format = format)
  
  return(final_date_string)
}

final_end_date <- add_hours_to_date(end_date, fclen)


cat("Collecting vobs data  from ",start_date," to ",final_end_date)
cat("vobs path es",vobs_path)

obs_data <- read_obs(
  dttm=seq_dates(start_date,final_end_date),
  file_path    = vobs_path,
  file_template = "vobs",
  output_format = "obstable",
  output_format_opts = obstable_opts(path=obs_path,template="obstable")
  )

