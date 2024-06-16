#!/usr/bin/env Rscript

# Basic script to run point verification and generate plots

library(harp)
library(purrr)
library(here)
library(argparse)
library(dplyr) # count, etc.
library(stringr) # str_to_title
library(forcats) # fcst_inorder
library(RColorBrewer) # Some colourmaps (some of which are colourblind friendly)
library(gridExtra) # For grid arrange
library(grid) # For grid arrange
library(pracma) # For logseq
#library(boomer)
library(mapproj)

###
source(Sys.getenv('CONFIG_R'))
source(here("R/visualization/fn_plot_point_verif.R"))
source(here("R/visualization/fn_plot_aux_scores.R"))
source(here("R/visualization/fn_plot_helpers.R"))

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
params <- CONFIG$params_details

###
start_date <- ifelse(is.null(args$start_date),CONFIG$shared$start_date,args$start_date)
end_date   <- ifelse(is.null(args$end_date),CONFIG$shared$end_date,args$end_date)
by_step    <- CONFIG$verif$by_step  #Read from config file
fcst_model <- CONFIG$verif$fcst_model
lead_time_str <- CONFIG$verif$lead_time
lead_time  <- eval(parse(text = lead_time_str))
fcst_type  <- CONFIG$verif$fcst_type
lags       <- "0s" #only for ensembles. Leaving here for the moment
fcst_path  <- CONFIG$verif$fcst_path
obs_path   <- CONFIG$verif$obs_path
verif_path <- CONFIG$verif$verif_path
grps       <- CONFIG$verif$grps
plot_output <- CONFIG$post$plot_output #  Load in png archive directory

# Verif results by lead_time for each fcst_cycle (should be default choice?)
grps <- list(c("lead_time","fcst_cycle"),"lead_time")

# Useful functions
# This does some renaming of groups (those with NA or "00;12", etc.)
# NB: May not be neccessary for updated version of harp
fn_verif_rename <- function(df){
  for (ii in seq(1,length(df),1)){
    for (var_rename in c("fcst_cycle","station_group","valid_hour")){
      if (var_rename %in% names(df[[ii]])){
        v_avail <- unique(df[[ii]][[var_rename]])
        v_rename <- v_avail[grep(";",v_avail)]
        if (length(v_rename)>0){
          df[[ii]] <- mutate(df[[ii]],"{var_rename}":=msub(get(var_rename),{{v_rename}},"All"))
        }
        df[[ii]][is.na(df[[ii]][[var_rename]]),var_rename] <- "All" # Rename any NAs introduced by NULL grouping
      }
    }
  }
  return(df)
}

# Add lat/lon values to verif file for SIDs
fn_sid_latlon <- function(df,fc){
  sid_toget <- df[[1]][["SID"]]; lat_sids <- NULL; lon_sids <- NULL; fc_tmp <- fc[[1]] 
  for (sid_i in sid_toget){
    lat_v <- unique(fc_tmp$lat[fc_tmp$SID == sid_i]); lon_v <- unique(fc_tmp$lon[fc_tmp$SID == sid_i]) 
    if ((length(lat_v)>1) || (length(lon_v)>1)){
      print(paste0("Warning: Why multiple lat/lons for SID=",sid_i,"? Will use workaround here. For reference, values are:"))
      print(paste0("Lat: ",paste0(lat_v,collapse = ",")," and Lon:",paste0(lon_v,collapse = ",")))
      lat_v = lat_v[1]; lon_v = lon_v[1]
    }
    lat_sids <- c(lat_sids,lat_v); lon_sids <- c(lon_sids,lon_v)
  }
  df[[1]]$lat <- lat_sids; df[[1]]$lon <- lon_sids
  return(df)
}
# Number of days considered
num_days <- as.numeric(difftime(str_datetime_to_datetime(end_date),str_datetime_to_datetime(start_date),units="days"))

# Some warnings in output
# Warning from recycling prolly comes from
# argument file_template. This does not change
# Default:     file_template = "fctable",

#Andrew's verification function below
# Function that runs the verification
run_verif <- function(prm_info, prm_name) {
  cat("Verifying ",prm_name,"\n")
  if (!is.null(prm_info$vc)) {
    vertical_coordinate <- prm_info$vc
  } else {
    vertical_coordinate <- NA_character_
  }
  
  # Read the forecast
  fcst <- read_point_forecast(
    dttm=seq_dttm(start_date,end_date,by_step),
    fcst_model    = fcst_model,
    fcst_type     = fcst_type,
    parameter     = prm_name,
    lead_time     = lead_time,
    lags          = lags,
    file_path     = fcst_path,
    get_lat_and_lon = TRUE, # Useful when lat/lon missing from vobs files
    vertical_coordinate = vertical_coordinate
  )
  
  # Find the common cases - for upper air parmeters we need to ensure 
  # that the level column  is included in the check
  fcst <- switch(
    vertical_coordinate,
    "pressure" = common_cases(fcst, p),
    "height"   = common_cases(fcst, z),
    common_cases(fcst)
  )
  # optional rescaling of forecasts using the scale_fcst part of the
  # params list. We use do.call to call the scale_point_forecast 
  # function with a named list containing the arguments. 
  if (!is.null(prm_info$scale_fcst)) {
    fcst <- do.call(
      scale_param,list(fcst, prm_info$scale_obs$scaling, prm_info$scale_obs$new_units, prm_info$scale_obs$mult)
    )
  }
  
  # Read the observations getting the dates and stations from 
  # the forecast
  obs <- read_point_obs(
    dttm=unique_valid_dttm(fcst),
    parameter  = prm_name,
    obs_path   = obs_path,
    stations   = unique_stations(fcst),
    min_allowed = prm_info$obsmin_val, # If obsmin/max not set, reverts to default
    max_allowed = prm_info$obsmax_val,
    vertical_coordinate = vertical_coordinate
  )
  
  # optional rescaling of observations using the scale_obs part of the
  # params list. We use do.call to call the scale_point_forecast 
  # function with a named list containing the arguments.
  if (!is.null(prm_info$scale_obs)) {
    obs <- do.call(
      scale_param, list(obs, prm_info$scale_obs$scaling, prm_info$scale_obs$new_units, prm_info$scale_obs$mult, col = {{prm_name}})
    )
  }
  
  # Join observations to the forecast
  fcst <- join_to_fcst(fcst, obs, force=TRUE)
  # Check for errors removing obs that are more than a certain number 
  # of standard deviations from the forecast. You could add a number 
  # of standard deviations to use in the params list
  fcst <- check_obs_against_fcst(fcst, prm_name)
  
  # Make sure that grps is a list so that it adds on the vertical 
  # coordinate group correctly
  if (!is.list(grps)) {
    grps <- list(grps)
  }
  
  grps <- switch(
    vertical_coordinate,
    "pressure" = purrr::map(grps, ~c(.x, "p")),
    "height"   = purrr::map(grps, ~c(.x, "z")),
    grps
  )

  # add valid_hour to fcst
  #fcst <- mutate_list(fcst,valid_hour=substr(YMDh(valid_dttm),9,10))
  fcst <- expand_date(fcst,valid_dttm) # Add in valid_year, valid_month, valid_day, valid_hour
  #fcst <- rig(expand_date)(fcst,valid_dttm) # Add in valid_year, valid_month, valid_day, valid_hour  
  fcst <- mutate_list(fcst,valid_hour = sprintf("%02d",valid_hour)) # Convert to character and pad
  # Remove stations that only occur very infrequently (for surface variables only)
  if (is.na(vertical_coordinate)){
    min_num_stations <- num_days # A first guess 
    station_count    <- fcst[[1]] %>% count(SID)
    sids_filter      <- station_count$SID[station_count$n > min_num_stations]
    fcst             <- filter_list(fcst,SID %in% sids_filter)
  }
    
  # Compute auxiliary scores and generate plots (surface only)
  if (is.na(vertical_coordinate)){
    fn_plot_aux_scores(fcst,plot_output)
  }
  
  # Do the verification
  if (fcst_type == "eps"){
    verif_fn <- "ens_verify"
  } else if (fcst_type == "det") {
    verif_fn <- "det_verify"
  }
    
  # Handle UA pressure verification (just revert to the standard grouping)
  if (!is.na(vertical_coordinate)){
    grps_c <- lapply(grps,function(x) x[x != "fcst_cycle"]) # Delete entries for fcst_cycle and station group
    grps_c <- lapply(grps_c,function(x) x[x != "station_group"])
    grps_c <- grps_c[lapply(grps_c,length)>0] # Remove empty entries
    grps_c <- unique(grps_c)
  } else {
    grps_c <- grps
  }
    
  verif <- get(verif_fn)(
    fcst, {{prm_name}}, thresholds = prm_info$thresholds, groupings = grps_c
  )
  verif <- fn_verif_rename(verif)
  verif_toplot <- verif # Used for passing to plotting script (as it may be modified below)
  verif_toplot[[2]][["lead_time"]] <- as.character(verif_toplot[[2]][["lead_time"]]) # For plotting purposes

  
  # Do some additional verif depending on UA parameter
  if (!is.na(vertical_coordinate)){
    # Group by valid_hour for profiles (threshold scores not required)
    grps_vh  <- lapply(grps_c,function(x) gsub("lead_time","valid_hour",x))
    verif_vh <-  get(verif_fn)(
      fcst, {{prm_name}}, thresholds = NULL, groupings = grps_vh
    )
    verif_vh <- fn_verif_rename(verif_vh)
      
  } else {
    # Compute scores as a function of valid_dttm (should take the same format as default lead_time groups)
    # Assuming threshold scores are not required
    grps_vd  <- lapply(grps_c,function(x) gsub("lead_time","valid_dttm",x))
    verif_vd <- get(verif_fn)(
      fcst, {{prm_name}}, thresholds = NULL, groupings = grps_vd
    )
    verif_vd <- fn_verif_rename(verif_vd)
   
    # Compute scores for each station for map purposes
    grps_sid  <- lapply(grps_c,function(x) gsub("lead_time","SID",x))
    # Replace fcst_cycle by valid_hour
    if ("fcst_cycle" %in% grps_sid[[1]]){
      grps_sid  <- lapply(grps_sid,function(x) gsub("fcst_cycle","valid_hour",x))
    } else {
      grps_sid <- list(c("SID","valid_hour"),"SID")
      print("Warning: This test script assumes fcst_cycle is one of the grouping variables!")
      print("Now using the following for grps_sid:")
      print(grps_sid)
    }
    verif_sid <- get(verif_fn)(
      fcst, {{prm_name}}, thresholds = NULL, groupings = grps_sid
    )
    verif_sid <- fn_verif_rename(verif_sid)
    # Need to add lat/lon to the SIDs
    verif_sid <- fn_sid_latlon(verif_sid,fcst)
    
    # Flag to compute the standard threshold scores over all lead_times and add
    alllt_thresholds <- TRUE
    if (alllt_thresholds){
      #  Compute threshold scores over all lead_times (as done in Monitor) (retain default group structure)
      grps_nolt <- lapply(grps_c,function(x) x[x != "lead_time"])
      grps_nolt <- grps_nolt[lapply(grps_nolt,length)>0] 
      # Note: This will remove character(0) entries but will miss the "All" option for fcst_cycle etc.
      # Hence we add a NULL below (NB: this may not be required with updated version of harp)
      grps_nolt <- append(grps_nolt,list(NULL)) 
      verif_alllt <- get(verif_fn)(
        fcst, {{prm_name}}, thresholds = prm_info$thresholds, groupings = grps_nolt
      )
      verif_alllt <- fn_verif_rename(verif_alllt)
      # Add in indication that we use all lead_times (important for plotting script and for binding below)
      verif_alllt <- mutate_list(verif_alllt,lead_time = "All") 
      
      print("Adding threshold scores over all lead_times")
      # Bind rows will match the column names
      verif_toplot[[2]] <- bind_rows(verif_toplot[[2]],verif_alllt[[2]])
    }
  }
  
  #  Generate point verif plots based on different groups
  fn_plot_point_verif(verif_toplot,plot_output)
  if (!is.na(vertical_coordinate)){
    fn_plot_point_verif(verif_vh,plot_output)
  } else {
    fn_plot_point_verif(verif_vd,plot_output)
    fn_plot_point_verif(verif_sid,plot_output,table_SIDS=FALSE)
  }
  
  # Save the scores
  save_point_verif(verif, verif_path = verif_path)
  
  # Return the data to the calling environment (normally global)
  verif
  
}

# Use possibly from the purrr package to allow the script to continue
# if it fails for a parameter - it returns NULL if it fails. See
# ?safely and ?quietly if you want to retain the errors.
#possibly(run_verif, otherwise = NULL)
possible_run_verif <- possibly(run_verif, otherwise = "Error found", quiet=FALSE)
#possible_run_verif <- run_verif
#print(possible_run_verif)

# Use imap from the purrr package to map each element of the params list
# to the possible_run_verif function. imap passes the element of the list
# as the first argument and the name of the element as the second.
verif <- imap(params, possible_run_verif)
#verif <- imap(params, run_verif)
# This will be addd in the visualization part
# You can open the results in a shiny app using 
# shiny_plot_point_verif(verif_path)

