#!/bin/bash 
#This part is only for running in SLURP at ecmwf
#SBATCH --output=harpverif2.out
#SBATCH --job-name=harpverif2
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=16000
#SBATCH --ntasks=1
#SBATCH --qos=nf
#SBATCH --time=23:30:00

set -x  
#### STEP 1: Path to your scritps, name of your verification (It will be the name of your output folder & will be shown in the shiny apps)

export VERIF_DIR=$PERM/deode_project/DE_330-verif-scripts/ #Put here location of your copy of DE_330's HARP point verification scripts
export CASE_STUDY=nl_eunice2022 # Define here a name for your verification exercise.
#export CASE_STUDY=AQCE_201701
cd $VERIF_DIR

######### NO NEED IN PRINCIPLE TO CHANGE THINGS IN THIS BLOCK: #######
# Function to replace placeholders with environment variables
replace_vars() {
  local input="$1"
  while IFS= read -r line; do
    eval echo "\"$line\""
  done < "$input"
}

module load R
cd $VERIF_DIR
MAIN_DIR=$(pwd)
CONFIG_DIR=$MAIN_DIR/config/   					# Directory for configuration files
RS_DIR=$MAIN_DIR/R/						# Directory where the R scripts are located
CONFIG_TEMPLATE=config_template.yml                     	# Modify this file with data from your runs to verify
CONFIG_YAML=${CONFIG_DIR}/${CONFIG_TEMPLATE}_${CASE_STUDY}	# This will be the name of the config file after preprocessing
CONFIG_R=$CONFIG_DIR/config_deode.R 				# Some specifics related to units, conversions, thresholds, etc.

# Process environment vars in input "template" YAML file and write to a pre-processed yaml file used by the verification scripts
replace_vars "${CONFIG_DIR}/${CONFIG_TEMPLATE}" > "${CONFIG_YAML}"
export CONFIG_YAML CONFIG_R

######## STEP 2: Customize below which parts of the verification you need to run
export RUN_POINT_VERF=no
export RUN_POINT_VERF_LOCAL=yes
export RUN_VOBS2SQL=no
export RUN_INTERPOL2SQL=no
export RUN_VFLD2SQL=no
export SCORECARDS=no
export SHOW_WEB_STATIC=no
export SHOW_WEB_DYNAMIC=no
export SHINY_PORT=3678 # Change this number if port is busy when launching web from ATOS' virtual desktop
export UPDATE_SHINYAPPS=no
######

if [ "$RUN_VOBS2SQL" == "yes" ]; then 
    echo "Running vobs2sql"
   $RS_DIR/point_pre_processing/vobs2sql.R  
fi 

if [ "$RUN_VFLD2SQL" == "yes" ]; then 
     echo "Running vfld2sql"
    $RS_DIR/point_pre_processing/vfld2sql.R 
fi 
if [ "$RUN_INTERPOL2SQL" == "yes" ]; then 
     echo "Running interpol2sql"
    $RS_DIR/point_pre_processing/interpol2sql.R 
fi

if [ "$RUN_POINT_VERF" == "yes" ]; then 
   echo "Running verification to get rds files"
   $RS_DIR/point_verif/point_verif.R 
   mkdir -p $RS_DIR/../plot_point_verif/cases/$CASE_STUDY/
   cp -R $RS_DIR/../cases/$CASE_STUDY/output/verif_results/*.rds $RS_DIR/../plot_point_verif/cases/$CASE_STUDY/
fi 

if [ "$RUN_POINT_VERF_LOCAL" == "yes" ]; then 
   echo "Running complete graphic verification set"
   $RS_DIR/point_verif/point_verif_local.R   
   mkdir -p $RS_DIR/../plot_point_verif_local/cases/$CASE_STUDY/
   cp -R $RS_DIR/../cases/$CASE_STUDY/output/*/*.png $RS_DIR/../plot_point_verif_local/cases/$CASE_STUDY/
fi 

if [ "$SCORECARDS" == "yes" ]; then 
   echo "Running scorecards generation"
   $RS_DIR/point_verif/create_scorecards.R   
fi 

if [ "$SHOW_WEB_STATIC" == "yes" ]; then
	$RS_DIR/visualization/shiny_launch_static.R
fi
if [ "$SHOW_WEB_DYNAMIC" == "yes" ]; then
        $RS_DIR/visualization/shiny_launch_dynamic.R
fi
if [ "$UPDATE_SHINYAPPSIO" == "yes" ]; then
        $RS_DIR/visualization/update_shinyappsio.R
fi

