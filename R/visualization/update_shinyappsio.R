#!/usr/bin/env Rscript

library(shiny)
library(here)
library(rsconnect)

deployApp(appDir='/perm/sp3c/deode_project/DE_330-verif-scripts/plot_point_verif/', appName="plot_point_verif",appTitle="DE_330 point verification dynamic")
deployApp(appDir='/$PERM/deode_project/DE_330-verif-scripts/plot_point_verif_local/', appName="plot_point_verif_local",appTitle="DE_330 point verification static")






