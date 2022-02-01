# Clear workspace and console
rm(list = ls())
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()
cat("\014")  

# Load necessary packages into R
library(stringr)
library(rdhs) 
# if data loading gives an error in rdhs::get_datasets(), then uninstall rhds and install using command:
# devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(readxl)
library(xlsx)
library(labelled)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(httr)
library(tidyr)
library(epitools)
library(dummies)
library(missForest)
library(rgdal)
library(ggridges)
library(RColorBrewer)
library(xtable)
library(stats)
library(VIM)
library(mice)
library(lme4)
library(stargazer)
library(corrplot)
library(ggeffects)
library(ggpubr)
library(merTools)

# Set seed
set.seed(1234)

# Set graphics device size
dev.new(height = 4000, width = 5142)
