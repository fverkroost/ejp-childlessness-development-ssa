# Prepare everything for function
# Check if inputs are correct and assign the inputs to the global environment
prepareFunction <- function(
  ageCutoff = c(40, 45), 
  lagWomen, 
  lagMen, 
  HIV = c(TRUE, FALSE), 
  sexRatios = c(TRUE, FALSE),
  sampleSizes = c(0, 50, 100), 
  childlessnessDefinition = c('everBorn', 'childDied'), 
  imputeIndividual = c(TRUE, FALSE)
){
  
  # Assign function input values to global environment
  assign('ageCutoff', ageCutoff, envir = globalenv())
  assign('lagWomen', lagWomen, envir = globalenv())
  assign('lagMen', lagMen, envir = globalenv())
  assign('HIV', HIV, envir = globalenv())
  assign('sexRatios', sexRatios, envir = globalenv())
  assign('sampleSizes', sampleSizes, envir = globalenv())
  assign('childlessnessDefinition', childlessnessDefinition, envir = globalenv())
  assign('imputeIndividual', imputeIndividual, envir = globalenv())
  
  # Create path name for outputting results
  my_hiv <- ifelse(HIV == TRUE, "_HIV", "")
  my_sr <- ifelse(sexRatios == TRUE, "_sexRatios", "")
  my_impind <- ifelse(imputeIndividual == TRUE, "_impInd", "")
  path <- paste0(
    'results/ageCut', 
    ageCutoff, 
    '_lagW', 
    lagWomen, 
    '_lagM', 
    lagMen, 
    my_hiv, 
    my_sr, 
    '_sampleCut', 
    sampleSizes, 
    '_CLN', 
    childlessnessDefinition, 
    my_impind, 
    '/'
  )
  assign('path', path, envir = globalenv())
  
  # Create output folder if it does not yet exist
  if (!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }
  
}

# Main function for executing all analyses
childlessnessHumanDevelopment <- function(
  ageCutoff = c(40, 45), 
  lagWomen, lagMen, 
  HIV = c(TRUE, FALSE), 
  sexRatios = c(TRUE, FALSE),
  sampleSizes = c(0, 50, 100), 
  childlessnessDefinition = c('everBorn', 'childDied'), 
  path, 
  imputeIndividual = c(TRUE, FALSE)
){
  
  # Load the data into R
  message("Data loading has begun!")
  invisible(source('script-data-loading.R'))
  
  # Prepare the data for usage, including variable recoding, sample selections, country selections,
  # age selections, region harmonisation, childlessness classification, removal of duplicate data
  message("Data preparation has begun!")
  invisible(source('script-data-prep.R'))
  
  # Plot data (e.g. tile plots, joyplots, worldmaps, scatterplots, distributions, Cleveland dot plot)
  message("Data plotting has begun!")
  invisible(source('script-data-plot.R'))
  
  # Perform revisions for EJP
  message("Execution of additional analyses has begun!")
  invisible(source('script-additional-analyses.R'))
  
  # Run the multilevel macro model
  message("Multilevel modelling has begun!")
  invisible(source('script-multilevel-model.R'))
  
}