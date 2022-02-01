message('WARNING! Downloading DHS survey data; note that these data may have changed since this code was written.')

# Define countries of interest in sub-Saharan Africa (SSA)
countries <- read_excel('data_files/ssa_country_codes.xlsx')
dhs_ssa <- na.omit(countries$DHS)

# Create directory to save the DHS data
dhs_data_folder = "dhs_data/"
if (!dir.exists(dhs_data_folder)){
  dir.create(dhs_data_folder)
}

# Check if necessary files (for replicability purposes) have already been downloaded
# If yes, use these; if no, then download them
necc_files <- readLines("data_files/necessary_dhs_files.txt")
necc_files_no_format = tolower(sub("\\..*", "", necc_files))
existing_dhs_files <- list.files(path = dhs_data_folder, pattern = "*.rds")
if (length(existing_dhs_files) > 0){
  
  # Keep individual files for sub-Saharan African countries
  print("Surveys already downloaded from DHS repository")
  keep_dhs_files <- existing_dhs_files[tolower(sub("\\..*", "", existing_dhs_files)) %in% necc_files_no_format]
  downloads <- as.list(paste0(dhs_data_folder, keep_dhs_files))
  dhs_files <- unlist(downloads)
  
} else {
  
  # Find all the surveys in SSA
  survs <- dhs_surveys(countryIds = dhs_ssa, all_results = TRUE)
  
  # Find names and locations of datasets that we want to download
  datasets <- list()
  for (i in 1:nrow(survs)){
    tryCatch({
      datasets[[i]] <- dhs_datasets(
        surveyIds = survs$SurveyId[i], 
        fileFormat = "SV"
      )
    }, 
    error = function(e){}
    )
  }
  datas <- plyr::rbind.fill(datasets)
  
  # Keep only the necessary surveys
  data_files = datas[tolower(sub("\\..*", "", datas$FileName)) %in% necc_files_no_format, ]
  files_to_download = sub("\\..*", "", datas$FileName)
  files_needed = sub("\\..*", "", necc_files)
  if (nrow(data_files) != length(necc_files)){
    files_needed_but_not_download = files_needed[which(!files_needed %in% files_to_download)]
    cat('The following files are needed but not downloaded:\n')
    cat(paste0(files_needed_but_not_download, "\n"))
    stop('Make sure all the necessary files are included in the DHS file download!')
  }
  
  # Download the datasets of interest
  # Try 5 times if it doesn't work directly
  downloads <- list()
  print("Downloading DHS surveys from the DHS repository")
  pb = txtProgressBar(min = 1, max = nrow(data_files)) 
  for (i in 1:nrow(data_files)){ 
    bo <- 0
    while (bo != 5){
      dwnld <- try(get_datasets(
        dataset_filenames = data_files$FileName[i], 
        encoding = "latin1", 
        download_option = "both"
      ))
      if (class(dwnld) == "try-error"){
        cat("Error in downloading data file")
        Sys.sleep(1)
        bo <- bo + 1
        print(paste0("Reconnecting for the ", bo, "nd time"))
      } else {
        downloads[[i]] <- dwnld[[1]][[1]]
        break
      } 
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  no_download <- which(!(substr(data_files$FileName, 1, 6) %in% substr(basename(unlist(downloads)), 1, 6)))
  if (length(no_download) > 0){
    cat("Cannot download following datasets: ", substr(basename(unlist(downloads)), 1, 6)[no_download], "\n")
  }
  downloads_org <- downloads
  downloads <- downloads[which(unlist(lapply(downloads, is.null)) == FALSE)]
  
  # Move files from cache to DHS data folder (to remember when code is run more than once)
  file.copy(
    from = unlist(downloads), 
    to = paste0(getwd(), "/", dhs_data_folder),
    overwrite = TRUE, 
    recursive = FALSE, 
    copy.mode = TRUE
  )
  
  # Get all downloaded DHS file names
  dhs_files <- list.files(path = dhs_data_folder, pattern = "*.rds")
  dhs_files <- paste0(dhs_data_folder, dhs_files)
  
}

# Define variables to get provinces (regionvars), individual variables (ovars) and healthcare variables (cvars)
regvars <- c("govern",	"006", 'depar',	"depart",	"dept",	"depto",	"devreg",	'regdist', 'district',
             "dist",	"domain",	"newzon",	"oblast",	"pref",	"prov",	"provin",	"reg", 'dist2', 'distr',
             "region",	"regnat",	"regnew",	"state", "depar", "qprov", "county", "cty", "cnty", "provi")
regionvars <- c(paste0('s', regvars), paste0('sm', regvars), paste0(c('v', 'mv'), '023'), 'mspref', 'MSPREF')
regionvars <- c(regionvars, toupper(regionvars))
ovars <- c("000","001","002","003", "005", "008", "010",'011',"012", "102", "130", "131", "134", "149", "022",      
           "190", "201", "212","501", "502", "505", "535", "602", "605", "020", "613", "213", '505A', '505B', 
           "101",	"024",	"023", '509', '525', '531', '511', '228', '206', '207', '218', '505a', '505b', "025")
cvars <- c('m3a.1', 'M3A.1', 'm3b.1', 'M3B.1', 'm15.1', 'M15.1', 'b3.01', 'B3.01',
           'm3a$1', 'M3A$1', 'm3b$1', 'M3B$1', 'm15$1', 'M15$1', 'b3$01', 'B3$01')      

# Maximum number of variables to choose from
maxvars <- length(c(ovars, regionvars, cvars)) + 1 

# Initialize empty matrix for storing missing variables per data set and empty list for storing data sets
missings <- as.data.frame(matrix(NA, nrow = length(dhs_files), ncol = maxvars))          
datalist_org <- vector("list", length(dhs_files))                                                      
names(datalist_org) <- rownames(missings) <- substr(unlist(lapply(dhs_files, basename)), 1, 6)                                                                  

# Throw error in case of a warning
options(warn = 2)

# Download the selected files from DHS repository
print("Loading DHS surveys from downloads into R")
pb = txtProgressBar(min = 1, max = length(dhs_files)) 
for (i in 1:length(dhs_files)){
  
  # Load dataset into R
  data <- readRDS(dhs_files[i])                
  
  # Define variable start letter for data (HV for household, V for women and MV for men)
  letter <- unique(substr(sub("[^[:alpha:]]+", "", names(data)[2:min(10, ncol(data))]), 1, 2)) 
  
  # Get ID variable and relevant variables
  idletter <- names(data)[1]
  vars <- c(idletter, paste0(letter, ovars), regionvars, cvars)
  
  # Save which variables are not in the data set
  falses <- vars[!(vars %in% names(data))]
  row <- c(falses, rep(NA, ncol(missings) - length(falses)))
  missings[i, ] <- row
  
  # Keep only the variables we need (from those present in the data)
  vars <- vars[vars %in% names(data)]
  datalist_org[[i]] <- data.frame(data[, vars])
  rm(data)
  if (letter != toupper(letter)){
    names(datalist_org[[i]]) <- toupper(vars)
  }
  
  # Remove dots from data names
  if (grepl(".", names(datalist_org[[i]])[1])){
    names(datalist_org[[i]])[1] <- gsub("\\.", "", names(datalist_org[[i]])[1])
  }
  
  setTxtProgressBar(pb,i)
  
}
close(pb)
options(warn = 1)

# Save list of data sets as R object
if (!dir.exists('saved_objects/')){
  dir.create('saved_objects/')
}
save(datalist_org, file = 'saved_objects/datalist_org.RData')

# Clean up working environment
rem_list <- ls()
env_vars = c("datalist_org", "ageCutoff", "childlessnessDefinition", 
             "HIV", "path", "sampleSizes", "sexRatios",
             "lagWomen", "lagMen", "imputeIndividual")
rm(list = rem_list[!rem_list %in% env_vars])
datalist <- datalist_org
