# Source functions necessary for executing this script
source('functions-data-prep.R')

# Remove surveys that have only ever-married or currently married men and women
indicator <- unlist(lapply(datalist, removeMarriedSamples))                 # make indicator for all-(wo)men samples
remnam <- !(names(datalist) %in% names(which(indicator == 0)))              # names of only (ever-)married files
datalist <- datalist[!(names(datalist) %in% remnam)]                        # remove (ever-)married files
names_datalist <- names(datalist)

# Create date variable from 008 and check minimum and maximum dates
datalist <- lapply(seq_along(datalist), createDate, z = datalist, n = names_datalist)
min(unique(unlist(lapply(datalist, function(x){unique(x$date)}))))
max(unique(unlist(lapply(datalist, function(x){unique(x$date)}))))

# Create country variable from name of the survey
datalist <- lapply(seq_along(datalist), countryFun, z = datalist, n = names_datalist)
unique(unlist(lapply(datalist, function(x){unique(x$country)})))

# Create age variable from 012
datalist <- lapply(datalist, ageFun)
unique(unlist(lapply(datalist, function(x){unique(x$age)})))

# Create gender variable from filename IR or MR
datalist <- lapply(seq_along(datalist), genderFun, z = datalist, n = names_datalist)
unique(unlist(lapply(datalist, function(x){unique(x$gender)})))

# Create residence variable from 025
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("025", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, residenceFun)
unique(unlist(lapply(datalist, function(x){unique(x$residence)})))

# Create indicator for pregnancy from 213
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("213", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, pregnantFun)
unique(unlist(lapply(datalist, function(x){unique(x$pregnant)})))

# Create children variable from 201
datalist <- lapply(datalist, childrenFun)
unique(unlist(lapply(datalist, function(x){unique(x$children)})))

# Create childlessness variable from 201
if (childlessnessDefinition == 'everBorn'){
  # Classify those who have had a child that died as not childless
  datalist <- lapply(datalist, childlessEverBorn)
  unique(unlist(lapply(datalist, function(x){unique(x$childless_dummy)})))
} else if (childlessnessDefinition == 'childDied'){
  # Classify those who have had a child that died as childless
  datalist <- lapply(datalist, childlessChildDied)
  unique(unlist(lapply(datalist, function(x){unique(x$childless_dummy)})))
}

# Create sample weight variable from 005
datalist <- lapply(datalist, weightFun)

# Create fertility preference variable from 602
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("602", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, fertilityFun)
unique(unlist(lapply(datalist, function(x){unique(x$fertPref)})))

# Create children desire variable from 605
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("605", names(x))])}))
vec[unique(names(vec))]
datalist <- invisible(lapply(datalist, desireFun))
unique(unlist(lapply(datalist, function(x){unique(x$desireOwn)})))

# Create ideal number of children variable from 613
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("613", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, idealFun)
unique(unlist(lapply(datalist, function(x){unique(x$idealChild)})))

# Create age at first childbirth variable from 212
unique(unlist(lapply(datalist, function(x){unique(as.character(x[, which(grepl('212', names(x)))]))})))
datalist <- lapply(seq_along(datalist), ageBirthFun, z = datalist, n = names_datalist)
unique(unlist(lapply(datalist, function(x){unique(x$ageBirth)})))

# Create age at first marriage/cohabitation variable from 511
unique(unlist(lapply(datalist, function(x){unique(as.character(x[, which(grepl('511', names(x)))]))})))
datalist <- lapply(datalist, ageCohabMar)
unique(unlist(lapply(datalist, function(x){unique(x$ageFirstCohabMar)})))

# Create age at first sexual intercourse variable from 525/531
unique(unlist(lapply(datalist, function(x){unique(as.character(x[, which(grepl('531', names(x)))]))})))
datalist <- lapply(datalist, ageFirstSex)
unique(unlist(lapply(datalist, function(x){unique(x$ageFirstSex)})))

# Create assistance at delivery variable from M3A/M3B
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("M3A", names(x))])}))
vec[unique(names(vec))]
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("M3B", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, assistanceDelivery)
unique(unlist(lapply(datalist, function(x){unique(x$deliveryNurse)})))
unique(unlist(lapply(datalist, function(x){unique(x$deliveryDoctor)})))

# Create polygamy variable from 505
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("505", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, polygamyFun)
unique(unlist(lapply(datalist, function(x){unique(x$polygamy)})))

# Create marital status variable from 501 and 502
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("501", names(x))])}))
vec[unique(names(vec))]
vec <- unlist(lapply(datalist, function(x){val_labels(x[, grepl("502", names(x))])}))
vec[unique(names(vec))]
datalist <- lapply(datalist, maritalFun)
combs <- unique(plyr::rbind.fill(lapply(datalist, maritalCombinations)))
datalist <- lapply(datalist, maritalStatus)
unique(unlist(lapply(datalist, function(x){unique(x$marital)})))

# Create birth cohort variable
datalist <- invisible(lapply(datalist, birthCohortfun))
min(na.omit(unlist(lapply(datalist, function(x){x$birthCohort}))))
max(na.omit(unlist(lapply(datalist, function(x){x$birthCohort}))))

# Make an id variable from (M)CASEID
datalist <- invisible(lapply(datalist, caseID))

# Harmonize regions in DHS data according to GDL data
dhs_gdl_regions <- data.frame(
  read_excel(
    'data_files/dhs_gdl_region_match.xlsx', 
    sheet = "with region vars", 
    col_types = "text", 
    trim_ws = FALSE
  )
)
datalist <- lapply(seq_along(datalist), regionFun, z = datalist, n = names_datalist, matchdf = dhs_gdl_regions)

# Make a year variable and make sure all years for a particular survey are the same
# Example: Survey 2006-2007 must all have year 2006; not 2007
datalist <- lapply(datalist, sameDate)

# Retain only relevant variables in data lists
unique(unlist(lapply(datalist, function(x){names(x)})))
variables <- c("date", "country", "age", "gender", "residence", "children", "year", "region",
               "childless_dummy", "weight", "fertPref", "desireOwn", "idealChild", "newRegion",
               "ageBirth", "polygamy", "marital", "birthCohort", "id", "pregnant", 
               'ageFirstCohabMar', 'ageFirstSex', 'deliveryDoctor', 'deliveryNurse')
datalist <- lapply(datalist, function(x){x <- x[, which(names(x) %in% variables)]})

# Fill up data frames with empty columns to match for merging
datalist <- lapply(datalist, addEmptyColumns, vars = variables)

# Distinguish between NAs from missing values and from question not being asked in fertility questions
nams602 <- names(which(!(unlist(lapply(datalist_org, function(x){any(grepl("602", names(x)))})))))
ind602 <- which(names_datalist %in% nams602)
nams613 <- names(which(!(unlist(lapply(datalist_org, function(x){any(grepl("613", names(x)))})))))
ind613 <- which(names_datalist %in% nams613)
for (i in 1:length(ind602)){
  datalist[[ind602[i]]]$fertPref <- rep("question not asked", nrow(datalist[[ind602[i]]]))
}
for (i in 1:length(ind613)){
  datalist[[ind613[i]]]$idealChild <- rep("question not asked", nrow(datalist[[ind613[i]]]))
}

# Remove double cases from data frames
datalist <- lapply(datalist, function(x){
  x <- x[!duplicated(x), ]
})

# Remove outliers w.r.t. age and number of children
datalist <- lapply(datalist, outlierFun)

# Merge all data frames into one data frame
africadata <- africadata_org <- plyr::rbind.fill(datalist)
africadata$date <- format(as.Date(paste0("12-", africadata_org$date), format="%d-%m-%Y"), "%m-%Y")
africadata["survey"] <- rep(names_datalist, sapply(datalist, nrow))

# Create variable with full country name and add ISO3 code
countries <- read_excel('data_files/ssa_country_codes.xlsx', sheet = 'DHS')
africadata["fullCountry"] <- countries$Name[match(africadata$country, countries$DHS)]
africadata["ISO3"] <- countries$ISO3[match(africadata$country, countries$DHS)]

# Plot age distributions for childlessness and age at first childbirth
distrBirthAgeChildless(africadata, path)

# Check that only married people are/can be in polygamous unions
table(africadata$polygamy, africadata$marital)

# Check how many files are AIS and MIS (and why polygamy is missing)
miss_allna <- sub("FL", "", unique(africadata$survey[which(africadata$polygamy %in% c('all NA'))]))
miss_notpres <- sub("FL", "", unique(africadata$survey[which(africadata$polygamy %in% c('variable not present'))]))
ais <- as.character(read.delim('data_files/aisfiles.txt')[, 1])
ais <- str_match(ais, "Filename=(.*?)DT.zip")[, 2]
mis <- as.character(read.delim('data_files/misfiles.txt')[, 1])
mis <- str_match(mis, "Filename=(.*?)DT.zip")[, 2]
miss_allna[which(!(miss_allna %in% c(ais, mis)))]
miss_notpres[which(!(miss_notpres %in% c(ais, mis)))]

# Set all NA and not present to NA in polygamy and pregnancy variables
africadata$polygamy[which(africadata$polygamy %in% c("all NA", "variable not present"))] <- NA
africadata$pregnant[which(africadata$pregnant %in% c("all NA", "question not asked"))] <- NA

# Age limitations
africadata_before_age_cutoff <- africadata
africadata <- africadata[which(africadata$age >= ageCutoff), ]

# Make childlessness classification according to decision tree
africadata["typeChildless"] <- NA
africadata$typeChildless[africadata$childless_dummy == 0] <- "children"
inds <- which(africadata$childless_dummy == 1)
africadata <- childlessTree(africadata, inds)
table(africadata$marital[inds])
table(africadata$typeChildless[inds])

# Remove irrelevant variables
sapply(africadata, function(x){sum(is.na(x))})
remvars <- c("pregnant", "desireOwn", "idealChild", "fertPref")
africadata <- africadata[, !names(africadata) %in% remvars]

# Create extensive polygamy variable (that takes marital status into account)
africadata['polygamyExtensive'] <- as.character(africadata$polygamy)
africadata$polygamyExtensive <- ifelse(africadata$polygamy == '0' & africadata$marital != 'marr/coh/rel', 'not marr/coh/rel', as.character(africadata$polygamy))
africadata$polygamyExtensive <- factor(africadata$polygamyExtensive, levels = c('0', '1', 'uncertain', 'not marr/coh/rel'))
africadata$polygamyExtensive <- as.character(africadata$polygamyExtensive)

# Remove ambiguous categories
africadata$polygamy[which(africadata$polygamy == "uncertain")] <- NA
africadata$typeChildless[which(africadata$typeChildless == "undetermined")] <- NA

# Download population data per age group for men
message('WARNING! Downloading United Nations population data; note that these data may have changed since this code was written.')
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx"
GET(url, write_disk("data_files/malePop.xlsx", overwrite = TRUE))         # download data from URL
malePop <- read_excel("data_files/malePop.xlsx", sheet = 1, range = "C17:AC18122", col_names = TRUE)
countryvar <- "Region, subregion, country or area *"
malePop[malePop[, countryvar] == "Democratic Republic of the Congo", countryvar] <- "Congo Democratic Republic"
malePop[malePop[, countryvar] == "Côte d'Ivoire", countryvar] <- "Cote d'Ivoire"
malePop[malePop[, countryvar] == "Eswatini", countryvar] <- "Swaziland"
malePop[malePop[, countryvar] == "United Republic of Tanzania", countryvar] <- "Tanzania" 

# Download population data per age group for women
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx"
GET(url, write_disk("data_files/femalePop.xlsx", overwrite = TRUE))       # download data from URL
femalePop <- read_excel("data_files/femalePop.xlsx", sheet = 1, range = "C17:AC18122", col_names = TRUE)
femalePop[femalePop[, countryvar] == "Democratic Republic of the Congo", countryvar] <- "Congo Democratic Republic"
femalePop[femalePop[, countryvar] == "Côte d'Ivoire", countryvar] <- "Cote d'Ivoire"
femalePop[femalePop[, countryvar] == "Eswatini", countryvar] <- "Swaziland"
femalePop[femalePop[, countryvar] == "United Republic of Tanzania", countryvar] <- "Tanzania"

# Add 5-year age groups to africadata
ageGroups <- data.frame(age = seq(0, 104), group = rep(names(femalePop)[which(grepl('^[0-9]', names(femalePop)))], each = 5))
africadata["ageGroup"] <- as.character(ageGroups$group[match(africadata$age, ageGroups$age)])

# Add sample size of original surveys (before data manipulation)
africadata["sampleSize"] <- unlist(lapply(datalist_org, nrow))[match(africadata$survey, names(datalist_org))]

# Add population size and weights to africadata
splits <- split(africadata, list(africadata$gender, africadata$year, africadata$fullCountry, africadata$ageGroup))
splits <- splits[unlist(lapply(splits, nrow)) > 0]
africadata <- rbind.fill(lapply(splits, popWeighting, malePop, femalePop))

# Include sample size limitations
splits <- split(africadata, list(africadata$survey, africadata$newRegion))
splits <- splits[which(unlist(lapply(splits, nrow)) > sampleSizes)]
africadata <- rbind.fill(splits)

# ---------------------------------------------------------------------------------
# Prepare data for the SHDI
# ---------------------------------------------------------------------------------

if (!file.exists("data_files/GDL-Sub-national-HDI-data.csv")){
  stop('Make sure to download the SHDI data first! See explanation below the error message in code.')
}

# Download indicator 'Sub-national HDI': 
# 1. Go to https://globaldatalab.org/shdi/shdi/
# 2. Choose indicator 'Sub-national HDI'.
# 3. Click 'Download this' and choose the .csv file format.
# 4. Save the downloaded file to folder 'data_files' in the RStudio project directory under name 'GDL-Sub-national-HDI-data.csv'

# Download indicator 'Educational index': 
# 1. Go to https://globaldatalab.org/shdi/shdi/
# 2. Choose indicator 'Educational index':.
# 3. Click 'Download this' and choose the .csv file format.
# 4. Save the downloaded file to folder 'data_files' in the RStudio project directory under name 'GDL-Educational-index-data.csv'

# Download indicator 'Health index': 
# 1. Go to https://globaldatalab.org/shdi/shdi/
# 2. Choose indicator 'Health index'.
# 3. Click 'Download this' and choose the .csv file format.
# 4. Save the downloaded file to folder 'data_files' in the RStudio project directory under name 'GDL-Health-index-data.csv'

# Download indicator 'Income index': 
# 1. Go to https://globaldatalab.org/shdi/shdi/
# 2. Choose indicator 'Income index'.
# 3. Click 'Download this' and choose the .csv file format.
# 4. Save the downloaded file to folder 'data_files' in the RStudio project directory under name 'GDL-Income-index-data.csv'

message('WARNING! Downloading Global Data Lab SHDI data; note that these data may have changed since this code was written.')
shdi_allcomp <- read.csv("data_files/GDL-Sub-national-HDI-data.csv")
shdi_edu <- read.csv("data_files/GDL-Educational-index-data.csv")
shdi_health <- read.csv("data_files/GDL-Health-index-data.csv")
shdi_income <- read.csv("data_files/GDL-Income-index-data.csv")

# Combine SHDI data for different indicators
shdi_list <- list(shdi_allcomp, shdi_edu, shdi_health, shdi_income)
shdi_total <- rbind.fill(shdi_list)
shdi_total["Indicator"] <- rep(c("SHDI", "Education", "Health", "Income"), unlist(lapply(shdi_list, nrow)))

# Keep only SSA observations in SHDI data
shdi_total <- shdi_total[shdi_total$ISO_Code %in% countries$ISO3, ]

# Change country names of SHDI data to match DHS data
shdi_total["Country_DHS"] <- dhs_gdl_regions$Country_DHS[match(shdi_total$Country, dhs_gdl_regions$Country_GDL)] 

# Plot region-country SHDI ratios over time per indicator as proof for assumption of stability
splits <- split(shdi_total, shdi_total$Country_DHS)
splits <- splits[unlist(lapply(splits, nrow)) > 0]
invisible(lapply(splits, plotCountryRegionRatioSHDI, path))

# Interpolate SHDI data (fill in missings for in-between years)
shdi_melt <- gather(shdi_total, "Year", "Value", -Country, -ISO_Code, -Level, -GDLCODE, -Region, -Indicator, -Country_DHS)
shdi_melt$Year <- sub("X", "", shdi_melt$Year)
shdi_melt <- spread(shdi_melt, "Indicator", "Value")
splits <- split(shdi_melt, list(shdi_melt$Country_DHS, shdi_melt$Region))
splits <- splits[unlist(lapply(splits, nrow)) > 0]
shdi_interpol <- rbind.fill(lapply(splits, interpolateSHDI))

# ---------------------------------------------------------------------------------
# Prepare data for the HIHD
# ---------------------------------------------------------------------------------

# Get names of HIHD files to be downloaded
cc <- as.character(unique(africadata$fullCountry))
cc[which(cc == "Central African Republic")] <- "CAR"
cc[which(cc == "Congo Democratic Republic")] <- "Congo Dem R"
cc[which(cc == "Cote d'Ivoire")] <- "Cote dIvoire"
cc[which(cc == "Timor-Leste")] <- "Timor Leste"
cc[which(cc == "Sao Tome and Principe")] <- NA
cc <- na.omit(cc)
cc <- gsub(" ", "_", cc)

# Download HIHD files into folder from URLs
message('WARNING! Downloading HIHD data from Espacio Investiga; note that these data may have changed since this code was written.')
url_list <- paste0("https://espacioinvestiga.org/00REPO/TablasExcel_HIHD/", cc, ".xls")
comp_list <- list()
cnams <- as.character(unique(africadata$fullCountry))
cnams <- cnams[which(!(cnams == "Sao Tome and Principe"))]
for (i in 1:length(url_list)){
  nam <- paste0('data_files/HIHD_components/', cc[i], '.xls')
  if (!dir.exists("data_files/HIHD_components")){
    dir.create("data_files/HIHD_components")
  }
  if (!(file.exists(nam))){
    download.file(url_list[i], nam, quiet = TRUE)
  }
  comp_list[[i]] <- read_excel(nam, range = "A3:H27")
  comp_list[[i]]['fullCountry'] <- cnams[i]
}

# Put all HIHD data into one data frame
compHIHD <- rbind.fill(comp_list)
compHIHD[compHIHD == ".."] <- NA
compHIHD[, !(names(compHIHD) == 'fullCountry')] <- apply(compHIHD[, !(names(compHIHD) == 'fullCountry')], 2, function(x) as.numeric(x))
compHIHD <- compHIHD[, c("Year", "HIHD", "Life_Expectancy", "Education", "Adjusted_Income", "fullCountry")]
names(compHIHD) <- c("Year", "HIHD", "Health", "Education", "Income", "fullCountry")

# Plot HIHD components over time per indicator
splits <- split(compHIHD, compHIHD$fullCountry)
splits <- splits[unlist(lapply(splits, nrow)) > 0]
# invisible(lapply(splits, plotComponentsHIHD, path))

# Interpolate HIHD data to get data for every year
range_years <- c(min(africadata$year) - (max(africadata$age) - 20), max(africadata$year))
range <- seq(min(range_years), max(range_years), 1)
hihd_interpol <- rbind.fill(lapply(splits, interpolateHIHD, range))

# ---------------------------------------------------------------------------------
# Combine data from the SHDI and HIHD to create the SHIHD
# ---------------------------------------------------------------------------------

# Combine SHDI and HIHD data to construct SHIHD
uni_country <- unique(africadata$fullCountry)
uni_year <- unique(hihd_interpol$Year)
shdi_indicator <- c("SHDI", "Education", "Health", 'Income')
hihd_indicator <- c("HIHD", "Education", "Health", 'Income')
newnams <- c("SHIHD", "SHIHD_Education", "SHIHD_Health", 'SHIHD_Income')
ratios_list <- list()
for (i in 1:length(uni_country)){
  
  # Subset SHDI and HIHD data to the current country
  sub_shdi <- shdi_interpol[shdi_interpol$Country_DHS == uni_country[i], ]
  sub_hihd <- hihd_interpol[hihd_interpol$fullCountry == uni_country[i], ]
  ratios_list[[i]] <- list()
  
  for (j in 1:length(uni_year)){
    
    # Find the closest year available in the data
    years <- as.numeric(sub_shdi$Year[!is.na(sub_shdi[, shdi_indicator[1]])])
    closest_year_shdi <- years[which.min(abs(years - uni_year[j]))]
    sdf <- sub_shdi[sub_shdi$Year == closest_year_shdi & sub_shdi$Region != "Total", ]
    ratios_list[[i]][[j]] <- data.frame(Region = sub_shdi$Region[sub_shdi$Year == closest_year_shdi],
                                        Year = uni_year[j], Country = uni_country[i])
    
    # Generate the subnational-national ratios from SHDI 
    # and use these to create a subnational HIHD
    for (z in 1:length(shdi_indicator)){
      subdf <- sub_shdi[sub_shdi$Year == closest_year_shdi & sub_shdi$Region != "Total", c("Region", shdi_indicator[z])]
      tot <- sub_shdi[sub_shdi$Year == closest_year_shdi & sub_shdi$Region == "Total", c("Region", shdi_indicator[z])]
      ratios <- data.frame(Region = subdf$Region, Ratio = subdf[, shdi_indicator[z]] / tot[, shdi_indicator[z]])
      names(ratios)[names(ratios) == "Ratio"] <- shdi_indicator[z]
      hihd_val = sub_hihd[sub_hihd$Year == uni_year[j], hihd_indicator[z]]
      ratios[newnams[z]] <- ratios[, shdi_indicator[z]] * hihd_val
      ratios_list[[i]][[j]][newnams[z]] <- NA
      if (length(hihd_val) > 0){
        rows <- match(ratios$Region, ratios_list[[i]][[j]]$Region)
        ratios_list[[i]][[j]][rows, newnams[z]] <- ratios[, newnams[z]] 
        rows <- which(ratios_list[[i]][[j]]$Region == "Total")
        ratios_list[[i]][[j]][rows, newnams[z]] <- hihd_val 
      }
    }
  }
  ratios_list[[i]] <- rbind.fill(ratios_list[[i]])
}
shihd <- rbind.fill(ratios_list)

# Add SHIHD to africadata (lagged by 20 years if necessary)
splits <- split(africadata, list(africadata$year, africadata$fullCountry, africadata$newRegion, africadata$gender))
splits <- splits[unlist(lapply(splits, nrow)) > 0]
africadata <- rbind.fill(lapply(splits, addSHIHD, lagWomen, lagMen, shihd))

# ---------------------------------------------------------------------------------
# Prepare data for the HIV prevalence
# ---------------------------------------------------------------------------------

if (HIV == TRUE){
  
  if (!dir.exists("data_files/HIV_prevalence/")){
    stop('Make sure to download the HIV data first! See explanation below the error message in code.')
  }
  message('WARNING! Downloading HIV data from different sources; note that these data may have changed since this code was written.')
  
  # Load HIV-GDL region machting scheme
  hiv_gdl_regions <- data.frame(
    read_excel(
      'data_files/dhs_gdl_region_match.xlsx', 
      sheet = 'HIV', 
      col_types = "text", 
      trim_ws = FALSE
    )
  )
  
  # Load regional data from STATcompiler
  statcomp <- read_excel("data_files/HIV_prevalence/regional/STATcompilerHIVdata.xlsx", range = "A4:E703")
  names(statcomp) <- c('Country', 'Survey', 'Characteristic', 'Women', 'Men')
  statcomp["Region"] <- hiv_gdl_regions$GDL_Region[match(statcomp$Characteristic, hiv_gdl_regions$HIV_Region)]
  statcomp$Women <- gsub("\\s*\\([^\\)]+\\)","", statcomp$Women)
  statcomp$Men <- gsub("\\s*\\([^\\)]+\\)","", statcomp$Men)
  statcomp['Year'] <- substr(statcomp$Survey, 1, 4)
  statcomp <- gather(statcomp, 'Gender', 'Value', -Country, -Survey, -Characteristic, -Region, -Year)
  
  # Load regional data from UNAIDS
  unaids_reg <- lapply(list.files(path = 'data_files/HIV_prevalence/regional', pattern = "*.csv", full.names = TRUE), read.csv)
  names(unaids_reg) <- unaids_countries <- list.files(path = 'data_files/HIV_prevalence/regional', pattern = "*.csv")
  unaids_countries <- gsub("RegionYoungMen.csv", "", unaids_countries)
  unaids_countries <- gsub("RegionYoungWomen.csv", "", unaids_countries)
  unaids_countries <- capitalize(unaids_countries)
  unaids_countries <- rep(unaids_countries, unlist(lapply(unaids_reg, nrow)))
  genders <- rep(rep(c("Men", "Women"), length(unaids_reg)/2), unlist(lapply(unaids_reg, nrow)))
  unaids_reg <- lapply(unaids_reg, function(x){
    x[, ] <- lapply(x[, ], as.character)
    return(x)
  })
  unaids_reg <- rbind.fill(unaids_reg)
  unaids_reg["Country"] <- unaids_countries
  unaids_reg["Gender"] <- genders
  unaids_reg["Region"] <- hiv_gdl_regions$GDL_Region[match(unaids_reg$States, hiv_gdl_regions$HIV_Region)]
  
  # Load national data from UNAIDS
  unaids_nat <- lapply(list.files(path = 'data_files/HIV_prevalence/national', pattern = "*.csv", full.names = TRUE), read.csv)
  genders <- rep(c("Men", "Women"), unlist(lapply(unaids_nat, nrow)))
  unaids_nat <- lapply(unaids_nat, function(x){
    x[, ] <- lapply(x[, ], as.character)
    return(x)
  })
  unaids_nat <- rbind.fill(unaids_nat)
  unaids_nat['Gender'] <- genders
  unaids_nat["Region"] <- "Total"
  
  # Bind regional and national UNAIDS data
  unaids <- rbind(unaids_reg[, !names(unaids_reg) == "States"], unaids_nat)
  unaids <- unaids[, !grepl("upper", names(unaids)) & !grepl("lower", names(unaids))]
  names(unaids) <- gsub("X", "", names(unaids))
  unaids <- gather(unaids, "Year", "Value", -Country, -Gender, -Region)
  unaids$Value <- gsub("[^0-9\\.]", "", unaids$Value) 
  
  # Change country names where necessary
  unaids$Country[unaids$Country == "CoteIvoire"] <- "Cote d'Ivoire"
  unaids$Country[unaids$Country == "Democratic Republic of the Congo"] <- "Congo Democratic Republic"
  unaids$Country[unaids$Country == "United Republic of Tanzania"] <- "Tanzania"
  statcomp$Country[statcomp$Country == "Swaziland"] <- "Eswatini"
  
  # Remove duplicated and all-NA rows
  hiv <- rbind(unaids, statcomp[, !grepl("Survey", names(statcomp)) & !grepl("Characteristic", names(statcomp))])
  hiv$Value <- as.numeric(hiv$Value)
  hiv <- hiv[!duplicated(hiv), ]
  hiv <- hiv[!apply(hiv, 1, function(x){sum(is.na(x))}) == ncol(hiv), ]
  
  # Compute SHIV by combining regional and national data
  uni_countries <- unique(africadata$fullCountry)
  uni_year <- unique(hiv$Year)
  uni_gender <- unique(hiv$Gender)
  ratios_list <- list()
  for (i in 1:length(uni_countries)){ 
    ratios_list[[i]] <- list()
    for (j in 1:length(uni_year)){ 
      ratios_list[[i]][[j]] <- list()
      for (z in 1:length(uni_gender)){ 
        subdf <- hiv[hiv$Country == uni_countries[i] & hiv$Year == uni_year[j] & hiv$Gender == uni_gender[z], ]
        reg_available <- (nrow(subdf) > 1 & !(all(subdf$Region == "Total")) & !any(is.na(subdf$Region == "Total")))
        nat_available <- (any(subdf$Region == "Total"))
        if (!reg_available & nat_available){
          years <- na.omit(unique(hiv$Year[hiv$Country == uni_countries[i] & hiv$Gender == uni_gender[z] & hiv$Region != "Total"]))
          if (length(years) > 0 & !(all(is.na(years)))){
            closest_year <- years[which.min(abs(as.numeric(years) - as.numeric(uni_year[j])))]
            sdf <- hiv[hiv$Country == uni_countries[i] & hiv$Year == closest_year & hiv$Gender == uni_gender[z], ]
            reg <- sdf[sdf$Region != "Total", ]
            tot <- sdf[sdf$Region == "Total", ]
            tot <- tot[complete.cases(tot), ]
            ratios <- data.frame(Region = reg$Region, Country = uni_countries[i], Gender = uni_gender[z],
                                 Year = uni_year[j], Ratio = reg$Value / mean(tot$Value))
            ratios["Value"] <- ratios$Ratio * mean(na.omit(subdf$Value[subdf$Year == uni_year[j] & subdf$Region == "Total"]))
            ratios_list[[i]][[j]][[z]] <- rbind(ratios[, !grepl("Ratio", names(ratios))], tot)
          } else {
            ratios_list[[i]][[j]][[z]] <- subdf
          }
        }
        if (reg_available & nat_available){
          ratios_list[[i]][[j]][[z]] <- subdf
        }
      }
      ratios_list[[i]][[j]] <- rbind.fill(ratios_list[[i]][[j]])
    }
    ratios_list[[i]] <- rbind.fill(ratios_list[[i]])
  }
  hiv <- rbind.fill(ratios_list)
  
  # Extrapolate HIV data to <1990 exponentially
  splits <- split(hiv, list(hiv$Country, hiv$Region, hiv$Gender))
  splits <- splits[unlist(lapply(splits, nrow)) > 0]
  hiv_interpol <- rbind.fill(lapply(splits, interpolateHIV))
  
  # Add HIV to africadata (with lag if necessary)
  # If regional data is not available, use national estimates
  splits <- split(africadata, list(africadata$year, africadata$fullCountry, africadata$newRegion, africadata$gender))
  splits <- splits[unlist(lapply(splits, nrow)) > 0]
  africadata <- rbind.fill(lapply(splits, addHIV, lagWomen, lagMen, hiv = hiv_interpol))
  
}

# Add sex ratios on the national level if input for sexRatios is TRUE (and lag)
if (sexRatios == TRUE){
  splits <- split(africadata, list(africadata$fullCountry, africadata$year, africadata$age, africadata$gender))
  splits <- splits[which(unlist(lapply(splits, nrow)) > 0)]
  africadata <- rbind.fill(lapply(splits, addSexRatios, femalePop, malePop, lagWomen, lagMen))
}

# Create age adjusted childlessness levels
splits <- split(africadata, list(africadata$country, africadata$gender, africadata$year))
splits <- splits[which(unlist(lapply(splits, nrow)) > 0)]
africadata <- rbind.fill(lapply(splits, ageAdjust, femalePop, malePop))

# Make sure all variables have the correct types
chars <- c('country', 'id', 'date', 'region', 'newRegion', 'fullCountry', 'ISO3', 'survey')
nums <- c('age', 'children', 'weight', 'ageBirth', 'ageFirstCohabMar', 'sampleSize',
          'ageFirstSex', 'birthCohort', 'year', 'popSize', 'countryWeight', 'finalWeight',
          paste0('SHIHD', c('', '_Education', '_Health', '_Income')), 'HIV', 'sexRatio',
          'nationalChildlessnessAgeadjusted', "nationalChildlessness")
facs <- c('gender', 'residence', 'typeChildless', 'polygamyExtensive', 'ageGroup',
          'deliveryDoctor', 'deliveryNurse', 'polygamy', 'childless_dummy', 'marital')
africadata[chars] <- sapply(africadata[chars], as.character)
africadata[nums] <- sapply(africadata[nums], function(x){as.numeric(as.character(x))})
africadata[facs] <- sapply(africadata[facs], function(x){as.factor(as.character(x))})

# Convert categorical variables to dummies and replace NA dummy variables by NA values in variable
dumdf <- dummy.data.frame(africadata[, facs])
for (j in 1:length(facs)){
  na_col <- which(grepl(facs[j], names(dumdf)) & grepl("NA", names(dumdf)))
  if (length(na_col) > 0){
    nas <- which(dumdf[, na_col] == 1)
    other_cols <- which(grepl(facs[j], names(dumdf)) & !grepl("NA", names(dumdf)))
    dumdf[nas, other_cols] <- NA
    dumdf <- dumdf[, -na_col]
  }
}

# Impute missing data at the individual level
# Calculate missingness in each variable and impute data per country (to shorten runtime)
apply(africadata, 2, function(x){sum(is.na(x)) / length(x) * 100})
missdata <- cbind(africadata[, nums], dumdf)
if (imputeIndividual == TRUE){
  splits <- split(africadata, list(africadata$fullCountry, africadata$year))
  splits <- splits[unlist(lapply(splits, nrow)) > 0]
  nrows_countries <- unlist(lapply(splits, nrow))
  inds <- rep(1:length(splits), nrows_countries)
  impute_data <- list()
  for (i in 1:length(splits)){
    print(paste0("Iteration ", i, ": ", names(nrows_countries)[i]))
    bo <- 0
    while (bo != 10){
      suppressWarnings({
        imp <- try(data.frame(missForest(as.matrix(missdata[inds == i, ]))$ximp))
      })
      if (class(imp) == "try-error"){
        cat("Error in imputing data set")
        bo <- bo + 1
        print(paste0("Retrying for the ", bo, "nd time"))
      } else {
        impute_data[[i]] <- imp
        break
      } 
    }
  }
  final_impute_data <- rbind.fill(impute_data)
} else {
  final_impute_data <- missdata
}

# Combine non-imputed and imputed data
dummydata <- cbind(africadata[, chars], final_impute_data)
colnames(dummydata) <- gsub("/", ".", colnames(dummydata))
colnames(dummydata) <- gsub(" ", ".", colnames(dummydata))

# Make data frame with information summarised per region and add (S)HDI
reg_list <- list(
  country = dummydata$country, 
  fullCountry = dummydata$fullCountry, 
  ISO3 = dummydata$ISO3, 
  newRegion = dummydata$newRegion, 
  gender = dummydata$gender1, 
  year = dummydata$year
)
nat_list <- list(
  country = dummydata$country, 
  fullCountry = dummydata$fullCountry, 
  ISO3 = dummydata$ISO3, 
  gender = dummydata$gender1, 
  year = dummydata$year
)
agg_vars <- c("age", "children", "weight", "ageBirth", "ageFirstCohabMar", "sampleSize", "ageFirstSex", 
              "birthCohort", "year", "popSize", "countryWeight", "finalWeight", "SHIHD", "SHIHD_Education",                      
              "SHIHD_Health", "SHIHD_Income", "HIV", "sexRatio", "nationalChildlessnessAgeadjusted",                         
              "nationalChildlessness", "residencerural", "residenceurban", "typeChildlesschildren", 
              "typeChildlesscircumstantial", "typeChildlessinvoluntary", "typeChildlessundecided", "typeChildlessvoluntary",
              "polygamyExtensive0", "polygamyExtensive1", "polygamyExtensivenot.marr.coh.rel", "polygamyExtensiveuncertain", 
              "deliveryDoctor1", "deliveryNurse1", "polygamy1", "childless_dummy1", "maritaldiv.sep",
              "maritalmarr.coh.rel","maritalnevermarr", "maritalwid")
regframe <- aggregate(
  dummydata[, na.omit(match(agg_vars, colnames(dummydata)))], 
  by = reg_list, 
  FUN = function(x){mean(na.omit(x))}
)
natframe <- aggregate(
  dummydata[, na.omit(match(agg_vars, colnames(dummydata)))], 
  by = nat_list, 
  FUN = function(x){mean(na.omit(x))}
)

# Add SHIHD and HIV to national data (should not be means of regional data, but separate values)
splits <- split(natframe, list(natframe$fullCountry, natframe$year, natframe$gender, natframe$age))
splits <- splits[unlist(lapply(splits, nrow)) > 0]
natframe <- rbind.fill(lapply(splits, nationalShihdHiv, shihd, hiv_interpol, lagWomen, lagMen))

# Add variable to africaframe with percentage childlessness
regframe['percChildlessness'] <- 100 * regframe$childless_dummy1
natframe['percChildlessness'] <- 100 * natframe$childless_dummy1

# Add GDL codes to plotting data (to match with geographical data)
message('WARNING! Downloading Global Data Lab geographical data; note that these data may have changed since this code was written.')
gdlcodes <- read_excel('data_files/GDL_shapes/GDL-Codes.xlsx', sheet = 2, trim_ws = FALSE)
regframe["GDLcode"] <- NA
for (i in 1:nrow(regframe)){
  rows <- which(gdlcodes$iso_code == regframe$ISO3[i] & gdlcodes$region == regframe$newRegion[i])
  regframe$GDLcode[i] <- as.character(unique(gdlcodes$GDLCode[rows]))
}

# Load shapefile for plotting and keep only African countries in shapefile for better plot
countries_africa <- read_excel('data_files/ssa_country_codes.xlsx', sheet = 'GDL')
shapefile <- readOGR(dsn = "data_files/GDL_shapes", layer = "GDL-SHDI-SHP-2")
shape <- shapefile[shapefile$iso_code %in% countries_africa$ISO3, ]
shape <- fortify(shape, region = 'GDLCode')
regframe['ID'] <- shapefile@data$OBJECTID[match(regframe$GDLcode, shapefile@data$GDLCode)]

# Keep only most recent year per region
newff <- regframe %>%
  group_by(fullCountry, newRegion, gender) %>% 
  arrange(desc(year)) %>% 
  slice(1)

