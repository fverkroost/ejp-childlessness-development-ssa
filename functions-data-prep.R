# Remove surveys with only ever-married or currently married individuals
removeMarriedSamples <- function(x){
  ind <- which(grepl('020', names(x)))                  # column number of 020 variable
  mind <- which(grepl('501', names(x)))                 # column number of 501 variable
  if (length(ind) > 0){
    y <- ifelse(all(x[, ind] == "0"), 1, 0)                        # if not ever-married sample (all 0), then y = 1 (keep), else y = 0 (remove)
  } else if (length(mind) > 0 & !all(is.na(x[, mind]))) {
    tab <- table(x[, mind])                                        # table of values in marital status variable
    nevind <- which(grepl("0", tolower(names(tab))))               # 0 = Never in union
    y <- ifelse(tab[nevind] > 0, 1, 0)                             # if value 0 occurs, then y = 1 (keep), else y = 0 (remove)
  } else {
    y <- 1
  }
  return(y)
}

# Create date variable from 008
createDate <- function(z, n, i){
  y <- z[[i]]
  ind <- which(grepl('008', names(y)))
  y["date"] <- NA
  if (length(ind) > 0){
    # Obtain CMC code and corresponding year and month
    CMC <- y[, ind]
    if (substr(n[[i]], 1, 2) == "ET"){ CMC <- CMC + 92 }  # CMC is different for Ethiopian surveys
    year <- 1900 + floor((CMC-1)/12)
    month <- CMC - 12*(year - 1900)
    
    # If year contains "duplicated", provide corrected year variable
    if (any(grepl("duplicated", as.character(year)))){
      year.corr <- unique(na.omit(as.numeric(unlist(strsplit(as.character(year), "[^0-9]+")))))
      year <- ifelse(grepl("duplicated", as.character(year)), year.corr, year.corr - 1)
    }
    
    # Construct the date and stop if there are missing values in the date variable
    jointdate <- paste0("12/", as.character(month) ,"/", as.character(year))
    y$date <- format(as.Date(jointdate, format="%d/%m/%Y"), "%m-%Y")
    if (any(is.na(y$date))){ stop('NA in date') }
  }
  return(y)
}

# Create country variable from 000
countryFun <- function(z, n, i){
  y <- z[[i]]
  y["country"] <- NA
  y$country <- as.character(rep(substr(n[[i]], 1, 2), nrow(y)))
  return(y)
}

# Create age variable from 012
ageFun <- function(x){
  ind <- which(grepl('012', names(x)))
  x["age"] <- NA
  if (length(ind) > 0){
    x$age <- as.numeric(x[, ind])
  }
  return(x)
}

# Create gender variable from file name (IR or MR)
genderFun <- function(z, n, i){
  y <- z[[i]]
  y["gender"] <- NA
  y$gender <- ifelse(substr(n[[i]], 3, 3) == 'I', 0, 1)
  return(y)
}

# Create residence type variable from 102 or 025 (dependent on survey)
residenceFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  variable <- ifelse(letter == "MV" | letter == "V", paste0(letter, c("102")), paste0(letter, c("025")))
  ind <- which(names(x) == variable)
  x["residence"] <- NA
  if (length(ind) > 0){
    x$residence <- ifelse(x[, ind] == 1, "urban", ifelse(is.na(x[, ind]), NA, "rural"))
  }
  return(x)
}

# Create pregnancy variable from 213
pregnantFun <- function(x){
  ind <- which(grepl('213', names(x)))
  x["pregnant"] <- NA
  if (length(ind) > 0){
    x$pregnant <- ifelse(x[, ind] %in% c(0, 8), "no/unsure", ifelse(is.na(x[, ind]) | x[, ind] == 9, NA, "yes"))
  }
  return(x)
}

# Create number of children variable
childrenFun <- function(x){
  ind <- which(grepl('201', names(x)))
  x["children"] <- NA
  if (length(ind) > 0){
    x$children <- x[, ind]
  } 
  return(x)
}

# Make childlessness variable from children ever born variable (201)
# Classify those who have had a child that died as not childless
childlessEverBorn <- function(x){
  if ("children" %in% names(x)){
    ind <-  which(grepl('201', names(x)))
    x["childless_dummy"] <- ifelse(as.numeric(as.character(x[, ind])) > 0, 0,
                                   ifelse(as.numeric(as.character(x[, ind])) == 0, 1, NA))
  } 
  return(x)
}

# Make childlessness variable from children currently living variable (218)
# Classify those who have had a child that died as childless
childlessChildDied <- function(x){
  ind <- which(grepl('218', names(x)))
  if (length(ind) > 0){
    x["childless_dummy"] <- ifelse(as.numeric(as.character(x[, ind])) > 0, 0,
                                   ifelse(is.na(as.numeric(as.character(x[, ind]))), NA, 1))
  } 
  return(x)
}

# Create sample weights variable from 005
weightFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  if (any(letter == "MV" | letter == "V" | letter == "HV")){
    variable <- paste0(letter, c("005"))
    ind <- which(names(x) == variable)
    x["weight"] <- NA
    if (length(ind) > 0){
      x$weight <- as.numeric(x[, ind])
    }
  }
  return(x)
}

# Create fertility intentions variable from 602
fertilityFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  if (any(letter == "MV" | letter == "V")){
    variable <- paste0(letter, c("602"))
    ind <- which(names(x) == variable)
    x["fertPref"] <- NA
    if (length(ind) > 0){
      x$fertPref <- ifelse(x[, ind] == 1, "have another", 
                           ifelse(x[, ind] == 2, "undecided",
                                  ifelse(x[, ind] == 3, "no more",
                                         ifelse(x[, ind] == 4, "sterilized",
                                                ifelse(x[, ind] %in% c(5, 7), "infecund",
                                                       ifelse(x[, ind] == 6, "never sexed",
                                                              ifelse(x[, ind] == 8, "no partner", NA)))))))
    } 
  }
  return(x)
}

# Create desire for children variable from 605
desireFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  if (any(letter == "MV" | letter == "V")){
    variable <- paste0(letter, c("605"))
    ind <- which(names(x) == variable)
    x["desireOwn"] <- NA
    if (length(ind) > 0){
      x$desireOwn <- ifelse(x[, ind] %in% c(1, 2, 3), "more", 
                            ifelse(x[, ind] == 4, "undecided",
                                   ifelse(x[, ind] == 5, "no more",
                                          ifelse(x[, ind] == 6, "sterilized",
                                                 ifelse(x[, ind] == 7, "infecund",
                                                        ifelse(x[, ind] == 8, "never sexed", NA))))))
    }
  }
  return(x)
}

# Create ideal number of children variable from 613
idealFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  if (any(letter == "MV" | letter == "V")){
    variable <- paste0(letter, c("613"))
    ind <- which(names(x) == variable)
    x["idealChild"] <- NA
    if (length(ind) > 0){
      any_var <- c("God decision", "Up to God", "Up to God / Allah", "God's decision", "Depends on God", "As God wishes",
                   "God/Allah's will", "God's will", "God's plan /knows", "Any number", "As God wills", "Any, as God sends",
                   "Any number/fatalistic", "Any number/Up to God", "Up to God/Allah", "As God gives", "God's will/ choice",
                   "Fatalistic", "Up to god", "as god wills", "Fatalistic/Up to God", "Fatalistic/up to God",
                   "Upto God, as many as possible","Fatalistic/Any", "50 or more", "As much as possible", 
                   "As many as possible", "Plenty, lots", "20 +", "40+", "As can care for", as.character(seq(1, 100)))
      undec_var <- c("Can't decide / never thought about it", "Don't know", "DK", "Don't know", "Have not thought of", 
                     "Don t know,Undecided", "don t know", "Ne sait pas", "Don t know")
      x$idealChild <- ifelse(x[, ind] %in% any_var, "any/fatalistic",
                             ifelse(x[, ind] %in% c("Depends on husband"), "depends on partner",
                                    ifelse(x[, ind] %in% undec_var, "undecided",
                                           ifelse(x[, ind] %in% c("None", "Doesn't want childr.", "0"), "none", 
                                                  ifelse(x[, ind] %in% c("Non-numeric response", "Depends", "None of the above"), "other", NA)))))
      
    } 
  }
  return(x)
}

# Create age at first childbirth variable from 212
ageBirthFun <- function(z, n, i){
  x <- z[[i]]
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  if (any(letter == "MV" | letter == "V")){
    variable <- paste0(letter, c("212"))
    ind <- which(names(x) == variable)
    x["ageBirth"] <- NA
    if (length(ind) > 0){
      if (is.factor(x[, ind])){
        x$ageBirth <- as.numeric(levels(x[, ind]))[x[, ind]]
      } else {
        x$ageBirth <- as.numeric(x[, ind])
      }
    } 
  }
  if (any(grepl('b3', names(x))) | any(grepl('B3', names(x)))){
    ind <- which(grepl('b3', names(x)) | grepl('B3', names(x)))
    if (all(is.na(x$ageBirth)) | sum(is.na(x$ageBirth)) > sum(is.na(x[, ind]))){
      CMC <- x[, ind]
      if (substr(n[[i]], 1, 2) == "NP"){
        if (substr(n[[i]], 5, 5) == "3"){
          CMC <- CMC + 519
        } else {
          CMC <- CMC - 681
        }
      }
      if (substr(n[[i]], 1, 2) == "ET"){
        CMC <- CMC + 92
      }
      year <- 1900 + floor((CMC-1)/12)
      month <- CMC - 12*(year - 1900)
      if (any(grepl("duplicated", as.character(year)))){
        year.corr <- unique(na.omit(as.numeric(unlist(strsplit(as.character(year), "[^0-9]+")))))
        year <- ifelse(grepl("duplicated", as.character(year)), year.corr, year.corr - 1)
      }
      jointdate <- paste0("12/", month ,"/", year)
      x["dateBirthChild"] <- NA
      x$dateBirthChild <- format(as.Date(jointdate, format="%d/%m/%Y"), "%Y-%m")
      if (any(is.na(x$dateBirthChild))){
        if (n[[i]] == "COPR22FL"){
          year <- 1990
          month <- 12
          jointdate <- paste0("12/", month ,"/", year)
          x["dateBirthChild"] <- NA
          x$dateBirthChild <- format(as.Date(jointdate, format="%d/%m/%Y"), "%Y-%m")
        } 
      }
      x['ageBirth'] <- x['dateBirthResp'] <- NA
      CMC <- x[, which(grepl('011', names(x)))]
      if (substr(n[[i]], 1, 2) == "ET"){
        CMC <- CMC + 92
      }
      year <- 1900 + floor((CMC-1)/12)
      month <- CMC - 12*(year - 1900)
      if (any(grepl("duplicated", as.character(year)))){
        year.corr <- unique(na.omit(as.numeric(unlist(strsplit(as.character(year), "[^0-9]+")))))
        year <- ifelse(grepl("duplicated", as.character(year)), year.corr, year.corr - 1)
      }
      jointdate <- paste0("12/", month ,"/", year)
      x["dateBirthResp"] <- NA
      x$dateBirthResp <- format(as.Date(jointdate, format="%d/%m/%Y"), "%Y-%m")
      monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
      lt$year*12 + lt$mon} 
      mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
      x$dateBirthChild <- ifelse(is.na(x$dateBirthChild), NA, paste0(x$dateBirthChild, '-01'))
      x$ageBirth <- round(-mondf(as.Date(x$dateBirthChild), as.Date(paste0(x$dateBirthResp, '-01'))) / 12)
    }
  }
  return(x)
}

# Create age at first marriage/cohabitation
ageCohabMar <- function(x){
  ind <- which(grepl('511', names(x)))
  x['ageFirstCohabMar'] <- NA
  if (length(ind) > 0){
    v <- as.character(x[, ind])
    x$ageFirstCohabMar <- ifelse(v %in% c('not consummated', 'Marr. not consum.'), 'not consummated',
                                 ifelse(v %in% c(as.character(seq(1, 7)), 'less than 8'), 'less than 8', v))
  } 
  return(x)
}

# Create age at first sex variable
ageFirstSex <- function(x){
  ind <- which(grepl('531', names(x)))
  x['ageFirstSex'] <- NA
  if (length(ind) > 0){
    v <- as.character(x[, ind])
    x$ageFirstSex <- ifelse(v %in% c('Not had sex', 'Not had intercourse', 'not had sex'), 'not had sex',
                            ifelse(v %in% c("Don't know", 'Refused to respond', 'Inconsistent', 'inconsistent'), 'other', 
                                   ifelse(v > 60, NA, v)))
  } 
  return(x)
}

# Create variable for assistance during delivery (nurse, doctor)
assistanceDelivery <- function(x){
  ind1 <- which(grepl('M3B', names(x)))
  ind2 <- which(grepl('M3A', names(x)))
  x['deliveryNurse'] <- x['deliveryDoctor'] <- NA
  if (length(ind1) > 0){
    x$deliveryNurse <- ifelse(as.character(x[, ind1]) == "1", 1, ifelse(as.character(x[, ind1]) == "0", 0, NA))
  }
  if (length(ind2) > 0){
    x$deliveryDoctor <- ifelse(as.character(x[, ind2]) == "1", 1, ifelse(as.character(x[, ind2]) == "0", 0, NA))
  }
  return(x)
}

# Make polygamy variable from 505
polygamyFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  ind <- which(grepl("505", names(x)) & !grepl("505A", names(x)) & !grepl("505B", names(x))) 
  x["polygamy"] <- NA
  if (length(ind) > 0){
    if (sum(is.na(x[, ind])) == nrow(x)){
      if (paste0(letter, c("505A")) %in% names(x)){
        value <- as.character(x[, which(names(x) == paste0(letter, c("505A")))])
        if (letter == "V"){
          x$polygamy <- ifelse(value %in% c("Don't know", "DK if oth wives exst","DK if other wives",
                                            "Don t know","DK","don't know", 96:98), "uncertain", 
                               ifelse(value %in% c("0","No other wives","no other wives"), "0",
                                      ifelse(value %in% c(as.character(seq(1,30,1)), "1 wife", "2 wives", "Has other wives", "1 or more",
                                                          "3+ wives", "Other wives/no qty", 93, "1 or more","5+"), "1", NA)))
        } else {
          x$polygamy <- ifelse(value %in% c("Don't know", "DK if oth wives exst","DK if other wives",
                                            "Don t know","DK","don't know", 96:98), "uncertain", 
                               ifelse(value %in% c("0","No other wives","no other wives", "No wives", "1", "1 wife"), "0",
                                      ifelse(value %in% c(as.character(seq(2,30,1)), "2 wives", "3+ wives", "Has other wives",
                                                          "Other wives/no qty", 93, "5+"), "1", NA)))
        }
      } else {
        x["polygamy"] <- rep("all NA", nrow(x))
      }
    } else {
      value <- as.character(x[, ind])
      if (letter == "V"){
        x$polygamy <- ifelse(value %in% c("Don't know", "DK if oth wives exst","DK if other wives",
                                          "Don t know","DK","don't know", 96:98), "uncertain", 
                             ifelse(value %in% c("0","No other wives","no other wives"), "0",
                                    ifelse(value %in% c(as.character(seq(1,30,1)), "1 wife", "2 wives", "Has other wives", "1 or more",
                                                        "3+ wives", "Other wives/no qty", 93, "1 or more","5+"), "1", NA)))
      } else {
        x$polygamy <- ifelse(value %in% c("Don't know", "DK if oth wives exst","DK if other wives",
                                          "Don t know","DK","don't know", 96:98), "uncertain", 
                             ifelse(value %in% c("0","No other wives","no other wives", "1", "1 wife", "No wives"), "0",
                                    ifelse(value %in% c(as.character(seq(2,30,1)), "2 wives", "3+ wives", "Has other wives",
                                                        "Other wives/no qty", 93, "5+"), "1", NA)))
      }
    }
  } else {
    x["polygamy"] <- rep("variable not present", nrow(x))
  }
  return(x)
}

# Make marital status variable from 501 and 502
maritalFun <- function(x){
  letter <- unique(sub("[^[:alpha:]]+", "", names(x)[3:5]))
  variable <- paste0(letter, c("501", "502"))
  if (letter == "V" | letter == "MV"){
    if (variable[1] %in% names(x)) {
      x["marital_stat"] <- NA
      ind <- which(names(x) == variable[1])
      x$marital_stat <- ifelse(x[, ind] %in% c(1, 2, 6), "married/cohabiting/relationship", 
                               ifelse(x[, ind] == 3, "widowed",
                                      ifelse(x[, ind] %in% c(4, 5), "divorced/separated", 
                                             ifelse(x[, ind] == 0, "never married", NA))))
    }
    if (variable[2] %in% names(x)) {
      x["marital_now"] <- NA
      ind <- which(names(x) == variable[2])
      x$marital_now <- ifelse(x[, ind] == 0, "never married",
                              ifelse(x[, ind] == 1, "currently married/cohabiting",
                                     ifelse(x[, ind] == 2, "formerly married/cohabiting", NA)))
    }
  }
  return(x)
}

# Combine information from different marital status variables
maritalCombinations <- function(x){
  ind1 <- which(names(x) == 'marital_now')
  ind2 <- which(names(x) == 'marital_stat')
  if (length(ind1) > 0 & length(ind2) > 0){
    df <- unique(x[, c(ind1, ind2)])
  } else if (length(ind1) > 0 & !(length(ind2) > 0)){
    df <- data.frame(matrix(NA, ncol = 2, nrow = length(unique(x[, ind1]))))
    df[, 1] <- unique(x[, ind1])
    df[, 2] <- rep(NA, nrow(df))
  } else if (length(ind2) > 0 & !(length(ind1) > 0)){
    df <- data.frame(matrix(NA, ncol = 2, nrow = length(unique(x[, ind2]))))
    df[, 2] <- unique(x[, ind2])
    df[, 1] <- rep(NA, nrow(df))
  } else if (!(length(ind1) > 0) & !(length(ind2) > 0)){
    df <- data.frame(matrix(NA, nrow = 1, ncol = 2))
  }
  names(df) <- c('marital_now', 'marital_stat')
  return(df)
}

# Combine marital status and current marital status into one variable
maritalStatus <- function(x){
  ind1 <- which(names(x) == 'marital_now')
  ind2 <- which(names(x) == 'marital_stat')
  x['marital'] <- NA
  if (length(ind1) > 0 & length(ind2) > 0){
    x$marital <- ifelse(x[, ind1] == 'currently married/cohabiting' & x[, ind2] == 'married/cohabiting/relationship', 'marr/coh/rel',
                        ifelse(is.na(x[, ind1]) & is.na(x[, ind2]), NA,
                               ifelse(x[, ind1] == 'never married' & x[, ind2] %in% c(NA, 'never married'), 'nevermarr',
                                      ifelse(x[, ind1] == 'formerly married/cohabiting' & x[, ind2] %in% c('divorced/separated'), 'div/sep',
                                             ifelse(x[, ind1] =='formerly married/cohabiting' & x[, ind2] == 'widowed', 'wid', NA)))))
  }
  return(x)
}

# Create birth cohort variable from 010
birthCohortfun <- function(x){
  ind <- which(grepl("010", names(x)))
  x["birthCohort"] <- value <- NA
  if (length(ind) > 0){
    value <- ifelse(is.numeric(x[, ind]), x[, ind],
                    ifelse(is.factor(x[, ind]), as.numeric(levels(x[, ind]))[x[, ind]], as.character(x[, ind])))
  }
  x$birthCohort <- as.numeric(ifelse(nchar(value) == 2, paste0("19", value), 
                                     ifelse(nchar(value) == 1, paste0("190", value), value)))
  return(x)
}

# Make ID variable from (M)CASEID
caseID <- function(x){
  x["id"] <- as.character(x[, which(names(x) %in% c("CASEID", "MCASEID"))])
  return(x)
}

# Harmonize regions in DHS data according to GDL data
regionFun <- function(z, n, i, matchdf){
  y <- z[[i]]
  submatch <- matchdf[substr(matchdf$Survey, 1, 6) == n[[i]], ]
  submatch <- submatch[rowSums(is.na(submatch)) != ncol(submatch), ]
  y["region"] <- y["newRegion"] <- NA
  if (!all(is.na(submatch)) & !nrow(submatch) == 0){
    
    # If no province variable exists/is needed, use the region variable
    matchvar <- ifelse(!all(is.na(submatch$Province_VAR)), "Province_VAR", "Region_VAR")
    
    # Get region/province variable name in DHS data
    regvar <- unique(submatch[, matchvar])
    
    # Create data frame with region/province names and codes (labelled vector)
    labval <- val_labels(y[, regvar])
    labdf <- data.frame(name = names(labval), code = labval)
    
    # Match to get region/province variable (labels) in DHS data
    y$region <- datavar <- as.character(labdf$name[match(y[, regvar], labdf$code)])
    
    # Match to get GDL region
    y$newRegion <- submatch$Region_GDL[match(datavar, submatch[, gsub("VAR", "DHS", matchvar)])]
    
  } else {
    # If we cannot match the DHS and GDL regions, we use the original DHS region
    regvar <- unique(submatch[, "Region_VAR"]) 
    labval <- val_labels(y[, regvar])
    labdf <- data.frame(name = names(labval), code = labval)
    y$region <- as.character(labdf$name[match(y[, regvar], labdf$code)])
  }
  return(y)
}

# Make a year variable and make sure all years for a particular survey are the same
sameDate <- function(x){
  dates <- unique(substr(x$date, 4, 7))
  dates <- dates[order(dates)]
  x["year"] <- rep(as.numeric(as.character(dates[1])), nrow(x))
  return(x)
}

# Fill up data frames with empty columns to match for merging
addEmptyColumns <- function(x, vars){
  missing_vars <- vars[!(vars %in% names(x))]
  missing_df <- data.frame(matrix(NA, nrow = nrow(x), ncol = length(missing_vars)))
  names(missing_df) <- missing_vars
  res <- cbind(x, missing_df)
  return(res)
}

# Remove outliers in number of children and age
outlierFun <- function(x){
  if ("children" %in% names(x) & "gender" %in% names(x)) {
    index <- which(x$children > (x$age - 12) & x$gender == 0)
    if (length(index) > 0) {
      x <- x[-index,]
    }
  }
  return(x)
}

# Round to the nearest decimal (base)
mround <- function(x, base){ 
  m <- base*round(x/base) 
  return(m)
} 

# Make table of ages with first childbirth and childlessness proportion
ageTableFun <- function(data){
  n <- max(data$age)
  tabs1 <- table(data$age)
  tabs2 <- table(data$age, data$childless_dummy)
  tabs3 <- table(data$ageBirth)
  tabs3 <- tabs3[which(names(tabs3) < 90)]
  ageTable <- data.frame(matrix(NA, nrow = n, ncol = 6))
  names(ageTable) <- c("Age", "N_total", "N_childless", "propChildless", "N_birth", "propAgeBirth")
  ageTable$Age <- seq(1, n)
  ageTable$N_total[match(names(tabs1), as.character(seq(1, n)))] <- tabs1
  ageTable$N_childless[match(rownames(tabs2), as.character(seq(1, n)))] <- tabs2[, 2]
  ageTable$propChildless[match(rownames(tabs2), as.character(seq(1, n)))] <- tabs2[, 2] / rowSums(tabs2) * 100
  ageTable$N_birth[match(names(tabs3), as.character(seq(1, n)))] <- tabs3
  ageTable$propAgeBirth[match(names(tabs3), as.character(seq(1, n)))] <- tabs3 / sum(tabs3) * 100
  return(ageTable)
}

# Make plots with age at first childbirth and childlessness distributions
distrBirthAgeChildless <- function(mydata, path){
  dfwomen <- ageTableFun(data = split(mydata, f = mydata$gender)$`0`)
  dfwomen["Gender"] <- rep("Women", nrow(dfwomen))
  dfmen <- ageTableFun(data = split(mydata, f = mydata$gender)$`1`)
  dfmen["Gender"] <- rep("Men", nrow(dfmen))
  df <- rbind(dfwomen, dfmen)
  plotData <- reshape2::melt(df[, which(names(df) %in% c("propChildless", "propAgeBirth", "Age", "Gender"))], id.vars = c("Age", "Gender"))
  
  # Plot age at first birth and childlessness
  g <- ggplot() + 
    labs(x = "Age", y = "Percentage") +
    scale_x_continuous(breaks = seq(0, mround(max(plotData$Age), 5), 5)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          strip.background = element_rect(fill = "white", colour = "black"),
          legend.position = c(1, 1), 
          legend.justification = c(1, 1), 
          legend.background = element_rect(fill = "white", colour = NA),
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    guides(linetype=guide_legend(override.aes=list(fill=NA))) +
    scale_linetype_discrete(labels = c("Childless", "Age at First Birth")) +
    facet_wrap(~ Gender)
  
  gc <- g + geom_line(data = plotData, aes(x = Age, y = value, colour = variable, linetype = variable)) 
  gc <- gc + scale_color_discrete(labels = c("Childless", "Age at First Birth"))
  # ggsave(paste0(path, 'ageDistr.png'), dpi = 600,  gc, height = 16.933, width = 21.768, units = "cm")
  gc <- gc + scale_x_continuous(limits = c(0, 50))
  ggsave(paste0(path, 'ageDistrLim50.png'), dpi = 600,  gc, height = 16.933, width = 21.768, units = "cm")
  
}

# Make childlessness classification according to decision tree
childlessTree <- function(africadata, indices){
  for (i in 1:nrow(africadata[inds,])){
    if (is.na(africadata$fertPref[inds[i]]) & is.na(africadata$idealChild[inds[i]]) & is.na(africadata$marital[inds[i]])){
      africadata$typeChildless[inds[i]] <- NA
    } else {
      if (is.na(africadata$fertPref[inds[i]]) & is.na(africadata$idealChild[inds[i]]) & !(is.na(africadata$marital[inds[i]]))){
        if (africadata$marital[inds[i]] == "marr/coh/rel"){
          africadata$typeChildless[inds[i]] <- "involuntary"
        } else {
          africadata$typeChildless[inds[i]] <- "circumstantial"
        }
      } else {
        if (africadata$fertPref[inds[i]] != "infecund" | is.na(africadata$fertPref[inds[i]])){
          if (africadata$idealChild[inds[i]] == "any/fatalistic" | is.na(africadata$idealChild[inds[i]])){
            if (africadata$marital[inds[i]] != "nevermarr" | is.na(africadata$marital[inds[i]])){
              if (africadata$marital[inds[i]] == "marr/coh/rel" | is.na(africadata$marital[inds[i]])){
                if (is.na(africadata$fertPref[inds[i]])){
                  africadata$typeChildless[inds[i]] <- "involuntary"
                } else if (africadata$fertPref[inds[i]] == "question not asked"){
                  africadata$typeChildless[inds[i]] <- "undetermined"
                } else if (africadata$fertPref[inds[i]] == "have another"){
                  africadata$typeChildless[inds[i]] <- "involuntary"
                } else if (africadata$fertPref[inds[i]] == "no more"){
                  africadata$typeChildless[inds[i]] <- "voluntary"
                } else if (africadata$fertPref[inds[i]] == "undecided"){
                  africadata$typeChildless[inds[i]] <- "undecided"
                } else if (africadata$fertPref[inds[i]] %in% c("no partner", "never sexed")){
                  africadata$typeChildless[inds[i]] <- "circumstantial"
                } else if (africadata$fertPref[inds[i]] == "sterilized"){
                  africadata$typeChildless[inds[i]] <- "involuntary"
                }
              } else {
                africadata$typeChildless[inds[i]] <- "circumstantial"
              }
            } else {
              africadata$typeChildless[inds[i]] <- "circumstantial"
            }
          } else if (africadata$idealChild[inds[i]] == "question not asked"){
            africadata$typeChildless[inds[i]] <- "undetermined"
          } else if (africadata$idealChild[inds[i]] %in% c("other", "depends on partner", "undecided")){
            africadata$typeChildless[inds[i]] <- "undecided"
          } else if (africadata$idealChild[inds[i]] == "none"){
            africadata$typeChildless[inds[i]] <- "voluntary"
          }
        } else {
          africadata$typeChildless[inds[i]] <- "involuntary"
        }
      }
    }
  }
  return(africadata)
}

# Add population size and weights to africadata
popWeighting <- function(x, malePop, femalePop){
  year <- unique(x$year)
  gender <- unique(x$gender)
  country <- unique(x$fullCountry)
  age <- unique(x$ageGroup)
  
  # Choose the correct data set
  if (gender == 0) {
    data <- femalePop
  } else {
    data <- malePop
  }
  
  # Perform weighting
  row <- which(data$`Region, subregion, country or area *` == country & data$`Reference date (as of 1 July)` == year)
  col <- which(names(data) == age)
  if (length(row) > 0 & length(col) > 0){
    val <- as.numeric(unlist(data[row, col]))
  } else {
    val <- NA
  }
  x["popSize"] <- rep(val, nrow(x))
  x["countryWeight"] <- x$popSize / x$sampleSize
  x["finalWeight"] <- x$weight / 1e6 * x$countryWeight
  
  return(x)
}

# Define own theme for plotting
my_theme <- function () { 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size = 14),
                     strip.text = element_text(size = 14),
                     strip.background = element_rect(fill = "white", colour = "black"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom",
                     legend.text = element_text(size = 14))
}

# Plot region-country SHDI ratios over time per indicator as proof for assumption of stability
plotCountryRegionRatioSHDI <- function(x, path){
  plot_sub <- gather(x, "Year", "Value", -Country, -ISO_Code, -Level, -GDLCODE, -Region, -Indicator, -Country_DHS)
  plot_sub$Year <- round(as.numeric(sub("X", "", plot_sub$Year)))
  plot_sub$Region <- gsub("\\s*\\([^\\)]+\\)", "", as.character(plot_sub$Region))
  plot_sub <- plot_sub[!is.na(plot_sub$Value), ]
  
  g <- ggplot()  +
    geom_line(data = plot_sub, aes(x = Year, y = Value, colour = Region, group = Region)) +
    my_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Indicator) +
    labs(x = NULL, colour = NULL, group = NULL, y = "Value") +
    ggtitle(unique(plot_sub$Country_DHS)) +
    scale_x_continuous(breaks = seq(min(plot_sub$Year), max(plot_sub$Year), 5))
  if (!dir.exists(paste0(path, 'trendsHDI/SHDIperCountry'))){
    dir.create(paste0(path, 'trendsHDI/SHDIperCountry'), recursive = TRUE)
  }
  ggsave(paste0(path, 'trendsHDI/SHDIperCountry/', gsub(" ", "", unique(plot_sub$Country_DHS)), '.png'), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")
}

# Interpolate SHDI data to get data for every year
interpolateSHDI <- function(x){
  y <- data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
  names(y) <- names(x)
  samevars <- c("Country", "ISO_Code", "Level", "GDLCODE", "Region", "Country_DHS", "Year")
  y[, samevars] <- x[, samevars]
  nams <- names(y)[!(names(y) %in% samevars)]
  for (i in 1:length(nams)){
    if (!all(is.na(x[, nams[i]]))){
      minyear <- min(x$Year[!is.na(x[, nams[i]])])
      maxyear <- max(x$Year[!is.na(x[, nams[i]])])
      minind <- which(x$Year == minyear)
      maxind <- which(x$Year == maxyear)
      y[c(minind:maxind), nams[i]] <- data.frame(approx(x$Year, x[, nams[i]], xout = seq(minyear, maxyear),
                                                        rule = 2, method = "linear", ties = mean))[, 2]
    } else {
      y[, nams[i]] <- NA
    }
  }
  return(y)
}

# Plot HIHD components over time per indicator
plotComponentsHIHD <- function(x, path){
  plot_sub <- gather(x, "Component", "Value", -fullCountry, -Year)
  plot_sub$Year <- round(as.numeric(plot_sub$Year))
  g <- ggplot()  +
    geom_line(data = plot_sub, aes(x = Year, y = Value, colour = Component, group = Component)) +
    my_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(unique(plot_sub$fullCountry)) +
    scale_x_continuous(breaks = seq(min(plot_sub$Year), max(plot_sub$Year), 10))
  if (!dir.exists(paste0(path, 'trendsHDI/HIHDperCountry'))){
    dir.create(paste0(path, 'trendsHDI/HIHDperCountry'), recursive = TRUE)
  }
  ggsave(paste0(path, 'trendsHDI/HIHDperCountry/', gsub(" ", "", unique(plot_sub$fullCountry)), '.png'), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")
}

# Interpolate HIHD data to get data for every year
interpolateHIHD <- function(x, range){
  y <- data.frame(matrix(NA, nrow = length(range), ncol = ncol(x)))
  names(y) <- names(x)
  y$Year <- range
  y$fullCountry <- rep(unique(x$fullCountry), nrow(y))
  nams <- names(y)[!(names(y) %in% c("Year", "fullCountry"))]
  if (!all(is.na(x[, nams]))){
    for (i in 1:length(nams)){
      y[, nams[i]] <- data.frame(approx(x$Year, x[, nams[i]], xout = range, 
                                        rule = 2, method = "linear", ties = mean))[, 2]
    }
  } else {
    y[, nams] <- NA
  }
  return(y)
}

# Add SHIHD to africadata (lagged by 20 years if necessary)
addSHIHD <- function(x, lagWomen, lagMen, shihd){
  country <- unique(x$fullCountry)
  region <- unique(x$newRegion)
  year <- unique(x$year)
  sex_lag <- ifelse(unique(x$gender) == "0", lagWomen, ifelse(unique(x$gender) == "1", lagMen, NA))
  row <- which(shihd$Year == (year - sex_lag) & shihd$Region == region & shihd$Country == country)
  vars <- paste0("SHIHD", c("", "_Education", "_Health", "_Income"))
  if (length(row) > 0){
    x[vars] <- shihd[row, vars]
  } else {
    x[vars] <- NA
  }
  return(x)
}

# Capitalize first letter of string
# Source: https://github.com/harrelfe/Hmisc/blob/master/R/capitalize.s
capitalize <- function(string) {
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

# Interpolate HIV data exponentially from 1970 onwards to data (usually available from 1990)
interpolateHIV <- function(x){
  
  if (!(all(is.na(x$Value)))){
    
    x <- aggregate(Value ~ Region + Country + Gender + Year, data = x, FUN = mean)
    minyear <- min(x$Year[!is.na(x$Value)])
    y <- log1p(c(0, x$Value[x$Year == minyear]))
    xvar <- as.numeric(c(1970, minyear))
    fit <- lm(y ~ xvar)
    pred <- predict(fit, data.frame(xvar = seq(1970, minyear)))
    pred <- exp(pred) - 1
    df <- data.frame(Year = seq(1970, minyear), Value = pred)
    df[c("Region", "Country", "Gender")] <- x[1, c("Region", "Country", "Gender")]
    nul <- data.frame(Year = seq(1900, 1969), Value = rep(0, length(seq(1900, 1969))))
    nul[c("Region", "Country", "Gender")] <- x[1, c("Region", "Country", "Gender")]
    y <- rbind(nul, df, x)
    
  } else {
    y <- data.frame(matrix(NA, nrow = 0, ncol = ncol(x)))
    names(y) <- c("Year", "Country", "Gender", "Year", "Value")
  }
  
  return(y)
}

# Add SHIHD to africadata (lagged by 20 years if necessary)
addHIV <- function(x, lagWomen, lagMen, hiv){
  country <- unique(x$fullCountry)
  region <- unique(x$newRegion)
  year <- unique(x$year)
  gender <- ifelse(unique(x$gender) == "0", "Women", ifelse(unique(x$gender) == "1", "Men", NA))
  sex_lag <- ifelse(unique(x$gender) == "0", lagWomen, ifelse(unique(x$gender) == "1", lagMen, NA))
  row <- which(hiv$Year == (year - sex_lag) & hiv$Region == region & hiv$Country == country & hiv$Gender == gender)
  if (length(row) <= 0){
    row <- which(hiv$Year == (year - sex_lag) & hiv$Country == country & hiv$Gender == gender & hiv$Region == 'Total')
  } 
  x["HIV"] <- mean(na.omit(hiv$Value[row])) / 100
  return(x)
}

# Add sex ratios on national level to africadata
addSexRatios <- function(x, femalePop, malePop, lagWomen, lagMen){
  
  # Specify values to search population data upon
  year <- unique(x$year)
  age <- unique(x$age)
  country <- unique(x$fullCountry)
  sex_lag <- ifelse(unique(x$gender) == "0", lagWomen, ifelse(unique(x$gender) == "1", lagMen, NA))
  
  # Find correct value corresponding to required age and country and gender
  targetyear <- year - (age - sex_lag)
  row_men <- which(malePop$`Region, subregion, country or area *` == country & malePop$`Reference date (as of 1 July)` == targetyear)
  row_women <- which(femalePop$`Region, subregion, country or area *` == country & femalePop$`Reference date (as of 1 July)` == targetyear)
  col_men <- which(grepl("15", names(malePop)) | grepl("24", names(malePop)))
  col_women <- which(grepl("15", names(femalePop)) | grepl("24", names(femalePop)))
  
  # Calculate number of men and women
  n_men <- sum(apply(malePop[row_men, col_men], 1, as.numeric))
  n_women <- sum(apply(femalePop[row_women, col_women], 1, as.numeric))
  
  # Number of women per 100 men
  x["sexRatio"] <- 100 / n_men * n_women # n_men / n_women
  
  return(x)
  
}

# Age adjusted childlessness levels
ageAdjust <- function(x, femalePop, malePop){
  
  # Choose the correct data
  if (unique(x$gender) == "0"){
    data <- femalePop
  } else {
    data <- malePop
  }
  nams <- c("AgeGroup", "Population", "Childless", "Standard")
  y <- data.frame(matrix(NA, nrow = length(unique(x$ageGroup)), ncol = length(nams)))
  colnames(y) <- nams
  y$AgeGroup <- unique(x$ageGroup)
  
  # Standard (population size sub-Saharan Africa)
  row <- which(data$`Region, subregion, country or area *` == "SUB-SAHARAN AFRICA" & 
                 data$`Reference date (as of 1 July)` == unique(x$year))
  cols <- match(unique(x$ageGroup), names(data))
  y$Standard <- as.numeric(t(data.frame(data[row, cols])))
  
  # Population (number of people in the sample)
  for (i in 1:nrow(y)){
    y$Population[i] <- length(which(x$ageGroup == y$AgeGroup[i]))
    y$Childless[i] <- length(which(x$ageGroup == y$AgeGroup[i] & x$childless_dummy == "1"))
  }
  
  # Add new information to data
  ageadj <- ageadjust.direct(y$Childless, y$Population, rate = NULL, y$Standard, conf.level = 0.95)
  df <- x
  df["nationalChildlessness"] <- ageadj[1]
  df["nationalChildlessnessAgeadjusted"] <- ageadj[2]
  
  return(df)
}

# Add SHIHD and HIV to national data (should not be means of regional data, but separate values)
nationalShihdHiv <- function(x, shihd, hiv, lagWomen, lagMen){
  y <- x
  sex_lag <- ifelse(unique(x$gender) == "0", lagWomen, ifelse(unique(x$gender) == "1", lagMen, NA))
  target_year <- round(unique(x$year) - (unique(x$age) - sex_lag))
  row_shihd <- which(shihd$Year == target_year & shihd$Country == unique(x$fullCountry) & shihd$Region == "Total")
  cols_shihd <- match(paste0("SHIHD", c("", "_Education", '_Income', "_Health")), colnames(shihd))
  cols_shihd_x <- match(paste0("SHIHD", c("", "_Education", '_Income', "_Health")), colnames(x))
  y[, cols_shihd_x] <- shihd[row_shihd, cols_shihd]
  sexe <- ifelse(unique(x$gender) == "0", "Women", ifelse(unique(x$gender) == "1", "Men", NA))
  row_hiv <- which(hiv$Year == target_year & hiv$Country == unique(x$fullCountry) & 
                     hiv$Region == "Total" & hiv$Gender == sexe)
  cols_hiv <- match("Value", colnames(hiv))
  cols_hiv_x <- match("HIV", colnames(x))
  y[, cols_hiv_x] <- mean(na.omit(hiv[row_hiv, cols_hiv]))
  return(y)
}
