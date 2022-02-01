dev.new(height = 4000, width = 5142)
source('functions-multilevel-model.R')

# Number of unique surveys available
length(unique(africadata$survey))

# Calculate per gender the average sample size for region-year combinations
avgobs <- data.frame(africadata %>% 
                       dplyr::group_by(fullCountry, year, newRegion, gender) %>%
                       dplyr::summarise(N = n()) %>%
                       dplyr::ungroup() %>%
                       dplyr::group_by(gender) %>%
                       dplyr::summarise(avg = mean(N)))
output <- print(xtable(avgobs, digits = rep(0, ncol(avgobs) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
fileConn <- file(paste0(path, "averageObservations.txt"))
writeLines(output, fileConn)
close(fileConn)

# Recover doctor and nurse delivery for males from female data -- regional
# Recover polygamy for females from male data (if missing in female data)
splits <- split(regframe, list(regframe$fullCountry, regframe$newRegion, regframe$year))
rows <- unlist(lapply(splits, nrow))
regframe <- rbind(rbind.fill(lapply(splits[rows == 2], recoverVariables)), rbind.fill(splits[rows == 1 | rows > 2]))

# Create time variable
regframe['time'] <- regframe$year - min(regframe$year)

# Plot missingness on the regional and individual levels
my_nams <- c("Age", "Age First Birth", "Age First Marriage", "Age First Sex", "HIV", "Sex Ratio",
             "SHIHD", "Education", "Health", "Income", "Residence", "Type Childlessness", 
             "Polygamy", 'Childlessness', "Marital Status") 
plotMissingness(data = regframe, 
                level = "regional", 
                nams = my_nams,
                vars = c("age", "ageBirth", "ageFirstCohabMar", "ageFirstSex", "HIV", "sexRatio",
                         paste0("SHIHD", c("", "_Education", "_Health", "_Income")), "residenceurban",
                         "typeChildlessvoluntary", "polygamy1", 'childless_dummy1', "maritaldiv.sep")
) 
plotMissingness(data = africadata, 
                level = "individual", 
                nams = my_nams,
                vars = c("age", "ageBirth", "ageFirstCohabMar", "ageFirstSex", "HIV", "sexRatio",
                         paste0("SHIHD", c("", "_Education", "_Health", "_Income")), "residence",
                         "typeChildless", "polygamy", 'childless_dummy', "marital")
) 

# Descriptive statistics of non-imputed data aggregated from individual imputed data
descvars <- c("ageBirth", "ageFirstCohabMar", "ageFirstSex", "HIV", "sexRatio",
              paste0("SHIHD", c("", "_Education", "_Health", "_Income")), "residenceurban",
              paste0("typeChildless", c("voluntary", "involuntary", "undecided", "circumstantial")), 
              "polygamy1", "deliveryDoctor1", "deliveryNurse1", 'childless_dummy1', "year", "age",
              paste0("marital", c("div.sep", "marr.coh.rel", "nevermarr", "wid")))
nams <- c("Age at first birth", "Age at first marriage/cohabitation", "Age at first sex", "HIV prevalence (%)", "Sex ratio (number of women per 100 men)",
          paste0("SHIHD - ", c("Overall", "Education", "Health", "Income")), "Urban residence (%)",
          paste0("Childlessness - ", c("Voluntary (%)", "Involuntary (%)", "Undecided (%)", "Circumstantial (%)")), 
          "Polygamy prevalence (%)", "Delivery assisted by doctor (%)", "Delivery assisted by nurse (%)", 
          'Childlessness - Overall (%)', "Year", "Age", paste0("Marital Status - ", c("Divorced/separated (%)", 
                                                                                      "Married/cohabiting/relationship (%)", "Never married (%)", "Widowed (%)")))
descriptiveStatistics(africadata, regframe, descvars, nams)

# Impute regional data
impvars <- c('gender', 'time', 'age', 'residenceurban', paste0("marital", c("div.sep", "nevermarr", "wid")),
             "ageBirth", "ageFirstCohabMar", "ageFirstSex", "HIV", "sexRatio", "polygamy1",
             'deliveryDoctor1', 'deliveryNurse1', "SHIHD", "fullCountry", "newRegion", "childless_dummy1",
             paste0("SHIHD_", c("Education", "Health", "Income")))
wframe <- regframe[regframe$gender == '0', impvars]
mframe <- regframe[regframe$gender == '1', impvars]
m_women <- round(length(which(complete.cases(wframe) == FALSE)) / nrow(wframe) * 100)
m_men <- round(length(which(complete.cases(mframe) == FALSE)) / nrow(mframe) * 100)
impWomen <- mice::mice(data = wframe, m = m_women, seed = 1234)
impMen <- mice::mice(data = mframe, m = m_men, seed = 1234)

# Plot regional-level scatter plots for imputed data
# scatterChildlessnessSHIHD(
#   append(mice::complete(impWomen, "all"), mice::complete(impMen, "all")), 
#   "childless_dummy1", 
#   path, 
#   compareModels = "anova", 
#   anova_sig = .05, 
#   diffAIC = NULL
# )

# Define classes for models
age_res <- c('age', 'residenceurban')
mar_nupt <- c(paste0('marital', c('nevermarr', 'div.sep', 'wid')), paste0('age', c('Birth', 'FirstCohabMar', 'FirstSex')))
poly_del <- c('polygamy1') 

# Fit models to all imputed data sets and pool estimates
# Singular fit warnings for men are for quadratic models
# and for quadratic random slopes model which is not used further in the analysis
# This is normal as these models are not parsimonious for men (they are for women)
# We will only use the linear models for men because these fit better
fitWomenG <- multilevelModelling(
  imp = impWomen, 
  depvar = 'childless_dummy1', 
  ageres = age_res, 
  marnupt = mar_nupt, 
  polydel = poly_del,
  HIV = HIV, 
  sexRatios = sexRatios, 
  gender = 'women'
)
fitMenG <- multilevelModelling(
  imp = impMen, 
  depvar = 'childless_dummy1', 
  ageres = age_res, 
  marnupt = mar_nupt, 
  polydel = poly_del,
  HIV = HIV, 
  sexRatios = sexRatios, 
  gender = 'men'
)

# Correlation plots of independent variables
matchdf <- data.frame(
  original = c("age", "residenceurban", paste0("marital", c("nevermarr", "div.sep", "wid")),
               paste0("age", c("Birth", "FirstCohabMar", "FirstSex")), "polygamy1",  "deliveryDoctor1", 
               "deliveryNurse1", paste0("poly_SHIHD_", 1:2),  paste0("poly_time_", 1:2)),
  new = c("Age", "Urban residence", "Never married", "Divorced/separated", "Widowed",
          "Age at first birth", "Age at first marriage", "Age at first sex", "Polygyny",
          "Delivery aid doctor", "Delivery aid nurse", "SHIHD (1st OP)",
          "SHIHD (2nd OP)", "Time (1st OP)", "Time (2nd OP)")
)
cordf <- list(wframe, mframe)
sexe <- c("women", "men")
for (i in 1:length(cordf)){
  subdf <- cordf[[i]][, c(age_res, mar_nupt, poly_del, "SHIHD", "time")]
  subdf <- subdf[complete.cases(subdf), ]
  subdf[paste0("poly_SHIHD_", 1:2)] <- poly(subdf$SHIHD, 2)
  subdf[paste0("poly_time_", 1:2)] <- poly(subdf$time, 2)
  subdf <- subdf[, -which(names(subdf) %in% c("SHIHD", "time"))]
  names(subdf) <- matchdf$new[match(names(subdf), matchdf$original)]
  png(file = paste0(path, "correlation_", sexe[i], ".png"), height = 700, width = 700, units = "px")
  corrplot::corrplot(cor(subdf), type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "black", method = "color")
  dev.off()
}

# Model comparison on the basis of AIC, BIC and log-likelihood (means and standard deviations from imputation)
modcompG <- modelComparison(fitWomenG, fitMenG, path)
modelResultsGrouped(fitWomenG, fitMenG, path, HIV, sexRatios, modcompG)

# Plot model predictions from all-variable models (linear, quadratic, both and best fit)
plotPredictions(fitWomenG, impWomen, wframe, fitMenG, impMen, mframe, diffAIC = 8)

# Print number of DHS, MIC and AIS surveys
dhsfiles <- read.delim("data_files/dhsfiles.txt")
misfiles <- read.delim("data_files/misfiles.txt")
aisfiles <- read.delim("data_files/aisfiles.txt")
allfiles <- list(dhsfiles, misfiles, aisfiles)
allfiles <- lapply(allfiles, function(x){substr(as.character(x[, 1]), 75, 80)})
fdat <- unique(substr(africadata$survey, 1, 6))
surveylist <- lapply(allfiles, function(x){(fdat[which(fdat %in% x)])})
lapply(surveylist, length)
sum(unlist(lapply(surveylist, length))) == length(fdat)

# Print empty and all NA variables in MIS and AIS surveys
splits <- split(africadata, africadata$survey)
MICS <- splits[which(substr(names(splits), 1, 6) %in% surveylist[[2]])]
lapply(MICS, function(x){names(x)[which(apply(x, 2, function(y){sum(is.na(y))}) == nrow(x))]})
AIS <- splits[which(substr(names(splits), 1, 6) %in% surveylist[[3]])]
lapply(AIS, function(x){names(x)[which(apply(x, 2, function(y){sum(is.na(y))}) == nrow(x))]})

# Print basic sample information
nams <- c('Nindividual', 'Nregion', 'Nregionyear', 'avgYearMostRecentObs', 
          'avgNumberObsPerRegion', 'avgNumberObsPerRegionYear', 'Nsurveys')
sampleInfo <- data.frame(matrix(NA, nrow = length(nams),  ncol = 3))
names(sampleInfo) <- c(NA, 'Women', 'Men')
sampleInfo[, 1] <- nams
sexes <- c("0", "1")
for (i in 1:length(sexes)){
  reg_sub <- regframe[regframe$gender == sexes[i], ]
  splits_reg <- split(reg_sub, list(reg_sub$fullCountry, reg_sub$newRegion))
  splits_reg <- splits_reg[unlist(lapply(splits_reg, nrow)) > 0]
  ind_sub <- africadata[africadata$gender == sexes[i], ]
  splits_ind <- split(ind_sub, list(ind_sub$fullCountry, ind_sub$newRegion))
  splits_ind <- splits_ind[unlist(lapply(splits_ind, nrow)) > 0]
  sampleInfo[, (i + 1)] <- c(nrow(ind_sub),
                             length(splits_ind),
                             nrow(reg_sub),
                             mean(unlist(lapply(splits_ind, function(x){max(x$year)}))),
                             mean(unlist(lapply(splits_ind, nrow))),
                             mean(unlist(lapply(splits_ind, function(x){mean(unlist(lapply(split(x, x$year), nrow)))}))),
                             length(unique(africadata$survey[africadata$gender == "0"])))
}
write.table(sampleInfo, paste0(path, 'sampleInformation.txt'), sep = "\t", row.names = FALSE)

# Overview of linear and quadratic model fits and comparison
modelComp <- data.frame(matrix(NA, nrow = 12, ncol = 10))
modvars <- c('fitSHIHD', 'fitSHIHD2', 'fitAgeRes', 'fitAgeRes2', 'fitMarNupt', 'fitMarNupt2', 
             'fitPolyDel', 'fitPolyDel2', 'fitFinal', 'fitFinal2')
modelComp[, 1] <- c(NA, 'Model', 'SHIHD', NA, 'AgeResidence', NA, 'MarNupt', NA, 'PolySexHivDelivery', NA, 'Total', NA)
modelComp[, 2] <- c(NA, 'Fit', rep(c('Linear', 'Quadratic'), 5))
modelComp[, 3] <- c(rep(NA, 2), mround(modcompG$`AIC mean men`[match(modvars, modcompG[, 1])], .001))
modelComp[, 4] <- c(rep(NA, 2), mround(modcompG$`BIC mean men`[match(modvars, modcompG[, 1])], .001))
modelComp[, 5] <- c(rep(NA, 2), mround(modcompG$`RMSE mean men`[match(modvars, modcompG[, 1])], .001))
modelComp[, 7] <- c(rep(NA, 2), mround(modcompG$`AIC mean women`[match(modvars, modcompG[, 1])], .001))
modelComp[, 8] <- c(rep(NA, 2), mround(modcompG$`BIC mean women`[match(modvars, modcompG[, 1])], .001))
modelComp[, 9] <- c(rep(NA, 2), mround(modcompG$`RMSE mean women`[match(modvars, modcompG[, 1])], .001))
modelComp[, 6] <- modelComp[, 10] <- rep(NA, nrow(modelComp))
modelComp[1, ] <- c(rep(NA, 2), 'Men', rep(NA, 3), 'Women', rep(NA, 3))
modelComp[2, ] <- c('Model', 'Fit', rep(c('AIC', 'BIC', 'RMSE', 'P-value'), 2))
output <- print(xtable(modelComp, digits = rep(3, ncol(modelComp) + 1)), include.rownames = FALSE)
fileConn <- file(paste0(path, "multilevel_results/multivariateModel.txt"))
writeLines(output, fileConn)
close(fileConn)

# Fit models for all types of childlessness and all components (linear)
depvars <- paste0('typeChildless', c("involuntary", "voluntary", "circumstantial"))
indepvars <- paste0("SHIHD", c("", "_Education", "_Income", "_Health"))
sexes <- unique(regframe$gender)
df_list <- res_list <- list()
for (i in 1:length(indepvars)){
  df_list[[i]] <- res_list[[i]] <- list()
  for (k in 1:length(sexes)){
    df_list[[i]][[k]] <- res_list[[i]][[k]] <- list()
    for (j in 1:length(depvars)){
      comp <- regframe[regframe$gender == sexes[k], ]
      comp <- comp[complete.cases(comp[, c(depvars[j], indepvars[i])]), ]
      my_mod <- lm(paste0(depvars[j], ' ~ poly(', indepvars[i], ', 1)'), data = comp)
      df <- data.frame(matrix(NA, nrow = 2*length(my_mod$coefficients) + 4, ncol = 1))
      colnames(df) <- paste(sexes[k], gsub("typeChildless", "", depvars[j]), indepvars[i], sep = "_")
      rownames(df) <- c(paste0(rep(names(my_mod$coefficients), each = 2), c("_b", "_sd")), "AIC", "BIC", "RMSE", "N")
      dd <- summary(my_mod)$coefficients
      sig <- ifelse(dd[, 4] >= 0.1, '', 
                    ifelse(dd[, 4] < 0.1 & dd[, 4] >= 0.05, '\\dagger', 
                           ifelse(dd[, 4] < 0.05 & dd[, 4] >= 0.01, '*', 
                                  ifelse(dd[, 4] < 0.01 & dd[, 4] >= .001, '**', 
                                         ifelse(dd[, 4] < 0.001, '***', NA)))))
      coefs <- paste0(mround(dd[, 1], .001), "$^{", sig, "}$")
      df[1:(nrow(df)-4), 1] <- as.vector(rbind(coefs, paste0("(", mround(dd[, 2], .001), ")")))
      df[((nrow(df)-3):(nrow(df)-1)), 1] <- mround(c(AIC(my_mod), BIC(my_mod), rmse(my_mod)), .001)
      df[nrow(df), 1] <- nrow(comp)
      rownames(df)[rownames(df) == paste0("poly(", indepvars[i], ", 1)_b")] <- paste0("poly(", indepvars[i], ", 2)1_b")
      rownames(df)[rownames(df) == paste0("poly(", indepvars[i], ", 1)_sd")] <- paste0("poly(", indepvars[i], ", 2)1_sd")
      df_list[[i]][[k]][[j]] <- df
    }
    min_ind <- which.max(unlist(lapply(df_list[[i]][[k]], nrow)))
    merge_df <- df_list[[i]][[k]][[min_ind]]
    inds <- c(1:length(df_list[[i]][[k]]))[-which.max(unlist(lapply(df_list[[i]][[k]], nrow)))]
    for (z in 1:length(inds)){
      merge_df[match(rownames(df_list[[i]][[k]][[inds[z]]]), rownames(df_list[[i]][[k]][[min_ind]])), colnames(df_list[[i]][[k]][[inds[z]]])] <- df_list[[i]][[k]][[inds[z]]]
    }
    res_list[[i]][[k]] <- merge_df
  }
  res_list[[i]] <- cbind(res_list[[i]][[1]], res_list[[i]][[2]])
  names(res_list[[i]]) <- substr(names(res_list[[i]]), 1, 7)
  res_list[[i]]["names"] <- rownames(res_list[[i]])
}
types_comps <- rbind.fill(res_list)
types_comps = cbind(types_comps$names, types_comps[, grepl("1", names(types_comps))], types_comps[, grepl("0", names(types_comps))])
output <- print(xtable(types_comps, digits = rep(3, ncol(types_comps) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
fileConn <- file(paste0(path, "typesComponentsLinear.txt"))
writeLines(output, fileConn)
close(fileConn)

# Plot of all human development components and childlessness types in linear regression (line color by gender)
depvars <- paste0('typeChildless', c("involuntary", "voluntary", "circumstantial"))
indepvars <- paste0("SHIHD", c("_Education", "_Income", "_Health"))
plotframe = regframe[, c("gender", depvars, indepvars)]
plotnams = c("Gender", "Involuntary", "Voluntary", "Circumstantial", "Education", "Income", "Health (Life Expectancy)")
names(plotframe) = plotnams
plotframe = subset(plotframe, Voluntary <= 0.08 & Involuntary <= 0.15)
melt_dep = reshape2::melt(plotframe[, plotnams], 
                          id.vars = c("Gender", "Education", "Income", "Health (Life Expectancy)"), 
                          variable.name = "childless_type", value.name = "childlessness")
meltdf = reshape2::melt(melt_dep, id.vars = c("Gender", "childless_type", "childlessness"), 
                        variable.name = "dev_type", value.name = "development")
meltdf$Gender = factor(ifelse(meltdf$Gender == "0", 'Women', "Men"))
g = ggplot() + 
  geom_point(data = meltdf, aes(x = development, y = childlessness, color = Gender, shape = Gender), alpha = 0.1) + 
  geom_smooth(data = meltdf, aes(x = development, y = childlessness, color = Gender, linetype = Gender, fill = Gender), method = "lm") + 
  my_theme() + 
  facet_grid(childless_type ~ dev_type, scales = 'free') +
  labs(x = 'SHIHD', y = "Childlessness (%)", shape = NULL, color = NULL, linetype = NULL, fill = NULL) +
  scale_colour_manual(values = c("royalblue1", "firebrick1")) +
  scale_fill_manual(values = c("royalblue1", "firebrick1")) +
  scale_linetype_manual(values = c("longdash", "solid"))
ggsave(paste0(path, "scatter_plots/regional/typeChildlessSHIHDcomp_lin_Genders.png"), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")



