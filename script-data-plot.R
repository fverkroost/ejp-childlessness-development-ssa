source('functions-data-plot.R')

# Descriptives about childlessness across regions and comparison of male and female
mf = subset(newff, gender == "1")
wf = subset(newff, gender == "0")
mf[order(mf$percChildlessness, decreasing = T), c("fullCountry", 'newRegion', "percChildlessness", "year")]
wf[order(wf$percChildlessness, decreasing = T), c("fullCountry", 'newRegion', "percChildlessness", "year")]
splits = split(newff, list(newff$fullCountry, newff$newRegion, newff$year))
splits = splits[unlist(lapply(splits, nrow)) > 0]
diff_cat = lapply(splits, difference_male_female_childlessness_category)
diff_mag = lapply(splits, difference_male_female_childlessness_magnitude)
tab_cat = table(unlist(diff_cat))
tab_cat['male bigger'] / sum(tab_cat) * 100
tab_mag = table(unlist(diff_mag))
tab_mag['male over 5 percent bigger'] / tab_cat['male bigger'] * 100

# Joyplot for age distributions across countries
joyPlot(africadata, path)

# Maps of childlessness (regional)
regionMaps(newff, shape, "percChildlessness", c(0, 1, 2, 3, 4, 6, 8, 10, 20, 30), path)

# Compare male and female childlessness
newff_female = newff[newff$gender == "0", ]
newff_male = newff[newff$gender == "1", ]
newff_female[order(newff_female$percChildlessness, decreasing = TRUE), c("percChildlessness", "fullCountry", "newRegion")]
newff_male[order(newff_male$percChildlessness, decreasing = TRUE), c("percChildlessness", "fullCountry", "newRegion")]
newff_spread = spread(newff[, c("percChildlessness", "fullCountry", "newRegion", "gender")], gender, percChildlessness)
newff_spread["male_childlessness_larger_than_female_childlessness"] = (newff_spread$`1` >= newff_spread$`0`)
newff_spread["male_childlessness_5pp_larger_than_female_childlessness"] = (newff_spread$`1` >= (newff_spread$`0` + 5))
n_male_larger_female = length(which(newff_spread$male_childlessness_larger_than_female_childlessness))
p_male_larger_female = length(which(newff_spread$male_childlessness_larger_than_female_childlessness)) / nrow(newff_spread) * 100
n_male_5pp_larger_female = length(which(newff_spread$male_childlessness_5pp_larger_than_female_childlessness))
n_male_5pp_larger_female_larger = length(which(newff_spread$male_childlessness_5pp_larger_than_female_childlessness)) / length(which(newff_spread$male_childlessness_larger_than_female_childlessness)) * 100
n_male_5pp_larger_female_total = length(which(newff_spread$male_childlessness_5pp_larger_than_female_childlessness)) / nrow(newff_spread) * 100
res = data.frame(
  n_male_larger_female = n_male_larger_female,
  p_male_larger_female = p_male_larger_female,
  n_male_5pp_larger_female = n_male_5pp_larger_female,
  n_male_5pp_larger_female_larger = n_male_5pp_larger_female_larger,
  n_male_5pp_larger_female_total = n_male_5pp_larger_female_total
)
output <- print(xtable(res, digits = rep(3, ncol(res) + 1)), sanitize.text.function = function(x){x})
fileConn <- file(paste0(path, "compare_female_male_childlessness_recent_regions.txt"))
writeLines(output, fileConn)
close(fileConn)

# Data distributions: black and white tile plot for survey availability
tilePlotting(africadata, path)

# Table of countries and original and selected sample sizes
countryTable(africadata, datalist_org, path)

# Childlessness trends over time (national)
# childlessnessTrends(natframe, path)

# Childlessness trends over time (regional)
# childlessnessTrends(regframe, path)

# Childlessness versus sample size (regional)
distSamp(africadata, path, 800)

# Plot SHIHD time trends
# splits <- split(shihd, shihd$Country)
# splits <- splits[unlist(lapply(splits, nrow)) > 0]
# lapply(splits, plotCountryRegionRatioSHIHD, path)

# Childlessness and SHIHD components (national)
scatterChildlessnessSHIHD(
  natframe, 
  "percChildlessness", 
  path, 
  compareModels = "anova", 
  anova_sig = .05, 
  diffAIC = NULL
)

# Childlessness and SHIHD components (regional)
scatterChildlessnessSHIHD(
  regframe, 
  "percChildlessness", 
  path, 
  compareModels = "anova", 
  anova_sig = .05, 
  diffAIC = NULL
)

# Childlessness and SHIHD components (national, age-adjusted)
# scatterChildlessnessSHIHD(
#   natframe, 
#   "nationalChildlessnessAgeadjusted", 
#   path, 
#   compareModels = "anova", 
#   anova_sig = .05, 
#   diffAIC = NULL
# )

# Childlessness types and SHIHD components (national)
# scatterChildlessnessTypesSHIHD(
#   frame = natframe, 
#   path, 
#   compareModels = "anova", 
#   anova_sig = .05, 
#   diffAIC = NULL
# )

# Childlessness types and SHIHD components (regional)
# scatterChildlessnessTypesSHIHD(
#   frame = regframe, 
#   path, 
#   compareModels = "anova", 
#   anova_sig = .05, 
#   diffAIC = NULL
# )
