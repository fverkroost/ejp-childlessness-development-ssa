#-------------------------------------------------------
# More information on HIHD and components
#-------------------------------------------------------

# Load and reshape raw HIHD data
hihd_folder = paste0(getwd(), "/", "data_files/HIHD_components")
file.list = list.files(path = hihd_folder, pattern='*.xls', full.names = TRUE)
file.list = file.list[!grepl("Timor", file.list)]
read_data = function(x){
  df = read_excel(x, range = "A3:H27")
  df["Country"] = gsub(".xls", "", gsub(paste0(hihd_folder, "/"), "", x))
  return(df)
}
df.list = lapply(file.list, read_data)
df = rbind.fill(df.list)
df = df[, c("Year", "HIHD", "Life_Expectancy", "Education", "Adjusted_Income", "Country")]
names(df) = c("Year", "HIHD", "Health", "Education", "Income", "Country")
vars = c("HIHD", "Health", "Education", "Income")
for (i in 1:length(vars)){
  df[,vars[i]] = as.numeric(df[,vars[i]])
}
df$Country = gsub("_", " ", df$Country)
df$Country[df$Country == "Cote dIvoire"] = "Cote d'Ivoire"
df$Country[df$Country == "Congo Dem R"] = "Congo Democratic Republic"
df$Country[df$Country == "CAR"] = "Central African Republic"
df_melt = reshape2::melt(df, id.vars = c("Year", "Country"))

# Plot HIHD trends and save to file
cut_years = c(1870, 1938, 1950)
for (i in 1:length(cut_years)){
  subdf = subset(df_melt, Year >= cut_years[i])
  g = ggplot() +
    geom_line(data = subdf, aes(x = Year, y = as.numeric(value), color = Country)) + 
    stat_summary(data = subdf, aes(x=Year, y = as.numeric(value),group=1), fun=mean, colour="black",size=1, geom="line",group=1) +
    facet_wrap(~variable) +
    my_theme() +
    labs(x = "Year", y = "Value", color = NULL) +
    scale_x_continuous(breaks = round(seq(min(subdf$Year), max(subdf$Year), length.out = 6)))
  ggsave(paste0(path, "trend_ssa_hihd_from_", cut_years[i],".png"), g, dpi = 600, height = 16.933, width = 26, units = "cm")
}

# Plot trends without distinguishing countries 
# (hard to see anyways because of spaghetti lines)
# g = ggplot() +
#   geom_line(data = df_melt, aes(x = Year, y = as.numeric(value), group = Country), color = "grey") + 
#   stat_summary(data = df_melt, aes(x=Year, y = as.numeric(value),group=1), fun=mean, colour="black",size=1, geom="line",group=1) +
#   facet_wrap(~variable) +
#   my_theme() +
#   labs(x = "Year", y = "Value", group = NULL) +
#   scale_x_continuous(breaks = seq(min(df_melt$Year), max(df_melt$Year), 29))
# ggsave(paste0(path, "trend_ssa_hihd_grey.png"), g, dpi = 600, height = 12, width = 21.768, units = "cm")

#-------------------------------------------------------
# Compare HIHD for EU-27 and SSA countries
#-------------------------------------------------------

if (FALSE){
  # Define EU countries for comparison
  eu27 = c("Austria",	"Italy","Belgium",	"Latvia","Bulgaria",	"Lithuania", "Croatia"	, 
           "Cyprus",	"Malta","Czech_Rep",	"Netherlands","Denmark"	,"Poland","Estonia",	"Portugal",
           "Finland",	"Romania","France",	"Slovak_Rep","Germany"	,"Slovenia","Greece",	"Spain",
           "Hungary",	"Sweden","Ireland", "UK")
  
  # Download HIHD files for EU into folder from URLs
  url_list <- paste0("https://espacioinvestiga.org/00REPO/TablasExcel_HIHD/", eu27, ".xls")
  comp_list <- list()
  if (!dir.exists("data_files/HIHD_EU")){
    dir.create("data_files/HIHD_EU")
  }
  for (i in 1:length(url_list)){
    nam <- paste0('data_files/HIHD_EU/', eu27[i], '.xls')
    if (!(file.exists(nam))){
      download.file(url_list[i], nam, quiet = TRUE)
    }
    comp_list[[i]] <- read_excel(nam, range = "A3:H27")
    comp_list[[i]]['Country'] <- eu27[i]
  }
  eu = rbind.fill(comp_list)
  eu = eu[, c("Year", "HIHD", "Life_Expectancy", "Education", "Adjusted_Income", "Country")]
  names(eu) = c("Year", "HIHD", "Health", "Education", "Income", "Country")
  vars = c("HIHD", "Health", "Education", "Income")
  for (i in 1:length(vars)){
    eu[,vars[i]] = as.numeric(eu[,vars[i]])
  }
  eu$Country = gsub("_", " ", eu$Country)
  
  # Compare SSA and EU HIHd
  eu %>%
    filter(Year == 2015) %>%
    summarise(avg_hihd = mean(HIHD, na.rm=T),
              avg_edu = mean(Education, na.rm=T),
              avg_inc = mean(Income, na.rm=T),
              avg_health = mean(Health, na.rm=T))
  
  df %>%
    filter(Year == 2015) %>%
    summarise(avg_hihd = mean(HIHD, na.rm=T),
              avg_edu = mean(Education, na.rm=T),
              avg_inc = mean(Income, na.rm=T),
              avg_health = mean(Health, na.rm=T))
}

#---------------------------------------------------------
# Testing for more information on orthogonal polynomials
#---------------------------------------------------------

if (FALSE){
  x = rnorm(100)
  degree = 3
  xbar = mean(x)
  xc = x - xbar
  index = seq(-1, degree, 1)
  p = list(0, 1, xc)
  l = list(NA, length(x), (xc %*% xc)[1,1])
  alpha = list(NA, 0, ((xc^2 %*% xc)/l[[3]])[1,1])
  beta = list(NA, NA, l[[3]] / l[[2]])
  p = append(p, as.list(rep(NA, length(index) - 3)))
  l = append(l, as.list(rep(NA, length(index) - 3)))
  alpha = append(alpha, as.list(rep(NA, length(index) - 3)))
  beta = append(beta, as.list(rep(NA, length(index) - 3)))
  
  for (i in 2:degree){
    place = which(index == i)
    p[[place]] = (xc - alpha[[place - 1]]) * p[[place - 1]] - beta[[place - 1]] * p[[place - 2]]
    l[[place]] = (p[[place]] %*% p[[place]])[1,1]
    alpha[[place]] = ((p[[place]]^2 %*% xc) / l[[place]])[1,1]
    beta[[place]] = l[[place]] / l[[place - 1]]
  }
  
  for (i in 0:degree){
    place = which(index == i)
    p[[place]] = p[[place]] / sqrt(l[[place]])
  }
  
  res = p[seq(length(p)-degree+1, length(p))]
  mat = matrix(NA, nrow = length(x), ncol = degree)
  for (i in 1:degree){
    mat[,i] = res[[i]]
  }
  poly(x, 3)[1:5,]
  mat[1:5,]
  xbar <- mean(x)
  x <- x - xbar
  X <- outer(x, 0L:degree, "^")
  QR <- qr(X)
  if (QR$rank < degree) 
    stop("'degree' must be less than number of unique points")
  z <- QR$qr
  z <- z * (row(z) == col(z))
  Z <- qr.qy(QR, z)
  norm2 <- colSums(Z^2)
  alpha <- (colSums(x * Z^2)/norm2 + xbar)[1L:degree]
  norm2 <- c(1, norm2)
  Z <- Z/rep(sqrt(norm2[-1L]), each = length(x))
  colnames(Z) <- 0L:degree
  Z <- Z[, -1, drop = FALSE]
  
  poly(x, 3)
}

#----------------------------------------------------------------
# National childlessness trend by type and gender
#----------------------------------------------------------------

natframe_melt = reshape2::melt(
  natframe, 
  measure.vars = c("nationalChildlessness", paste0("typeChildless", c("voluntary", "involuntary", "circumstantial")))
)
natframe_melt$variable = as.character(natframe_melt$variable)
natframe_melt$gender = as.character(natframe_melt$gender)
natframe_melt$variable[natframe_melt$variable == "nationalChildlessness"] = "Overall"
natframe_melt$variable[natframe_melt$variable == "typeChildlessvoluntary"] = "Voluntary"
natframe_melt$variable[natframe_melt$variable == "typeChildlessinvoluntary"] = "Involuntary"
natframe_melt$variable[natframe_melt$variable == "typeChildlesscircumstantial"] = "Circumstantial"
natframe_melt$variable = factor(natframe_melt$variable, levels=c("Overall", "Involuntary", "Voluntary", "Circumstantial"))
natframe_melt$gender[natframe_melt$gender == "0"] = "Women"
natframe_melt$gender[natframe_melt$gender == "1"] = "Men"
splits = split(natframe_melt, list(natframe_melt$gender, natframe_melt$fullCountry))
natframe_points = rbind.fill(splits[unlist(lapply(splits, function(x){length(unique(x$year))})) == 1])

# Plot trends and save (line form)
g = ggplot() +
  geom_line(data = natframe_melt, aes(x = year, y = 100*value, color = fullCountry)) +
  geom_point(data = natframe_points, aes(x = year, y = 100*value, color = fullCountry)) + 
  facet_grid(variable ~ gender, scales = "free") + 
  my_theme() +
  labs(x="Year",y="Childlessness (%)",color=NULL)
ggsave(paste0(path, "trendsNationalTypeGenderLine.png"), g, dpi = 600, height = 30, width = 28, units = "cm")

# Plot trends and save (boxplot form)
# g = ggplot() +
#   geom_boxplot(data = natframe_melt, aes(x = factor(year), y = 100*value)) +
#   facet_grid(variable ~ gender, scales = "free") + 
#   my_theme() +
#   labs(x="Year",y="Childlessness (%)",color=NULL) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggsave(paste0(path, "trendsNationalTypeGenderBox.png"), g, dpi = 600, height = 30, width = 28, units = "cm")

#----------------------------------------------------------------------
# Nicely visualised trends with three periods
#----------------------------------------------------------------------

quants = quantile(natframe$year, seq(0, 1, 0.25))
year_quants = quants[2:4]
year_std = 2
trenddf = data.frame(matrix(NA, nrow = 0, ncol = 0))
uni_country = unique(natframe$fullCountry)
for (i in 1:length(uni_country)){
  for (j in 1:length(year_quants)){
    for (u in 1:2){
      subdf = natframe[natframe$fullCountry == uni_country[i] & natframe$gender == (u - 1), ]
      all_years = subdf$year
      if (length(all_years) > 0){
        years_range = all_years[all_years %in% seq(year_quants[j] - year_std, year_quants[j] + year_std, 1)]
        if (length(years_range) > 0){
          closest_year = years_range[which.min(years_range - year_quants[j])][1]
          if (nrow(trenddf) == 0){
            trenddf = subdf[subdf$year == closest_year, c("fullCountry", "year", "gender", "percChildlessness")]
          } else {
            trenddf = rbind(trenddf, subdf[subdf$year == closest_year, c("fullCountry", "year", "gender", "percChildlessness")])
          }
        }
      }
    }
  }
}
trenddf["sex"] = ifelse(trenddf$gender == 0, "Women", ifelse(trenddf$gender == 1, "Men", NA))
trenddf["year_zone"] = ifelse(trenddf$year %in% seq(year_quants[1] - year_std, year_quants[1] + year_std, 1), paste0("Period 1 (", year_quants[1] - year_std, " - ", year_quants[1] + year_std, ")"),
                              ifelse(trenddf$year %in% seq(year_quants[2] - year_std, year_quants[2] + year_std, 1), paste0("Period 2 (", year_quants[2] - year_std, " - ", year_quants[2] + year_std, ")"), 
                                     ifelse(trenddf$year %in% seq(year_quants[3] - year_std, year_quants[3] + year_std, 1), paste0("Period 3 (", year_quants[3] - year_std, " - ", year_quants[3] + year_std, ")"), NA)))

# Plotting theme for this plot specifically
theme_trend <- function () { 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size = 14),
                     strip.text = element_text(size = 14),
                     strip.background = element_rect(fill = "white", colour = "black"),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom",
                     legend.text = element_text(size = 14))
}

# Ordering by childlessness per period (women)
ft = subset(trenddf, sex == "Women")
ft_wide = ft %>% 
  dplyr::select(-year) %>%
  spread(year_zone, percChildlessness) %>%
  arrange(`Period 1 (1997 - 2001)`, `Period 2 (2005 - 2009)`, `Period 3 (2011 - 2015)`)
country_order_women = levels(factor(ft_wide$fullCountry, levels = ft_wide$fullCountry))
ft_wide["smallest_value"] = ifelse(!is.na(ft_wide$`Period 1 (1997 - 2001)`), ft_wide$`Period 1 (1997 - 2001)`,
                                   ifelse(!is.na(ft_wide$`Period 2 (2005 - 2009)`), ft_wide$`Period 2 (2005 - 2009)`,
                                          ft_wide$`Period 3 (2011 - 2015)`))
ft_wide = ft_wide[order(ft_wide$smallest_value), ]
country_order_women_na = levels(factor(ft_wide$fullCountry, levels = ft_wide$fullCountry))

# Ordering by childlessness per period (men)
mt = subset(trenddf, sex == "Men")
mt_wide = mt %>% 
  dplyr::select(-year) %>%
  spread(year_zone, percChildlessness) %>%
  arrange(`Period 1 (1997 - 2001)`, `Period 2 (2005 - 2009)`, `Period 3 (2011 - 2015)`)
country_order_men = levels(factor(mt_wide$fullCountry, levels = mt_wide$fullCountry))
mt_wide["smallest_value"] = ifelse(!is.na(mt_wide$`Period 1 (1997 - 2001)`), mt_wide$`Period 1 (1997 - 2001)`,
                                   ifelse(!is.na(mt_wide$`Period 2 (2005 - 2009)`), mt_wide$`Period 2 (2005 - 2009)`,
                                          mt_wide$`Period 3 (2011 - 2015)`))
mt_wide = mt_wide[order(mt_wide$smallest_value), ]
country_order_men_na = levels(factor(mt_wide$fullCountry, levels = mt_wide$fullCountry))

# Ordered plot for women
gf = ggplot() + 
  geom_point(data = ft, aes(x = percChildlessness, y = fullCountry, color = year_zone, shape = year_zone), size = 3) +
  facet_wrap(~sex) +
  labs(x = "Childlessness (%)", y = NULL, color = NULL, shape = NULL) +
  scale_x_continuous(breaks = seq(ceiling(min(na.omit(ft$percChildlessness))), floor(max(na.omit(ft$percChildlessness)))), minor_breaks = NULL) + 
  theme_trend()
gf_order = gf + scale_y_discrete(limits = rev(country_order_women), position = "right") 
gf_order_na = gf + scale_y_discrete(limits = rev(country_order_women_na), position = "right") 

# Ordered plot for men
gm = ggplot() + 
  geom_point(data = mt, aes(x = percChildlessness, y = fullCountry, color = year_zone, shape = year_zone), size = 3) +
  facet_wrap(~sex) + 
  labs(x = "Childlessness (%)", y = NULL, color = NULL, shape = NULL) +
  scale_x_continuous(breaks = seq(ceiling(min(na.omit(mt$percChildlessness))), floor(max(na.omit(mt$percChildlessness)))), minor_breaks = NULL) + 
  theme_trend()
gm_order = gm + scale_y_discrete(limits = rev(country_order_men))
gm_order_na = gm + scale_y_discrete(limits = rev(country_order_men_na))

# Combined plot (ordered separately for men and women)
g = ggarrange(gm_order, gf_order, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
# ggsave(paste0(path, "childlessness_trend_ssa_plot_sorted.png"), dpi = 600,  g, height = 20, width = 25.7, units = "cm")
g = ggarrange(gm_order_na, gf_order_na, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(paste0(path, "childlessness_trend_ssa_plot_sorted_na.png"), dpi = 600,  g, height = 20, width = 25.7, units = "cm")

# Compute information about the number of countries for which 
# male childlessness is larger than female childlessness
sub = trenddf %>% 
  dplyr::select(-sex) %>% 
  tidyr::spread(gender, percChildlessness) %>%
  dplyr::mutate(men_bigger_than_women = `1` >= `0`) %>%
  dplyr::filter(!is.na(men_bigger_than_women)) 
sub %>%
  dplyr::group_by(year_zone) %>%
  dplyr::summarise(length(which(men_bigger_than_women)) / n())
sub %>%
  dplyr::group_by(year_zone) %>%
  dplyr::summarise(n())
sub %>% 
  dplyr::filter(year_zone == "Period 1 (1997 - 2001)" & men_bigger_than_women == TRUE) %>% 
  dplyr::pull(fullCountry)
sub %>% 
  dplyr::filter(year_zone == "Period 2 (2005 - 2009)" & men_bigger_than_women == TRUE) %>% 
  dplyr::pull(fullCountry)
sub %>% 
  dplyr::filter(year_zone == "Period 3 (2011 - 2015)" & men_bigger_than_women == TRUE) %>% 
  dplyr::pull(fullCountry)

# Function to compute change in development for given period for given region
change_function = function(x){
  x = x[!is.na(x$SHIHD), ]
  if (nrow(x) > 0){
    change_df = data.frame(country = unique(x$fullCountry), 
                           region = unique(x$newRegion), 
                           begin_year = min(x$year), 
                           end_year = max(x$year), 
                           begin_shihd = x$SHIHD[x$year == min(x$year)],
                           end_shihd = x$SHIHD[x$year == max(x$year)],
                           change = x$SHIHD[x$year == max(x$year)] - x$SHIHD[x$year == min(x$year)])
  } else {
    change_df = data.frame(matrix(NA, nrow = 0, ncol = 7))
    names(change_df) = c("country", "region", "begin_year", "end_year", "begin_shihd", "end_shihd", "change")
  }
  return(change_df)
}

# Compute change in development over observed periods
women_frame = subset(regframe, gender == "0")
splits = split(women_frame, list(women_frame$fullCountry, women_frame$newRegion))
splits = splits[unlist(lapply(splits, nrow)) > 0]
change_list = lapply(splits, change_function)
change_final = rbind.fill(change_list)
change_final[order(change_final$change, decreasing = TRUE), ]
change_final_round = change_final
change_final_round$begin_shihd = round(change_final_round$begin_shihd, 1)
change_final_round$end_shihd = round(change_final_round$end_shihd, 1)
change_final_round[change_final_round$begin_shihd == 0.1 & change_final_round$end_shihd == 0.3, ]
change_final_round[change_final_round$begin_shihd == 0.2 & change_final_round$end_shihd == 0.3, ]

