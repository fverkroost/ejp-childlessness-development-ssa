# Calculate root mean squared error of model
rmse <- function(fit){
  return(sqrt(mean((fit$residuals)^2)))
}

# Descriptives about childlessness across regions and comparison of male and female (categorical)
difference_male_female_childlessness_category = function(x){
  mc = x$percChildlessness[x$gender == "1"]
  fc = x$percChildlessness[x$gender == "0"]
  res = ifelse(mc == fc, 'equal',
               ifelse(mc > fc, "male bigger", 
                      ifelse(fc > mc, "female bigger")))
  return(res)
}

# Descriptives about childlessness across regions and comparison of male and female (magnitude)
difference_male_female_childlessness_magnitude = function(x){
  mc = x$percChildlessness[x$gender == "1"]
  fc = x$percChildlessness[x$gender == "0"]
  res = ifelse(mc == fc, 'equal',
               ifelse(mc > (fc + 5), "male over 5 percent bigger", 
                      ifelse(mc > fc & !(mc > (fc + 5)), "male less than 5 percent bigger",
                             ifelse(fc > (mc + 5), "female over 5 percent bigger", 
                                    ifelse(fc > mc & !(fc > (mc + 5)), "female less than 5 percent bigger")))))
  return(res)
}

# Joyplot for age distributions across countries (faceted on gender)
joyPlot <- function(agedata, path){
  agedata["sex"] <- ifelse(agedata$gender == "0", "Women", ifelse(agedata$gender == "1", "Men", NA))
  unis <- unique(agedata$fullCountry)
  agedata$fullCountry <- factor(agedata$fullCountry, levels = unis[order(unis)])
  g <- ggplot(agedata, aes(x = age, y = fullCountry)) +
    geom_density_ridges(scale = 0.9, color = "red", fill = "red")  +
    theme_minimal(base_size = 14) + theme(axis.text.y = element_text(vjust = 0))  +
    scale_x_continuous(limits = c(40, 64), expand = c(0.01, 0))  +
    scale_y_discrete(limits = rev(levels(agedata$fullCountry)), expand = c(0.01, 0))  +
    labs(x = "Age", y = NULL)  +
    facet_wrap(~ sex, scales = "free_x") +
    my_theme() +
    theme(panel.spacing = unit(1, "lines")) 
  ggsave(paste0(path, "joyPlot.png"), dpi = 600, g, height = 16.933, width = 21.768, units = "cm")
}

# Make regional plot maps for sample size, childlessness and HDI
regionMaps <- function(data, shp, targetvar, brks, path){
  agg <- aggregate(data[, targetvar], by = list(GDLcode = data$GDLcode, gender = data$gender), FUN = function(x){mean(na.omit(x))})
  agg['cutvar'] <- cut(agg[, targetvar], breaks = c(brks, ceiling(max(agg[, targetvar]))), include.lowest = TRUE)
  agg$gender <- as.character(agg$gender)
  agg$gender <- ifelse(agg$gender == "0", "Women", ifelse(agg$gender == "1", "Men", NA))
  g <- ggplot() +
    geom_map(data = shp, map = shp, aes(x = long, y = lat, group = group, map_id = id),
             fill = 'white', colour = '#7f7f7f', size = 0.1) +
    geom_map(data = agg, map = shp, aes(fill = cutvar, map_id = GDLcode), colour = '#7f7f7f', size = 0.1) +
    scale_fill_manual(values = rev(brewer.pal(length(brks) + 1, "RdYlBu")),  
                      labels = c(names(table(agg$cutvar))[1:(length(table(agg$cutvar)) - 1)], '30+')) +
    scale_y_continuous(breaks = c(), limits = c(-32, 35)) +
    scale_x_continuous(breaks = c(), limits = c(-15, 49)) +
    labs(fill = "Childlessness (%)", title = NULL, x = NULL, y = NULL) +
    my_theme() +
    theme(panel.border = element_blank(), 
          strip.background = element_blank(),
          axis.line = element_blank()) +
    facet_wrap(~ gender)
  ggsave(paste0(path, 'mapChildlessness.png'), dpi = 600,  g, height = 16.933, width = 21.768, units = "cm")
}

# Make tile plot for available surveys faceted on gender
tilePlotting <- function(plotData, path){
  
  # Reformat data to desired form
  df <- expand.grid(unique(plotData$year), unique(plotData$fullCountry), unique(plotData$gender))
  names(df) <- c('year', 'fullCountry', 'gender')
  totdf <- rbind(df, df)
  totdf["Gender"] <- rep(c("Women", "Men"), rep(nrow(df), 2))
  sex <- rep(c("0", "1"), rep(nrow(df), 2))
  totdf["Value"] <- NA
  for (i in 1:nrow(totdf)){
    row <- which(plotData$fullCountry == totdf$fullCountry[i] & plotData$year == totdf$year[i] & plotData$gender == sex[i])
    totdf$Value[i] <- ifelse(length(row) > 0, 1, 0)
  }
  totdf$Value <- as.factor(as.character(totdf$Value))
  
  # Create plot
  g <- ggplot(totdf, aes(x = year, y = fullCountry, fill = Value))  +
    geom_tile()  +
    scale_fill_manual(values = c("white", "black"))  +
    guides(fill = FALSE)  +
    scale_x_continuous(breaks = round(seq(min(totdf$year), max(totdf$year), 5))) +
    scale_y_discrete(limits = rev(levels(totdf$fullCountry)[order(levels(totdf$fullCountry))])) +
    my_theme() +
    theme(panel.spacing = unit(1, "lines"))  +
    labs(x = NULL, y = NULL)  +
    facet_wrap(~ Gender)
  ggsave(paste0(path, "surveyYear.png"), dpi = 600, g, height = 16.933, width = 21.768, units = "cm")
}

# Table of countries and original and selected sample sizes
countryTable <- function(africadata, datalist_org, path){
  
  df <- data.frame(Country = unique(africadata$fullCountry), ISO3 = unique(africadata$ISO3), country = unique(africadata$country))
  sex_letter <- c("M", "I")
  sex_number <- c("1", "0")
  sex_word <- c("Men", "Women")
  for (i in 1:2){
    sublist <- datalist_org[substr(names(datalist_org), 3, 3) == sex_letter[i]]
    subdata <- africadata[africadata$gender == sex_number[i], ]
    org_sample_size <- data.frame(sample_size = unlist(lapply(sublist, nrow)), 
                                  country = substr(names(sublist), 1, 2))
    agg <- aggregate(sample_size ~ country, data = org_sample_size, FUN = sum)
    df[paste0(sex_word[i], "_Original")] <- df[paste0(sex_word[i], "_Selected")] <- NA
    agg <- agg[agg$country %in% df$country, ]
    df[match(agg$country, df$country), paste0(sex_word[i], "_Original")] <- agg$sample_size
    vec <- unlist(lapply(split(subdata, subdata$fullCountry), nrow))
    df[match(names(vec), df$Country), paste0(sex_word[i], "_Selected")] <- vec
  }
  df <- df[, !names(df) == "country"]
  df <- df[order(df$Country), ]
  output <- print(xtable(df, digits = rep(3, ncol(df) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
  fileConn <- file(paste0(path, "sampleSizes.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}


# Childlessness trends over time
childlessnessTrends <- function(frame, path){
  
  if ("newRegion" %in% names(frame)){
    groups <- list(year = frame$year, fullCountry = frame$fullCountry, gender = frame$gender, region = frame$newRegion)
    level <- 'regional'
  } else {
    groups <- list(year = frame$year, fullCountry = frame$fullCountry, gender = frame$gender)
    level <- 'national'
  }
  vars <- c("childless_dummy1", paste0("typeChildless", c("circumstantial", "voluntary", "involuntary")))
  nams <- c("Overall", "Circumstantial", "Voluntary", "Involuntary")
  plotdf <- list()
  for (j in 1:length(vars)){
    plotdf[[j]] <- aggregate(frame[, vars[j]], by = groups, FUN = function(x){100*mean(na.omit(x))})
    plotdf[[j]]["variable"] <- rep(nams[j], nrow(plotdf[[j]]))
  }
  plotdf <- rbind.fill(plotdf)
  plotdf$gender <- ifelse(plotdf$gender == "0", "Women", ifelse(plotdf$gender == "1", "Men", NA))
  plotdf <- plotdf[plotdf$x <= 15 & !is.na(plotdf$x), ]
  plotdf$variable <- factor(plotdf$variable, levels = c("Overall", "Involuntary", "Voluntary", "Circumstantial"))
  
  if ("newRegion" %in% names(frame)){
    g <- ggplot(data = plotdf, aes(x = year, y = x, color = region))
  } else {
    g <- ggplot(data = plotdf, aes(x = year, y = x, color = fullCountry))
  }
  g <- g + facet_grid(variable ~ gender, scales = "free_y") +
    geom_line() +
    geom_point(alpha = 0.6) +
    my_theme() +
    scale_x_continuous(breaks = seq(min(plotdf$year), max(plotdf$year), 5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, color = NULL, y = "Childlessness (%)")
  if ("newRegion" %in% names(frame)){
    g <- g + theme(legend.position = "none")
  }
  ggsave(paste0(path, level, "Trends.png"), g, dpi = 600, height = 5000/200, width = 6428/200, units = "cm")
  g = g + geom_smooth(data = plotdf, aes(x = year, y = x), color = "black") 
  ggsave(paste0(path, level, "TrendsAverageLoess.png"), g, dpi = 600, height = 5000/200, width = 6428/200, units = "cm")
}

# Plot sample size versus percentage childlessness
distSamp <- function(africadata, path, limit){
  
  # Reformat data into desired form
  frame <- africadata %>% 
    dplyr::group_by(fullCountry, year, newRegion, gender) %>%
    dplyr::mutate(N = n(), percChildlessness = mean(na.omit(as.numeric(as.character(childless_dummy))))*100)
  frame["sex"] <- ifelse(frame$gender == "0", "Women", ifelse(frame$gender == "1", "Men", NA))
  
  # Create plot
  g <- ggplot()  +
    geom_point(data = frame, aes(x = N, y = percChildlessness), alpha = 0.6) +
    labs(x = 'Sample Size', y = 'Childlessness (%)') +
    facet_wrap(~ frame$sex, scales = 'free_x') +
    my_theme() +
    xlim(c(0, limit))
  ggsave(paste0(path, 'SampleSizeChildlessness.png'), dpi = 600,  g, height = 16.933, width = 21.768, units = "cm")
  # ggsave(paste0(path, 'SampleSizeChildlessness', limit, '.png'), dpi = 600,  g1, height = 16.933, width = 21.768, units = "cm")
}

# Plot SHIHD time trends
plotCountryRegionRatioSHIHD <- function(x, path){
  
  # Reformat data into desired form
  plot_sub <- gather(x, "Indicator", "Value", -Country, -Region, -Year)
  plot_sub <- plot_sub[grepl("SHIHD", plot_sub$Indicator), ]
  plot_sub$Indicator <- gsub("SHIHD_", "", plot_sub$Indicator)
  plot_sub <- plot_sub[plot_sub$Region != "Total", ]
  
  # Create plot
  g <- ggplot()  +
    geom_line(data = plot_sub, aes(x = Year, y = Value, colour = Region, group = Region)) +
    my_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(min(plot_sub$Year), max(plot_sub$Year), 10)) +
    labs(x = NULL, y = "Value", color = NULL, group = NULL) +
    facet_wrap(~ Indicator) +
    ggtitle(unique(plot_sub$Country))
  if (!dir.exists(paste0(path, 'trendsHDI/SHIHDperCountry/withoutLegend'))){
    dir.create(paste0(path, 'trendsHDI/SHIHDperCountry/withoutLegend'), recursive = TRUE)
  }
  if (!dir.exists(paste0(path, 'trendsHDI/SHIHDperCountry/withLegend'))){
    dir.create(paste0(path, 'trendsHDI/SHIHDperCountry/withLegend'), recursive = TRUE)
  }
  ggsave(paste0(path, 'trendsHDI/SHIHDperCountry/withLegend/', gsub(" ", "", unique(plot_sub$Country)), '.png'), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  g <- g + theme(legend.position="none")
  ggsave(paste0(path, 'trendsHDI/SHIHDperCountry/withoutLegend/', gsub(" ", "", unique(plot_sub$Country)), '.png'), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")
}

# Perform linear or polynomial robust regression
mcdEstimates <- function(data, depvar, indepvar, fit = c('lm', 'poly')){
  
  if (fit == 'lm'){
    X <- data.matrix(data[, which(names(data) == indepvar)])
    X <- poly(X, 1)
    nams <- c(depvar, indepvar)
  } else if (fit == 'poly'){
    X <- data.matrix(data[, which(names(data) == indepvar)])
    X <- poly(X, 2)
    nams <- c(depvar, indepvar, paste0(indepvar, '2'))
  }
  X_icpt <- cbind(matrix(rep(1, nrow(X)), ncol = 1), X)
  y <- data.matrix(data[, which(names(data) == depvar)])
  totdf <- data.frame(cbind(y, X))
  names(totdf) <- nams
  mcd <- robustbase::covMcd(totdf) # n variables
  n <- ncol(totdf)
  N <- nrow(totdf)
  scatter <- mcd$cov # dim n x n
  location <- mcd$center # dim n x n
  sigma_xx <- scatter[which(rownames(scatter) != depvar), which(colnames(scatter) != depvar)] # dim (n-1) x (n-1)
  sigma_yy <- scatter[which(rownames(scatter) == depvar), which(colnames(scatter) == depvar)] # dim 1 x 1
  sigma_xy <- scatter[which(rownames(scatter) == depvar), which(colnames(scatter) != depvar)] # dim (n-1) x 1
  beta <- solve(sigma_xx) %*% matrix(sigma_xy, nrow = n - 1) # dim (n-1) x 1
  mu_y <- location[which(names(location) == depvar)] # dim 1 x 1
  mu_x <- matrix(location[which(names(location) != depvar)], nrow = n - 1) # dim (n-1) x 1
  intercept <- mu_y - t(mu_x) %*% beta
  
  # Calculate predicted values for y
  y_pred <- c(intercept) + c(X %*% beta)
  my_res <- y - y_pred
  df_res <- ifelse(fit == "lm", nrow(data) - 2, nrow(data) - 3)
  RMSE <- sqrt(mean(my_res^2))
  RSS <- sum(my_res^2)
  
  # Fit models
  moddata <- data
  moddata["Value1"] <- poly(data[, indepvar], 2)[, 1]
  moddata["Value2"] <- poly(data[, indepvar], 2)[, 2]
  if (fit == "lm"){
    mod <- lm(paste0(depvar, ' ~ Value1'), data = moddata)
  } else {
    mod <- lm(paste0(depvar, ' ~ Value1 + Value2'), data = moddata)
  }
  
  # Subtract coefficients
  my_coefs <- c(intercept, beta)
  names(my_coefs) <- c('(Intercept)', paste0("Value", 1:(length(my_coefs)-1)))
  mod$coefficients <- my_coefs
  
  return(list(RSS = RSS, df_res = df_res, my_fit = mod))
}

# Childlessness and SHIHD components (national and regional depending on input to frame)
scatterChildlessnessSHIHD <- function(frame, targetvar, path, compareModels, anova_sig, diffAIC){
  
  # Reformat data into desired form
  indepvars <- paste0("SHIHD", c("", "_Education", "_Health", "_Income"))
  if (class(frame) == "list"){
    rows <- unlist(lapply(frame, nrow))
    start <- which(rows != rows[1])[1]
    women_array <- array(NA, dim = c(nrow(frame[[1]]), length(c(targetvar, indepvars)), length(c(1:(start - 1)))))
    men_array <- array(NA, dim = c(nrow(frame[[start]]), length(c(targetvar, indepvars)), length(c(start:length(frame)))))
    for (i in 1:length(frame)) { 
      if (i %in% c(1:(start - 1))){
        women_array[,, i] <- as.matrix(frame[[i]][, c(targetvar, indepvars)])
      } else {
        men_array[,, i - (start - 1)] <- as.matrix(frame[[i]][, c(targetvar, indepvars)])
      }
    }
    women_frame <- data.frame(apply(women_array, c(1,2), mean))
    men_frame <- data.frame(apply(men_array, c(1,2), mean))
    frame <- rbind(women_frame, men_frame)
    names(frame) <- c(targetvar, indepvars)
    frame["gender"] <- rep(c("0", "1"), c(nrow(women_frame), nrow(men_frame)))
    type <- "imp"
  } else {
    type <- "no_imp"
  }
  
  folder <- ifelse(grepl("adjusted", targetvar), "age_adjusted/", ifelse(type == "imp", "imputed/", ""))
  
  # Prepare data
  plotdf <- gather(frame, "Indicator", "Value", indepvars)
  plotdf$gender <- ifelse(plotdf$gender == "0", "Women", ifelse(plotdf$gender == "1", "Men", NA))
  plotdf$Indicator <- gsub('SHIHD_', "", plotdf$Indicator)
  
  # Scatterplot
  g <- ggplot() + 
    geom_point(data = plotdf, aes_string(x = "Value", y = targetvar), alpha = 0.6) +
    facet_grid(gender ~ Indicator, scales = "free_x") +
    my_theme() +
    labs(x = NULL, y = "Childlessness (%)") +
    theme(panel.spacing = unit(1.5, "lines")) 
  
  # Add correlation to plot
  splits <- split(plotdf, list(plotdf$gender, plotdf$Indicator))
  r <- unlist(lapply(splits, function(x){
    x <- x[, c("Value", targetvar)]
    comp <- x[complete.cases(x), ]
    return(sprintf("%.2f", mround(cor(comp)[1, 2], .01)))
  }))
  
  # Data for annotations for correlations in facets
  ann_text <- data.frame(gender = sub('\\..*', '', names(r)), 
                         Indicator = sub('.*\\.', '', names(r)),
                         Value = unlist(lapply(splits, function(x){max(na.omit(x$Value))})), 
                         targetvar = max(na.omit(frame[, targetvar])), 
                         lab = paste0('Corr = ', r)
  )
  g <- g + geom_label(data = ann_text, aes(x = Value*0.86, y = targetvar, label = lab, fontface = "italic"), 
                      label.padding = unit(0.25, "lines"), size = 3)
  
  # Get best model fits and fitted line data
  bestFit <- function(x, targetvar){
    comp <- x[complete.cases(x[, c(targetvar, "Value")]), ]
    lin <- lm(paste0(targetvar, ' ~ poly(Value, 1)'), data = comp)
    quad <- lm(paste0(targetvar, ' ~ poly(Value, 2)'), data = comp)
    if (compareModels == "anova"){
      ind <- ifelse(anova(lin, quad)[2,6] < anova_sig, 2, 1)
    } else {
      ind <- ifelse((AIC(quad) - AIC(lin)) < (-diffAIC), 2, 1)
    }
    pplot <- cbind(x[, c(targetvar, "Value")], predict(list(lin, quad)[[ind]], x, interval = "confidence"))
    return(pplot)
  }
  my_fits <- rbind.fill(lapply(splits, bestFit, targetvar))
  my_fits['gender'] <- rep(sub('\\..*', '', names(r)), unlist(lapply(splits, nrow)))
  my_fits['Indicator'] <- rep(sub('.*\\.', '', names(r)), unlist(lapply(splits, nrow)))
  
  # Add linear, quadratic or both fitted lines
  g1 <- g + geom_smooth(data = plotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ x)
  g2 <- g + geom_smooth(data = plotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ poly(x, 2))
  g3 <- g + geom_smooth(data = plotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ x) +
    geom_smooth(data = plotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                color = "green", fill = "green", linetype = "dashed", formula = y ~ poly(x, 2))
  g4 <- g + geom_line(data = my_fits, aes(x = Value, y = fit), size = 1, color = "red") + 
    geom_ribbon(data = my_fits, aes(x = Value, ymin = lwr, ymax = upr), fill = "red", alpha = 0.3)
  
  # Save plots in directories
  level <- ifelse(any(grepl("egion", names(frame))), "regional", ifelse(grepl("imputed", folder), "regional", "national"))
  if (!dir.exists(paste0(path, 'scatter_plots/', folder, level))){
    dir.create(paste0(path, 'scatter_plots/', folder, level), recursive = TRUE)
  }
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHDcomp_lin.png"), g1, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHDcomp_quad.png"), g2, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHDcomp_linquad.png"), g3, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHDcomp_bestfit.png"), g4, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  
  # Model goodness of fit
  fits <- lapply(splits, function(x){
    comp <- x[complete.cases(x[, c(targetvar, "Value")]), ]
    mod_lin <- lm(paste0(targetvar, " ~ poly(Value, 1)"), data = comp)
    mod_quad <- lm(paste0(targetvar, " ~ poly(Value, 2)"), data = comp)
    df <- data.frame(AIC = c(AIC(mod_lin), AIC(mod_quad)),
                     BIC = c(BIC(mod_lin), BIC(mod_quad)),
                     RMSE = c(rmse(mod_lin), rmse(mod_quad)),
                     pVal = c(NA, anova(mod_lin, mod_quad)[2, 6]))
    return(df)
  })
  df <- cbind(rbind.fill(fits[!grepl("Women", names(fits))]), rbind.fill(fits[grepl("Women", names(fits))]))
  colnames(df) <- paste0(names(df), "_", rep(c("Men", "Women"), each = length(names(df))/2))
  rownames(df) <- paste0(sub('.*\\.', '', names(fits)), "_", c("Linear", "Quadratic"))
  
  # Create folder
  if (!dir.exists(paste0(path, 'goodness_of_fit/', folder, level))){
    dir.create(paste0(path, 'goodness_of_fit/', folder, level), recursive = TRUE)
  }
  
  # Save goodness-of-fit
  output <- print(xtable(df, digits = rep(3, ncol(df) + 1)), sanitize.text.function = function(x){x})
  fileConn <- file(paste0(path, "goodness_of_fit/", folder, level, "/childlessSHIHD.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
  # Plot for only SHIHD, not the components
  subplotdf <- plotdf[plotdf$Indicator == "SHIHD", ]
  subfits <- my_fits[my_fits$Indicator == "SHIHD", ]
  g <- ggplot() +
    geom_point(data = subplotdf, aes_string(x = "Value", y = targetvar), alpha = 0.6) +
    facet_wrap(~ gender) +
    my_theme() +
    theme(panel.spacing = unit(1.5, "lines"))  +
    labs(x = "SHIHD", y = "Childlessness (%)") +
    geom_label(data = ann_text[ann_text$Indicator == "SHIHD", ], label.padding = unit(0.25, "lines"), size = 3,
               aes(x = Value*0.9, y = targetvar, label = lab, fontface = "italic"))
  g1 <- g + geom_smooth(data = subplotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ x)
  g2 <- g + geom_smooth(data = subplotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ poly(x, 2))
  g3 <- g + geom_smooth(data = subplotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                        color = "red", fill = "red", formula = y ~ x) +
    geom_smooth(data = subplotdf, aes_string(x = "Value", y = targetvar), method = "lm", 
                color = "green", fill = "green", linetype = "dashed", formula = y ~ poly(x, 2))
  g4 <- g + geom_line(data = subfits, aes(x = Value, y = fit), size = 1, color = "red") + 
    geom_ribbon(data = subfits, aes(x = Value, ymin = lwr, ymax = upr), fill = "red", alpha = 0.3)
  
  # Save plots in directories
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHD_lin.png"), g1, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHD_quad.png"), g2, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHD_linquad.png"), g3, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHD_bestfit.png"), g4, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  
  # Plot robust MCD estimator
  if (FALSE){
    if (targetvar == 'percChildlessness'){
      
      folder <- 'robust_mcd/'
      
      # Get data for best MCD fits for men and women and combine these
      newdata <- subfits[, c(targetvar, "Value", "gender")]
      sexes <- c("Men", "Women")
      pdat <- list()
      for (j in 1:length(sexes)){
        my_sub <- newdata[newdata$gender == sexes[j], ]
        my_sub <- my_sub[complete.cases(my_sub[, c(targetvar, "Value")]), ]
        lin <- mcdEstimates(my_sub, targetvar, 'Value', fit = 'lm')
        quad <- mcdEstimates(my_sub, targetvar, 'Value', fit = 'poly')
        my_F <- ((lin$RSS - quad$RSS)/(lin$df_res - quad$df_res)) / (quad$RSS / quad$df_res)
        p_val <- pf(my_F, lin$df_res - quad$df_res, quad$df_res, lower.tail = FALSE)
        my_sub[c("Value1", "Value2")] <- poly(my_sub$Value, 2)
        ind <- ifelse(p_val < 0.05, 2, 1)
        my_fit <- list(lin$my_fit, quad$my_fit)[[ind]]
        pdat[[j]] <- cbind(my_sub, predict(my_fit, my_sub, interval = "confidence"))
      }
      pdat <- rbind.fill(pdat)
      
      # Make plots with robust MCD estimator lines (linear, quadratic, both and best fit)
      g <- ggplot() +
        geom_point(data = pdat, aes_string(x = "Value", y = targetvar), alpha = 0.6) +
        facet_wrap(~ gender) +
        theme(panel.spacing = unit(1.5, "lines"))  +
        my_theme() +
        labs(x = "SHIHD", y = "Childlessness (%)") +
        geom_label(data = ann_text[ann_text$Indicator == "SHIHD", ], label.padding = unit(0.25, "lines"), size = 3,
                   aes(x = 0.9*max(pdat$Value), y = targetvar, label = lab, fontface = "italic")) +
        geom_line(data = pdat, aes(x = Value, y = fit), size = 1, color = "red") + 
        geom_ribbon(data = pdat, aes(x = Value, ymin = lwr, ymax = upr), fill = "red", alpha = 0.3)
      
      # Save plot in directory
      if (!dir.exists(paste0(path, 'scatter_plots/', folder, level))){
        dir.create(paste0(path, 'scatter_plots/', folder, level), recursive = TRUE)
      }
      ggsave(paste0(path, 'scatter_plots/', folder, level, "/childlessSHIHD_bestfit.png"), g, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    }
  }
  
}

# Childlessness types and SHIHD components (national and regional depending on input to frame)
scatterChildlessnessTypesSHIHD <- function(frame, path, compareModels, anova_sig, diffAIC){
  
  level <- ifelse(any(grepl("egion", names(frame))), "regional", "national")
  sexes <- c("Men", "Women")
  fits <- list()
  for (i in 1:length(unique(frame$gender))){
    
    plotdf <- frame
    plotdf$gender <- ifelse(plotdf$gender == "0", "Women", ifelse(plotdf$gender == "1", "Men", NA))
    
    # Prepare data
    plotdf <- plotdf[plotdf$gender == sexes[i], ]
    plotdf <- gather(plotdf, "Indicator", "shihdValue", paste0("SHIHD", c("", "_Education", "_Health", "_Income")))
    plotdf <- gather(plotdf, "typeChildless", "childlessValue", paste0("typeChildless", c("voluntary", "involuntary", "circumstantial", "undecided")))
    plotdf$Indicator <- gsub('SHIHD_', "", plotdf$Indicator)
    plotdf$typeChildless <- capitalize(gsub("typeChildless", "", plotdf$typeChildless))
    plotdf$childlessValue <- 100 * plotdf$childlessValue
    plotdf <- plotdf[plotdf$typeChildless != "Undecided", ]
    
    # Scatterplot
    g <- ggplot() +
      geom_point(data = plotdf, aes(x = shihdValue, y = childlessValue), alpha = 0.6) +
      facet_grid(typeChildless ~ Indicator, scales = "free_x") +
      my_theme() +
      labs(x = NULL, y = "Childlessness (%)") +
      theme(panel.spacing = unit(1, "lines")) 
    
    # Add correlation to plot
    splits <- split(plotdf, list(plotdf$Indicator, plotdf$typeChildless))
    r <- unlist(lapply(splits, function(x){
      x <- x[, c("shihdValue", 'childlessValue')]
      comp <- x[complete.cases(x), ]
      return(sprintf("%.2f", mround(cor(comp)[1, 2], .01)))
    }))
    
    # Data for text annotation for correlations in facets
    ann_text <- data.frame(Indicator = sub('\\..*', '', names(r)),
                           typeChildless = sub('.*\\.', '', names(r)),
                           childlessValue = max(na.omit(plotdf$childlessValue)), 
                           shihdValue = unlist(lapply(splits, function(x){max(na.omit(x$shihdValue))})),  
                           lab = paste0('Corr = ', r)
    )
    g <- g + geom_label(data = ann_text, aes(x = shihdValue*0.83, y = 0.95*childlessValue, label = lab, fontface = "italic"), 
                        label.padding = unit(0.25, "lines"), size = 3)
    
    bestFit <- function(x){
      comp <- x[complete.cases(x[, c('shihdValue', 'childlessValue')]), ]
      lin <- lm(childlessValue ~ poly(shihdValue, 1), data = comp)
      quad <- lm(childlessValue ~ poly(shihdValue, 2), data = comp)
      if (compareModels == "anova"){
        ind <- ifelse(anova(lin, quad)[2,6] < anova_sig, 2, 1)
      } else {
        ind <- ifelse((AIC(quad) - AIC(lin)) < (-diffAIC), 2, 1)
      }
      pplot <- cbind(x[, c("childlessValue", "shihdValue")], predict(list(lin, quad)[[ind]], x, interval = "confidence"))
      return(pplot)
    }
    my_fits <- rbind.fill(lapply(splits, bestFit))
    my_fits['typeChildless'] <- rep(sub('.*\\.', '', names(r)), unlist(lapply(splits, nrow)))
    my_fits['Indicator'] <- rep(sub('\\..*', '', names(r)), unlist(lapply(splits, nrow)))
    
    # Add linear, quadratic or both fitted lines
    g1 <- g + geom_smooth(data = plotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ x)
    g2 <- g + geom_smooth(data = plotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ poly(x, 2))
    g3 <- g + geom_smooth(data = plotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ x) + 
      geom_smooth(data = plotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                  color = "green", fill = "green", linetype = "dashed", formula = y ~ poly(x, 2))
    g4 <- g + geom_line(data = my_fits, aes(x = shihdValue, y = fit), size = 1, color = "red") + 
      geom_ribbon(data = my_fits, aes(x = shihdValue, ymin = lwr, ymax = upr), fill = "red", alpha = 0.3)
    
    if (!dir.exists(paste0(path, 'scatter_plots/', level))){
      dir.create(paste0(path, 'scatter_plots/', level), recursive = TRUE)
    }
    
    # Save plots to folder
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHDcomp_lin", "_", sexes[i],".png"), g1, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHDcomp_quad", "_", sexes[i],".png"), g2, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHDcomp_linquad", "_", sexes[i],".png"), g3, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHDcomp_bestfit", "_", sexes[i],".png"), g4, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    
    # Model goodness of fit
    fits[[i]] <- lapply(splits, function(x){
      comp <- x[complete.cases(x[, c("shihdValue", "childlessValue")]), ]
      mod_lin <- lm(childlessValue ~ poly(shihdValue, 1), data = comp)
      mod_quad <- lm(childlessValue ~ poly(shihdValue, 2), data = comp)
      df <- data.frame(AIC = c(AIC(mod_lin), AIC(mod_quad)),
                       BIC = c(BIC(mod_lin), BIC(mod_quad)),
                       RMSE = c(rmse(mod_lin), rmse(mod_quad)),
                       pVal = c(NA, anova(mod_lin, mod_quad)[2, 6]))
      return(df)
    })
    
    # Plot for only SHIHD, not the components
    subplotdf <- plotdf[plotdf$Indicator == "SHIHD", ]
    g <- ggplot() +
      geom_point(data = plotdf, aes(x = shihdValue, y = childlessValue), alpha = 0.6) +
      facet_wrap(~ typeChildless) +
      my_theme() +
      labs(x = "SHIHD", y = "Childlessness (%)") +
      theme(panel.spacing = unit(1, "lines"))  +
      geom_label(data = ann_text[ann_text$Indicator == "SHIHD", ], aes(x = shihdValue*0.83, y = 0.95*childlessValue, label = lab, fontface = "italic"), 
                 label.padding = unit(0.25, "lines"), size = 3)
    
    # Add linear, quadratic or both fitted lines
    g1 <- g + geom_smooth(data = subplotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ x)
    g2 <- g + geom_smooth(data = subplotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ poly(x, 2))
    g3 <- g + geom_smooth(data = subplotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                          color = "red", fill = "red", formula = y ~ x) + 
      geom_smooth(data = subplotdf, aes(x = shihdValue, y = childlessValue), method = "lm", 
                  color = "green", fill = "green", linetype = "dashed", formula = y ~ poly(x, 2))
    g4 <- g + geom_line(data = my_fits[my_fits$Indicator == "SHIHD", ], aes(x = shihdValue, y = fit), size = 1, color = "red") + 
      geom_ribbon(data = my_fits[my_fits$Indicator == "SHIHD", ], aes(x = shihdValue, ymin = lwr, ymax = upr), fill = "red", alpha = 0.3)
    
    # Save plots to folder
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHD_lin", "_", sexes[i],".png"), g1, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHD_quad", "_", sexes[i],".png"), g2, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHD_linquad", "_", sexes[i],".png"), g3, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    ggsave(paste0(path, 'scatter_plots/', level, "/typeChildlessSHIHD_bestfit", "_", sexes[i],".png"), g4, dpi = 600, height = 16.933, width = 21.768, units = "cm")
    
  }
  
  # Combine goodness of fit for men and women
  df <- cbind(rbind.fill(fits[[1]]), rbind.fill(fits[[2]]))
  colnames(df) <- paste0(names(df), "_", rep(c("Men", "Women"), each = length(names(df))/2))
  rownames(df) <- paste0(rep(names(fits[[1]]), each = 2), "_",c("Linear", "Quadratic"))
  
  # Create folder
  if (!dir.exists(paste0(path, 'goodness_of_fit/', level))){
    dir.create(paste0(path, 'goodness_of_fit/', level), recursive = TRUE)
  }
  
  # Save goodness-of-fit
  output <- print(xtable(df, digits = rep(3, ncol(df) + 1)), sanitize.text.function = function(x){x})
  fileConn <- file(paste0(path, "goodness_of_fit/", level, "/typesChildlessSHIHD.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}