# Find all even indices in vector
even <- function(x) x%%2 == 0 

# Recover doctor and nurse delivery for males from female data
# Recover polygamy for females from male data (if missing in female data)
recoverVariables <- function(x){
  if (!is.na(x$deliveryDoctor1[x$gender == "0"])){
    x$deliveryDoctor1[x$gender == "1"] <- x$deliveryDoctor1[x$gender == "0"]
  }
  if (!is.na(x$deliveryNurse1[x$gender == "0"])){
    x$deliveryNurse1[x$gender == "1"] <- x$deliveryNurse1[x$gender == "0"]
  }
  if (!is.na(x$polygamy1[x$gender == "1"]) & is.na(x$polygamy1[x$gender == "0"])){
    x$polygamy1[x$gender == "0"] <- x$polygamy1[x$gender == "1"]
  }
  return(x)
}

# Plot missingness on the regional and individual levels
plotMissingness <- function(data, vars, nams, level){
  
  uni <- unique(data$gender)
  for (i in 1:length(uni)){
    
    # Create directory for missingness results
    sexe <- ifelse(uni[i] == "0", "Women", ifelse(uni[i] == "1", "Men", NA))
    if (!dir.exists(paste0(path, 'missingness'))){
      dir.create(paste0(path, 'missingness'))
    }
    dat <- data[data$gender == uni[i], vars]
    
    # Plot aggregation missingness plot
    aggr(dat, delimiter = NULL, plot = TRUE, col = c("skyblue", "red", "orange"), bars = TRUE,
         numbers = FALSE, prop = TRUE, combined = FALSE, varheight = FALSE,
         only.miss = FALSE, border = par("fg"), sortVars = FALSE,
         sortCombs = TRUE, ylabs = NULL, axes = TRUE, labels = nams, cex.lab = 0.69, 
         cex.axis = 0.55, cex.numbers = par("cex"), gap = 2, las = 2, ylab = 'Proportion of Missings')
    dev.copy(png, paste0(path, 'missingness/aggr_', level, '_', sexe, '.png'))
    dev.off()
    
    # Plot missingness pattern
    # md.pattern(dat)
    # dev.copy(png, paste0(path, 'missingness/pattern_', level, "_", sexe,'.png'))
    # dev.off()
    
    # Output missingness pattern information to text
    # md <- md.pattern(dat)
    # output <- print(xtable(md, digits = rep(3, ncol(md) + 1)), include.rownames = TRUE)
    # fileConn <- file(paste0(path, "missingness/mdpattern_", level, "_", sexe, ".txt"))
    # writeLines(output, fileConn)
    # close(fileConn)
    
  }
}

# Descriptive statistics of non-imputed data aggregated from individual imputed data
descriptiveStatistics <- function(inddata, regdata, vars, nams){
  
  # Compute descriptive statistics in nice table
  perc_vars <- c("HIV", "nationalChildlessnessAgeadjusted", "nationalChildlessness", "residencerural", "residenceurban",    
                 paste0("typeChildless", c("involuntary", "undecided", "voluntary", "children", "circumstantial")), 
                 paste0("polygamyExtensive", c("0", "1", "not.marr.coh.rel", "uncertain")), paste0("delivery", c("Doctor1", "Nurse1")),
                 "polygamy1", "childless_dummy1", paste0("marital", c("div.sep", "marr.coh.rel", "nevermarr", "wid")),"percChildlessness")
  regdata[, perc_vars] <- regdata[, perc_vars] * 100
  sexes <- unique(regdata$gender)
  gens <- ifelse(sexes == "0", "Women", ifelse(sexes == "1", "Men", NA))
  desclist <- list()
  nams <- c(nams, c("N_ind", "N_individuals", "N_subnational_regions", "N_subnational_region_year_combinations)"))
  colnams <- c("Mean", "St. Dev.", "Min", "Max", "N (%)")
  for (i in 1:length(sexes)){
    dat <- regdata[regdata$gender == sexes[i], ]
    df <- data.frame(matrix(NA, nrow = length(nams), ncol = length(colnams)))
    rownames(df) <- nams
    colnames(df) <- colnams
    df$Mean[1:length(vars)] <- mround(apply(dat[, vars], 2, function(x){mean(na.omit(x))}), .001)
    df$`St. Dev.`[1:length(vars)] <- mround(apply(dat[, vars], 2, function(x){sd(na.omit(x))}), .001)
    df$Min[1:length(vars)] <- mround(apply(dat[, vars], 2, function(x){min(na.omit(x))}), .001)
    df$Max[1:length(vars)] <- mround(apply(dat[, vars], 2, function(x){max(na.omit(x))}), .001)
    df$`N (%)`[1:length(vars)] <- mround(apply(dat[, vars], 2, function(x){(1 - sum(is.na(x))/length(x))*100}), .001)
    subind <- subset(inddata, gender == sexes[i])
    splits <- split(subind, list(subind$year, subind$newRegion, subind$fullCountry))
    splits <- splits[unlist(lapply(splits, nrow)) > 0]
    nrows <- unlist(lapply(splits, nrow))
    df[length(vars) + 1, ] <- c(mround(mean(nrows), .001), mround(sd(nrows), .001), mround(min(nrows), .001), mround(max(nrows), .001), NA)
    df$Mean[(length(vars) + 2):nrow(df)] <- round(c(nrow(inddata[inddata$gender == sexes[i], ]), 
                                                    nrow(unique(dat[, c("fullCountry", "newRegion")])), nrow(dat)))
    desclist[[i]] <- df
  }
  descdf <- cbind(desclist[[2]], desclist[[1]])
  descdf <- rbind(Gender = rep(rev(gens), each = 5), descdf)
  rownames(descdf) <- gsub("_", " ", rownames(descdf))
  
  # Print results to table
  # output <- print(xtable(descdf, digits = rep(3, ncol(descdf) + 1)), include.rownames = TRUE, 
  #                 sanitize.text.function = function(x){x})
  # fileConn <- file(paste0(path, "descriptiveStatistics_NA.txt"))
  # writeLines(output, fileConn)
  # close(fileConn)
  
  # Print results to table
  output <- print(xtable(descdf[, !grepl("%", names(descdf))], digits = rep(3, ncol(descdf) - 1)), include.rownames = TRUE, 
                  sanitize.text.function = function(x){x})
  fileConn <- file(paste0(path, "descriptiveStatistics.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}

# Pool model criteria and results and goodness of fit measures
poolResults <- function(fit, fitname, gender, random = c(TRUE, FALSE)){
  
  if (!dir.exists(paste0(path, 'multilevel_results/ANOVA'))){
    dir.create(paste0(path, 'multilevel_results/ANOVA'), recursive = TRUE)
  }
  
  pool.fit <- pool(fit)
  assign(paste0('pool.', fitname), pool.fit, envir = globalenv())
  
  aic.fit <- c(mean(unlist(lapply(fit$analyses, AIC))), sd(unlist(lapply(fit$analyses, AIC))))
  assign(paste0('aic.', fitname), aic.fit, envir = globalenv())
  
  bic.fit <- c(mean(unlist(lapply(fit$analyses, BIC))), sd(unlist(lapply(fit$analyses, BIC))))
  assign(paste0('bic.', fitname), bic.fit, envir = globalenv())
  
  logLik.fit <- c(mean(unlist(lapply(fit$analyses, logLik))), sd(unlist(lapply(fit$analyses, logLik))))
  assign(paste0('logLik.', fitname), logLik.fit, envir = globalenv())
  
  if (class(fit$analyses[[1]]) == "lm"){
    rmse.fit <- c(mean(unlist(lapply(fit$analyses, rmse))), sd(unlist(lapply(fit$analyses, rmse))))
  } else if (class(fit$analyses[[1]]) == "lmerMod"){
    rmse.fit <- c(mean(unlist(lapply(fit$analyses, RMSE.merMod))), sd(unlist(lapply(fit$analyses, RMSE.merMod))))
  }
  assign(paste0('rmse.', fitname), rmse.fit, envir = globalenv())
  
  if (random == TRUE){ randomEffects(fit, fitname, gender) }
  
}

# Pool ggeffects predictions across multiple imputations
# Source:https://github.com/strengejacke/ggeffects/blob/master/R/pool_predictions.R
pool_predictions <- function(x, ...) {
  
  # check input -----
  
  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x
  
  if (!all(sapply(x, inherits, "ggeffects"))) {
    stop("'x' must be a list of 'ggeffects' objects, as returned by 'ggpredict()', 'ggemmeans()' or 'ggeffect()'.", call. = FALSE)
  }
  
  # check if all x-levels are identical
  if (!all(apply(as.data.frame(sapply(x, function(i) i$x), simplify = TRUE), 1, function(j) length(unique(j)) == 1))) {
    stop(paste0("Cannot pool predictions. The values of the focal term '", attributes(x[[1]])$terms,"' are not identical across predictions."), call. = FALSE)
  }
  
  # preparation ----
  
  len <- length(x)
  ci <- attributes(x[[1]])$ci.lvl
  if (is.null(ci)){
    ci = unique(round(1-2*pnorm((x[[1]]$conf.high - x[[1]]$predicted) / x[[1]]$std.error,lower.tail=FALSE), 2))
  }
  link_inv <- attributes(x[[1]])$link_inverse
  link_fun <- attributes(x[[1]])$link_function
  
  if (is.null(link_inv)) {
    link_inv <- function(x) x
  }
  if (is.null(link_fun)) {
    link_fun <- function(x) x
  }
  
  # pool predictions -----
  
  pooled_predictions <- original_x[[1]]
  n_rows <- nrow(original_x[[1]])
  
  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) {
      link_fun(j$predicted[i])
    }))
    pooled_predictions$predicted[i] <- mean(pooled_pred, na.rm = TRUE)
    
    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) {
      j$std.error[i]
    }))
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$std.error[i] <- sqrt(tmp)
  }
  
  # confidence intervals ----
  
  alpha <- (1 + ci) / 2
  fac <- stats::qnorm(alpha)
  pooled_predictions$conf.low <- link_inv(pooled_predictions$predicted - fac * pooled_predictions$std.error)
  pooled_predictions$conf.high <- link_inv(pooled_predictions$predicted + fac * pooled_predictions$std.error)
  
  # backtransform
  pooled_predictions$predicted <- link_inv(pooled_predictions$predicted)
  
  # constant values
  constant.values <- as.data.frame(do.call(rbind, lapply(original_x, function(x) {
    as.data.frame(attributes(x)$constant.values)
  })))
  
  attr(pooled_predictions, "constant.values") <- lapply(constant.values, function(i) {
    if (is.numeric(i)) {
      mean(i)
    } else {
      unique(i)
    }
  })
  
  pooled_predictions
}

# Extract pooled random effects
randomEffects <- function(fit, filename, gender){
  
  # Compute random effects for model
  sdvar <- lapply(fit$analyses, function(x){as.data.frame(VarCorr(x))[, c('vcov', 'sdcor')]})
  info <- lapply(fit$analyses, function(x){as.data.frame(VarCorr(x))[, c('grp', 'var1', 'var2')]})
  sdvar_pool <- Reduce("+", sdvar) / length(sdvar)
  re <- cbind(info[[1]], sdvar_pool)
  colls <- ncol(ranef(fit$analyses[[1]])[[1]])
  re['corr'] <- NA
  if (colls == 2){
    corr_intercept_poly1 <- mean(unlist(lapply(fit$analyses, function(x){cor(ranef(x)[[1]])[1, 2]})))
    re$corr[grepl("Inter", re$var1) & grepl("poly", re$var2)] <- corr_intercept_poly1
  } else if (colls == 3){
    corr_intercept_poly1 <-  mean(unlist(lapply(fit$analyses, function(x){cor(ranef(x)[[1]])[1, 2]})))
    corr_intercept_poly2 <-  mean(unlist(lapply(fit$analyses, function(x){cor(ranef(x)[[1]])[1, 3]})))
    corr_poly1_poly2 <-  mean(unlist(lapply(fit$analyses, function(x){cor(ranef(x)[[1]])[2, 3]})))
    re$corr[grepl("Inter", re$var1) & grepl(")1", re$var2)] <- corr_intercept_poly1
    re$corr[grepl("Inter", re$var1) & grepl(")2", re$var2)] <- corr_intercept_poly2
    re$corr[grepl(")1", re$var1) & grepl(")2", re$var2)] <- corr_poly1_poly2
  }
  
  # Output results
  if (!dir.exists(paste0(path, 'multilevel_results/randomEffects'))){
    dir.create(paste0(path, 'multilevel_results/randomEffects'), recursive = TRUE)
  }
  output <- print(xtable(re, digits = rep(3, ncol(re) + 1)), include.rownames = FALSE)
  fileConn <- file(paste0(path, "multilevel_results/randomEffects/", gender, filename, ".txt"))
  writeLines(output, fileConn)
  close(fileConn)
}

# Multilevel models grouped by variables
# Multilevel models grouped by variables
multilevelModelling = function(
  imp, 
  depvar, 
  ageres, 
  marnupt, 
  polydel,
  HIV = c(TRUE, FALSE), 
  sexRatios = c(TRUE, FALSE), 
  gender = c('women', 'men')
){
  
  if (!dir.exists(paste0(path, 'multilevel_results/ANOVA'))){
    dir.create(paste0(path, 'multilevel_results/ANOVA'), recursive = TRUE)
  }
  
  # Intercept model
  message("Fitting the intercept model.")
  intercept = suppressWarnings(with(imp, lm(as.formula(paste0(depvar, ' ~ 1')))))
  poolResults(intercept, 'intercept', gender, random = FALSE)
  
  # Random intercept model
  message("Fitting the random intercept model.")
  randomIntercept = suppressWarnings(with(imp, lmer(as.formula(paste0(depvar, ' ~ 1 + (1 | fullCountry)')))))
  poolResults(randomIntercept, 'randomIntercept', gender, random = FALSE)
  
  # Random intercept with time model
  message("Fitting the linear SHIHD random intercept model.")
  timeRI = suppressWarnings(with(imp, lmer(as.formula(paste0(depvar, ' ~ poly(SHIHD, 1) + (1 | fullCountry)')))))
  poolResults(timeRI, 'timeRI', gender, random = FALSE)
  
  # Random intercept and slope with time model
  message("Fitting the linear SHIHD random slopes model.")
  timeRS = suppressWarnings(with(imp, lmer(as.formula(paste0(depvar, ' ~ poly(SHIHD, 1) + (1 + poly(SHIHD, 1) | fullCountry)')))))
  poolResults(timeRS, 'timeRS', gender, random = FALSE)
  
  # Add second-order time polynomial
  message("Fitting the quadratic SHIHD random slopes model.")
  timeQ = suppressWarnings(with(imp, lmer(as.formula(paste0(depvar, ' ~ poly(SHIHD, 2) + (1 + poly(SHIHD, 2) | fullCountry)')))))
  poolResults(timeQ, 'timeQ', gender, random = FALSE)
  
  # Add third-order time polynomial (commented out because no improvement in model fit)
  # timeC = with(imp, lmer(as.formula(paste0(depvar, ' ~ poly(time, 3) + (1 + poly(SHIHD, 2) | fullCountry)'))))
  # poolResults(timeC, 'timeC', gender, random = FALSE)
  
  diff = c(-2*logLik.intercept[1]-(-2*logLik.randomIntercept[1]), 
           -2*logLik.randomIntercept[1]-(-2*logLik.timeRI[1]),
           -2*logLik.timeRI[1]-(-2*logLik.timeRS[1]), 
           -2*logLik.timeRS[1]-(-2*logLik.timeQ[1]))
  pvals = pchisq(diff, df = 2, lower.tail=FALSE)
  
  colls = c('AIC', 'BIC', 'RMSE', 'P-value')
  tab = data.frame(matrix(NA, nrow = length(diff) + 1, ncol = length(colls)))
  colnames(tab) = colls
  rownames(tab) = c('intercept', 'randomIntercept', 'timeRI', 'timeRS', 'timeQ')
  tab[1, ] = c(aic.intercept[1], bic.intercept[1], rmse.intercept[1], NA)
  tab[2, ] = c(aic.randomIntercept[1], bic.randomIntercept[1], rmse.randomIntercept[1], pvals[1])
  tab[3, ] = c(aic.timeRI[1], bic.timeRI[1], rmse.timeRI[1], pvals[2])
  tab[4, ] = c(aic.timeRS[1], bic.timeRS[1], rmse.timeRS[1], pvals[3])
  tab[5, ] = c(aic.timeQ[1], bic.timeQ[1], rmse.timeQ[1], pvals[4])
  
  output = print(xtable(tab, digits = rep(3, ncol(tab) + 1)), include.rownames = TRUE)
  fileConn = file(paste0(path, 'multilevel_results/ANOVA/', gender, 'basicModels.txt'))
  writeLines(output, fileConn)
  close(fileConn)
  
  # Specify model variables and formulae
  if (HIV == TRUE){
    polydel = c(polydel, 'HIV')
  }
  if (sexRatios == TRUE){
    polydel = c(polydel, 'sexRatio')
  }
  final = c(ageres, marnupt, polydel)
  
  # Fit models
  form_shihd = paste0(depvar, ' ~ poly(time, 1) + poly(SHIHD, 1) + (1 + poly(SHIHD, 1) | fullCountry)')
  form_shihd2 = paste0(depvar, ' ~ poly(time, 2) + poly(SHIHD, 2) + (1 + poly(SHIHD, 2) | fullCountry)')
  message("Fitting the linear SHIHD model.")
  fitSHIHD = suppressWarnings(with(imp, lmer(form_shihd)))
  message("Fitting the quadratic SHIHD model.")
  fitSHIHD2 = suppressWarnings(with(imp, lmer(form_shihd2)))
  message("Fitting the linear age and residence model.")
  fitAgeRes = suppressWarnings(with(imp, lmer(paste0(form_shihd, ' + ', paste(ageres, collapse = " + ")))))
  message("Fitting the quadratic age and residence model.")
  fitAgeRes2 = suppressWarnings(with(imp, lmer(paste0(form_shihd2, ' + ', paste(ageres, collapse = " + ")))))
  message("Fitting the linear marriage and nuptiality model.")
  fitMarNupt = suppressWarnings(with(imp, lmer(paste0(form_shihd, ' + ', paste(marnupt, collapse = " + ")))))
  message("Fitting the quadratic marriage and nuptiality model.")
  fitMarNupt2 = suppressWarnings(with(imp, lmer(paste0(form_shihd2, ' + ', paste(marnupt, collapse = " + ")))))
  message("Fitting the linear polygyny and delivery model.")
  fitPolyDel = suppressWarnings(with(imp, lmer(paste0(form_shihd, ' + ', paste(polydel, collapse = " + ")))))
  message("Fitting the quadratic polygyny and delivery model.")
  fitPolyDel2 = suppressWarnings(with(imp, lmer(paste0(form_shihd2, ' + ', paste(polydel, collapse = " + ")))))
  message("Fitting the linear total model.")
  fitFinal = suppressWarnings(with(imp, lmer(paste0(form_shihd, ' + ', paste(final, collapse = " + ")))))
  message("Fitting the quadratic total model.")
  fitFinal2 = suppressWarnings(with(imp, lmer(paste0(form_shihd2, ' + ', paste(final, collapse = " + ")))))
  
  fe_names = names(fixef(fitFinal2$analyses[[1]]))
  indep_vars = c()
  if (any(grepl("time", fe_names))){ indep_vars = c(indep_vars, "time") }
  if (any(grepl("SHIHD", fe_names))){ indep_vars = c(indep_vars, "SHIHD") }
  indep_vars = c(indep_vars, fe_names[!grepl("Intercept", fe_names) & !grepl("poly\\(", fe_names)])
  
  # Matching data to get nice variable names
  matchdf = data.frame(
    original = c("time", "SHIHD","age","residenceurban","maritalnevermarr","maritaldiv.sep","maritalwid","ageBirth",       
                 "ageFirstCohabMar", "ageFirstSex","HIV","sexRatio", "polygamy1"),
    new = c("Time", "SHIHD", "Average age", "Urban residence (%)", "Never married (%)", "Divorced/separated (%)",
            "Widowed (%)", "Average age at first birth", "Average age at first marriage/cohabitation", 
            "Average age at first sex", "HIV prevalence (%)", "Number of women per 100 men", "Polygyny prevalence (%)")
  )
  
  # Compute and plot marginal effects for total models
  if (!dir.exists(paste0(path, "marginal_effects/"))){
    dir.create(paste0(path, "marginal_effects/"), recursive = TRUE)
  }
  ggeff_pool_linear = ggeff_pool_quadratic = vector(mode = "list", length = length(indep_vars))
  for (j in 1:length(indep_vars)){
    predictions_linear = predictions_quadratic = vector(mode = "list", length = imp$m)
    for (i in 1:imp$m){
      imputed_dataset = mice::complete(imp, i)
      assign("imputed_dataset", imputed_dataset, envir = globalenv())
      fmla_lin = paste0(form_shihd, ' + ', paste(final, collapse = " + "))
      mod_lin = suppressWarnings(lme4::lmer(fmla_lin, data = imputed_dataset))
      fmla_quad = paste0(form_shihd2, ' + ', paste(final, collapse = " + "))
      mod_quad = suppressWarnings(lme4::lmer(fmla_quad, data = imputed_dataset))
      if (i == 1){ 
        marg_terms = paste0(indep_vars[j], " [all]")
        predictions_linear[[i]] = suppressWarnings(ggeffects::ggpredict(model = mod_lin, terms = marg_terms))
        assign("marginal_values_lin", predictions_linear[[i]]$x, envir = globalenv())
        predictions_quadratic[[i]] = suppressWarnings(ggeffects::ggpredict(model = mod_quad, terms = marg_terms))
        assign("marginal_values_quad", predictions_quadratic[[i]]$x, envir = globalenv())
      } else {
        marg_terms_lin = paste0(indep_vars[j], " ", paste0("[", paste(marginal_values_lin, collapse = ", "), "]"))
        predictions_linear[[i]] = suppressWarnings(ggeffects::ggpredict(mod_lin, terms = marg_terms_lin))
        marg_terms_quad = paste0(indep_vars[j], " ", paste0("[", paste(marginal_values_quad, collapse = ", "), "]"))
        predictions_quadratic[[i]] = suppressWarnings(ggeffects::ggpredict(mod_quad, terms = marg_terms_quad))
      }
    }
    ggeff_pool_linear[[indep_vars[j]]] = pool_predictions(predictions_linear)
    ggeff_pool_linear[[indep_vars[j]]]["independent"] = as.character(matchdf$new[matchdf$original == indep_vars[j]])
    ggeff_pool_quadratic[[indep_vars[j]]] = pool_predictions(predictions_quadratic)
    ggeff_pool_quadratic[[indep_vars[j]]]["independent"] = as.character(matchdf$new[matchdf$original == indep_vars[j]])
    rm(marginal_values_lin, marginal_values_quad, predictions_linear, predictions_quadratic)
  }
  ggeff_pool_linear_df = rbind.fill(ggeff_pool_linear)
  write.csv(ggeff_pool_linear_df, paste0(path, 'marginal_effects/marginal_effects_linear_', gender, '.csv'))
  ggeff_pool_quadratic_df = rbind.fill(ggeff_pool_quadratic)
  write.csv(ggeff_pool_quadratic_df, paste0(path, 'marginal_effects/marginal_effects_quadratic_', gender, '.csv'))
  rm(ggeff_pool_linear)
  rm(ggeff_pool_quadratic)
  
  # Plot marginal effects (one facet per independent variable) for full models
  marg = ggplot() + 
    my_theme() +
    labs(x="Value of the independent variable", y = "Childlessness (%, predicted)") +
    facet_wrap(~independent, scales = "free_x", labeller = labeller(independent = label_wrap_gen(22))) +
    theme(panel.spacing = unit(1.25, "lines"))
  marg_linear = marg +
    geom_line(data=ggeff_pool_linear_df, aes(x = x, y = 100*predicted)) +
    geom_ribbon(data=ggeff_pool_linear_df, aes(x = x, ymin = 100*conf.low, ymax = 100*conf.high), alpha = 0.5) 
  ggsave(paste0(path, 'marginal_effects/marginal_effects_linear_', gender, '.png'), marg_linear, dpi = 600, height = 20, width = 25, units = "cm")
  marg_quadratic = marg + 
    geom_line(data=ggeff_pool_quadratic_df, aes(x = x, y = 100*predicted)) +
    geom_ribbon(data=ggeff_pool_quadratic_df, aes(x = x, ymin = 100*conf.low, ymax = 100*conf.high), alpha = 0.5) 
  ggsave(paste0(path, 'marginal_effects/marginal_effects_quadratic_', gender, '.png'), marg_quadratic, dpi = 600, height = 20, width = 25, units = "cm")
  
  # Pool model results over imputed data sets
  poolResults(fitSHIHD, 'fitSHIHD', gender, random = TRUE)
  poolResults(fitSHIHD2, 'fitSHIHD2', gender, random = TRUE)
  poolResults(fitAgeRes, 'fitAgeRes', gender, random = TRUE)
  poolResults(fitAgeRes2, 'fitAgeRes2', gender, random = TRUE)
  poolResults(fitMarNupt, 'fitMarNupt', gender, random = TRUE)
  poolResults(fitMarNupt2, 'fitMarNupt2', gender, random = TRUE)
  poolResults(fitPolyDel, 'fitPolyDel', gender, random = TRUE)
  poolResults(fitPolyDel2, 'fitPolyDel2', gender, random = TRUE)
  poolResults(fitFinal, 'fitFinal', gender, random = TRUE)
  poolResults(fitFinal2, 'fitFinal2', gender, random = TRUE)
  
  # Compare linear and quadratic fit
  capture.output(anova(fitSHIHD, fitSHIHD2), file = paste0(path, 'multilevel_results/ANOVA/', gender, 'SHIHD.txt'))
  capture.output(anova(fitAgeRes, fitAgeRes2), file = paste0(path, 'multilevel_results/ANOVA/', gender, 'ageRes.txt'))
  capture.output(anova(fitMarNupt, fitMarNupt2), file = paste0(path, 'multilevel_results/ANOVA/', gender, 'marNupt.txt'))
  capture.output(anova(fitPolyDel, fitPolyDel2), file = paste0(path, 'multilevel_results/ANOVA/', gender, 'polyDel.txt'))
  capture.output(anova(fitFinal, fitFinal2), file = paste0(path, 'multilevel_results/ANOVA/', gender, 'Final.txt'))
  
  res = list(intercept = pool.intercept, randomIntercept = pool.randomIntercept, timeRI = pool.timeRI, 
             timeRS = pool.timeRS, timeQ = pool.timeQ, 
             fitSHIHD = pool.fitSHIHD, fitSHIHD2 = pool.fitSHIHD2, fitFinal = pool.fitFinal, fitFinal2 = pool.fitFinal2,
             fitAgeRes = pool.fitAgeRes, fitAgeRes2 = pool.fitAgeRes2, fitMarNupt = pool.fitMarNupt, fitMarNupt2 = pool.fitMarNupt2, 
             fitPolyDel = pool.fitPolyDel, fitPolyDel2 = pool.fitPolyDel2,
             
             aic.intercept = aic.intercept, aic.randomIntercept = aic.randomIntercept, aic.timeRI = aic.timeRI, 
             aic.timeRS = aic.timeRS, aic.timeQ = aic.timeQ, 
             aic.fitSHIHD = aic.fitSHIHD, aic.fitSHIHD2 = aic.fitSHIHD2, aic.fitFinal = aic.fitFinal, aic.fitFinal2 = aic.fitFinal2,
             aic.fitAgeRes = aic.fitAgeRes, aic.fitAgeRes2 = aic.fitAgeRes2, aic.fitMarNupt = aic.fitMarNupt, 
             aic.fitMarNupt2 = aic.fitMarNupt2, aic.fitPolyDel = aic.fitPolyDel, aic.fitPolyDel2 = aic.fitPolyDel2,
             
             bic.intercept = bic.intercept, bic.randomIntercept = bic.randomIntercept, bic.timeRI = bic.timeRI, 
             bic.timeRS = bic.timeRS, bic.timeQ = bic.timeQ, 
             bic.fitSHIHD = bic.fitSHIHD, bic.fitSHIHD2 = bic.fitSHIHD2, bic.fitFinal = bic.fitFinal, bic.fitFinal2 = bic.fitFinal2,
             bic.fitAgeRes = bic.fitAgeRes, bic.fitAgeRes2 = bic.fitAgeRes2, bic.fitMarNupt = bic.fitMarNupt, 
             bic.fitMarNupt2 = bic.fitMarNupt2, bic.fitPolyDel = bic.fitPolyDel, bic.fitPolyDel2 = bic.fitPolyDel2,
             
             logLik.intercept = logLik.intercept, logLik.randomIntercept = logLik.randomIntercept, logLik.timeRI = logLik.timeRI, 
             logLik.timeRS = logLik.timeRS, logLik.timeQ = logLik.timeQ, 
             logLik.fitSHIHD = logLik.fitSHIHD, logLik.fitSHIHD2 = logLik.fitSHIHD2, logLik.fitFinal = logLik.fitFinal, logLik.fitFinal2 = logLik.fitFinal2,
             logLik.fitAgeRes = logLik.fitAgeRes, logLik.fitAgeRes2 = logLik.fitAgeRes2, logLik.fitMarNupt = logLik.fitMarNupt, 
             logLik.fitMarNupt2 = logLik.fitMarNupt2, logLik.fitPolyDel = logLik.fitPolyDel, logLik.fitPolyDel2 = logLik.fitPolyDel2,
             
             rmse.intercept = rmse.intercept, rmse.randomIntercept = rmse.randomIntercept, rmse.timeRI = rmse.timeRI, 
             rmse.timeRS = rmse.timeRS, rmse.timeQ = rmse.timeQ, 
             rmse.fitSHIHD = rmse.fitSHIHD, rmse.fitSHIHD2 = rmse.fitSHIHD2, rmse.fitFinal = rmse.fitFinal, rmse.fitFinal2 = rmse.fitFinal2,
             rmse.fitAgeRes = rmse.fitAgeRes, rmse.fitAgeRes2 = rmse.fitAgeRes2, rmse.fitMarNupt = rmse.fitMarNupt, 
             rmse.fitMarNupt2 = rmse.fitMarNupt2, rmse.fitPolyDel = rmse.fitPolyDel, rmse.fitPolyDel2 = rmse.fitPolyDel2)
  
  return(res)
  
}

# Model comparison on the basis of AIC, BIC and log-likelihood (means and standard deviations from imputation)
modelComparison <- function(fitWomen, fitMen, path){
  if (all(names(fitWomen) == names(fitMen))){
    aic.inds <- which(grepl('aic.', names(fitWomen)))
    bic.inds <- which(grepl('bic.', names(fitWomen)))
    logLik.inds <- which(grepl('logLik.', names(fitWomen)))
    rmse.inds <- which(grepl('rmse.', names(fitWomen)))
  } else {
    stop('ERROR: Female and male fits do not have same names: adjust function accordingly!')
  }
  
  # Create table of model comparison results
  nams <- c('Model', 'AIC mean men', 'AIC sd men', 'BIC mean men', 'BIC sd men', 
            'logLik mean men', 'logLik sd men', 'RMSE mean men', "RMSE sd men",
            'AIC mean women', 'AIC sd women', 'BIC mean women', 'BIC sd women', 
            'logLik mean women', 'logLik sd women', 'RMSE mean women', "RMSE sd women")
  modelComp <- data.frame(matrix(NA, nrow = length(aic.inds), ncol = length(nams)))
  names(modelComp) <- nams
  modelComp[, 1] <- gsub('aic.', '', names(fitWomen)[aic.inds])
  modelComp[, 2] <- unlist(lapply(fitMen[aic.inds], function(x){x[1]}))
  modelComp[, 3] <- unlist(lapply(fitMen[aic.inds], function(x){x[2]}))
  modelComp[, 4] <- unlist(lapply(fitMen[bic.inds], function(x){x[1]}))
  modelComp[, 5] <- unlist(lapply(fitMen[bic.inds], function(x){x[2]}))
  modelComp[, 6] <- unlist(lapply(fitMen[logLik.inds], function(x){x[1]}))
  modelComp[, 7] <- unlist(lapply(fitMen[logLik.inds], function(x){x[2]}))
  modelComp[, 8] <- unlist(lapply(fitMen[rmse.inds], function(x){x[1]}))
  modelComp[, 9] <- unlist(lapply(fitMen[rmse.inds], function(x){x[2]}))
  modelComp[, 10] <- unlist(lapply(fitWomen[aic.inds], function(x){x[1]}))
  modelComp[, 11] <- unlist(lapply(fitWomen[aic.inds], function(x){x[2]}))
  modelComp[, 12] <- unlist(lapply(fitWomen[bic.inds], function(x){x[1]}))
  modelComp[, 13] <- unlist(lapply(fitWomen[bic.inds], function(x){x[2]}))
  modelComp[, 14] <- unlist(lapply(fitWomen[logLik.inds], function(x){x[1]}))
  modelComp[, 15] <- unlist(lapply(fitWomen[logLik.inds], function(x){x[2]}))
  modelComp[, 16] <- unlist(lapply(fitWomen[rmse.inds], function(x){x[1]}))
  modelComp[, 17] <- unlist(lapply(fitWomen[rmse.inds], function(x){x[2]}))
  
  if (!dir.exists(paste0(path, 'multilevel_results'))){
    dir.create(paste0(path, 'multilevel_results'))
  }
  
  # Output results combined
  output <- print(xtable(modelComp, digits = rep(3, ncol(modelComp) + 1)), include.rownames = FALSE)
  fileConn <- file(paste0(path, "multilevel_results/modelComparison.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
  # Output results per gender
  sexes <- c('women', 'men')
  for (i in 1:length(sexes)){
    modCompSex <- modelComp[, match(c('Model', paste(rep(c('AIC', 'BIC', 'logLik', 'RMSE'), 
                                                         each = 2), c('mean', 'sd'), sexes[i])), names(modelComp))]
    modCompSex <- modCompSex[order(modCompSex[, grepl('AIC', names(modCompSex)) & grepl('mean', names(modCompSex))]), ]
    output <- print(xtable(modCompSex, digits = rep(3, ncol(modCompSex) + 1)), include.rownames = FALSE)
    fileConn <- file(paste0(path, "multilevel_results/modelComparison", capitalize(sexes[i]), ".txt"))
    writeLines(output, fileConn)
    close(fileConn)
  }
  
  return(modelComp)
  
}

# Output model results to LaTeX table
modelResultsGrouped <- function(fitWomen, fitMen, path, HIV, sexRatios, modcomp){
  
  # For both genders, create a table of model results
  for (j in 1:2){
    
    if (j == 1){
      fitdata <- fitWomen
      sexe <- 'Women'
      modComp <- modcomp[, c(1, which(grepl('mean', names(modcomp)) & grepl('women', names(modcomp))))]
    } else {
      fitdata <- fitMen
      sexe <- 'Men'
      modComp <- modcomp[, c(1, which(grepl('mean', names(modcomp)) & !(grepl('women', names(modcomp)))))]
    }
    
    targetVars <- list(c("fitSHIHD", "fitSHIHD2", 'fitAgeRes', 'fitAgeRes2', 'fitMarNupt', 'fitMarNupt2', 
                         'fitPolyDel', 'fitPolyDel2', "fitFinal", "fitFinal2"))
    timevars <- c('Time', "")
    for (k in 1:length(targetVars)){
      
      if (k == 1){
        my_fits <- list(fitdata$fitFinal, fitdata$fitFinal2)
      } else if (k == 2){
        my_fits <- list(fitdata$fitFinalMinTime, fitdata$fitFinalMinTime2)
      }
      
      mod.inds <- match(targetVars[[k]], names(fitdata))
      mod.inds1 <- mod.inds[which(!grepl('2', names(fitdata)[mod.inds]))]
      mod.inds2 <- mod.inds[which(grepl('2', names(fitdata)[mod.inds]))]
      my_mods <- list(mod.inds1, mod.inds2)
      type_fit <- c("Linear", 'Quadratic')
      
      # Create table of model results per fit type
      for (z in 1:length(type_fit)){
        
        vec <- rep(rownames(summary(my_fits[[z]])), each = 2)
        vec[even(1:length(vec))] <- paste0(vec[even(1:length(vec))], '_sd')
        regr <- data.frame(matrix(NA, nrow = 2*nrow(summary(my_fits[[z]])) + 1, ncol = length(fitdata[my_mods[[z]]]) + 1))
        regr[, 1] <- c('Model', vec)
        regr[1, ] <- c('Model', names(fitdata[my_mods[[z]]]))
        
        for (i in 2:ncol(regr)){ 
          dd <- summary(fitdata[my_mods[[z]]][[which(names(fitdata)[my_mods[[z]]] == regr[1, i])]])
          sig <- ifelse(dd[, 5] >= 0.1, '', 
                        ifelse(dd[, 5] < 0.1 & dd[, 5] >= 0.05, '\\dagger', 
                               ifelse(dd[,5] < 0.05 & dd[,5] >= 0.01, '*', 
                                      ifelse(dd[,5] < 0.01 & dd[,5] >= .001, '**', 
                                             ifelse(dd[,5] < 0.001, '***', NA)))))
          regr[match(rownames(dd), regr[, 1]), i] <- paste0(mround(dd[, 1], .001), "$^{", sig, "}$")
          regr[match(paste0(rownames(dd), '_sd'), regr[, 1]), i] <- paste0('(', mround(dd[, 5], .001), ')')
        }
        
        rowss <- c('AIC', 'BIC', 'logLik')
        IC <- data.frame(matrix(NA, nrow = length(rowss), ncol = ncol(regr)))
        IC[, 1] <- rowss
        IC[1, -c(1)] <- mround(modComp[match(regr[1, -c(1)], modComp$Model), which(grepl('AIC', names(modComp)))], .001)
        IC[2, -c(1)] <- mround(modComp[match(regr[1, -c(1)], modComp$Model), which(grepl('BIC', names(modComp)))], .001)
        IC[3, -c(1)] <- mround(modComp[match(regr[1, -c(1)], modComp$Model), which(grepl('logLik', names(modComp)))], .001)
        
        if (j == 1 & z == 2){
          my_women <- rbind(regr, IC)
        }
        if (j == 2 & z == 1){
          my_men <- rbind(regr, IC)
        }
        regr[which(grepl('_sd', regr[, 1])), 1] <- NA
        regr <- rbind(regr, IC)
        
        if (!dir.exists(paste0(path, 'multilevel_results'))){
          dir.create(paste0(path, 'multilevel_results'))
        }
        
        # Print ourput results to text file
        output <- print(xtable(regr, digits = rep(3, ncol(regr) + 1)), include.rownames = FALSE, sanitize.text.function=function(x){x})
        fileConn <- file(paste0(path, "multilevel_results/", type_fit[z], sexe, timevars[k], "Grouped.txt"))
        writeLines(output, fileConn)
        close(fileConn)
        
      }
      
    }
    
  }
  
  # Format tables
  my_models <- my_women
  my_men$X1[my_men$X1 == "poly(time, 1)"] <- "poly(time, 2)1"
  my_men$X1[my_men$X1 == "poly(time, 1)_sd"] <- "poly(time, 2)1_sd"
  my_men$X1[my_men$X1 == "poly(SHIHD, 1)"] <- "poly(SHIHD, 2)1"
  my_men$X1[my_men$X1 == "poly(SHIHD, 1)_sd"] <- "poly(SHIHD, 2)1_sd"
  names(my_models) <- c("Model", paste0("WM", 1:5))
  my_models[paste0("MM", 1:5)] <- NA
  my_models[match(my_men[, 1], my_women[, 1]), paste0("MM", 1:5)] <- my_men[, 2:6] 
  my_models[which(grepl('_sd', my_models[, 1])), 1] <- NA
  my_models <- my_models[, c("Model", paste0("MM", 1:5), paste0("WM", 1:5))]
  
  # Print results to text file
  output <- print(xtable(my_models, digits = rep(3, ncol(my_models) + 1)), 
                  include.rownames = FALSE, sanitize.text.function=function(x){x})
  fileConn <- file(paste0(path, "multilevel_results/linMenquadWomenTimeGrouped.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}

# Get fitted data for pooled results (fitted on original data before imputation)
plotModelFits <- function(fit, frame){
  
  if (all(class(frame) != "data.frame")){
    
    preds <- shihds <- data.frame(matrix(NA, nrow = nrow(frame[[1]])))
    for (j in 1:length(frame)){
      subdf <- frame[[j]][, na.omit(match(rownames(fit$pooled), names(frame[[j]])))]
      subdf["(Intercept)"] <- 1
      subdf["poly(time, 1)"] <- poly(frame[[j]]$time, 1)
      subdf["poly(time, 2)1"] <- poly(frame[[j]]$time, 2)[, 1]
      subdf["poly(time, 2)2"] <- poly(frame[[j]]$time, 2)[, 2]
      subdf["poly(SHIHD, 1)"] <- poly(frame[[j]]$SHIHD, 1)
      subdf["poly(SHIHD, 2)1"] <- poly(frame[[j]]$SHIHD, 2)[, 1]
      subdf["poly(SHIHD, 2)2"] <- poly(frame[[j]]$SHIHD, 2)[, 2]
      subdf <- subdf[, na.omit(match(rownames(fit$pooled), names(subdf)))]
      preds[, j] <- data.frame(as.matrix(subdf) %*% as.matrix(fit$pooled[, 1]))
      shihds[, ] <- frame[[j]]$SHIHD
    }
    
    subdf["SHIHD"] <- apply(shihds, 1, mean)
    subdf["pred"] <- apply(preds, 1, mean)
    
  } else {
    subdf <- frame[, na.omit(match(rownames(fit$pooled), names(frame)))]
    subdf["(Intercept)"] <- 1
    subdf["poly(time, 2)1"] <- poly(frame$time, 2)[, 1]
    subdf["poly(time, 2)2"] <- poly(frame$time, 2)[, 2]
    subdf["poly(SHIHD, 1)"] <- poly(frame$time, 1)
    subdf["poly(SHIHD, 2)1"] <- poly(frame$SHIHD, 2)[, 1]
    subdf["poly(SHIHD, 2)2"] <- poly(frame$SHIHD, 2)[, 2]
    subdf <- subdf[, na.omit(match(rownames(fit$pooled), names(subdf)))]
    subdf["pred"] <- c(as.matrix(subdf) %*% as.matrix(fit$pooled[,1]))
    subdf["SHIHD"] <- frame$SHIHD
  }
  
  subdf <- subdf[, c("SHIHD", "pred")]
  
  return(subdf)
}

# Plot model predictions from all-variable models (linear, quadratic, both and best fit)
plotPredictions <- function(fitWomenG, impWomen, wframe, fitMenG, impMen, mframe, diffAIC){
  
  # Obtain data for best (linear or quadratic) fit
  fits <- list(fitWomenG, fitMenG)
  imps <- list(impWomen, impMen)
  pfin <- list()
  for (j in 1:length(fits)){
    fitlist <- list(fits[[j]]$fitFinal, fits[[j]]$fitFinal2)
    ind <- ifelse((fits[[j]]$aic.fitFinal2[1] - fits[[j]]$aic.fitFinal[1]) < (-diffAIC), 2, 1)
    plotfit <- fitlist[[ind]]
    pplot <- plotModelFits(plotfit, mice::complete(imps[[j]], "all"))
    if (ind == 1){
      pfit <- lm(pred ~ poly(SHIHD, 1), data = pplot)
    } else if (ind == 2){
      pfit <- lm(pred ~ poly(SHIHD, 2), data = pplot)
    }
    pfin[[j]] <- cbind(pplot, predict(pfit, pplot, interval = "confidence"))
  }
  
  # Combine data and get plot annotations with correlations
  plotdat <- rbind(cbind(childless = wframe$childless_dummy1, pfin[[1]]), 
                   cbind(childless = mframe$childless_dummy1, pfin[[2]]))
  plotdat["gender"] <- rep(c("Women", "Men"), unlist(lapply(pfin, nrow)))
  plotdat <- plotdat[complete.cases(plotdat[, c("childless", "SHIHD")]), ]
  r <- mround(c(cor(plotdat[plotdat$gender == "Women", c("SHIHD", 'childless')])[1, 2], 
                cor(plotdat[plotdat$gender == "Men", c("SHIHD", 'childless')])[1, 2]), .01)
  plotdat$childless <- plotdat$childless * 100
  plotdat <- plotdat[plotdat$childless <= 25, ]
  ann_text <- data.frame(gender = c("Women", "Men"), SHIHD = max(plotdat$SHIHD), 
                         childless = max(na.omit(plotdat$childless)), lab = paste0('Corr = ', r))
  
  # Make the plots
  g <- ggplot() +
    geom_point(data = plotdat, aes(x = SHIHD, y = childless), alpha = 0.6) +
    facet_wrap(~ gender) +
    geom_label(data = ann_text, aes(x = SHIHD*0.9, y = childless, label = lab, fontface = "italic"), 
               label.padding = unit(0.25, "lines"), size = 3) +
    my_theme() +
    scale_y_continuous(limits = c(0, max(plotdat$childless))) +
    labs(x = "SHIHD", y = "Childlessness (%)") 
  g1 <- g + geom_smooth(data = plotdat, aes(x = SHIHD, y = childless), method = "lm", 
                        formula = y ~ poly(x, 1), color = "red", fill = "red", linetype = "dashed")
  g2 <- g + geom_smooth(data = plotdat, aes(x = SHIHD, y = childless), method = "lm", 
                        formula = y ~ poly(x, 2), color = "green", fill = "green")
  g3 <- g1 + geom_smooth(data = plotdat, aes(x = SHIHD, y = childless),
                         method = "lm", formula = y ~ poly(x, 2), color = "green", fill = "green")
  g4 <- g + geom_line(data = plotdat, aes(x = SHIHD, y = fit*100), color = "red", size = 1) + 
    geom_ribbon(data = plotdat, aes(x = SHIHD, ymin = lwr*100, ymax = upr*100), fill = "red", alpha = 0.3)
  
  # Save the plots
  if (!dir.exists(paste0(path, 'scatter_plots/regional'))){
    dir.create(paste0(path, 'scatter_plots/regional'), recursive = TRUE)
  }
  ggsave(paste0(path, 'scatter_plots/regional/finalFit_lin.png'), g1, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/regional/finalFit_quad.png'), g2, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/regional/finalFit_linquad.png'), g3, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  ggsave(paste0(path, 'scatter_plots/regional/finalFit_bestfit.png'), g4, dpi = 600, height = 16.933, width = 21.768, units = "cm")
  
}

