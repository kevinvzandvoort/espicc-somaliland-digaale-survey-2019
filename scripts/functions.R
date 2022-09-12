#' Wrapper around survey::svymean
#' - enables stratification by multiple variables
#' - returns both mean and corresponding 95% confidence interval
svyMean2 = function(x, design, digits=-1, multiply=1, by=NULL, na.rm=F){
  svyMeanCI = function(x, design, digits, multiply, na.rm){
    y = survey::svymean(x, design, na.rm=na.rm)
    y_ci = confint(y)
    z = data.table(variable = names(y), option = names(y), mean = as.numeric(y)*multiply, ci95_low = y_ci[,1]*multiply,
                   ci95_high = y_ci[,2]*multiply)
    
    #separate options
    for(l in labels(terms(x))){
      z[grepl(l, variable), variable := l]
      z[grepl(l, variable), option := gsub(l, "", option)]
    }
    
    if(digits >= 0)
      z[, c('mean', 'ci95_low', 'ci95_high') := lapply(.SD, round, digits=digits),
        .SDcols=c("mean", "ci95_low", "ci95_high")]
    
    z = z[order(variable)]
    return(z)
  }
  
  if(is.null(by)){
    y = svyMeanCI(x, design, digits, multiply, na.rm)
  } else {
    if(length(by) != 1)
      stop("Can only stratify by one variable currently")
    
    values = design$variables[, ..by] %>% unlist %>% unique %>% sort
    y = values %>%
      lapply(function(y) svyMeanCI(x, subset(design, get(by) == y), digits, multiply, na.rm)[, (by) := y]) %>% rbindlist
    
    y = y[order(variable, get(by))]
  }
  
  return(y)
}

#' Wrapper around survey::svytotal
#' - enables stratification by multiple variables
#' - returns both mean and corresponding 95% confidence interval
svyTotal2 = function(x, design, digits=-1, by=NULL, na.rm=F){
  svyTotalCI = function(x, design, digits, na.rm){
    y = survey::svytotal(x, design, na.rm=na.rm)
    y_ci = confint(y)
    z = data.table(variable = names(y), option = names(y), total = y, ci95_low = y_ci[,1], ci95_high = y_ci[,2])
    
    #separate options
    for(l in labels(terms(x))){
      z[grepl(l, variable), variable := l]
      z[grepl(l, variable), option := gsub(l, "", option)]
    }
    
    if(digits >= 0)
      z[, c('total', 'ci95_low', 'ci95_high') := lapply(.SD, round, digits=digits),
        .SDcols=c("total", "ci95_low", "ci95_high")]
    
    z = z[order(variable)]
    return(z)
  }
  
  if(is.null(by)){
    y = svyTotalCI(x, design, digits, na.rm)
  } else {
    if(length(by) != 1)
      stop("Can only stratify by one variable currently")
    
    values = design$variables[, ..by] %>% unlist %>% unique %>% sort
    y = values %>% lapply(function(y) svyTotalCI(x, subset(design, get(by) == y), digits, na.rm)[, (by) := y]) %>%
      rbindlist
    
    y = y[order(variable, get(by))]
  }
  return(y)
}

#' construct a contact matrix from total number of contacts by age
#' - returns a list with four objects:
#'  - total_contacts: matrix with the total contacts passed to the function (before adjusting for reciprocity)
#'  - rate_unadjusted: matrix with the contact rates before adjusting for reciprocity of total contacts
#'  - rate_adjusted: matrix with the contact rates after adjusting for reciprocity of total contacts
#'  - prob: matrix with the per-capita contact rates (adjusted for reciprocity of total contacts)
constructContactMatrix <- function(total_contacts, sample_popsize, population_popsize, adjust_ratio=1){
  rate_unadjusted <- total_contacts
  for(j in 1:ncol(rate_unadjusted)){
    rate_unadjusted[,j] <- total_contacts[,j]/sample_popsize[j]
  }
  
  rate_adjusted <- rate_unadjusted
  for(j in 1:ncol(rate_adjusted)){
    for(i in 1:nrow(rate_adjusted)){
      rate_adjusted[i,j] =
        (rate_unadjusted[i,j] + rate_unadjusted[j,i] * population_popsize[i]/population_popsize[j])/2
    }
  }
  rate_adjusted * adjust_ratio
  
  prob <- rate_adjusted/population_popsize
  #' cannot contact oneself, so need to subtract N with 1 for i == j
  diag(prob) <- diag(rate_adjusted)/(population_popsize-1)
  
  list(total = total_contacts, rate_unadjusted = rate_unadjusted, rate_adjusted = rate_adjusted, prob = prob)
}

#' Adjusts a contact matrix for a population with a different population distribution, keeping the per-capita contact
#'  rates constant
adjustContactMatrix <- function(matrices, new_popsize){
  #' keep the same probability of contact
  constructContactMatrix(t(t(matrices$prob * (new_popsize - 1)) * new_popsize), new_popsize, new_popsize) %>%
    return
}

#' Reshape a contact matrix in a long (melted) format that can easily be used to plot
reshapeCM <- function(cm, age_grps = age_groups){
  cm = as.data.table(cm)
  cm[, "contact_age_group"] <- factor(age_grps[, age_group], age_grps[, age_group], age_grps[, name])
  cm = melt(cm, id.vars="contact_age_group", variable.name="part_age_group", variable.factor=FALSE, value.factor=FALSE)
  cm[, contact_age_group := contact_age_group]
  return(cm)
}

#' Calculate the total number of age-pairs in all households in a population
#' - accounts for not being able to contact oneself
totalAgePairs <- function(households){
  households <- households[, c("household_id", "age_group")]
  out <- unique(households[, household_id]) %>% lapply(function(x){
      y = households[household_id == x];
      lapply(1:nrow(y), function(i){y[-i][, age_group2 := y[i, age_group]]}) %>%
        rbindlist %>% return()
    }) %>% rbindlist
  
  out = as.matrix(dcast(out, age_group2 ~ age_group, fun.aggregate = length)[, -"age_group2"])
  return(out)
}

#' Wrapper around kableExtra::kable
kblOut = function(data, formats = c(pdf = "latex", html = "html"), out_name = NULL,
                  out_dir = sprintf("%s/output/%s/tables", analysis_dir, OUTPUT_DIR), align, other_functions = NULL, ...){
  for(f in 1:length(formats)){
    falign = switch(formats[f], "html" = gsub("|", "", align, fixed=T), align)
    k = kbl(data, format = formats[f], align = falign, ...)
    if(is.list(other_functions)){
      for(i in 1:length(other_functions)){
        k = k %>% (other_functions[[i]])
      }
    }
    
    k %>% save_kable(sprintf("%s/%s/%s", out_dir, names(formats)[f], out_name)) %>%
      tryCatch(error = function(e){message(e)}, warning = function(w){message(w)})
  }
}

#' Wrapper around stats::glm
parseGLM = function(x, covariates, exp=TRUE, conflevel=0.95, digits_est=2, digits_pval=3){
  zcoefs = coef(summary(x))
  coefs = zcoefs[unlist(sapply(covariates, function(x, coefnames) which(grepl(x, coefnames, fixed=T)),
                               rownames(zcoefs))), ]
  
  if(!is.matrix(coefs))
    coefs = coefs %>% as.matrix %>% t
  
  zconfints = confint(x, level=conflevel)
  confints = zconfints[unlist(sapply(covariates, function(x, coefnames) which(grepl(x, coefnames, fixed=T)),
                                     rownames(zconfints))), ]
  
  if(!is.matrix(confints))
    confints = confints %>% as.matrix %>% t
  
  z = data.table(
    variable = rownames(zcoefs)[unlist(sapply(covariates, function(x, coefnames) which(grepl(x, coefnames, fixed=T)),
                                              rownames(zcoefs)))],
    option = rownames(zcoefs)[unlist(sapply(covariates, function(x, coefnames) which(grepl(x, coefnames, fixed=T)),
                                            rownames(zcoefs)))],
    est = coefs[, 1], ci_low = confints[, 1], ci_high = confints[, 2], pval = coefs[, 4])
  
  if(exp)
    z[, c("est", "ci_low", "ci_high") := .(exp(est), exp(ci_low), exp(ci_high))]
  
  z[, c("est", "ci_low", "ci_high", "pval") :=
      .(round(est, digits_est), round(ci_low, digits_est), round(ci_high, digits_est), round(pval, digits_pval))]
  
  #separate options
  for(l in covariates){
    z[grepl(l, variable), variable := l]
    z[grepl(l, variable), option := gsub(l, "", option)]
    if(l %in% names(x$xlevels)){
      z[grepl(l, variable), type := "categorical"]
      z = z %>%
        rbind(data.table(variable = l,
                         option = x[["xlevels"]][[l]][!x[["xlevels"]][[l]] %in% z[grepl(l, variable), option]],
                         est = 1, ci_low = NA_real_, ci_high = NA_real_, pval = NA_real_, type = "categorical"))
      
    } else {
      z[grepl(l, variable), type := "continuous"]
    }
  }
  
  z[, variable := factor(variable, covariates)]
  z[, option := factor(option, unlist(x$xlevels))]
  setorder(z, variable, option)
  
  return(z)
}
