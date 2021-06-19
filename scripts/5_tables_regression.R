#' Logistic regression
#' - This analysis assesses the relationship between the collected covariates and self-reported pneumonia incidence
#' - We do not used weighted estimates for the regression
#' - Participants were asked whether they were ever diagnosed with pneumonia, and if so, whether this happened in the last 6 months
#' - Pneumonia is assessed as 1) self-reported pneumonia ever or 2) self-reported pneumonia in last 6 months
#' - We a priori adjusted estimates for age and sex

#' Check if need to add age as continuous or categorical variable
#' - Age by year has been removed from the regression_data, so the lrtest below not work in this repository
#' - However, code has been retained to document methods used
#' There was no evidence against simpler model (age as continuous variable)
# pneumonia_age_cont = glm(pneumonia ~ participant_age_years, regression_data, family=stats::binomial(link="logit"))
pneumonia_age_cat = glm(pneumonia ~ participant_age_group_sample, regression_data, family=stats::binomial(link="logit"))
# lmtest::lrtest(pneumonia_age_cont, pneumonia_age_cat)

#' There was strong evidence in favour of more complex model (age as categorical variable)
#' As pneumonia in last 6m is likely a more reliable estimate, choose to include age as categorical variable in ALL models
# pneumonia_6m_age_cont = glm(pneumonia_6m ~ participant_age_years, regression_data, family=stats::binomial(link="logit"))
pneumonia_6m_age_cat = glm(pneumonia_6m ~ participant_age_group_sample, regression_data, family=stats::binomial(link="logit"))
# lmtest::lrtest(pneumonia_6m_age_cont, pneumonia_6m_age_cat)

#' We only conducted a univariate analysis

#' Outcome variable to be used in the regression
outcome = "pneumonia"

#' A-priori defined confounding variables to add to all regressions in all ages
priori_conf_all = c("participant_age_group_sample", "participant_sex")

#' Variables to be assessed for all ages
covariates_all = c(
  "Household size" = "household_members",
  "Household members <5y" = "hhmem_u5",
  "Household members <2y" = "hhmem_u2",
  "Household settled in Digaale" = "house_settle",
  "Total number of rooms" = "rooms_total",
  "Reported draft in shelter" = "house_draft",
  "Reported leakage in shelter" = "house_leakage",
  "Use charcoal for cooking" = "house_fuel_charcoal",
  "Use firewood for cooking" = "house_fuel_firewood",
  "Use ventilation when cooking" = "house_ventilation",
  "Household member who uses khat" = "house_substance_use_khat",
  "Household member who smokes" = "house_substance_use_smoke",
  "Household member who uses snuff" = "house_substance_use_snuff",
  "Total number of direct contacts" = "total_recorded_contacts",
  "Total number of physical contacts" = "physical_contacts",
  "Total number of contacts at home" = "home_contacts",
  "Total number of contacts at school" = "school_contacts",
  "Total number of contacts at work" = "work_contacts",
  "Total number of contacts at other settings" = "other_contacts")

#' A-priori defined confounding variables to add to all regressions in the U5 subgroup
#' - Note that age in months is included as a continuous variable in the U5 subgroup analyses
priori_conf_u5 = c("participant_age_month", "participant_sex")

#' Variables to be assessed in participants U5 only (anthropometrics were only collected for U5)
covariates_u5 = c(
  "Weight by age" = "weight_for_age",
  "Height by age" = "height_for_age",
  "Weight by height" = "weight_for_height",
  "Middle-Upper Arm Circumference" = "muac_level")

#' Categories used to group models in table
covariates_categories = c(
  "Demographic characteristics" = "Household size",
  "Demographic characteristics" = "Household members <5y",
  "Demographic characteristics" = "Household members <2y",
  "Household settled in Digaale" = "Household settled in Digaale",
  "Quality of shelter" = "Total number of rooms",
  "Quality of shelter" = "Reported draft in shelter",
  "Quality of shelter" = "Reported leakage in shelter",
  "Indoor air pollution" = "Use charcoal for cooking",
  "Indoor air pollution" = "Use firewood for cooking",
  "Indoor air pollution" = "Use ventilation when cooking",
  "Substance use in household" = "Household member who uses khat",
  "Substance use in household" = "Household member who smokes",
  "Substance use in household" = "Household member who uses snuff",
  "Contact behaviour" = "Total number of direct contacts",
  "Contact behaviour" = "Total number of physical contacts",
  "Contact behaviour" = "Total number of contacts at home",
  "Contact behaviour" = "Total number of contacts at school",
  "Contact behaviour" = "Total number of contacts at work",
  "Contact behaviour" = "Total number of contacts at other settings",
  "Malnutrition in U5" = "Weight by age",
  "Malnutrition in U5" = "Height by age",
  "Malnutrition in U5" = "Weight by height",
  "Malnutrition in U5" = "Middle-Upper Arm Circumference")

#' This runs one single logistic regression model for each covariate with the selected outcome (pneumonia)
#' - a priori defined variables are automatically added to each model

#' Analysis in all age-groups
regression_all = 1:length(covariates_all) %>%
  lapply(function(l, covariates_all){
    covars = covariates_all[[l]]
    x = as.formula(paste0(outcome, "~", paste0(c(paste0(priori_conf_all, collapse="+"),
                                                 paste0(covars, collapse="+")), collapse="+"))) %>%
      glm(regression_data, family=stats::binomial(link="logit")) %>% parseGLM(covars) %>% .[, model := l]
    return(x)},
    covariates_all) %>% rbindlist

#' Analysis in the U5
regression_u5 = 1:length(covariates_u5) %>%
  lapply(function(l, covariates_u5){
    covars = covariates_u5[[l]]
    x = as.formula(paste0(outcome, "~", paste0(c(paste0(priori_conf_u5, collapse="+"),
                                                 paste0(covars, collapse="+")), collapse="+"))) %>%
      glm(regression_data[!is.na(participant_age_month)], family=stats::binomial(link="logit")) %>%
      parseGLM(covars) %>% .[, model := l]
    return(x)}, covariates_u5) %>% rbindlist

#' Format estimates
regression_all[, variable := factor(variable, covariates_all, names(covariates_all))]
regression_u5[, variable := factor(variable, covariates_u5, names(covariates_u5))]
regression_u5[option == "Severely wasted (z <= -3)", c("ci_low", "ci_high", "pval") := .(NA_real_, NA_real_, NA_real_)]

#' Combine estimates in one table
regression = rbind(regression_all[, data := "all"], regression_u5[, data := "<u5 y"])
regression[, category := factor(variable, covariates_categories, names(covariates_categories))]
regression[, ci_format := paste0(ci_low," - ", ci_high)]
regression[is.na(ci_low) | is.na(ci_high), ci_format := NA_character_]
regression[type == "continuous", c("option", "variable") := .(variable, NA_character_)]
table_sD1_regression_pneumonia = regression[, c("category", "variable", "option", "est", "ci_format", "pval")]

table_sD1_regression_pneumonia %>%
  kblOut(booktabs=T, align=c("l", "l","l","l", "l","l"), linesep = "",
         col.names=c("", "Variable", "", "OR", "95% CI", "p-value"), other_functions = list(
           function(x) collapse_rows(x, 1:2, row_group_label_position = "stack",
                                     row_group_label_fonts = list(list(bold=T, italic=T), list(bold=F, italic=T))),
           function(x) kable_styling(x, latex_options = "scale_down")), out_name = "table_sD1_regression_pneumonia")

#' This runs one single logistic regression model for each covariate with the selected outcome (pneumonia in last 6m)
#' - a priori defined variables are automatically added to each model
outcome = "pneumonia_6m"

#' Analysis in all age-groups
regression_all = 1:length(covariates_all) %>%
  lapply(function(l, covariates_all){
    covars = covariates_all[[l]]
    x = as.formula(paste0(outcome, "~", paste0(c(paste0(priori_conf_all, collapse="+"),
                                                 paste0(covars, collapse="+")), collapse="+"))) %>%
      glm(regression_data, family=stats::binomial(link="logit")) %>% parseGLM(covars) %>% .[, model := l]
    return(x)}, covariates_all) %>% rbindlist

#' Analysis in the U5
regression_u5 = 1:length(covariates_u5) %>%
  lapply(function(l, covariates_u5){
    covars = covariates_u5[[l]]
    x = as.formula(paste0(outcome, "~", paste0(c(paste0(priori_conf_u5, collapse="+"),
                                                 paste0(covars, collapse="+")), collapse="+"))) %>%
      glm(regression_data[!is.na(participant_age_month)], family=stats::binomial(link="logit")) %>%
      parseGLM(covars) %>% .[, model := l]
    return(x)}, covariates_u5) %>% rbindlist

#' Format estimates
regression_all[, variable := factor(variable, covariates_all, names(covariates_all))]
regression_u5[, variable := factor(variable, covariates_u5, names(covariates_u5))]
regression_u5[option == "Severely wasted (z <= -3)", c("ci_low", "ci_high", "pval") := .(NA_real_, NA_real_, NA_real_)]

#' Combine estimates in one table
regression = rbind(regression_all[, data := "all"], regression_u5[, data := "<u5 y"])
regression[, category := factor(variable, covariates_categories, names(covariates_categories))]
regression[, ci_format := paste0(ci_low," - ", ci_high)]
regression[is.na(ci_low) | is.na(ci_high), ci_format := NA_character_]
regression[type == "continuous", c("option", "variable") := .(variable, NA_character_)]

table_sD2_regression_pneumonia_6m = regression[, c("category", "variable", "option", "est", "ci_format", "pval")]

table_sD2_regression_pneumonia_6m %>%
  kblOut(booktabs=T, align=c("l", "l","l","l", "l","l"), linesep = "",
         col.names=c("", "Variable", "", "OR", "95% CI", "p-value"), other_functions = list(
           function(x) collapse_rows(x, 1:2, row_group_label_position = "stack",
                                     row_group_label_fonts = list(list(bold=T, italic=T), list(bold=F, italic=T))),
           function(x) kable_styling(x, latex_options = "scale_down")), out_name = "table_sD2_regression_pneumonia_6m")

rm("covariates_all", "covariates_categories", "covariates_u5", "outcome", "pneumonia_6m_age_cat", "pneumonia_age_cat",
   "priori_conf_all", "priori_conf_u5", "regression", "regression_all", "regression_u5")

setwd(analysis_dir)