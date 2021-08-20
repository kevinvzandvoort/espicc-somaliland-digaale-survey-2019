#' This script generates a template for the variables sheet in the data
#'  dictionary

library(data.table)
library(magrittr)

datasets =
  c("contact_data_contactees", "contact_data_contactors", "household_data",
    "household_data_members", "household_data_members_migration",
    "household_data_members_mortality", "missing_houses_manual_categories",
    "nutrition_data", "participant_data", "regression_data")

#categorical variables that are factors
categorical_variables_factors_withsep =
  c("contact_setting", "house_fuel", "household_substance_use")

categorical_variables_factors_nosep =
  c("household_present", "household_consent", "water_source", "house_leakage",
  "house_draft", "house_ventilation", "house_settle", "household_member_sex",
  "household_member_inmigration", "status")

categorical_variables_factors = c(categorical_variables_factors_withsep,
                                  categorical_variables_factors_nosep)

datasets %>% lapply(function(d){
  data = get(d)
  
  data_cnames = data %>% colnames
  data_types =
    sapply(data_cnames, function(x, data) data[, class(get(x))], data)
  factor_levels =
    lapply(data_cnames, function(x,data) data[, get(x)] %>% levels, data)
  
  lapply(1:length(data_cnames), function(i){
    if(data_cnames[i] %in% categorical_variables_factors){
      if(data_cnames[i] %in% categorical_variables_factors_withsep){
        factor_levels[[i]] =
          data[, get(data_cnames[i])] %>%
          sapply(function(x) strsplit(x, "; ")) %>%
          unlist %>% na.omit %>% unique %>% sort    
      } else {
        factor_levels[[i]] =
          data[, get(data_cnames[i])] %>% unique %>% sort
      }
    }
    
    data.table(variable = data_cnames[i], description = NA_character_,
      type = data_types[i],
      levels = {
        if(!data_cnames[i] %in% categorical_variables_factors)
          seq_len(length(factor_levels[[i]]))
      }, values = factor_levels[[i]])
  }) %>%
    rbindlist(fill=TRUE) %>%
    .[, dataset := d] %>% return
}) %>%
  rbindlist(fill=TRUE) %>%
  .[, c("dataset", "variable", "description", "type", "levels", "values")] %>%
  .[, dataset := paste0(dataset,".RDS")] %>%
  fwrite("./data/data_dictionary.csv")

#' Note data_dictionary is manually edited to add description and saved as
#'  data_dictionary.xlsx