library(socialmixr)
library(data.table)
library(ggplot2)

empirical_matrices_sourcedata = list(
  "Mossong" = list(
    "country" = "United Kingdom",
    "location" = NA,
    "iso3" = "GBR",
    "cm_name_matrix_old" = "UK",
    "cm_name_populations" = "UK",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.1043437",
    "year" = 2006
  ),
  "Zhang" = list(
    "country" = "China",
    "location" = "Shanghai",
    "iso3" = "CHN",
    "cm_name_matrix_old" = "China",
    "cm_name_populations" = "China",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.3366396",
    "year" = 2018
  ),
  "Beraud" = list(
    "country" = "France",
    "location" = NA,
    "iso3" = "FRA",
    "cm_name_matrix_old" = "France",
    "cm_name_populations" = "France",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.1157918",
    "year" = 2012
  ),
  "Leung" = list(
    "country" = "Hong Kong",
    "location" = "Hong Kong",
    "iso3" = "HKG",
    "cm_name_matrix_old" = "Hong Kong SAR, China",
    "cm_name_populations" = "China, Hong Kong SAR",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.1165561",
    "year" = 2016
  ),
  #' Exclude Kiti et al as low number of 50+
  #"Kiti" = list(
  #  "country" = "Kenya",
  #  "location" = NA,
  #  "iso3" = "KEN",
  #  "cm_name_matrix_old" = "Kenya",
  #  "cm_name_populations" = "Kenya",
  #  "type" = c("zenodo", "local_data", "matrix")[2],
  #  "link" = "./data/local_contact_data_other/kenya/",
  #  "year" = 2011
  #),
  "Grijalva" = list(
    "country" = "Peru",
    "location" = "San Marcos Highlands",
    "iso3" = "PER",
    "cm_name_matrix_old" = "Peru",
    "cm_name_populations" = "Peru",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.1095664",
    "year" = 2011
  ),
  "le Polain de Waroux" = list(
    "country" = "Uganda",
    "location" = "Sheema",
    "iso3" = "UGA",
    "cm_name_matrix_old" = "Uganda",
    "cm_name_populations" = "Uganda",
    "type" = c("zenodo", "local_data", "matrix")[2],
    "link" = "./data/local_contact_data_other/uganda/",
    "year" = 2014
  ),
  "Horby" = list(
    "country" = "Vietnam",
    "location" = "Red River Delta",
    "iso3" = "VNM",
    "cm_name_matrix_old" = "Viet Nam",
    "cm_name_populations" = "Viet Nam",
    "type" = c("zenodo", "local_data", "matrix")[1],
    "link" = "https://doi.org/10.5281/zenodo.1289473",
    "year" = 2007
  ),
  "Melegaro" = list(
    "country" = "Zimbabwe",
    "location" = "Manicaland",
    "iso3" = "ZWE",
    "cm_name_matrix_old" = "Zimbabwe",
    "cm_name_populations" = "Zimbabwe",
    "type" = c("zenodo", "local_data", "matrix")[2],
    "link" = "./data/local_contact_data_other/zimbabwe/",
    "year" = 2013
  )
)

empirical_matrices_sourcedata <- rbindlist(
  lapply(
    empirical_matrices_sourcedata,
    as.data.table
  ), fill = TRUE
)[, study := names(empirical_matrices_sourcedata)]

espicc_agegroups <- data.table(
  age_from = seq(0, 60, 10),
  age_to = c(seq(9, 59, 10), 120)
)[, name := paste0(age_from, "-", age_to)]

runs = 1
output = list()
for(i in 1:nrow(empirical_matrices_sourcedata)){
  
  if(empirical_matrices_sourcedata[i, type] == "local_data"){
    participants <- fread(sprintf("%s/participants.csv", empirical_matrices_sourcedata[i, link]))[, year := empirical_matrices_sourcedata[i, year]]
    contacts <- fread(sprintf("%s/contacts.csv", empirical_matrices_sourcedata[i, link]))[, year := empirical_matrices_sourcedata[i, year]]
    
    if(empirical_matrices_sourcedata[i, study] == "Kiti") {
      participants <- participants[, -"cnt_home"]
      contacts[, cnt_home := cnt_home=="Yes"]
    } else {
      contacts[, cnt_home := tolower(location)=="house"]
    }
    
    survey_data <- socialmixr::survey(participants, contacts)
  } else {
    survey_data <- socialmixr::get_survey(survey = empirical_matrices_sourcedata[i, link]) 
  }
  
  #' Hong Kong and Vietnam population data is not read automatically in socialmixr, need to specify survey.pop manually
  if(empirical_matrices_sourcedata[i, country] == "Hong Kong"){
    cm_rates <- socialmixr::contact_matrix(
      survey_data, survey.pop="China, Hong Kong SAR",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = TRUE, symmetric = FALSE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
    
    cm_rates_unw <- socialmixr::contact_matrix(
      survey_data, survey.pop="China, Hong Kong SAR",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = FALSE, symmetric = FALSE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
    
    cm_rates_adj <- socialmixr::contact_matrix(
      survey_data, survey.pop="China, Hong Kong SAR",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = TRUE, symmetric = TRUE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
  } else if(empirical_matrices_sourcedata[i, country] == "Vietnam") {
    cm_rates <- socialmixr::contact_matrix(
      survey_data, survey.pop="Viet Nam",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = TRUE, symmetric = FALSE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
    
    cm_rates_unw <- socialmixr::contact_matrix(
      survey_data, survey.pop="Viet Nam",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = FALSE, symmetric = FALSE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
    
    cm_rates_adj <- socialmixr::contact_matrix(
      survey_data, survey.pop="Viet Nam",
      age.limits = espicc_agegroups[, age_from], bootstrap = FALSE, n = runs,
      weigh.dayofweek = TRUE, symmetric = TRUE,
      estimated.contact.age = "sample", estimated.participant.age = "mean",
      missing.participant.age = "remove", missing.contact.age = "sample")
  } else {
    cm_rates <- socialmixr::contact_matrix(
      survey_data, age.limits = espicc_agegroups[, age_from], bootstrap = FALSE,
      n = runs, weigh.dayofweek = TRUE, symmetric = FALSE, estimated.contact.age = "sample",
      estimated.participant.age = "mean", missing.participant.age = "remove",
      missing.contact.age = "sample")
    
    cm_rates_unw <- socialmixr::contact_matrix(
      survey_data, age.limits = espicc_agegroups[, age_from], bootstrap = FALSE,
      n = runs, weigh.dayofweek = FALSE, symmetric = FALSE, estimated.contact.age = "sample",
      estimated.participant.age = "mean", missing.participant.age = "remove",
      missing.contact.age = "sample")
    
    cm_rates_adj <- socialmixr::contact_matrix(
      survey_data, age.limits = espicc_agegroups[, age_from], bootstrap = FALSE,
      n = runs, weigh.dayofweek = TRUE, symmetric = TRUE, estimated.contact.age = "sample",
      estimated.participant.age = "mean", missing.participant.age = "remove",
      missing.contact.age = "sample")
  }
  
  #' Socialmatrix outputs contactors as rows, change to have contactors as columns, contactees as rows
  cm_rates$matrix = t(cm_rates$matrix)
  cm_rates_total = cm_rates$matrix %*% diag(cm_rates_adj$demography[, population])
  
  name = empirical_matrices_sourcedata[i, c("location", "country", "study")] %>%
    unlist %>% (function(x) paste0({if(is.na(x["location"])) x["country"] else x[c("location", "country")]}, collapse=" - ") %>% paste0(" (", x[3], " et al)"))
  
  output[[name]] = list(ratio = cm_rates_total/t(cm_rates_total),
                        rate_unadj = cm_rates$matrix,
                        rate_adj = t(cm_rates_adj$matrix),
                        observations = t(cm_rates_unw$matrix) %*% diag(cm_rates_adj$participants[, participants]))
}
