#' The total number of unique households from where contactors are sampled
#' - household identifiers have been removed from the data
n_households_contactors = 426 #length(unique(contact_data[, household_id]))

#' Generate categories for household size based on quantiles (will be used in post-stratification)
household_size_groups = household_data_members %>% copy %>%
  .[, household_size := .N, by="household_id"] %>%
  .[, c("household_id", "household_size")] %>% unique %>%
  .[, household_size] %>% quantile(c(0.25, 0.5, 0.75, 1)) %>%
  (function(x){
    z = c(0, x)
    list(names = paste0(z[-length(z)] + 1, "-", z[-1]) %>%
           sapply(seq_along(.), function(i, n, z){
             rep(n[i], diff(z)[i])
           }, ., z) %>% unlist,
         values = c(1:x[length(x)]) %>% as.character) %>% return
  })

#' Generate categories for household size based on 33rd, 66th, and 100th percentiles
#'  for only those families with <5yo
household_size_groups_u5 = household_data_members %>% copy %>%
  .[household_id %in% household_data_members[u5 == TRUE, unique(household_id)]] %>%
  .[, household_size := .N, by="household_id"] %>%
  .[, c("household_id", "household_size")] %>% unique %>%
  .[, household_size] %>% quantile(c(0.33, 0.66, 1)) %>%
  (function(x){
    z = c(0, x)
    list(names = paste0(z[-length(z)] + 1, "-", z[-1]) %>%
           sapply(seq_along(.), function(i, n, z){
             rep(n[i], diff(z)[i])
           }, ., z) %>% unlist,
         values = c(1:x[length(x)]) %>% as.character) %>% return
  })

#' Post-stratification is done on the joint age- and sex-distribution of the population
#'  - The age- and sex-distribution is calculated as the observed distribution in all households included in the survey
#'    inflated to account for households not included in the sample
#'  - We also conduct several sensitivity analyses based on the weights that will be used
poststratification_strata_list = list(
  "age_sex_householdsize" =
    household_data_members %>% copy %>%
    .[, household_size := .N, by="household_id"] %>%
    .[, household_size_group := factor(as.character(household_size), household_size_groups$values,
                                       household_size_groups$names)] %>%
    .[, .(Freq = .N * fpc_inflate_N_factor), by=c("age_group_sample", "household_member_sex", "household_size_group")] %>%
    .[, stype := paste(household_size_group, age_group_sample, household_member_sex, sep="-")] %>%
    setorder(household_size_group, household_member_sex, age_group_sample) %>%
    .[, c("stype", "Freq")],
  "age_householdsize" =
    household_data_members %>% copy %>%
    .[, household_size := .N, by="household_id"] %>%
    .[, household_size_group := factor(as.character(household_size), household_size_groups$values,
                                       household_size_groups$names)] %>%
    .[, .(Freq = .N * fpc_inflate_N_factor), by=c("age_group_sample", "household_size_group")] %>%
    .[, stype := paste(household_size_group, age_group_sample, sep="-")] %>%
    setorder(household_size_group, age_group_sample) %>%
    .[, c("stype", "Freq")],
  "age_sex" =
    household_data_members[, .(Freq = .N * fpc_inflate_N_factor), by=c("household_member_sex", "age_group_sample")] %>%
    .[, stype := paste0(household_member_sex, "-", age_group_sample)] %>%
    setorder(household_member_sex, age_group_sample) %>%
    .[,c("stype", "Freq")],
  "age" =
    household_data_members[, .(Freq = .N * fpc_inflate_N_factor), by=c("age_group_sample")] %>%
    .[, stype := age_group_sample] %>%
    setorder(age_group_sample) %>%
    .[,c("stype", "Freq")],
  "none" = household_data_members %>% copy %>% .[, stype := "none"] %>%
    .[, .(Freq = .N * fpc_inflate_N_factor), by=c("stype")])

#' Separately done for u5 for certain sub-analyses
#' - age in years has been removed from the household members data, therefore hard-coded here
poststratification_strata_u5_list = readRDS("./data/u5_poststratification_freq.RDS")
poststratification_strata_u5_list$age_householdsize =
  poststratification_strata_u5_list$age_sex_householdsize %>% copy %>%
  .[, .(Freq = sum(Freq)), by=c("household_member_age", "household_size_group_u5")]
poststratification_strata_u5_list$age_sex_householdsize =
  poststratification_strata_u5_list$age_sex_householdsize %>%
  .[, stype := paste(household_size_group_u5, household_member_age, household_member_sex, sep="-")]
  #.[, stype := paste(household_size_group_u5, household_member_age, sep="-")]
poststratification_strata_u5_list$age_householdsize =
  poststratification_strata_u5_list$age_householdsize %>%
  .[, stype := paste(household_size_group_u5, household_member_age, sep="-")]
poststratification_strata_u5_list$age_sex =
  poststratification_strata_u5_list$age_sex %>%
  .[, stype := paste(household_member_sex, household_member_age, sep="-")]
poststratification_strata_u5_list$age =
  poststratification_strata_u5_list$age %>% .[, stype := household_member_age]
poststratification_strata_u5_list$none =
  poststratification_strata_u5_list$none %>% .[, stype := "none"]

#' Retrieve the population distribution for the poststratification strata that are used
poststratification_strata = poststratification_strata_list[[POSTSTRATIFICATION_STRATA]]
poststratification_strata_u5 = poststratification_strata_u5_list[[POSTSTRATIFICATION_STRATA]] %>% .[, c("stype", "Freq")]

#' We apply the post-stratification strata to our participant using the appropriate methods from the Survey package
#' - the population size in each age- and sex-stratum is used to apply a finite population correction (fpc)
participant_data = participant_data %>%
  #' Initially, data was weighted by the inverse of the household size and the age-gender distribution,
  #'  weighting by household size was removed in a revision, and now set to 1 for all participants. Post-stratification
  #'  weights are still applied
  #.[, ps := 1/household_size * (n_households_contactors/n_shelters)] %>%
  #.[, pw := 1/ps] %>%
  .[, pw := 1] %>%
  .[, pw := pw * .N/sum(pw)] %>%
  .[, household_size_group := factor(as.character(household_size), household_size_groups$values,
                                     household_size_groups$names)] %>%
  .[, stype_age_sex_householdsize := paste(household_size_group, participant_age_group_sample, participant_sex, sep="-")] %>%
  .[, stype_age_householdsize := paste(household_size_group, participant_age_group_sample, sep="-")] %>%
  .[, stype_age_sex := paste(participant_sex, participant_age_group_sample, sep="-")] %>%
  .[, stype_age := participant_age_group_sample] %>%
  .[, stype_none := "none"]

participant_data = participant_data %>%
  .[, stype := get(paste0("stype_", POSTSTRATIFICATION_STRATA))] %>%
  merge(poststratification_strata, by="stype") %>% .[, fpc := Freq]

#' Check who reported having been diagnosed with pneumonia in the last 6 months
participant_data[participant_consent == "yes", pneumonia_6m := FALSE]
participant_data[!is.na(morbidities_pneumonia_time),
                 pneumonia_6m := morbidities_pneumonia_time == "Less than six months ago"]

#' Combine travel categories of 10_50km and 50km_gt in 10km_gt (10km or greater; very few reported 50km_gt)
#' Recode don't know or refuse to answer as a missing value
participant_data[travel_0_5km %in% c("don’t know", "refuse to answer"), travel_0_5km := NA]
participant_data[travel_5_10km %in% c("don’t know", "refuse to answer"), travel_5_10km := NA]
participant_data[travel_10_50km %in% c("don’t know", "refuse to answer"), travel_10_50km := NA]
participant_data[travel_50km_gt %in% c("don’t know", "refuse to answer"), travel_50km_gt := NA]
participant_data[, travel_10km_gt := pmin(as.numeric(travel_10_50km), as.numeric(travel_50km_gt), na.rm=T)]
participant_data[, travel_10km_gt := factor(travel_10km_gt,
                                            levels=1:length(attr(participant_data$travel_50km_gt, "levels")),
                                            labels=attr(participant_data$travel_50km_gt, "levels"))]

#' Create a survey design object to process participant estimates
#' - we set partial=TRUE as there are no male participants aged 30-49yo who live in household of size 5-6 or 7-12
#'   in our sample
participant_data_design_ps = survey::svydesign(ids=~1, probs=NULL, strata=~stype, fpc=~fpc, weights=~pw,
                                               data=participant_data[participant_consent == "yes"]) %>%
  postStratify(~stype, poststratification_strata, partial=TRUE)

#' Trim weights that are outliers so weights don't overinfluence data
wlow = quantile(weights(participant_data_design_ps), 0.05)
whigh = quantile(weights(participant_data_design_ps), 0.95)
participant_data_design_ps = participant_data_design_ps %>%
  survey::trimWeights(whigh, wlow)

#' Indirect contacts (contacts_other_est) are estimated as the median of the range in the selected category
contact_data_contactors = contact_data_contactors %>%
  .[contacts_other_est == "0", contacts_other_median := 0] %>%
  .[contacts_other_est == "1-2", contacts_other_median := median(1,2)] %>%
  .[contacts_other_est == "3-5", contacts_other_median := median(3,5)] %>%
  .[contacts_other_est == "6-10", contacts_other_median := median(6,10)] %>%
  .[contacts_other_est == "11-20", contacts_other_median := median(11,20)] %>%
  .[contacts_other_est == ">20", contacts_other_median := 20]

#' We combine the contact data with the aggregate number of different types of contact, per person
contact_data = contact_data_contactors %>%
  merge(contact_data_contactees[, .SD[, .(
    "total_recorded_contacts" = .N,
    "home_contacts" = sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("Home", "Another house")))),
    "home_only_contacts" = sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("Home")))),
    "other_house_contacts" = sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("Another house")))),
    "school_contacts" = sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("School")))),
    "work_contacts" = sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("Work")))),
    "school_or_work_contacts" =
      sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("School", "Work")))),
    "other_contacts" =
      sum(sapply(strsplit(contact_setting, "; "), function(x) any(x %in% c("Market", "Other", "Shop", "Transport",
                                                                           "Mosque", "Water source")))),
    "home_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("Home", "Another house"))}) &
            contact_type == "Physical"),
    "home_only_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("Home"))}) & contact_type == "Physical"),
    "other_house_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("Another house"))}) &
            contact_type == "Physical"),
    "school_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("School"))}) & contact_type == "Physical"),
    "work_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("Work"))}) & contact_type == "Physical"),
    "school_or_work_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("School", "Work"))}) &
            contact_type == "Physical"),
    "other_contacts_physical" =
      sum(sapply(strsplit(contact_setting, "; "), function(x){ any(x %in% c("Market", "Other", "Shop", "Transport",
                                                                            "Mosque", "Water source"))}) &
            contact_type == "Physical"),
    "physical_contacts" = sum(contact_type=="Physical"))], by="contactor_id"],
    by.x="id", by.y="contactor_id", all.x=TRUE)

#' We can combine the additional participant data with the contact data
#' - this will also assign the correct stype to the contact data
contact_data = contact_data[, -c("participant_sex", "pw")] %>% merge(participant_data, by.x="id", by.y="contactor_id")

rm("participant_data")

#' Weekend days and non-weekend days are given different weights, to ensure representativeness of the contact estimates
#' - note that the weekend in Somaliland falls on Fridays and Saturdays
contact_data[contacts_day %in% c("Friday", "Saturday"), pw_weekday := 1]
contact_data[!contacts_day %in% c("Friday", "Saturday"), pw_weekday := 5/2]
contact_data[, pw_weekday := pw * pw_weekday]
contact_data[, pw_weekday := pw_weekday * (.N/sum(pw_weekday))]

#' We create a design object for the contact matrix, accounting for a finite population correction and sample weights
#'  including weight of day of the week
contact_data_frequency_ps =
  svydesign(ids=~1, probs=NULL, strata=~stype, fpc=~fpc, weights=~pw_weekday,
            data=contact_data[participant_consent == "yes"]) %>%
  postStratify(~stype, poststratification_strata, partial=TRUE)

#' Trim weights so no individual has too high influence
wlow = quantile(weights(contact_data_frequency_ps), 0.05)
whigh = quantile(weights(contact_data_frequency_ps), 0.95)
contact_data_frequency_ps = contact_data_frequency_ps %>% trimWeights(whigh, wlow)

#' We also calculate the proportion of contacts in each category
contact_data_full = contact_data_contactors %>%
  merge(contact_data_contactees, by.x="id", by.y="contactor_id", all.x=TRUE)

#' Create temporary tables to calculate proportion of contact types
z_contact_frequency = contact_data_full %>%
  .[contact_frequency %in% c("Never met before", "At least once per month", "Less than once per month"),
    contact_frequency := "Less than once per week"] %>%
  .[, as.data.table(.SD[, table(contact_frequency)]), by="id"] %>%
  .[, variable := tolower(gsub(" ","_",paste0("contact_frequency","_",contact_frequency,"_total")))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_contact_frequency_physical = contact_data_full %>%
  .[contact_frequency %in% c("Never met before", "At least once per month", "Less than once per month"),
    contact_frequency := "Less than once per week"] %>%
  .[, as.data.table(.SD[, table(contact_type, contact_frequency)]), by="id"] %>%
  .[, variable := tolower(gsub(" ", "_", paste0("contact_frequency","_",contact_frequency,"_",contact_type)))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_contact_relationship = contact_data_full %>%
  .[!contact_relationship %in% c("refuse to answer", "don’t know")] %>%
  .[, as.data.table(.SD[, table(contact_relationship)]), by="id"] %>%
  .[, variable := tolower(gsub(" ","_",paste0("contact_relationship","_",contact_relationship,"_total")))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_contact_relationship_physical = contact_data_full %>%
  .[!contact_relationship %in% c("refuse to answer", "don’t know")] %>%
  .[, as.data.table(.SD[, table(contact_type, contact_relationship)]), by="id"] %>%
  .[, variable := tolower(gsub(" ","_",paste0("contact_relationship","_",contact_relationship,"_",contact_type)))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_contact_duration = contact_data_full %>%
  .[!contact_duration %in% c("refuse to answer", "don’t know")] %>%
  .[, as.data.table(.SD[, table(contact_duration)]), by="id"] %>%
  .[, variable := tolower(gsub(" ","_",paste0("contact_duration","_",contact_duration,"_total")))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_contact_duration_physical = contact_data_full %>%
  .[!contact_duration %in% c("refuse to answer", "don’t know")] %>%
  .[, as.data.table(.SD[, table(contact_type, contact_duration)]), by="id"] %>%
  .[, variable := tolower(gsub(" ","_",paste0("contact_duration","_",contact_duration,"_",contact_type)))] %>%
  .[, c("id", "variable", "N")] %>%
  dcast(...~variable, value.var="N")

z_merge = z_contact_frequency %>%
  merge(z_contact_frequency_physical, by="id") %>%
  merge(z_contact_relationship, by="id") %>%
  merge(z_contact_relationship_physical, by="id") %>%
  merge(z_contact_duration, by="id") %>%
  merge(z_contact_duration_physical, by="id") 

rm("z_contact_frequency", "z_contact_frequency_physical", "z_contact_relationship", "z_contact_relationship_physical",
   "z_contact_duration", "z_contact_duration_physical", "contact_data_contactors")

#' We combine all in one dataset holding the proportion of contacts of a certain type, per contactor
contact_data_analysis_prop =
  contact_data[participant_consent == "yes",
               c("id", "participant_sex", "participant_age_group_80", "participant_age_group_60",
                 "participant_age_group_sample", "pw_weekday", "stype", "fpc", "contacts_total",
                 "total_recorded_contacts", "contacts_other_median",
                 sapply(c("home_contacts", "home_only_contacts", "other_house_contacts", "school_contacts",
                          "work_contacts", "school_or_work_contacts", "other_contacts"),
                        function(x) paste0(x, c("", "_physical")))), with=F] %>%
  merge(z_merge[, -c(colnames(z_merge)[grepl("know", colnames(z_merge)) | grepl("refuse", colnames(z_merge))]), with=F],
        by="id") %>%
  melt(id.vars = c("id", "participant_sex", "participant_age_group_80", "participant_age_group_60",
                   "participant_age_group_sample", "pw_weekday", "stype", "fpc", "contacts_total",
                   "total_recorded_contacts", "contacts_other_median")) %>%
  .[, prop := value/total_recorded_contacts] %>%
  .[, -"value"] %>%
  dcast(...~variable, value.var = "prop")

#' We create a survey design object for this data
contact_data_proportion_ps =
  svydesign(ids=~1, probs=NULL, strata=~stype, fpc=~fpc, weights=~pw_weekday, data=contact_data_analysis_prop) %>%
  postStratify(~stype, poststratification_strata, partial=TRUE)

rm("contact_data_analysis_prop", "z_merge")

#' Trim weights so no individual has too high influence
wlow = quantile(weights(contact_data_proportion_ps), 0.05)
whigh = quantile(weights(contact_data_proportion_ps), 0.95)
contact_data_proportion_ps = contact_data_proportion_ps %>% trimWeights(whigh, wlow)

#' Process household_data
household_data = household_data %>%
  .[, house_fuel_charcoal := grepl("charcoal", house_fuel, ignore.case = TRUE)] %>%
  .[, house_fuel_firewood := grepl("firewood", house_fuel, ignore.case = TRUE)] %>%
  .[, house_water_source_rainwater := grepl("rainwater", water_source, ignore.case = TRUE)] %>%
  .[, house_water_source_tanktruck := grepl("tanker truck", water_source, ignore.case = TRUE)] %>%
  .[, house_substance_use_snuff := grepl("snuff", household_substance_use, ignore.case = TRUE)] %>%
  .[, house_substance_use_smoke := grepl("smoke", household_substance_use, ignore.case = TRUE)] %>%
  .[, house_substance_use_khat := grepl("khat", household_substance_use, ignore.case = TRUE)] %>%
  .[, house_substance_use_alcohol := grepl("alcohol", household_substance_use, ignore.case = TRUE)] %>%
  .[, house_substance_use_none := grepl("none", household_substance_use, ignore.case = TRUE)]

#' Create a survey design object for the household data
#' - Households have been sampled following a simple random sample
household_data_svy =
  svydesign(ids=~1, probs=NULL, strata=NULL, fpc=~fpc, weights=NULL, data=household_data[household_consent == "yes"] %>%
              .[, fpc := n_shelters_for_fpc])

#' Create a survey design for the household members data
household_data_members_svy = svydesign(ids=~1, probs=NULL, strata=NULL, fpc = ~fpc, weights=NULL,
                                       data=household_data_members[, fpc := .N * fpc_inflate_N_factor])

#' Skip these steps if only running contact data in sensitivity analysis
if(!CONTACT_DATA_ONLY){
  #' Process data to be used for person-time (for estimating birth, death, and migration rates)
  household_data_members[, born := 0]
  household_data_members[infant == TRUE & household_member_inmigration == "yes", born := 1]
  
  #' assume that 50% of children <1 are born in last 6 months
  household_data_members[id %in% sample(
    x = household_data_members[infant == TRUE & born == 0, id],
    size = floor(household_data_members[infant == TRUE, .(N=.N, born=sum(born))][, (N-born)/2]),
    replace = FALSE), born := 1]
  
  household_data_members[, c("death", "migrate_out", "migrate_in") :=
                           .(0, 0, as.numeric(household_member_inmigration == "yes" & born == 0))]
  
  household_data_members_mortality[, id := seq_len(.N)]
  household_data_members_mortality[, born := 0] 
  household_data_members_mortality[infant == TRUE & joined_6m == "yes", born := 1]
  household_data_members_mortality[id %in% sample(
    x = household_data_members_mortality[infant == TRUE & born == 0, id],
    size = floor(household_data_members_mortality[infant == TRUE, .(N=.N, born=sum(born))][, (N-born)/2]),
    replace = FALSE), born := 1]
  
  household_data_members_mortality[, c("death", "migrate_out", "migrate_in") :=
                                     .(1, 0, as.numeric(joined_6m == "yes" & born == 0))]
  
  household_data_members_migration[, id := seq_len(.N)]
  household_data_members_migration[, born := 0]
  household_data_members_migration[infant == TRUE & joined_6m == "yes", born := 1]
  household_data_members_migration[id %in% sample(
    x = household_data_members_mortality[infant == TRUE & born == 0, id],
    size = floor(household_data_members_migration[infant == TRUE, .(N=.N, born=sum(born))][, (N-born)/2]),
    replace = FALSE), born := 1]
  
  household_data_members_migration[, c("death", "migrate_out", "migrate_in") :=
                                     .(0, 1, as.numeric(joined_6m == "yes" & born == 0))]
  
  #' We do not know exactly when events happened in the last six months
  #' As to not underestimate uncertainty, we impute the time of the event by sampling from uniform distributions
  #' We create 10000 imputed datasets, and use these to estimate uncertainty in this data
  #' - See, Vandormael et al, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5837439/
  message("Creating 10000 imputed datasets to estimate events in the past 6 months, may take a few (<5) mins")
  household_data_members_rates_imp = seq_len(10000) %>% lapply(function(i){
    if(i %% 1000 == 0) message(sprintf("%s/%s", i, 10000))
    household_data_members_rates = rbind(
      household_data_members[, c("u5", "born", "death", "migrate_in", "migrate_out", "age_group_sample")],
      household_data_members_mortality[, c("u5", "born", "death", "migrate_in", "migrate_out", "age_group_sample")],
      household_data_members_migration[, c("u5", "born", "death", "migrate_in", "migrate_out", "age_group_sample")])
    
    household_data_members_rates[, follow_up_time := 6]
    household_data_members_rates[born == 1 | migrate_in == 1, follow_up_time := follow_up_time - runif(.N, 0, follow_up_time)]
    household_data_members_rates[death == 1 | migrate_out == 1, follow_up_time := follow_up_time - runif(.N, 0, follow_up_time)]
    household_data_members_rates[, fpc := .N * fpc_inflate_N_factor]
    
    return(household_data_members_rates)})
  household_data_members_rates_imp = mitools::imputationList(household_data_members_rates_imp)
  
  #' Create a survey design object for the household member rates 
  #' - Used to assess mortality rate, birthrate, and migration rates
  household_data_members_rates_svy_imp =
    survey::svydesign(ids = ~1, probs = NULL, strata = NULL, fpc = ~fpc, weights = NULL,
                      data = household_data_members_rates_imp)
  message("Finished creating imputed datasets. Created object household_data_members_rates_svy_imp")
  #rm("household_data_members", "household_data_members_mortality", "household_data_members_migration")
  
  #' Process nutrition data
  
  #' zscorer package requires sex to be coded as 1 for males, and 2 for females
  nutrition_data[, participant_sex_var := as.numeric(factor(participant_sex, levels=c("male", "female")))]
  #' zscorer package requires standing to be coded as 1 for yes (measuring height), and 2 for no (measuring length)
  nutrition_data[, standing := as.numeric(factor(height_type, levels=c("height", "length")))]
  nutrition_data[is.na(height_type), standing := 3]
  
  #' Use zscorer to estimate wfa, hfa, wfh, and mfa z-scores
  wfa = zscorer::addWGSR(nutrition_data[, c("id", "participant_sex_var", "weight", "participant_age_days", "standing")],
                         sex = "participant_sex_var", firstPart = "weight", secondPart = "participant_age_days",
                         index = "wfa", standing = "standing")
  nutrition_data = merge(nutrition_data, wfa[,c("id","wfaz")], by="id")
  
  hfa = zscorer::addWGSR(nutrition_data[, c("id", "participant_sex_var", "height", "participant_age_days", "standing")],
                         sex = "participant_sex_var", firstPart = "height", secondPart = "participant_age_days",
                         index = "hfa", standing = "standing")
  nutrition_data = merge(nutrition_data, hfa[,c("id","hfaz")], by="id")
  
  wfh = zscorer::addWGSR(nutrition_data[, c("id", "participant_sex_var", "height", "weight", "standing")],
                         sex = "participant_sex_var", firstPart = "weight", secondPart = "height", index = "wfh",
                         standing = "standing")
  nutrition_data = merge(nutrition_data, wfh[,c("id","wfhz")], by="id")
  
  mfa = zscorer::addWGSR(nutrition_data[, c("id", "participant_sex_var", "muac", "participant_age_days", "standing")],
                         sex = "participant_sex_var", firstPart = "muac", secondPart = "participant_age_days",
                         index = "mfa", standing = "standing")
  nutrition_data = merge(nutrition_data, mfa[,c("id","mfaz")], by="id")
  
  rm("wfa", "hfa", "wfh", "mfa")
  
  #' recode z-scores as categories
  nutrition_data[wfaz > -2, weight_for_age := 1]
  nutrition_data[wfaz <= -2, weight_for_age := 2]
  nutrition_data[wfaz <= -3, weight_for_age := 3]
  nutrition_data[, weight_for_age := factor(weight_for_age, 1:3, c("Not underweight (z > -2)", "Underweight (z <= -2)",
                                                                   "Underweight (z <= -3)"))]
  nutrition_data[hfaz > -2, height_for_age := 1]
  nutrition_data[hfaz <= -2, height_for_age := 2]
  nutrition_data[hfaz <= -3, height_for_age := 3]
  nutrition_data[, height_for_age := factor(height_for_age, 1:3, c("Not stunted (z > -2)", "Stunted (z <= -2)",
                                                                   "Severely stunted (z <= -3)"))]
  nutrition_data[wfhz > -2, weight_for_height := 1]
  nutrition_data[wfhz <= -2, weight_for_height := 2]
  nutrition_data[wfhz <= -3, weight_for_height := 3]
  nutrition_data[, weight_for_height := factor(weight_for_height, 1:3, c("Not wasted (z > -2)", "Wasted (z <= -2)",
                                                                         "Severely wasted (z <= -3)"))]
  nutrition_data[(muac * 10) >= 125, muac_level := 1]
  nutrition_data[(muac * 10) < 125, muac_level := 2]
  nutrition_data[(muac * 10) < 115, muac_level := 3]
  nutrition_data[, muac_level := factor(muac_level, 1:3, c("Not wasted (>= 125mm)", "Wasted (< -125mm)",
                                                           "Severely wasted (< -115mm)"))]
  
  #' Set U5 stypes for nutrition data
  nutrition_data = 
    nutrition_data %>%
    merge(household_data[!is.na(household_members), .(id=id, household_size=household_members)],
          by.x="household_id", by.y="id", all.x=TRUE)
  nutrition_data %>%
    .[, household_size_group_u5 := factor(as.character(household_size),
                                          household_size_groups_u5$values, household_size_groups_u5$names)] %>%
    .[, participant_age_years := floor(participant_age_days/365)] %>%
    .[, stype_age_sex_householdsize := paste(household_size_group_u5, participant_age_years, participant_sex, sep="-")] %>%
    .[, stype_age_householdsize := paste(household_size_group_u5, participant_age_years, sep="-")] %>%
    .[, stype_age_sex := paste(participant_sex, participant_age_years, sep="-")] %>%
    .[, stype_age := participant_age_years] %>%
    .[, stype_none := "none"]
  
  #' Remove superfluous columns
  nutrition_data = nutrition_data[, -c("household_size", "household_size_group_u5", "participant_age_years")]
  nutrition_data[, stype := get(paste0("stype_", POSTSTRATIFICATION_STRATA))]
  nutrition_data = nutrition_data[, -c("stype_age_sex_householdsize", "stype_age_householdsize", "stype_age_sex", "stype_age", "stype_none")]
  rm("household_data")
  
  #' Set correct stype
  nutrition_data = nutrition_data %>%
    merge(poststratification_strata_u5, by="stype") %>% .[, fpc := Freq] %>% .[, -"Freq"]
  
  #' Create survey design object for nutrition data
  nutrition_data_strat_ps = survey::svydesign(
    ids=~1, probs=NULL, strata=~stype, variables=NULL, fpc=~fpc, weights=NULL,
    data=nutrition_data[participant_consent == "yes"]) %>% survey::postStratify(nutrition_data_strat, strata=~stype,
                                                                                population=poststratification_strata_u5, partial=TRUE)
  
  #' Trim weights to remove any outliers
  wlow = quantile(weights(nutrition_data_strat_ps), 0.05)
  whigh = quantile(weights(nutrition_data_strat_ps), 0.95)
  nutrition_data_strat_ps = nutrition_data_strat_ps %>% trimWeights(whigh, wlow)
  
  rm("nutrition_data")
  
  #' The regression_data object is a combined anonymised and aggregated dataset of contact_data, household_data,
  #'  and nutrition_data. This data has been aggregated using the same functions as listed for these individual datasets,
  #'  but identifiers to combine these datasets have been removed.
}

