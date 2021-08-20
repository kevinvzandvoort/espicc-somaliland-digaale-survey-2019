#' This script has been used to generate the data for the record in the Zenodo Social contact data community, that can
#' be used with the socialmixr package
#' - install.packages("socialmixr") to install the socialmixr package
#' - Zenodo record: (DOI: )

dir.create("./zenodo_socialmixr")

#' Create participant_common.csv file
contact_data_contactors %>% copy %>%
  .[, c("part_id", "part_age", "part_gender") := .(id, participant_age_years, participant_sex)] %>%
  .[, part_gender := factor(part_gender, c("female", "male"), c("F", "M"))] %>%
  .[, c("part_id", "part_age", "part_gender")] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_participant_common.csv")

#' Create participant_extra.csv file
contact_data_contactors[, c("id", "contacts_other_est", "pw")] %>%
  merge(participant_data[, c("occupation", "household_size", "contactor_id")], by.x="id", by.y="contactor_id") %>%
  .[, c("part_id", "sample_weight") := .(id, pw)] %>%
  .[, c("part_id", "sample_weight", "household_size", "contacts_other_est")] %>%
  .[, country := "Somaliland"] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_participant_extra.csv")

#' Create contact_common.csv file
contact_data_contactees = contact_data_contactees %>% setorder(contactor_id) %>% .[, contactee_id := .I]
contact_common = contact_data_contactees %>% copy %>% setorder(contactor_id) %>%
  .[, c("part_id", "cont_id", "cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max", "cnt_gender",
        "cnt_home", "cnt_work", "cnt_school", "cnt_shop", "cnt_otherplace",
        "frequency_multi", "phys_contact", "duration_multi") :=
      .(contactor_id, contactee_id, contact_age, NA_integer_, NA_integer_, contact_sex,
        grepl("Home", contact_setting, F, T) | grepl("Another house", contact_setting, F, T),
        grepl("Work", contact_setting, F, T), grepl("School", contact_setting, F, T),
        grepl("Shop", contact_setting, F, T) | grepl("Market", contact_setting, F, T),
        grepl("Water source", contact_setting, F, T) | grepl("Transport", contact_setting, F, T) |
          grepl("Mosque", contact_setting, F, T) | grepl("Other", contact_setting, F, T),
        as.numeric(contact_frequency), as.numeric(contact_type), as.numeric(contact_duration))] %>%
  .[, cnt_gender := factor(cnt_gender, c("female", "male"), c("F", "M"))]
contact_common[frequency_multi == 6, frequency_multi := 98]
contact_common[frequency_multi == 7, frequency_multi := 99]
contact_common %>%
  .[, c("part_id", "cont_id", "cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max", "cnt_gender",
        "cnt_home", "cnt_work", "cnt_school", "cnt_shop", "cnt_otherplace",
        "frequency_multi", "phys_contact", "duration_multi")] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_contact_common.csv")

#' Create contact_extra.csv file
contact_data_contactees[, c("contactee_id", "contact_relationship", "contact_setting", "contact_city")] %>%
  .[, cont_id := contactee_id] %>% .[, c("cont_id", "contact_relationship", "contact_setting", "contact_city")] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_contact_extra.csv")

#' Create sday.csv file
contact_data_contactors[, c("id", "contacts_day", "contacts_date")] %>%
  .[, c("part_id", "sday_id", "day", "month", "year", "dayofweek", "dayofweek_name", "weekend") :=
      .(id, format(contacts_date, "%Y%m%d"), format(contacts_date, "%d"), format(contacts_date, "%m"),
        format(contacts_date, "%Y"), format(contacts_date, "%w"), contacts_day,
        contacts_day %in% c("Friday", "Saturday"))] %>%
  .[, -c("id", "contacts_day", "contacts_date")] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_sday.csv")

#' Create survey_population.csv file
#' - Note that this is a nonstandard file, and not automatically loaded by socialmixr
#' - Can be used to make data representative for the Digaale population
n_population_size_fpc %>% merge(age_groups_80, by.x="age_group_80", by.y="name") %>%
  .[, c("lower.age.limit", "population") := .(age_from, total)] %>%.[, c("lower.age.limit", "population")] %>%
  fwrite("./zenodo_socialmixr/espicc_somaliland_digaale_survey_population.csv")