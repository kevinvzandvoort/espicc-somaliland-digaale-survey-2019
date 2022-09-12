#' Create contact matrices (E and F in figure 2 equivalent)

#' Calculate sum of all sample weights by age (used in constructing contact matrices)
n_contacts_sample_fpc_weighted = contact_data_frequency_ps$variables[, c("id", "participant_age_group_60")] %>%
  cbind(weights(contact_data_frequency_ps)) %>%
  .[, weight := V2] %>% .[, -"V2"] %>%
  .[, .SD[, .(total = sum(weight))], by="participant_age_group_60"] %>%
  .[order(participant_age_group_60)]

#' Population size by age (used in constructing contact matrices)
n_population_size_fpc =
  household_data_members_svy$variables[, .SD[, .(total = .N * fpc_inflate_N_factor)], by="age_group_60"] %>%
  .[order(age_group_60)]

#' Calculate the contact matrix (post-stratified for age-sex and weighted for weekend and non-weekend days)
contact_matrices_empirical_all_weighted =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  merge(participant_data_design_ps$variables[, c("contactor_id", "participant_age_group_60")], by.x="id",
        by.y="contactor_id") %>%
  .[!is.na(contact_age_group_60), c("participant_sex", "contact_sex", "participant_age_group_60",
                                    "contact_age_group_60", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_60, participant_age_group_60)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[, total], n_population_size_fpc[, total])

#' Store data in list
contacts_sensitivity_data[[POSTSTRATIFICATION_STRATA]] = contact_matrices_empirical_all_weighted

rm("n_contacts_sample_fpc_weighted", "n_population_size_fpc", "contact_matrices_empirical_all_weighted")
setwd(analysis_dir)
