household_data = readRDS(sprintf("%s/data/household_data.RDS", analysis_dir))
household_data_members = readRDS(sprintf("%s/data/household_data_members.RDS", analysis_dir))
household_data_members_mortality = readRDS(sprintf("%s/data/household_data_members_mortality.RDS", analysis_dir))
household_data_members_migration = readRDS(sprintf("%s/data/household_data_members_migration.RDS", analysis_dir))
missing_houses_manual_categories = readRDS(sprintf("%s/data/missing_houses_manual_categories.RDS", analysis_dir))
regression_data = readRDS(sprintf("%s/data/regression_data.RDS", analysis_dir))
participant_data = readRDS(sprintf("%s/data/participant_data.RDS", analysis_dir))
contact_data_contactors = readRDS(sprintf("%s/data/contact_data_contactors.RDS", analysis_dir))
contact_data_contactees = readRDS(sprintf("%s/data/contact_data_contactees.RDS", analysis_dir))
nutrition_data = readRDS(sprintf("%s/data/nutrition_data.RDS", analysis_dir))

#' Add different age groups to data (to be used in different analyses)
age_groups_80 = data.table(
  age_from = c(seq(0, 80, 10)),
  age_to = c(seq(9, 79, 10), 100)
)[, name := paste0(age_from, "-", age_to)][age_to == 100, name := "80+"]

age_groups_60 = data.table(
  age_from = c(seq(0, 60, 10)),
  age_to = c(seq(9, 59, 10), 100)
)[, name := paste0(age_from, "-", age_to)][age_to == 100, name := "60+"]

age_groups_sample = data.table(
  age_from = c(0, 2, 6, 15, 30, 50),
  age_to = c(1, 5, 14, 29, 49, 100)
)[, name := paste0(age_from, "-", age_to)][age_to == 1, name := "<2"][age_to == 100, name := "50+"]

age_groups_wide = data.table(
  age_from = c(seq(0, 20, 10), seq(40, 60, 20)),
  age_to = c(seq(9, 19, 10), seq(39, 59, 20), 200)
)[, name := paste0(age_from, "-", age_to)] %>% .[age_from==60, name := "60+"]

for(i in 1:nrow(age_groups_80)){
  household_data_members_mortality[age >= age_groups_80[i, age_from] & age <= age_groups_80[i, age_to],
                                   age_group_80 := factor(age_groups_80[i, name], age_groups_80[, name])]
  household_data_members_migration[age >= age_groups_80[i, age_from] & age <= age_groups_80[i, age_to],
                                   age_group_80 := factor(age_groups_80[i, name], age_groups_80[, name])]
  contact_data_contactees[contact_age >= age_groups_80[i, age_from] & contact_age <= age_groups_80[i, age_to],
                          contact_age_group_80 := age_groups_80[i, name]]
}
household_data_members_mortality[, age_group_80 := factor(age_group_80, age_groups_80$name)]
household_data_members_migration[, age_group_80 := factor(age_group_80, age_groups_80$name)]
contact_data_contactees[, contact_age_group_80 := factor(contact_age_group_80, age_groups_80$name)]
participant_data[, participant_age_group_80 := factor(participant_age_group_80, age_groups_80$name)]
household_data_members[, age_group_80 := factor(age_group_80, age_groups_80$name)]

for(i in 1:nrow(age_groups_60)){
  household_data_members_mortality[age >= age_groups_60[i, age_from] & age <= age_groups_60[i, age_to],
                                   age_group_60 := factor(age_groups_60[i, name], age_groups_60[, name])]
  household_data_members_migration[age >= age_groups_60[i, age_from] & age <= age_groups_60[i, age_to],
                                   age_group_60 := factor(age_groups_60[i, name], age_groups_60[, name])]
  contact_data_contactees[contact_age >= age_groups_60[i, age_from] & contact_age <= age_groups_60[i, age_to],
                          contact_age_group_60 := age_groups_60[i, name]]
}
household_data_members_mortality[, age_group_60 := factor(age_group_60, age_groups_60$name)]
household_data_members_migration[, age_group_60 := factor(age_group_60, age_groups_60$name)]
contact_data_contactees[, contact_age_group_60 := factor(contact_age_group_60, age_groups_60$name)]
participant_data[, participant_age_group_60 := factor(participant_age_group_60, age_groups_60$name)]
household_data_members[, age_group_60 := factor(age_group_60, age_groups_60$name)]

for(i in 1:nrow(age_groups_sample)){
  household_data_members_mortality[age >= age_groups_sample[i, age_from] & age <= age_groups_sample[i, age_to],
                                   age_group_sample := factor(age_groups_sample[i, name], age_groups_sample[, name])]
  household_data_members_migration[age >= age_groups_sample[i, age_from] & age <= age_groups_sample[i, age_to],
                                   age_group_sample := factor(age_groups_sample[i, name], age_groups_sample[, name])]
  contact_data_contactees[contact_age >= age_groups_sample[i, age_from] & contact_age <= age_groups_sample[i, age_to],
                          contact_age_group_sample := age_groups_sample[i, name]]
}
household_data_members_mortality[, age_group_sample := factor(age_group_sample, age_groups_sample$name)]
household_data_members_migration[, age_group_sample := factor(age_group_sample, age_groups_sample$name)]
contact_data_contactees[, contact_age_group_sample := factor(contact_age_group_sample, age_groups_sample$name)]
participant_data[, participant_age_group_sample := factor(participant_age_group_sample, age_groups_sample$name)]
household_data_members[, age_group_sample := factor(age_group_sample, age_groups_sample$name)]
regression_data[, participant_age_group_sample := factor(participant_age_group_sample, age_groups_sample$name)]

for(i in 1:nrow(age_groups_wide)){
  contact_data_contactors[participant_age_years >= age_groups_wide[i, age_from] &
                            participant_age_years <= age_groups_wide[i, age_to],
                          participant_age_group_wide := age_groups_wide[i, name]]
  
  contact_data_contactees[contact_age >= age_groups_wide[i, age_from] & contact_age <= age_groups_wide[i, age_to],
                          contact_age_group_wide := age_groups_wide[i, name]]
}
contact_data_contactors[, participant_age_group_wide := factor(participant_age_group_wide, age_groups_wide$name)]
contact_data_contactees[, contact_age_group_wide := factor(contact_age_group_wide, age_groups_wide$name)]

household_data_members_mortality[, infant := age == 0]
household_data_members_mortality[, u5 := age < 5]

household_data_members_migration[, infant := age == 0]
household_data_members_migration[, u5 := age < 5]