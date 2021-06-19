#' Create all contact related graphs presented in the supplemental material

#' Create contact matrix with all intra-household contacts
contact_matrices_empirical_all_weighted_within_hh =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  merge(participant_data_design_ps$variables[, c("contactor_id", "participant_age_group_80")], by.x="id",
        by.y="contactor_id") %>%
  .[!is.na(contact_age_group_80) & contact_relationship == "Household member",
    c("participant_sex", "contact_sex", "participant_age_group_80", "contact_age_group_80", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_80, participant_age_group_80)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[, total], n_population_size_fpc[, total])

#' Create contact matrix with all extra-household contacts
contact_matrices_empirical_all_weighted_outside_hh =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  merge(participant_data_design_ps$variables[, c("contactor_id", "participant_age_group_80")], by.x="id",
        by.y="contactor_id") %>%
  .[!is.na(contact_age_group_80) & contact_relationship != "Household member",
    c("participant_sex", "contact_sex", "participant_age_group_80", "contact_age_group_80", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_80, participant_age_group_80)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[, total], n_population_size_fpc[, total])

figure_data_contacts_within_hh = contact_matrices_empirical_all_weighted_within_hh$rate_adjusted %>%
  reshapeCM(age_groups_80[, age_group := name])

figure_data_contacts_outside_hh = contact_matrices_empirical_all_weighted_outside_hh$rate_adjusted %>%
  reshapeCM(age_groups_80[, age_group := name])

figure_data_contact_rate = rbind(
  figure_data_contacts_within_hh[, type := "Intra-household"],
  figure_data_contacts_outside_hh[, type := "Extra-household"])

#' Create section A for supplemental figure C2
figure_contact_rate = figure_data_contact_rate %>%
  ggplot(aes(y=factor(contact_age_group, age_groups_80[, name]), x=factor(part_age_group, age_groups_80[, name]),
             fill=value))+
  facet_grid(.~factor(type, c("Intra-household", "Extra-household")))+
  geom_tile()+
  labs(y="Age contactee", x="Age contactor", fill="Contact rate", title="Mean contacts per day")+
  scale_fill_gradientn(colors=c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000"),
                       na.value = "#A7A8AA", values =
                         scales::rescale(quantile(figure_data_contact_rate[, value],
                                                  c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
    theme_minimal()+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.38), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          plot.title = element_text(hjust = 0.54), legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0.1, 0, 0.9, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF"))

figure_data_contacts_within_hh_per_capita = contact_matrices_empirical_all_weighted_within_hh$prob %>%
  reshapeCM(age_groups_80[, age_group := name])

figure_data_contacts_outside_hh_per_capita = contact_matrices_empirical_all_weighted_outside_hh$prob %>%
  reshapeCM(age_groups_80[, age_group := name])

figure_data_contact_rate_per_capita = rbind(
  figure_data_contacts_within_hh_per_capita[, type := "Intra-household"],
  figure_data_contacts_outside_hh_per_capita[, type := "Extra-household"])

#' Create section B for supplemental figure C2
figure_contact_rate_per_capita = figure_data_contact_rate_per_capita %>%
  ggplot(aes(y=factor(contact_age_group, age_groups_80[, name]), x=factor(part_age_group, age_groups_80[, name]),
             fill=value*1000))+
  facet_grid(.~factor(type, c("Intra-household", "Extra-household")))+
  geom_tile()+
  labs(y="Age contactee", x="Age contactor", fill="Per capita\nontact rate", title="Contact rate per 1000 per day")+
  scale_fill_gradientn(colors=c("#32006E", "#1E22AA", "#858dd5", "#3EBFAC", "#EFE048", "#F9BE00", "#ff5000"),
                       na.value = "#A7A8AA", values =
                         scales::rescale(quantile(figure_data_contact_rate_per_capita[, value],
                                                  c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
    theme_minimal()+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.38), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          plot.title = element_text(hjust = 0.54), legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0.1, 0, 0.9, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF"))

#' Rotate 90 degrees when inserting in document so height = double_col width
x11(height = plot_double_col, width = plot_double_col*0.8)
(figure_sC2_intra_extra_household_contacts =
  figure_contact_rate/figure_contact_rate_per_capita+plot_annotation(tag_levels = "A")&
  theme(plot.tag = element_text(size=10), plot.title.position = "plot", plot.tag.position = c(0,0.99)))
dev.off()

rm("figure_contact_rate", "figure_contact_rate_per_capita", "figure_data_contacts_within_hh_per_capita",
     "figure_data_contacts_outside_hh_per_capita", "figure_data_contact_rate")
    
for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/figures/%s/figure_sC2_intra_extra_household_contacts.%s", analysis_dir, ext, ext),
         plot = figure_sC2_intra_extra_household_contacts, width = plot_double_col*0.8, height = plot_double_col,
         units = "in", dpi = 300)

#' Supplemental figure C3 - contact rates by age and gender
#' - ensures that total contacts made by males of age i with females of age j are symmetrical
#'   with total contacts made by females of age j with males of age i
n_contacts_sample_fpc_weighted =
  contact_data_frequency_ps$variables[, c("id", "participant_age_group_wide", "participant_sex")] %>%
  cbind(weights(contact_data_frequency_ps)) %>% .[, weight := V2] %>% .[, -"V2"] %>%
  .[, .SD[, .(total = sum(weight))], by=c("participant_sex", "participant_age_group_wide")] %>%
  .[order(participant_sex, participant_age_group_wide)]

#' Need to use the wide age group due to sparsity of data in individual strata
age_groups_80[, age_group_wide := NA_character_]
for(i in 1:nrow(age_groups_wide)){
  age_groups_80[(age_from <= age_groups_wide[i, age_from] | age_to <= age_groups_wide[i, age_to]) &
                  is.na(age_group_wide), age_group_wide := age_groups_wide[i, name]]
}

#' Get population size by wide age group
n_population_size_fpc = household_data_members_svy$variables %>%
  merge(age_groups_80[, c("name", "age_group_wide")], by.x="age_group_80", by.y="name") %>%
  .[, .(total = .N), by=c("household_member_sex", "age_group_wide")] %>% setorder(household_member_sex, age_group_wide)

contact_matrices_male_male =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  .[participant_sex == "male" & contact_sex == "male",
    c("participant_sex", "contact_sex", "participant_age_group_wide", "contact_age_group_wide", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_wide, participant_age_group_wide)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[participant_sex == "male", total],
                         n_population_size_fpc[household_member_sex == "male", total])

contact_matrices_female_female =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  .[participant_sex == "female" & contact_sex == "female",
    c("participant_sex", "contact_sex", "participant_age_group_wide", "contact_age_group_wide", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_wide, participant_age_group_wide)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[participant_sex == "female", total],
                         n_population_size_fpc[household_member_sex == "female", total])

contact_matrices_female_male =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  .[participant_sex == "female" & contact_sex == "male",
    c("participant_sex", "contact_sex", "participant_age_group_wide", "contact_age_group_wide", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_wide, participant_age_group_wide)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[participant_sex == "female", total],
                         n_population_size_fpc[household_member_sex == "male", total])

contact_matrices_male_female =
  contact_data_full %>%
  merge(contact_data_frequency_ps$variables[, "id"] %>%
          cbind(weights(contact_data_frequency_ps)) %>%
          .[, weight := V2] %>% .[, -"V2"], by="id") %>%
  .[participant_sex == "male" & contact_sex == "female",
    c("participant_sex", "contact_sex", "participant_age_group_wide", "contact_age_group_wide", "weight")] %>%
  .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_wide, participant_age_group_wide)] %>%
  dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
  .[, -"contact_age_group"] %>% as.matrix() %>%
  constructContactMatrix(n_contacts_sample_fpc_weighted[participant_sex == "male", total],
                         n_population_size_fpc[household_member_sex == "female", total])

popsize_female = n_population_size_fpc[household_member_sex == "female", total]
popsize_male = n_population_size_fpc[household_member_sex == "male", total]

#' Need to manually need to make individual matrices symmetrical (averaging over both as in the standard approach)
for(i in 1:nrow(contact_matrices_male_female$rate_adjusted)){
  for(j in 1:ncol(contact_matrices_male_female$rate_adjusted)){
    contact_matrices_male_female$rate_adjusted[i, j] =
      0.5 * (contact_matrices_male_female$rate_unadjusted[i,j] +
               contact_matrices_female_male$rate_unadjusted[j,i]*(popsize_female[i]/popsize_male[j]))
    contact_matrices_female_male$rate_adjusted[i, j] =
      0.5 * (contact_matrices_female_male$rate_unadjusted[i,j] +
               contact_matrices_male_female$rate_unadjusted[j,i]*(popsize_male[i]/popsize_female[j]))
  }
}

age_groups_wide[, age_group := .I]
contacts_male_male = reshapeCM(contact_matrices_male_male$rate_adjusted, age_groups_wide)
contacts_male_male[, participant := "male"][, contact := "male"]

contacts_female_female = reshapeCM(contact_matrices_female_female$rate_adjusted, age_groups_wide)
contacts_female_female[, participant := "female"][, contact := "female"]

contacts_female_male = reshapeCM(contact_matrices_female_male$rate_adjusted, age_groups_wide)
contacts_female_male[, participant := "female"][, contact := "male"]

contacts_male_female = reshapeCM(contact_matrices_male_female$rate_adjusted, age_groups_wide)
contacts_male_female[, participant := "male"][, contact := "female"]

contacts_sex_compare <- rbind(contacts_male_male, contacts_female_female, contacts_male_female, contacts_female_male)

x11(height = plot_single_col*1.1, width = plot_single_col)

figure_sC3_contact_bysex = contacts_sex_compare %>%
  copy() %>% .[, c("contactor", "contactee") := .(participant, contact)] %>%
  ggplot(aes(y=factor(contact_age_group, age_groups_wide[, name]),
             x=factor(part_age_group, age_groups_wide[, name]), fill=value))+
  geom_tile()+
  labs(y="Age contactee", x="Age contactor", fill="Contact rate")+
  facet_grid(contactee~contactor, labeller = label_both)+
  scale_fill_gradientn(colors=c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000"),
                       na.value = "#A7A8AA", values =
                         scales::rescale(quantile(contacts_sex_compare[, value],
                                                  c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
    theme_minimal()+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.29), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          plot.title = element_text(hjust = 0.54), legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0.1, 0, 0.9, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF"))

dev.off()

for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/figures/%s/figure_sC3_contact_bysex.%s", analysis_dir, ext, ext),
         plot = figure_sC3_contact_bysex, width = plot_single_col, height = plot_single_col*1.1, units = "in",
         dpi = 300)

#' Compare reported household age pairs with expected
#' - A difference could highlight underreporting in household contacts, or <1 contact per day between members of a
#'   household

household_data_members = household_data_members_svy$variables
population_house_total = totalAgePairs(household_data_members[, c("age_group") := .(age_group_80)])
n_population_size_fpc = household_data_members[, .SD[, .(total = .N * fpc_inflate_N_factor)], by="age_group_80"] %>%
  .[order(age_group_80)]

#' note that the probability is the probability in the entire population
#' - in any one household, probability of contact == 1
contact_matrices_household_agepairs = constructContactMatrix(population_house_total,
  household_data_members[, .N, by="age_group"][order(age_group), N], n_population_size_fpc[, total])

figure_data_contacts_within_hh_agepairs = contact_matrices_household_agepairs$rate_adjusted %>%
  reshapeCM(age_groups_80[, age_group := name])

contacts_household_compare = 
  rbind(figure_data_contacts_within_hh[, type := "Reported"],
        figure_data_contacts_within_hh_agepairs[, type := "Household member pairs (expected)"])

#' Use single_col height and rotate in manuscript
x11(width = plot_double_col*0.75, height = plot_single_col)

figure_sC4_household_contacts_expected_reported =
  ggplot(contacts_household_compare, aes(y=factor(contact_age_group, age_groups_80[, name]),
                                         x=factor(part_age_group, age_groups_80[, name]), fill=value))+
  geom_tile()+
  labs(y="Age contactee", x="Age contactor", fill="Contact rate")+
  facet_wrap("type", ncol=3)+
  scale_fill_gradientn(colors=c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000"),
                       na.value = "#A7A8AA", values =
                         scales::rescale(quantile(contacts_household_compare[, value],
                                                  c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
    theme_minimal()+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.33), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          plot.title = element_text(hjust = 0.54), legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0.1, 0, 0.9, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF"))

dev.off()

for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/figures/%s/figure_sC4_household_contacts_expected_reported.%s", analysis_dir, ext,
                 ext), plot = figure_sC4_household_contacts_expected_reported, width = plot_double_col*0.75,
         height = plot_single_col, units = "in", dpi = 300)

#' The maximum eigenvalue is 35% lower for all household contacts in the empirical data compared to those based on all
#'  household age pairs, but the overall patterns are similar
(1 - (contact_matrices_empirical_all_weighted_within_hh$rate_adjusted %>% eigen %>% .[["values"]] %>% max)/
  (contact_matrices_household_agepairs$rate_adjusted %>% eigen %>% .[["values"]] %>% max)) * 100

#' Bootstrapped contact matrices
bootstrapped_matrices = list()
nboots = 10000

for(b in 1:nboots){
  if(b %% 10 == 0)
    message(sprintf("%s/%s", b, nboots))
  
  contact_data_bootstrap = contact_data[participant_consent == "yes"] %>% .[sample(1:.N, .N, TRUE)]
  
  #' Make sure there is at least one participant in each age group
  while(contact_data_bootstrap[, .N, by="participant_age_group_80"] %>%
        .[order(participant_age_group_80)] %>%
        merge(age_groups_80[, "name"], by.x="participant_age_group_80", by.y="name", all.y=TRUE) %>%
        .[, sum(is.na(N))] != 0){
    contact_data_bootstrap = contact_data[participant_consent == "yes"] %>% .[sample(1:.N, .N, TRUE)]  
  }
  
  contact_data_bootstrap[, bootstrap_id := .I]
  contact_data_bootstrap_ps = svydesign(ids=~1, probs=NULL, strata=~stype, fpc=~fpc, weights=~pw_weekday,
                                            data=contact_data_bootstrap) %>%
    postStratify(~stype, poststratification_strata)
  
  #' Trim weights so no individual has too high influence
  wlow = quantile(weights(contact_data_bootstrap_ps), 0.05)
  whigh = quantile(weights(contact_data_bootstrap_ps), 0.95)
  contact_data_bootstrap_ps = contact_data_bootstrap_ps %>%
    trimWeights(whigh, wlow)
  
  n_contacts_bootstrap_weighted = contact_data_bootstrap_ps$variables[, c("id", "participant_age_group_80")] %>%
    cbind(weights(contact_data_bootstrap_ps)) %>%
    .[, weight := V2] %>% .[, -"V2"] %>%
    .[, .SD[, .(total = sum(weight))], by="participant_age_group_80"] %>%
    .[order(participant_age_group_80)]
  
  contact_data_full_bootstrap = contact_data_bootstrap %>%
    merge(contact_data_contactees, by.x="id", by.y="contactor_id", all.x=TRUE)
  
  contact_matrices_bootstrap =
    contact_data_full_bootstrap %>%
    merge(contact_data_bootstrap_ps$variables[, "id"] %>%
            cbind(weights(contact_data_bootstrap_ps)) %>%
            .[, weight := V2] %>% .[, -"V2"] %>% unique, by="id", all.x=TRUE) %>%
    .[!is.na(contact_age_group_80), c("participant_sex", "contact_sex", "participant_age_group_80",
                                      "contact_age_group_80", "weight")] %>%
    .[, c("contact_age_group", "participant_age_group") := .(contact_age_group_80, participant_age_group_80)] %>%
    dcast(contact_age_group ~ participant_age_group, value.var = "weight", fun.aggregate = sum) %>%
    .[, -"contact_age_group"] %>% as.matrix() %>%
    constructContactMatrix(n_contacts_bootstrap_weighted[, total], n_population_size_fpc[, total])
  
  bootstrapped_matrices[[b]] = contact_matrices_bootstrap$rate_adjusted
}

boots_eigenvalues = bootstrapped_matrices %>%
  sapply(function(x) x %>% eigen(only.values = TRUE) %>% .[["values"]] %>% max)

#' Select minimum position from a vector of numbers
minPos = function(x){
  which(x == min(x))[1]
}
boot_mean = minPos(abs(boots_eigenvalues - mean(boots_eigenvalues)))
boot_low = minPos(abs(boots_eigenvalues - quantile(boots_eigenvalues, 0.025)))
boot_high = minPos(abs(boots_eigenvalues - quantile(boots_eigenvalues, 0.975)))

boot_matrices_data = list(
  reshapeCM(bootstrapped_matrices[[boot_mean]], age_groups_80[, age_group := name]) %>%
    .[, type := "mean"],
  reshapeCM(bootstrapped_matrices[[boot_low]], age_groups_80[, age_group := name]) %>%
    .[, type := "low"],
  reshapeCM(bootstrapped_matrices[[boot_high]], age_groups_80[, age_group := name]) %>%
    .[, type := "high"]) %>% rbindlist

x11(width = plot_double_col, height = plot_single_col)

(figure_sC1_bootstrapped_matrices = boot_matrices_data %>%
    ggplot(aes(y=factor(contact_age_group, age_groups_80[, name]),
               x=factor(part_age_group, age_groups_80[, name]), fill=value))+
    geom_tile()+
    labs(y="Age contactee", x="Age contactor")+
    facet_wrap(.~factor(type, c("mean", "low", "high"), c("Mean", "Lower", "Higher")), ncol=3)+
    scale_fill_gradientn(colors=c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000"),
                         na.value = "#A7A8AA", values =
                           scales::rescale(quantile(contacts_household_compare[, value],
                                                    c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
    theme_minimal()+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.33), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          plot.title = element_text(hjust = 0.54), legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(2.5, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0.1, 0, 0.9, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF", colour="#FFFFFF")))

dev.off()

for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/figures/%s/figure_sC1_bootstrapped_matrices.%s", analysis_dir, ext, ext),
         plot = figure_sC1_bootstrapped_matrices, width = plot_double_col, height = plot_single_col, units = "in",
         dpi = 300)

rm("b", "boot_high", "boot_low", "boot_matrices_data", "boot_mean", "boots_eigenvalues", "bootstrapped_matrices",
   "contact_data_bootstrap", "contact_data_bootstrap_ps", "contact_data_full_bootstrap", "contact_matrices_bootstrap",
   "contact_matrices_empirical_all_weighted_outside_hh", "contact_matrices_empirical_all_weighted_within_hh",
   "contact_matrices_female_female", "contact_matrices_female_male", "contacts_household_compare",
   "contacts_male_female", "contacts_male_male", "contacts_sex_compare", "figure_data_contact_rate_per_capita",
   "figure_data_contacts_outside_hh", "figure_data_contacts_within_hh", "figure_data_contacts_within_hh_agepairs",
   "household_data_members", "j", "n_contacts_bootstrap_weighted", "nboots", "popsize_female", "popsize_male",
   "population_house_total")

setwd(analysis_dir)
