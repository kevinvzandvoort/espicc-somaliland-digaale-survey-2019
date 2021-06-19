#' Create section A of plot: histogram of household sizes
population_distribution_householdsize = 
  ggplot(data = household_data_svy$variables)+
  geom_bar(data = household_data_svy$variables[!is.na(household_members), .SD[, .(count = .N)],
                                               by=c("household_members")] %>%
             .[, household_members := as.numeric(household_members)] %>%
             rbind(data.table(household_members=13, count=0)) %>% .[order(household_members)],
           stat="identity", aes(x=household_members, y=count))+
  scale_y_continuous(breaks=c(0, seq(0, 100, 20)), labels=c(0, abs(seq(0, 100, 20))), limits=c(0,100))+
  scale_x_continuous(breaks = seq(1, max(household_data_svy$variables[, household_members]+1, na.rm=T), 2))+
  labs(x="Household size", y="Households", title="Household size")+
  theme_minimal()+  
  theme(panel.grid.minor=element_blank())+
  theme_multiplot

#' Create section B of plot: age- and sex-distribution of all included householdmembers
#' - Note that this is used as a proxy for the age- and sex-distribution of the population in Digaale IDP camp
population_distribution_householdmembers = ggplot()+
  geom_bar(data = household_data_members_svy$variables[, .SD[, .(total = .N)],
                                                       by=c("household_member_sex", "age_group_80")] %>%
             .[household_member_sex=="female"] %>% copy() %>% .[, household_member_sex := "Female"], stat="identity",
           aes(x=age_group_80, y=-1*total, fill=household_member_sex))+
  geom_bar(data = household_data_members_svy$variables[, .SD[, .(total = .N)],
                                                       by=c("household_member_sex", "age_group_80")] %>%
             .[household_member_sex=="male"] %>% copy() %>% .[, household_member_sex := "Male"], stat="identity",
           aes(x=age_group_80, y=total, fill=household_member_sex))+
  coord_flip()+
  scale_y_continuous(breaks=c(0, seq(-450, 450, 150)), labels=c(0, abs(seq(-450, 450, 150))), limits=c(-360,360))+
  labs(x="Age group", y="People", fill="Sex", title="Population")+
  geom_hline(yintercept=0)+
  scale_fill_manual(values=c("Female" = "#333333", "Male" = "#A7A8AA"))+
  theme_minimal()+
  theme(legend.position=c(0.85, 0.9))+
  theme_multiplot

#' Create section C of plot: age- and sex-distribution of participants included in the (contact) survey
population_distribution_participants =
  ggplot(data = contact_data_frequency_ps$variables, aes(fill=participant_sex))+
  geom_bar(data = contact_data_frequency_ps$variables[, .SD[, .(total = .N)],
                                                      by=c("participant_sex", "participant_age_group_80")] %>%
             .[participant_sex=="female"] %>% copy() %>% .[, participant_sex := "Female"],
           stat="identity", aes(x=participant_age_group_80, y=-1*total))+
  geom_bar(data = contact_data_frequency_ps$variables[, .SD[, .(total = .N)],
                                                      by=c("participant_sex", "participant_age_group_80")] %>%
             .[participant_sex=="male"] %>% copy() %>% .[, participant_sex := "Male"], stat="identity",
           aes(x=participant_age_group_80, y=total))+
  coord_flip()+
  scale_y_continuous(breaks=c(0, seq(-150, 150, 50)), labels=c(0, abs( seq(-150, 150, 50))), limits=c(-150, 150))+
  labs(x="Age group", y="People", fill="Sex", title="Participants")+
  geom_hline(yintercept=0)+
  scale_fill_manual(values=c("Female" = "#333333", "Male" = "#A7A8AA"))+
  theme_minimal()+
  theme(legend.position = "none")+
  theme_multiplot

#' Create section D of plot: age- and sex-distribution of contactees reported in the (contact) survey
population_distribution_contacts =
  ggplot(data = contact_data_contactees, aes(fill=contact_sex))+
  geom_bar(data = contact_data_contactees[, .SD[, .(total = .N)], by=c("contact_sex", "contact_age_group_80")] %>%
             .[contact_sex=="female"] %>% copy() %>% .[, contact_sex := "Female"] %>% .[order(contact_age_group_80)],
           stat="identity", aes(x=contact_age_group_80, y=-1*total))+
  geom_bar(data = contact_data_contactees[, .SD[, .(total = .N)], by=c("contact_sex", "contact_age_group_80")] %>%
             .[contact_sex=="male"] %>% copy() %>% .[, contact_sex := "Male"] %>% .[order(contact_age_group_80)],
           stat="identity", aes(x=contact_age_group_80, y=total))+
  coord_flip()+
  scale_x_discrete(limits=age_groups_80[, name])+
  scale_y_continuous(breaks=c(0, seq(-750, 750, 250)), limits=c(-700,700), labels=c(0, abs( seq(-750, 750, 250))))+
  labs(x="Age group", y="People", fill="Sex", title="Contactees")+
  geom_hline(yintercept=0)+
  scale_fill_manual(values=c("Female" = "#333333", "Male" = "#A7A8AA"))+
  theme_minimal()+
  theme(legend.position="none")+
  theme_multiplot

#' Create a plot spaning two columns and 1.6 w:h ratio
x11(width = plot_double_col, height = plot_double_col/1.6)

(figure1_demographics = (population_distribution_householdsize + population_distribution_householdmembers)/
  (population_distribution_participants + population_distribution_contacts)+
  plot_annotation(tag_levels = "A")+
  theme_minimal())

dev.off()

for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/figures/%s/figure1_demographics.%s", analysis_dir, ext, ext),
         plot = figure1_demographics, width = plot_double_col, height = plot_double_col/1.6, units = "in", dpi = 300)

rm("population_distribution_householdsize", "population_distribution_householdmembers", "population_distribution_participants", "population_distribution_contacts")

setwd(analysis_dir)
