#' Create section A of figure 2: frequency of contacts by contact type
figure2.1 = svyMean2(
  ~contact_frequency_daily_or_almost_daily_total+contact_frequency_daily_or_almost_daily_physical+
    contact_frequency_at_least_once_per_week_total+contact_frequency_at_least_once_per_week_physical+
    contact_frequency_less_than_once_per_week_total+contact_frequency_less_than_once_per_week_physical,
  contact_data_proportion_ps, na.rm = TRUE) %>%
  .[, type := factor(grepl("physical", variable), c(FALSE, TRUE), c("All", "Physical"))] %>%
  .[, variable := gsub("_physical", "", gsub("_total", "", gsub("contact_frequency_","",variable)))] %>%
  .[, variable := factor(variable, c("daily_or_almost_daily", "at_least_once_per_week","less_than_once_per_week"),
                         c("Daily/almost daily", "≥ once per week", "< once per week"))] %>%
  ggplot(aes(x=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=mean, colour=type))+
  geom_segment(aes(xend=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=ci95_low, yend=ci95_high), size=1.25)+
  geom_point(size=1.5)+
  scale_x_continuous(breaks=1:3, labels=c("Daily/almost daily", "≥ once per week", "< once per week"))+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,NA))+
  scale_color_grey()+
  coord_flip()+
  labs(title="Contact frequency", x="", y="Contacts", colour="Contact type")+
  guides(colour = guide_legend(override.aes = list(shape=15, size = 3, linetype=0)))+
  theme(legend.position=c(0.89, 1.1), panel.grid.minor.y = element_blank())+
  theme_multiplot

#' Create section B of figure 2: duration of contacts by contact type
figure2.2 = svyMean2(~`contact_duration_<15min_total`+`contact_duration_<15min_physical`+
                       `contact_duration_15min-1h_total`+`contact_duration_15min-1h_physical`+
                       `contact_duration_1h-2h_total`+`contact_duration_1h-2h_physical`+
                       `contact_duration_2h-4h_total`+`contact_duration_2h-4h_physical`+
                       `contact_duration_>4h_total`+`contact_duration_>4h_physical`,
                      contact_data_proportion_ps, na.rm=T) %>%
  .[, type := factor(grepl("physical", variable), c(FALSE, TRUE), c("All", "Physical"))] %>%
  .[, variable := gsub("`", "", gsub("_physical", "", gsub("_total", "", gsub("contact_duration_","",variable))))] %>%
  .[, variable := factor(variable, c("<15min", "15min-1h","1h-2h","2h-4h",">4h"),
                         c("<15m", "15m-1h","1h-2h","2h-4h",">4h"))] %>%
  ggplot(aes(x=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=mean, colour=type))+
  geom_segment(aes(xend=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=ci95_low, yend=ci95_high), size=1.25)+
  geom_point(size=1.5)+
  scale_x_continuous(breaks=1:5, labels=c("<15m", "15m-1h","1h-2h","2h-4h",">4h"))+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,NA))+
  scale_color_grey()+
  coord_flip()+
  labs(title="Contact duration", x="", y="Contacts", colour="Contact type")+
  guides(colour = guide_legend(override.aes = list(shape=15, size = 3, linetype=0)))+
  theme(legend.position="none", panel.grid.minor.y = element_blank())+theme_multiplot

#' Create section C of figure 2: contactor-contactee relationship by contact type
figure2.3 = svyMean2(
  ~`contact_relationship_other_total`+`contact_relationship_other_physical`+
    `contact_relationship_coworker_or_schoolmate_total`+`contact_relationship_coworker_or_schoolmate_physical`+
    `contact_relationship_friend_total`+`contact_relationship_friend_physical`+
    `contact_relationship_other_relative_total`+`contact_relationship_other_relative_physical`+
    `contact_relationship_household_member_total`+`contact_relationship_household_member_physical`,
  contact_data_proportion_ps, na.rm=T) %>%
  .[, type := factor(grepl("physical", variable), c(FALSE, TRUE), c("All", "Physical"))] %>%
  .[, variable := gsub("`", "", gsub("_physical", "",
                                     gsub("_total", "", gsub("contact_relationship_","",variable))))] %>%
  .[, variable := factor(variable, c("household_member", "other_relative", "friend", "coworker_or_schoolmate", "other"),
                         c("Household member", "Other relative", "Friend", "Coworker/schoolmate", "Other"))] %>%
  ggplot(aes(x=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=mean, colour=type))+
  geom_segment(aes(xend=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=ci95_low, yend=ci95_high), size=1.25)+
  geom_point(size=1.5)+
  scale_x_continuous(breaks=1:5, labels=c("Household member", "Other relative", "Friend", "Coworker/schoolmate",
                                          "Other"))+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,NA))+
  scale_color_grey()+
  coord_flip()+
  labs(title="Contact relationship", x="", y="Contacts", colour="Contact type")+
  theme(legend.position="none", panel.grid.minor.y = element_blank())+theme_multiplot

#' Create section D of figure 2: contact setting by contact type
figure2.4 = svyMean2(
  ~`home_only_contacts`+`home_only_contacts_physical`+`other_house_contacts`+`other_house_contacts_physical`+
    `school_contacts`+`school_contacts_physical`+`work_contacts`+`work_contacts_physical`+
    `other_contacts`+`other_contacts_physical`,
  contact_data_proportion_ps, na.rm=T) %>%
  .[, type := factor(grepl("physical", option), c(FALSE, TRUE), c("All", "Physical"))] %>%
  .[, variable := gsub("`", "", gsub("_physical", "",
                                     gsub("_total", "", gsub("contact_relationship_","",variable))))] %>%
  .[, variable := factor(variable, c("home_only_contacts", "other_house_contacts", "school_contacts", "work_contacts",
                                     "other_contacts"), c("Home", "Another house", "School", "Work", "Other"))] %>%
    ggplot(aes(x=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=mean, colour=type))+
    geom_segment(aes(xend=as.numeric(variable)+(as.numeric(type) - 1.5)*0.1, y=ci95_low, yend=ci95_high), size=1.25)+
    geom_point(size=1.5)+
    scale_x_continuous(breaks=1:5, labels=c("Home", "Another house", "School", "Work", "Other"))+
    theme_minimal()+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,NA))+
  scale_color_grey()+
  coord_flip()+
  labs(title="Contact setting", x="", y="Contacts", colour="Contact type")+
  guides(colour = guide_legend(override.aes = list(shape=15, size = 3, linetype=0)))+
  theme(legend.position="none", panel.grid.minor.y = element_blank())+theme_multiplot

#' Create contact matrices (E and F in figure 2)

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

#' Calculate the contact matrix (poststratified for age-sex and weighted for weekend and non-weekend days)
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
contacts_all_weighted = reshapeCM(contact_matrices_empirical_all_weighted$rate_adjusted,
                                  age_groups_60[, age_group := name])

#' Create section E of figure 2: contact matrix
figure2.5_weighted = contacts_all_weighted %>%
  ggplot(aes(
    y=factor(contact_age_group, age_groups_60[, name]), x=factor(part_age_group, age_groups_60[, name]), fill=value))+
  geom_tile()+
  labs(y="Age contactee", x="Age contactor", fill="Contact rate", title="Mean contacts per day")+
  #scale_fill_gradientn(colors=c("#0D5257", "#00BF6F", "#07e88a", "#8af5c8", "#ffe09a", "#FFB81C", "#FE5000"),
  #                     na.value = "#A7A8AA", values = scales::rescale(quantile(contacts_all_weighted[, value],
  #                                                                            c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
  scale_fill_viridis(option = "D", values = scales::rescale(quantile(contacts_all_weighted[, value])),
                     labels = function(x) sprintf("%.1f", x))+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.3), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF"))

#' Create section F of figure 2: per-capita contact matrix
contacts_all_per_capita_weighted = reshapeCM(contact_matrices_empirical_all_weighted$prob, age_groups_60)
figure2.6_weighted = contacts_all_per_capita_weighted %>%
  ggplot(aes(y=factor(contact_age_group, age_groups_60[, name]), x=factor(part_age_group, age_groups_60[, name]),
             fill=value * 1000))+
  geom_tile()+
  labs(y="Age contactee",x="Age contactor",fill="Per capita\ncontact rate",
       title="Contact rate per 1000 per day")+
  #scale_fill_gradientn(colors=c("#32006E", "#1E22AA", "#858dd5", "#3EBFAC", "#EFE048", "#F9BE00", "#ff5000"),
  #                     na.value = "#A7A8AA", values = scales::rescale(
  #                       quantile(contacts_all_per_capita_weighted[, value], c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1))))+
  scale_fill_viridis(option = "C", values = scales::rescale(quantile(contacts_all_weighted[, value])),
                     labels = function(x) sprintf("%.1f", x))+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.3), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF"))

x11(width = plot_double_col, height = plot_double_col*0.9)

#' Allocate whitespace between two columns in layout for the combined plot
rel_width=20
E_AC_ratio = 2.5 #ratio of contact matrix and AC/BD area of plot
layout <- c(
  area(t = 1, b = 2, l = 1, r = 1+rel_width), area(t = 1, b = 2, l = 3+rel_width, r = 3+rel_width*2),
  area(t = 3, b = 4, l = 1, r = 1+rel_width), area(t = 3, b = 4, l = 3+rel_width, r = 3+rel_width*2),
  area(t = 5, b = 5+2*E_AC_ratio, l = 1, r = 1+rel_width), area(t = 5, b = 5+2*E_AC_ratio, l = 3+rel_width, r = 3+rel_width*2))

(figure2_contacts = wrap_elements(full = figure2.1) +wrap_elements(full = figure2.2) +wrap_elements(full = figure2.3)+
    wrap_elements(full = figure2.4) +wrap_elements(full = figure2.5_weighted) +wrap_elements(full = figure2.6_weighted)+
    plot_layout(design = layout)+plot_annotation(tag_levels = "A")&
    theme(plot.tag = element_text(size=10), plot.title.position = "plot", plot.tag.position = c(0,0.99)))

dev.off()

for(ext in c("png", "pdf"))
  ggsave(sprintf("%s/output/%s/figures/%s/figure2_contacts.%s", analysis_dir, OUTPUT_DIR, ext, ext),
         plot = figure2_contacts, width = plot_double_col, height = plot_double_col*0.9, units = "in", dpi = 300)

#x11(width = (34.8-1)/2.54, height = (34.8-1)/2.54 * (8/16))
#(plot_poster = wrap_elements(full = figure2.5_weighted+theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
#                                              axis.text = element_text(size = 14),
#                                              legend.text = element_text(size=14),
#                                              axis.title = element_text(size = 18),
#                                              text = element_text(size = 14),
#                                              strip.text = element_text(size = 18),
#                                              title = element_text(size = 18),
#                                              plot.title = element_text(face = "bold", size=18),
#                                              legend.title = element_text(face = "bold")))+
#  wrap_elements(full = figure2.6_weighted+theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
#                                                axis.text = element_text(size = 14),
#                                                legend.text = element_text(size=14),
#                                                axis.title = element_text(size = 18),
#                                                text = element_text(size = 14),
#                                                strip.text = element_text(size = 18),
#                                                title = element_text(size = 18),
#                                                plot.title = element_text(face = "bold", size=18),
#                                                legend.title = element_text(face = "bold")))+
#  plot_annotation(tag_levels = "A")&
#  theme(
#        plot.margin = unit(c(0.1, 0, -0.9, 0), "cm"), plot.tag = element_text(size=18, face="bold")))
#ggsave(filename = "~/workspace/espicc_poster.jpg", width = (34.8-1)/2.54, height = (34.8-1)/2.54 * (8/16), dpi=300)

#' Calculate total number of expected population-wide contacts, without adjusting for reciprocity
#' - used in 8_figures_supp.R
total_population_contacts_unadjusted = contact_matrices_empirical_all_weighted$rate_unadjusted %*% diag(n_population_size_fpc[, total])
colnames(total_population_contacts_unadjusted) = age_groups_60[, age_group]

rm("contact_matrices_empirical_all_weighted", "contacts_all_per_capita_weighted", "E_AC_ratio",
   "figure2.1", "figure2.2", "figure2.3", "figure2.4", "figure2.5_weighted", "figure2.6_weighted", "layout",
   "rel_width")

setwd(analysis_dir)
