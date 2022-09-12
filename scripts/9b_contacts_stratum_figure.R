weight_types = c("I" = "age_sex", "II" = "age_householdsize", "III" = "age", "IV" = "none")

contacts_sensitivity_data_rate_adj = contacts_sensitivity_data %>% names %>%
  lapply(function(name, data) data[[name]][["rate_adjusted"]] %>%
           reshapeCM(age_groups_60[, age_group := name]) %>% .[, type := name] %>%
           .[, type := factor(type, weight_types, names(weight_types))] %>% return,
         contacts_sensitivity_data) %>%
  rbindlist

#' Get largest eigenvalue for each matrix
contacts_sensitivity_data %>% sapply(function(x) x[["rate_adjusted"]] %>% eigen(only.values = TRUE) %>% .[["values"]] %>% max %>% Re) %>% round(3)

contacts_sensitivity_data_rate_prob = contacts_sensitivity_data %>% names %>%
  lapply(function(name, data) data[[name]][["prob"]] %>%
           reshapeCM(age_groups_60[, age_group := name]) %>% .[, type := name] %>%
           .[, type := factor(type, weight_types, names(weight_types))] %>% return,
         contacts_sensitivity_data) %>%
  rbindlist



contacts_sensitivity_data_rate_adj_diff_main = contacts_sensitivity_data %>% names %>%
  subset(!. == "age_sex") %>%
  lapply(function(name, data) data[[name]][["rate_adjusted"]] %>%
           (function(x) x/contacts_sensitivity_data$age_sex$rate_adjusted) %>%
           reshapeCM(age_groups_60[, age_group := name]) %>% .[, type := name] %>%
           .[, type := factor(type, weight_types, names(weight_types))] %>% return,
         contacts_sensitivity_data) %>%
  rbindlist

#' Compare contact rates per weight-type
(figure_sensitivity_weights = contacts_sensitivity_data_rate_adj %>%
  ggplot(aes(
    y=factor(contact_age_group, age_groups_60[, name]),
    x=factor(part_age_group, age_groups_60[, name]), fill=value))+
  facet_wrap("type")+
  geom_tile()+
  geom_text(aes(label=round(value, 1)), color="#FFFFFF", fontface="bold")+
  labs(y="Age contactee", x="Age contactor", fill="Contact rate", title="Contact rate per day")+
  scale_fill_viridis(option = "D", values = scales::rescale(quantile(contacts_all_weighted[, value])),
                     labels = function(x) sprintf("%.1f", x))+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.3), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF")))

#' Compare per-capita contact rates per weight-type
(figure_sensitivity_weights = contacts_sensitivity_data_rate_prob %>%
    ggplot(aes(
      y=factor(contact_age_group, age_groups_60[, name]),
      x=factor(part_age_group, age_groups_60[, name]), fill=value * 1000))+
    facet_wrap("type")+
    geom_tile()+
    geom_text(aes(label=round(value * 1000, 1)), color="#FFFFFF", fontface="bold")+
    labs(y="Age contactee", x="Age contactor", fill="Contact rate", title="Contact rate per 1000 per day")+
    scale_fill_viridis(option = "C", values = scales::rescale(quantile(contacts_all_weighted[, value])),
                       labels = function(x) sprintf("%.1f", x))+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.3), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF")))

x11(height = plot_single_col*1, width = plot_double_col)
(figure_sensitivity_weights_reldiff = contacts_sensitivity_data_rate_adj_diff_main %>%
    ggplot(aes(
      y=factor(contact_age_group, age_groups_60[, name]),
      x=factor(part_age_group, age_groups_60[, name]), fill=value))+
    facet_wrap("type")+
    geom_tile()+
    geom_label(aes(label = sprintf("%.2f", value)), colour = "#000000", fontface = "bold", fill="#FFFFFF",
               size=3, label.padding = unit(0.10, "lines"))+
    labs(y="Age contactee", x="Age contactor", fill="Relative difference", title="Relative difference in daily contact rate, compared to I")+
    scale_fill_viridis(option = "H", trans = "log", values = scales::rescale(quantile(contacts_sensitivity_data_rate_adj_diff_main$value %>% log)),
                       labels = function(x) sprintf("%.1f", x))+
    theme_multiplot+
    theme(legend.position=c(0.5, -0.35), axis.text.x = element_text(angle = 45, vjust=1, hjust = 1),
          legend.title = element_blank(), legend.direction = "horizontal",
          legend.key.width = unit(1.2, "cm"), legend.key.height = unit(0.25, "cm"),
          legend.box.background = element_blank(), plot.margin = unit(c(0, 0, 0.8, 0), "cm"),
          panel.background = element_rect(fill = "#FFFFFF")))

layout <- c(
  area(t = 1, b = 100, l = 1, r = 3),
  area(t = 101, b = 150, l = 1, r = 4))

x11(height = plot_single_col*2.6, width = plot_double_col)
(contact_weights_sensitivity_figure = (figure_sensitivity_weights&theme(legend.position = "right",
                                  legend.direction = "vertical",
                                  legend.key.width = unit(0.25, "cm"),
                                  legend.key.height = unit(1, "cm")))/figure_sensitivity_weights_reldiff+plot_layout(design = layout)+plot_annotation(tag_levels = "A")&
  theme(plot.tag = element_text(size=10), plot.title.position = "plot", plot.tag.position = c(0,0.99)))

for(output_dir in paste0("ps_", c("age_householdsize", "age_sex", "age", "none")))
  for(ext in c("png", "pdf"))
    ggsave(sprintf("%s/output/%s/figures/%s/figure_sD1_contacts_sensitivity.%s", analysis_dir, output_dir, ext, ext),
           plot = contact_weights_sensitivity_figure, width = plot_double_col, height = plot_single_col*2.6, units = "in", dpi = 300)

dev.off()

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
