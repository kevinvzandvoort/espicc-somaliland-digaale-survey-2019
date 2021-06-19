#' Set global formatting options
options(knitr.kable.NA = "")
options(scipen=999)
set.seed(10)

#' Set global plot styling options
page_outer_margin = 0.523 #inch
page_inner_margin = page_outer_margin/2 #inch
page_width = 8 #inch
page_height = 11 #inch
plot_single_col = (page_width - page_outer_margin*2 - page_inner_margin)/2 #inch
plot_double_col = page_width - page_outer_margin*2 #inch

#' Additional ggplot theme options to be used when combining plots (utilizing patchwork)
theme_multiplot = ggplot2::theme(legend.title = element_text(size=9), legend.text = element_text(size = 8),
                                 legend.box.background = element_rect(fill="#FFFFFF", colour="#000000", size = 0.2),
                                 legend.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"),
                                 legend.key.size = unit(0.3, 'cm'), axis.title = element_text(size=10),
                                 axis.text = element_text(size = 8),
                                 plot.title = element_text(size=10, face = "bold", hjust=0.62),
                                 plot.title.position = "plot", plot.tag = element_text(size=10),
                                 plot.tag.position = c(0, 0.99), strip.text = element_text(face = "plain", size=9))

#' Create output folders
for(d in c("./output", "output/tables", "output/tables/pdf", "output/tables/html", "output/figures",
           "output/figures/png", "output/figures/pdf"))
  sprintf("%s/%s", analysis_dir, d) %>% dir.create()

#' Load helper functions
source(sprintf("%s/scripts/functions.R", analysis_dir))

#' Load the cleaned and anonymized data, and assign age-groups
#' - Nb some data already have allocated age-groups, to preserve anonymity of the participants
source(sprintf("%s/scripts/load_data.R", analysis_dir))
