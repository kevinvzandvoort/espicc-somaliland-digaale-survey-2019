if(!CONTACT_DATA_ONLY){
  #' Create table 2: the reported frequency of travel outside of Digaale IDP camp
  table2_travel = svyMean2(~travel_0_5km+travel_5_10km+travel_10km_gt, contact_data_frequency_ps, na.rm=T, digits=1,
                           multiply = 100) %>% .[!option %in% c("don’t know", "refuse to answer")]
  
  table2_travel[, ci95_low := pmax(0, ci95_low)]
  table2_travel[, mean_label := sprintf("%s%%", mean)]
  table2_travel[, confint := sprintf("%s - %s", ci95_low, ci95_high)]
  table2_travel[, variable := factor(variable, c("travel_0_5km", "travel_5_10km", "travel_10km_gt"),
                                     c("< 5km", "5 - 10km", "> 10km"))]
  
  table2_travel = table2_travel %>%
    dcast(variable~option, value.var=c("mean_label", "confint")) %>%
    .[, c("variable", sapply(c("Most days of the week", "At least once per week", "At least once per month",
                               "Less than once per month", "Never"),
                             function(x) paste0(c("mean_label_", "confint_"), x))), with=F]
  
  if(make_table) table2_travel %>%
    kblOut(booktabs=T, align=c("l|","r","l|","r", "l|","r", "l|","r", "l|","r", "l"), linesep = "", col.names=NULL,
           other_functions = list(
             function(x) add_header_above(x, c(`Travel distance` = 1, `Most days of the week` = 2,
                                               `At least once per week` = 2, `At least once per month` = 2,
                                               `Less than once per month` = 2, `Never` = 2))),
           out_name = "table2_travel")
  
  #' Create table 3: mean number of reported daily contacts by age, contact type and contact setting
  table3_contacts = svyMean2(~contacts_total+home_contacts+school_or_work_contacts+other_contacts+contacts_other_median+
                               physical_contacts, contact_data_frequency_ps, na.rm=T, by="participant_age_group_sample",
                             digits=1)
  table3_contacts[, ci95_low := pmax(0, ci95_low)]
  table3_contacts[, confint := sprintf("%s - %s", ci95_low, ci95_high)]
  table3_contacts[, variable := factor(variable, c("contacts_total", "home_contacts", "school_or_work_contacts",
                                                   "other_contacts", "physical_contacts", "contacts_other_median"),
                                       c("Total", "Home or another house", "School or work", "Other", "Physical",
                                         "Indirect"))]
  
  table3_contacts = table3_contacts %>%
    dcast(participant_age_group_sample~variable, value.var=c("mean", "confint")) %>%
    .[, c("participant_age_group_sample",
          sapply(c("Total", "Physical", "Indirect", "Home or another house", "School or work", "Other"),
                 function(x) paste0(c("mean_", "confint_"), x))), with=F]
  
  #' Write the table to the output folder
  if(make_table) table3_contacts %>%
    kblOut(booktabs=T, formats=c(pdf="latex"),
           align=c("l|","r","l|","r", "l|","r", "l|","r", "l|","r", "l|","r", "l|"), linesep = "", col.names=NULL,
           other_functions =
             list(function(x) add_header_above(x, c(`Age group` = 1, `Total (Direct)` = 2,`Physical (Direct)` = 2,
                                                    `Total (Indirect)` = 2, `Home or another house` = 2,
                                                    `School or work` = 2, `Other` = 2)),
                  function(x) add_header_above(x, c(` ` = 1, `Contact type` = 6, `Contact setting (Direct)` = 6))),
           out_name = "table3_contacts")
  
  if(make_table) table3_contacts %>%
    kblOut(booktabs=T, formats=c(html="html"),
           align=c("l|","r","l|","r", "l|","r", "l|","r", "l|","r", "l|","r", "l|"), linesep = "", col.names=NULL,
           other_functions =
             list(function(x) add_header_above(x, c(`Age group` = 1, `Total (Direct)` = 2,`Physical (Direct)` = 2,
                                                    `Total (Indirect)` = 2, `Home or another house` = 2,
                                                    `School or work` = 2, `Other` = 2)),
                  function(x) add_header_above(x, c(` ` = 1, `Contact type` = 3, `Contact setting (Direct)` = 3))),
           out_name = "table3_contacts")
  
}

#' Create Supplemental table C1:
#' - Average daily number of (direct) contacts reported at school or work settings, excluding individuals who reported
#'   no school and work contacts
#' - This is a sensitivity analysis of estimates in table 3
table_sC1_school_work_sens = svyMean2(~school_or_work_contacts,
                                      subset(contact_data_frequency_ps, school_or_work_contacts > 0),
                                      by="participant_age_group_sample", na.rm=T, digits=1)
table_sC1_school_work_sens[, ci95_low := pmax(0, ci95_low)]
table_sC1_school_work_sens[, confint := sprintf("%s - %s", ci95_low, ci95_high)]
table_sC1_school_work_sens[, variable := factor(variable, c("contacts_total", "home_contacts",
                                                            "school_or_work_contacts", "other_contacts",
                                                            "physical_contacts", "contacts_other_median"),
                                                c("Total", "Home or another house", "School or work", "Other",
                                                  "Physical", "Indirect"))]

table_sC1_school_work_sens = table_sC1_school_work_sens %>%
  dcast(participant_age_group_sample~variable, value.var=c("mean", "confint")) %>%
  .[, c("participant_age_group_sample", sapply(c("School or work"), function(x) paste0(c("mean_", "confint_"), x))),
    with=F]

if(make_table) table_sC1_school_work_sens %>%
  kblOut(booktabs=T, formats=c(pdf="latex", html="html"), align=c("l|","r","l"), linesep = "", col.names=NULL,
         other_functions = list(function(x) add_header_above(x, c(`Age group` = 1, `Total (Direct)` = 2))),
         out_name = "table_sC1_school_work_sens")

#' Create supplemental table with totals in contact data
contact_data_numbers = household_data_members[, .N, by = "age_group_60"] %>% .[, N := round(N * fpc_inflate_N_factor)] %>%
  merge(contact_data[, .N, by= "participant_age_group_60"], by.x = "age_group_60", by.y = "participant_age_group_60") %>%
  merge(contact_data_full %>%
          merge(participant_data_design_ps$variables[, c("contactor_id", "participant_age_group_60")], by.x="id", by.y="contactor_id") %>%
          .[!is.na(contact_age_group_60)] %>% dcast(participant_age_group_60 ~ contact_age_group_60, fun.aggregate = length),
        by.x = "age_group_60", by.y = "participant_age_group_60")

if(make_table) contact_data_numbers %>%
  kblOut(booktabs=T, formats=c(pdf="latex", html="html"),
         align=c("l|","l|","l|","l", "l","l", "l","l", "l","l"), linesep = "", col.names=NULL,
         other_functions =
           list(function(x) add_header_above(x, c(`Age group` = 1, `N (population)` = 1,`n (participants)` = 1,
                                                  `0-9` = 1, `10-19` = 1, `20-29` = 1, `30-39` = 1, `40-49` = 1, `50-59` = 1, `60+` = 1)),
                function(x) add_header_above(x, c(`Contactors` = 3,
                                                  `Contactees` = 7))),
         out_name = "table_sZ1_contact_numbers")

#Create supplemental table with realised sample size
contact_data[, .N, by="participant_age_group_sample"] %>% .[order(participant_age_group_sample)] %>%
  .[, c("target_sample_size", "realised_sample_size") := .(c(200, 100, 100, 100, 100, 100), N)] %>%
  .[order(participant_age_group_sample)] %>%
  kblOut(booktabs=T, align=c("l","l","l","l"), linesep = "", col.names=c("Age group", "N", "Target sample size", "Realised sample size"),
         other_functions = list(function(x) kable_styling(x, latex_options = "scale_down")),
         out_name = "table_sZ0_sample_size")

setwd(analysis_dir)
