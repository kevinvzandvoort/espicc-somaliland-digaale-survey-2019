#' Number of shelters in Digaale
(n_shelters = nrow(household_data))

#' Number of shelters where a household member was present / not present when conducting the survey
(n_shelters_present = nrow(household_data[household_present != "no"]))
(n_shelters_notpresent = nrow(household_data[household_present == "no"]))

#' Number of shelters where a household member consented
(n_shelters_consented = nrow(household_data[household_present != "no" & household_consent == "yes"]))

#' We conducted a simple random sample of 96 shelters where no individual was present on any visit, and collected
#'  information about 73 shelters
(n_shelters_notpresent_sampled = nrow(missing_houses_manual_categories))
(n_shelters_notpresent_sampled_included = nrow(missing_houses_manual_categories[status!="unknown"]))

#' We apply a finite population correction (fpc) to estimate the proportion of vacant shelters and calculate sample
#'  weights (pw_all and pw_included) for the shelters included in this small assessment
missing_houses_manual_categories[, c("fpc", "pw_all", "pw_included") := .(
  n_shelters_notpresent, n_shelters_notpresent_sampled/n_shelters_notpresent,
  n_shelters_notpresent_sampled_included/n_shelters_notpresent)]

#' We create a survey design object to calculate population representative estimates
survey_shelters_notpresent_included_srs = survey::svydesign(
  ids=~0, probs=~pw_included, strata=NULL, variables=NULL, fpc=~fpc,
  data=missing_houses_manual_categories[status != "unknown"])

#' We use the lower bound of the estimated number of shelters that are vacant to calculate the most conservative
#'  estimate to be used in the fpc
n_shelters_empty = svyTotal2(~status, survey_shelters_notpresent_included_srs, digits=0, na.rm=F) %>%
  .[option != "occupied", ci95_low] %>% sum

#' We estimate the total number of non-vacant shelters in Digaale
n_shelters_for_fpc = n_shelters - svyTotal2(~status, survey_shelters_notpresent_included_srs, digits=0, na.rm=F) %>%
  .[option != "occupied", ci95_low] %>% sum

#' We estimate how many of all households living in Digaale were present at the time of the survey, and use the inverse
#'  of this number to calculate the fpc_inflate_N_factor
#'  - This estimate will be used to inflate the population size as observed in our study, as an estimate of the total
#'    population size living in Digaale
fpc_inflate_N_factor = n_shelters_for_fpc/n_shelters_present

#' We summarize the results of the small random sample across shelters where no individual was present on multiple
#'  visits in the empty_households table (Supplemental Table B1 in the manuscript)
table_sB1_vacant_households = survey_shelters_notpresent_included_srs$variables[, .N, by=status] %>%
  merge(svyMean2(~status, survey_shelters_notpresent_included_srs, digits=1, multiply=100, na.rm=F), by.x="status",
        by.y="option") %>%
  merge(svyTotal2(~status, survey_shelters_notpresent_included_srs, digits=0, na.rm=F), by.x="status", by.y="option",
        suffixes=c(".prop",".total"), all.x=T) %>%
  .[, c("status", "prop_format", "prop_format_ci", "total_format", "total_format_ci") :=
      .(str_to_sentence(status), sprintf("%s%%", mean), sprintf("%s - %s", ci95_low.prop, ci95_high.prop),
        as.numeric(total), ifelse(!is.na(ci95_low.total) & !is.na(ci95_high.total),
                                  sprintf("%s - %s", ci95_low.total, ci95_high.total), NA))] %>%
  .[,c("status", "N", "prop_format", "prop_format_ci", "total_format", "total_format_ci")] %>%
  rbind(.[, .(status="Total", N=sum(N), total_format=sum(total_format))], fill = TRUE, use.names = TRUE)


if(make_table) table_sB1_vacant_households %>%
  kblOut(col.names = c("Status of shelters not present", "n", "%", "(95% CI)", "N", "(95% CI)"),
           align = c("l|", "l|", "r", "l", "|r", "l"), booktabs = TRUE, out_name = "table_sB1_vacant_households")

#' We also summarize these estimates of the total number of shelters and households in Digaale included in the study
#'  in the households_visited table (Supplemental Table B2 in the manuscript)
table_sB2_households_visited = data.table(type = c("Total shelters in Digaale", "Assumed inhabited shelters in Digaale",
                    "Household present during survey", "Households consented that were present"),
                    N = c(n_shelters, n_shelters_for_fpc, n_shelters_present, n_shelters_consented),
                    prop = c(NA, c(n_shelters_for_fpc/n_shelters, n_shelters_present/n_shelters_for_fpc,
                                   n_shelters_consented/n_shelters_present) %>%
                               sapply(function(x) sprintf("%s%%", round(x * 100, 0)))))

if(make_table) table_sB2_households_visited %>%
  kblOut(col.names = c("Shelters", "N", "%"), align = c("l|", "l|", "l"), booktabs = TRUE,
           out_name = "table_sB2_households_visited")

rm("missing_houses_manual_categories", "survey_shelters_notpresent_included_srs")
setwd(analysis_dir)