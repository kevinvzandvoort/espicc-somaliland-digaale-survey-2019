#' Estimate birth-, mortality-, and migration-rates
m.birthrate = svyglm(formula=born~offset(log(follow_up_time)), design=household_data_members_rates_svy,
                     family=poisson())
m.migrate_in = svyglm(formula=migrate_in~offset(log(follow_up_time)), design=household_data_members_rates_svy,
                      family=poisson())
m.migrate_out = svyglm(formula=migrate_out~offset(log(follow_up_time)), design=household_data_members_rates_svy,
                       family=poisson())
m.mortality = svyglm(formula=death~offset(log(follow_up_time)), design=household_data_members_rates_svy,
                     family=poisson())
m.mortality.u5 = svyglm(formula=death~offset(log(follow_up_time)), design=subset(household_data_members_rates_svy,
                                                                                 u5==TRUE), family=poisson())
#' Create the first section of table 1 (Demographic characteristics)
table1.1 = rbind(
  data.table(
    variable_name = "Median household size",
    n = household_data_svy$variables[household_consent == "yes", .N],
    mean = svyquantile(~household_members, household_data_svy, c(0.5), na.rm=T)[[1]][1,1],
    confint_low = svyquantile(~household_members, household_data_svy, c(0.25), na.rm=T)[[1]][1,1],
    confint_high = svyquantile(~household_members, household_data_svy, c(0.75), na.rm=T)[[1]][1,1],
    type = "IQR"),
  #' Detailed age-data by year have been removed from the household_members_dataset, so values are hardcoded here
  data.table(
    variable_name = "Median age",
    n = household_data_members_svy$variables[, .N],
    #mean = svyquantile(~household_member_age, household_data_members_srs, c(0.5), na.rm=T)[1,1],
    mean = 15,
    #confint_low = svyquantile(~household_member_age, household_data_members_svy, c(0.25), na.rm=T)[1,1],
    confint_low = 7,
    #confint_high = svyquantile(~household_member_age, household_data_members_svy, c(0.75), na.rm=T)[1,1],
    confint_high = 34,
    type = "IQR"),
  data.table(
    variable_name = "Crude birth rate (per 1000 per year)",
    n = household_data_members_rates_svy$variables[, sum(born==1)],
    mean = round(1000 * 12 * exp(coef(m.birthrate)), 1),
    confint_low = round(1000 * 12 * exp(confint(m.birthrate)[1]), 1),
    confint_high = round(1000 * 12 * exp(confint(m.birthrate)[2]), 1),
    type = "rate"),
  data.table(
    variable_name = "Crude death rate (per 1000 per year)",
    n = household_data_members_rates_svy$variables[, sum(death==1)],
    mean = round(1000 * 12 * exp(coef(m.mortality)), 1),
    confint_low = round(1000 * 12 * exp(confint(m.mortality)[1]), 1),
    confint_high = round(1000 * 12 * exp(confint(m.mortality)[2]), 1),
    type = "rate"),
  data.table(
    variable_name = "Crude U5 death rate (per 1000 per year)",
    n = household_data_members_rates_svy$variables[u5 == TRUE, sum(death==1)],
    mean = round(1000 * 12 * exp(coef(m.mortality.u5)), 1),
    confint_low = round(1000 * 12 * exp(confint(m.mortality.u5)[1]), 1),
    confint_high = round(1000 * 12 * exp(confint(m.mortality.u5)[2]), 1),
    type = "rate"),
  data.table(
    variable_name = "Crude in-migration rate (per 1000 per year)",
    n = household_data_members_rates_svy$variables[, sum(migrate_in==1)],
    mean = round(1000 * 12 * exp(coef(m.migrate_in)), 1),
    confint_low = round(1000 * 12 * exp(confint(m.migrate_in)[1]), 1),
    confint_high = round(1000 * 12 * exp(confint(m.migrate_in)[2]), 1),
    type = "rate"),
  data.table(
    variable_name = "Crude out-migration rate (per 1000 per year)",
    n = household_data_members_rates_svy$variables[, sum(migrate_out==1)],
    mean = round(1000 * 12 * exp(coef(m.migrate_out)), 1),
    confint_low = round(1000 * 12 * exp(confint(m.migrate_out)[1]), 1),
    confint_high = round(1000 * 12 * exp(confint(m.migrate_out)[2]), 1),
    type = "rate"))
table1.1[, heading := "Demographic characteristics"]

rm("m.birthrate", "m.migrate_in", "m.migrate_out", "m.mortality", "m.mortality.u5")

#' Create the second section of table 1 (When did the household settle in Digaale)
table1.2 = merge(
  svyMean2(~house_settle, household_data_svy, multiply = 100, digits = 1, na.rm=T)[grepl("ago", option)],
  household_data_svy$variables[household_consent == "yes", .(n=.N), by="house_settle"],
  by.x="option", by.y="house_settle")
colnames(table1.2) = c("variable_name", "heading", "mean", "confint_low", "confint_high", "n")

table1.2[, heading := "Household settled in Digaale"]
table1.2[, type := "percentage"]
table1.2[, variable_name := factor(variable_name, c("<1 year ago", "1-2 years ago", "2-3 years ago", ">3 years ago"))]
table1.2 = table1.2[order(variable_name)]

#' Create the third section of table 1 (Quality of shelters)
table1.3 = rbind(
  svyMean2(~rooms_total, household_data_svy, digits=1, na.rm=T)[,type := "mean"] %>%
    .[, n := household_data_svy$variables$rooms_total %>% sum(na.rm=TRUE)],
  merge(
    svyMean2(~house_draft+house_leakage, household_data_svy, multiply = 100, digits=1, na.rm=T)[option == "yes"],
    melt(household_data_svy$variables[, .(house_leakage=sum(house_leakage=="yes", na.rm=T),
                                          house_draft=sum(house_draft=="yes", na.rm=T))], value.name = "n",
         id.vars=integer(0)), by="variable")[, type := "percentage"], use.names=T, fill=T)
colnames(table1.3) = c("variable_name", "heading", "mean", "confint_low", "confint_high", "type", "n")
table1.3[, heading := "Quality of shelters"]
table1.3[, variable_name := factor(variable_name, c("rooms_total", "house_draft", "house_leakage"),
                                   c("Total number of rooms (mean)", "Reported draft in shelter",
                                     "Reported leakage in shelter"))]
table1.3 = table1.3[order(variable_name)]

#' Create the fourth section of table 1 (Indoor air pollution)
table1.4a = merge(
  svyMean2(~house_fuel_charcoal+house_fuel_firewood, household_data_svy, multiply = 100, digits=1, na.rm=T) %>%
    .[option == TRUE],
  melt(household_data_svy$variables[, .(house_fuel_charcoal=sum(house_fuel_charcoal),
                                        house_fuel_firewood=sum(house_fuel_firewood))],
       value.name="n", id.vars=integer(0)), by="variable")
colnames(table1.4a) = c("variable_name", "subheading", "mean", "confint_low", "confint_high", "n")
table1.4a[, subheading := "Cooking fuel used"]
table1.4a[, variable_name := factor(variable_name, c("house_fuel_charcoal","house_fuel_firewood"),
                                    c("Charcoal", "Firewood"))]

table1.4b = merge(
  svyMean2(~house_ventilation, household_data_svy, multiply = 100, digits=1, na.rm=T) %>%
    .[option %in% c("cook outside", "yes", "no")],
  household_data_svy$variables[!is.na(house_ventilation), .(n=.N), by="house_ventilation"], by.x="option",
  by.y="house_ventilation")
colnames(table1.4b) = c("variable_name", "subheading", "mean", "confint_low", "confint_high", "n")
table1.4b[, subheading := "Ventilation in cooking area"]
table1.4b[, variable_name := factor(variable_name, c("cook outside", "yes", "no"),
                                    c("Cook outside", "Ventilation absent", "Ventilation present"))]
table1.4b = table1.4b[order(variable_name)]

table1.4 = rbind(table1.4a, table1.4b)[, heading := "Indoor air pollution"]
table1.4[, type := "percentage"]

#' Create the fifth section of table 1 (Primary water source)
table1.5 = merge(svyMean2(~house_water_source_rainwater+house_water_source_tanktruck, household_data_svy,
                          multiply = 100, digits=1, na.rm=T)[option == T],
                 melt(household_data_svy$variables[!is.na(water_source),
                                                   .(house_water_source_rainwater=sum(house_water_source_rainwater),
                                                     house_water_source_tanktruck=sum(house_water_source_tanktruck))],
                      value.name="n", id.vars=integer(0)), by="variable")
colnames(table1.5) = c("variable_name", "heading", "mean", "confint_low", "confint_high", "n")
table1.5[, heading := "Primary water source"]
table1.5[, type := "percentage"]
table1.5[, variable_name := factor(variable_name, c("house_water_source_tanktruck", "house_water_source_rainwater"),
                                   c("Tanker truck delivery", "Rainwater collection"))]
table1.5 = table1.5[order(variable_name)]

#' Create the sixth section of table 1 (Substance use in the household)
table1.6 = merge(
  svyMean2(~house_substance_use_none+house_substance_use_snuff+house_substance_use_smoke+house_substance_use_khat+
             house_substance_use_alcohol, household_data_svy, multiply = 100, digits=1, na.rm=T)[option == T],
  melt(household_data_svy$variables[!is.na(house_substance_use_none),
                                    .(house_substance_use_none=sum(house_substance_use_none),
                                      house_substance_use_snuff=sum(house_substance_use_snuff),
                                      house_substance_use_smoke=sum(house_substance_use_smoke),
                                      house_substance_use_khat=sum(house_substance_use_khat),
                                      house_substance_use_alcohol=sum(house_substance_use_alcohol))], value.name="n",
       id.vars=integer(0)), by="variable")
colnames(table1.6) = c("variable_name", "heading", "mean", "confint_low", "confint_high", "n")
table1.6[, heading := "Substance use in household"]
table1.6[, type := "percentage"]
table1.6[, variable_name := factor(variable_name, paste0("house_substance_use_",
                                                         c("none", "khat", "smoke", "snuff", "alcohol")),
                                   str_to_sentence(c("none", "khat", "smoke", "snuff", "alcohol")))]
table1.6 = table1.6[order(variable_name)]

#' Create the seventh section of table 1 (Malnutrition in U5)
table1.7 = merge(svyMean2(~weight_for_age+height_for_age+weight_for_height+muac_level, nutrition_data_strat_ps,
                          multiply=100, digits=1, na.rm=T),
                 melt(nutrition_data_strat_ps$variables,
                      measure.vars=c("height_for_age", "muac_level", "weight_for_age", "weight_for_height")) %>%
                   .[, .SD[!is.na(value), .(n=.N), by="value"], by="variable"], by.x=c("variable", "option"),
                 by.y=c("variable", "value"), all=TRUE)
table1.7[is.na(n), n:=0]
colnames(table1.7) = c("subheading", "variable_name", "mean", "confint_low", "confint_high", "n")
table1.7[, heading := "Prevalence of malnutrition in U5"]
table1.7[, type := "percentage"]
table1.7[, subheading := factor(subheading, c("weight_for_age", "height_for_age", "weight_for_height", "muac_level"),
                                c("Weight for age", "Height for age", "Weight for height",
                                  "Middle-Upper Arm Circumference"))]
table1.7[, variable_name := factor(variable_name,
                                   c("Not underweight (z > -2)", "Underweight (z <= -2)", "Underweight (z <= -3)",
                                     "Not stunted (z > -2)", "Stunted (z <= -2)", "Severely stunted (z <= -3)",
                                     "Not wasted (z > -2)", "Wasted (z <= -2)", "Severely wasted (z <= -3)",
                                     "Not wasted (>= 125mm)", "Wasted (< -125mm)", "Severely wasted (< -115mm)"),
                                   c("Not underweight (z > -2)", "Underweight (z <= -2)",
                                     "Severely underweight (z <= -3)", "Not stunted (z > -2)", "Stunted (z <= -2)",
                                     "Severely stunted (z <= -3)", "Not wasted (z > -2)", "Wasted (z <= -2)",
                                     "Severely wasted (z <= -3)", "Not wasted (>= 125mm)", "Wasted (< 125mm)",
                                     "Severely wasted (< 115mm)"))]
table1.7 = table1.7[order(subheading, variable_name)]

#' Create the eigth section of table 1 (Self-reported pneumonia)
table1.8 = merge(rbind(svyMean2(~pneumonia, participant_data_design_ps, na.rm=T, multiply = 100, digits=1,
                                by="participant_age_group_sample")[option == T],
                       svyMean2(~pneumonia_6m, participant_data_design_ps, na.rm=T, multiply = 100, digits=1,
                                by="participant_age_group_sample")[option == T]),
                 melt(participant_data_design_ps$variables, measure.vars=c("pneumonia", "pneumonia_6m")) %>%
                   .[, .SD[!is.na(value), .(n=.N), by="value"], by=c("variable", "participant_age_group_sample")] %>%
                   .[value == TRUE, -"value"], by=c("variable", "participant_age_group_sample"))
colnames(table1.8) = c("subheading", "variable_name", "heading", "mean", "confint_low", "confint_high", "n")
table1.8[, heading := "Cumulative incidence of self reported pneumonia"]
table1.8[, type := "percentage"]
table1.8[, subheading := factor(subheading, c("pneumonia", "pneumonia_6m"), c("Ever", "In the last six months"))]
table1.8 = table1.8[order(subheading)]
table1.8[, variable_name := paste0(variable_name, " years old")]

#' Combine al sections in the table
table1_characteristics_and_risk_factors =
  rbind(table1.1, table1.2, table1.3, table1.4, table1.5, table1.6, table1.7, table1.8, fill=T, use.names=T) %>%
  .[, c("heading", "subheading", "variable_name", "n", "mean", "confint_low", "confint_high", "type")]
table1_characteristics_and_risk_factors[, confint := sprintf("%s - %s%s", pmax(0,confint_low), confint_high,
                                                             ifelse(type == "IQR", " (IQR)", ""))]
table1_characteristics_and_risk_factors[, mean_format := ifelse(type == "percentage", sprintf("%s%%", mean), mean)]

rm("table1.1", "table1.2", "table1.3", "table1.4", "table1.4a", "table1.4b", "table1.5", "table1.6", "table1.7",
   "table1.8")

#' Write the table to the output folder
table1_characteristics_and_risk_factors[, c("heading", "subheading", "variable_name", "n", "mean_format",
                                            "confint")] %>%
  kblOut(booktabs=T, align=c("l","l","l","l","r","l"), linesep = "", col.names=c("", "", "", "total", "value", ""),
         other_functions = list(function(x) collapse_rows(x, 1:2, row_group_label_position = "stack",
                                                          row_group_label_fonts = list(list(bold=T, italic=T),
                                                                                       list(bold=F, italic=T))),
                                function(x) kable_styling(x, latex_options = "scale_down")),
         out_name = "table1_characteristics_and_risk_factors")

setwd(analysis_dir)
