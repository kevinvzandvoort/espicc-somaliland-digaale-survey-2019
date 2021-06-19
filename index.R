#' Install and load dependencies
#' - make sure you have pacman installed: install.packages("pacman")
#' - or install and load manually
pacman::p_load(data.table, survey, ggplot2, patchwork, magrittr, kableExtra, stringr, lmtest, zscorer)

#' Set the analysis directory (usually the root directory of repo)
analysis_dir = getwd()

#' Set global options, create output folders, load helper functions, and load data
source(sprintf("%s/scripts/0_setup.R", analysis_dir))

#' This estimates the total population size in Digaale, and FPC to use in the models
#' - creates table_sB1_vacant_households
#' - creates table_sB2_households_visited
source(sprintf("%s/scripts/1_fpc_correction.R", analysis_dir))

#' This does some additional cleaning of the data, assigns age-groups, creates new variables, and creates the survey
#' objects to use when correcting for FPC and to do the poststratification
source(sprintf("%s/scripts/2_prepare_data.R", analysis_dir))

#' Create table 1: Characteristics of participating households and prevalence of risk factors in Digaale IDP camp
#' - creates table1_characteristics_and_risk_factors
source(sprintf("%s/scripts/3_table1.R", analysis_dir))

#' Create tables 2, 3, and supplemental table C1
#' - creates table2_travel
#' - creates table3_contacts
#' - creates table_sC1_school_work_sens
source(sprintf("%s/scripts/4_tables_other.R", analysis_dir))

#' Regression analysis assessing collected variables with self-reported pneumonia incidence
#' - creates table_sD1_regression_pneumonia
#' - creates table_sD2_regression_pneumonia_6m
source(sprintf("%s/scripts/5_tables_regression.R", analysis_dir))

#' Create figure 1: Demographic distributions in Digaale IDP camp
#' - creates figure1_demographics
source(sprintf("%s/scripts/6_figure1.R", analysis_dir))

#' Create figure 2: Contact frequencies, types, and matrices
#' - creates figure2_contacts
source(sprintf("%s/scripts/7_figure2.R", analysis_dir))

#' Additional analyses contact estimates
#' - creates figure_sC1_bootstrapped_matrices
#' - creates figure_sC2_intra_extra_household_contacts
#' - creates figure_sC3_contact_bysex
#' - creates figure_sC4_household_contacts_expected_reported
source(sprintf("%s/scripts/8_figures_supp.R", analysis_dir))
