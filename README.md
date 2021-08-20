# Social contacts and other risk factors for respiratory infections among internally displaced people in Somaliland. Data and analyses.

This repository contains the data and code used for all analyses described in our manuscript:

Van Zandvoort K, Bobe MO, Hassan AI, Abdi MI, Ahmed MS, Warsame MY, Wais MA, Diggle E, Satzke C, Mulholland K, Egeh MM, Hassan MM, Hergeeye MA, Eggo RM, Checchi F, Flasche S, *Social contacts and other risk factors for respiratory infections among internally displaced people in Somaliland*. Available at <http://example.com>.

This work is part of a larger study: Evaluating Strategies for Pneumococcal Immunization Campaigns in Crises [(ESPICC)](https://www.elrha.org/project/pneumococcal-vaccination-strategies-for-crisis-affected-populations/).

### How to download

You can clone the repository or download the zip from this URL: <https://github.com/kevinvzandvoort/espicc-somaliland-digaale-survey-2019/archive/refs/heads/main.zip>.

### Questionnaires

This survey was implemented using Open Data Kit. Android tablets were provided by LSHTM Open Data Kit <https://opendatakit.lshtm.ac.uk>

- Questionnaires were programmed in xlsx, and can be found in the `./questionnaire/xlsx` folder
- They were converted in xls files uploaded to an ODK server and used with *ODK Collect* during fieldwork. The xls files can be found in the `./questionnaire/xls` folder.
  - The xls for the household questionnaire was manually edited to randomly select household members (based on their age), for inclusion in the contact survey. Edits are wrapped within comments listed as `<MANUAL EDIT>` and `<END MANUAL>`.

The following questionnaires are available:

- `s1_household`
  - A household survey asking about household-level risk factors and household demographics
- `s2_contacts`
  - A contact survey asking about social contacts within the 24 hours before the survey, and individual-level risk factors for respiratory infections
- `s3_anthropometry`
  - A form to enter anthropometric measures
- `s4_missing_houses`
  - A form to ask neighbours of shelters that were absent on all visits about the status of these shelters

### Included data sets

Only a subset of the data collected with these questionnaires during the survey has been used for this analysis.
Data has been anonymized, and links between household-, contact-, and nutrition- data have been removed.
The anonymized data can be used to replicate all analyses, figures, and tables in the manuscript.
All data is stored in the `./data` folder. A data dictonary is provided in `./data/data_dictionary.xlsx`

The following datasets are included:

- `household_data.RDS`
  - Reported household-level risk-factors
  - collected with the `s1_household` form
- `household_data_members.RDS`
  - Age-group and sex of household members
  - collected with the `s1_household` form
- `household_data_members_migration.RDS`
  - Age of people reported to have left surveyed households in the six months preceding the survey
  - collected with the `s1_household` form
- `household_data_members_mortality.RDS`
  - Age of people reported to have died in surveyed households in the six months preceding the survey
  - collected with the `s1_household` form
- `missing_houses_manual_categories.RDS`
  - Status of shelters where no individual was present on repeat visits, according to their neighbours
  - collected with the `s4_missing_houses` form
- `participant_data.RDS`
  - Non-contact related individual-level risk factors
  - collected with the `s2_contacts` form
- `contact_data_contactors.RDS`
  - Contact-related information from contactors (participants in the contact survey)
  - collected with the `s2_contacts` form
- `contact_data_contactees.RDS`
  - Information about contactees reported by contactors
  - collected with the `s2_contacts` form
- `nutrition_data.RDS`
  - anthropometric assessments of children aged 6 to 59 months old, who were included in the contact survey
  - collected with the `s3_anthropometry` form
- `regression_data.RDS`
  - combined (aggregated) datasets of contact, participant, nutrition, and household level data, used for logistic regression analysis

### Figures and tables

Code for the analysis can be found in the `./scripts` folder.
The analysis can be replicated by running the `index.R` file (in R), which sources these scripts.

Figures and tables will be created in a newly formed `./output` folder

### Socialmixr

The `./scripts/socialmixr_zenodo_data.R` script generates the data that can be used with the `socialmixr` package.
This data has been uploaded to Zenodo: <"https://zenodo.org/record/5226281#.YR-TzlvTVH6">.

To use data in `socialmixr`:
```r
pacman::p_load(magrittr, socialmixr, data.table)

#' Get data from Zenodo
digaale_contact_data =
  socialmixr::get_survey("https://zenodo.org/record/5226281#.YR-TzlvTVH6")

#' The estimated population size in Digaale (for provided age groups)
#'  can manually be downloaded
digaale_survey_population =
  data.table::fread("https://zenodo.org/record/5226281/files/espicc_somaliland_digaale_survey_population.csv")

#' Note that weekends fall on Fridays and Saturdays in Somaliland.
#' - The dayofweek variable provided in the dataset has been kept
#'   consistent with R defaults (0: Sunday to 6: Saturday)
digaale_contact_data$participants[, c("dayofweek", "dayofweek_name", "weekend")] %>%
  unique %>% setorder(dayofweek) %>% .[]
#' socialmixr currently assumes the weekend to fall on dayofweek
#'  6 (Saturday) and 0 (Sunday)
#' - dayofweek can be manually edited so that Fridays and Saturdays
#'   are taken as the weekend, if you wish to weight contacts by
#'   weekday
digaale_contact_data$participants[, dayofweek := ifelse(dayofweek == 6, 0, dayofweek + 1)]

#' The contact matrix can then be constructed as follows
#' - The provided survey_population can be used to construct a
#'   population representative matrix for Digaale IDP camp
#' - As the sample is not self-weighing (oversampling of young
#'   age groups), it is recommended to apply the survey_weight
#'   as weights
digaale_contact_matrix = digaale_contact_data %>%
  socialmixr::contact_matrix(survey.pop = digaale_survey_population,
                             age.limits = digaale_survey_population$lower.age.limit,
                             symmetric = TRUE, weights = "survey_weight", weigh.dayofweek = TRUE)

#' Note socialmixr's contact matrices show contactors in rows
#'  and contactees in columns
digaale_contact_matrix$matrix %>% round(1)
```
```
contact.age.group
     [0,10) [10,20) [20,30) [30,40) [40,50) [50,60) [60,70) [70,80)    80+
[1,]    3.9     1.2     0.6     0.8     0.5     0.3     0.2     0.1     0.1
[2,]    1.6     4.5     1.0     0.8     0.6     0.4     0.2     0.1     0.1
[3,]    1.9     2.6     2.7     1.7     1.2     0.7     0.2     0.2     0.2
[4,]    2.8     2.1     1.7     2.9     1.4     1.1     0.4     0.3     0.2
[5,]    2.3     1.9     1.6     1.8     1.6     1.0     0.6     0.3     0.2
[6,]    1.8     2.0     1.5     2.1     1.6     1.6     0.8     0.4     0.4
[7,]    2.3     1.4     0.6     1.1     1.4     1.1     0.9     0.6     0.4
[8,]    1.2     0.8     0.8     1.4     1.1     1.0     0.9     0.8     0.4
[9,]    1.2     1.2     1.2     1.0     0.7     1.1     0.9     0.6     0.5
```
