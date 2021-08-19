# Social contacts and other risk factors for respiratory infections among internally displaced people in Somaliland
  
This repository contains the data and code used for all analyses described in our manuscript:
  
Van Zandvoort K, Bobe MO, Hassan AI, Abdi MI, Ahmed MS, Warsame MY, Wais MA, Diggle E, Satzke C, Mulholland K, Egeh MM, Hassan MM, Hergeeye MA, Eggo RM, Checchi F, Flasche S, *Social contacts and other risk factors for respiratory infections among internally displaced people in Somaliland*. Available at <http://example.com>.

This work is part of a larger study: Evaluating Strategies for Pneumococcal Immunization Campaigns in Crises [(ESPICC)](https://www.elrha.org/project/pneumococcal-vaccination-strategies-for-crisis-affected-populations/).

### How to download

You can clone the repository or download the zip from this URL: <https://github.com/kevinvzandvoort/ESPICC-Somaliland-Digaale-survey-2019/archive/refs/heads/main.zip>.

### Questionnaires

This survey was implemented using Open Data Kit, on Android tablets supported by LSHTM Open Data Kit <https://opendatakit.lshtm.ac.uk>

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
All data is stored in the `./data` folder.

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
