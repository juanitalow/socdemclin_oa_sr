# Summary scripts
## Data and summary of study data
This contains all scripts that can be applied on raw file ```included_studies_cleaned060625.csv``` in socdemclin_oa_sr/data
<br>
1. Summarising the reporting of sociodemographic and clinical variables "yes" "no" "unclear" by study authors, year, and title:
   * ```summary_socidem_clin_supplementary.R```
2. Organising countries and income levels: ```country codes.R```
3. Cleaning and organising sociodemographic variables according to PROGRESS-Plus Framework:
   * Place of residence: ```Place_upload.R```
   * Race and ethnicity: ```Raceethnicity_upload.R```
   * Occupation: ```Employment_upload.R```
   * Gender/Sex: ```sexgendercheck.R```
   * Religion: _extracted manually as only 1 RCT_
   * Education: ```Education_upload.R```
   * Socioeconomic status: ```income_upload.R```. Will need to manually match income level by year manually after.
   * Social capital: ```Socialcap_upload.R```
   * Plus: ```Plus_upload.R```
<br>
4. Organising clincial variables: ```Clinical variables codes.R```  

