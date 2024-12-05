# Linked CPES analysis
NCRAS-Macmillan project exploring associations between clinical characteristics and sexuality and language status, using linked NCPES data

This project uses response data from the National Cancer Patient Experience Survey (CPES) in England, linked to the cancer registry data, to explore associations between patient-reported sexuality and language status (whether they speak English as a first language) and clinical characteristics (stage at diagnosis, route to diagnosis, and survival). The project conducted descriptive statistical analysis, and multiple logistic regression analysis with associated assessment of power and interaction. Kaplan-Meier survival analysis is also used to explore survival. The code in this repository extracts the linked CPES data, cleans it into analysable datasets for each outcome of interest, then includes code to run regressions, subgroup regressions, power calculations, and univariate analyses. 

This project is part of the partnership between the National Cancer Registration and Analysis Service (NCRAS) in NDRS, and Macmillan Cancer Support. 

Project status: 
This project is in development. The code will be actively maintained annually by the analyst(s) working on the project, though there is no specified end point for code development. 


Point of contact: 
TBC 


Data requirements: 
- cancer registrations in AV2022 and related tables for IMD, cancer site 
- routes to diagnosis data from AV2020.RTD2020
- CPES response data from 2017-2022 excluding 2020
- CPES linkage tables for the relevant years 


Outputs: 
The project contains code to run regression models and other statistics, which can be used in outputs in various ways. It also contains the functionality to generate bar graphs of descriptive statistics. A summary of the results was generated for handover purposes in November 2024, but no further outputs have yet been developed. 


Prerequisites:
None beyond standard Level 2 CAS access


How to install and use:
Clone the Github repo and run the scripts as required for analysis purposes, though the data extraction and prep scripts should be run before the analysis scripts


License:
MIT (see license file)


Other legal or regulatory requirements:
None

Other notes:
The below is taken from a technical summary document developed for handover purposes by the analyst who developed this project (Lizzie Augarde) in November 2024:

-----
Data extraction:
The project uses CPES responses from 2017-2022 linked to the registry data, using the standard linkage methodology using NHS number and tumour information;
-	Description of linkage process 
-	Example of previous R files for linkage
-	New RAP linkage process
The script “DATA EXTRACTION AND PREP.R” does the following:
1.	Pulls the linked data for each CPES year in sequence, adding route to diagnosis data where available for the relevant year
2.	Adds IMD data for each respondent and checks for the same respondent having different recorded LSOAs of resident between years
3.	Converts the sexuality and language variables to binary, using the relevant question number in each survey 
4.	Cleans the ethnicity data 
5.	Saves the full linked respondents dataset as an RData file

Data prep:
There are then 3 data prep scripts, one for each of the outcomes of interest – stage at diagnosis, routes to diagnosis, and survival. Each one makes sure the relevant variables are set up correctly (e.g. applies the national stats stage functions to the stage info, creates binary survival variables), then keeps only the earliest response for each patient (see Function section). The scripts then write out the data frame for each outcome. A few notes:
-	Routes to diagnosis come from the RTDXXXX schemas in CAS. Details are available here: N:\INFO\_LIVE\NCIN\CAS\zz - svn - do not change\Routes to Diagnosis\AV2020
-	Stage data is taken directly from AT_TUMOUR_ENGLAND, then the stage functions below are applied. NCRAS staging SOP is here: NCRAS staging SOP
Stage_functions_tidy.R:
-	Contains 2 functions to create grouped stage variables from various staging systems 
-	Limits stage reporting to cancers with stage_pi_detail == “Y” i.e. is considered stageable and has a valid stage value 
-	Provided by Chloe Bright, used in the national statistics CMA publication 

Analysis:
There are several analysis scripts which contain the code required to do various aspects of the analysis. To do this project, lots of different models and tests were run on different combinations of confounders etc. Code for each individual model/test isn’t included, as this would be very repetitive. The analysis scripts contain all the required code to run each aspect of the analysis which was conducted, and could be reused to run additional models or adapted for other research questions. 
-	ANALYSIS – UNIVARIATE.R – this script contains code for univariate analysis of various confounding variables. The script starts with a query for all patients diagnosed in 2017-2022, to allow for comparisons between the study cohort and the general cancer population. This extract follows the standard counting cases SOP. This script also contains code to do univariate analyses by sexuality and language status to describe the composition of each group.
-	REGRESSIONS.R – this script contains the regression modelling process which was undertaken for all the multiple logistic regression modelling done in this project. The process use tidymodels recipes to preprocess the data, and the R base glm command for running the models. Links to resources I used to work this out are included in the script. For some models, some additional testing was done to explore where significant associations were found (e.g. interaction testing). Some of this code is included in the script as well. 
-	REGRESSIONS – SUBGROUPS.R – this script is very similar to the previous script, but includes code to work with subgroups within the respondent cohort (e.g. those with screenable cancers). 
-	ANALYSIS – SURVIVAL.R – this script contains the survival analysis process. Binary survival variables are first created for 1 and 5 year survival. Then the script contains an overall Kaplan-Meier survival analysis for the entire cohort at 1 year, then the same for the sexual minority and NNES populations. The script then contains code to plot KM curves, though these were difficult to interpret because of the high probability of survival. The script then uses survdiff to do a comparative survival analysis by sexuality and language status. Lastly, the script contains some code for doing a multiple logistic regression model of survival probability to account for age. 
-	ANALYSIS – POWER SEXUALITY. R – this script contains the power calculations process for each of the analyses looking at sexuality. The process uses the pwr package (could expand in the future to use powerSurvEpi for the survival analysis), and roughly follows the process described here: https://rpubs.com/sypark0215/223385 
-	ANALYSIS – POWER LANGUAGE.R – this is the same for the language analyses 

Other scripts:
There are a few other scripts in the project, which represent parts of the exploratory data analysis completed for this project. These are not up to date or fully commented. There is also a Quarto presentation document with the beginnings of a slide deck of the results, though this wasn’t pursued as an output given timings before I left and the lack of significant results. 
-	Multiple_responses_functions.R
o	Contains 2 functions to deal with multiple CPES responses  
o	Compare_rows function to identify and mark duplicate responses, responses in different years for the same tumour, and responses in different years for different tumours (not used in the project)
o	Rank_multiple_responses function to rank multiple responses from the same patient relating to the same tumour, marking the earliest response as 1
o	These aren’t used anymore, as the first isn’t needed, and the rank_multiple_responses function does the same thing as grouping by PATIENTID, ordering by datayear and using rownumber (because any duplicates in the same year are removed before the CPES data reaches NDRS). Left the script in the project for reference/future use 





