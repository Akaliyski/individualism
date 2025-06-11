
*************************************************************************************************
* Project: Individualism-Collectivism:Reconstructing Hofstede's Dimension of Cultural Differences
* Author: Plamen Akaliyski (Lingnan University)
* Contact: akaliyski.plamen@gmail.com
* Date created: 2025-05-29
* Last updated: 2025-05-29
*************************************************************************************************

clear all
set more off
version 18.0

* Define global path to your folder (adapt as needed)
global path "C:\Users\akali\OneDrive - Lingnan University\Desktop\Replication_Files"

global raw_data_path "$path\Data\Raw_data"
global proc_data_path "$path\Data\Processed_data"
global R_data_path "$path\Data\R_data"
global output_path "$path\Output"


*************************************************************************************************
*************************************************************************************************
**************************** STUDY 1: ASSESSING HOFSTEDE'S INDEX ********************************
*************************************************************************************************
*************************************************************************************************

* Load data
use  "$raw_data_path\IDV_dimensions_and_obesity_data", clear

* 2. (Re)Construct new individualism-related indices
gen autonomy_Schw = ((AFFAUT + INTELLAU/2)) - EMBEDD // Create a cultural dimension following Schwartz (2006)
gen idv_Inglehart = (self_expression + secular_rational)/2 // Combine the two dimensions
gen idv_globe = 6.36 - coll_GLOBE // Reverse-code so that higher scores = higher individualism

drop coll_GLOBE

* Rename variables for clarity
rename indiv idv_Minkov
rename individualism_BW idv_BW
rename self_expression selfexp_Ingl
rename idv idv_Hofs
rename individualism_MK idv_MK

* Label variables
label variable autonomy_Schw "Autonomy vs. Embeddedness (Schwartz) 2006"
label variable idv_Minkov "Individualism (Minkov) 2017"
label variable idv_BW "Individualism vs. Collectivism (Beugelsdijk & Welzel) 2018"
label variable selfexp_Ingl "Self expression (Inglehart) 2005-2020"
label variable idv_Hofs "Individualism (Hofstede) 1970"
label variable EVI "Emancipative Values Welzel ?"
label variable idv_Inglehart "Individualism Inglehart"
label variable idv_globe "Individualism GLOBE"
label variable idv_MK "Individualism Minkov and Kaasa 2022"
label variable flexibility_MK "Flexibility Minkov and Kaasa 2022"
label variable flex "Flexibility Minkov 2007"


global idv_dimensions idv_Hofs idv_BW idv_Minkov idv_globe idv_Inglehart EVI autonomy_Schw 

* Inspect summary statistics of I-C-related indices
sum $idv_dimensions

drop NAMES_STD _merge

gen  country_IDV_dim = country 
* Create COW country codes for matching at later stages
kountry country_short , from (iso3c) to (cown)
rename _COWN_ cowcode

* Harmonize COW country codes so that they can match with other datasets
recode cowcode 200 = 201  345 = 348 // change codes for UK and Serbia 
replace cowcode = 713 in 195 // Hong Kong
replace cowcode = 714 in 198 // Taiwan

drop if cowcode ==.  // this is necessary because otherwise the datasets cannot be matched by cowcode
save "$proc_data_path\idv_prepared", replace // save data for later use

keep country_code country cowcode obesity_m obesity_f country_short country EN_EA $idv_dimensions flexibility_MK flex country_IDV_dim
save "$proc_data_path\idv_dimensions_subset", replace  // save data with I-C dimensions for later use


	**************************************************************************************************
    ****************************** Study 1a: Convergent Validity *************************************
	**************************************************************************************************

	* Load the dataset with all I-C measures
 use "$proc_data_path\idv_dimensions_subset", clear

* Keep only countries with complete data across all I-C dimensions
 gen sample = 1 if idv_H !=. & idv_Minkov !=. & idv_BW !=. & autonomy_Schw !=. &  idv_Inglehart !=. & EVI !=. & idv_globe !=.

*rescale all dimensions to vary between 0 and 100
foreach v of varlist $idv_dimensions {
    qui summ `v' if sample == 1
    gen `v'2 = (`v' - r(min)) / (r(max) - r(min))*100 if sample==1
	
	drop `v' 
	rename `v'2 `v' 
}

* Construct mean-based aggregate index using all six dimensions and compute reliability (Cronbach's alpha)
alpha idv_Minkov idv_BW autonomy_Schw idv_Inglehart EVI idv_globe, gen (IDV_combined) 

* Difference from Hofstede's index
gen IDV_difference = idv_H - IDV_combined


* Construct a WVS-based subindex
gen idv_WVS_conbined = (idv_BW + idv_Inglehart + EVI)/3

* Alternative composite index with reliability (Cronbach's alpha) 
alpha idv_Minkov idv_WVS_conbined autonomy_Schw idv_globe, gen (IDV_combined2) 

* Difference from Hofstede's index (alternative)
gen IDV_difference2 = idv_H - IDV_combined2


* Keep only complete cases
keep if sample == 1

* Keep only relevant variables
keep country EN_EA $idv_dimensions idv_WVS_conbined IDV_combined IDV_difference IDV_combined2 IDV_difference2 cowcode

* Correlations with all I-C dimensions (with Bonferroni correction)
pwcorr $idv_dimensions, sig  bonferroni 

* Change the working directory and export correlations using asdoc for Table 2
cd "$output_path"
asdoc cor $idv_dimensions, label replace save(Table2.doc)

* Additional correlations with WVS-based composite index (for SOM Table S1)
asdoc corr idv_Hof idv_Minkov idv_WVS_conbined autonomy_Schw idv_globe , label replace save(TableS1.doc)

* Standardize country names for export
replace country = "Türkiye" if country == "Turkey"
replace country = "Taiwan" if country == "Taiwan ROC"

* Export to R for data visualization
saveold "$R_data_path\Difference_from_Hofstede.dta", version(12) replace

	
	
************** Analysis of the difference between each I-C dimension and Hofstede's I-C (for SOM Tables S2 to S7)
  use "$proc_data_path\idv_dimensions_subset", clear

	drop if idv_Hofs == .
	*** Create variables for the differences between Hofstede and each of the indicators: 
	* Hofstede vs. Minkov 
	qui summ idv_Minkov if idv_Minkov !=. 
		gen idv_M2 = (idv_Minkov - r(min)) / (r(max) - r(min))*100 if idv_Minkov !=. 
	qui summ idv_Hofs if idv_Minkov !=. 
		gen idv_H2 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if idv_Minkov !=. 
				gen diff_H_M = idv_H2 - idv_M2
				
	* Hofstede vs. Beugelsdijk & Welzel
	qui summ idv_BW if idv_BW !=. 
		gen idv_BW2 = (idv_BW- r(min)) / (r(max) - r(min))*100 if idv_BW !=. 
	qui summ idv_Hofs if idv_BW !=. 
		gen idv_H3 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if idv_BW !=. 
				gen diff_H_BW = idv_H3 - idv_BW2
	
	* Hofstede vs. GLOBE
	qui summ idv_globe if idv_globe !=. 
		gen GL2 = (idv_globe- r(min)) / (r(max) - r(min))*100 if idv_globe !=. 
	qui summ idv_Hofs if idv_globe !=. 
		gen idv_H4 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if idv_globe !=. 
				gen diff_H_GL = idv_H4 - GL2
	
	* Hofstede vs. Inglehart
	qui summ idv_Ingl if idv_Ingl !=. 
		gen INGL2 = (idv_Ingl- r(min)) / (r(max) - r(min))*100 if idv_Ingl !=. 
	qui summ idv_Hofs if idv_Ingl !=. 
		gen idv_H5 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if idv_Ingl !=. 
				gen diff_H_INGL = idv_H5 - INGL2
	
	* Hofstede vs. Schwartz
	qui summ autonomy_Schw if autonomy_Schw !=. 
		gen A2 = (autonomy_Schw - r(min)) / (r(max) - r(min))*100 if autonomy_Schw !=. 
	qui summ idv_Hofs if autonomy_Schw !=. 
		gen idv_H6 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if autonomy_Schw !=. 
				gen diff_H_A = idv_H6 - A2
				
	* Hofstede vs. Welzel's EVI			
	qui summ EVI if EVI !=. 
		gen EVI2 = (EVI - r(min)) / (r(max) - r(min))*100 if EVI !=. 
	qui summ idv_Hofs if EVI !=. 
		gen idv_H7 = (idv_Hofs - r(min)) / (r(max) - r(min))*100 if EVI !=. 
				gen diff_H_EVI = idv_H7 - EVI2				
	
	* Drop intermediate vars
	drop idv_M2 idv_H2 idv_BW2 idv_H3 INGL2 idv_H4 A2 idv_H5 EVI2 idv_H6 idv_H7
	
	* Keep only relevant output variables
    keep country country_IDV_dim EN_EA  diff_H_M diff_H_BW diff_H_GL diff_H_INGL diff_H_A diff_H_EVI 
	
	* Export for R visualization
	saveold "$R_data_path\All individualism measures.dta", version (12) replace

	* Export correlations between I-C dimension differences (for SOM Footnote 10)
	asdoc cor  diff_H_BW diff_H_M diff_H_GL diff_H_EVI diff_H_INGL diff_H_A, label replace save(Footnote10.doc)
	
	
	
	
	
	
	******************************************************************************************
	*************************** Study 1b: Nomological validity *******************************
	******************************************************************************************
	* Load processed I-C dimension data
	use "$proc_data_path\idv_dimensions_subset", replace 
	
	* Harmonize UK's cowcode to match QoG dataset
	recode cowcode (201=200)
	
	* Merge with QoG dataset (already preprocessed for merge; data older than year 2000 were deleted as the file was excessively large)
	drop country
	merge 1:n cowcode using "$raw_data_path\my_qog_std_ts_jan21"  
	drop _merge
	
	* Keep only 2018 data
	keep if year == 2018
	
	* Log-transform GDP per capita
	gen gdppc_ln = ln(wdi_gdpcappppcur)
	
	* Retain only relevant variables for analysis
	keep country_code cowcode country cname country_IDV_dim ccodealp cname_year ccodealp_year idv_Hofs idv_BW idv_Minkov idv_globe idv_Inglehart EVI autonomy_Schw vdem_libdem spi_ospi ffp_hr ti_cpi vdem_corr undp_hdi gdppc_ln		
	
	* Correlations with nomological network variables (run only on complete cases for each correlation)
	   foreach var of varlist ffp_hr vdem_libdem spi_ospi ti_cpi vdem_corr undp_hdi gdppc_ln {
		corr idv_Hofs idv_BW idv_Minkov idv_globe idv_Inglehart EVI autonomy_Schw `var'  if !missing(idv_Hofs, idv_BW, idv_Minkov, idv_globe, idv_Inglehart, EVI, autonomy_Schw)
   }
   
   * Export note: Selected correlation coefficients (i.e., between each I-C and each predictor) were manually copied from the Stata Results window and compiled into an Excel file ("Study2b_Nomological_Correlations.xlsx") for data visualization in R (Figure 3).

	
	
	
	
	
	*********************************************************************************************
	*********************** Study 1c: Implications for Predictive Utility ***********************
	*********************************************************************************************
	
	* Load the dataset with I-C indices and obesity data
	use "$proc_data_path\idv_prepared", clear

	* Merge with QoG dataset (already preprocessed for merge)
	* Note: 'country' is str in master and numeric in using, but not used in merge. 'force' avoids type conflict.
	merge 1:n cowcode using "$raw_data_path\my_qog_std_ts_jan21",  force 
	drop _merge

	* Retain only data from 2015 for comparability across countries
	keep if year_QoG == 2015

	* Rescale I-C dimensions to range from 0 to 100 (for comparability)
	global idv_dimensions idv* autonomy_Schw EVI
	foreach v of varlist $idv_dimensions {
    qui summ `v'
    gen `v'2 = (`v' - r(min)) / (r(max) - r(min))*100 
	
	drop `v' 
	rename `v'2 `v' 
	}

	* Define dependent variables: female and male obesity
	global DV_f obesity_f
	global DV_m obesity_m	  

	* Define control variables for female and male models, respectively
	global IV_f dr_eg undernurish uneven_eco_dev yr_sch_f gdppc_ln fi_sog healthexp_perc_gdp wgi_avg
	global IV_m dr_eg undernurish uneven_eco_dev yr_sch_m gdppc_ln fi_sog healthexp_perc_gdp wgi_avg

	* Regressions: Assess predictive utility of each I-C index controlling for socioeconomic covariates

	*Hofstede
reg $DV_f idv_Hofs $IV_f, b 
	est store H_f
reg $DV_m idv_Hofs $IV_m, b
	est store H_m

	*Beugelsdijk & Welzel
reg $DV_f idv_BW $IV_f, b 
	est store BW_f
reg $DV_m idv_BW $IV_m, b
	est store BW_m
	
	*Schwartz
reg $DV_f autonomy_Schw $IV_f, b 
	est store S_f
reg $DV_m autonomy_Schw $IV_m, b
	est store S_m
	
	*Minkov
reg $DV_f idv_Minkov $IV_f, b 
	est store M_f
reg $DV_m idv_Minkov $IV_m, b
	est store M_m

	*Inglehart
reg $DV_f idv_Inglehart $IV_f, b 
	est store I_f
reg $DV_m idv_Inglehart $IV_m, b
	est store I_m

	*Welzel EVI
reg $DV_f EVI $IV_f, b 
	est store W_f
reg $DV_m EVI $IV_m, b
	est store W_m 
	
*GLOBE
reg $DV_f idv_globe $IV_f, b 
	est store G_f
reg $DV_m idv_globe $IV_m, b
	est store G_m 

	* Export regression coefficients (for Figure 4): Only I-C predictors, exclude controls
	esttab H_f H_m BW_f BW_m S_f S_m M_f M_m I_f I_m W_f W_m G_f G_m  using "Data for Figure 4.csv", drop(dr_eg yr_sch_f undernurish uneven_eco_dev yr_sch_m gdppc_ln fi_sog healthexp_perc_gdp wgi_avg) b(3) se(3) label  noparentheses replace 
	
	* Note on output export (Figure 4):The regression output from the above models was exported to CSV using esttab,
* but the final figure used in the paper (Figure 4) is based on a manually curated file titled "Study2c_individualism_and_obesity.xlsx". This file was compiled by copying the regression coefficients and standard errors for each I-C index from the Stata Results window or the esttab output, and restructuring them for use in R for data visualization.

	
	
	*****************
* Repeating the same predictive models but restricting to common sample with non-missing data across all I-C dimensions (for comparability; SOM Figure S8)
	
	gen sample = 1 if idv_H !=. & idv_BW !=. & autonomy_Schw !=. &  selfexp_Ingl !=. & EVI !=. & idv_Minkov !=. & idv_globe !=. 
	
*Hofstede
reg $DV_f idv_Hofs $IV_f if sample==1, b 
	est store H_f2
		vif
reg $DV_m idv_Hofs $IV_m if sample==1, b
	est store H_m2
		vif

*Beugelsdijk & Welzel
reg $DV_f idv_BW $IV_f if sample==1, b 
	est store BW_f2
		vif
reg $DV_m idv_BW $IV_m if sample==1, b
	est store BW_m2
		vif
		
*Minkov
reg $DV_f idv_Minkov $IV_f if sample==1, b 
	est store M_f2
reg $DV_m idv_Minkov $IV_m if sample==1, b
	est store M_m2
		vif
			
*Schwartz
reg $DV_f autonomy_Schw $IV_f if sample==1, b 
	est store S_f2
		vif
reg $DV_m autonomy_Schw $IV_m if sample==1, b
	est store S_m2
		vif
		
*Inglehart
reg $DV_f idv_Inglehart $IV_f if sample==1, b 
	est store I_f2
		vif
reg $DV_m idv_Inglehart $IV_m if sample==1, b
	est store I_m2
		vif
		
*Welzel EVI
reg $DV_f EVI $IV_f if sample==1, b 
	est store W_f2
		vif
reg $DV_m EVI $IV_m if sample==1, b
	est store W_m2
		vif

*GLOBE
reg $DV_f idv_globe $IV_f if sample==1, b 
	est store G_f2
		vif
reg $DV_m idv_globe $IV_m if sample==1, b
	est store G_m2
		vif
		
	* Export regression coefficients (for SOM Figure S8): Only I-C predictors, exclude controls
	esttab H_f2 H_m2 BW_f2 BW_m2 M_m2 M_f2 S_f2 S_m2 I_f2 I_m2 W_f2 W_m2 G_f2 G_m2 using "Data for Figure S8.csv", drop(dr_eg yr_sch_f undernurish uneven_eco_dev yr_sch_m gdppc_ln fi_sog healthexp_perc_gdp wgi_avg) b(3) se(3) label  noparentheses replace 
	
	
	
	
	
	
	
	
	
***********************************************************************************************************************
***********************************************************************************************************************
********************************** STUDY 2: CREATING A NEW INDIVIDUALISM INDEX ****************************************
***********************************************************************************************************************
***********************************************************************************************************************

	* Data preparation: Wave 6 
	* We use Wave 6 data from the Integrated_values_surveys_1981-2021. However, we use EVS_WVS_Joint_Stata_v5_0 for Wave 7 data because it has 4 more countries/regions compared to the Integrated_values_surveys_1981-2021.
	* Since the Integrated_values_surveys_1981-2021 was too large (>1GB) and Github accepts files only under 100 MB, we: 
		*1. Filtered the data to WVS Wave 6 only using the command "keep if S002VS == 6" after opening the original file. 
		*2. Dropped some unuased socio-demographic and country-specific variables using the command "drop X026 - X050B".
	
	use "$raw_data_path\Integrated_values_surveys_1981-2021 - WVS Wave 6, trimmed.dta", clear
	
	* Fix missing country labels 
	label define S003 496 "Mongolia", add
	label define S003 404 "Kenya", add
	
	* Standardize key identifiers
	gen wave = 6	
	rename S003 country
	rename COW_NUM cowcode
	rename S017 weights
	rename S020 year 
	
	* Count number of countries
	distinct country 
	
	* Retain only relevant variables
	keep country cowcode COW_ALPHA wave year weights 	A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 D054 D026_05 A006 C001_01 C001 C002_01 C002 D060 D081 D001_B G007_18_B G007_33_B G007_34_B G007_35_B G007_36_B A124_09 A124_02 A124_06 F028 F118 F119 F120 F121 F123 F122 F132 D059 D078   A124_03 A124_08  E035 E039 E041  F114A F115 F116 F117   	X001 X003 S016 X025R 
	
	* Save cleaned Wave 6 data
	save "$proc_data_path\wave 6 individual level.dta", replace 
	
	
	*** Load and prepare Wave 7 data (WVS7 + EVS5) from the joint values survey
		use "$raw_data_path\EVS_WVS_Joint_Stata_v5_0.dta", clear
	
	* Harmonize variables across datasets
	drop wave 
	gen wave = 7 
	rename gwght weights 
	rename cntry country 
	rename cntrycow cowcode
	rename lnge_num S016
	
	* Keep same set of variables as in Wave 6
	keep country cowcode wave year weights cntry_AN  A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 D054 D026_05 A006 C001_01 C001 C002_01 C002 D060 D081 D001_B G007_18_B G007_33_B G007_34_B G007_35_B G007_36_B A124_09 A124_02 A124_06 F028 F118 F119 F120 F121 F123 F122 F132 D059 D078   A124_03 A124_08   E035 E039  F114A F115 F116 F117 	X001 X003 S016 X025R
	
	* Count number of countries
	distinct country 
	
	* Append Wave 6 and Wave 7 datasets into one pooled file
	append using "$proc_data_path\wave 6 individual level.dta"
	
	* Fix missing country label for Maldives
	label values country S003
	label define S003 462 Maldives, modify 
	
	
	* Recode missing values consistently across all relevant variables
	mvdecode A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 D054 D026_05 A006 C001_01 C001 C002_01 C002 D060 D081 D001_B G007_18_B G007_33_B G007_34_B G007_35_B G007_36_B A124_09 A124_02 A124_06 F028 F118 F120 F121 F123 F122 F132 D059 D078   A124_03 A124_08  E035 E039 E041  F114A F115 F116 F117 F119	X001 X003 S016 X025R , mv(-1 -2 -3 -4 -5) 
 
	recode A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 D054 D026_05 A006 C001_01 C001 C002_01 C002 D060 D081 D001_B G007_18_B G007_33_B G007_34_B G007_35_B G007_36_B A124_09 A124_02 A124_06 F028 F118 F120 F121 F123 F122 F132 D059 D078   A124_03 A124_08   E035 E039 E041 F114A F115 F116 F117 F119	X001 X003 S016 X025R (.a .b .c .d .e = .)
 
	* Remove cases with inconsistent child quality selections (more than 5 or 0)
 	egen count_total = rowtotal (A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042)
	bysort country: tab count_total, missing
 
 foreach var of varlist A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 {
    gen `var'a = `var'
}

* Recode to missing separately for each wave for reporting
 foreach var of varlist A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 {
    replace `var' = . if count_total > 5 & wave == 7 
	replace `var' = . if count_total == 0  & wave == 7 
}

 foreach var of varlist A027 A029 A030 A032 A034 A035 A038 A039 A040 A041 A042 {
    replace `var' = . if count_total > 5 & wave == 6
	replace `var' = . if count_total == 0  & wave == 6
}


	* Recode and rename sociodemographic variables
	rename X001 female
	recode female 1=0 2=100 
	rename X003 age
	rename S016 language_interview
	rename X025R education_level
	
	* Log language breakdown by wave as output is too long
	log using my_logfile.log, text replace
	bysort country: tab language_interview if wave == 6
	bysort country: tab language_interview if wave == 7
	log close

	* Create binary dummies for education level
	gen educ_low = 0 
		replace educ_low = 100 if education_level == 1
	gen educ_mid = 0 
		replace educ_mid = 100 if education_level == 2
	gen educ_high = 0
		replace educ_high = 100 if education_level == 3
	
	
	
 
	* Recode the items for the I-C index to range from 0 (collectivist) to 100 (individualist)
	
* Childrearing for Self-Direction (vs. Obedience) Subindex
	recode A029 1 = 100
	rename A029 independence
	
	recode A034 1=100  
	rename A034 imagination

	recode A042 1=0 0=100 
	rename A042 obedience
	
	recode A040 1=0 0=100
	rename A040 faith
			
			* Versions for comparing with full sample (including invalid child quality responses)
			recode A029a 1 = 100
			rename A029a independencea
	
			recode A034a 1=100  
			rename A034a imaginationa

			recode A042a 1=0 0=100 
			rename A042a obediencea
	
			recode A040a 1=0 0=100
			rename A040a faitha
		
* Distancing from (vs. Prioritizing) Traditional Duties Subindex
	recode D054 1=0 2=33 3=66 4=100
	rename D054 parents_proud
	
	recode D026_05 1=0 2=25 3=50 4=75 5=100
	rename D026_05 parents_ill
	
	recode F028 6=5 7=6 8=7 // recoding as point 5 was missing in the original coding
	gen rel_service = (F028 - 1) * 100 / 6
	drop F028
	
	recode A006 4=100 3=66.6 2=33.3 1=0
	rename A006 rel_imp
	
	
	
*Egalitarian (vs. Discriminatory) Beliefs Subindex
	recode C001_01 1=0 2=25 3=50 4=75 5=100
	rename C001_01 gender_job_1
	
		*This variable is the same as the previous but has 3 categories instead of 5 and it has all countries 
		recode C001 1=0 2=100 3=50
		rename C001 gender_job_2 
	
		*Compute the mean between them because the second one is more refined but it misses a few countries (r at the individual level is .944)
	egen gender_job = rowmean(gender_job_1 gender_job_2)    // computing the average and filling in the missing values
	drop gender_job_1 gender_job_2
	
	recode C002 1=0 2=100 3=50
	rename C002 immig_jobs_1	
	
		*This variable is the same as the previous but has 3 categories instead of 5 and it has all countries 
		recode C002_01 1=0 2=25 3=50 4=75 5=100
		rename C002_01 immig_jobs_2 
	
		*Compute the mean between them because the second one is more refined but it misses a few countries(r at the individual level is .926)
	egen immig_jobs = rowmean(immig_jobs_1 immig_jobs_2)    // computing the average and filling in the missing values
	drop immig_jobs_1 immig_jobs_2
	
	recode D060 1=0 2=33 3=66 4=100
	rename D060 gender_edu		
	
	recode D081 5=0 4=25 3=50 2=75 1=100
	rename D081 parent_homosex
	
	recode D059 1=0 2=33 3=66 4=100
	rename D059 gender_pol
	
	recode D078 1=0 2=33 3=66 4=100
	rename D078 gender_business
	
*Equal (vs. Group-Based) Trust Subindex
	recode D001_B 1=100 2=66.6 3=33.3 4=0
	rename D001_B trust_family
	
	recode G007_18_B 1=100 2=66.6 3=33.3 4=0
	rename G007_18_B trust_neigh
	
	recode G007_33_B 1=100 2=66.6 3=33.3 4=0
	rename G007_33_B trust_persknow
	
	recode G007_34_B 1=100 2=66.6 3=33.3 4=0
	rename G007_34_B trust_persfirst
	
	recode G007_35_B 1=100 2=66.6 3=33.3 4=0
	rename G007_35_B trust_otherrelig
	
	recode G007_36_B 1=100 2=66.6 3=33.3 4=0
	rename G007_36_B trust_othernation
	 
	* Calculate standard deviation of trust across targets — high SD indicates stronger ingroup bias 
	egen nonmiss_count = rownonmiss(trust_family trust_neigh trust_persknow trust_persfirst trust_otherrelig trust_othernation)
	egen trust_SD = rowsd(trust_family trust_neigh trust_persknow trust_persfirst trust_otherrelig trust_othernation) if nonmiss_count == 6
	drop nonmiss_count
	
*Accepting (vs. Excluding) Diverse Others Subindex
	recode A124_09 0=100 1=0
	rename A124_09 neigh_homo	
	
	recode A124_02 0=100 1=0
	rename A124_02 neigh_race
	
	recode A124_06 0=100 1=0
	rename A124_06 neigh_immig					
					
					* two additional items retained for robustness/sensitivity checks
					recode A124_08  0=100 1=0
					rename A124_08  neigh_drugs
					
					recode A124_03 0=100 1=0
					rename A124_03 neigh_drunk
	    
*Accepting (vs. Condemning) Diverse Life Choices Subindex
	gen homosexuality = ((F118-1)/9)*100   
		drop F118
	gen abortion = ((F120-1)/9)*100
		drop F120
	gen divorce = ((F121-1)/9)*100
		drop F121
	gen suicide = ((F123-1)/9)*100   
		drop F123
	gen euthanasia = ((F122-1)/9)*100  
		drop F122
	gen cas_sex = ((F132-1)/9)*100  
		drop F132
		
			*one more item retained for robustness/sensitivity checks
			gen prostitution = ((F119-1)/9)*100  
			drop F119
		
		
	*items for discriminant validity	
		rename E035 income_differ
		gen compet_good = 11 - E039 // reverse coding so that higehr value mean stronger embrace of competition 
		gen zero_sum_WVS = 11 - E041  // reverse coding so that higehr value mean stronger zero sum beliefs	
	
	alpha F114A F115 F116 F117, gen (anomie_WVS)
	
	* Save the final prepared individual-level dataset for both waves
	save "$proc_data_path\WVS both waves individual level.dta", replace
	
	
	
	
	
	
	*************************************************************************************************
	********************* Study 2a: Building a New Individualism Index with WAVE 7 ******************
	*************************************************************************************************
	
	* Load processed individual-level dataset (both waves)
	use "$proc_data_path\WVS both waves individual level.dta", clear 
	
	* Keep only WVS Wave 7 data for index construction
	keep if wave == 7 
	
	* Create country-level sample size for aggregation
	bysort country: gen samplesize = _N
	
	* Keep only relevant variables 
	keep country samplesize wave cowcode year weights independence* imagination* obedience* faith* parents_ill parents_proud homosex abortion divorce euthanasia suicide cas_sex prostitution neigh_* faith rel_service rel_imp immig_jobs gender_edu gender_job trust_persfirst trust_otherrelig trust_othernation trust_family trust_neigh trust_persknow trust_SD parent_homosex gender*  income_differ compet_good zero_sum_WVS anomie_WVS  female age educ_low educ_mid educ_high
	
	* Calculate standard deviation of age within countries
	egen age_SD = sd(age), by(country)
		
	* Collapse (aggregate) to country-level means (weighted)
	collapse cowcode year rel_imp - samplesize age_SD [pw=weights], by (country)
	
	* Imputations of missing cases by regressions using the remaining items in the same subindex as predictors (for SOM Part D)
	reg homosexuality abortion divorce suicide euthanasia 
	predict homosex_imputed, xb
	replace homosexuality = homosex_imputed if missing(homosexuality)
	drop homosex_imputed
	
	reg cas_sex homosexuality abortion divorce suicide euthanasia 
	predict cas_sex_imputed, xb
	replace cas_sex = cas_sex_imputed if missing(cas_sex)
	drop cas_sex_imputed
	
	reg neigh_homo neigh_immig neigh_race 
	predict neigh_homo_imputed, xb
	replace neigh_homo = neigh_homo_imputed if missing(neigh_homo)
	drop neigh_homo_imputed
	
	reg parent_homosex gender_job gender_edu immig_jobs
	predict parent_homosex_imputed, xb
	replace parent_homosex = parent_homosex_imputed if missing(parent_homosex)
	drop parent_homosex_imputed
			
	
		**** Create the New Individualism-Collectivism Index with wave 7 
	* Childrearing for self-direction (vs. conformity)
	alpha independence imagination obedience faith, gen (childrearing7) casewise
	label variable childrearing7 "Childrearing for self-direction (vs. conformity)"
	
				* Check robustness with full sample (i.e., not excluding problematic cases)
				alpha independencea imaginationa obediencea faitha, gen (childrearing7a) casewise
						
						* Report comparison of the two versions
						corr childrearing7*
						sum childrearing7*
						drop childrearing7a
	
	* Distancing from (vs. Prioritizing) Traditional Duties
	alpha parents_proud rel_service rel_imp parents_ill, gen (obligations7) casewise
	label variable obligations7 "Distancing from (vs. Prioritizing) Traditional Duties"
				
				* alternative version - using the same items as in wave 6 
				alpha parents_proud rel_service rel_imp, gen (obligations7s) casewise
				label variable obligations7s "De-emphasising normative obligations - short version"
		
	* Egalitarian (vs. discriminatory) beliefs
	alpha gender_job immig_job gender_pol parent_homosex gender_business gender_edu, gen (equality7) casewise
	label variable equality7 "Egalitarian (vs. discriminatory) beliefs"
	
				* alternative - using the same items as in wave 6  
				alpha gender_job immig_job gender_pol gender_business gender_edu, gen (equality7s) casewise
				label variable equality7s "Egalitarian (vs. discriminatory) beliefs - short version"
				
* Equal (vs. group-based) trust
	* Invert trust_SD to align with I-C index (higher = more equal trust); 54.77 = theoretical max SD
	gen trustSD7 = 100 - ((trust_SD/54.77)*100)
	label variable trustSD7 "Equal (vs. group-based) trust"
	
		* alternative version 1: using only outgroup trust
			alpha trust_persfirst trust_otherrelig trust_othernation, gen (trust_alt7) casewise 
			label variable trust_alt7 "Outgroup trust"  
			
		* alternative version 2: using difference between in-group and out-group trust
			gen temp = ((trust_family + trust_neigh + trust_persknow)/3) - ((trust_persfirst + trust_otherrelig + trust_othernation)/3) 
			gen trust_2alt7 = 100 - temp
		
		* Compare trust measures
		corr trustSD7 trust_alt7 trust_2alt7 
			
	
* Accepting (vs. excluding) diverse others	
	alpha neigh_homo neigh_immig neigh_race, gen (neighbors7)
	label variable neighbors7 "Accepting (vs. excluding) diverse others"	
			
			* alternative version includes stigmatized groups
				alpha neigh_homo neigh_immig neigh_race neigh_drugs neigh_drunk, gen (neighbors7a)
				
				* Check correlation among these items
				corr neigh_homo neigh_immig neigh_race neigh_drugs neigh_drunk

* Accepting (vs. condemning) diverse life choices
	alpha homosex abortion divorce suicide euthanasia cas_sex, gen (permissivness7) casewise		
	label variable permissivness7 "Accepting (vs. condemning) diverse life choices"
	
				* alternative version - using the same items as in wave 6 
				alpha homosex abortion divorce suicide euthanasia, gen (permissivness7s) casewise		
				label variable permissivness7s "Accepting (vs. condemning) diverse life choices - short version"
				
				* Check correlation with the additional item
				corr homosex abortion divorce suicide euthanasia cas_sex prostitution
				
* New Individualism-Collectivism Index	
	alpha childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7, gen (individualism7) casewise
	label variable individualism7 "Individualism-Collectivism Index Wave 7"	
				
				* alternative version - using the same items for all subindices as in wave 6 
				alpha childrearing7 obligations7s equality7s trustSD7 neighbors7 permissivness7s, gen (individualism7s) casewise
				label variable individualism7 "Individualism-Collectivism Index Wave 7 - short version"	
	
	* Check summary statistics and correlations among subindices and the composite I-C index (Table 5)
	summ childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7 individualism7 
	corr childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7 individualism7 

	* Rename variables to indicate that they come from Wave 7 when merging with Wave 6 later
	foreach var of varlist rel_imp independence imagination faith obedience neigh_race neigh_immig neigh_homo parents_ill parents_proud gender_pol gender_edu gender_business parent_homosex rel_service gender_job immig_jobs trust_SD homosexuality abortion divorce suicide euthanasia cas_sex {
		gen `var'7 = `var'
		drop `var'
			}

			
	***** Evaluate the model with PCA
	pca childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7
		estat loadings, cnorm(eigen)
			screeplot
	
	***** Evaluate the model with Parallel analysis
		*PCA
		paran childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7, iterations(100) all graph 
		
		*EFA (Principal Factors)
		paran childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7, factor (ipf) iterations(100) all graph 
	
		
	** Exploratory factor analysis
	factor childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7, ipf 
		screeplot
		predict ic_factor_pf7
	
	corr individualism7 ic_factor_pf7

	
	* Confirmatory factor analysis 
sem (childrearing7 obligations7 equality7 trustSD7 neighbors7 permissivness7 <- f1), ///
  latent(f1) cov(e.childrearing7*e.obligations7) cov(e.equality7*e.neighbors7) standardized
	estat gof, stats(all)
	estat mindices

	*save aggregated dataset used for latter use
	save "$proc_data_path\WVS wave 7 aggregate level.dta", replace	
		

		
		
		
		
		
	*****************************************************************
	**** Study 2b: Replicating the New I-C Index using wave 6 data **
	*****************************************************************	
	
	* Load the individual-level dataset and filter for Wave 6 observations
	use "$proc_data_path\WVS both waves individual level.dta", clear 
	keep if wave == 6 
	
	label values country S003
		
	* Generate country-specific sample size for Wave 6
	bysort country: gen samplesize6 = _N
	
	* Keep only the necessary variables
	keep country wave year cowcode samplesize6 weights independence* imagination* obedience* faith* parents_proud homosex abortion divorce euthanasia suicide neigh_* faith rel_service rel_imp immig_jobs gender_edu gender_job trust_persfirst trust_otherrelig trust_othernation trust_family trust_neigh trust_persknow trust_SD  gender*  neigh_* income_differ compet_good zero_sum_WVS anomie_WVS female age edu*	
	
	* Compute standard deviation of age within each country (for descriptive analysis)
	egen age_SD = sd(age), by(country)
	
	* Collapse (aggregate) the dataset to the country level using population weights
	collapse cowcode year rel_imp - samplesize6 age_SD [pw=weights], by (country)	
	
	* Drop countries with excessive missingness on key components
	drop if country == 554 //  New Zealand: missing all 6 trust items
	drop if country == 818 // Egypt: missing all "neighbor" items and others

	* Impute missing values via regression-based prediction
	reg rel_service parents_proud rel_imp 
	predict rel_service_imputed, xb
	replace rel_service = rel_service_imputed if missing(rel_service)
	drop rel_service_imputed
	
	reg homosexuality abortion divorce suicide 
	predict homosex_imputed, xb
	replace homosexuality = homosex_imputed if missing(homosexuality)
	drop homosex_imputed
	
	reg euthanasia homosexuality abortion divorce suicide 
	predict euthanasia_imputed, xb
	replace euthanasia = euthanasia_imputed if missing(euthanasia)
	drop euthanasia_imputed
	
	reg neigh_homo neigh_immig neigh_race 
	predict neigh_homo_imputed, xb
	replace neigh_homo = neigh_homo_imputed if missing(neigh_homo)
	drop neigh_homo_imputed	
	
	reg immig_jobs gender_job gender_edu
	predict immig_jobs_imputed, xb
	replace immig_jobs = immig_jobs_imputed if missing(immig_jobs)
	drop immig_jobs_imputed		

		
		**** Create the New Individualism-Collectivism Index with wave 6 
* Childrearing for self-direction (vs. conformity)
	alpha independence imagination obedience faith, gen (childrearing6) casewise
	label variable childrearing6 "Childrearing for self-direction (vs. conformity)"
				
				*alternative without dropping inaccurate responses
				alpha independencea imaginationa obediencea faitha, gen (childrearing6a) casewise
						
						*reporting comparison 
						corr childrearing6*
						sum childrearing6*
						drop childrearing6a
	
* Distancing from (vs. Prioritizing) Traditional Duties
	alpha parents_proud rel_service rel_imp, gen (obligations6) casewise
	label variable obligations6 "Distancing from (vs. Prioritizing) Traditional Duties"
		
* Egalitarian (vs. discriminatory) beliefs
	alpha gender_job immig_job gender_pol gender_business gender_edu, gen (equality6) casewise
	label variable equality6 "Egalitarian (vs. discriminatory) beliefs"
					
* Equal (vs. group-based) trust
	gen trustSD6 = 100 - ((trust_SD/54.77)*100)
	label variable trustSD6 "Equal (vs. group-based) trust"
	
* Accepting (vs. excluding) diverse others	
	alpha neigh_homo neigh_immig neigh_race, gen (neighbors6)
	label variable neighbors6 "Accepting (vs. excluding) diverse others"			
			
					* extended version including more outgroups
					alpha neigh_*, gen (neighbors6long)
					label variable neighbors6 "Accepting (vs. excluding) diverse others - long version"	
	
* Accepting (vs. condemning) diverse life choices
	alpha homosex abortion divorce suicide euthanasia, gen (permissivness6) casewise		
	label variable permissivness6 "Accepting (vs. condemning) diverse life choices"
	
	* Final composite I-C index (Wave 6)
	alpha childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6, gen (individualism6) casewise
	label variable individualism6 "Individualism-Collectivism Index Wave 6"
	
	* Correlation table for summary (Table 7)
	corr  childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6 individualism6
	
	* Rename original variables to include "6" suffix for clarity in merged datasets
	foreach var of varlist rel_imp independence imagination faith obedience neigh_race neigh_immig neigh_homo parents_proud gender_pol gender_edu gender_business rel_service gender_job immig_jobs trust_SD homosexuality abortion divorce suicide euthanasia female age age_SD edu* year {
		gen `var'6 = `var'
		drop `var'
			}
			
			
		***** Evaluation of the I-C index structure using PCA
		pca childrearing obligations equality trustSD neighbors6 permissivness 
		estat loadings, cnorm(eigen)
			screeplot
			
		***** Evaluation with Parallel Analysis			
		* PCA
		paran childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6, iterations(100) all graph 
			
		*EFA (Principal Factors)
		paran childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6, factor (pf) iterations(100) all graph 
	
		
	** Exploratory factor analysis
	factor childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6, pf 
		screeplot
		predict ic_factor_pf6
	
	corr individualism6 ic_factor_pf6

	* Confirmatory factor analysis 
sem (childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6 <- f1), ///
  latent(f1) cov(e.childrearing6*e.obligations6) cov(e.equality6*e.neighbors6) standardized
  
	estat gof, stats(all)
	estat mindices
	
	
	* Sociodemographic variables check (Table 3)	 
	browse country year samplesize female age* edu*	
	
	* Save the final Wave 6 aggregate dataset 
	save "$proc_data_path\WVS wave 6 aggregate level.dta", replace	
	
	
		
	***** Test-Retest Analysis: Comparing Wave 6 and Wave 7 Country Scores *****
	
	* Load Wave 6 aggregate dataset
	use  "$proc_data_path\WVS wave 6 aggregate level.dta", replace	
	
	* Merge with Wave 7 aggregate dataset on country
	merge 1:1 country using "$proc_data_path\WVS wave 7 aggregate level.dta"	
		drop _merge

	* Reconcile sociodemographic variables: fill missing Wave 7 values with Wave 6, then drop Wave 6 originals
	foreach var in year samplesize age age_SD female educ_low educ_mid educ_high {
		replace `var' = `var'6 if `var' ==.
		drop `var'6
		}
	
	* Correlate the values for each subindex and the composite index between the two waves (Table 7)
	corr childrearing* 
	corr obligations* 
	corr equality*  
	corr trustSD*  
	corr neighbors* 
	corr permissivness*  
	corr individualism* 
	
	* Descriptive statistics for subindices and overall index for complete cases only (Table 7)
	sum childrearing* obligations* equality* trustSD*  neighbors* permissivness* individualism* if !missing(individualism6, individualism7) 

	* Paired t-tests to assess test–retest stability between waves (Table 7)
	ttest childrearing6 == childrearing7
	ttest obligations6 == obligations7
			ttest obligations6 == obligations7s // short version for wave 7 subindex
	ttest equality6 == equality7
			ttest equality6 == equality7s // short version for wave 7 subindex
	ttest trustSD6 == trustSD7 
	ttest neighbors6 == neighbors7 
	ttest permissivness6 == permissivness7 	
			ttest permissivness6 == permissivness7s // short version for wave 7 subindex
	ttest individualism6 == individualism7
			ttest individualism6 == individualism7s	 // short version for wave 7 subindex
				
				

				
				
				
				
				
	**********************************************************************
	***************  Study 2c: Mapping Global Variation  *****************
	**********************************************************************

	* Impute missing Wave 7 subindices from Wave 6 values
		
* Childrearing for self-direction (vs. obedience) 
	reg independence7 independence6 imagination6 obedience6 faith6
		predict independence7_imputed, xb
		replace independence7 = independence7_imputed if missing(independence7)
		drop independence7_imputed	
	
	reg imagination7 imagination6 independence6  obedience6 faith6
		predict imagination7_imputed, xb
		replace imagination7 = imagination7_imputed if missing(imagination7)
		drop imagination7_imputed		
		
	reg obedience7 independence6 imagination6 obedience6 faith6
		predict obedience7_imputed, xb
		replace obedience7 = obedience7_imputed if missing(obedience7)
		drop obedience7_imputed		
		
	reg faith7 faith6 independence6 imagination6 obedience6 
		predict faith7_imputed, xb
		replace faith7 = faith7_imputed if missing(faith7)
		drop faith7_imputed
	
* Distancing from (vs. prioritizing) traditional duties
	reg parents_proud7 parents_proud6 rel_service6 rel_imp6
		predict parents_proud7_imputed, xb
		replace parents_proud7 = parents_proud7_imputed if missing(parents_proud7)
		drop parents_proud7_imputed		
	
	reg parents_ill7 parents_proud6 rel_service6 rel_imp6
		predict parents_ill7_imputed, xb
		replace parents_ill7 = parents_ill7_imputed if missing(parents_ill7)
		drop parents_ill7_imputed		
		
	reg rel_service7 parents_proud6 rel_service6 rel_imp6
		predict rel_service7_imputed, xb
		replace rel_service7 = rel_service7_imputed if missing(rel_service7)
		drop rel_service7_imputed			
	
	reg rel_imp7 parents_proud6 rel_service6 rel_imp6
		predict rel_imp7_imputed, xb
		replace rel_imp7 = rel_imp7_imputed if missing(rel_imp7)
		drop rel_imp7_imputed		
		
* Egalitarian (vs. discriminatory) beliefs
	reg gender_job7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict gender_job7_imputed, xb
		replace gender_job7 = gender_job7_imputed if missing(gender_job7)
		drop gender_job7_imputed		
	
	reg immig_jobs7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict immig_jobs7_imputed, xb
		replace immig_jobs7 = immig_jobs7_imputed if missing(immig_jobs7)
		drop immig_jobs7_imputed		
	
	reg gender_edu7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict gender_edu7_imputed, xb
		replace gender_edu7 = gender_edu7_imputed if missing(gender_edu7)
		drop gender_edu7_imputed		
		
	reg gender_business7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict gender_business7_imputed, xb
		replace gender_business7 = gender_business7_imputed if missing(gender_business7)
		drop gender_business7_imputed		
		
	reg gender_pol7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict gender_pol7_imputed, xb
		replace gender_pol7 = gender_pol7_imputed if missing(gender_pol7)
		drop gender_pol7_imputed				
		
	reg parent_homosex7 gender_job6 immig_jobs6 gender_pol6 gender_business6 gender_edu6
		predict parent_homosex7_imputed, xb
		replace parent_homosex7 = parent_homosex7_imputed if missing(parent_homosex7)
		drop parent_homosex7_imputed				

* Equal (vs. group-based) trust
	reg trustSD7 trustSD6 
		predict trustSD7_imputed, xb
		replace trustSD7 = trustSD7_imputed if missing(trustSD7)
		drop trustSD7_imputed	

* Accepting (vs. excluding) diverse others	
	reg neigh_homo7 neigh_homo6 neigh_immig6 neigh_race6
		predict neigh_homo7_imputed, xb
		replace neigh_homo7 = neigh_homo7_imputed if missing(neigh_homo7)
		drop neigh_homo7_imputed	

	reg neigh_race7 neigh_homo6 neigh_immig6 neigh_race6
		predict neigh_race7_imputed, xb
		replace neigh_race7 = neigh_race7_imputed if missing(neigh_race7)
		drop neigh_race7_imputed		
		
	reg neigh_immig7 neigh_homo6 neigh_immig6 neigh_race6
		predict neigh_immig7_imputed, xb
		replace neigh_immig7 = neigh_immig7_imputed if missing(neigh_immig7)
		drop neigh_immig7_imputed	
					
* Accepting (vs. condemning) diverse life choices
	reg homosexuality7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict homosexuality7_imputed, xb
		replace homosexuality7 = homosexuality7_imputed if missing(homosexuality7)
		drop homosexuality7_imputed		
		
	reg abortion7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict abortion7_imputed, xb
		replace abortion7 = abortion7_imputed if missing(abortion7)
		drop abortion7_imputed				
		
	reg divorce7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict divorce7_imputed, xb
		replace divorce7 = divorce7_imputed if missing(divorce7)
		drop divorce7_imputed			
		
	reg suicide7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict suicide7_imputed, xb
		replace suicide7 = suicide7_imputed if missing(suicide7)
		drop suicide7_imputed			
		
	reg euthanasia7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict euthanasia7_imputed, xb
		replace euthanasia7 = euthanasia7_imputed if missing(euthanasia7)
		drop euthanasia7_imputed		

	reg cas_sex7 homosexuality6 abortion6 divorce6 suicide6 euthanasia6
		predict cas_sex7_imputed, xb
		replace cas_sex7 = cas_sex7_imputed if missing(cas_sex7)
		drop cas_sex7_imputed	
		
	drop childrearing6 obligations6 equality6 trustSD6 neighbors6 permissivness6 individualism6	
		
		
		 **** Creating the final new Individualism-Collectivism Index for 102 countries	
		 
* Childrearing for self-direction (vs. conformity)
	alpha independence7 imagination7 obedience7 faith7, gen (childrearing) casewise
	label variable childrearing "Childrearing for self-direction (vs. conformity)"
	
* Distancing from (vs. prioritizing) traditional duties
	alpha parents_proud7 parents_ill7 rel_service7 rel_imp7, gen (obligations) casewise
	label variable obligations "Distancing from (vs. prioritizing) traditional duties"
		
* Egalitarian (vs. discriminatory) beliefs
	alpha gender_job7 immig_jobs7 gender_pol7 parent_homosex7 gender_business7 gender_edu7, gen (equality) casewise
	label variable equality "Egalitarian (vs. discriminatory) beliefs"

* Equal (vs. group-based) trust
	gen trustSD = trustSD7
	label variable trustSD7 "Equal (vs. group-based) trust"
	
* Accepting (vs. excluding) diverse others	
	alpha neigh_homo7 neigh_immig7 neigh_race7, gen (neighbors)
	label variable neighbors "Accepting (vs. excluding) diverse others"	

* Accepting (vs. condemning) diverse life choices
	alpha homosexuality7 abortion7 divorce7 suicide7 euthanasia7 cas_sex7, gen (permissivness) casewise		
	label variable permissivness "Accepting (vs. condemning) diverse life choices"
	
*** Individualism-Collectivism Index
	alpha childrearing obligations equality trustSD neighbors permissivness, gen (individualism) casewise
	label variable individualism "Individualism-Collectivism Index 102 countries"	
	
	* Correlation matrices for subindices and composite index
	corr childrearing obligations equality trustSD neighbors permissivness individualism 		
    pwcorr childrearing obligations equality trustSD neighbors permissivness individualism, sig		

		
	*** Creating a table in the appendix to share all country-level I-C scores ***
		* Generate string version of country code to include the WVS country code
		tostring country, gen (countryn)
	
	* Creating a new categorical variable to arrange the countries alphabetically instead of by country number
	decode country, gen (countryname) 
	sort countryname
	
	* Export variables to Excel for Appendix Table A1
	export excel countryname cowcode countryn year samplesize childrearing obligations equality trustSD neighbors permissivness individualism using "$output_path\All country scores", firstrow(variables) replace	
	
	* Merge country-level I-C data with cultural zone classifications
	merge 1:1 country using "$raw_data_path\culture_zones" 
	keep if _merge == 3
	drop _merge B
	
	* Encode culture_zone as numeric for grouping
	encode culture_zone, gen(culture_zones)
	drop culture_zone
	
	* Export country rankings for mapping (Figure 7)
	destring countryn, replace
	export excel country countryn individualism using "$R_data_path\Data_IC_Country_Ranking", firstrow(variables) replace
	
	* Sociodemographics deails for Table 3
	sort country
	browse country year samplesize female age age_SD educ_low educ_mid educ_high culture_zones	
	
	* Save the fully merged aggregated dataset for future analyses
	save "$proc_data_path\WVS aggregated.dta", replace	
	
	* Compute and export mean subindices by culture zone for Figure 8
foreach var of varlist childrearing obligations equality trustSD neighbors permissivness individualism {
    bysort culture_zones: egen `var'_mean = mean(`var')
	}

	collapse childrearing_mean obligations_mean equality_mean trustSD_mean neighbors_mean permissivness_mean individualism_mean, by (culture_zones)
	
	export excel _all using "$R_data_path\IDV by culture zone.xlsx", firstrow(variables) replace
	
	
	
	
	
	
	
	
	
	
	*********************************************************************************************************
	*****************************  Study 2d: Validating the New I-C Index ***********************************
	*********************************************************************************************************
		
	* Load the aggregated I-C dataset
	use "$proc_data_path\WVS aggregated.dta", clear
	
	* Merge with data on other I-C dimensions
	merge 1:1 cowcode using "$proc_data_path\idv_dimensions_subset", force
	drop _merge
	
		* Rescale indices to 0–100 for comparability (i.e., Figure 10)
		gen individualism_minmax = ((individualism - 26.70072) / (75.65214 - 26.70072)) * 100
		gen idv_Hofs_minmax = ((idv_H - 6) / (91 - 6)) * 100
	
	* Merge with Beugelsdijk & Welzel's historical dataset
	merge 1:1 cowcode using "$raw_data_path\Historical data from Beugelsdijk and Welzel"
	drop _merge
	
	* Restrict to valid countries
	keep if country!=.

	* Merge with QoG data	
	merge 1:n country using "$raw_data_path\my_qog_std_ts_jan21"   
	drop if _merge==2 | _merge==1 
	drop _merge

	keep if year_QoG == 2015
		
	* Merge with other cultural and validity datasets
	merge 1:1 cowcode using "$raw_data_path\Other cultural variables.dta"  
	drop _merge
	
	merge 1:1 cowcode using "$raw_data_path\Discriminant validity.dta"
	drop _merge
	
	merge 1:1 cowcode using "$raw_data_path\Selfishness_cooperation.dta",  force 
	drop _merge	
	
	* Create regional group dummies
	gen EA_ES = 0
	replace EA_ES = 1 if culture_zones == 4 | country == 826 // English-speaking (including Great Britain)
	replace EA_ES = 2 if culture_zones == 1 & country != 496 // East Asia (excluding Mongolia)
		label define lEA_ES 0 "All countries" 1 "English-speaking" 2 "East Asia"
		label values EA_ES lEA_ES
	
	* Recoding variables
	gen gdppc_ln = ln(wdi_gdpcappppcur)		
	gen gdppc = wdi_gdpcapcon2010/1000
	gen education = (gea_ea2534f + gea_ea2534m + gea_ea3544f + gea_ea3544m + gea_ea4554f + gea_ea4554m + gea_ea5564f + gea_ea5564m + gea_ea65f + gea_ea65m)/10	// the values for different age groups and genders were averaged	
	gen wgi_average_r = .68 - wgi_average // reverse-code so that higher score = more selfishness
	gen average_cooperation_r = 83.22 -  average_cooperation // reverse-code so that higher score = less cooperation

	
	* Retain final variable selection for validation and regression analyses
	keep country individualism culture_zone idv_Hofs EA_ES gdppc wdi_empser education ffp_fsi ti_cpi van_index vdem_corr vdem_libdem wbgi_rle whr_hap wbgi_gee gggi_ggi wbgi_cce vdem_pubcorr undp_hdi gdppc_ln dr_ig egov_hci gii_gii hf_efiscore rsf_pfi spi_opp spi_ospi vdem_corr vdem_mecorrpt wdi_lifexp wdi_mortinf wbgi_vae hf_prights hf_business gpi_ss ffp_ued ffp_sl ffp_hr dr_ig coll_Pelham idv_globe idv_Inglehart autonomy_Schw idv_BW EVI idv_Minkov flexibility_MK flex cTw4 difusdist3 murschal7 FamTyp5n kinship_intensity rel_mob breakdown_social_fabric breakdown_leadership perception_anomie anomie_level zero_sum_belief average_cooperation_r income_differ compet_good zero_sum_WVS anomie_WVS wgi_average_r egoism_altruism self_vs_other_care
	 
	* Keep only valid cases 
	drop if country ==  .
	
	
	
			***************  Convergent Validity   **************
    * Restrict to countries with both Hofstede and new I-C scores
	foreach var of varlist idv_BW idv_Minkov idv_globe idv_Inglehart EVI autonomy_Schw {
   	pwcorr individualism idv_H `var' if idv_H !=., sig obs
	}
   
    * Full I-C sample: correlations without Hofstede filter
	foreach var of varlist idv_BW idv_Minkov idv_globe idv_Inglehart EVI autonomy_Schw {
   	pwcorr individualism `var', sig obs
    }
	* Note: Correlation tables were copied into Table 10 manually
  
  
  
     
			***************  Discriminant Validity   **************
   	  * Restrict to countries with both Hofstede and new I-C scores
	 foreach var of varlist wgi_average_r self_vs_other_care average_cooperation_r egoism_altruism anomie_WVS perception_anomie compet_good income_differ zero_sum_WVS zero_sum_belief flex rel_mob  {
   	pwcorr individualism idv_H `var' if idv_H !=., sig
	 }
   
     * Full I-C sample: correlations without Hofstede filter
     foreach var of varlist wgi_average_r self_vs_other_care average_cooperation_r egoism_altruism anomie_WVS perception_anomie compet_good income_differ zero_sum_WVS zero_sum_belief flex rel_mob  {
   	pwcorr individualism `var', sig
    }
	* Note: Correlation tables were copied into Table 10 manually
   
pwcorr wgi_average_r self_vs_other_care average_cooperation_r egoism_altruism anomie_WVS perception_anomie compet_good income_differ zero_sum_WVS zero_sum_belief flex rel_mob, sig obs
matrix R = r(C)  
	* The resulting correlation matrix (r(C)) was saved and formatted for SOM Table S6
  
  
  
  
			********* Nomological Validity *********
	* Correlates each predictor with both the new index (full sample)
   foreach var of varlist ti_cpi van_index vdem_corr vdem_libdem wbgi_rle wbgi_gee gggi_ggi undp_hdi gdppc_ln dr_ig egov_hci gii_gii hf_efiscore rsf_pfi spi_opp spi_ospi vdem_mecorrpt wdi_lifexp wdi_mortinf wdi_empser wbgi_vae hf_prights hf_business gpi_ss ffp_sl ffp_hr education coll_Pelham {
		corr `var' individualism
   }
   * Output was manually transferred to SOM Table S6
			
	* Correlates each predictor with both the new index and Hofstede's index (fixed samples)
   foreach var of varlist ti_cpi van_index vdem_corr vdem_libdem wbgi_rle wbgi_gee gggi_ggi undp_hdi gdppc_ln dr_ig egov_hci gii_gii hf_efiscore rsf_pfi spi_opp spi_ospi vdem_mecorrpt wdi_lifexp wdi_mortinf wdi_empser wbgi_vae hf_prights hf_business gpi_ss ffp_sl ffp_hr education coll_Pelham {
		corr `var' individualism idv_H
   }
   * Output was manually transferred to "Indicators for Individualism.xlsx" for visualization in R (Figure 9)
      
   

   
   
   
   


	*********************************************************************************************************
	************************  Study 2e: Comparing Predictive Models of I-C **********************************
	*********************************************************************************************************
	
	* Standardize each predictor (mean=0, SD=1) to obtain standardized coefficients in the regressions
	foreach var in individualism idv_Hofs gdppc education ffp_fsi wdi_empser cTw4 difusdist3 murschal7 FamTyp5n kinship_intensity {
	    egen z_`var' = std(`var') 
	}
	
	* Generate a sample indicator
	gen sample = !missing(idv_Hofs, individualism)
	
	***** Regressions with contemporary development indicators as predictors for Table 12 (unstandardized variables)	
		
		* Save list of predictors
	global predictors1 gdppc education ffp_fsi wdi_empser
	
	*** Model I
	reg idv_Hofs $predictors1 if sample==1
		est store pred_H
	
	reg individualism $predictors1 if sample==1
		est store pred_novel
	
	*** Model II including culture zones 
	reg idv_Hofs $predictors1 i.EA_ES if sample==1
		est store cultzone_H
	
	reg individualism $predictors1 i.EA_ES if sample==1 
		est store cultzone_novel
	
	* Repeat New I-C regressions on full sample 
	*** Model I
	reg individualism $predictors1 
		est store novel_f_sample
	
	*** Model II including culture zones 
	reg individualism $predictors1 i.EA_ES
		est store cultz_novel_f_sample
		
	esttab pred_H pred_novel novel_f_sample cultzone_H cultzone_novel cultz_novel_f_sample using "C:\Users\akali\Desktop\Hofstede Individualism\predictions2.rtf", b(2) se(2) r2(2) ar2(2)  label  replace  title(Table ?. Contemporary Predictors of Individualism2)  compress
	
				**** Contemporary Predictors of I-C – Standardized
				
				* Save list of standardized predictors
				global predictors1_z z_gdppc z_education z_ffp_fsi z_wdi_empser
				
				*** model I: 
				reg z_idv_Hofs $predictors1_z if sample==1
				reg z_individualism $predictors1_z if sample==1
	
				*** Model II including culture zones 
				reg z_idv_Hofs $predictors1_z i.EA_ES if sample==1
				reg z_individualism $predictors1_z i.EA_ES if sample==1 
	
				*same analyses but using the whole samples:
				*** model I: contemporary development factors
				reg z_individualism $predictors1_z
	
				*** Model II including culture zones 
				reg z_individualism $predictors1_z i.EA_ES
					*Note: manually copy and paste coeffiecients into Table 12 
		
	
	**** Historical predictors 
		* Save list of predictors
		global predictors2 cTw4 difusdist3 murschal7 FamTyp5n kinship_intensity
		*** Model I: 
		reg idv_Hofs $predictors2 if sample==1 
			est store hist_pred_H
			
		reg individualism $predictors2 if sample==1
			est store hist_pred_novel
		
		*** Model II with culture zones 
		reg idv_Hofs $predictors2 i.EA_ES if sample==1 
			est store hist_cultzone_H		
			
		reg individualism $predictors2 i.EA_ES if sample==1
			est store hist_cultzone_novel
		
		* Full sample
		reg individualism $predictors2
			est store hist_novel_f_sample
			
		reg individualism $predictors2 i.EA_ES
			est store culzone_hist_novel_f_sample
		
		* Export all unstandardized models	
		esttab hist_pred_H hist_pred_novel hist_novel_f_sample hist_cultzone_H hist_cultzone_novel culzone_hist_novel_f_sample using "C:\Users\akali\Desktop\Hofstede Individualism\historical predictors.rtf", b(2) se(2) r2(2) ar2(2)  label  replace  title(Table ?. Historical Predictors of Individualism)   compress	
	
			
				**** Historical Predictors of I-C – Standardized
			
				* Save list of standardized predictors
				global predictors2_z z_cTw4 z_difusdist3 z_murschal7 z_FamTyp5n z_kinship_intensity
				
				*** Model I: 
				reg z_idv_Hofs $predictors2_z if sample==1 
				reg z_individualism $predictors2_z if sample==1
		
				*** Model II with culture zones 
				reg z_idv_Hofs $predictors2_z i.EA_ES if sample==1 
				reg z_individualism $predictors2_z i.EA_ES if sample==1
		
				* Full sample
				reg z_individualism $predictors2_z
				reg z_individualism $predictors2_z i.EA_ES

	* Create a correlation matrix between predictors to include in SOM Table S6
	corr individualism idv_Hofs gdppc education ffp_fsi wdi_empser cTw4 difusdist3 murschal7 FamTyp5n kinship_intensity

***********************************************  END  ***************************************************
*********************************************************************************************************


	
	






