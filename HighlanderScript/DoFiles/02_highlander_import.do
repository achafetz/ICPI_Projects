**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: import site level Fact View dataset for each OU
**   Date: October 21, 2016
**   Updated: 10/26

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
	- See notes - https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_02_import.md
*/
********************************************************************************


*check to see if site level dta file exists
	capture confirm file "$output/temp_orig_site_${ctry}.dta"
	*if not file exists, import it
	if _rc{
		di in yellow "`=upper(${ctry})': import site level data"
		
		*import
		qui: import delimited ///
			"$fvdata/ALL Site Dataset ${datestamp}/site_im_${datestamp}_${ctry}.txt", ///
			clear
			
		*keep only HTC_TST, TX_NEW, and TX_CURR
		qui: keep if (indicator=="HTC_TST" & ///
			inlist(disaggregate, "Age/Sex/Result", ///
				"Age/Sex Aggregated/Result", "Results", "Total Numerator")) | ///
			///(indicator=="CARE_NEW" & inlist(disaggregate, "Age/Sex", ///
				///"Age/Sex Aggregated", "Total Numerator")) | ///
			(indicator=="TX_CURR" & inlist(disaggregate,"Age/Sex", ///
				"Age/Sex Aggregated", "Age/Sex, Aggregated", "Total Numerator")) | ///
			(indicator=="TX_NEW" & inlist(disaggregate, "Age/Sex", ///
				"Age/Sex Aggregated", "Total Numerator"))
		
		* merge in aggregated Highlander Script age groups & edit type
		qui: merge m:1 age using  "$output\temp_agegpcw.dta", nogen ///
			keep(match master) noreport
		qui: replace hs_type = "Coarse" if inlist(indicator, "TX_CURR", "TX_NEW") & ///
			inlist(disaggregate, "Age/Sex Aggregated", "Age/Sex, Aggregated") ///
			& age=="<01"
		qui: replace hs_type = "Total Numerator" if disaggregate=="Total Numerator"
		qui: replace hs_type = "Results" if disaggregate== "Results" 
		
		* create a unique id by type (facility, community, military)
			* demarcated by f_, c_, and m_ at front
			* military doesn't have a unique id so script uses mechanism uid
		qui: tostring type*, replace //in OUs with no data, . is recorded and 
			* seen as numeric, so need to first string variables 
		qui: gen fcm_uid = ""
			replace fcm_uid = "f_" + facilityuid if facilityuid!=""
			replace fcm_uid = "c_" + communityuid if facilityuid=="" &  ///
				(typecommunity =="Y" | communityuid!="") & typemilitary!="Y"
			replace fcm_uid = "m_" + mechanismuid if typemilitary=="Y" 
		
		qui: save  "$output/temp_orig_site_${ctry}", replace
		clear
	}
	else{
		di in yellow"${ctry}: site level dta file exists"
	}
	*end
	
