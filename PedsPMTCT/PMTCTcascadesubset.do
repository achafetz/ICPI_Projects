**   PMTCT Cascade Data			
**   COP FY16			
**   Aaron Chafetz			
**   Purpose: generate data subset for use in PMTCT cascade			
**   Date: August 22, 2016			
**   Updated: 1/13/2017		
			
/* NOTES			
	- Data source: ICPIFactView - PSNU by IM Level [ICPI Data Store]			
*/			
********************************************************************************			
			
*set directories
	global source "C:\Users\achafetz\Documents\ICPI\Data"
	global save  "C:\Users\achafetz\Documents\ICPI\Peds\PMTCT Cascade"
	
*set date of frozen instance - needs to be changed w/ updated data
	global datestamp "20161230_v2_2"
	
*import frozen instance for PMTCT Cascade Tool
	capture confirm file "$source\ICPI_FactView_PSNU_IM_${datestamp}.dta"
		if !_rc{
			di "Use Existing Dataset"
			use "$source\ICPI_FactView_PSNU_IM_${datestamp}.dta", clear
		}
		else{
			di "Import Dataset"
			import delimited "$source\ICPI_Fact_View_PSNU_IM_${datestamp}.txt", clear
			save "$source\ICPI_FactView_PSNU_IM_${datestamp}.dta", replace
		}
	*end

*drop unused prioritization
	drop fy17snuprioritization
*create SAPR variable to sum up necessary variables
	egen fy2016sapr = rowtotal(fy2016q1 fy2016q2)		
		replace fy2016sapr = fy2016q2 if inlist(indicator, "TX_CURR", ///	
			"OVC_SERV", "PMTCT_ARV", "KP_PREV", "PP_PREV", "CARE_CURR")
		replace fy2016sapr =. if fy2016sapr==0 //should be missing	
			
*create cumulative variable to sum up necessary variables thru Q3			
	egen fy2016cum = rowtotal(fy2016q1 fy2016q2 fy2016q3)		
		replace fy2016cum = fy2016q2 if inlist(indicator, "TX_CURR", ///	
			"OVC_SERV", "PMTCT_ARV", "KP_PREV", "PP_PREV", "CARE_CURR")
		replace fy2016cum =. if fy2016cum==0 //should be missing	
			
*replcae fy2016q3 w/ fy2016 cumulative			
	replace fy2016q3 = fy2016cum 		
	drop fy2016cum	
			
* delete extrainous vars/obs			
	keep if inlist(indicator, "PMTCT_STAT", "PMTCT_ARV", "PMTCT_EID", ///		
		"PMTCT_EID_2MO", "PMTCT_EID_POS_2MO", "PMTCT_EID_POS_12MO", ///
		"PMTCT_ARV", "PMTCT_FO") | ///		
		(indicator=="TX_NEW" &  disaggregate=="Age/Sex" & age=="<01")		
	rename ïregion region		
	rename fy2015q2 fy2015sapr
	
	local varlist region operatingunit countryname  ///	
		snu1 psnu fy16snuprioritization ///
		fundingagency primepartner mechanismid implementingmechanismname ///	
		indicator disaggregate age otherdisaggregate numeratordenom ///	
		fy2015sapr fy2015apr fy2016_targets fy2016q1 fy2016sapr fy2016q3 ///
		fy2016apr
	
	keep `varlist'
	order `varlist'	
			
*export			
	export delimited using "$save\PMTCTdata", nolabel replace dataf		
