**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: pull HTC_TST fine & coarse age/sex/result data to compare to
**				Imran's output for the highlander script for select OUs
**   Date: October 21, 2016
**   Updated:

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20161010 [ICPI Data Store]
*/
********************************************************************************


*set directories
	global source "C:\Users\achafetz\Documents\ICPI\Data"
	global save  "C:\Users\achafetz\Documents\ICPI\Peds"
*set date of frozen instance - needs to be changed w/ updated data
	local datestamp "20161010"
*import frozen instance for ACT Monitoring Tool
	capture confirm file "$source\ICPIFactView_SNUbyIM`datestamp'.dta"
		if !_rc{
			di "Use Existing Dataset"
			use "$source\ICPIFactView_SNUbyIM`datestamp'.dta", clear
		}
		else{
			di "Import Dataset"
			import delimited "$source\ICPI_Fact_View_PSNU_IM_`datestamp'.txt", clear
			save "$source\ICPIFactView_SNUbyIM`datestamp'.dta", replace
		}
	*end
	
keep if indicator=="HTC_TST" & inlist(disaggregate, "Age/Sex Aggregated/Result", ///
	"Age/Sex/Result", "Results", "Total Numerator") & inlist(operatingunit, "South Africa", "Tanzania", ///
	"Ukraine", "Nigeria")

gen fy2016cum = fy2016q1 + fy2016q2 +  fy2016q3

gen hsa = "<15" if inlist(age, "<01", "01-04", "05-09", "10-14", "<15")
	replace hsa = "15+" if hsa=="" & inlist(age, "15+","15-19", "20-24", ///
		"25-49", "50+")
gen hst = "Finer" if inlist(disaggregate, "Age/Sex", "Age/Sex/Result")
	replace hst = "Coarse" if inlist(disaggregate, "Age/Sex Aggregated/Result", ///
		"Age/Sex Aggregated", "Age/Sex, Aggregated")
	replace hst = "TotNum" if disaggregate=="Total Numerator"
	replace hst = "Result" if disaggregate=="Results"

collapse (sum) fy2016cum, by(operatingunit hsa hst sex result)

*reshape wide fy2016cum, i(operatingunit sex resultstatus hsa) j(hst, string)
