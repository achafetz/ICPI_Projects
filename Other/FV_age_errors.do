**  Identify Age Errors within Fact View dataset
**     mismatch - finer age containing <15/15+ and PMTCT_EID containing 0-12m
**  Aaron Chafetz 
**  Date: Nov 1, 2016


global output "C:\Users\achafetz\Documents\GitHub\PartnerProgress/StataOutput"

*list of problematic IMs
	use "$output\ICPIFactView_SNUbyIM20161010.dta", clear
	keep if inlist(disaggregate, "Age/Sex", "Age/Sex/Result", "InfantTest") ///
		& inlist(age,"<15","15+","[months] 00-12")

*disaggregate totals for 
	use "$output\ICPIFactView_SNUbyIM20161010.dta", clear	
	keep if (operatingunit=="Kenya" & indicator=="HTC_TST" & disaggregate=="Age/Sex/Result") | ///
		(operatingunit=="Haiti" & inlist(indicator, "TX_CURR", "TX_NEW") & disaggregate=="Age/Sex") | ///
		(operatingunit== "Mozambique" & disaggregate=="InfantTest")
	collapse (sum) fy*, by(operatingunit indicator disaggregate)
	drop fy2015apr fy2016_targets
