**   EA-SAPR Tool
**   COP FY16
**   Aaron Chafetz
**   Purpose: generate output for Excel tool for OUs to check
**				MER data reported by IM
**   Date: Aug 15, 2016
**   Updated: 10/6/17

/* NOTES
	- Data source: ICPIFactView - OU_IM_20160801 [ICPI Data Store]
	- Report aggregates DSD and TA
	- Creates a SAPR result (Q1+Q2 or Q2 depending on indicator)
*/
********************************************************************************

*import OU IM data
	import delimited "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_OU_IM_20170815_v1_1.txt", clear 

*keep only top level numerators & KP disaggs
	keep if disaggregate=="Total Numerator" & inlist(indicator, "TX_CURR", ///
		"VMMC_CIRC", "HTS_TST", "OVC_SERV", "PP_PREV") | ///
		(indicator=="KP_PREV" & disaggregate=="KeyPop")

*create SAPR variable to sum up necessary variables
	egen fy2017sap = rowtotal(fy2017q1 fy2017q2)
		replace fy2017sapr = fy2017q2 if inlist(indicator, "TX_CURR", ///
			"OVC_SERV")
		replace fy2017sapr =. if fy2017sapr==0 //should be missing
		
	collapse (sum) fy2017sapr, by(operatingunit, countryname, mechanismid, ///
		primepartner, fundingagency, implementingmechanismname, indicator, ///
		numeratordenom, disaggregate, otherdisaggregate)
	
*export
	export delimited using "SAPR_FVdata_FY17Q3v1_1", nolabel replace dataf
