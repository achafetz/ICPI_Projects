**   Q2 Performance Report
**   COP FY17
**   Aaron Chafetz
**   Purpose: create dataset for cumulative results for key indicators that will
**		be evaluated during the COP meetings
**   Date: April 6, 2017
**   Updated: 4/13/17

*import data
	cd C:\Users\achafetz\Documents\ICPI\Data\
	*import delimited "ICPI_FactView_PSNU_IM_20170324_v2_2", clear
	use "ICPI_FactView_PSNU_IM_20170324_v2_2.dta", clear
*subset to 3 key indicators
	keep if inlist(indicator, "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", ///
		"VMMC_CIRC") & disaggregate=="Total Numerator"
		
*aggregate to IM level
	collapse (sum) fy2015q2-fy2017q1, by(operatingunit fundingagency ///
		mechanismid fy17snuprioritization indicator)
		
*create cumulative results
	gen fy2015q2_cum = fy2015q2
	gen fy2015q3_cum = fy2015q2 + fy2015q3 if indicator!="TX_CURR"
	gen fy2015q4_cum = fy2015apr
	gen fy2016q1_cum = fy2016q1 if indicator!="TX_CURR"
		replace fy2016q1_cum = fy2015apr + ((fy2016q2-fy2015apr)/2)  if indicator=="TX_CURR"
	gen fy2016q2_cum = fy2016q1_cum + fy2016q2
	gen fy2016q3_cum = fy2016q2_cum + fy2016q3 if indicator!="TX_CURR"
	gen fy2016q4_cum = fy2016apr
	gen fy2017q1_cum = fy2017q1

/*
*projected Q2
	gen fy2017q2_cum_proj = fy2017q1_cum*(1+((fy2016q2_cum-fy2016q1_cum)/fy2016q1_cum))
	replace fy2016q1_cum = . if indicator=="TX_CURR"  
*/	
*recode 0 as missing
	recode fy20* (0 = .)
*add label for missing
	replace fy17snuprioritization="[not classified]" if fy17snuprioritization=="" 
*copy dataset to excel
	br
