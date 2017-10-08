** Identifying Duplicated SNUs in the Fact View Dataset
** Aaron Chafetz
** Date: Jan 6, 2017
** Updated: Jan 24, 2017


/*
Notes
	- Source: ICPI Fact View dataset
*/

** IDENFITY DUPLICATES **

*open dataset
	use "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_PSNU_20161230_v2_2.dta"

*remove extraneous variables
	keep operatingunit snu1uid snu1 psnu psnuuid

*collapse dataset to just have unique list of OU, PSNU, and UID
	gen n = 1
	collapse (max) n, by(operatingunit snu1uid snu1 psnu psnuuid)
	
*flag any repeated PSNU names with different UIDs
	gen loc = snu1 + "/" + psnu
	sort operatingunit loc
	gen flag = 1 if loc==loc[_n-1] | loc==loc[_n+1]
	
*view/export list
	sort operatingunit psnu
	br operatingunit snu1 psnu psnuuid if flag ==1 
	



