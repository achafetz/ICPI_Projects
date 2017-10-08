** Identifying Duplicated SNUs in the Fact View Dataset
** Aaron Chafetz
** Date: Jan 6, 2017
** Updated: 10/7/2017


/*
Notes
	- Source: ICPI Fact View PSNU dataset
*/

** IDENFITY DUPLICATES **

*open dataset
	import delimited "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_PSNU_20170922_v2_1.txt", clear

*remove extraneous variables
	keep operatingunit snu1 psnu psnuuid fy2017cum fy2017_targets

*collapse dataset to just have unique list of OU, PSNU, and UID
	egen fy2017cum = rowtotal(fy2017q1 fy2017q3)
	collapse fy*, by(operatingunit snu1 psnu psnuuid)
	
*flag any repeated PSNU names with different UIDs
	duplicates report operatingunit snu1 psnu //see how many duplicates exist
	duplicates tag operatingunit snu1 psnu, gen(dup) //flag duplicates
	
*view/export list
	br if dup==1
	



