* Create dataset for TX_CURR in Tanzania
* Aaron Chafetz
* Date: 10/25/16

*set directory
	cd "C:\Users\achafetz\Downloads\" //change

	
** FACILITY AND COMMUNITY NAMES **
* source - DATIM

*import community and facility names, saving output as .dta
	foreach t in community facility {
	import excel "TNZnames.xlsx", sheet("`t'") clear firstrow
		save "tnz_`t'_names"
		}
		*end

		
** SITE LEVEL DATASET **
* source - ICPI Site by IM Level Fact View (10/10/16)

*import site level data for TNZ
	import delimited "Site_IM_20161010_Tanzania\Site_IM_20161010_Tanzania\Site_IM_20161010_Tanzania.txt", clear

*subset to just TX_CURR data
	keep if indicator=="TX_CURR"

*merge 
	foreach t in community facility{
		*merge facility and community names into dataset, dropping unmatched names from names file
		merge m:1 `t'uid using "tnz_`t'_names", nogen keep(match master)
		*order the names before the uid
		order `t'_name, before(`t'uid)
		}
		*end
		
*export
	export delimited using "tnz_namedsites"


