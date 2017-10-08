**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: create a dataset
**   Date: October 19, 2016
**   Updated: 10/24

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
	- note: https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_05_appendctrys.md
*/
********************************************************************************

*append all ou files together for PSNU and site/IM level choices
	foreach t in choice psnu{
		clear
		fs "$output/temp_hs_`t'_*.dta"
		foreach f in `r(files)' {
			qui: append using "$output/`f'", force
		}
		save "$output/hs_`t'_ALL", replace
	}
	*end
	
*append composite file to PSNU by IM Fact View dataset
	use "$fvdata/ICPIFactView_SNUbyIM20160909", clear
	qui: append using "$output/hs_psnu_ALL"
	order hs_agegp hs_choice highlander, before(fy2015q2)
	save "$fvdata/ICPIFactView_SNUbyIM_Highlander_20160909", replace
