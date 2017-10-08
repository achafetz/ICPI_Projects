**   Highlander Script
**   COP FY17
**   Aaron Chafetz
**   Purpose: remove temporary files
**   Date: October 23, 2016
**   Updated: 

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
*/
********************************************************************************

	*remove temporary files
		*identify files to delete
		fs "$output/temp_hs_*.dta"
		*loop over files
		foreach f in `r(files)'{
			erase "$output/`f'"
			}
