**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: develop age groupings for the Highlander Script
**   Date: October 19, 2016
**   Updated: 10/24

/* NOTES
	- Looking across just HTC_TST, CARE_NEW, TX_NEW, and TX_CURR
	- https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_01_import.md
*/
********************************************************************************

*check to see if crosswalk talble exists
capture confirm file "$output\temp_agegpcw.dta"
	*if dta file does not exist, create it
	if _rc{
	
	*create highlander age groups crosswalk
		input str6 (age hs_agegp hs_type) //crosswalk table
			"<01" "<15" "Finer"
			"01-14" "<15" "Finer"
			"<15" "<15" "Coarse"
			"01-04" "<15" "Finer"
			"05-09" "<15" "Finer"
			"05-14" "<15" "Finer"
			"10-14" "<15" "Finer"
			"15+" "15+" "Coarse"
			"15-19" "15+" "Finer"
			"20+" "15+" "Finer"
			"20-24" "15+" "Finer"
			"25-49" "15+" "Finer"
			"50+" "15+" "Finer"
			end
	*label variables
		lab var age "Ages (Finer/Coarse) entered in DATIM"
		lab var hs_agegp "Age group for purposes of the Highlander Script"
		lab var hs_type "Finer v Coarse for Highlander Script"
	*save
		save "$output\temp_agegpcw.dta"
	}
	*end
