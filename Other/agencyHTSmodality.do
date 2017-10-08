**   HTS Modalities Distribution by Agency
**   FY17
**   Aaron Chafetz
**   Purpose: view HTS data by modality and agency
**   Date: Aug 1, 2016
**   Updated: 8/2

*setup
	global fvdata "C:/Users/achafetz/Documents/ICPI/Data"
	global store "C:/Users/achafetz/Downloads"
	global output "C:/Users/achafetz/Documents/GitHub/PartnerProgress/StataOutput"

*import OU by IM Fact View
	use "$fvdata/ICPI_FactView_OU_IM_20170702_v2_1.dta", clear

*subset to just modality data (HTS, HTS_NEG, HTS_POS)
	keep if indicator if strpos(indicator, "HTS") & ///
		standardizeddisaggregate=="Modality/MostCompleteAgeDisagg"
	
* merge in offical names from FACTS INFo 
	*IM names & partner names can vary within a mech id
	merge m:1 mechanismid using "$output/officialnames.dta", ///
		update replace nogen keep(1 3 4 5) //keep all but non match from using
			
*collapse so only one obs per 
	collapse fy2017q*, by(operatingunit primepartner fundingagency ///
		mechanismid implementingmechanismname indicator modality)

*create a cumulative variable
	egen fy2017cum = rowtotal(fy2017q*)
	
*reshape to calculate positivity
	reshape wide fy2017*, i(operatingunit primepartner fundingagency ///
		mechanismid implementingmechanismname modality) j(indicator, string)
		
*positivity
	recode *HTS_TST_* (. = 0)	
	ds *HTS_TST
	foreach x in `r(varlist)' {
		egen `x'_TOT = rowtotal(`x'_NEG `x'_POS)
		gen `x'_YIELD = `x'_POS/`x'_TOT
		}
		*end

*reshape long
	reshape long
	order indicator, before(modality)
	drop if inlist(fy2017cum, 0, .)
	
*export to Excel to use in pivot table
	export excel using "$store/HTSmodalitites.xlsx", firstrow(variables) replace
