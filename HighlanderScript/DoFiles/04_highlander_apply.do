**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: apply Highlander choice by Site/IM/Indicator/IndicatorType/Period
**   Date: October 19, 2016
**   Updated: 10/24

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
	- note: https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_04_apply.md
*/
********************************************************************************

*reopen original site file 
	use "$output/temp_orig_site_${ctry}", clear

* remove any sites with no data for all quarters 
	egen rowtot = rowtotal (fy*)
	drop if rowtot==0
	drop rowtot

* reshape long so fiscal years are in rows
	*create id for reshape
		gen id = _n
	*rename fiscal years to (1) have common stub and (2) retain their name in reshape 
		drop *apr //fy2016_target
		ds fy*
		foreach yr in `r(varlist)'{
			rename `yr' y`yr'
			}
			*end
*reshape
	reshape long y@, i(id) j(pd, string)	
	drop id //needed for reshape	
	
*merge keeping only those that are a match and in the master
	merge m:1 pd psnuuid fcm_uid indicator indicatortype mechanismid ///
		using "$output\temp_hs_choice_${ctry}", nogen keep(match master)

*fill missing (id will be missing if any one cell in the row is blank)
	ds, has(type string)
	foreach v in `r(varlist)'{
		qui: replace `v' = "na" if `v'=="" | `v'=="NULL"
		}
		*end

*remove space in name for reshape
	replace hs_type = "TotNum" if hs_type=="Total Numerator"

*reshape
	egen id = group(pd psnuuid fcm_uid mechanismid indicator indicatortype ///
		primepartner implementingmechanismname disaggregate age sex result ///
		otherdisaggregate status typecommunity)	
	reshape wide y, i(id) j(hs_type, string)
	drop id 
* clean up names, removing y and making lower case
	ds y*
	foreach x in `r(varlist)'{
		rename `x' `=lower("`=subinstr("`x'","y","",.)'")'
		}
		*end
	replace hs_choice = 99 if hs_choice==.
		lab def hs_choice 99 "n/a", modify
*drop blank rows
	egen rowtot = rowtotal (coarse finer results totnum)
	drop if rowtot==0
	drop rowtot

*highlander value based on hs_choice
	gen hs_val =.
		replace hs_val=finer if inlist(hs_choice, 1,4,6)
		replace hs_val=coarse if inlist(hs_choice, 2,5,7)
		replace hs_val=finer + coarse if hs_choice==3
		replace hs_val=results if hs_choice==8
		replace hs_val=totnum if hs_choice==9

*drop 
	drop coarse-totnum
	
*reshape
	egen id = group(psnuuid fcm_uid implementingmechanismname mechanismid ///
		primepartner indicator indicatortype disaggregate age sex result ///
		otherdisaggregate status hs_choice typecommunity)
	reshape wide hs_val, i(id) j(pd, string)
	drop id
*remove hs_val from variable names so back to fy2015q2, fy2015q3, etc
	ds hs_val*
	foreach x in `r(varlist)'{
		rename `x' `=subinstr("`x'","hs_val","",.)'
		}
		*end

*drop blank rows
	egen rowtot = rowtotal (fy*)
	drop if rowtot==0
	drop rowtot
*remove na
	ds, has(type string)
		foreach v in `r(varlist)'{
			qui: replace `v' = "" if `v'=="na"
			}
			*end
*reorder
	drop fcm_uid status
	order fy*, after(hs_choice)

*add apr figure
	*some countries missing quarters (eg Caribbean)
	foreach i of numlist 2/4{
		capture confirm variable fy2015q`i'
		if _rc qui: gen fy2015q`i'= .
		}
		*end
		order fy2015q2 fy2015q3 fy2015q4, after(hs_choice)
	*create apr variable
	egen fy2015apr = rowtotal(fy2015q2 fy2015q3 fy2015q4)
	*use only q2 and q4 for TX_CURR
	egen fy2015apr_tx = rowtotal(fy2015q2 fy2015q4) ///
		if indicator=="TX_CURR" & fy2015q3!=.
	replace fy2015apr = fy2015apr_tx if fy2015apr_tx!=.
		drop fy2015apr_tx
		order fy2015apr, after(fy2015q4)

*highlander flag (add to all to identify after merging)
	gen highlander = "Y"
	order highlander, before(fy2015q2)

*collapse to psnu level
	collapse (sum) fy*, by(ïregion-implementingmechanismname indicator-highlander)
	
*save
	save "$output/temp_hs_psnu_${ctry}", replace
