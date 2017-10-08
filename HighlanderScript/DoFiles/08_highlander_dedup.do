**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: 
**   Date: October 19, 2016
**   Updated: 10/24

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
	- notes - https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_03_choice.md
*/
********************************************************************************
foreach ou of global ctrylist{
		global ctry `ou'

* open data
	use  "$output/temp_orig_site_${ctry}", replace //run 02_highlander_import first
	* only HTC_TST, TX_NEW, and TX_CURR

* collapse so one observation per site (removed fundingagency)
	collapse (sum) fy*, by(operatingunit psnu psnuuid snuprioritization ///
		fcm_uid indicatortype indicator hs_type mechanismid) 

*drop dedups (apply decision at after with merge)
	keep if inlist(mechanismid, 0, 1)
	sum mechanismid
	di "`r(N)'"
		if `r(N)'==0  exit
	
	
* remove any sites with no data for all quarters 
	drop *apr //fy2016_targets
		/*want to just look at quarterly data; apr should be recalculatedneed 
			need to think about if targets should/shouldn't be determined by 
			results selection*/
	egen rowtot = rowtotal (fy*)
	drop if rowtot==0
	drop rowtot
	sum mechanismid
	di "`r(N)'"
		if `r(N)'==0  exit

* reshape long so fiscal years are in rows
	*rename fiscal years to (1) have common stub and (2) retain their name after the reshape 
		ds fy*
		foreach yr in `r(varlist)'{
			rename `yr' y`yr'
			}
			*end
	*reshape, where i identifies the variables that make each obs unique and j is the new variabel
		reshape long y@, i(psnuuid fcm_uid mechanismid indicatortype ///
			indicator hs_type) j(pd, string)	

*reshape wide, adding Highlander types as columns for doing analysis
	*remove space in name for reshape (otherwise it gets dropped in reshape)
		replace hs_type = "TotNum" if hs_type=="Total Numerator"

	* reshape
		reshape wide y, i(psnuuid fcm_uid mechanismid indicatortype ///
			indicator pd) j(hs_type, string)
	* clean up names, removing y and making lower case
		ds y*
		foreach x in `r(varlist)'{
			rename `x' `=lower("`=subinstr("`x'","y","",.)'")'
			}
			*end

*drop if no data in row	(different for HTC because using result)
	foreach x in coarse finer results totnum{
		capture confirm variable `x'
		if _rc qui: gen `x'= .
		}
		*end
		order coarse finer results totnum, after(pd)
	egen rowtot = rowtotal(coarse-totnum)
	drop if rowtot==0
	drop row*

* save for merging
	save "$output\temp_hs_dedup_${ctry}", replace
}
*******************************************************************************

*append dedup file
	clear
	fs "$output/temp_hs_dedup_*.dta"
	foreach f in `r(files)' {
		qui: append using "$output/`f'", force
	}
	save "$output/hs_dedup_ALL", replace
	
	
*open data
	use "$output/hs_choice_ALL", clear
* agg to fine v coarse
	gen hs_choice_agg = "Finer" if  inlist(hs_choice, 1, 4, 6)
		replace hs_choice_agg = "Coarse" if inlist(hs_choice, 2, 5, 7)
		replace hs_choice_agg = "F+C" if hs_choice==3
		replace hs_choice_agg = "Result" if hs_choice==8
		replace hs_choice_agg = "TotNum" if hs_choice==9
	
	tab hs_choice_agg, gen(hs_agg_)
	qui: collapse (max) hs_agg_1-hs_agg_5, by(operatingunit psnuuid fcm_uid indicatortype indicator pd psnu snuprioritization)
	qui: egen hs_agg_choice_count = rowtotal(hs_agg_1-hs_agg_5)
	tab hs_agg_choice_count
	tab operatingunit hs_agg_choice_count, row
	keep if hs_agg_choice_count>1
	drop hs_agg_choice_count
	save "$output/temp_multi_choice", replace
	
	use "$output/hs_dedup_ALL", clear
	merge m:1 operatingunit psnuuid fcm_uid indicator indicatortype pd ///
		using "$output/temp_multi_choice"
	
	keep if _merge==3
	drop _merge
		
	order pd operatingunit psnu psnuuid snuprioritization fcm_uid indicatortype indicator
	sort pd operatingunit psnu psnuuid snuprioritization fcm_uid indicatortype indicator
	save "$output/temp_dedup_multi_choice", replace
	drop mechanismid mechanismid coarse finer results totnum hs_agg_1 hs_agg_2 hs_agg_3 hs_agg_4 hs_agg_5
	gen flag_site = 1
	save "$output/temp_dedup_multi_sites_list", replace
	
	use "$output/hs_psnu_ALL", clear
	merge m:1 age using  "$output\temp_agegpcw.dta", nogen ///
			keep(match master) //noreport
	replace hs_type = "Coarse" if inlist(indicator, "TX_CURR", "TX_NEW") & ///
			inlist(disaggregate, "Age/Sex Aggregated", "Age/Sex, Aggregated") ///
			& age=="<01"
	replace hs_type = "Total Numerator" if disaggregate=="Total Numerator"
	replace hs_type = "Results" if disaggregate== "Results" 
	*rename reshape 
		ds fy*
		foreach yr in `r(varlist)'{
			rename `yr' y`yr'
			}
			*end
	reshape long fy, 
	
	merge m:1 pd operatingunit psnu psnuuid snuprioritization fcm_uid ///
		indicatortype indicator using "$output/temp_dedup_multi_sites_list"
