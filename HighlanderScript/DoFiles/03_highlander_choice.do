**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: develop a model of the Highlander Script
**   Date: October 19, 2016
**   Updated: 10/24

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
	- notes - https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/Notes_03_choice.md
*/
********************************************************************************

* open data
	use  "$output/temp_orig_site_${ctry}", replace //run 02_highlander_import first
	* only HTC_TST, TX_NEW, and TX_CURR

* collapse so one observation per site (removed fundingagency)
	collapse (sum) fy*, by(operatingunit psnu psnuuid snuprioritization ///
		fcm_uid indicatortype indicator hs_type mechanismid) 

*drop dedups (apply decision at after with merge)
	drop if inlist(mechanismid, 0, 1)
	
* remove any sites with no data for all quarters 
	drop *apr //fy2016_targets
		/*want to just look at quarterly data; apr should be recalculatedneed 
			need to think about if targets should/shouldn't be determined by 
			results selection*/
	egen rowtot = rowtotal (fy*)
	drop if rowtot==0
	drop rowtot

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
	egen rowtot = rowtotal(coarse-totnum)
	drop if rowtot==0
	drop row*

/*
            Highlander denominaor
	|--------|----------------------------------------|
	| Option | Completeness Denominiator              |
	|--------|----------------------------------------|
	| 1      | Total Numerator used                   |
	| 2      | Result used (HTC) - complete           |
	| 3      | Result used (HTC) - no total numerator |
	| 4      | Total Numerator used (HTC)             |
	| 5      | No Total Numerator                     |
	|--------|----------------------------------------|
*/
* deterime which denominator to used to calc completeness 
	*upper and lower bounds are set in _runall
	gen r_pct = results/totnum //determine completeness of result
	gen hs_denom_desc=.
		lab var hs_denom_desc "Type of Numerator for Highlander Script"
		lab def hs_denom_desc 1 "Total Numerator used" 2 "Result used (HTC)" ///
			3 "Result used (HTC) - no total numerator" ///
			4 "Total Numerator used (HTC)" 5 "No Total Numerator"
		lab val hs_denom_desc hs_denom_desc
		replace hs_denom_desc = 1 if !inlist(totnum,0,.) & indicator!="HTC_TST" 
		replace hs_denom_desc = 2 if !inlist(result,0,.) & indicator=="HTC_TST" ///
			& (r_pct>=$lb & r_pct<=$ub)
		replace hs_denom_desc = 3 if inlist(totnum,0,.) & indicator=="HTC_TST"  ///
			& !inlist(result,0,.)
		replace hs_denom_desc = 4 if !inlist(totnum,0,.) & indicator=="HTC_TST" ///
			& (r_pct<$lb | r_pct>$ub)
		replace hs_denom_desc = 5 if inlist(totnum,0,.) & hs_denom_desc==.

* create denominator based on description
	gen hs_denom = totnum if inlist(hs_denom_desc, 1, 4)
		replace hs_denom = result if inlist(hs_denom_desc, 2, 3)
		lab var hs_denom "Highlander Denominator"
		
* create Highlander indicators used for making selection
	foreach v in finer coarse results totnum{
		capture confirm variable `v'
		if _rc qui: gen `v'=.
		}
	order coarse finer results totnum , before(operatingunit)
	gen f_pct = finer/hs_denom
	gen c_pct = coarse/hs_denom
	gen fc_pct= (finer + coarse)/hs_denom
	gen f_prox = abs(1-f_pct)
	gen c_prox = abs(1-c_pct)
	
/*
		Highlander Script Result
	|-------|------------------------------|
	| Order | Highlander Result            |
	|-------|------------------------------|
	| 1     | Fine (complete)              |
	| 2     | Coarse (complete)            |
	| 3     | Fine + Coarse (complete)     |
	| 4     | Fine (incomplete)            |
	| 5     | Coarse (incomplete)          |
	| 6     | Fine (max, no num)           |
	| 7     | Coarse (max, no num)         |
	| 8     | Result (no fine or coarse)   |
	| 9     | Total Numerator (no disaggs) |
	|-------|------------------------------|
*/
*determine highlander script choice by above ordering
	di "       Disagg is complete if: " $lb*100 "% <= numerator <= " $ub*100 "%"
	gen hs_choice = .
		lab var hs_choice "Highlander Disagg Choice"
		lab def hs_choice 1 "Fine (complete)" 2 "Coarse (complete)" ///
			3 "Fine + Coarse (complete)" 4 "Fine (incomplete)" ///
			5 "Coarse (incomplete)" 6 "Fine (max, no num)" ///
			7 "Coarse (max, no num)" 8 "Result (no fine/coarse)" ///
			9 "Total Num (no disaggs)"
		lab val hs_choice hs_choice
		replace hs_choice = 1 if (f_pct>=$lb & f_pct<=$ub)
		replace hs_choice = 2 if (c_pct>=$lb & c_pct<=$ub) & hs_choice==.
		replace hs_choice = 3 if (fc_pct>=$lb & fc_pct<=$ub) & hs_choice==.
		replace hs_choice = 4 if f_prox <= c_prox & !inlist(f_prox,0,.) & !inlist(finer,0,.) & hs_choice==.
		replace hs_choice = 5 if (c_prox < f_prox) & !inlist(c_prox,0,.) & !inlist(coarse,0,.) & hs_choice==.
		replace hs_choice = 6 if ((finer >= coarse) & !inlist(finer,0,.) | (!inlist(finer,0,.) & inlist(coarse,0,.))) & hs_choice==.
		replace hs_choice = 7 if ((coarse > finer) & !inlist(coarse,0,.) | (!inlist(coarse,0,.) & inlist(finer,0,.))) & hs_choice==.
		replace hs_choice = 8 if !inlist(result,0,.) & hs_choice==.
		replace hs_choice = 9 if !inlist(totnum,0,.) & hs_choice==.

* keep only variables pertinent to site information for merge
	drop coarse-totnum r_pct hs_denom-c_prox hs_denom_desc

* sort for merging
	sort psnuuid fcm_uid indicator indicatortype 
* reorder
	order operatingunit psnu psnuuid snuprioritization mechanismid fcm_uid indicator indicatortype pd hs_choice

* save for merging
	save "$output\temp_hs_choice_${ctry}", replace

*******************************************************************************
