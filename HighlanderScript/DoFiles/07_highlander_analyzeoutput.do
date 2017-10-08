**   Highlander Script
**   COP FY17
**   Aaron Chafetz
**   Purpose: summary output tables
**   Date: October 25, 2016
**   Updated: 

/* NOTES
	- Data source: ICPI_Fact_View_Site_IM_20160915 [ICPI Data Store]
*/
********************************************************************************


* Raw number of Sites by IMs and Indicator Type

	*open data
		use "$output/hs_choice_ALL", clear

	*frequency of each Highlander Script selection
		tab operatingunit hs_choice, row
		bysort indicatortype: tab operatingunit hs_choice, row

		
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
	* Are there sites with different Highlander selections?
		
		tab hs_choice, gen(hs_)
		qui: collapse (max) hs_1-hs_9, by(operatingunit psnuuid fcm_uid indicatortype indicator pd psnu snuprioritization)
		qui: egen hs_choice_count = rowtotal(hs_1-hs_9)
		tab hs_choice_count
		tab operatingunit hs_choice_count, row
		
		

	

	*export
	*set today's date for saving
		global date = subinstr("`c(current_date)'", " ", "", .)
		use "$output/hs_choice_ALL", clear
		export delimited using "$excel\HS_Choice_${date}", ///
				nolabel replace dataf
		use "$output/hs_psnu_ALL", clear
		export delimited using "$excel\HS_Values_PSNU_Agg_${date}", ///
				nolabel replace dataf
		