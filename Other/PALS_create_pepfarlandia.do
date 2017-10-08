**   PEPFAR Applied Learning Summit - Data Viz Data Sets
**   FY17
**   Aaron Chafetz
**   Purpose: created "anonymized" datasets
**   Date: Aug 15, 2017
**   Updated: 8/18/17

*** high level dataset for handout ***

*import
	use "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_PSNU_IM_20170702_v2_1.dta", clear	

*subset to just one OU and key indicators
	keep if operatingunit=="Cote d'Ivoire" & inlist(indicator, "HTS_TST", ///
		"HTS_TST_POS", "OVC_SERV", "PMTCT_STAT", "TX_CURR", "TX_NEW", "VMMC_CIRC") ///
		& disaggregate=="Total Numerator"

*simplify snu prioritizations	
	gen snuprioritization = 2	
		replace snuprioritization = 1 if strpos(fy17snuprioritization,"Scale")>0
		
		lab def sc 1 "Scale-Up" 2 "Other"
		lab val snuprioritization sc
*aggregate up to indicator and prioritization level	
	collapse (sum) fy2016_targets-fy2016q4 fy2017_targets-fy2017q2, by(indicator snuprioritization)	

*reorder variables	
	order fy2016_targets, after(fy2016q4)	
	order fy2017_targets, after(fy2017q2)	

*round data for ease of use
	ds fy*	
	foreach fy in `r(varlist)'{	
		replace `fy' = round(`fy', 100)
		}
		*end

		
*** limited factview output for Excel use ***

*import
	use "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_PSNU_IM_20170702_v2_1.dta", clear	

* subset to key OU and indicators
	keep if operatingunit=="Cote d'Ivoire" & ///
		inlist(indicator, "HTS_TST_POS", "TX_NEW") & ///
		inlist(standardizeddisaggregate, "Modality/MostCompleteAgeDisagg", ///
			"MostCompleteAgeDisagg", "Total Numerator")

*mask psnus (using snu1 to reduce psnu count)	
	*new list of psnus and snu1s
	preserve
	clear
	input str42 snu1 str42 snu1_new str42 psnu_new
		"Abidjan 1-Grands Ponts" "North" "Hogsfeet"
		"Abidjan 2" "North" "Banrockburn"
		"Agneby-Tiassa-Me" "North" "Lundy"
		"Belier" "North" "Ballater"
		"Bounkani-gontougo" "North" "Murkwell"
		"Cavally-Guemon" "North" "Luton"
		"Gbeke" "South" "Stratford"
		"Gbokle-Nawa-San Pedro" "South" "Thralkeld"
		"Goh" "South" "Swindmore"
		"Hambol" "South" "Fallkirk"
		"Haut-Sassandra" "South" "Skargness"
		"Indenie-Djuablin" "South" "Coalfell"
		"Kabadougou-Bafing-Folon" "East" "Solaris"
		"Loh-Djiboua" "East" "Briar Glen"
		"Marahoue" "East" "Lullin "
		"N'zi-ifou" "East" "Meteli"
		"Poro-Tchologo-Bagoue" "East" "Landow"
		"Sud-Comoe" "East" "Tardide"
		"Tonkpi" "West" "Sharpton"
		"Worodougou-Bere" "West" "Rivermouth"
		"_Military Cote d'Ivoire" "_Military" "_Military"
	end
	
	*sort for merge
	sort snu1
	
	*save
	tempfile temp_snu
	save "`temp_snu'"
	restore
	
	*merge
	sort snu1
	merge m:1 snu1 using "`temp_snu'", nogen
	
	
*mask mechanisms
	*new mechanisms
	preserve
	clear
	input int mechanismid int mechanismid_new str120 primepartner_new str267 implementingmechanismname_new
		0 0 "Dedup" "Dedup"
		1 1 "Dedup" "Dedup"
		10276 20234 "TLASH" "Soar"
		12557 20235 "SDWEA" "Moxie System"
		12631 20236 "CSBSQ" "GistPro"
		12679 20237 "QHJPS" "Visage-2"
		12803 20238 "KQJXE" "Starfish"
		13046 20239 "TRNDF" "Mini"
		13272 20240 "PZNVP" "Paragon"
		13525 20241 "SZZSA" "Lambda"
		13539 20242 "WJNHT" "Nemo"
		13561 20243 "YHPNV" "ImpEdge"
		13616 20244 "JHCSV" "Vela 2"
		13624 20245 "EBNIR" "Onion"
		13631 20246 "WCAUJ" "Chronos"
		13651 20247 "PZNVP" "Draco"
		16685 20248 "CAHXS" "Spearpoint"
		17494 20249 "TLASH" "Gamma"
		17496 20250 "SDWEA" "Eggshell"
		17515 20251 "QIESD" "Spiral"
		17610 20252 "ZKCAI" "Delta"
		17763 20253 "RTNBO" "Orion"
		17918 20254 "XDSRZ" "Theta"
		17942 20255 "CAHXS" "Halo"
		18287 20256 "TBD" "Blueberry"
		18288 20257 "TBD" "Omicron"
		18289 20258 "TBD" "Droplet"
		18290 20259 "TBD" "Lyra 1"
		18291 20260 "TBD" "Lyra 2"
		18292 20261 "TBD" "Lyra 3"
		18293 20262 "TBD" "Lyra 4"
		18296 20263 "QHJPS" "Iris"
		18595 20264 "TBD" "Corona"
		18596 20265 "TBD" "Wave"
		18597 20266 "TBD" "Fisheye"
		18606 20267 "TBD" "Butterfly"
		end
	
	*sort for merge
	sort mechanismid
	
	*save
	tempfile temp_mech
	save "`temp_mech'"
	restore
	
	*merge
	sort mechanismid
	merge m:1 mechanismid using "`temp_mech'", nogen

	
*simplify snu prioritizations	
	gen fy17snuprioritization_new = "Scale-Up" if strpos(fy17snuprioritization,"Scale")>0
		replace fy17snuprioritization_new = "Other" if fy17snuprioritization_new!="Scale-Up"
		
*replace original variables with masked ones
	foreach n in snu1 psnu mechanismid implementingmechanismname primepartner fy17snuprioritization{
		replace `n' = `n'_new
		drop `n'_new
		}
		*end
		
*rename country
	replace operatingunit = "PEPFARlandia"

*clear region and uids for deidentification
	foreach i in ïregion regionuid operatingunituid countryname snu1uid psnuuid mechanismuid fy16snuprioritization{
		replace `i' = ""
		}
		*end

*rename region
	rename ïregion region
	
*remove blank rows
	egen row_tot = rowtotal(fy2016* fy2017*)
		drop if row_tot==0
		drop row_tot
		
*export file for use
	export delimited ///
		"C:\Users\achafetz\Documents\ICPI\PALS\FactView_PEPFARlandia_fy17q2.txt", ///
		nolabel replace dataf delimiter(tab)
	
