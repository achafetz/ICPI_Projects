/*
*import Fact View Q1 IM/PSNU dataset
	import delimited "C:\Users\achafetz\Documents\ICPI\Data\ICPI_FactView_PSNU_IM_20170215_v1_1.txt", clear
	keep if operatingunit=="Nigeria"
	save "C:\Users\achafetz\Downloads\temp_Nigeria_fy17q1.dta", replace
*/

*use Nigeria dataset, keeping only HTC Pos
use "C:\Users\achafetz\Downloads\temp_Nigeria_fy17q1.dta", clear
keep if inlist(indicator, "HTS_TST_POS", "HTS_TST_NEG")


* create age groupings as seperate file 
	preserve
	clear
	input str20 age str20 age2
		"15-19"	"15-24"
		"20-24"	"15-24"
		"25-49"	"25+"
		"50+"	"25+"
	end
	tempfile temp_agegr
	save "`temp_agegr'"
	restore
	
*merge age groups into master
	merge m:1 age using "`temp_agegr'", nogen

*sex
	gen sex2 = "M" if sex == "Male"
	replace sex2 = "F" if sex=="Female"
	
*grouping
	gen grp = age2 + " " + sex2 if age2!="" & sex2!=""

*sdp
	clonevar sdp = disaggregate
	split sdp, p("/")
	keep if inlist(sdp1, "Index", "Inpat", "TBClinic", "VMMC", "OtherPITC") | ///
		inlist(sdp1, "HomeMod", "IndexMod", "MobileMod", "OtherMod", "VCT", ///
		"VCTMod")

*collapse
	drop if grp==""
	collapse (sum) fy2017q1, by(psnu sdp1 indicator grp)

*create total and replace NEG with total values for reshape
	egen tot = total(fy2017q1), by(psnu grp sdp1)
	replace fy2017q1 = tot if indicator=="HTS_TST_NEG"
	replace sdp1 = sdp1 + "_tot" if indicator=="HTS_TST_NEG"
	drop indicator tot
	
*reshape
	reshape wide fy2017q1, i(psnu grp) j(sdp1, string)
	
*clean
	ds fy2017q1*
	foreach x in `r(varlist)'{
		rename `x' `=lower(subinstr("`x'", "fy2017q1","", .))'
		}
		*end
	order psnu grp index inpat tbclinic vmmc otherpitc homemod indexmod ///
		mobilemod othermod vct vctmod index_tot inpat_tot tbclinic_tot ///
		vmmc_tot otherpitc_tot homemod_tot indexmod_tot	mobilemod_tot ///
		othermod_tot vct_tot vctmod_tot
		
*make sure every district has all age/sex groups
	preserve
	gen n = 1
	collapse n, by(psnu)
	drop n
	expand 5
	bysort psnu: gen n = _n
	gen grp = "15-24 F" if n==1
		replace grp = "25+ F" if n==2
		replace grp = "15-24 M" if n==3
		replace grp = "25+ M" if n==4
		replace grp = "HOLD" if n==5
		drop n
	sort psnu grp
	tempfile temp_grps
	save "`temp_grps'"
	restore	
	merge 1:1 psnu grp using "`temp_grps'", nogen
	sort psnu grp
	
*create proportion
	egen dtot = rowtotal(index- vctmod)
	foreach x of varlist index- vctmod{
		gen p_`x' = `x' / dtot
		}
		*end
*create yield
	foreach x of varlist index- vctmod{
		gen y_`x' = `x' / `x'_tot
		}
		*end
		
*gen tot age dist by psnu
	*egen psnu_dtot = total(dtot) if grp!="HOLD", by(psnu)
	*gen  p_psnugrpshare= dtot / psnu_dtot
	
*clean
	keep psnu grp p_* y_*
	gen artgap =.
		order psnu grp artgap p_psnugrpshare
	gen space =.
		order space, before(y_index)
	replace psnu="" if grp=="HOLD"
	replace grp="" if grp=="HOLD"

export excel using "C:/Users/achafetz/Documents/GitHub/ICPI/Other/HTC_Disagg_Allocation_NGA.xlsx", ///
	cell("B4") sheetmodify firstrow(variables)

	
	

	
	
