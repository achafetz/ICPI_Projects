** District proportion of PLHIV
** Aaron Chafetz
** Date: Feb 14, 2017

cd "C:\Users\achafetz\Downloads\"

*import data
	import excel "extract 3.xlsx", firstrow case(lower) clear

*split age & sex into seperate columns
	split configuration, p(";")

* create age groupings as seperate file 
	preserve
	clear
	input str5 configuration1 str5 age	
		"0-4 "	"0to14"
		"5-9 "	"0to14"
		"10-14"	"0to14"
		"15-19"	"15to24"
		"20-24"	"15to24"
		"25-29"	"o25"
		"30-34"	"o25"
		"35-39"	"o25"
		"40-44"	"o25"
		"45-49"	"o25"
		"50-54"	"o25"
		"55-59"	"o25"
		"60-64"	"o25"
		"65-69"	"o25"
		"70-74"	"o25"
		"75-79"	"o25"
		"80+"	"o25"
	end
	tempfile temp_agegr
	save "`temp_agegr'"
	restore
	
*merge age groups into master
	merge m:1 configuration1 using "`temp_agegr'", nogen
	
*create new sex labels
	gen grp = strtrim(lower(configuration2)) + "_" + age

*collapse by district
	collapse (sum) plhiv_2016, by(subnationalregion grp)

*reshape
	reshape wide plhiv_2016@, i(subnationalregion) j(grp, string)
	ds plhiv*
	foreach x in `r(varlist)' {
		rename `x' `=subinstr("`x'", "plhiv_2016","", .)'
		}
		*end

*create district total
	egen dtot = rowtotal(female* male*)

*create proportion
	ds female* male*
	foreach x in `r(varlist)'{
		gen p_`x' = `x' / dtot
		}
		*end

*drop excess variables
	drop female* male* dtot
