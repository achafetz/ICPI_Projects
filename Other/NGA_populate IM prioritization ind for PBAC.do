**   Nigeria PBAC IM/Prioritization/Indicator Schema
**   COP FY17
**   Aaron Chafetz
**   Purpose: create a base look up list to use in their adapted PBAC
**   Date: May 1, 2017
**   Updated:

********************************************************************************


clear 

gen mechid = ""
gen prioritization = ""
gen ind = ""

foreach mech in "14505" "14664" "16797" "17728" "17729" "17735" "17737" "17747" ///
	"18010" "18075" "18441" "18655" "18657" "18615" "18656"{
	
	foreach ptype in "ScaleUp Sat" "ScaleUp Agg" "Sustained" "Sustained Com" ///
	"Attained" "Mil" "KP CBCTS"{
		
		foreach x in "Patient Year TX_CURR (1<x<15)" "Patient Year TX_CURR (>15)" ///
		"PMTCT_ART" "PMTCT_EID" "PMTCT_STAT" "HTC PITC" "HTC CBCT" "HTC KP" ///
		"OVC_SERV" "KP_PREV_PWID" "KP_PREV_MSMTG" "KP_PREV_FSW" "CBCTS" ///
		"KP CBCTS"{
		
			set obs `=_N+1'
			replace mechid = "`mech'" in `=_N'
			replace prioritization = "`ptype'" in `=_N'
			replace ind = "`x'" in `=_N'
		
			}
		}
	}
		*end
		
	br


