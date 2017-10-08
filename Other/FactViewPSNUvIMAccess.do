***************************************************************************************
**	Factview dataset modification
**	Joshua Davis & Aaron Chafetz
**	Purpose: break out PSNU and PSNU x IM Factview datasets 1) by OU and 2) by agency
**	Date: 9.22.16
**	Updated: 10.7.16

/*	Notes: 
	- uses the PSNU x IM Factview dataset
	- The code below will bring the data into Stata and create OU-level files 
		and agency-level files
	- running this code requires Stata software and should be open in 
		the do file editor in Stata
	- items showing up as green are text/notes; all commands are colored 
		blue, black and or red (in the do file editor)
	- to execute a line or series of lines, highlight the selection and 
		hit the execute button at the top (or Ctrl + D). Some lines much be 
		executes at the same time and are noted as such. 
*/


////////////////////////////////////////////////////////////////////////////////

// SETUP //

**	Idenfity folder path
	* Copy the file path to where the datasets (PSNU by IM) are located
	* note: the path should be in quotes
	cd "C:\Users\PEPFAR1\Documents\ICPI\data" //USER - change file path (red text)

** Identify date of dataset (at end of file name)
	*replace date yyyymmdd with the date of the Factview dataset
	*note: date should be in quotes
	global datestamp "20160909" //USER - change date (red text)

** Import dataset
	* file should be stored in file path listed above
	* note: user doesn't need to update anything here
	import delimited "ICPI_Fact_View_PSNU_IM_$datestamp.txt", clear

	
////////////////////////////////////////////////////////////////////////////////

// SUBSET DATASET //

/* Examples are listed below and user should select the subsets relevant to 
	their work. Not all subsets should or need to be run.*/

** Agency Specific Subsetting
	* if you are interested in the data for just one or a handful of agencies
	* note: Agencies should be listed in quotes and divided by a comma
	* Agencies - DOD, Dedup, HHS/CDC, HHS/HRSA, HSS/NIH, PC, State/AF, State/PRM, USAID
	keep if inlist(fundingagency, "HSS/CDC", "USAID") //USER - change operating unit(s)
	
** OU Specific Subsetting
	* if you are interested in the data for just one or a handful of operating units
	* note: OUs should be listed in quotes and divided by a comma
	keep if inlist(operatingunit, "Kenya", "South Africa") //USER - change operating unit(s)

** Indicator Specific Subsetting
	* if you are interested in a particular or handful of indicators
	* note: should be listed in quotes and divided by a comma
	keep if inlist(indicator, "TX_CURR", "TX_NEW") //USER - change indicator(s)

** Disagg Specific Subsetting
	* if you are interested in a particular or handful of disaggs
	/* example for one set: keep data if indicator is HTC_TST and disaggs are Total numerator or Results
		OR if indicator is TX_CURR and */
	keep if (indicator == "HTC_TST" & inlist(disaggregate, "Total Numerator", "Results")
	
	/* example for one set: keep data if indicator is HTC_TST and disaggs are Total numerator or Results
		OR if indicator is TX_CURR and disaggregate is total numerator */
	keep if (indicator == "HTC_TST" & inlist(disaggregate, "Total Numerator", "Results") | (indicator=="TX_CURR" & inlist(disaggregate, "Total Numerator")

////////////////////////////////////////////////////////////////////////////////

// EXPORT DATASET //
	
/* Examples are listed below and user should select the export option relevant 
	to their work. */

** Export data to csv file
	*note: data file will be saved to the directory/file path listed above
	export delimited using "ICPIFactView_PSNUbyIM_$datestamp_subset", nolabel replace dataf

** Export seperate files for each OU 
	*set up to loop through all countries in dataset
	*note: highlight all lines in section and then execute all at the same time
	qui:levelsof operatingunit, local(levels)
	foreach ou of local levels {
		preserve
		di "export dataset: `ou' "
		qui:keep if operatingunit=="`ou'"
		qui: export delimited using "ICPIFactView_PSNUbyIM_$datestamp_`ou'", nolabel replace dataf
		restore
		}
		*end

** Export seperate files for each Agency
	*set up to loop through all agencies in dataset
	*note: highlight all lines in section and then execute all at the same time
	qui:levelsof fundingagency, local(levels)
	foreach agency of local levels {
		preserve
		di "export dataset: `agency' "
		qui:keep if fundingagency== "`agency'"
		qui: export delimited using "ICPIFactView_PSNUbyIM_$datestamp_`=subinstr("`agency'","/","_",1)'", nolabel replace dataf
		restore
		}
		*end
