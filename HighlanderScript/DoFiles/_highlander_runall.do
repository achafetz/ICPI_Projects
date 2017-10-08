**   Highlander Script
**   COP FY16
**   Aaron Chafetz
**   Purpose: run through all do files
**   Date: October 22, 2016
**   Updated: 10/25

/* NOTES

	*** When running for the first time: ***
	- Run the 00_initialize do file first to set up folder structure and  
		all their paths that do files run rely on
	- Adjust set directory (a) in this file and projectpath 00_initialize to
		match your folder path
	- Adjust fvdata folder in 00_initialize - PEPFAR data is large and store
		locally in a central location for easy of access for other projects; 
		add folder containing all OUs ICPI Fact View Site by IM datasets
		and ICPI Fact View PSNU by IM (if desired, this can be added to the 
		RawData folder of this project)
	- Adjust the PSNU by IM name and/or location in 05_appendctrys
	- Adjust the datestamp (c) below to reflect the file date on the Site
		by IM folder and files
	- Adjust the upper and lower bounds for completeness if desired
*/
********************************************************************************


** RUN ALL DO FILES FOR HIGHLANDER SCRIPT **

** GLOBAL VARIABLES **

	* a: set directory
		cd "C:/Users/achafetz/Documents/GitHub/ICPI/HighlanderScript/DoFiles/"
		
	* b: list of countries to run the highlander script on
		global ctrylist angola asiaregional botswana burma burundi cambodia ///
			cameroon caribbeanregion centralamerica centralasia civ ///
			dominicanrepublic drc ethiopia ghana guyana haiti india indonesia ///
			kenya lesotho malawi mozambique namibia nigeria png rwanda ///
			southafrica southsudan swaziland tanzania uganda ukraine ///
			vietnam zambia zimbabwe
				
	* c: datestamp for latest site file
		global datestamp "20160915"
		
	* d: set upper and lower bounds for completeness - 95-101%
		global lb .95   //lower bound
		global ub 1.01  //upper bound
		di "   Disagg is complete if: " $lb*100 "% <= numerator <= " $ub*100 "%"

** SETUP **
	
	* 00 initialize folder structure
		di in yellow "   00 initialize"
		run 00_highlander_initialize
		
	* 01 create a crosswalk table for highlander age groups and categories
		di in yellow "   01 generate crosswalk table"
		run "$do/01_highlander_agegroups"
	
	foreach ou of global ctrylist{
		global ctry `ou'
		
	* 02 import data and structure it for use
		di in yellow _newline "`=upper("${ctry}")':" _newline "   02 import ...running"
		run "$do/02_highlander_import"
		
** HIGHLANDER SCRIPT **

	* 03 run Highlander Script on countries to make finer/coarse/... selection
		di in yellow "   03 choice ...running"
		run "$do/03_highlander_choice"
		
	* 04 apply selection to full dataset
		di in yellow "   04 apply  ...running"
		run "$do/04_highlander_apply"
		di in yellow "             ...saved"	
		}
		*end

** APPEND **
	* 05 append files
		di in yellow "   05 appending  ...running"
		run "$do/05_highlander_appendctrys"
	
** CLEAN UP **
	* 06 remove temp files
		di in yellow "   06 cleaning  ...running"
		run "$do/06_highlander_cleartemp"
