## Do File Notes
### 02_highlander_import

PURPOSE - import site level Fact View dataset for each OU

- Dependent on initial setup from `00_highlander_initialize` and crosswalk from `01_highlander_agegroups`
- Checks to see if a `.dta` file exists for the country (at site by IM level) and creates one if it does not
- When importing the site by IM dataset, only HTC_TST, TX_CURR, and TX_NEW are imported as those are the key indicators for the analysis
- The highlander age categories (created in `01_highlander_agegroups`) are merge onto the site data for use in collapsing and identifying coarse v finer   disaggs in other do files
  * Since "<01" can also be used as a course disaggregate, we need to replace the the `hs-type` designation for TX_CURR and TX_NEW "Age/Sex Aggregated"
- Data can be recorded at the facility, community, or military levels.  Each of these are unique identifiers (UIDs) is listed in seperate columns. A new variable is created and contains the UIDs from these variables (`fcm_uid`, where fcm stands for facility/community/military).
  * Since military sites do not have a UID, the mechanim UID was used. In the `fcm_uid`, the designation is labled before the UID, using "f_", "c_", or "m_".
