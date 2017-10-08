## Do File Notes
### 04_highlander_choice

PURPOSE - apply Highlander choice by Site/IM/Indicator/IndicatorType/Period

- Dependent on initial setup from `00_highlander_initialize`, crosswalk from `01_highlander_agegroups`,  site data imported in `02_highlander_import`, and decision from `03_highlander_choice`
- Start by eliminating rows with no data from the original site dataset and reshape to match the `03_highlander_choice` for merging purposes

A. reshape long based on `fy*` variables

| ou    | fcm_uid | pd       | y  |
|-------|---------|----------|----|
| Haiti | XsmcDs  | fy2015q2 | 5  |
| Haiti | sfCGrk  | fy2015q2 | 18 |
| Haiti | fGnumd  | fy2015q2 | 15 |
| Haiti | XsmcDs  | fy2015q2 | 5  |

- Merge the Highlander Script choice (Finer, Coarse, Result, Total Numerator) from `03_highlander_choice` onto the original site file
  -  Merge matches the two datasets (the choice file and site dataset) based using a number of indicators as the key - `pd psnuuid fcm_uid indicator indicatortype mechanismid`
- Reshape again to use the choice selections in taking the actual values

B. reshape to wide dataset by `hs_type` (and generate hs_val)
| ou    | fcm_uid | hs_type  | hs_choice           | coarse | fine | results | totnum | hs_val |
|-------|---------|----------|---------------------|--------|------|---------|--------|--------|
| Haiti | XsmcDs  | fy2015q2 | Fine (complete)     | .      | 5    | .       | 5      | 5      |
| Haiti | XsmcDs  | fy2015q2 | Fine (incomplete)   | .      | 18   | .       | 50     | 18     |
| Haiti | XsmcDs  | fy2015q2 | Coarse (incomplete) | 15     | 10   | .       | .      | 15     |

- Reshape the dataset one last time to get just the chosen `hs_val` in the format of the Fact View dataset
C. reshape with just `hs_val` to match Fact View dataset

| ou    | fcm_uid | hs_choice           | fy2015q2 | fy2015q3 | fy2015q4 |
|-------|---------|---------------------|----------|----------|----------|
| Haiti | XsmcDs  | Fine (complete)     | 5        | .        | .        |
| Haiti | XsmcDs  | Fine (incomplete)   | 18       | .        | .        |
| Haiti | XsmcDs  | Coarse (incomplete) | 15       | .        | .        |

- Create a new `fy2015apr` variable based on the values from the Highlander Script, so `highlander` with a value of "Y" was added
- Lastly, need to aggregate the site level data up to the PSNU level (dropping `communityuid communityprioritization typecommunity facilityuid facilityprioritization typefacility`)
