## Do File Notes
### 03_highlander_choice

PURPOSE - develop a model of the Highlander Script

- Dependent on initial setup from `00_highlander_initialize`, crosswalk from `01_highlander_agegroups`, and site data imported in `02_highlander_import`
- The goal of the `03_highlander_choice` do file is to create a list indicating the [Highlander selection] (https://github.com/achafetz/ICPI/blob/master/HighlanderScript/Documents/highlanderscriptlogic.PNG) at the lowest level of analysis, i.e. by:
  * operating unit
  * PSNU
  * mechanism
  * site
  * indicator
  * indicator type
  * period

- The `collapse` command <sup>1</sup> eliminates most of the variables/columns in the dataset and is the first step in getting to to the lowest level of analysis. Below is the list of variables that are included (**bolded variable name**) and dropped ("X") in the merge.
  - The unique ids (UIDs) for facility and community are dropped, but they are contained in the fcm_uid variable at the end, which also adds a UID for military, using just the mechanism UID.
  - `fy2016_targets` and `fy2015apr` variables are dropped. The APR variable is recreated later on using the Highlander Script selection values. At this time, targets have not been included.

| Variable                  | Dropped |
|---------------------------|:-------:|
| region                    |    X    |
| regionuid                 |    X    |
| **operatingunit**         |         |
| operatingunituid          |    X    |
| countryname               |    X    |
| snu1                      |    X    |
| **psnu**                  |         |
| **psnuuid**               |         |
| **snuprioritization**     |         |
| typemilitary              |    X    |
| mechanismuid              |    X    |
| primepartner              |    X    |
| fundingagency             |    X    |
| **mechanismid**           |         |
| implementingmechanismname |    X    |
| communityuid              |    X    |
| communityprioritization   |    X    |
| typecommunity             |    X    |
| facilityuid               |    X    |
| facilityprioritization    |    X    |
| typefacility              |    X    |
| **indicator**             |         |
| numeratordenom            |    X    |
| **indicatortype**         |         |
| disaggregate              |    X    |
| categoryoptioncomboname   |    X    |
| age                       |    X    |
| sex                       |    X    |
| resultstatus              |    X    |
| otherdisaggregate         |    X    |
| coarsedisaggregate        |    X    |
| status                    |    X    |
| **fy2015q2**              |         |
| **fy2015q3**              |         |
| **fy2015q4**              |         |
| **fy2015apr**             |         |
| fy2016_targets            |    X    |
| **fy2016q1**              |         |
| **fy2016q2**              |         |
| **fy2016q3**              |         |
| hs_agegp                  |    X    |
| **hs_type**               |         |
| **fcm_uid**               |         |

- Dedups are not concidered at this stage and removed since they are at a higher level than site/IM and will be included later in the processing
- To cut down on the processing and increase efficiency, the code, when relevant, eliminates lines with no data (value = 0 or .)   
- The `reshape` command in Stata essentially tranposes the dataset, making the dataset long or wide. The first long reshape is necessary so that the periods are in one column and to allow for the second reshape to wide with the highlander types (`hs_type`) to be column variables (this will make it easy for Stata to the calculations as new variables across rows).

A. original (wide) dataset
| ou    | fcm_uid | hs_type | fy2015q2 | fy2015q3 | fy2015q4 |
|-------|---------|---------|----------|----------|----------|
| Haiti | XsmcDs  | Fine    | 4        | 23       | 15       |
| Haiti | XsmcDs  | TotNum  | 6        | 30       | 16       |

B . reshape to long dataset creating  `pd` from `fy*` variables
| ou    | fcm_uid | hs_type | pd       | y  |
|-------|---------|---------|----------|----|
| Haiti | XsmcDs  | Fine    | fy2015q2 | 4  |
| Haiti | XsmcDs  | Fine    | fy2015q3 | 23 |
| Haiti | XsmcDs  | Fine    | fy2015q4 | 15 |
| Haiti | XsmcDs  | TotNum  | fy2015q2 | 6  |

C. reshape to wide dataset by `hs_type`
| ou    | fcm_uid | hs_type  | coarse | fine | results | totnum |
|-------|---------|----------|--------|------|---------|--------|
| Haiti | XsmcDs  | fy2015q2 | .      | 4    | .       | 6      |
| Haiti | XsmcDs  | fy2015q3 | .      | 23   | .       | 30     |
| Haiti | XsmcDs  | fy2015q4 | .      | 15   | .       | 16     |

- Identify which denominator to use to calculate completeness (upper and lower bounds are set in the `runall` dofile, where the default is set to 95-101%
  - HTC_TST compares its disaggs to results (Negative + Positive) where possible

| Option | Completeness Denominiator              |
|--------|----------------------------------------|
| 1      | Total Numerator used                   |
| 2      | Result used (HTC) - complete           |
| 3      | Result used (HTC) - no total numerator |
| 4      | Total Numerator used (HTC)             |
| 5      | No Total Numerator                     |

- Using the denominator determined in the prior step, the Highlander Script calculates the completeness of the different options and as well as their proximinity to 100% complete

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

- All extra variables are removed at this stage and the results save, indicating by IM/Site/indicatortype/period which value to choose use

<sup>1</sup> In the code, the`collapse` command aggregates (sums) the values up to to the level of analysis. For example, the original dataset contains disaggregate information like `age` (01-04, <15, etc). Since we only need to know whether it's a coarse or finer disaggregate, which is captured in `hs_type`, we can remove this variable and aggregate up to the coarse v finer distinction.
