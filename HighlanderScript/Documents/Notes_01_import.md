## Do File Notes
### 01_highlander_agegroups

PURPOSE - develop age groupings for the Highlander Script

- Checks to see if the `.dta` file exists yet for the crosswalk table; if it does, it skips this do file
- A crosswalk table is created to be used in the highlander Script, where ages are the key between the Fact View datasets and this table
- In the `02_highlander_import` file, this crosswalk is merged with the site by IM data. The data will be collapse in `03_highlander_choice` by age group and type.
- Note that "<01" can be used for both finer and coarse with TX_NEW and TX_CURR.  After mergining with the site data, the type is replaced with "Coarse" if "<01" for TX_NEW and TX_CURR and the disaggregate is "Age/Sex[,] Aggregated" 

| Age   | Age Group | Type   |
|-------|-----------|--------|
| <01   | <15       | Finer  |
| 01-14 | <15       | Finer  |
| 01-04 | <15       | Finer  |
| 05-09 | <15       | Finer  |
| 05-14 | <15       | Finer  |
| 10-14 | <15       | Finer  |
| <15   | <15       | Coarse |
| 15+   | 15+       | Coarse |
| 15-19 | 15+       | Finer  |
| 20+   | 15+       | Finer  |
| 20-24 | 15+       | Finer  |
| 25-49 | 15+       | Finer  |
| 50+   | 15+       | Finer  |
