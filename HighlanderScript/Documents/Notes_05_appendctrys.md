## Do File Notes
### 05_highlander_append

PURPOSE - create a dataset

- - Dependent on initial setup from `00_highlander_initialize`, crosswalk from `01_highlander_agegroups`,  site data imported in `02_highlander_import`, decision from `03_highlander_choice`, aggregated Highlander Script dataset at PSNU level from `04_highlander_apply` and a PSNU dataset to append to

- Loop over all countries, appending their choice and highlander data in two seperate datasets
- The all country value data will be appended to the PSNU level dataset
- The choice data can be used for analysis (eg how many site/IMs were finer v coarse)
