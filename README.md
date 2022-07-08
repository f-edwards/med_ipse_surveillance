# med_ipse_surveillance
Replication package for The prevalence and racial inequality of medical surveillance of infants for child maltreatment and substance exposure in the US 2010 - 2019

Depends on NCANDS child file 2010 - 2019 available upon request from NDACAN (https://www.ndacan.acf.hhs.gov/datasets/datasets-list-ncands-child-file.cfm)

Depends on SEER population data single ages 4 expanded races by origin data available from NCI: https://seer.cancer.gov/popdata/download.html

contact: frank.edwards@rutgers.edu

workflow for replication:

1) data processing and imputation; impute_ncands10_20.r for national analyses (requires NCANDS; requires SEER population data)
2) data processing; check_state_quality.r for substance use variable validation
3) data processing and imputation; impute_ncands_ipse.r for state analyses with IPSE subset (requires NCANDS; requires SEER)
4) data processing; make_fig_data.r 
5) produce manuscript visuals; figures.r
