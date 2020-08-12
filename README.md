# Anomaly Detection in Medicare Healthcare Data

### Background
Medicare is a U.S. healthcare program that provides insurance, primarily to individuals 65 years or older, to offload some of the financial burden associated with medical care. Even so, healthcare costs are high and continue to increase. Healthcare fraud is a major contributor to these inflating healthcare expenses which causes substantial monetary loss in Medicare and insurance industry. 

### Purpose
The aim of this project is to combine the Center for Medicaid/Medicare Service's (CMS) Provider Utilization and Payment Data files with the Office of Inspector General's List of Excluded Entities & Individuals (LEIE) in order to profile providers and detect anomalies.

### Introduction
CMS publishes data which describes provider use of the medicare healthcare system. This data includes utilization patterns, cost information and demographic information which describes each provider’s medicare population. The Office of Inspector General publishes a List of Excluded Individual/Entities (LEIE) which includes a list of providers which had been excluded from federally funded healthcare programs. The reasons for exclusion vary and include fraudulent related reasons. The goal of this project will be to cluster and profile providers based on the information in both of these data sources. ​Providers need to be profiled according to their utilization and payment patterns in order to find similar utilization and payment patterns with those providers identified by LEIE dataset. The focus of this project will be on Physician and Other Supplier Datasets from CMS and will not include information from Inpatient and Outpatient data.

### Data
1) LEIE: Office of Inspector General List of Excluded Individuals/Entities
- List: ​https://oig.hhs.gov/exclusions/exclusions_list.asp
- Exclusions Program Information: ​https://oig.hhs.gov/exclusions/index.asp
2) CMS Center for Medicaid & Medicare Services Provider Utilization and Payment Data: Physician and Other Supplier
https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/index.html

### Data Description
1) LEIE - total 73,000 records
2) CMS Provider Utilization and Payment Data (year 2012 to 2017 with 9+ million records per year) -> total 56 Million records

### Strategy
1) Sampling 200,000 random records from each of 6 years - total 1.2 Million records
2) Filter CMS 2017 for top 10 HCPCS codes (accounts for 20% of charges made by providers) - total 1.5 Million records
* HCPCS code: Healthcare Common Procedure Coding System

### Results
Matching LEIE and CMS 2017 top 10HCPCS file - total 141 unique provider IDs were excluded
- these matchings were based on NPI, lastname, firstname, zipcode, street address 

Class imbalance: add a weight factor
- Fraud: 1 / (excluded / total) = 4634.1214
- Not Fraud: 1 / (non-excluded / total) = 1.0002




