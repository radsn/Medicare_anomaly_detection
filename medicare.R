## Read in the tab delimited medicare file
## File size is 2.1 GB for the plain text file, so reading takes time.
## May want to ask Brian for the SAS file converted to csv, since it will be cleaner.

## Load libraries
library(tidyr)
library(ggplot2)
library(corrr)
library(DescTools)
library(stringr)
library(ggthemes)
library(mapproj)
library(plotly)
library(readr)
library(dplyr)

# Read in the large datafile
medicare1 = read.csv('Medicare_PUF_CY2017.txt', sep="\t", header=TRUE, stringsAsFactors = F)

#Remove the first row of the file since it is gibberish. 
medicare <- medicare1[-c(1), ]
dim(medicare) ## There are 9,847,443 rows

# Write out the modified file to read elsewhere
write.csv(medicare, file='medicaredf.csv', row.names=F)

# Basic diagnostics on the medicare data
colnames(medicare)
class(medicare)
head(medicare)
summary(medicare)

sapply(medicare, class)
str(medicare) #Similar to proc contents in SAS.

#### 1 Number unique NPIs There are 1032911 unique NPIs ####
#Counting number of unique NPIs in dataset
uniq_npi = unique(medicare$npi)
length(uniq_npi) ## a little over 1 million unique NPIs.

#### 2 Surnames grouped by unique NPIs 1032911 unique surnames ####
#unique values in surname after grouping by npi -- matches the number of unique NPIs
uniq_surname= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_last_org_name)
sum(is.na(uniq_surname$nppes_provider_last_org_name))
unisur = unique(uniq_surname)
dim(unisur)
colnames(unisur)
summary(unisur)

#### 3 First names grouped by NPIs. 1032911 unique first names ####
#unique values in first name after grouping by NPI -- matches the number of unique NPIs
uniq_name= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_first_name)
sum(is.na(uniq_name$nppes_provider_first_name))
uninam = unique(uniq_name)
dim(uninam)
colnames(uninam)
summary(uninam)

#### 4 Middle initials grouped by NPIs. 1032911 unique middle initials ####
#unique values in middle name after grouping by NPI -- matches the number of unique NPIs
uniq_mi= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_mi)
sum(is.na(uniq_mi$nppes_provider_mi))
unimi = unique(uniq_mi)
dim(unimi)
colnames(unimi)
summary(unimi)

#### 5 Credentials (degrees) grouped by NPI. Names are mixed up here. Blanks are orgs ####
# unique values in credentials after grouping by NPI -- matches the number of unique NPIs
uniq_cred= medicare %>% 
  group_by(npi) %>%  
  select(nppes_credentials)
sum(is.na(uniq_cred$nppes_credentials))
unicred = unique(uniq_cred)
dim(unicred)
colnames(unicred)
summary(unicred)
table(unicred$nppes_credentials, exclude = NULL)

#### 6 Gender. there are missings here for orgs. Needs to be classified as such. #### 
# unique values in gender after grouping by NPI -- matches the number of unique NPIs
uniq_gen= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_gender)
sum(is.na(uniq_gen$nppes_provider_gender))
unigen = unique(uniq_gen)
dim(unigen)
colnames(unigen)
summary(unigen)
table(unigen$nppes_provider_gender, exclude = NULL) 

#### 7 Entity code -- 2 level response I/O (individual or org.)  ####
# unique values in entity code after grouping by NPI -- matches the number of unique NPIs
uniq_ec= medicare %>% 
  group_by(npi) %>%  
  select(nppes_entity_code)
sum(is.na(uniq_ec$nppes_entity_code))
uniec = unique(uniq_ec)
dim(uniec)
colnames(uniec)
summary(uniec) 
table(uniec$nppes_entity_code, exclude= NULL)

#### 8 Street address 1 -- same number as unique NPIs ####
# unique values in provider street address 1 after grouping by NPI -- matches the number of unique NPIs
uniq_st1= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_street1)
sum(is.na(uniq_st1$nppes_provider_street1))
unist1 = unique(uniq_st1)
dim(unist1)
colnames(unist1)
summary(unist1)


#### 9 Street address 2 -- same number as unique NPIs ####
# unique values in provider street address 2 after grouping by NPI -- matches the number of unique NPIs
uniq_st2= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_street2)
sum(is.na(uniq_st2$nppes_provider_street2))
unist2 = unique(uniq_st2)
dim(unist2)
colnames(unist1)
summary(unist1)
table(unist1$nppes_provider_street1, exclude = NULL)


#### 10 City -- same number as unique NPIs ####
# unique values in provider city after grouping by NPI -- matches the number of unique NPIs
uniq_city= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_city)
sum(is.na(uniq_city$nppes_provider_city))
unicity = unique(uniq_city, na.rm=T)
dim(unicity)
class(unicity)
colnames(unicity)
summary(unicity)
table(unicity$nppes_provider_city, exclude = NULL)

#### 11 Zip code. The length of the zip codes is wrong in many if not most cases. ####
# unique values in provider zip after grouping by NPI -- matches the number of unique NPIs
uniq_zip= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_zip)
sum(is.na(uniq_zip$nppes_provider_zip))
unizip = unique(uniq_zip)
dim(unizip)
colnames(unizip)
summary(unizip)
table(unizip$nppes_provider_zip, exclude = NULL)
length(unizip$nppes_provider_zip)


#### 12 State. There are XX and ZZs here which should likely be omitted ####
# unique values in provider state after grouping by NPI -- matches the number of unique NPIs
uniq_sta= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_state)
sum(is.na(uniq_sta$nppes_provider_state))
unista = unique(uniq_sta)
dim(unista)
colnames(unista)
summary(unista)
table(unista$nppes_provider_state)


#### 13 Country. There are many in other countries and they should likely be omitted ####
# unique values in provider country after grouping by NPI -- matches the number of unique NPIs
uniq_cou= medicare %>% 
  group_by(npi) %>%  
  select(nppes_provider_country)
sum(is.na(uniq_cou$nppes_provider_country))
unicou = unique(uniq_cou)
dim(unicou)
colnames(unicou)
summary(unicou)
table(unicou$nppes_provider_country, exclude = NULL)


#### 14 Provider Type (specialization) -- same number as NPIs ####
# unique values in provider type after grouping by NPI -- matches the number of unique NPIs
uniq_typ= medicare %>% 
  group_by(npi) %>%  
  select(provider_type)
sum(is.na(uniq_typ$provider_type))
unityp = unique(uniq_typ, na.rm=T)
dim(unityp)
colnames(unityp)
summary(unityp)
table(unityp$provider_type, exclude = NULL)


#### 15 Participation indicator (participates in medicare Y/N) -- 2 level response Y/N ####
# unique values in participation indicator after grouping by NPI -- 1004 higher than number of unique NPIs
uniq_idc= medicare %>% 
  group_by(npi) %>%  
  select(medicare_participation_indicator)
sum(is.na(uniq_idc$medicare_participation_indicator))
uniidc = unique(uniq_idc)
dim(uniidc)
colnames(uniidc)
summary(uniidc)
table(uniidc$medicare_participation_indicator, exclude = NULL)

#### 16 Place of service -- 2 level response F/O (facility/non-facility) ####
# unique values in place of service after grouping by NPI -- 243,065 higher than number of unique NPIs
uniq_pos= medicare %>% 
  group_by(npi) %>%  
  select(place_of_service)
sum(is.na(uniq_pos$place_of_service))
unipos = unique(uniq_pos)
dim(unipos)
colnames(unipos)
summary(unipos)
table(unipos$place_of_service, exclude = NULL)

#### 17 HCPCS code. Code for services provided. ####
# unique values in hcpcs_code after grouping by NPI -- about 8.47 million more than number of unique NPIs
uniq_hcode= medicare %>% 
  group_by(npi) %>%  
  select(hcpcs_code)
colnames(uniq_hcode)
sum(is.na(uniq_hcode$hcpcs_code))
unihcode = unique(uniq_hcode, na.rm=T)
dim(unihcode)
colnames(unihcode)
summary(unihcode)
table(unihcode$hcpcs_code)

#### 18 HCPCS code description -- sentences describing medical services provided ####
# unique values in hcpcs_description after grouping by NPI -- about 8.35 mill more than number of unique NPIs
uniq_hdesc= medicare %>% 
  group_by(npi) %>%  
  select(hcpcs_description)
sum(is.na(uniq_hdesc$hcpcs_description))
unidesc = unique(uniq_hdesc)
dim(unidesc)
colnames(unidesc)
summary(unidesc)
table(unidesc$hcpcs_description, exclude = NULL)


#### 19 HCPCS drug indicator (indicates if hcpcs code is on med pt B drug sale price file) -- 2 level response Y/N ####
# unique values in hcpcs drug indicator after grouping by NPI -- 235,188 higher than number of unique NPIs
uniq_didx= medicare %>% 
  group_by(npi) %>%  
  select(hcpcs_drug_indicator)
sum(is.na(uniq_didx$hcpcs_drug_indicator))
unididx = unique(uniq_didx)
dim(unididx)
colnames(unididx)
summary(unididx)
table(unididx$hcpcs_drug_indicator, exclude = NULL)

#### 20 Line service count (# svc provided) -- continuous var but skewed ####
# unique values in line service count after grouping by NPI -- 7.7 mill more than number of unique NPIs
uniq_lsc= medicare %>% 
  group_by(npi) %>%  
  select(line_srvc_cnt)
sum(is.na(uniq_lsc$line_srvc_cnt))
unilsc = unique(uniq_lsc)
dim(unilsc)
colnames(unilsc)
summary(unilsc)
class(unilsc$line_srvc_cnt)
plot(density(unilsc$line_srvc_cnt), main = "Distribution of line service count")
table(unilsc$line_srvc_cnt, exclude = NULL)

#### 21 Beneficiary unique count continuous var but skewed ####
# unique values in beneficiary unique count after grouping by NPI -- 7.4 mill more number of unique NPIs
uniq_buc= medicare %>% 
  group_by(npi) %>%  
  select(bene_unique_cnt)
sum(is.na(uniq_buc$bene_unique_cnt))
unibuc = unique(uniq_buc)
dim(unibuc)
colnames(unibuc)
summary(unibuc)
class(unibuc$bene_unique_cnt)
plot(density(unibuc$bene_unique_cnt), main = "Distribution of beneficiary unique count")
table(unibuc$bene_unique_cnt, exclude = NULL)


#### 22 Beneficiary service count. Continuous var but skewed ####
# unique values in beneficiary per day service count after grouping by NPI -- 7.6 mil more than number of unique NPIs
uniq_dbsc= medicare %>% 
  group_by(npi) %>%  
  select(bene_day_srvc_cnt)
sum(is.na(uniq_dbsc$bene_day_srvc_cnt))
unidbsc = unique(uniq_dbsc)
dim(unidbsc)
colnames(unidbsc)
summary(unidbsc)
class(unidbsc$bene_day_srvc_cnt)
plot(density(unidbsc$bene_day_srvc_cnt), main = "Distribution of beneficiary daily service count")
table(unidbsc$bene_day_srvc_cnt, exclude = NULL)


#### 23 Avg medicare allowed amt. Continuous but skewed ####
# unique values in avg medicare allowed amt after grouping by NPI -- 8.46 mill more than number of unique NPIs
# This is the amount medicare is allowed to pay overall for the service.
uniq_avgallow= medicare %>% 
  group_by(npi) %>%  
  select(average_Medicare_allowed_amt)
sum(is.na(uniq_avgallow$average_Medicare_allowed_amt))
uniavgallow = unique(uniq_avgallow)
dim(uniavgallow)
colnames(uniavgallow)
summary(uniavgallow)
class(uniavgallow$average_Medicare_allowed_amt)
plot(density(uniavgallow$average_Medicare_allowed_amt), 
     main = "Distribution of medicare allowed amount")
table(uniavgallow$average_Medicare_allowed_amt, exclude = NULL)


#### 24 Avg submitted charge. Continuous and skewed. ####
# unique values in avg submitted charge amt after grouping by NPI -- 7.9 mill above number of unique NPIs
# avg. charge submitted by the provider to medicare.
uniq_avgcrg= medicare %>% 
  group_by(npi) %>%  
  select(average_submitted_chrg_amt)
sum(is.na(uniq_avgcrg$average_submitted_chrg_amt))
uniavgcrg = unique(uniq_avgcrg)
dim(uniavgcrg)
colnames(uniavgcrg)
summary(uniavgcrg)
class(uniavgcrg$average_submitted_chrg_amt)
plot(density(uniavgcrg$average_submitted_chrg_amt), 
     main = "Distribution of avg. charged amount")
table(uniavgcrg$average_submitted_chrg_amt, exclude = NULL)


#### 25 Avg medicare payment. Continuous and skewed ####
# unique values in avg medicare payment amt after grouping by NPI -- 8.7 mill above number of unique NPIs
# Avg. amount medicare paid.
uniq_avgpay= medicare %>% 
  group_by(npi) %>%  
  select(average_Medicare_payment_amt)
sum(is.na(uniq_avgpay$average_Medicare_payment_amt))
uniavgpay = unique(uniq_avgpay)
dim(uniavgpay)
colnames(uniavgpay)
summary(uniavgpay)
class(uniavgpay$average_Medicare_payment_amt)
plot(density(uniavgpay$average_Medicare_payment_amt), 
     main = "Distribution of avg. payment amount")
table(uniavgpay$average_Medicare_payment_amt, exclude = NULL)



#### 26 Avg medicare std. amt. Continuous and skewed ####
# unique values in avg medicare standard amt after grouping by NPI -- 8.7 mill above number of unique NPIs
# This amount has been standardized on location. 
uniq_avgstd= medicare %>% 
  group_by(npi) %>%  
  select(average_Medicare_standard_amt)
sum(is.na(uniq_avgstd$average_Medicare_standard_amt))
uniavgstd = unique(uniq_avgstd)
dim(uniavgstd)
colnames(uniavgstd)
summary(uniavgstd)
class(uniavgstd$average_Medicare_standard_amt)
plot(density(uniavgstd$average_Medicare_standard_amt), 
     main = "Distribution of std. amount")
table(uniavgstd$average_Medicare_standard_amt, exclude = NULL)





#### Check if the data can be collapsed to have only the million unique NPIs -- needs work ####

# medicare2 = medicare %>% 
#   group_by(npi,nppes_provider_last_org_name,nppes_provider_first_name,nppes_provider_mi,
#            nppes_credentials,nppes_provider_gender, nppes_entity_code,nppes_provider_street1,
#            nppes_provider_street2,nppes_provider_city,nppes_provider_zip,nppes_provider_state,
#            nppes_provider_country,provider_type,medicare_participation_indicator) %>% 
#   mutate(key1 = paste0('plserv_code_', row_number()), 
#          key2 = paste0('hcps_code_', row_number()),
#          key3 = paste0('hcpsdesc_code_', row_number()),
#          key4 = paste0('hcpsdg_code_', row_number()), 
#          key5 = paste0('linsrvcv_code_', row_number()),
#          key6 = paste0('benuniq_code_', row_number()), 
#          key7 = paste0('bendsrv_code_', row_number()),
#          key8 = paste0('avgmedal_code_', row_number()), 
#          key9 = paste0('avgsubchr_code_', row_number()),
#          key10 = paste0('avgmedpay_code_', row_number()), 
#          key11 = paste0('avgmedstd_code_', row_number())) %>% 
#   spread(key1, place_of_service, 
#          key2, hcpcs_code, 
#          key3, hcpcs_description, 
#          key4, hcpcs_drug_indicator, 
#          key5, line_srvc_cnt, 
#          key6, bene_unique_cnt,
#          key7, bene_day_srvc_cnt,
#          key8, average_Medicare_allowed_amt,
#          key9, average_submitted_chrg_amt,
#          key10, average_Medicare_payment_amt,
#          key11, average_Medicare_standard_amt)

