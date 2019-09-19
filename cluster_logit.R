## Read in the final datafile for the Capstone project 
## Run basic tests to enable feature reduction/selection

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
combo = read.csv('widedf.csv', header=TRUE, stringsAsFactors = F)

# Check the file
class(combo)
head(combo)
summary(combo)
dim(combo)
sapply(combo, class)
str(combo)
colnames(combo)

##Check the frequencies of the categorical vars.

table(combo$Target, exclude = NULL) ##No Missing value here
table(combo$weight, exclude = NULL) ##No Missing value here
table(combo$state, exclude = NULL) # No missings here
table(combo$gender, exclude = NULL) #No missings here
table(combo$EXCLTYPE, exclude = NULL) #No missings here 
table(combo$provider_type, exclude = NULL) #No missings here 


## Select all numeric variables.
combo2 <- select_if(combo, is.numeric)

colnames(combo2)

##Check the variables

####The histograms for the Net Allow Pay variable: All are terrible. ####
plot(density(combo2$Net_allow_pay_66984), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99213), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99214), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99223), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99232), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99233), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99284), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99285), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99285), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_99291), main = "Overall Distribution of net allowed pay") #not useful.
plot(density(combo2$Net_allow_pay_A0427), main = "Overall Distribution of net allowed pay") #not useful.


##Take the ln of the Net Allow Pay is not possible for most due to negative values. 
summary(combo2$Net_allow_pay_66984)
summary(combo2$Net_allow_pay_99213)
summary(combo2$Net_allow_pay_99214)
summary(combo2$Net_allow_pay_99223)
summary(combo2$Net_allow_pay_99232)
summary(combo2$Net_allow_pay_99233)
summary(combo2$Net_allow_pay_99284)
summary(combo2$Net_allow_pay_99285)
summary(combo2$Net_allow_pay_99291)
summary(combo2$Net_allow_pay_A0427) ## This may be doable. Need to check frequencies.

##### The histograms for the Net Submit Pay variables: All are terrible. ####
plot(density(combo2$Net_submit_pay_66984), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99213), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99214), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99223), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99232), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99233), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99284), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99285), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99285), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_99291), main = "Overall Distribution of net submit pay") #not useful.
plot(density(combo2$Net_submit_pay_A0427), main = "Overall Distribution of net submit pay") #not useful.


##Taking the ln of the Net Submit Pay is not possible for most due to negative values. 
summary(combo2$Net_submit_pay_66984)
summary(combo2$Net_submit_pay_99213)
summary(combo2$Net_submit_pay_99214)
summary(combo2$Net_submit_pay_99223)
summary(combo2$Net_submit_pay_99232)
summary(combo2$Net_submit_pay_99233)
summary(combo2$Net_submit_pay_99284) # may be possible. Check frequencies.
summary(combo2$Net_submit_pay_99285)
summary(combo2$Net_submit_pay_99291)
summary(combo2$Net_submit_pay_A0427) # May be possible. Check frequencies.

##### The histograms for the Total Medicare Allowed Amount variables: All are terrible. ####
plot(density(combo2$Total_Medicare_allowed_amt_66984), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99213), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99214), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99223), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99232), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99233), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99284), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99285), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_99291), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.
plot(density(combo2$Total_Medicare_allowed_amt_A0427), main = "Overall Distribution of Total_Medicare_allowed_amt") #not useful.


##Taking the ln of the Total_Medicare_allowed_amt is possible 
summary(combo2$Total_Medicare_allowed_amt_66984)
summary(combo2$Total_Medicare_allowed_amt_99213)
summary(combo2$Total_Medicare_allowed_amt_99214)
summary(combo2$Total_Medicare_allowed_amt_99223)
summary(combo2$Total_Medicare_allowed_amt_99232)
summary(combo2$Total_Medicare_allowed_amt_99233)
summary(combo2$Total_Medicare_allowed_amt_99284) 
summary(combo2$Total_Medicare_allowed_amt_99285)
summary(combo2$Total_Medicare_allowed_amt_99291)
summary(combo2$Total_Medicare_allowed_amt_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$Total_Medicare_allowed_amt_66984 = combo2$Total_Medicare_allowed_amt_66984+1
combo$LTMAA_66984 = log(combo2$Total_Medicare_allowed_amt_66984)
plot(density(combo2$LTMAA_66984), main = "Overall Distribution of Total_Medicare_allowed_amt") #much better

combo2$Total_Medicare_allowed_amt_99213 = combo2$Total_Medicare_allowed_amt_99213+1
combo$LTMAA_99213 = log(combo2$Total_Medicare_allowed_amt_99213)
plot(density(combo2$LTMAA_99213), main = "Overall Distribution of Total_Medicare_allowed_amt") #Odd distribution

combo2$Total_Medicare_allowed_amt_99214 = combo2$Total_Medicare_allowed_amt_99214+1
combo$LTMAA_99214 = log(combo2$Total_Medicare_allowed_amt_99214)
plot(density(combo2$LTMAA_99214), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99223 = combo2$Total_Medicare_allowed_amt_99223+1
combo$LTMAA_99223 = log(combo2$Total_Medicare_allowed_amt_99223)
plot(density(combo2$LTMAA_99223), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99232 = combo2$Total_Medicare_allowed_amt_99232+1
combo$LTMAA_99232 = log(combo2$Total_Medicare_allowed_amt_99232)
plot(density(combo2$LTMAA_99232), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99233 = combo2$Total_Medicare_allowed_amt_99233+1
combo$LTMAA_99233 = log(combo2$Total_Medicare_allowed_amt_99233)
plot(density(combo2$LTMAA_99233), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99284 = combo2$Total_Medicare_allowed_amt_99284+1
combo$LTMAA_99284 = log(combo2$Total_Medicare_allowed_amt_99284)
plot(density(combo2$LTMAA_99284), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99285 = combo2$Total_Medicare_allowed_amt_99285+1
combo$LTMAA_99285 = log(combo2$Total_Medicare_allowed_amt_99285)
plot(density(combo2$LTMAA_99285), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_99291 = combo2$Total_Medicare_allowed_amt_99291+1
combo$LTMAA_99291 = log(combo2$Total_Medicare_allowed_amt_99291)
plot(density(combo2$LTMAA_99291), main = "Overall Distribution of Total_Medicare_allowed_amt") #same odd distribution as 213

combo2$Total_Medicare_allowed_amt_A0427 = combo2$Total_Medicare_allowed_amt_A0427+1
combo$LTMAA_A0427 = log(combo2$Total_Medicare_allowed_amt_A0427)
plot(density(combo2$LTMAA_A0427), main = "Overall Distribution of Total_Medicare_allowed_amt") #Poor distribution likely because of low freq.


##### The histograms for the Total Medicare Standard Payment Amount variables: All are terrible. ####
plot(density(combo2$Total_Medicare_std_payment_amt_66984), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99213), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99214), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99223), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99232), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99233), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99284), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99285), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_99291), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.
plot(density(combo2$Total_Medicare_std_payment_amt_A0427), main = "Overall Distribution of Total_Medicare_std_payment_amt") #not useful.


##Taking the ln of the Total_Medicare_std_payment_amt is possible 
summary(combo2$Total_Medicare_std_payment_amt_66984)
summary(combo2$Total_Medicare_std_payment_amt_99213)
summary(combo2$Total_Medicare_std_payment_amt_99214)
summary(combo2$Total_Medicare_std_payment_amt_99223)
summary(combo2$Total_Medicare_std_payment_amt_99232)
summary(combo2$Total_Medicare_std_payment_amt_99233)
summary(combo2$Total_Medicare_std_payment_amt_99284) 
summary(combo2$Total_Medicare_std_payment_amt_99285)
summary(combo2$Total_Medicare_std_payment_amt_99291)
summary(combo2$Total_Medicare_std_payment_amt_A0427) 


##Set up the log of the variables.
# First add one to all values
combo2$Total_Medicare_std_payment_amt_66984 = combo2$Total_Medicare_std_payment_amt_66984+1
combo$LTMSPA_66984 = log(combo2$Total_Medicare_std_payment_amt_66984)
plot(density(combo2$LTMSPA_66984), main = "Overall Distribution of Total_Medicare_std_payment_amt") #much better

combo2$Total_Medicare_std_payment_amt_99213 = combo2$Total_Medicare_std_payment_amt_99213+1
combo$LTMSPA_99213 = log(combo2$Total_Medicare_std_payment_amt_99213)
plot(density(combo2$LTMSPA_99213), main = "Overall Distribution of Total_Medicare_std_payment_amt") #Odd distribution

combo2$Total_Medicare_std_payment_amt_99214 = combo2$Total_Medicare_std_payment_amt_99214+1
combo$LTMSPA_99214 = log(combo2$Total_Medicare_std_payment_amt_99214)
plot(density(combo2$LTMSPA_99214), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99223 = combo2$Total_Medicare_std_payment_amt_99223+1
combo$LTMSPA_99223 = log(combo2$Total_Medicare_std_payment_amt_99223)
plot(density(combo2$LTMSPA_99223), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99232 = combo2$Total_Medicare_std_payment_amt_99232+1
combo$LTMSPA_99232 = log(combo2$Total_Medicare_std_payment_amt_99232)
plot(density(combo2$LTMSPA_99232), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99233 = combo2$Total_Medicare_std_payment_amt_99233+1
combo$LTMSPA_99233 = log(combo2$Total_Medicare_std_payment_amt_99233)
plot(density(combo2$LTMSPA_99233), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99284 = combo2$Total_Medicare_std_payment_amt_99284+1
combo$LTMSPA_99284 = log(combo2$Total_Medicare_std_payment_amt_99284)
plot(density(combo2$LTMSPA_99284), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99285 = combo2$Total_Medicare_std_payment_amt_99285+1
combo$LTMSPA_99285 = log(combo2$Total_Medicare_std_payment_amt_99285)
plot(density(combo2$LTMSPA_99285), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_99291 = combo2$Total_Medicare_std_payment_amt_99291+1
combo$LTMSPA_99291 = log(combo2$Total_Medicare_std_payment_amt_99291)
plot(density(combo2$LTMSPA_99291), main = "Overall Distribution of Total_Medicare_std_payment_amt") #same odd distribution as 213

combo2$Total_Medicare_std_payment_amt_A0427 = combo2$Total_Medicare_std_payment_amt_A0427+1
combo$LTMSPA_A0427 = log(combo2$Total_Medicare_std_payment_amt_A0427)
plot(density(combo2$LTMSPA_A0427), main = "Overall Distribution of Total_Medicare_std_payment_amt") #Poor distribution likely because of low freq.


##### The histograms for the Total Submitted Charge Amount variables: All are terrible. ####
plot(density(combo2$Total_submitted_chrg_amt_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99213), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99214), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99223), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99232), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99233), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99284), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99285), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_99291), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$Total_submitted_chrg_amt_A0427), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.


##Taking the ln of the Total_submitted_chrg_amt is possible 
summary(combo2$Total_submitted_chrg_amt_66984)
summary(combo2$Total_submitted_chrg_amt_99213)
summary(combo2$Total_submitted_chrg_amt_99214)
summary(combo2$Total_submitted_chrg_amt_99223)
summary(combo2$Total_submitted_chrg_amt_99232)
summary(combo2$Total_submitted_chrg_amt_99233)
summary(combo2$Total_submitted_chrg_amt_99284) 
summary(combo2$Total_submitted_chrg_amt_99285)
summary(combo2$Total_submitted_chrg_amt_99291)
summary(combo2$Total_submitted_chrg_amt_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$Total_submitted_chrg_amt_66984 = combo2$Total_submitted_chrg_amt_66984+1
combo$LSCA_66984 = log(combo2$Total_submitted_chrg_amt_66984)
plot(density(combo2$LSCA_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #much better

combo2$Total_submitted_chrg_amt_99213 = combo2$Total_submitted_chrg_amt_99213+1
combo$LSCA_99213 = log(combo2$Total_submitted_chrg_amt_99213)
plot(density(combo2$LSCA_99213), main = "Overall Distribution of Total_submitted_chrg_amt") #Odd distribution

combo2$Total_submitted_chrg_amt_99214 = combo2$Total_submitted_chrg_amt_99214+1
combo$LSCA_99214 = log(combo2$Total_submitted_chrg_amt_99214)
plot(density(combo2$LSCA_99214), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99223 = combo2$Total_submitted_chrg_amt_99223+1
combo$LSCA_99223 = log(combo2$Total_submitted_chrg_amt_99223)
plot(density(combo2$LSCA_99223), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99232 = combo2$Total_submitted_chrg_amt_99232+1
combo$LSCA_99232 = log(combo2$Total_submitted_chrg_amt_99232)
plot(density(combo2$LSCA_99232), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99233 = combo2$Total_submitted_chrg_amt_99233+1
combo$LSCA_99233 = log(combo2$Total_submitted_chrg_amt_99233)
plot(density(combo2$LSCA_99233), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99284 = combo2$Total_submitted_chrg_amt_99284+1
combo$LSCA_99284 = log(combo2$Total_submitted_chrg_amt_99284)
plot(density(combo2$LSCA_99284), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99285 = combo2$Total_submitted_chrg_amt_99285+1
combo$LSCA_99285 = log(combo2$Total_submitted_chrg_amt_99285)
plot(density(combo2$LSCA_99285), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_99291 = combo2$Total_submitted_chrg_amt_99291+1
combo$LSCA_99291 = log(combo2$Total_submitted_chrg_amt_99291)
plot(density(combo2$LSCA_99291), main = "Overall Distribution of Total_submitted_chrg_amt") #same odd distribution as 213

combo2$Total_submitted_chrg_amt_A0427 = combo2$Total_submitted_chrg_amt_A0427+1
combo$LSCA_A0427 = log(combo2$Total_submitted_chrg_amt_A0427)
plot(density(combo2$LSCA_A0427), main = "Overall Distribution of Total_submitted_chrg_amt") #Poor distribution likely because of low freq.


##### The histograms for the Average Medicare Allowed Amount variables: All are terrible. ####
plot(density(combo2$average_Medicare_allowed_amt_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99213), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99214), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99223), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99232), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99233), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99284), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99285), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_99291), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.
plot(density(combo2$average_Medicare_allowed_amt_A0427), main = "Overall Distribution of average_Medicare_allowed_amt") #not useful.


##Taking the ln of the average_Medicare_allowed_amt is possible 
summary(combo2$average_Medicare_allowed_amt_66984)
summary(combo2$average_Medicare_allowed_amt_99213)
summary(combo2$average_Medicare_allowed_amt_99214)
summary(combo2$average_Medicare_allowed_amt_99223)
summary(combo2$average_Medicare_allowed_amt_99232)
summary(combo2$average_Medicare_allowed_amt_99233)
summary(combo2$average_Medicare_allowed_amt_99284) 
summary(combo2$average_Medicare_allowed_amt_99285)
summary(combo2$average_Medicare_allowed_amt_99291)
summary(combo2$average_Medicare_allowed_amt_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$average_Medicare_allowed_amt_66984 = combo2$average_Medicare_allowed_amt_66984+1
combo$LMAA_66984 = log(combo2$average_Medicare_allowed_amt_66984)
plot(density(combo2$LMAA_66984), main = "Overall Distribution of average_Medicare_allowed_amt") #much better

combo2$average_Medicare_allowed_amt_99213 = combo2$average_Medicare_allowed_amt_99213+1
combo$LMAA_99213 = log(combo2$average_Medicare_allowed_amt_99213)
plot(density(combo2$LMAA_99213), main = "Overall Distribution of average_Medicare_allowed_amt") #Odd distribution

combo2$average_Medicare_allowed_amt_99214 = combo2$average_Medicare_allowed_amt_99214+1
combo$LMAA_99214 = log(combo2$average_Medicare_allowed_amt_99214)
plot(density(combo2$LMAA_99214), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99223 = combo2$average_Medicare_allowed_amt_99223+1
combo$LMAA_99223 = log(combo2$average_Medicare_allowed_amt_99223)
plot(density(combo2$LMAA_99223), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99232 = combo2$average_Medicare_allowed_amt_99232+1
combo$LMAA_99232 = log(combo2$average_Medicare_allowed_amt_99232)
plot(density(combo2$LMAA_99232), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99233 = combo2$average_Medicare_allowed_amt_99233+1
combo$LMAA_99233 = log(combo2$average_Medicare_allowed_amt_99233)
plot(density(combo2$LMAA_99233), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99284 = combo2$average_Medicare_allowed_amt_99284+1
combo$LMAA_99284 = log(combo2$average_Medicare_allowed_amt_99284)
plot(density(combo2$LMAA_99284), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99285 = combo2$average_Medicare_allowed_amt_99285+1
combo$LMAA_99285 = log(combo2$average_Medicare_allowed_amt_99285)
plot(density(combo2$LMAA_99285), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_99291 = combo2$average_Medicare_allowed_amt_99291+1
combo$LMAA_99291 = log(combo2$average_Medicare_allowed_amt_99291)
plot(density(combo2$LMAA_99291), main = "Overall Distribution of average_Medicare_allowed_amt") #same odd distribution as 213

combo2$average_Medicare_allowed_amt_A0427 = combo2$average_Medicare_allowed_amt_A0427+1
combo$LMAA_A0427 = log(combo2$average_Medicare_allowed_amt_A0427)
plot(density(combo2$LMAA_A0427), main = "Overall Distribution of average_Medicare_allowed_amt") #Poor distribution likely because of low freq.

##### The histograms for the Average Medicare standard Amount variables: All are terrible. ####
plot(density(combo2$average_Medicare_standard_amt_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99213), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99214), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99223), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99232), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99233), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99284), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99285), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_99291), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.
plot(density(combo2$average_Medicare_standard_amt_A0427), main = "Overall Distribution of average_Medicare_standard_amt") #not useful.


##Taking the ln of the average_Medicare_standard_amt is possible 
summary(combo2$average_Medicare_standard_amt_66984)
summary(combo2$average_Medicare_standard_amt_99213)
summary(combo2$average_Medicare_standard_amt_99214)
summary(combo2$average_Medicare_standard_amt_99223)
summary(combo2$average_Medicare_standard_amt_99232)
summary(combo2$average_Medicare_standard_amt_99233)
summary(combo2$average_Medicare_standard_amt_99284) 
summary(combo2$average_Medicare_standard_amt_99285)
summary(combo2$average_Medicare_standard_amt_99291)
summary(combo2$average_Medicare_standard_amt_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$average_Medicare_standard_amt_66984 = combo2$average_Medicare_standard_amt_66984+1
combo$LAMSA_66984 = log(combo2$average_Medicare_standard_amt_66984)
plot(density(combo2$LAMSA_66984), main = "Overall Distribution of average_Medicare_standard_amt") #much better

combo2$average_Medicare_standard_amt_99213 = combo2$average_Medicare_standard_amt_99213+1
combo$LAMSA_99213 = log(combo2$average_Medicare_standard_amt_99213)
plot(density(combo2$LAMSA_99213), main = "Overall Distribution of average_Medicare_standard_amt") #Odd distribution

combo2$average_Medicare_standard_amt_99214 = combo2$average_Medicare_standard_amt_99214+1
combo$LAMSA_99214 = log(combo2$average_Medicare_standard_amt_99214)
plot(density(combo2$LAMSA_99214), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99223 = combo2$average_Medicare_standard_amt_99223+1
combo$LAMSA_99223 = log(combo2$average_Medicare_standard_amt_99223)
plot(density(combo2$LAMSA_99223), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99232 = combo2$average_Medicare_standard_amt_99232+1
combo$LAMSA_99232 = log(combo2$average_Medicare_standard_amt_99232)
plot(density(combo2$LAMSA_99232), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99233 = combo2$average_Medicare_standard_amt_99233+1
combo$LAMSA_99233 = log(combo2$average_Medicare_standard_amt_99233)
plot(density(combo2$LAMSA_99233), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99284 = combo2$average_Medicare_standard_amt_99284+1
combo$LAMSA_99284 = log(combo2$average_Medicare_standard_amt_99284)
plot(density(combo2$LAMSA_99284), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99285 = combo2$average_Medicare_standard_amt_99285+1
combo$LAMSA_99285 = log(combo2$average_Medicare_standard_amt_99285)
plot(density(combo2$LAMSA_99285), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_99291 = combo2$average_Medicare_standard_amt_99291+1
combo$LAMSA_99291 = log(combo2$average_Medicare_standard_amt_99291)
plot(density(combo2$LAMSA_99291), main = "Overall Distribution of average_Medicare_standard_amt") #same odd distribution as 213

combo2$average_Medicare_standard_amt_A0427 = combo2$average_Medicare_standard_amt_A0427+1
combo$LAMSA_A0427 = log(combo2$average_Medicare_standard_amt_A0427)
plot(density(combo2$LAMSA_A0427), main = "Overall Distribution of average_Medicare_standard_amt") #Poor distribution likely because of low freq.


##### The histograms for the Average Medicare charge Amount variables: All are terrible. ####
plot(density(combo2$average_submitted_chrg_amt_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99213), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99214), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99223), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99232), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99233), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99284), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99285), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_99291), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.
plot(density(combo2$average_submitted_chrg_amt_A0427), main = "Overall Distribution of average_submitted_chrg_amt") #not useful.


##Taking the ln of the average_submitted_chrg_amt is possible 
summary(combo2$average_submitted_chrg_amt_66984)
summary(combo2$average_submitted_chrg_amt_99213)
summary(combo2$average_submitted_chrg_amt_99214)
summary(combo2$average_submitted_chrg_amt_99223)
summary(combo2$average_submitted_chrg_amt_99232)
summary(combo2$average_submitted_chrg_amt_99233)
summary(combo2$average_submitted_chrg_amt_99284) 
summary(combo2$average_submitted_chrg_amt_99285)
summary(combo2$average_submitted_chrg_amt_99291)
summary(combo2$average_submitted_chrg_amt_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$average_submitted_chrg_amt_66984 = combo2$average_submitted_chrg_amt_66984+1
combo$LASCA_66984 = log(combo2$average_submitted_chrg_amt_66984)
plot(density(combo2$LASCA_66984), main = "Overall Distribution of average_submitted_chrg_amt") #much better

combo2$average_submitted_chrg_amt_99213 = combo2$average_submitted_chrg_amt_99213+1
combo$LASCA_99213 = log(combo2$average_submitted_chrg_amt_99213)
plot(density(combo2$LASCA_99213), main = "Overall Distribution of average_submitted_chrg_amt") #Odd distribution

combo2$average_submitted_chrg_amt_99214 = combo2$average_submitted_chrg_amt_99214+1
combo$LASCA_99214 = log(combo2$average_submitted_chrg_amt_99214)
plot(density(combo2$LASCA_99214), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99223 = combo2$average_submitted_chrg_amt_99223+1
combo$LASCA_99223 = log(combo2$average_submitted_chrg_amt_99223)
plot(density(combo2$LASCA_99223), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99232 = combo2$average_submitted_chrg_amt_99232+1
combo$LASCA_99232 = log(combo2$average_submitted_chrg_amt_99232)
plot(density(combo2$LASCA_99232), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99233 = combo2$average_submitted_chrg_amt_99233+1
combo$LASCA_99233 = log(combo2$average_submitted_chrg_amt_99233)
plot(density(combo2$LASCA_99233), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99284 = combo2$average_submitted_chrg_amt_99284+1
combo$LASCA_99284 = log(combo2$average_submitted_chrg_amt_99284)
plot(density(combo2$LASCA_99284), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99285 = combo2$average_submitted_chrg_amt_99285+1
combo$LASCA_99285 = log(combo2$average_submitted_chrg_amt_99285)
plot(density(combo2$LASCA_99285), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_99291 = combo2$average_submitted_chrg_amt_99291+1
combo$LASCA_99291 = log(combo2$average_submitted_chrg_amt_99291)
plot(density(combo2$LASCA_99291), main = "Overall Distribution of average_submitted_chrg_amt") #same odd distribution as 213

combo2$average_submitted_chrg_amt_A0427 = combo2$average_submitted_chrg_amt_A0427+1
combo$LASCA_A0427 = log(combo2$average_submitted_chrg_amt_A0427)
plot(density(combo2$LASCA_A0427), main = "Overall Distribution of average_submitted_chrg_amt") #Poor distribution likely because of low freq.



##### The histograms for the Ratio of paid to allowed Amount variables: All are terrible. ####
plot(density(combo2$ratio_pay.allowed_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$ratio_pay.allowed_99213), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99214), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99223), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99232), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99233), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99284), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99285), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_99291), main = "Overall Distribution of ratio_pay.allowed") #not useful.
plot(density(combo2$ratio_pay.allowed_A0427), main = "Overall Distribution of ratio_pay.allowed") #not useful.


##Taking the ln of the ratio_pay.allowed is possible 
summary(combo2$ratio_pay.allowed_66984)
summary(combo2$ratio_pay.allowed_99213)
summary(combo2$ratio_pay.allowed_99214)
summary(combo2$ratio_pay.allowed_99223)
summary(combo2$ratio_pay.allowed_99232)
summary(combo2$ratio_pay.allowed_99233)
summary(combo2$ratio_pay.allowed_99284) 
summary(combo2$ratio_pay.allowed_99285)
summary(combo2$ratio_pay.allowed_99291)
summary(combo2$ratio_pay.allowed_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$ratio_pay.allowed_66984 = combo2$ratio_pay.allowed_66984+1
combo$LRPA_66984 = log(combo2$ratio_pay.allowed_66984)
plot(density(combo2$LRPA_66984), main = "Overall Distribution of ratio_pay.allowed") #much better
summary(combo$LRPA_66984)

combo2$ratio_pay.allowed_99213 = combo2$ratio_pay.allowed_99213+1
combo$LRPA_99213 = log(combo2$ratio_pay.allowed_99213)
plot(density(combo2$LRPA_99213), main = "Overall Distribution of ratio_pay.allowed") #Odd distribution
summary(combo$LRPA_99213)

combo2$ratio_pay.allowed_99214 = combo2$ratio_pay.allowed_99214+1
combo$LRPA_99214 = log(combo2$ratio_pay.allowed_99214)
plot(density(combo2$LRPA_99214), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99214)

combo2$ratio_pay.allowed_99223 = combo2$ratio_pay.allowed_99223+1
combo$LRPA_99223 = log(combo2$ratio_pay.allowed_99223)
plot(density(combo2$LRPA_99223), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99223)

combo2$ratio_pay.allowed_99232 = combo2$ratio_pay.allowed_99232+1
combo$LRPA_99232 = log(combo2$ratio_pay.allowed_99232)
plot(density(combo2$LRPA_99232), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99232)

combo2$ratio_pay.allowed_99233 = combo2$ratio_pay.allowed_99233+1
combo$LRPA_99233 = log(combo2$ratio_pay.allowed_99233)
plot(density(combo2$LRPA_99233), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99233)

combo2$ratio_pay.allowed_99284 = combo2$ratio_pay.allowed_99284+1
combo$LRPA_99284 = log(combo2$ratio_pay.allowed_99284)
plot(density(combo2$LRPA_99284), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99284)
summary(combo2$ratio_pay.allowed_99284)

combo2$ratio_pay.allowed_99285 = combo2$ratio_pay.allowed_99285+1
combo$LRPA_99285 = log(combo2$ratio_pay.allowed_99285)
plot(density(combo2$LRPA_99285), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99285)

combo2$ratio_pay.allowed_99291 = combo2$ratio_pay.allowed_99291+1
combo$LRPA_99291 = log(combo2$ratio_pay.allowed_99291)
plot(density(combo2$LRPA_99291), main = "Overall Distribution of ratio_pay.allowed") #same odd distribution as 213
summary(combo$LRPA_99291)

combo2$ratio_pay.allowed_A0427 = combo2$ratio_pay.allowed_A0427+1
combo$LRPA_A0427 = log(combo2$ratio_pay.allowed_A0427)
plot(density(combo2$LRPA_A0427), main = "Overall Distribution of ratio_pay.allowed") #Poor distribution likely because of low freq.
summary(combo$LRPA_A0427)

##### The histograms for the Ratio of paid to submit Amount variables: All are terrible. ####
plot(density(combo2$ratio_pay.submit_66984), main = "Overall Distribution of Total_submitted_chrg_amt") #not useful.
plot(density(combo2$ratio_pay.submit_99213), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99214), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99223), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99232), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99233), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99284), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99285), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_99291), main = "Overall Distribution of ratio_pay.submit") #not useful.
plot(density(combo2$ratio_pay.submit_A0427), main = "Overall Distribution of ratio_pay.submit") #not useful.


##Taking the ln of the ratio_pay.submit is possible 
summary(combo2$ratio_pay.submit_66984)
summary(combo2$ratio_pay.submit_99213)
summary(combo2$ratio_pay.submit_99214)
summary(combo2$ratio_pay.submit_99223)
summary(combo2$ratio_pay.submit_99232)
summary(combo2$ratio_pay.submit_99233)
summary(combo2$ratio_pay.submit_99284) 
summary(combo2$ratio_pay.submit_99285)
summary(combo2$ratio_pay.submit_99291)
summary(combo2$ratio_pay.submit_A0427) 

##Set up the log of the variables.
# First add one to all values
combo2$ratio_pay.submit_66984 = combo2$ratio_pay.submit_66984+1
combo$LPS_66984 = log(combo2$ratio_pay.submit_66984)
plot(density(combo2$LPS_66984), main = "Overall Distribution of ratio_pay.submit") #much better

combo2$ratio_pay.submit_99213 = combo2$ratio_pay.submit_99213+1
combo$LPS_99213 = log(combo2$ratio_pay.submit_99213)
plot(density(combo2$LPS_99213), main = "Overall Distribution of ratio_pay.submit") #Odd distribution

combo2$ratio_pay.submit_99214 = combo2$ratio_pay.submit_99214+1
combo$LPS_99214 = log(combo2$ratio_pay.submit_99214)
plot(density(combo2$LPS_99214), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99223 = combo2$ratio_pay.submit_99223+1
combo$LPS_99223 = log(combo2$ratio_pay.submit_99223)
plot(density(combo2$LPS_99223), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99232 = combo2$ratio_pay.submit_99232+1
combo$LPS_99232 = log(combo2$ratio_pay.submit_99232)
plot(density(combo2$LPS_99232), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99233 = combo2$ratio_pay.submit_99233+1
combo$LPS_99233 = log(combo2$ratio_pay.submit_99233)
plot(density(combo2$LPS_99233), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99284 = combo2$ratio_pay.submit_99284+1
combo$LPS_99284 = log(combo2$ratio_pay.submit_99284)
plot(density(combo2$LPS_99284), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99285 = combo2$ratio_pay.submit_99285+1
combo$LPS_99285 = log(combo2$ratio_pay.submit_99285)
plot(density(combo2$LPS_99285), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_99291 = combo2$ratio_pay.submit_99291+1
combo$LPS_99291 = log(combo2$ratio_pay.submit_99291)
plot(density(combo2$LPS_99291), main = "Overall Distribution of ratio_pay.submit") #same odd distribution as 213

combo2$ratio_pay.submit_A0427 = combo2$ratio_pay.submit_A0427+1
combo$LPS_A0427 = log(combo2$ratio_pay.submit_A0427)
plot(density(combo2$LPS_A0427), main = "Overall Distribution of ratio_pay.submit") #Poor distribution likely because of low freq.











####Check the correlation of the two ratio variables. First among each other ####
colnames(combo)

ratio1data = combo2 %>% select(LRPA_66984, LRPA_99213, LRPA_99214,LRPA_99223, LRPA_99232, 
                               LRPA_99233, LRPA_99284,LRPA_99285, LRPA_99291, LRPA_A0427)
class(ratio1data)
cor(ratio1data)

ratio2data = combo2 %>% select(LPS_66984,LPS_99213,LPS_99214, LPS_99223, LPS_99232, 
                               LPS_99233, LPS_99284, LPS_99285, LPS_99291,LPS_A0427)                          
                             
   
class(ratio2data)
cor(ratio2data)


ratio3data = combo2 %>% select(LRPA_66984, LRPA_99213, LRPA_99214,LRPA_99223, LRPA_99232, 
                               LRPA_99233, LRPA_99284,LRPA_99285, LRPA_99291, LRPA_A0427,
                               LPS_66984,LPS_99213,LPS_99214, LPS_99223, LPS_99232, 
                               LPS_99233, LPS_99284, LPS_99285, LPS_99291,LPS_A0427)
class(ratio3data)
cor(ratio3data)


countdata = combo2 %>% select(line_srvc_cnt_66984, line_srvc_cnt_99213, line_srvc_cnt_99214, 
                              line_srvc_cnt_99223, 
                              line_srvc_cnt_99232, line_srvc_cnt_99233, line_srvc_cnt_99284, 
                              line_srvc_cnt_99285,                 
                              line_srvc_cnt_99291, line_srvc_cnt_A0427,
                              bene_unique_cnt_66984,bene_unique_cnt_99213, bene_unique_cnt_99214,
                              bene_unique_cnt_99223, bene_unique_cnt_99232, bene_unique_cnt_99233,
                              bene_unique_cnt_99284,bene_unique_cnt_99285, bene_unique_cnt_99291,
                              bene_unique_cnt_A0427)

class(countdata)
cor(countdata)


####Create the grouping variables  -- for region and for provider type ####

colnames(combo)

####Create multiple variables for regions ###
#### Northeast ####
combo$state = as.factor(combo$state)
combo$northeast = NULL
combo$northeast[combo$state %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 
                                   'NJ', 'NY', 'PA')] = 1
combo$northeast[combo$state %in% c('IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD',
                                   'DE', 'FL','GA', 'MD','NC','SC', 'VA','DC','WV','AL','KY','MS','TN',
                                   'AR','LA','OK','TX',
                                   'AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 
                                   'CA', 'HI', 'OR', 'WA')] = 0
combo$northeast = as.factor(combo$northeast)
table(combo$northeast, exclude = NULL)

#### Midwest ####
combo$midwest = NULL
combo$midwest[combo$state %in% c('IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')] = 1
combo$midwest[combo$state %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT','NJ', 'NY', 'PA',
                                 'DE', 'FL','GA', 'MD','NC','SC', 'VA','DC','WV','AL','KY','MS','TN',
                                 'AR','LA','OK','TX',
                                 'AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 
                                 'CA', 'HI', 'OR', 'WA')] = 0
combo$midwest = as.factor(combo$midwest)
table(combo$midwest, exclude = NULL)

#### South ####
combo$south = NULL
combo$south[combo$state %in% c('DE', 'FL','GA', 'MD','NC','SC', 'VA','DC','WV','AL','KY','MS','TN',
                                 'AR','LA','OK','TX')] = 1
combo$south[combo$state %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT','NJ', 'NY', 'PA',
                                 'IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD',
                                 'AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 
                                 'CA', 'HI', 'OR', 'WA')] = 0

combo$south = as.factor(combo$south)
table(combo$south, exclude = NULL)

#### West ####
combo$west = NULL
combo$west[combo$state %in% c('AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 
                              'CA', 'HI', 'OR', 'WA')] = 1
combo$west[combo$state %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT','NJ', 'NY', 'PA',
                               'IL','IN','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD',
                              'DE', 'FL','GA', 'MD','NC','SC', 'VA','DC','WV','AL','KY','MS','TN',
                              'AR','LA','OK','TX')] = 0

combo$west = as.factor(combo$west)
table(combo$west, exclude = NULL)

### Create the groupings for provider type ###

#### Basic care -- PCP etc. ####
combo$basic = NULL
combo$basic[combo$provider_type %in% c('Internal Medicine','Nurse Practitioner','Family Practice',
                                       'Physician Assistant','Emergency Medicine','Orthopedic Surgery',
                                       'Obstetrics & Gynecology','General Surgery',
                                       'Physical Medicine and Rehabilitation','Infectious Disease',
                                       'Endocrinology','Hospitalist','General Practice',
                                       'Allergy/ Immunology',
                                       'Pain Management','Interventional Pain Management',
                                       'Certified Clinical Nurse Specialist','Pediatric Medicine',
                                       'Diagnostic Radiology','Undefined Physician type',
                                       'Certified Nurse Midwife','Preventive Medicine','Sleep Medicine',
                                       'Addiction Medicine','Certified Registered Nurse Anesthetist (CRNA)',
                                       'Physical Therapist in Private Practice','Clinic or Group Practice',
                                       'Pathology','Registered Dietitian or Nutrition Professional',
                                       'Medical Toxicology','Ambulance Service Provider',
                                       'Speech Language Pathologist','Licensed Clinical Social Worker',
                                       'Chiropractic','Unknown Supplier/Provider Specialty',
                                       'Osteopathic Manipulative Medicine',
                                       'Hospice and Palliative Care')] = 1
combo$basic[combo$provider_type %in% c('Anesthesiology','Urology','Dermatology','Psychiatry','Rheumatology',
                                       'Neurology','Nephrology','Otolaryngology',
                                       'Plastic and Reconstructive Surgery','Ophthalmology',
                                       'Gastroenterology','Geriatric Medicine','Critical Care (Intensivists)',
                                       'Podiatry','Optometry','Sports Medicine','Hand Surgery',
                                       'Interventional Radiology',
                                       'Maxillofacial Surgery','Neuropsychiatry','Peripheral Vascular Disease',
                                       'Oral Surgery (Dentist only)','Geriatric Psychiatry','Nuclear Medicine', 
                                       'Psychologist, Clinical','Dentist',
                                       'Radiation Oncology','Cardiology','Interventional Cardiology',
                                       'Hematology-Oncology','Medical Oncology','Thoracic Surgery',
                                       'Neurosurgery','Clinical Cardiac Electrophysiology','Cardiac Surgery',
                                       'Hematology','Advanced Heart Failure and Transplant Cardiology',
                                       'Vascular Surgery','Surgical Oncology','Pulmonary Disease',
                                       'Colorectal Surgery (Proctology)','Gynecological Oncology')] = 0

combo$basic = as.factor(combo$basic)
table(combo$basic, exclude = NULL)

#### Specialists ####

combo$specialist = NULL
combo$specialist[combo$provider_type %in% c('Anesthesiology','Urology','Dermatology','Psychiatry','Rheumatology',
                                       'Neurology','Nephrology','Otolaryngology',
                                       'Plastic and Reconstructive Surgery','Ophthalmology',
                                       'Gastroenterology','Geriatric Medicine','Critical Care (Intensivists)',
                                       'Podiatry','Optometry','Sports Medicine','Hand Surgery',
                                       'Interventional Radiology',
                                       'Maxillofacial Surgery','Neuropsychiatry','Peripheral Vascular Disease',
                                       'Oral Surgery (Dentist only)','Geriatric Psychiatry','Nuclear Medicine', 
                                       'Psychologist, Clinical','Dentist')] = 1
combo$specialist[combo$provider_type %in% c('Internal Medicine','Nurse Practitioner','Family Practice',
                                       'Physician Assistant','Emergency Medicine','Orthopedic Surgery',
                                       'Obstetrics & Gynecology','General Surgery',
                                       'Physical Medicine and Rehabilitation','Infectious Disease',
                                       'Endocrinology','Hospitalist','General Practice',
                                       'Allergy/ Immunology',
                                       'Pain Management','Interventional Pain Management',
                                       'Certified Clinical Nurse Specialist','Pediatric Medicine',
                                       'Diagnostic Radiology','Undefined Physician type',
                                       'Certified Nurse Midwife','Preventive Medicine','Sleep Medicine',
                                       'Addiction Medicine','Certified Registered Nurse Anesthetist (CRNA)',
                                       'Physical Therapist in Private Practice','Clinic or Group Practice',
                                       'Pathology','Registered Dietitian or Nutrition Professional',
                                       'Medical Toxicology','Ambulance Service Provider',
                                       'Speech Language Pathologist','Licensed Clinical Social Worker',
                                       'Chiropractic','Unknown Supplier/Provider Specialty',
                                       'Osteopathic Manipulative Medicine',
                                       'Hospice and Palliative Care',
                                       
                                       'Radiation Oncology','Cardiology','Interventional Cardiology',
                                       'Hematology-Oncology','Medical Oncology','Thoracic Surgery',
                                       'Neurosurgery','Clinical Cardiac Electrophysiology','Cardiac Surgery',
                                       'Hematology','Advanced Heart Failure and Transplant Cardiology',
                                       'Vascular Surgery','Surgical Oncology','Pulmonary Disease',
                                       'Colorectal Surgery (Proctology)','Gynecological Oncology')] = 0

combo$specialist = as.factor(combo$specialist)
table(combo$specialist, exclude = NULL)

#### Super-specialty medicine ####
combo$sup_special = NULL
combo$sup_special[combo$provider_type %in% c('Radiation Oncology','Cardiology','Interventional Cardiology',
                                             'Hematology-Oncology','Medical Oncology','Thoracic Surgery',
                                             'Neurosurgery','Clinical Cardiac Electrophysiology','Cardiac Surgery',
                                             'Hematology','Advanced Heart Failure and Transplant Cardiology',
                                             'Vascular Surgery','Surgical Oncology','Pulmonary Disease',
                                             'Colorectal Surgery (Proctology)','Gynecological Oncology')] = 1
combo$sup_special[combo$provider_type %in% c('Internal Medicine','Nurse Practitioner','Family Practice',
                                            'Physician Assistant','Emergency Medicine','Orthopedic Surgery',
                                            'Obstetrics & Gynecology','General Surgery',
                                            'Physical Medicine and Rehabilitation','Infectious Disease',
                                            'Endocrinology','Hospitalist','General Practice',
                                            'Allergy/ Immunology',
                                            'Pain Management','Interventional Pain Management',
                                            'Certified Clinical Nurse Specialist','Pediatric Medicine',
                                            'Diagnostic Radiology','Undefined Physician type',
                                            'Certified Nurse Midwife','Preventive Medicine','Sleep Medicine',
                                            'Addiction Medicine','Certified Registered Nurse Anesthetist (CRNA)',
                                            'Physical Therapist in Private Practice','Clinic or Group Practice',
                                            'Pathology','Registered Dietitian or Nutrition Professional',
                                            'Medical Toxicology','Ambulance Service Provider',
                                            'Speech Language Pathologist','Licensed Clinical Social Worker',
                                            'Chiropractic','Unknown Supplier/Provider Specialty',
                                            'Osteopathic Manipulative Medicine',
                                            'Hospice and Palliative Care',
                                            
                                            'Anesthesiology','Urology','Dermatology','Psychiatry','Rheumatology',
                                            'Neurology','Nephrology','Otolaryngology',
                                            'Plastic and Reconstructive Surgery','Ophthalmology',
                                            'Gastroenterology','Geriatric Medicine','Critical Care (Intensivists)',
                                            'Podiatry','Optometry','Sports Medicine','Hand Surgery',
                                            'Interventional Radiology',
                                            'Maxillofacial Surgery','Neuropsychiatry','Peripheral Vascular Disease',
                                            'Oral Surgery (Dentist only)','Geriatric Psychiatry','Nuclear Medicine', 
                                            'Psychologist, Clinical','Dentist')] = 0
                                            
combo$specialist = as.factor(combo$specialist)
table(combo$specialist, exclude = NULL)

#### Write a csv file ####

write.csv(combo, file='recodedf.csv', row.names=F)

#### ensure that all categorical vars are factors and check the type for year ####
class(combo$gender)
combo$gender = as.factor(combo$gender)
table(combo$gender, exclude = NULL)

class(combo$year)
combo$year = as.integer(combo$year) ## This was a mistake, since most obs have none associated with them.
table(combo$year, exclude=NULL)


#### Test a k-means cluster on these data -- Time intensive, likely for the same reason as HC ####

colnames(combo)

# For the kmeans run convert the factors to integers. Create a new set of vars for this.
combo$ne = as.integer(combo$northeast)
combo$mw = as.integer(combo$midwest)
combo$so = as.integer(combo$south)
combo$we = as.integer(combo$west)
combo$pcp = as.integer(combo$basic)
combo$spc = as.integer(combo$specialist)
combo$supspc = as.integer(combo$sup_special)
combo$sex = as.integer(combo$gender)
class(combo$sex)
table(combo$sex)

kmeansdata = combo %>% select(LPS_66984,LPS_99213,LPS_99214, LPS_99223, LPS_99232, 
                               LPS_99233, LPS_99284, LPS_99285, LPS_99291,LPS_A0427, ne,
                              mw, so, we, pcp, spc, supspc, sex,
                              bene_unique_cnt_66984,bene_unique_cnt_99213, bene_unique_cnt_99214,
                              bene_unique_cnt_99223, bene_unique_cnt_99232, bene_unique_cnt_99233,
                              bene_unique_cnt_99284,bene_unique_cnt_99285, bene_unique_cnt_99291,
                              bene_unique_cnt_A0427)                          

sapply(kmeansdata, class)

## Scale all the variables in the dataset
combo_scale = as.data.frame(scale(kmeansdata))

#Conducting the K-Means algorithm on the whole dataset.
set.seed(0)
km_combo = kmeans(combo_scale, centers = 8)

#Inspecting the output of the kmeans() function.
km_combo

combo$cluscol = km_combo$cluster

colnames(combo)
summary(combo$cluscol)

#Visualizing the results against the truth.
par(mfrow = c(1, 2))
plot(combo_scale$LPS_66984, combo_scale$LPS_99213,
     xlab = "hcpcs 66984", ylab = "hcpcs 99213",
     main = "Single K-Means Attempt", col = km_combo$cluster)

#Plotting the cluster centers over the data.
par(mfrow = c(1, 1))
plot(combo_scale$LPS_66984, combo_scale$LPS_99213,
     xlab = "hcpcs 66984", ylab = "hcpcs 99213",
     main = "Single K-Means Attempt", col = km_combo$cluster)
points(km_combo$centers[, 4], km_combo$centers[, 2], pch = 16, col = "blue")

#A function to help determine the number of clusters when we do not have an
#idea ahead of time.
wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 100, nstart = 100)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}

#Visualizing the scree plot for the scaled combo data: this is not working properly
#choice.
wssplot(combo_scale)


#### Trying the weighted clustering -- not able to output clusters ####

kmeansdata1 = combo %>% select(LPS_66984,LPS_99213,LPS_99214, LPS_99223, LPS_99232, 
                              LPS_99233, LPS_99284, LPS_99285, LPS_99291,LPS_A0427, ne,
                              mw, so, we, pcp, spc, supspc, sex,
                              bene_unique_cnt_66984,bene_unique_cnt_99213, bene_unique_cnt_99214,
                              bene_unique_cnt_99223, bene_unique_cnt_99232, bene_unique_cnt_99233,
                              bene_unique_cnt_99284,bene_unique_cnt_99285, bene_unique_cnt_99291,
                              bene_unique_cnt_A0427)
weight = combo$weight

## Scale all the variables in the dataset
combo_scale1 = as.data.frame(scale(kmeansdata))

cl =  cclust(combo_scale1, k=8, save.data=TRUE, weights = weight, method="hardcl")
cl  

cl2 =  cclust(combo_scale1, k=8, weights = weight, method="hardcl")
cl2

combo$cluscol2 = cl$cluster

#### Test a hierarchical cluster on the data: unable to do it because the data are too big ####
library(flexclust) #Loading the flexclust library.

d = dist(combo_scale)

fit.single = hclust(d, method = "single")
fit.complete = hclust(d, method = "complete")
fit.average = hclust(d, method = "average")




####Do some diagnostics on the clusters ####

ggplot(data = combo,aes(x = cluscol)) + geom_bar(aes(fill = Target))
ggplot(data = combo,aes(x = cluscol)) + geom_bar()

## Set up a chi-sq test of dependence.
clustab=table(combo$Target, combo$cluscol)
clustab
chisq.test(clustab) ##significant indicating dependence.

##ANOVA is not the right test to run here, but it is significant.
summary(aov(combo$Target ~ combo$cluscol))

##Set up tests to check clusters by the hcpcs code ratio of paid to submitted.
summary(aov(combo$LPS_66984 ~ combo$cluscol)) #Significant
summary(aov(combo$LPS_99213 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99214 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99223 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99232 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99233 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99284 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99285 ~ combo$cluscol)) # "
summary(aov(combo$LPS_99291 ~ combo$cluscol)) # "
summary(aov(combo$LPS_A0427 ~ combo$cluscol)) # not significant

##Set up tests to check clusters by regions
regtab1=table(combo$northeast, combo$cluscol)
regtab1
chisq.test(regtab1) ##significant indicating dependence.

regtab2=table(combo$midwest, combo$cluscol)
regtab2
chisq.test(regtab2) ##significant indicating dependence.

regtab3=table(combo$south, combo$cluscol)
regtab3
chisq.test(regtab3) ##significant indicating dependence.

regtab4=table(combo$west, combo$cluscol)
regtab4
chisq.test(regtab4) ##significant indicating dependence.

##Set up tests to check clusters by beneficiary count
summary(aov(combo$bene_unique_cnt_66984 ~ combo$cluscol)) #Significant
summary(aov(combo$bene_unique_cnt_99213 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99214 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99223 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99232 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99233 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99284 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99285 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_99291 ~ combo$cluscol)) #"
summary(aov(combo$bene_unique_cnt_A0427 ~ combo$cluscol)) # Not significant

##Set up tests to check clusters by provider type
provtab1=table(combo$basic, combo$cluscol)
provtab1
chisq.test(provtab1) ##significant indicating dependence.

provtab2=table(combo$specialist, combo$cluscol)
provtab2
chisq.test(provtab2) ##significant indicating dependence.

provtab3=table(combo$sup_special, combo$cluscol)
provtab3
chisq.test(provtab3) ##significant indicating dependence.

##Setup tests to check clusters by gender
sextab=table(combo$sex, combo$cluscol)
sextab
chisq.test(sextab) ##significant indicating dependence.


#### Run a simple logit with weights using the same data as the KMeans, except now add weight ####

logitdata = combo %>% select(LPS_66984,LPS_99213,LPS_99214, LPS_99223, LPS_99232, 
                              LPS_99233, LPS_99284, LPS_99285, LPS_99291,LPS_A0427, northeast,
                              midwest, south, west, basic, specialist, sup_special, gender,
                              bene_unique_cnt_66984,bene_unique_cnt_99213, bene_unique_cnt_99214,
                              bene_unique_cnt_99223, bene_unique_cnt_99232, bene_unique_cnt_99233,
                              bene_unique_cnt_99284,bene_unique_cnt_99285, bene_unique_cnt_99291,
                              bene_unique_cnt_A0427, cluscol, weight, Target)                          

logit_overall=glm(Target ~ LPS_66984 + LPS_99213+LPS_99214+ LPS_99223 + LPS_99232+ 
                  LPS_99233+ LPS_99284+ LPS_99285+ LPS_99291+LPS_A0427+ northeast+
                  midwest+ south+ west+ basic+ specialist+ sup_special+ gender+
                  bene_unique_cnt_66984+bene_unique_cnt_99213+ bene_unique_cnt_99214+
                  bene_unique_cnt_99223+ bene_unique_cnt_99232+ bene_unique_cnt_99233+
                  bene_unique_cnt_99284+bene_unique_cnt_99285+ bene_unique_cnt_99291+
                  bene_unique_cnt_A0427 + cluscol,
                  data=logitdata, weights=weight)


summary(logit_overall) #Investigating the overall fit of the model.

exp(logit_overall$coefficients)
