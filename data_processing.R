#~~~
# Load packages, process data sets
# 
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


# Load packages and install if necessary
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)


# Function for Standard Error
se = function(.dat, na.rm=TRUE) { sd(.dat) / sqrt(length(.dat)) }


### Run the "data_import.R" script first to download and import archived data sets from EDI.


#--
# Process data sets
#--

# Update data set names and column types 

## Environmental Data
temp_sal = dt5 %>%
   mutate(treatment = as.character(treatment)) %>%
   mutate(across(where(is.integer), ~as.double(.))) %>%
   # calculate mean temperature variable
   #  average between weekly minimum and maximum temperatures
   mutate(mean_temp = (min_temp + max_temp) / 2)


#_Seagrass Structure
sg_structure = dt4 %>%
   mutate(treatment = as.character(treatment)) %>%
   mutate(across(where(is.integer), ~as.double(.)))


#_Seagrass Aboveground Biomass
ag_biomass = dt1 %>%
   mutate(treatment = as.character(treatment)) %>%
   mutate(across(where(is.integer), ~as.double(.)))


#_Seagrass Linear Growth Rates
length_growth = dt2 %>%
   mutate(treatment = as.character(treatment)) %>%
   mutate(across(where(is.integer), ~as.double(.)))


#_Seagrass Rates of Production
mass_growth = dt3 %>%
   mutate(treatment = as.character(treatment)) %>%
   mutate(across(where(is.integer), ~as.double(.)))



#--
# P:B Ratios
#--

# calculate P:B ratios from production and aboveground biomass data sets

#_Reference plots
ref_plots = mass_growth %>%
   filter(treatment=="reference") %>%
   # align production measurements with biomass measurements 
   #  biomass measured on: 1999-07-20, 1999-09-23, 2000-01-22, 2000-06-24, 2000-11-21
   filter(interval %in% c(1, 4, 16, 27, 37)) %>%
   # combine by interval variable
   mutate(interval = case_when(interval==1 ~ 1,
                               interval==4 ~ 2,
                               interval==16 ~ 3,
                               interval==27 ~ 4,
                               interval==37 ~ 5)) %>%
   select(-date, - exp_week) %>%
   # add growth rates to biomass data set
   right_join(ag_biomass %>%
                 filter(treatment=="reference"))


#_Experimental Clipped plots
clip_plots = mass_growth %>%
   filter(treatment %in% c("summer", "winter")) %>%
   # add growth rates to biomass data set
   right_join(ag_biomass %>%
                 filter(treatment %in% c("summer", "winter")))


#_Combine
comp_growth = full_join(ref_plots, clip_plots) %>%
   relocate(gr_mass, .after=last_col()) %>%
   relocate(date, .before=interval) %>%
   # P:B ratio
   mutate(PB = (gr_mass / ag_biomass) * 100)


# remove temporary objects
rm(ref_plots, clip_plots)

