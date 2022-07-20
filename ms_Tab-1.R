#~~~
# Script to create Table 1 - AIC and model selection
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


# Run "data_processing", "stats_linear-growth", "stats_production", "stats_PB", and "stats_LAI" scripts first


#- LINEAR GROWTH
# model structures from 'stats_linear-growth'

# F2: treatment * (temperature + salinity) + (1|plot) + corAR1
# F1: treatment * (temperature + salinity) + (1|plot)
# F3: treatment * (temperature + salinity)
#-F4: treatment + temperature + salinity   + (1|plot)            <- best model
# T1: treatment + temperature              + (1|plot)
# S1: treatment + salinity                 + (1|plot)


#- PRODUCTION
# model structures from 'stats_production'

# F2: treatment * (temperature + salinity) + (1|plot) + corAR1
# F1: treatment * (temperature + salinity) + (1|plot)
# F3: treatment * (temperature + salinity)            + corAR1
# F4: treatment + temperature + salinity   + (1|plot) + corAR1
#-T1: treatment + temperature              + (1|plot) + corAR1   <- best model
# S1: treatment + salinity                 + (1|plot) + corAR1


#- P:B RATIO
# model structures from 'stats_PB'

#-F2: treatment * (temperature + salinity) + (1|plot) + corAR1   <- best model
# F1: treatment * (temperature + salinity) + (1|plot)
# F3: treatment * (temperature + salinity)            + corAR1
# F4: treatment + temperature + salinity   + (1|plot) + corAR1
# T1: treatment * temperature              + (1|plot) + corAR1
# S1: treatment * salinity                 + (1|plot) + corAR1


#- LEAF AREA INDEX
# model structures from 'stats_LAI'

# F2: treatment * (temperature + salinity) + (1|plot) + corAR1
# F1: treatment * (temperature + salinity) + (1|plot)
# F3: treatment * (temperature + salinity)            + corAR1
#-F5: treatment * temperature + salinity   + (1|plot) + corAR1   <- best model
# F4: treatment + temperature + salinity   + (1|plot) + corAR1
# T1: treatment * temperature              + (1|plot) + corAR1
# S2: treatment + salinity                 + (1|plot) + corAR1



#- AIC table output

tab.aic = aic.linear %>% mutate(response = "Linear Growth") %>%
   bind_rows(aic.prod %>% mutate(response = "Production")) %>%
   bind_rows(aic.pb %>% mutate(response = "P:B Ratio")) %>%
   bind_rows(aic.lai %>% mutate(response = "Leaf Area Index")) %>%
   relocate(response, .after=model) %>%
   rename(DF = df,
          "Response variable" = response) %>%
   mutate(AIC = round(AIC, 1))


