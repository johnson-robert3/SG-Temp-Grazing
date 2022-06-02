#~~~
# Mixed effects models for P:B ratios
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


## Run "data_processing" script first.


# Load packages and install if necessary
if (!require(nlme)) install.packages('nlme')
library(nlme)



# P:B ratio model dataset
mdat_pb = comp_growth %>%
   left_join(temp_sal %>%
                select(-date)) %>%
   # add a "time" variable (for autocorrelation)
   mutate(plot = as.factor(plot)) %>%
   group_by(plot) %>%
   arrange(date, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup() %>%
   # remove NA rows
   filter(!(is.na(PB) | is.na(mean_temp)))


#--
# Effects of temperature and salinity on P:B ratios
#--


#_Full Model

# start with the most complex model: 
   # temperature, salinity, and treatment as fixed effects
   # include "plot" as a random effect to account for repeated measures sampling
   # include an autocorrelation function for "time" (numeric data; grouped within plot) to account for temporal autocorrelation in data


f1 = lme(PB ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         random = ~ 1 | plot, data = mdat_pb %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

# is the model significantly better with an AR(1) autocorrelation structure?
f2 = update(f1, correlation = corAR1(form = ~ time | plot, value = ACF(f1, form = ~ time | plot)[2,2]))

anova(f1, f2)  # yes; f2 is better model with autocorrelation

summary(update(f2, method="REML"))

# is the model significantly better without random effects?
f3 = gls(PB ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         data = mdat_pb %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")
f3 = update(f3, correlation = corAR1(form = ~ time | plot, value = ACF(f3, form = ~ time | plot)[2,2]))

anova(f2, f3)  # ns; delta_AIC = 2; use f2

# is the model better without interaction terms?
f4 = update(f1, . ~ . - mean_temp:treatment - salinity:treatment)
f4 = update(f4, correlation = corAR1(form = ~ time | plot, value = ACF(f4, form = ~ time | plot)[2,2]))

anova(f2, f4)  # no; f2 is the better model with interactions


#_Temperature only

   # is the model significantly different if salinity is removed?

t1 = update(f1, . ~ . - salinity - salinity:treatment)
t1 = update(t1, correlation = corAR1(form = ~ time | plot, value = ACF(t1, form = ~ time | plot)[2,2]))

anova(f2, t1)  # yes, significantly different when salinity is removed; f2 is better model


#_Salinity only

   # is the model significantly different if temperature is removed?

s1 = update(f1, . ~ . - mean_temp - mean_temp:treatment)
s1 = update(s1, correlation = corAR1(form = ~ time | plot, value = ACF(s1, form = ~ time | plot)[2,2]))

anova(f2, s1)  # yes, significantly different when temperature is removed; f2 is better model



#--
# Best-fit Model
#--

#_Best model for P:B ratio
# model f2
lme.pb = lme(PB ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
             random = ~ 1 | plot, data = mdat_pb %>% mutate(treatment = fct_relevel(treatment, "reference")), method="REML")

lme.pb = update(lme.pb, correlation = corAR1(form = ~ time | plot, value = ACF(lme.pb, form = ~ time | plot)[2,2]))


# model output
summary(lme.pb)
broom.mixed::tidy(lme.pb)

# R-squared values: marginal (on fixed effects only) and conditional (on fixed and random effects)
MuMIn::r.squaredGLMM(lme.pb)


   ## remove objects
   rm(f1,f2,f3,f4,t1,s1)
   ##


