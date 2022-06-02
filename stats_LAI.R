#~~~
# Mixed effects models for linear growth rates
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


## Run "data_processing" script first.


# Load packages and install if necessary
if (!require(nlme)) install.packages('nlme')
library(nlme)



# LAI model dataset
mdat_lai = sg_structure %>% 
   left_join(temp_sal %>% 
                select(-date) %>%
                # adjust the weeks that are offset from the winter treatment structure data nearest date
                mutate(exp_week = if_else(treatment=="winter" & exp_week %in% c(58, 66, 76), exp_week + 1, exp_week))) %>%
   # exclude pre-clipping LAI values for experimentally clipped plots
   filter(!(treatment %in% c("summer", "winter") & interval==1)) %>%
   # add a "time" variable (for autocorrelation)
   mutate(plot = as.factor(plot)) %>%
   group_by(plot) %>%
   arrange(date, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup()


#--
# Effects of temperature and salinity on leaf area index
#--


#_Full Model

# start with the most complex model: 
   # temperature, salinity, and treatment as fixed effects
   # include "plot" as a random effect to account for repeated measures sampling
   # include an autocorrelation function for "time" (numeric data; grouped within plot) to account for temporal autocorrelation in data


f1 = lme(lai ~ treatment + mean_temp + salinity + mean_temp:treatment + salinity:treatment, 
         random = ~ 1 | plot, data = mdat_lai %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

# is the model significantly better with an AR(1) autocorrelation structure?
f2 = update(f1, correlation = corAR1(form = ~ time | plot, value = ACF(f1, form = ~ time | plot)[2,2]))

anova(f1, f2)  # yes; f2 is better model with autocorrelation

# is the model significantly better without random effects?
f3 = gls(lai ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         data = mdat_lai %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")
f3 = update(f3, correlation = corAR1(form = ~ time | plot, value = ACF(f3, form = ~ time | plot)[2,2]))

anova(f2, f3)  # no; f2 is the better model

# is the model better without interaction terms?
f4 = update(f1, . ~ . - mean_temp:treatment - salinity:treatment)
f4 = update(f4, correlation = corAR1(form = ~ time | plot, value = ACF(f4, form = ~ time | plot)[2,2]))

anova(f2, f4)  # no; f2 is better model

# is the model better with a temp interaction, but no salinity interaction?
f5 = update(f1, . ~ . - salinity:treatment)
f5 = update(f5, correlation = corAR1(form = ~ time | plot, value = ACF(f5, form = ~ time | plot)[2,2]))

anova(f4, f5)  # yes, f5 is the better model
anova(f2, f5)  # ns; delta_AIC = 0.2; use simpler model f5 (f5 also has fewer DF)

summary(update(f5, method="REML"))


#_Temperature only

   # is the model significantly different if salinity is removed?

t1 = update(f1, . ~ . - salinity - salinity:treatment)
t1 = update(t1, correlation = corAR1(form = ~ time | plot, value = ACF(t1, form = ~ time | plot)[2,2]))

anova(f2, t1)  # sig., delta_AIC = 3.85
anova(f5, t1)  # sig., delta_AIC = 3.63; f5 is better

summary(update(t1, method="REML"))
MuMIn::r.squaredGLMM(update(t1, method="REML"))


#_Salinity only

   # is the model significantly different if temperature is removed?

s1 = update(f1, . ~ . - mean_temp - mean_temp:treatment)
s1 = update(s1, correlation = corAR1(form = ~ time | plot, value = ACF(s1, form = ~ time | plot)[2,2]))

anova(f2, s1)  # yes, significantly different when temperature is removed
anova(f5, s1)  # yes; f5 is better model

# without interaction
s2 = update(f1, . ~ . - mean_temp - mean_temp:treatment - salinity:treatment)
s2 = update(s2, correlation = corAR1(form = ~ time | plot, value = ACF(s2, form = ~ time | plot)[2,2]))

anova(s1, s2)  # ns; use simpler model s2 without an interaction
anova(f5, s2)  # sig.; f5 is better model

summary(update(s2, method="REML"))  # salinity does not have a significant effect on LAI (when it is the only environmental variable)



#--
# Best-fit Model
#--

#_Best model for LAI
# model f5
lme.lai = lme(lai ~ mean_temp + salinity + treatment + mean_temp:treatment, 
              random = ~ 1 | plot, data = mdat_lai %>% mutate(treatment = fct_relevel(treatment, "reference")), method="REML")

lme.lai = update(lme.lai, correlation = corAR1(form = ~ time | plot, value = ACF(lme.lai, form = ~ time | plot)[2,2]))


# model output
summary(lme.lai)
broom.mixed::tidy(lme.lai)

# R-squared values: marginal (on fixed effects only) and conditional (on fixed and random effects)
MuMIn::r.squaredGLMM(lme.lai)


   ## remove objects
   rm(f1,f2,f3,f4,f5,t1,s1,s2)
   ##

