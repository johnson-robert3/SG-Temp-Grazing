#~~~
# Mixed effects models for mass growth rates (production)
#
# By: R. A. Johnson
# Email: rajohnson6@wisc.edu
#~~~


## Run "data_processing" script first.


# Load packages and install if necessary
if (!require(nlme)) install.packages('nlme')
library(nlme)



# Production model dataset
mdat_prod = mass_growth %>%
   select(-interval) %>%
   left_join(temp_sal %>%
                select(-date) %>%
                # adjust the week when it is offset by 1 from the reference treatment production data nearest date
                mutate(exp_week = if_else(treatment=="reference" & exp_week==75, exp_week - 1, exp_week))) %>%
   # add a "time" variable (for autocorrelation)
   mutate(plot = as.factor(plot)) %>%
   group_by(plot) %>%
   arrange(date, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup()


#--
# Effects of temperature and salinity on production
#--


#_Full Model

# start with the most complex model: 
   # temperature, salinity, and treatment as fixed effects
   # include "plot" as a random effect to account for repeated measures sampling
   # include an autocorrelation function for "time" (numeric data; grouped within plot) to account for temporal autocorrelation in data


f1 = lme(gr_mass ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         random = ~ 1 | plot, data = mdat_prod %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

# is the model significantly better with an AR(1) autocorrelation structure?
f2 = update(f1, correlation = corAR1(form = ~ time | plot, value = ACF(f1, form = ~ time | plot)[2,2]))

anova(f1, f2)  # yes; f2 is better model with autocorrelation

# is the model significantly better without random effects?
f3 = gls(gr_mass ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         data = mdat_prod %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")
f3 = update(f3, correlation = corAR1(form = ~ time | plot, value = ACF(f3, form = ~ time | plot)[2,2]))

anova(f2, f3)  # no; f2 is the better model

# is the model better without interaction terms?
f4 = update(f1, . ~ . - mean_temp:treatment - salinity:treatment)
f4 = update(f4, correlation = corAR1(form = ~ time | plot, value = ACF(f4, form = ~ time | plot)[2,2]))

anova(f2, f4)  # ns; use simpler model f4

summary(update(f4, method="REML"))

# without random effects 
f5 = gls(gr_mass ~ mean_temp + salinity + treatment, 
         data = mdat_prod %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")
f5 = update(f5, correlation = corAR1(form = ~ time | plot, value = ACF(f5, form = ~ time | plot)[2,2]))

anova(f4, f5)  # sig.; f4 is better model


#_Temperature only

   # is the model significantly different if salinity is removed?

t1 = update(f1, . ~ . - salinity - mean_temp:treatment - salinity:treatment)
t1 = update(t1, correlation = corAR1(form = ~ time | plot, value = ACF(t1, form = ~ time | plot)[2,2]))

anova(f4, t1)  # ns; i.e., model is no better when salinity is included; delta_AIC < 1; simpler model t1 is better

summary(update(t1, method="REML"))


#_Salinity only

   # is the model significantly different if temperature is removed?

s1 = update(f1, . ~ . - mean_temp - mean_temp:treatment - salinity:treatment)
s1 = update(s1, correlation = corAR1(form = ~ time | plot, value = ACF(s1, form = ~ time | plot)[2,2]))

anova(f4, s1)  # yes, significantly different when temperature is removed; f4 is better model



#--
# Best-fit Model
#--

#_Best model for production
# model t1
lme.prod = lme(gr_mass ~ mean_temp + treatment, 
               random = ~ 1 | plot, data = mdat_prod %>% mutate(treatment = fct_relevel(treatment, "reference")), method="REML")

lme.prod = update(lme.prod, correlation = corAR1(form = ~ time | plot, value = ACF(lme.prod, form = ~ time | plot)[2,2]))


# model output
summary(lme.prod)
broom.mixed::tidy(lme.prod)

# R-squared values: marginal (on fixed effects only) and conditional (on fixed and random effects)
MuMIn::r.squaredGLMM(lme.prod)


   ## remove objects
   rm(f1,f2,f3,f4,f5,t1,s1)
   ##

