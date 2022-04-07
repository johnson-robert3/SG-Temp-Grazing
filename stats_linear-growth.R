#~~~
# Mixed effects models for linear growth rates
#
# By: R. A. Johnson
# Email: rajohnson6@wisc.edu
#~~~


## Run "data_processing" script first.


# Load packages and install if necessary
if (!require(nlme)) install.packages('nlme')
library(nlme)



# Linear growth model dataset
mdat_linear = length_growth %>%
   select(-interval) %>%
   left_join(temp_sal %>%
                select(-date) %>%
                # adjust the week when it is offset by 1 from the reference treatment linear growth data nearest date
                mutate(exp_week = if_else(treatment=="reference" & exp_week==75, exp_week - 1, exp_week))) %>%
   # add a "time" variable (for autocorrelation)
   mutate(plot = as.factor(plot)) %>%
   group_by(plot) %>%
   arrange(date, .by_group=TRUE) %>%
   mutate(time = seq_len(n())) %>%
   ungroup()


#--
# Effects of temperature and salinity on linear growth
#--


#_Full Model

# start with the most complex model: 
   # temperature, salinity, and treatment as fixed effects
   # include "plot" as a random effect to account for repeated measures sampling
   # include an autocorrelation function for "time" (numeric data; grouped within plot) to account for temporal autocorrelation in data


f1 = lme(gr_length ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         random = ~ 1 | plot, data = mdat_linear %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

# is the model significantly better with an AR(1) autocorrelation structure?
f2 = update(f1, correlation = corAR1(form = ~ time | plot, value = ACF(f1, form = ~ time | plot)[2,2]))

anova(f1, f2)  # ns; f1 is better model without autocorrelation

# is the model significantly better without random effects?
f3 = gls(gr_length ~ mean_temp + salinity + treatment + mean_temp:treatment + salinity:treatment, 
         data = mdat_linear %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

anova(f1, f3)  # no; f1 is the better model

# is the model better without interaction terms?
f4 = update(f1, . ~ . - mean_temp:treatment - salinity:treatment)

anova(f1, f4)  # ns; delta_AIC < 1; use simpler model f4 without interactions

summary(update(f4, method="REML"))

# without random effects 
f5 = gls(gr_length ~ mean_temp + salinity + treatment, 
         data = mdat_linear %>% mutate(treatment = fct_relevel(treatment, "reference")), method="ML")

anova(f4, f5)  # sig.; f4 is better model

# AR1
f6 = update(f4, correlation = corAR1(form = ~ time | plot, value = ACF(f4, form = ~ time | plot)[2,2]))

anova(f4, f6)  # ns; f4 is better model


#_Temperature only

   # is the model significantly different if salinity is removed?

t1 = update(f4, . ~ . - salinity)

anova(f4, t1)  # yes, significantly different when salinity is removed; f4 is better model


#_Salinity only

   # is the model significantly different if temperature is removed?

s1 = update(f4, . ~ . - mean_temp)

anova(f4, s1)  # yes, significantly different when temperature is removed; f4 is better model



#--
# Best-fit Model
#--

#_Best model for linear growth
# model f4
lme.linear = lme(gr_length ~ mean_temp + salinity + treatment, 
                 random = ~ 1 | plot, data = mdat_linear %>% mutate(treatment = fct_relevel(treatment, "reference")), method="REML")


# model output
summary(lme.linear)
broom.mixed::tidy(lme.linear)

# R-squared values: marginal (on fixed effects only) and conditional (on fixed and random effects)
MuMIn::r.squaredGLMM(lme.linear)


   ## remove objects
   rm(f1,f2,f3,f4,f5,f6,t1,s1)
   ##

