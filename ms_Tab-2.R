#~~~
# Script to create Table 2 - best-fit model summaries
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


# Run "data_processing", "stats_linear-growth", "stats_production", "stats_PB", and "stats_LAI" scripts first


# Create tidy tables of model output
out.linear = broom.mixed::tidy(lme.linear) %>%
   mutate(mod = rep("Linear", nrow(.)))

out.prod = broom.mixed::tidy(lme.prod) %>%
   mutate(mod = rep("Prod", nrow(.)))

out.pb = broom.mixed::tidy(lme.pb) %>%
   mutate(mod = rep("P:B", nrow(.)))

out.lai = broom.mixed::tidy(lme.lai) %>%
   mutate(mod = rep("LAI", nrow(.)))


# Model summary output table
tab.mod = bind_rows(out.linear, out.prod, out.pb, out.lai) %>%
   # fixed effects only (i.e., exclude random effects output)
   filter(effect=="fixed") %>%
   relocate(mod) %>%
   select(-effect, -group) %>%
   # round to two decimals
   mutate(across(where(is.numeric), ~round(., 2))) %>%
   # combine estimate and standard error
   unite(col = "Estimate ± SE", 
         estimate, std.error, 
         sep = " ± ", 
         remove=TRUE) %>%
   # clean up fixed effect variable names
   mutate(term = case_when(term=="mean_temp" ~ 'Temperature',
                           term=="salinity" ~ 'Salinity',
                           term=="treatmentclip" ~ 'Treatment (Summer)',
                           term=="treatmentnew" ~ "Treatment (Winter)",
                           term=='mean_temp:treatmentclip' ~ 'Temperature:Treatment (Summer)',
                           term=='mean_temp:treatmentnew' ~ 'Temperature:Treatment (Winter)',
                           term=='salinity:treatmentclip' ~ 'Salinity:Treatment (Summer)',
                           term=='salinity:treatmentnew' ~ 'Salinity:Treatment (Winter)',
                           TRUE ~ term)) %>%
   # clean up column names
   rename("Fixed effect" = term,
          DF = df,
          t = statistic,
          p = p.value)


