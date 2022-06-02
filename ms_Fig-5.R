#~~~
# Script to create Figure 5
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


## Run "data_processing" and "stats_LAI" scripts first


# Load packages and install if necessary
if (!require(cowplot)) install.packages('cowplot')
library(cowplot)

if (!require(viridis)) install.packages('viridis')
library(viridis)


# colors for data points
ref = "#a8dda8"
summ = "#51c2d5"
wint = "#0f3460"


#---
# Leaf Area Index
#---

## Panel A
## LAI over time, all treatments

# windows(height=3, width=4)
p1 =
ggplot(mdat_lai %>%
          # calculate mean and SE by treatment
          group_by(treatment, date) %>%
          summarize(se_lai = se(lai),
                    lai = mean(lai, na.rm=TRUE)) %>%
          ungroup()) +  
   # data
   geom_errorbar(aes(x = date,
                     ymax = lai + se_lai,
                     ymin = lai - se_lai), 
                 width=0, color = "gray40") +
   geom_point(aes(x = date, y = lai, fill = treatment), shape=21, size=2.5) +
   # scales
   scale_fill_manual(name = "Treatment",
                     breaks = c("reference", "summer", "winter"), 
                     values = c("reference" = ref, "summer" = summ, "winter" = wint)) +
   scale_x_date(name = "Date",
                limits = as_date(c("1999-07-01", "2000-12-15")),
                breaks = as_date(c("1999-07-01", "1999-12-01", "2000-05-01", "2000-10-01")),
                date_labels = "%b %Y") +
   scale_y_continuous(name = "Leaf area index", 
                      limits = c(0, 2.5), 
                      breaks = seq(0, 2.5, 0.5)) +
   # figure aesthetics
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.text.x = element_text(hjust=0.2),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         legend.position = "none")


## Panel B
## LAI vs temperature

# windows(height=3, width=4)
p2 =
ggplot(mdat_lai %>%
          # calculate treatment means
          group_by(treatment, date) %>%
          summarize(lai = mean(lai, na.rm=TRUE),
                    mean_temp = mean(mean_temp, na.rm=TRUE)) %>%
          ungroup()) +
   # regression lines from LME model
   geom_line(data = mdat_lai %>%
                # model-predicted values
                mutate(pred = predict(update(lme.lai, .~. - salinity))) %>%
                group_by(treatment, date) %>%
                summarize(across(c(mean_temp, pred), ~mean(., na.rm=TRUE))) %>%
                ungroup() %>%
                # only reference treatment is significant in model
                filter(treatment=="reference"),
             aes(x = mean_temp, y = pred), color = ref,
             linetype=1, size=0.5, show.legend=FALSE) +
   # data points
   geom_point(aes(x = mean_temp, y = lai, fill = treatment), shape=21, size=2.5) +
   scale_fill_manual(name = NULL,
                     breaks = c("reference", "summer", "winter"), 
                     values = c("reference" = ref, "summer" = summ, "winter" = wint),
                     labels = c("reference" = "Reference", "summer" = "Summer-Initiated", "winter" = "Winter-Initiated"),
                     guide = guide_legend(override.aes = list(size=2))) +
   # plot
   scale_x_continuous(name = expression(Temperature~(degree*C)), 
                      limits = c(23, 32), 
                      breaks = seq(23, 31, 2)) +
   scale_y_continuous(name = "Leaf area index", 
                      limits = c(0, 2.7), 
                      breaks = seq(0, 2.5, 0.5)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         # adjust the legend
         legend.position = c(0.19, 0.86),
         legend.background = element_blank(),
         legend.text = element_text(size=rel(0.75)),
         legend.key.size = unit(5, "mm"))


## Panel C
## LAI vs salinity

# windows(height=3, width=4)
p3 =
ggplot(mdat_lai %>%
          # calculate treatment means
          group_by(treatment, date) %>%
          summarize(lai = mean(lai, na.rm=TRUE),
                    salinity = mean(salinity, na.rm=TRUE)) %>%
          ungroup()) +
   # data points
   geom_point(aes(x = salinity, y = lai, fill = treatment), shape=21, size=2.5) +
   scale_fill_manual(name = NULL,
                     breaks = c("reference", "summer", "winter"), 
                     values = c("reference" = ref, "summer" = summ, "winter" = wint),
                     labels = c("reference" = "Reference", "summer" = "Summer-Initiated", "winter" = "Winter-Initiated")) +
   # plot
   scale_x_continuous(name = expression(Salinity~('‰')),
                      limits = c(35, 41.5),
                      breaks = seq(35, 41, 2)) +
   scale_y_continuous(name = "Leaf area index", 
                      limits = c(0, 2.5), 
                      breaks = seq(0, 2.5, 0.5)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         legend.position = "none")


# Multi-panel figure

windows(height = 9, width=4)
plot_grid(p1, p2, p3, 
          ncol=1, align="v",
          labels = "auto", label_size=11,
          label_y = 0.99, label_x = 0.01)


