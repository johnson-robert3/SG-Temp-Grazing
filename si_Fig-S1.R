#~~~
# Script to create supplemental Figure S1
#
# By: R. A. Johnson
# Email: rajohnson6@wisc.edu
#~~~


## Run "data_processing" script first


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
# Aboveground Biomass
#---

windows(height=3.3, width=4)
ggplot(ag_biomass %>%
          # exclude pre-clipping measurements
          filter(interval!=0) %>%
          # calculate mean and SE by treatment
          group_by(treatment, date) %>%
          summarize(se_ag_biomass = se(ag_biomass),
                    ag_biomass = mean(ag_biomass, na.rm=TRUE)) %>%
          ungroup()) +
   # data
   geom_errorbar(aes(x = date,
                     ymax = ag_biomass + se_ag_biomass,
                     ymin = ag_biomass - se_ag_biomass), 
                 color = "gray60", width=0) +
   geom_point(aes(x = date, y = ag_biomass, fill = treatment), shape=21, size=2.5) +
   #plot
   scale_fill_manual(name = NULL,
                     breaks = c("reference", "summer", "winter"),
                     values = c("reference" = ref, "summer" = summ, "winter" = wint),
                     labels = c("reference" = "Reference", "summer" = "Summer-Initiated", "winter" = "Winter-Initiated"),
                     guide = guide_legend(override.aes = list(size=2))) +
   scale_x_date(name = "Date",
                limits = as_date(c("1999-07-01", "2000-12-15")),
                breaks = as_date(c("1999-07-01", "1999-12-01", "2000-05-01", "2000-10-01")),
                date_labels = "%b %Y") +
   scale_y_continuous(name = expression(Aboveground~biomass~(g~DM~m^-2)),
                      limits = c(0, 100),
                      breaks = seq(0, 100, 20)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.text.x = element_text(hjust=0.2),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         # adjust the legend
         legend.direction = "horizontal",
         legend.position = "top",
         legend.margin = margin(c(0,0,-3,0)),
         legend.background = element_blank(),
         legend.text = element_text(size=rel(0.75)),
         legend.key.size = unit(5, "mm"))


