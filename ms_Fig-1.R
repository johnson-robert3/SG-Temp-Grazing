#~~~
# Script to create Figure 1
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


#---
# Temperature & Salinity
#---

windows(height=3, width=4)
ggplot(temp_sal %>% 
          # use only reference treatment data for plotting (summer-clip data are the same; winter-clip data are only after Feb. 2000)
          filter(treatment=="reference")) +
   # temp and salinity data points
   geom_point(aes(x = date, y = mean_temp, fill = mean_temp), shape=21, size=2) +
   geom_point(aes(x = date, y = salinity), shape=21, size=2, fill="gray70") +
   # scales
   scale_fill_viridis_c(option = "inferno",
                        name = expression((degree*C)),
                        alpha = 0.7,
                        guide = guide_colorbar(label.position="top",
                                               title.vjust = 0.2)) +
   scale_y_continuous(name = expression(Temperature~(degree*C)),
                      limits = c(20, 45),
                      sec.axis = sec_axis(~ ., name = "Salinity (ppt)")) +
   scale_x_date(name = "Date",
                breaks = as_date(c("1999-07-01", "1999-12-01", "2000-05-01", "2000-10-01")),
                limits = as_date(c("1999-07-01", "2000-12-15")),
                date_labels = "%b %Y") +
   # figure aesthetics
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         # adjust position of x-axis date labels
         axis.text.x = element_text(hjust=0.2),
         # add padding around axis labels
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y.left = element_text(margin = margin(r=0.5, unit="line")),
         axis.title.y.right = element_text(margin = margin(l=1, unit="line")),
         # adjust placement of colorbar legend
         legend.position = c(0.8, 0.1),
         legend.direction = "horizontal",
         # adjust size of legend
         legend.title = element_text(size = rel(0.7)),
         legend.text = element_text(size = rel(0.7)),
         legend.key.size = unit(3.5, "mm"))


