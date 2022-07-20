#~~~
# Script to create Figure 2
#
# By: R. A. Johnson
# Email: robert.a.johnson@wisc.edu
#~~~


## Run "data_processing" and "stats_linear-growth" scripts first


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
# Linear Growth
#---

## Panel A
## reference and summer-clipped plots over time mapped to temperature

a =
ggplot(mdat_linear %>% 
          # exclude winter-clipped treatment
          filter(treatment!="winter") %>%
          # calculate mean and SE by treatment
          group_by(treatment, date) %>%
          summarize(se_gr_length = se(gr_length),
                    gr_length = mean(gr_length, na.rm=TRUE),
                    mean_temp = mean(mean_temp, na.rm=TRUE)) %>%
          ungroup()) +
   # add lines
   geom_line(aes(x = date, y = gr_length, group = treatment, linetype = treatment), size=0.5, show.legend=FALSE, color="gray40") +
   geom_errorbar(aes(x = date,
                     ymax = gr_length + se_gr_length,
                     ymin = gr_length - se_gr_length),
                 width=0, color="gray40") +
   # add white points to cover lines behind data points
   geom_point(aes(x = date, y = gr_length, shape = treatment), size=2.5, fill="white") +
   # add data points
   geom_point(aes(x = date, y = gr_length, shape = treatment, fill = mean_temp), size=2.5) +
   # scales
   scale_fill_viridis_c(option = "inferno",
                        name = expression((degree*C)),
                        alpha = 0.7) +
   scale_shape_manual(name = NULL,
                      breaks = c("reference", "summer"),
                      values = c("reference" = 22, "summer" = 21),
                      labels = c("reference" = "Reference", "summer" = "Summer-Initiated"),
                      guide = guide_legend(override.aes = list(size=2))) +
   scale_linetype_manual(name = NULL, 
                         breaks = c("reference", "summer"),
                         values = c("reference" = 2, "summer" = 1)) +
   scale_x_date(name = "Date",
                breaks = as_date(c("1999-07-01", "1999-12-01", "2000-05-01", "2000-10-01")),
                limits = as_date(c("1999-07-01", "2000-12-15")),
                date_labels = "%b %Y") +
   scale_y_continuous(name = expression(Linear~growth~(mm~d^-1)),
                      limits = c(0.9, 4),
                      breaks = seq(1, 4, 0.5)) +
   # figure aesthetics
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.text.x = element_text(hjust=0.2),
         axis.title = element_text(size=rel(0.85)), 
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")))

# extract the temperature colorbar legend
t = a +
   guides(fill = guide_colorbar(label.position = "top",
                                title.vjust = 0.2), 
          shape = "none") +
   theme(legend.background = element_blank(),
         legend.title = element_text(size = rel(0.7)),
         legend.text = element_text(size = rel(0.7)),
         legend.direction = "horizontal",
         legend.key.size = unit(3.5, "mm"))

t = get_legend(t)

# plot without colorbar legend
p1 = a +
   guides(fill = "none", 
          shape = guide_legend(override.aes = list(size=2))) +
   theme(legend.position = c(0.19, 0.13),
         legend.background = element_blank(),
         legend.text = element_text(size = rel(0.75)), 
         legend.key.size = unit(5, "mm"))


## Panel B
## summer- vs winter-initiated clipped plots over time

p2 =
ggplot(mdat_linear %>% 
          # select clipped treatment data
          filter(treatment!="reference", year(date)==2000) %>%
          # calculate mean and SE by treatment
          group_by(treatment, date) %>%
          summarize(se_gr_length = se(gr_length),
                    gr_length = mean(gr_length, na.rm=TRUE)) %>%
          ungroup()) +
   #data
   geom_errorbar(aes(x = date,
                     ymax = gr_length + se_gr_length,
                     ymin = gr_length - se_gr_length), 
                 width=0, color = "gray40") +
   geom_point(aes(x = date, y = gr_length, fill = treatment), shape=21, size=2.5) +
   # scales
   scale_fill_manual(name = NULL,
                     breaks = c("summer", "winter"), 
                     values = c("summer" = summ, "winter" = wint),
                     labels = c("summer" = "Summer-Initiated", "winter" = "Winter-Initiated"),
                     guide = guide_legend(override.aes = list(size=2))) +
   scale_x_date(name = "Date",
                limits = as_date(c("1999-12-21", "2000-12-15")),
                breaks = as_date(c("2000-01-01", "2000-04-01", "2000-07-01", "2000-10-01")), 
                date_labels = "%b %Y") +
   scale_y_continuous(name = expression(Linear~growth~(mm~d^-1)),
                      limits = c(0.9, 4),
                      breaks = seq(1, 4, 0.5)) +
   # figure aesthetics
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.text.x = element_text(hjust=0.2),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         # adjust the legend
         legend.position = c(0.19, 0.9),
         legend.background = element_blank(),
         legend.text = element_text(size=rel(0.75)),
         legend.key.size = unit(5, "mm"))


## Panel C
## Linear growth rates vs temperature

p3 =
ggplot(mdat_linear %>% 
          # calculate mean by treatment
          group_by(treatment, date) %>%
          summarize(gr_length = mean(gr_length, na.rm=TRUE),
                    mean_temp = mean(mean_temp, na.rm=TRUE)) %>%
          ungroup()) +
   # regression lines from LME model
   geom_line(data = mdat_linear %>%
                # model-predicted values
                mutate(pred = predict(update(lme.linear, .~. - salinity))) %>%
                group_by(treatment, date) %>%
                summarize(across(c(mean_temp, pred), ~mean(., na.rm=T))) %>%
                ungroup(),
             aes(x = mean_temp, y = pred, group = treatment, color = treatment),
             size=0.5, linetype=1, show.legend=FALSE) +
   scale_color_manual(breaks = c("reference", "summer", "winter"),
                      values = c("reference" = ref, "summer" = summ, "winter" = wint)) +
   # data points
   geom_point(aes(x = mean_temp, y = gr_length, fill = treatment), shape=21, size=2.5) +
   scale_fill_manual(name = NULL,
                     breaks = c("reference", "summer", "winter"), 
                     values = c("reference" = ref, "summer" = summ, "winter" = wint),
                     labels = c("reference" = "Reference", "summer" = "Summer-Initiated", "winter" = "Winter-Initiated"),
                     guide = guide_legend(override.aes = list(size=2))) +
   # plot
   scale_x_continuous(name = expression(Temperature~(degree*C)), 
                      limits = c(23, 32), 
                      breaks = seq(23, 31, 2)) +
   scale_y_continuous(name = expression(Linear~growth~(mm~d^-1)), 
                      limits = c(1, 4),
                      breaks = seq(1, 4, 0.5)) +
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


## Panel D
## Linear growth rates vs salinity

p4 =
ggplot(mdat_linear %>% 
          # calculate mean by treatment
          group_by(treatment, date) %>%
          summarize(gr_length = mean(gr_length, na.rm=TRUE),
                    salinity = mean(salinity, na.rm=TRUE)) %>%
          ungroup()) +
   # regression lines from LME model
   geom_line(data = mdat_linear %>%
                # model-predicted values
                mutate(pred = predict(update(lme.linear, .~. - mean_temp))) %>%
                group_by(treatment, date) %>%
                summarize(across(c(salinity, pred), ~mean(., na.rm=TRUE))) %>%
                ungroup(),
             aes(x = salinity, y = pred, group = treatment, color = treatment),
             linetype=1, size=0.5, show.legend=FALSE) +
   scale_color_manual(breaks = c("reference", "summer", "winter"),
                      values = c("reference" = ref, "summer" = summ, "winter" = wint)) +
   # data points
   geom_point(aes(x = salinity, y = gr_length, fill = treatment), shape=21, size=2.5) +
   scale_fill_manual(name = NULL,
                     breaks = c("reference", "summer", "winter"), 
                     values = c("reference" = ref, "summer" = summ, "winter" = wint),
                     labels = c("reference" = "Reference", "summer" = "Summer-Initiated", "winter" = "Winter-Initiated")) +
   # plot
   scale_x_continuous(name = expression(Salinity~(psu)),
                      limits = c(34, 42), 
                      breaks = seq(34, 42, 2)) +
   scale_y_continuous(name = expression(Linear~growth~(mm~d^-1)),
                      limits = c(1, 4), 
                      breaks = seq(1, 4, 0.5)) +
   theme_classic() +
   theme(panel.border = element_rect(fill=NA, color="black"),
         axis.text = element_text(color="black", size=rel(0.75)),
         axis.title = element_text(size=rel(0.85)),
         axis.title.x = element_text(margin = margin(t=0.5, unit="line")),
         axis.title.y = element_text(margin = margin(r=0.5, unit="line")),
         legend.position = "none")



# Multi-panel figure

windows(height=6, width=8)
plot_grid(p1, p2, p3, p4, 
          ncol=2, align="hv",
          labels = "auto", label_size=11,
          label_y = 0.99, label_x = 0.01) %>% 
   # add colorbar legend to p1
   ggdraw() + 
   draw_plot(t, x=0.315, y=0.58, height=0.1, width=0.2)


