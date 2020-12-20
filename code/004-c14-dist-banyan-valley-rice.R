library(Bchron)
library(here)
library(tidyverse)
# remotes::install_github("ropensci/c14bazAAR")

dates <- read.csv(here("data/dates.csv"), stringsAsFactors = FALSE, check.names = FALSE)

# extract sites
# banyan valley cave (tham boong hoong)
bvc_rice <- dates[32:34, ]

#-----------------------
library(c14bazAAR)
my_dates_tbl_c14list_calibrated_bvc_rice <- 
  as.c14_date_list(bvc_rice %>% 
                     mutate(c14age = age,
                            c14std = sd)) %>% 
  calibrate(choices = "calprobdistr",
            calCurves = rep("intcal20", length(sc$age)))

my_dates_tbl_c14list_calibrated_cal_dens_bvc_rice <- 
  my_dates_tbl_c14list_calibrated_bvc_rice %>% 
  tidyr::unnest(cols = c(calprobdistr)) %>% 
  mutate(plotting_order = fct_rev(fct_inorder (as.factor(`Lab Code`))))

my_dates_tbl_c14list_calibrated_cal_dens_bvc_rice %>% 
  ggplot() +
  # a special geom for ridgeplots is provided by the ggridges package
  ggridges::geom_ridgeline(
    # the relevant variables that have to be mapped for this geom are 
    # x (the time -- here the calibrated age transformed to calBC), 
    # y (the individual lab number of the dates) and
    # height (the probability for each year and date) 
    aes(x = calage, 
        y = plotting_order  , 
        height = density,
        fill = Material),
    # ridgeplots lack a scientifically clear y axis for each 
    # distribution plot and we can adjust the scaling to our needs
    scale = 100,
    size = 0.25
  ) +
  guides(fill = FALSE) +
  scale_x_reverse() +
  xlab("Calibrated date (cal BP)") +
  ylab("") +
  theme_bw(base_size = 10) +
  theme(strip.text.x = element_blank()
  ) +
  # layer 1 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 2.5, ymax = Inf,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 100, y = 3.5, 
           label = paste0("Layer ", unique(bvc_rice$Layer)[1]), size = 3) +
  # layer 2 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = 2.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 100, y = 1.5, 
           label = paste0("Layer ", unique(bvc_rice$Layer)[2]), size = 3) #


ggsave(here::here("figures/004-c14-ages-banyan-valley-rice.png"),
       width = 13.5,
       height = 10,
       units = "cm")



