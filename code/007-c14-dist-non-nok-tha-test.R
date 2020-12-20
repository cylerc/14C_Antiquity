library(Bchron)
library(here)
library(tidyverse)
# remotes::install_github("ropensci/c14bazAAR")

dates <- read.csv(here("data/dates.csv"), stringsAsFactors = FALSE, check.names = FALSE)

# extract sites
# non nok tha test excavation
nnt_green <- dates[c(106, 108:109), ]

#-----------------------
library(c14bazAAR)
my_dates_tbl_c14list_calibrated_nnt_green <- 
  as.c14_date_list(nnt_green %>% 
                     mutate(c14age = age,
                            c14std = sd)) %>% 
  calibrate(choices = "calprobdistr",
            calCurves = rep("intcal20", length(nnt_green$age)))

my_dates_tbl_c14list_calibrated_cal_dens_nnt_green <- 
  my_dates_tbl_c14list_calibrated_nnt_green %>% 
  tidyr::unnest(cols = c(calprobdistr)) %>% 
  arrange(positions) %>% 
  mutate(plotting_order = fct_rev(fct_inorder (as.factor(`Lab Code`))))

layer_text_size <- 3

my_dates_tbl_c14list_calibrated_cal_dens_nnt_green %>% 
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
  scale_x_reverse() +
  xlab("Calibrated date (cal BP)") +
  ylab("") +
  theme_bw(base_size = 12) +
  guides(fill = FALSE)  +
  # layer 1 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 2.5, ymax = Inf,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 2800, y = 3.5, 
           label = paste0("Layer/Level ", unique(nnt_green$`Layer/Level`)[1]), size = layer_text_size) +
  # layer 2 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 1.5, ymax = 2.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 2500, y = 2, 
           label = paste0("Layer/Level ", unique(nnt_green$`Layer/Level`)[2]), size = layer_text_size) +
  # layer 3 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = 1.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 2500, y = 1, 
           label = paste0("Layer/Level ", unique(nnt_green$`Layer/Level`)[3]), size = layer_text_size) #

ggsave(here::here("figures/007-c14-ages-non-nok-tha-test.png"),
       width = 13.5,
       height = 13,
       units = "cm")



