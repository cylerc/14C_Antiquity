library(Bchron)
library(here)
library(tidyverse)
# remotes::install_github("ropensci/c14bazAAR")

dates <- read.csv(here("data/dates.csv"), stringsAsFactors = FALSE, check.names = FALSE)

# extract sites
# steep cliff cave (tham phaa can)
scc <- dates[20:31, ]

#-----------------------
library(c14bazAAR)
my_dates_tbl_c14list_calibrated_scc <- 
as.c14_date_list(scc %>% 
                   mutate(c14age = age,
                          c14std = sd)) %>% 
  calibrate(choices = "calprobdistr",
            calCurves = rep("intcal20", length(scc$age)))

my_dates_tbl_c14list_calibrated_cal_dens_scc <- 
  my_dates_tbl_c14list_calibrated_scc %>% 
  tidyr::unnest(cols = c(calprobdistr)) %>% 
  mutate(plotting_order = fct_rev(fct_inorder (as.factor(`Lab Code`))))

my_dates_tbl_c14list_calibrated_cal_dens_scc %>% 
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
  theme_bw(base_size = 6) +
  theme(
    strip.text.x = element_blank(),
    strip.background = element_rect(colour = "white", 
                                    fill = "white"),
    legend.position = c(.94, .15),
    legend.title = element_text(size = 4), 
    legend.text  = element_text(size = 4),
    legend.key.width = unit(0.25, 'cm'),
    legend.key.size = unit(0.25, "cm")
  ) +
  # layer 2 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 8.5, ymax = Inf,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 12500, y = 10, 
           label = paste0("Layer ", unique(scc$Layer)[1]), size = 3) +
  # layer 3 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 7.5, ymax = 8.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 12500, y = 8, 
           label = paste0("Layer ", unique(scc$Layer)[2]), size = 3) +
  # layer 4 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 2.5, ymax = 7.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 12500, y = 5.5, 
           label = paste0("Layer ", unique(scc$Layer)[3]), size = 3) +
  # layer 5 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = 7.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 12500, y = 1.5, 
           label = paste0("Layer ", unique(scc$Layer)[4]), size = 3) 

ggsave(here::here("figures/003-c14-ages-steep-cliff-cave.png"),
       width = 13.5,
       height = 6,
       units = "cm")



