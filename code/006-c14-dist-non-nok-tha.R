library(Bchron)
library(here)
library(tidyverse)
# remotes::install_github("ropensci/c14bazAAR")

dates <- read.csv(here("data/dates.csv"), stringsAsFactors = FALSE, check.names = FALSE)

# extract sites
# non nok tha (partridge mound)
nnt <- dates[46:105, ] %>% 
  arrange(positions)

#-----------------------
library(c14bazAAR)
my_dates_tbl_c14list_calibrated_nnt <- 
  as.c14_date_list(nnt %>% 
                     mutate(c14age = age,
                            c14std = sd)) %>% 
  calibrate(choices = "calprobdistr",
            calCurves = rep("intcal20", length(nnt$age)))

my_dates_tbl_c14list_calibrated_cal_dens_nnt <- 
  my_dates_tbl_c14list_calibrated_nnt %>% 
  tidyr::unnest(cols = c(calprobdistr)) %>% 
  arrange(positions) %>% 
  mutate(plotting_order = fct_rev(fct_inorder (as.factor(`Lab Code`))))

layer_text_size <- 1.7

my_dates_tbl_c14list_calibrated_cal_dens_nnt %>% 
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
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.4,.92),
        legend.title = element_text(size = 4), 
        legend.text  = element_text(size = 4),
        legend.key.width = unit(0.25, 'cm'),
        legend.key.size = unit(0.25, "cm")
  ) +
   # layer 1 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 59.5, ymax = Inf,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 60, 
           label = paste0("Layer ", unique(nnt$Layer)[1]), size = layer_text_size) +
  # layer 2 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 56.5, ymax = 59.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 58.0, 
           label = paste0("Layer ", unique(nnt$Layer)[2]), size = layer_text_size) +
  # layer 3 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 55.5, ymax = 56.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 56, 
           label = paste0("Layer ", unique(nnt$Layer)[3]), size = layer_text_size) +
  # layer 4 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 54.5, ymax = 55.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 55, 
           label = paste0("Layer ", unique(nnt$Layer)[4]), size = layer_text_size) +
  # layer 5 rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 52.5, ymax = 54.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 53.5, 
           label = paste0("Layer ", unique(nnt$Layer)[5]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 50.5, ymax = 52.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 51.5, 
           label = paste0("Layer ", unique(nnt$Layer)[6]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 48.5, ymax = 50.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 49.5, 
           label = paste0("Layer ", unique(nnt$Layer)[7]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 39.5, ymax = 48.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 44.5, 
           label = paste0("Layer ", unique(nnt$Layer)[8]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 35.5, ymax = 39.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 37.5, 
           label = paste0("Layer ", unique(nnt$Layer)[9]), size = layer_text_size)  +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 32.5, ymax = 35.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 33.5, 
           label = paste0("Layer ", unique(nnt$Layer)[10]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 31.5, ymax = 32.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 32, 
           label = paste0("Layer ", unique(nnt$Layer)[11]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 28.5, ymax = 31.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 30, 
           label = paste0("Layer ", unique(nnt$Layer)[12]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 21.5, ymax = 28.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 25.5, 
           label = paste0("Layer ", unique(nnt$Layer)[13]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 19.5, ymax = 21.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 20.5, 
           label = paste0("Layer ", unique(nnt$Layer)[14]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 12.5, ymax = 19.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 16, 
           label = paste0("Layer ", unique(nnt$Layer)[15]), size = layer_text_size) +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 11.5, ymax = 12.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 12, 
           label = paste0("Layer ", unique(nnt$Layer)[16]), size = layer_text_size)  +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = 1.5, ymax = 11.5,
            alpha = 0.1, fill = "grey30") +
  annotate("text", x = 9000, y = 5.5, 
           label = paste0("Layer ", unique(nnt$Layer)[17]), size = layer_text_size)  +
  # layer  rect
  geom_rect(data = data.frame(x = 1),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = 1.5,
            alpha = 0.1, fill = "white") +
  annotate("text", x = 9000, y = 1, 
           label = paste0("Layer ", unique(nnt$Layer)[18]), size = layer_text_size)


ggsave(here::here("figures/006-c14-ages-non-nok-tha-mound.png"),
       width = 13.5,
       height = 13,
       units = "cm")



