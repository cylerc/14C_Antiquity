library(Bchron)
library(here)

dates <- read.csv(here("data/dates.csv"), stringsAsFactors = FALSE, check.names = FALSE)

# extract sites
# spirit cave (tham phii maen)
sc <- dates[1:19, ]

#-----------------------
library(c14bazAAR)
my_dates_tbl_c14list_calibrated_sc <- 
  as.c14_date_list(sc %>% 
                     mutate(c14age = age,
                            c14std = sd)) %>% 
  calibrate(choices = "calprobdistr",
            calCurves = rep("intcal20", length(sc$age)))

my_dates_tbl_c14list_calibrated_cal_dens_sc <- 
  my_dates_tbl_c14list_calibrated_sc %>% 
  tidyr::unnest(cols = c(calprobdistr)) %>% 
  mutate(plotting_order = fct_rev(fct_inorder (as.factor(`Lab Code`))))


my_dates_tbl_c14list_calibrated_cal_dens_sc %>%
  ggplot() +
  # a special geom for ridgeplots is provided by the ggridges package
  ggridges::geom_ridgeline(
    # the relevant variables that have to be mapped for this geom are
    # x (the time -- here the calibrated age transformed to calBC),
    # y (the individual lab number of the dates) and
    # height (the probability for each year and date)
    aes(
      x = calage,
      y = plotting_order  ,
      height = density,
      fill = Material
    ),
    scale = 100,
    size = 0.25) +
    
    theme_minimal() +
      # layer 1 rect
      geom_rect(
        data = data.frame(x = 1),
        xmin = -Inf,
        xmax = Inf,
        ymin = 15.5,
        ymax = Inf,
        alpha = 0.1,
        fill = "grey30"
      ) +
      annotate(
        "text",
        x = 14000,
        y = 18,
        label = paste0("Layer ", unique(sc$Layer)[1]),
        size = 3
      ) +
      # layer 2 rect
      geom_rect(
        data = data.frame(x = 1),
        xmin = -Inf,
        xmax = Inf,
        ymin = 11.5,
        ymax = 15.5,
        alpha = 0.1,
        fill = "white"
      ) +
      annotate(
        "text",
        x = 14000,
        y = 14,
        label = paste0("Layer ", unique(sc$Layer)[2]),
        size = 3
      ) +
      # layer 2a rect
      geom_rect(
        data = data.frame(x = 1),
        xmin = -Inf,
        xmax = Inf,
        ymin = 9.5,
        ymax = 11.5,
        alpha = 0.1,
        fill = "grey30"
      ) +
      annotate(
        "text",
        x = 14000,
        y = 10.5,
        label = paste0("Layer ", unique(sc$Layer)[3]),
        size = 3
      ) +
      # layer 3 or 4 rect
      geom_rect(
        data = data.frame(x = 1),
        xmin = -Inf,
        xmax = Inf,
        ymin = 8.5,
        ymax = 9.5,
        alpha = 0.1,
        fill = "white"
      ) +
      annotate(
        "text",
        x = 14000,
        y = 9,
        label = paste0("Layer ", unique(sc$Layer)[4]),
        size = 3
      ) +
      # layer  rect
      geom_rect(
        data = data.frame(x = 1),
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = 8.5,
        alpha = 0.1,
        fill = "grey30"
      ) +
      annotate(
        "text",
        x = 14000,
        y = 4,
        label = paste0("Layer ", unique(sc$Layer)[5]),
        size = 3
      ) +
      scale_x_reverse() +
      xlab("Calibrated date (cal BP)") +
      ylab("") +
      theme_bw(base_size = 10) +
      theme(
        strip.text.x = element_blank(),
        strip.background = element_rect(colour = "white", 
                                        fill = "white"),
        legend.position = c(.85, .2)
      )



ggsave(
  here::here("figures/002-c14-ages-spirit-cave.png"),
  width = 13.5,
  height = 10,
  units = "cm"
)

