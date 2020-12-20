library(c14bazAAR)
library(tidyverse)
library(oxcAAR)

#---------------------------------------------------
my_dates_tbl <- 
   tribble(
~"name",                     ~"c14age", ~"c14std", ~"layer", ~"material",
"UGAMS-29451 Bone apatite" , 7460,     30,            2,        "bone",
"UGAMS-29448 M. laosensis" , 8096,     30,            2,        "shell",
"UGAMS-29452 Bone apatite" , 8180,     30,            2,        "bone",
"UGAMS-29455 Charcoal"     , 8300,     30,            2,        "charcoal",
"GaK-4531 Charcoal"        , 7497,    160,            3,        "charcoal",
"GaK-4530 Charcoal"        , 5178,    110,            4,        "charcoal",
"UGAMS-29453 Bone apatite" , 8100,     30,            4,        "bone",
"UGAMS-29449 M. laosensis" , 8806,     30,            4,        "shell",
"UGAMS-29447 Canarium sp." , 9140,     30,            4,        "shell",
"UGAMS-29456 Charcoal"     , 9960,     30,            4,        "charcoal",
"UGAMS-29454 Bone apatite" , 9020,     30,            5,        "bone",
"UGAMS-29450 M. laosensis" , 9456,     30,            5,        "shell"
)

my_dates_tbl_c14list <- 
   as.c14_date_list(my_dates_tbl)

my_dates_tbl_c14list_calibrated <- 
   my_dates_tbl_c14list %>%
   calibrate(choices = "calprobdistr")

my_dates_tbl_c14list_calibrated_cal_dens <- 
   my_dates_tbl_c14list_calibrated %>% 
   tidyr::unnest(cols = c(calprobdistr)) %>% 
   mutate(name = fct_rev(fct_inorder (as.factor(name))))

my_dates_tbl_c14list_calibrated_cal_dens %>%
   ggplot() +
   # a special geom for ridgeplots is provided by the ggridges package
   ggridges::geom_ridgeline(
      # the relevant variables that have to be mapped for this geom are 
      # x (the time -- here the calibrated age transformed to calBC), 
      # y (the individual lab number of the dates) and
      # height (the probability for each year and date) 
      aes(x = calage, 
          y = name  , 
          height = density,
          fill = material),
      # ridgeplots lack a scientifically clear y axis for each 
      # distribution plot and we can adjust the scaling to our needs
      scale = 60
   ) +
   # layer 5 rect
   geom_rect(data = data.frame(x = 1),
             xmin = -Inf, xmax = Inf, 
             ymin = 0.5, ymax = 2.5,
             alpha = 0.1, fill = "grey30") +
   annotate("text", x = 11500, y = 2.2, 
            label = "Layer 5", size = 3) +
   # layer 4
   annotate("text", x = 11500, y = 7.2, 
            label = "Layer 4", size = 3) +
   # layer 3
   geom_rect(data = data.frame(x = 1),
             xmin = -Inf, xmax = Inf, 
             ymin = 7.5, ymax = 8.5,
             alpha = 0.1, fill = "grey30") +
   annotate("text", x = 11500, y = 8.2, 
            label = "Layer 3", size = 3) +
   # layer 2
   annotate("text", x = 11500, y = 12.2, 
            label = "Layer 2", size = 3) +
   scale_x_reverse() +
   xlab("Calibrated date (cal BP)") +
   ylab("") +
   theme_bw() +
   theme(strip.text.x = element_blank(),
         strip.background = element_rect(colour="white", fill="white"),
         legend.position=c(.9,.35)
   ) 

