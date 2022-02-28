# Modeling the source and exposure of Pb contaminated potable water in Scotland----
## Kate Moloney
## Dissertation data analysis 2021/2022

getwd()
library(dplyr)
library(tidyverse) 
library(ggplot2)

Pb_data <- read.csv("Pb_tidy.csv")
str(Pb_data)


### Setting theme----
theme_ps <- function(){            # creating a new theme function
  theme(axis.title = element_text(size = 14,
                                  face = "bold"),
        axis.text.x = element_text(size = 11,
                                   vjust = 1,
                                   face = "bold"), 
        axis.text.y = element_text(size = 11, face = "bold"),  # define font,
        # font sizes, alignment
        #legend.position = "none",  # remove legend
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # create plot
        # margins
        panel.grid = element_blank())
}

### Setting plot save function ----
plot_save <- function(plot_name, # first put the plot object name
                      file_name = "plot", # give it a title 
                      width = 13, # set the width, height and dpi
                      height = 8, 
                      dpi = 150) {
  
  ggsave(
    paste0(file_name, ".png"), plot_name, width = width,  # save as png
    height = height, dpi = dpi) 
  
  ggsave(
    paste0(file_name, ".pdf"), plot_name, width = width, # save as pdf
    height = height, dpi = dpi
  )
}

#### Reservoir----
(Pb_resevoir_plot <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = Supply_reservoir)) +
    geom_point(size = 4) +                                               # Changing point size              # Adding linear model fit
    theme_ps() + 
    ylab("Pb206/Pb207\n") +                             
    xlab("\nPb208/Pb207") +
    theme(legend.position = "right")
)

#### OS Region----
(Pb_OS_plot <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = OS_grid_region)) +
    geom_point(size = 4) +                                               # Changing point size              # Adding linear model fit
    theme_ps() + 
    ylab("Pb206/Pb207\n") +                             
    xlab("\nPb208/Pb207") +
    theme(legend.position = "right")
)

(Pb_OS_plot <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = OS_grouping)) +
    geom_point(size = 4) +                                               # Changing point size              # Adding linear model fit
    theme_ps() + 
    ylab("Pb206/Pb207\n") +                             
    xlab("\nPb208/Pb207") +
    theme(legend.position = "right")
)


