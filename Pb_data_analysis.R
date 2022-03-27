# Modeling the source and exposure of Pb contaminated potable water in Scotland----
## Kate Moloney
## Dissertation data analysis 2021/2022

getwd()
library(dplyr)
library(tidyverse) 
library(ggplot2)
library(tidyr)  

Pb_data <- read.csv("Data/Pb_tidy.csv")
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
    geom_point(size = 2) +                                               # Changing point size              # Adding linear model fit
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
   xlim(1.05,1.2) +
   ylim(2.25, 2.57)+
  theme(legend.position = "bottom") +
   theme_ps()
)


plot_save(Pb_resevoir_plot, file_name = "Plots/Mixing plot grouped by supply reservoir", width = 13, 
          height = 8, dpi = 150) 


#### OS Region----
(Pb_OS_plot <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207,
                      colour = OS_grid_region,)) +
    geom_point(size = 2) + # Changing point size         
    xlim(1.05,1.18) +
    ylim(2.0, 2.75)+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "right") +
   labs(fill= "OS grid region")+
    theme_ps()
)
plot_save(Pb_OS_plot, file_name = "Plots/Mixing plot grouped by OS grid", width = 13, 
          height = 8, dpi = 150) 

(Pb_OS_NS <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = OS_grouping)) +
    geom_point(size = 4) +   # Changing point size     
    xlim(1.1,1.18) +
    ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "right")
)
plot_save(Pb_OS_NS, file_name = "Plots/Mixing plot grouped by OS region", width = 13, 
          height = 8, dpi = 150) 

#### Total Pb----

Pb_data <- Pb_data %>% 
           group_by(Legislative_cutoffs =
                  case_when(
                  Total_Pb <1 ~ "<1",
                  Total_Pb >=1 & Total_Pb <=5 ~ "1-5",
                  Total_Pb >5 ~ ">5", 
                  ))
                 
levels(Pb_data$Total_Pb)<- c("<1", "1-5", ">5")
   

(Pb_legcutoff_plot <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = Legislative_cutoffs)) +
    geom_point(size = 2) +  # Changing point size 
    xlim(1.05,1.18) +
    ylim(2,2.6) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "right")
)

plot_save(Pb_legcutoff_plot, file_name = "Plots/Mixing plot grouped by legaslative cutoffs", width = 13, 
          height = 8, dpi = 150) 

(Pb_less_one_plot<- ggplot(Pb_data, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= (case_when(
                                     Total_Pb <1 ~ "<1")))) +
                        geom_point(size = 2) + # Changing point size
                       # xlim(1.05,1.18) +
                       # ylim(2,2.75) +
                        theme_ps() + 
                        xlab("Pb206/Pb207\n") +                             
                        ylab("\nPb208/Pb207") +
                        theme(legend.position = "bottom")
)



(Pb_one_five_plot<- ggplot(Pb_data, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= case_when(
                                     Total_Pb >=1 & Total_Pb <=5 ~ "1-5"))) +
    geom_point(size = 2) + # Changing point size
    # xlim(1.05,1.18) +
    # ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "bottom")
)

(Pb_greater_five_plot<- ggplot(Pb_data, aes(x= Pb206_207 , y = Pb208_207,
                                        colour= case_when(Total_Pb >5 ~ ">5"))) +
    geom_point(size = 2) + # Changing point size
    # xlim(1.05,1.18) +
    # ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "bottom")
)


#### GLM----
(hist_Total_Pb<- ggplot(Pb_data, aes(x = Total_Pb)) + 
   geom_histogram(aes(y = ..count..), binwidth = 0.7,
                  colour = "honeydew4", fill = "peru") +
     scale_y_log10() +
     scale_x_log10() +
   #theme_ps() +
   labs(x = "\nTotal pb(in ug L -1)", # edit axis labels 
        y = "Frequency\n")
)


plot_save(hist_Total_Pb, file_name = "Plots/Histogram of Total Pb", width = 13, 
          height = 8, dpi = 150)

(hist_Pb206_207<- ggplot(Pb_data, aes(x = Pb206_207)) + 
    geom_histogram(aes(y = ..count..), binwidth = 0.01,
                   colour = "honeydew4", fill = "peru") +
    scale_y_log10() +
    scale_x_log10() +
    #theme_ps() +
    labs(x = "\nPb206/Pb207", 
         y = "Frequency\n") # edit axis labels 
)

plot_save(hist_Total_Pb, file_name = "Plots/Histogram of Pb206/Pb207", width = 13, 
          height = 8, dpi = 150)

(hist_Pb208_207<- ggplot(Pb_data, aes(x = Pb208_207)) + 
    geom_histogram(aes(y = ..count..), binwidth = 0.01,
                   colour = "honeydew4", fill = "peru") +
    scale_y_log10() +
    scale_x_log10() +
    #theme_ps() +
    labs(x = "\nPb208/Pb207", 
         y = "Frequency\n") # edit axis labels 
)

plot_save(hist_Total_Pb, file_name = "Plots/Histogram of Pb208/Pb207", width = 13, 
          height = 8, dpi = 150)

