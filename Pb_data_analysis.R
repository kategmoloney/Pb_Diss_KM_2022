# Modeling the source and exposure of Pb contaminated potable water in Scotland----
## Kate Moloney
## Dissertation data analysis 2021/2022

remotes::update_packages("rlang")

getwd()
library(dplyr)
remove.packages(c("tidyverse","ggplot2"))
install.packages("tidyverse", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
library(tidyverse)  
library(ggplot2)
library(tidyr) 
install.packages("ggpubr")
library(ggpubr) 

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

##### Plot postcode data ??? ---- 
Postcode_data <- read.csv("Data/Sample_postcodes.csv")
require(rgdal)
require(sp)
install.packages("plotGoogleMaps")
require(plotGoogleMaps)


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
   scale_fill_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                 "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +               
   scale_colour_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                  "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +
    theme(legend.position = "left")+
   labs(fill= "OS grid region")+
    theme_ps()
)
plot_save(Pb_OS_plot, file_name = "Plots/Mixing plot grouped by OS grid", width = 13, 
          height = 8, dpi = 150) 

(Pb_OS_NorthSouth <- ggplot(Pb_data, aes (x = Pb206_207 , y = Pb208_207, colour = OS_grouping)) +
    geom_point(size = 2, alpha = 0.5) +   # Changing point size     
    #geom_quantile(method = "lm", aes(fill = OS_grouping)) +
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
   # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
   # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "right")
)

plot_save(Pb_legcutoff_plot, file_name = "Plots/Mixing plot grouped by legaslative cutoffs", width = 13, 
          height = 8, dpi = 150) 



##### Legislative cutoff plots---- 

Pb_less_one <- subset(Pb_data, Total_Pb <1)   # Subsetting data into Total Pb <1
Pb_one_five <- subset(Pb_data, Total_Pb >=1 & Total_Pb <=5 ) # Subsetting data into Total Pb 1-5
Pb_greater_five <- subset(Pb_data, Total_Pb >5)  # Subsetting data into Total Pb >5


(Pb_less_one_plot<- ggplot(Pb_less_one, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= OS_grid_region)) +
                        geom_point(size = 2) + # Changing point size
                       # xlim(1.05,1.18) +
                       # ylim(2,2.75) +
                        theme_ps() + 
                        xlab("Pb206/Pb207\n") +                             
                        ylab("\nPb208/Pb207") +
                        scale_fill_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                 "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +               
                        scale_colour_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                   "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +
                        theme(legend.position = "bottom") +
                        theme_ps()
)

plot_save(Pb_less_one_plot, file_name = "Plots/Total Pb less than 1 grouped by OS region", 
          width = 13, height = 8, dpi = 150) 


(Pb_one_five_plot<- ggplot(Pb_one_five, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= OS_grid_region)) +
    geom_point(size = 2) + # Changing point size
    # xlim(1.05,1.18) +
    # ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    scale_fill_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                 "brown3")) +               
    scale_colour_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                   "brown3")) +
    theme(legend.position = "bottom") +
    theme_ps() 
)

plot_save(Pb_one_five_plot, file_name = "Plots/Mixing plot of total Pb 1-5 groupd by OS region", 
          width = 13, height = 8, dpi = 150) 

(Pb_greater_five_plot<- ggplot(Pb_greater_five, aes(x= Pb206_207 , y = Pb208_207,
                                        colour= OS_grid_region)) +
    geom_point(size = 2) + # Changing point size
    # xlim(1.05,1.18) +
    # ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    scale_fill_manual(values = c("pink3", "yellow2", "aquamarine2", "darkorchid")) +               
    scale_colour_manual(values = c("pink3", "yellow2", "aquamarine2", "darkorchid")) +
    theme(legend.position = "bottom") +
    theme_ps()
)

plot_save(Pb_greater_five_plot, file_name = "Plots/Mixing plot of total Pb greater than 5 groupd by OS region", 
          width = 13, height = 8, dpi = 150) 

(Total_Pb_LC_groups_plots <- ggarrange(Pb_less_one_plot ,Pb_one_five_plot, Pb_greater_five_plot,
                                      labels= c("Total Pb <1 μg L-1", "Total Pb 1-5 μg L-1",
                                                "Total Pb >5 μg L-1")))


#### Four main OS regions ---- 

Five_main_OS <- subset(Pb_data, OS_grid_region %in% c("NS", "NT", "NO", "NJ", "NX"))

(OS_grouping_five_main_plot <- ggplot(Five_main_OS,aes(x= Pb206_207 , y = Pb208_207,
                                                       colour= OS_grouping))) +
    geom_point(size = 2, alpha = 0.5) +  # Changing point size and transparency
    #xlim(1.05,1.18) +
    #ylim(2,2.6) +
    theme_ps() + 
    scale_fill_manual(values = c("lightsalmon", "cadetblue4")) +
    scale_colour_manual(values = c("lightsalmon", "cadetblue4")) +
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "bottom")

plot_save(OS_grouping_five_main_plot, file_name = "Plots/Mixing plot of five main OS regions grouped by North/South classification", 
          width = 13, height = 8, dpi = 150) 


(Reservoir_five_main_plot <- ggplot(Five_main_OS,aes(x= Pb206_207 , y = Pb208_207,
                                                       colour= Supply_reservoir))) +
  geom_point(size = 2, alpha = 0.5) +  # Changing point size and transparency
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  #scale_fill_manual(values = c("lightsalmon", "cadetblue4")) +
  #scale_colour_manual(values = c("lightsalmon", "cadetblue4")) +
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")
                           
NS_samples <- subset(Pb_data, OS_grid_region == "NS")
NT_samples <- subset(Pb_data, OS_grid_region == "NT")
NO_samples <- subset(Pb_data, OS_grid_region == "NO")
NJ_samples <- subset(Pb_data, OS_grid_region == "NJ")
NX_samples <- subset(Pb_data, OS_grid_region == "NX")

### NS
(NS_plot <- ggplot(NS_samples, aes(x= Pb206_207 , y = Pb208_207,
                                        colour= Supply_reservoir))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(NS_plot, file_name = "Plots/Mixing plot NS samples grouped by reservoir", 
          width = 13, height = 8, dpi = 150) 

(NS_total_Pb <- ggplot(NS_samples, aes(x= Pb206_207 , y = Pb208_207, 
                                       colour= Legislative_cutoffs)) +
    geom_point(size = 2) +  # Changing point size 
    #xlim(1.05,1.18) +
    #ylim(2,2.6) +
    theme_ps() + 
    # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
    # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "bottom"))

(NS_legcutoffs_facet <- NS_total_Pb + facet_grid(cols = vars(Legislative_cutoffs)))

plot_save(NS_legcutoffs_facet, file_name = "Plots/NS samples faceted by legislative cutoffs", 
          width = 13, height = 8, dpi = 150) 

### NT
(NT_plot <- ggplot(NT_samples, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Supply_reservoir))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(NT_plot, file_name = "Plots/Mixing plot NT samples grouped by reservoir", 
          width = 13, height = 8, dpi = 150) 

(NT_total_Pb <- ggplot(NT_samples, aes(x= Pb206_207 , y = Pb208_207, 
                                       colour= Legislative_cutoffs)) +
    geom_point(size = 2) +  # Changing point size 
    #xlim(1.05,1.18) +
    #ylim(2,2.6) +
    theme_ps() + 
    # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
    # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "bottom"))

(NT_legcutoffs_facet <- NT_total_Pb + facet_grid(cols = vars(Legislative_cutoffs)))

plot_save(NT_legcutoffs_facet, file_name = "Plots/NT samples faceted by legislative cutoffs", 
          width = 13, height = 8, dpi = 150) 

### NX

(NX_plot <- ggplot(NX_samples, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Supply_reservoir))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(NX_plot, file_name = "Plots/Mixing plot NX samples grouped by reservoir", 
          width = 13, height = 8, dpi = 150) 

(NX_total_Pb <- ggplot(NX_samples, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Legislative_cutoffs))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

(NX_legcutoffs_facet <- NX_total_Pb + facet_grid(cols = vars(Legislative_cutoffs)))



### NO

(NO_plot <- ggplot(NO_samples, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Supply_reservoir))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(NO_plot, file_name = "Plots/Mixing plot NO samples grouped by reservoir", 
          width = 13, height = 8, dpi = 150) 

(NO_total_Pb <- ggplot(NO_samples, aes(x= Pb206_207 , y = Pb208_207,
                                       colour= Legislative_cutoffs))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

(NO_legcutoffs_facet <- NO_total_Pb + facet_grid(cols = vars(Legislative_cutoffs)))

### NJ

(NJ_plot <- ggplot(NJ_samples, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Supply_reservoir))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(NJ_plot, file_name = "Plots/Mixing plot NJ samples grouped by reservoir", 
          width = 13, height = 8, dpi = 150) 


(Five_main_OS_regions_plot <- ggarrange(NJ_plot, NX_plot, NT_plot, NS_plot, NO_plot,
                                       labels= c("NJ samples grouped by reservoir", "NX samples grouped by reservoir",
                                                 "NT samples grouped by reservoir", "NS samples grouped by reservoir",
                                                 "NO samples grouped by reservoir")))



##### Pipe signatures---- 
Pipe_sigs_data <- read.csv("Data/Pipe_sigs.csv")

(Pipe_sigs_plot <- ggplot(Pipe_sigs_data, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Pb_classification))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(Pipe_sigs_plot, file_name = "Plots/Mixing plot pf pipe signatures with other environmental materials", 
          width = 13, height = 8, dpi = 150) 






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

