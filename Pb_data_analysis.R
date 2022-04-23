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
library(ggmap)
install.packages("rworldmap")
library(rworldmap)
install.packages("ggforce")
library(ggforce)
library(MASS)
library(shiny)

Pb_data <- read.csv("Data/Pb_tidy.csv")
Pb_total <- read.csv("Data/Pb_total_LOD.csv")
Pb_ratios <- read.csv("Data/Pb_ratio_data.csv")
#Pb_data <- Pb_data %>% filter(Total_Pb >= 0.13)   # removing total Pb data below LOD
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

##### Plot postcode data ---- 
Postcode_data <- read.csv("Data/Postcodes_LAT_LONG.csv")
                       # header = TRUE, sep = "\t")
Postcode_coordinates <- subset(Postcode_data [c("Postcode", "OS_grid_region", 
                                               "Latitude", "Longitude")])

(prelim_postocde_map <- ggplot(Postcode_coordinates, aes(x= Longitude, y= Latitude,
                                                         colour= OS_grid_region))+
                               geom_point())

world <- getMap(resolution = "low")

(Postcode_map_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = Postcode_coordinates,               # Add coordinate data
               aes(x = Longitude, y = Latitude, 
                   colour = OS_grid_region)) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Ordanance survey region")))

world@data$ADMIN

# Call the vector in `borders()`
world_Scotland <- world[world@data$ADMIN == "United Kingdom", ]

(Scotland_postcodes <- ggplot() +
    geom_polygon(data = world_Scotland, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = Postcode_coordinates,  # Add and plot speices data
               aes(x = Longitude, y = Latitude, 
                   colour = OS_grid_region)) +
    coord_quickmap() + 
    facet_zoom(xlim = c(-8, 1), ylim = c(54.5, 60)) +
   #xlim(-8, 1) +  # Set x axis limits
    #ylim(54, 60) +  # Set y axis limits
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Ordnance Survey region")))

plot_save(Scotland_postcodes, file_name = "Plots/Sample postcode distribution", width = 13, 
          height = 8, dpi = 150) 


#### Reservoir----
(Pb_resevoir_plot <- ggplot(Pb_ratios, aes (x = Pb206_207 , y = Pb208_207, colour = Supply_reservoir)) +
    geom_point(size = 2, alpha = 0.75) +                                               # Changing point size              # Adding linear model fit
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
(Pb_OS_plot <- ggplot(Pb_ratios, aes (x = Pb206_207 , y = Pb208_207,
                      colour = OS_grid_region,)) +
    geom_point(size = 2, alpha = 0.6) + # Changing point size         
    xlim(1.05,1.18) +
    ylim(2.0, 2.75)+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
   scale_fill_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                 "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +               
   scale_colour_manual(values = c("pink3", "yellow2", "royalblue3", "aquamarine2", "darkorchid",
                                  "brown3", "cadetblue3", "coral2" ,"orange1" ,"darkolivegreen2")) +
    theme(legend.position = "bottom")+
   labs(fill= "OS grid region")+
    theme_ps()
)

plot_save(Pb_OS_plot, file_name = "Plots/Mixing plot grouped by OS grid", width = 13, 
          height = 8, dpi = 150) 

(OS_region_facet <- Pb_OS_plot + facet_grid(cols = vars(Legislative_cutoffs)))

plot_save(OS_region_facet, file_name = "Plots/Facet plot grouped by OS grid and legislative cutoffs", width = 13, 
          height = 8, dpi = 150) 

(Pb_OS_NorthSouth <- ggplot(Pb_total, aes (x = Pb206_207 , y = Pb208_207, colour = OS_grouping)) +
    geom_point(size = 2, alpha = 0.5) +   # Changing point size     
    #geom_quantile(method = "lm", aes(fill = OS_grouping)) +
    xlim(1.1,1.18) +
    ylim(2,2.75) +
    theme_ps() + 
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "bottom")
)
plot_save(Pb_OS_NorthSouth, file_name = "Plots/Mixing plot grouped by OS region", width = 13, 
          height = 8, dpi = 150) 

(OS_grouping_facet <- Pb_OS_NorthSouth + facet_grid(cols = vars((Legislative_cutoffs))))

plot_save(OS_grouping_facet, file_name = "Plots/North South divide of total data facted by leg cutoffs", width = 13, 
          height = 8, dpi = 150) 

(OS_regions_sep_and_group <- ggarrange(Pb_OS_plot, Pb_OS_NorthSouth,
                                       labels = c("10 OS regions", 
                                        "OS regions grouped by North/South divide")))

plot_save(OS_regions_sep_and_group, file_name = "Plots/OS regions alongisde North South grouping", width = 13, 
          height = 8, dpi = 150) 

#### Pb206/Pb207 ----
(Pb206_207_plot <-  ggplot(Pb_ratios, aes(x= OS_grid_region , y = Pb206_207,
                                        colour= OS_grouping)) +
   geom_point(size = 2, alpha = 0.6) + # Changing point size
   # xlim(1.05,1.18) +
   # ylim(2,2.75) +
   theme_ps() + 
   xlab("OS Region\n") +                             
   ylab("\nPb206/Pb207") +
   theme(legend.position = "bottom") +
   theme_ps()
)

plot_save(Pb206_207_plot, file_name = "Plots/Pb206_Pb207 per OS region", width = 13, 
          height = 8, dpi = 150) 

(Pb208_207_plot <-  ggplot(Pb_ratios, aes(x= OS_grid_region , y = Pb208_207,
                                        colour= OS_grouping)) +
    geom_point(size = 2, alpha = 0.6) + # Changing point size
    # xlim(1.05,1.18) +
    # ylim(2,2.75) +
    theme_ps() + 
    xlab("OS Region\n") +                             
    ylab("\nPb208/Pb207") +
    theme(legend.position = "bottom") +
    theme_ps()
)

plot_save(Pb208_207_plot, file_name = "Plots/Pb208_Pb207 per OS region", width = 13, 
          height = 8, dpi = 150)

(Isotope_ratios_plot <- ggarrange(Pb206_207_plot, Pb208_207_plot,
                                  labels = "Pb206/Pb207", "Pb208/Pb207"))

#### Total Pb----
(Total_Pb_plot <- ggplot(Pb_total, aes(x= OS_grid_region , y = Total_Pb,
                                      colour= OS_grouping)) +
   geom_point(size = 2, alpha = 0.6) + # Changing point size
   # xlim(1.05,1.18) +
   # ylim(2,2.75) +
   theme_ps() + 
   xlab("OS Region\n") +                             
   ylab("\nTotal Pb") +
   theme(legend.position = "bottom") +
   theme_ps()
)

Pb_data <- Pb_data %>% 
           group_by(Legislative_cutoffs =
                  case_when(
                  Total_Pb <1 ~ "<1",
                  Total_Pb >=1 & Total_Pb <=5 ~ "1-5",
                  Total_Pb >5 ~ ">5", 
                  ))

Pb_total <- Pb_total %>% 
  group_by(Legislative_cutoffs =
             case_when(
               Total_Pb <1 ~ "<1",
               Total_Pb >=1 & Total_Pb <=5 ~ "1-5",
               Total_Pb >5 ~ ">5", 
             ))


levels(Pb_data$Total_Pb)<- c("<1", "1-5", ">5")
   

(Pb_legcutoff_plot <- ggplot(Pb_total, aes (x = Pb206_207 , y = Pb208_207, colour = Legislative_cutoffs)) +
    geom_point(size = 2, alpha = 0.6) +  # Changing point size 
    xlim(1.05,1.18) +
    ylim(2,2.6) +
    theme_ps() + 
   # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
   # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
    xlab("Pb206/Pb207\n") +                             
    ylab("\nPb208/Pb207")+
    theme(legend.position = "bottom")
)

plot_save(Pb_legcutoff_plot, file_name = "Plots/Mixing plot grouped by legaslative cutoffs", width = 13, 
          height = 8, dpi = 150) 

(Leg_cutoffs_facet <- Pb_legcutoff_plot + facet_grid(cols = vars(Legislative_cutoffs)))

plot_save(Leg_cutoffs_facet, file_name = "Plots/Facet of legaslative cutoffs", width = 13, 
          height = 8, dpi = 150) 



##### Legislative cutoff plots---- 

Pb_less_one <- subset(Pb_total, Total_Pb <1)   # Subsetting data into Total Pb <1
Pb_one_five <- subset(Pb_total, Total_Pb >=1 & Total_Pb <=5 ) # Subsetting data into Total Pb 1-5
Pb_greater_five <- subset(Pb_total, Total_Pb >5)  # Subsetting data into Total Pb >5


(Pb_less_one_plot<- ggplot(Pb_less_one, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= OS_grid_region)) +
                        geom_point(size = 2, alpha = 0.6) + # Changing point size
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

(Pb_less_one_facet <- Pb_less_one_plot + facet_grid(cols = vars(OS_grid_region)))


(Pb_one_five_plot<- ggplot(Pb_one_five, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= OS_grid_region)) +
    geom_point(size = 2, alpha = 0.6) + # Changing point size
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
    geom_point(size = 2, alpha = 0.6) + # Changing point size
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

(Pb_greater_five_facet <- Pb_greater_five_plot + facet_grid(cols = vars(OS_grid_region)))

(Total_Pb_LC_groups_plots <- ggarrange(Pb_less_one_plot ,Pb_one_five_plot, Pb_greater_five_plot,
                                      labels= c("Total Pb <1 μg L-1", "Total Pb 1-5 μg L-1",
                                                "Total Pb <1 μg L-1")))

(Total_Pb_lessone_greaterfive <- ggarrange (Pb_less_one_facet, Pb_greater_five_facet,
                                            labels= "Total Pb <1 μg L-1", 
                                            "Total Pb <1 μg L-1"))


#### Five main OS regions ---- 

Five_main_OS <- subset(Pb_total, OS_grid_region %in% c("NS", "NT", "NO", "NJ", "NX"))

(OS_region_five_main_plot <- ggplot(Five_main_OS,aes(x= Pb206_207 , y = Pb208_207,
                                                       colour= OS_grid_region))) +
  geom_point(size = 2, alpha = 0.5) +  # Changing point size and transparency
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  #scale_fill_manual(values = c("lightsalmon", "cadetblue4")) +
 # scale_colour_manual(values = c("lightsalmon", "cadetblue4")) +
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  labs(caption = "Five main OS regions", )+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5))

plot_save(OS_region_five_main_plot, file_name = "Plots/Mixing plot of five main OS regions grouped by OS region", 
          width = 13, height = 8, dpi = 150) 

(Five_mainOS_facet <- OS_region_five_main_plot + 
    facet_grid(cols = vars(Legislative_cutoffs)))

(OS_five_main_legcutoff_plot <- ggplot(Five_main_OS,aes(x= Pb206_207 , y = Pb208_207,
                                                    colour= Legislative_cutoffs))) +
  geom_point(size = 2, alpha = 0.5) +  # Changing point size and transparency
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  #scale_fill_manual(values = c("lightsalmon", "cadetblue4")) +
  # scale_colour_manual(values = c("lightsalmon", "cadetblue4")) +
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  labs(caption = "Five main OS regions", )+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5))

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
    labs(caption = "North/South grouping of five main OS regions", )+
    theme(legend.position = "bottom", 
          plot.caption = element_text(hjust = 0.5))

plot_save(OS_grouping_five_main_plot, file_name = "Plots/Mixing plot of five main OS regions grouped by North/South classification", 
          width = 13, height = 8, dpi = 150) 

(Five_mainOS_facet <- OS_grouping_five_main_plot + facet_grid
                              (cols = vars(Legislative_cutoffs)))


(Five_mainOS_plots <- ggarrange(OS_region_five_main_plot, OS_grouping_five_main_plot,
                                labels = "Five main OS regions", 
                                "North/South grouping of five main OS regions"))

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
                           
NS_samples <- subset(Pb_total, OS_grid_region == "NS")
NT_samples <- subset(Pb_total, OS_grid_region == "NT")
NO_samples <- subset(Pb_total, OS_grid_region == "NO")
NJ_samples <- subset(Pb_total, OS_grid_region == "NJ")
NX_samples <- subset(Pb_total, OS_grid_region == "NX")

### Central Belt (NT and NS)
Central_data <- subset(Pb_total, OS_grid_region %in% c("NS", "NT"))

(Central_plot <- ggplot(Central_data, aes(x= Pb206_207 , y = Pb208_207,
                                   colour= Legislative_cutoffs))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  labs(caption = "Central Belt samples")+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5))

(Central_facet <- Central_plot + facet_grid(cols = vars(Legislative_cutoffs)))

plot_save(Central_plot, file_name = "Plots/Mixing plot Central Belt grouped by OS", 
          width = 13, height = 8, dpi = 150) 


### North East region
NorthEast_data <- subset(Pb_total, OS_grid_region %in% c("NO", "NJ"))

(NorthEast_plot <- ggplot(NorthEast_data, aes(x= Pb206_207 , y = Pb208_207,
                                          colour= OS_grid_region))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  labs(caption = "North east samples")+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5))

plot_save(Central_plot, file_name = "Plots/Mixing plot North East grouped by OS", 
          width = 13, height = 8, dpi = 150) 

### Central vs North East 
Central_NE_data <- subset(Pb_data, OS_grid_region %in% c("NO", "NJ", "NS", "NT"))

(Central_NE_plot <- ggplot(Central_NE_data, aes(x= Pb206_207 , y = Pb208_207,
                                              colour= OS_grouping))) +
  geom_point(size = 2) +  # Changing point size 
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  labs(caption = "North east and Central Belt samples")+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5))

(Central_NE_facet <- Central_NE_plot + facet_grid(cols = vars(Legislative_cutoffs)))


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

(NS_reservoir_facet <- NS_plot + facet_grid(cols = vars(Legislative_cutoffs)))


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

(NT_legcutoffs_facet <- NT_plot + facet_grid(cols = vars(Legislative_cutoffs)))

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
                                   colour= Pb_material, shape=Pb_material))) +
  geom_point(size = 2, alpha = 0.5) +  # Changing point size 
  geom_errorbar(aes(x = Pb206_207, ymin = Pb208_207-St_dev_y, ymax = Pb208_207+St_dev_y), width = 0) + 
  geom_errorbarh(aes(y = Pb208_207, xmin = Pb206_207-St_dev_x, xmax = Pb206_207+St_dev_x), height = 0)+
  scale_shape_manual(values = c("EH pipe"=15, "Geological"=18, "Scottish ore"= 11, 
                               "UK petrol"=16, "G Pipe"=19, "Paint"=20, "UK coal"=21,
                                "Water sample"=22, "IV Pipe"=17)) +
   #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(Pipe_sigs_plot, file_name = "Plots/Mixing plot of pipe signatures with other environmental materials", 
          width = 13, height = 8, dpi = 150) 

Central_and_pipes <- read.csv("Data/Central_and_Pipe_ratios_final.csv")

(Central_and_pipes_plot <- ggplot(Central_and_pipes, aes(x= Pb206_207 , y = Pb208_207,
                                              colour= Pb_material, shape = Pb_material))) +
  geom_point(size = 2.5, alpha = 0.5) +  # Changing point size 
  geom_errorbar(aes(x = Pb206_207, ymin = Pb208_207-St_dev_y, ymax = Pb208_207+St_dev_y), width = 0) + 
  geom_errorbarh(aes(y = Pb208_207, xmin = Pb206_207-St_dev_x, xmax = Pb206_207+St_dev_x), height = 0)+
  scale_shape_manual(values = c("EH pipe"=15, "Geological"=18, "Scottish ore"= 11, 
                                "UK petrol"=16, "G pipe"=19, "Paint"=20, "UK coal"=21,
                                "Water sample"=22))+
  #xlim(1.15,1.18) +
  ylim(2.25,2.5) +
  theme_ps() + 
  # scale_fill_manual(values = c("pink3", "yellow2", "royalblue3")) +
  # scale_colour_manual(c("pink3", "yellow2", "royalblue3"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")

plot_save(Central_and_pipes_plot, file_name = "Plots/Mixing plot of pipe sigs and Central Bellt samples ", 
          width = 13, height = 8, dpi = 150) 


NE_and_pipe <- read.csv("Data/NE_Pipe_and_Samples.csv")

(NE_and_pipes_plot <- ggplot(NE_and_pipe, aes(x= Pb206_207 , y = Pb208_207,
                                    colour= Pb_Material, shape= Pb_Material))) +
  geom_point(size = 2.5, alpha = 0.5) +  # Changing point size 
  geom_errorbar(aes(x = Pb206_207, ymin = Pb208_207-St_dev_y, ymax = Pb208_207+St_dev_y), width = 0) + 
  geom_errorbarh(aes(y = Pb208_207, xmin = Pb206_207-St_dev_x, xmax = Pb206_207+St_dev_x), height = 0)+
  scale_shape_manual(values = c("IV pipe"=17, "Geological"=18, "Scottish ore"=11, 
                                "UK petrol"=16, "Paint"=20, "UK coal"=21,
                                "Water sample"=22)) +
  #xlim(1.05,1.18) +
  #ylim(2,2.6) +
  theme_ps() + 
  #scale_fill_manual(values = c("#698B22", "#436EEE", "#B03060",
                              # "#CD0000", "#EE7600", "#8B7500")) +
  #scale_colour_manual(c("#698B22", "#436EEE", "#B03060",
                          #"#CD0000", "#EE7600", "#8B7500"))+
  xlab("Pb206/Pb207\n") +                             
  ylab("\nPb208/Pb207")+
  theme(legend.position = "bottom")


plot_save(NE_and_pipes_plot, file_name = "Plots/Mixing plot of pipe sigs and North East samples ", 
          width = 13, height = 8, dpi = 150) 


##### Chi squared test ----

Pb_total <- Pb_total %>%                   # grouping Total Pb data into < and > than MCL of 5μg L-1
  group_by(MCL_threshold =
             case_when(
               Total_Pb <=5 ~ "less than five",
               Total_Pb >5 ~ "greater than five"
             ))


Pb_data_stats <- subset(Pb_total[c("Postcode", "OS_grouping", "MCL_threshold")])

chisq.test(Pb_data_stats$OS_grouping, Pb_data_stats$MCL_threshold,
           simulate.p.value = TRUE)

(Chi_boxplot <- ggplot(Pb_data,aes(x = OS_grouping, y = MCL_threshold))+
    geom_boxplot(aes(fill = OS_grouping))+
    theme_ps() +
    #scale_fill_manual(values = c("navy", "forestgreen", "violetred")) +               # Adding custom colours
    #scale_colour_manual(values = c("navy", "forestgreen", "violetred")) +             # Adding custom colours
    ylab("MCL threshold\n") +                             
    xlab("\nOS grouping") +
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) +
    theme(legend.position = "none")
)

#### GLM----
(hist_Total_Pb<- ggplot(Pb_total, aes(x = Total_Pb)) + 
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

