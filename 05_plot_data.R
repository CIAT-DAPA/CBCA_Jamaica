## . Lectura de resultados
#path_op <- paste0(plugin_path, "/OUTP/")
library(tidyverse)
library(data.table)
library(Hmisc)
library(lubridate)
source("D:/03_DEVELOPER/aquacrop-R/read_outputs_aquacrop.R")


obs_data <- read_csv("data/FAOSTAT_data_3-3-2021.csv") 

FAOSTAT_yield <- obs_data %>% select(Element, Item, Year, Unit, Value) %>% 
  filter(Element == "Yield", Year>1989) %>%
  mutate(Yield  = Value/10000,
         cultivar = factor(Item, labels = c("Groundnut", "SweetPotato", "Tomato"))) 

season_files <- list.dirs(full.names = T) %>% str_subset("plugin") %>% str_subset("OUTP") %>% list.files(pattern = "season", full.names = T)

saf_read_season <- possibly(read_aquacrop_season, NULL)


#estructura del archivo PRM
file_str <- c("id_name", "cultivar", "soil", "id2", "crop_sys")

## REad _p seanson and daily
season_data <- map(season_files,  ~saf_read_season(.x, ""))


sim_data <- season_data %>% bind_rows() %>% arrange(Year1) %>% 
  mutate(File = str_replace(File, ".PRM", "")) %>%
  separate(File, file_str, sep = "_") %>% drop_na() %>%
  mutate(crop_sys =  if_else(str_detect(crop_sys, "IRR"), "Irrigated", "Rainfed"),
         cultivar = str_remove(cultivar, "GDDCRO")) %>%
  mutate(date = make_date(Year1, Month1, Day1), yday = yday(date),
         sow_season = if_else(id2=="S1", "Season_A", "Season_B"))


sim_data %>% 
  ggplot(aes(sow_season,  Yield, fill= soil)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_grid(cultivar ~ id_name+crop_sys , scales = "free") +
  labs(title = "Jamaica Crop Simulation - AquaCrop",
       x= "Sowing Season",
       y= "Yield (T/ha)") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))

sim_data %>% 
  ggplot(aes(sow_season,  Cycle, fill= soil)) + 
  geom_boxplot(outlier.shape=NA) +
  facet_grid(cultivar ~ id_name+crop_sys , scales = "free") +
  labs(title = "Jamaica Crop Simulation - AquaCrop",
       x= "Sowing Season",
       y= "Crop Cycle - Days after emergence/transplant") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))


sim_data %>% 
  ggplot(aes(Year1,  Yield, color=soil)) + 
  geom_smooth(alpha=0.3, se = F) +
  labs(title = "Jamaica Crop Simulation - AquaCrop",
       subtitle = "SeasonA = May-Apr  --  SeasonB = Sep-Oct",
       x= "Year",
       y= "Yield (T/ha) - (dry matter)",
       color = "Soil: ") +
  facet_grid(cultivar+crop_sys~id_name, scales = "free") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))

sim_data %>% filter(crop_sys == "Rainfed") %>%
  mutate(Yield = case_when(cultivar == "SweetPotato" ~ Yield/0.25,
                           cultivar == "Tomato" ~ Yield/0.15,
                           TRUE ~ Yield)) %>%
  ggplot(aes(Year1,  Yield)) + 
  geom_boxplot(aes(fill = id_name, group = interaction(id_name, Year1))) + 
  geom_line(data = FAOSTAT_yield, aes(Year, Yield, color = "FAOSTAT"), size =1) +
  labs(title = "Jamaica: Crop Yield Simulation with AquaCrop vs FAOSTAT data",
       subtitle = "Crop Season:  A = May-Apr  --  B = Sep-Oct",
       x= "Year",
       y= "Yield (T/ha)",
       fill = "Yield: ",
       color = "FAOSTAT") +
  facet_grid(cultivar~ ., scales = "free") +
  scale_color_manual(name = "", values = "red")+
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold")) 


FAOSTAT_yield %>%
  ggplot() +
  geom_line(aes(Year, Yield), color= "darkgreen", size = 1) +
    facet_grid(cultivar ~., scales = "free") +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    #    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold")) 


# Plor hidric demand to rise potential production
plot_agroclim_hidric <- function(season_data, id_name, file_str = NA, x_breaks = "1 year"){
  
  if(is.na(file_str)){
    data <- season_data
  } else {
    data <- season_data %>% 
      mutate(date = make_date(Year1, Month1, Day1),
             File = str_replace(File, ".PRM", "")) %>%
      separate(File, file_str, sep = "_") %>%
      mutate(crop_sys = ifelse(str_detect(crop_sys, pattern = "IRR"), "irrigated", crop_sys))
  }
  
  
  data %>%
    ggplot(aes(x = date, 
               y = Irri, color = soil, group = interaction(soil, date))) +
    stat_summary(fun.data = mean_cl_boot, position=position_dodge(.9)) +
    #  facet_wrap(cultivar ~.) +
    facet_grid(cultivar ~., scales = "free") +
    #  scale_x_date(date_labels = "%b %d")+
    theme_bw() +
    theme(
      legend.position="bottom",
      #    legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    #  guides(fill=legend.title())
    labs(x= "Fecha",
         y= paste0("Requerimientos hidricos suplementario(mm)"),
         title = paste0("Simulacion Agroclimatica - ", id_name),
         subtitle = "Modelo de cultivo: AquaCrop (V6) - http://www.fao.org/aquacrop/",
         color = "Suelo: ", 
         caption = "Fuente: AquaCrop-R(https://github.com/jrodriguez88/aquacrop-R)") +
    scale_color_viridis_d(option = "E") +
    scale_x_date(date_breaks = x_breaks , date_labels =  "%Y", limits = c(min(data$date), max(data$date)))
  
  
  
}


plot_agroclim_hidric(sim_data, "Jamaica", file_str = NA,  x_breaks = "5 years" )







