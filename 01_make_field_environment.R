#### CBCA Jamaica - Make field environments (weather +  soil)
# https://github.com/jrodriguez88/
# Author: Rodriguez-Espinoza J.
# 2021

## Load packages

library(tidyverse)
library(lubridate)
library(readxl)
library(naniar)
library(skimr)


source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_nasapower.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_soil_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/utils/utils_crop_model.R", encoding = "UTF-8")


## download data
# 3 parishes
sites <- read_csv("data/pts.csv") %>% 
  set_names(c('lon', "lat", "Site", "desc")) %>% select(Site, lat, lon)

# base line data
ini_date <- ymd("1990-01-01")
end_date <- ymd("2019-12-31")

weather_data_raw <- sites %>% 
  mutate(data =  map2(lat, lon,
                      ~get_data_nasapower(ini_date = ymd("1990-01-01"), end_date =ymd("2019-12-31"), lat = .x, lon = .y)))

wdata <- weather_data_raw %>% 
  mutate(data = map2(data, lat, ~.x %>% 
                      from_nasa_to_model %>% basic_qc_nasa %>%
                      mutate(
    extraT = extrat(lubridate::yday(date), radians(.y))$ExtraTerrestrialSolarRadiationDaily,
    srad = if_else(is.na(srad), 0.19*sqrt(tmax - tmin)*extraT, srad)) %>% select(-extraT) %>% rename(wvel = wspd)))
  
  
## read data

## tidy data and quality control
#https://metservice.gov.jm/30-yr-mean-rainfall-in-mm/
#https://metservice.gov.jm/2017/06/11/20-year-mean-temperatures-1996-2015/

skimr::skim(wdata$data[[1]])


wdata %>% unnest(data) %>%  
  group_by(Site, year = year(date), month = month(date)) %>%
  summarise(rain = sum(rain), 
            tmin = mean(tmin), 
            tmax = mean(tmax), 
            srad = mean(srad), 
            rhum = mean(rhum),
            wspd = mean(wvel)) %>% 
  ungroup() %>% gather(var, value, -c(Site, year, month)) %>%
  ggplot(aes(factor(month), value)) + 
  geom_boxplot(aes(fill = Site)) + 
  facet_wrap(~var, scales = "free") + 
  labs(x = "month", title = paste("Climate -  Jamaica"), subtitle = "30-year Data 1990-2019", caption = "Source: NASAPOWER - https://power.larc.nasa.gov/") +
  theme_bw()


## write files

wdata %>% mutate(alt = c(150, 45, 12), id = str_replace(Site, "_", ""),
                 co2_f = "MaunaLoa.CO2") %>%
  mutate(to_aquacrop = pmap(list(x = id,
                                 y = data,
                                 z = lat, 
                                 k = alt, 
                                 m = co2_f),
                            function(x,y,z,k,m) make_weather_aquacrop(path = "aquacrop_files/wth/", 
                                                                      id_name = x, 
                                                                      wth_data = y, 
                                                                      lat = z,
                                                                      alt = k,
                                                                      co2_file = m))) %>% pull(to_aquacrop)

