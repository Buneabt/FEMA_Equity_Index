library(tidyr)
library(ggplot2)
library(here)
library(tidyverse)

df <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES >0)

df1 <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES > 0, COUNTY == "Jasper County", STATE == "Missouri")



ggplot(data = df1, aes(x = SPL_THEMES)) + 
  geom_histogram()


calculate_aid <- function(affected_area_data, total_aid) {
  
  #Get our national Baseline, we can do work on these stats
  national_baseline <- read.csv("svi_interactive_map.csv") %>% 
    filter(SPL_THEMES > 0) %>% select(SPL_THEMES) %>% 
    select(SPL_THEMES)
  
  #An allocation funciton (how we will do our equity)
  allocation_func <- function(x) {
    return(exp(0.3*x))
  }
  # Find our how far our points are from the center
  affected_area_data <- mutate(
                               affected_area_data, SPL_CENTER = (affected_area_data$SPL_THEMES - median(national_baseline$SPL_THEMES)),
                               AllocationScore = allocation_func(SPL_CENTER),
                               #Normalize it here 
                               AllocationPercentage = AllocationScore / sum(AllocationScore),
                               Aid_Per_FIPS = AllocationPercentage * total_aid
                               )
  
  return_data <- affected_area_data %>% 
                select(FIPS, SPL_CENTER, AllocationScore, AllocationPercentage, Aid_Per_FIPS)
  
  return(return_data)
}

  result <- calculate_aid(df1, 174*10^6) #Jasper County Location data, Plus aid sent during Jopin $174 million USD
  
  ggplot(data = result, aes(x = AllocationPercentage)) +
    geom_histogram()
  
  
  
  
  
  
  
  
  
  