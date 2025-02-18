library(tidyr)
library(ggplot2)
library(here)
library(tidyverse)

df <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES >0)

df1 <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES > 0, COUNTY == "Jasper County", STATE == "Missouri")


mu <- mean(df$SPL_THEMES)
sigma <- sd(df$SPL_THEMES)


ggplot(data = df, aes(x = SPL_THEMES)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 50) +
  # Add normal distribution
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                color = "red", linewidth = 1) +
  # Add legend
  theme_minimal() +
  labs(title = "National SVI Distribution with Theoretical Fits",
       x = "SPL_THEMES",
       y = "Density")


calculate_aid <- function(affected_area_data, total_aid) {
  
  #Get our national Baseline, we can do work on these stats
  national_baseline <- read.csv("svi_interactive_map.csv") %>% 
    filter(SPL_THEMES > 0) %>% select(SPL_THEMES) %>% 
    select(SPL_THEMES)
  
  national_sd <- sd(national_baseline$SPL_THEMES)
  national_mean <- mean(national_baseline$SPL_THEMES)
  
  # Set floor and ceiling (1.5 standard deviations)
  floor_value <- -1.5 * national_sd
  ceiling_value <- 1.5 * national_sd
  
  #An allocation funciton (how we will do our equity)
  allocation_func <- function(x) {
    return(exp(0.5*x))
  }
  # Find our how far our points are from the center
  affected_area_data <- mutate(
      affected_area_data, 
      SPLCENTER = (SPL_THEMES - national_mean),

      SPLCENTER = pmin(pmax(SPLCENTER, floor_value), ceiling_value),
      AllocationScore = allocation_func(SPLCENTER),
      AllocationPercentage = AllocationScore / sum(AllocationScore),
      AidPerFIPS = AllocationPercentage * total_aid,
      AllocationPercentage = round(AllocationPercentage,4),
      AllocationScore = round(AllocationScore,4),
      SPLCENTER = round(SPLCENTER,4)
  )
  
  return_data <- affected_area_data %>% 
                select(FIPS, SPLCENTER, AllocationScore, AllocationPercentage, AidPerFIPS) %>% 
                arrange(desc(AidPerFIPS))
  
  return(return_data)
}

  result <- calculate_aid(df1, 174*10^6) #Jasper County Location data, Plus aid sent during Joplin $174 million USD (this assumes all went towards individuals and not general rebuilding)
  
  ggplot(data = result, aes(x = AllocationPercentage)) +
    geom_histogram()
  
  
  
  
  
  
  
  
  
  