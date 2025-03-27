library(tidyr)
library(ggplot2)
library(here)
library(tidyverse)
library(purrr)

df <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES >0)

df1 <- read.csv("svi_interactive_map.csv") %>% 
  filter(SPL_THEMES > 0)


df2 <- readxl::read_excel("MissouribyCounty.xlsx", sheet = "SVI_Damaged FIPS")


df1$FIPS <- as.numeric(df1$FIPS)

df1 <- df1[df1$FIPS %in% c(
                            29097010400,
                            29097010500,
                            29097010600,
                            29097010700,
                            29097010800,
                            29097010900,
                            29097011900,
                            29145020501,
                            29145020502,
                            29145020601
                           ),
           ]


mu <- mean(df$SPL_THEMES)
sigma <- sd(df$SPL_THEMES)



dfJoplin_Updated <- data.frame(
  FIPS = c(
    29097010400,
    29097010500,
    29097010600,
    29097010700,
    29097010800,
    29097010900,
    29097011900,
    29145020501,
    29145020502,
    29145020601
  ), 
  SPL_THEMES = c(
    -0.3792,
    -0.3130,
    -1.2726,
    -0.4433,
    -1.2168,
    -0.4251,
    0.2214,
    -0.0939,
    -0.3755,
    -0.5786
  )
)

dfJoplin_Updated$SPL_THEMES <- -1 * dfJoplin_Updated$SPL_THEMES 





ggplot(data = df, aes(x = SPL_THEMES)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 50) +
  # Add normal distribution
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                color = "red", linewidth = 1) +
  # Add legend
  theme_minimal() +
  labs(title = "National SVI Distribution with Normal Distribution Fit",
       x = "SPL_THEMES",
       y = "Density")



# This will let users change the damage level of each FIPS to calculate a different weight
damageAssessment <- function(affected_area_data) {
  cat("Input Damage Assessment for Each FIPS with 10 (Total Destruction) being the most severe and 1 (No Damage) being the least:\n\n")
  
  for(i in 1:nrow(affected_area_data)) {
    fips <- affected_area_data$FIPS[i]
    current_damage <- affected_area_data$DamageLvl[i]
    
    # Display prompt for current FIPS
    cat(paste0(fips, " Current Value[", current_damage, "] New Input: "))
    
    # Get user input with validation (must be 1-10)
    valid_input <- FALSE
    while(!valid_input) {
      user_input_raw <- readline()
      
      # Check if the input is empty (just hitting enter)
      if(user_input_raw == "") {
        valid_input <- TRUE
        # Keep the previous value (no change needed)
      } else {
        user_input <- as.integer(user_input_raw)
        
        if(!is.na(user_input) && user_input >= 1 && user_input <= 10) {
          valid_input <- TRUE
          affected_area_data$DamageLvl[i] <- user_input
        } else {
          cat("Please enter a number between 1 and 10 (or press Enter to keep previous value): ")
        }
      }
    }
  }
  
  return(affected_area_data)
}





calculate_aid <- function(affected_area_data, total_aid = NULL) {
  
  #Get our national Baseline, we can do work on these stats
  national_baseline <- read.csv("svi_interactive_map.csv") %>% 
    filter(SPL_THEMES > 0) %>% select(SPL_THEMES)
  
  sample_sd <- sd(affected_area_data$SPL_THEMES)
  sample_mean <- mean(affected_area_data$SPL_THEMES)
  
  # Set floor and ceiling (1.5 standard deviations)
  floor_value <- -1.5 * sample_sd
  ceiling_value <- 1.5 * sample_sd
  
  #An allocation funciton (how we will do our equity)
  allocation_func <- function(x) {
    return(exp(0.5*x))
  }
  
  if(!"DamageLvl" %in% colnames(affected_area_data)) {
    affected_area_data <- affected_area_data %>% 
      mutate(DamageLvl = 5)
  }

  affected_area_data <- damageAssessment(affected_area_data)
  
  # Find our how far our points are from the center 
  
  
  affected_area_data <- mutate(
      affected_area_data, 
      
      AllocationScore = allocation_func(SPL_THEMES) * (affected_area_data$DamageLvl/5),
      AllocationPercentage = AllocationScore / sum(AllocationScore),
      AllocationPercentage = round(AllocationPercentage,4),
      AllocationScore = round(AllocationScore,4)
  )
  
  # Add AidPerFIPS conditionally
  if(!is.null(total_aid)) {
    affected_area_data <- affected_area_data %>%
      mutate(AidPerFIPS = AllocationPercentage * total_aid)
  } else {
    affected_area_data <- affected_area_data %>%
      mutate(AidPerFIPS = 0)
  }
  
  
  return_data <- affected_area_data %>% 
                select(FIPS, DamageLvl, AllocationScore, AllocationPercentage, AidPerFIPS) %>% 
                arrange(desc(AidPerFIPS))
  
  return(return_data)
}




  #result <- calculate_aid(df1, 1.04*10^5) #Jasper County Location data, Plus aid sent during Joplin $174 million USD (this assumes all went towards individuals and not general rebuilding)
  
  resultJoplin <- calculate_aid(dfJoplin_Updated)
  
  write.csv(resultJoplin , "JoplinAllocations")
  
  ggplot(data = result, aes(x = AllocationPercentage)) +
    geom_histogram()
  
  
  
  
  

  
  
  
  
  
  