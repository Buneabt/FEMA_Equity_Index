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

  
  write_excel_csv(resultJoplin , "JoplinAllocations.csv")
  ggplot(data = result, aes(x = AllocationPercentage)) +
    geom_histogram()
  
  
  
  
  

  
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(ggplot2)
  library(RColorBrewer)
  
  # Set your Census API key
  census_api_key("1e4554fc55b2d758ee8843c0e0659fb4e6bf666e") # Uncomment and replace with your key
  options(tigris_use_cache = TRUE)
  
  # Create data frame from Joplin results
  joplin_data <- tribble(
    ~FIPS, ~DamageLvl, ~AllocationScore, ~AllocationPercentage, ~AidPerFIPS, ~County, ~FIPS_short,
    29097010400, 5, 1.2088, 0.0924, 0, "Jasper", "01040",
    29097010500, 5, 1.1694, 0.0894, 0, "Jasper", "01050",
    29097010600, 5, 1.8895, 0.1445, 0, "Jasper", "01060",
    29097010700, 5, 1.2481, 0.0955, 0, "Jasper", "01070", 
    29097010800, 5, 1.8375, 0.1405, 0, "Jasper", "01080",
    29097010900, 5, 1.2368, 0.0946, 0, "Jasper", "01090",
    29097011900, 5, 0.8952, 0.0685, 0, "Jasper", "01190",
    29145020501, 5, 1.0481, 0.0802, 0, "Newton", "02050",
    29145020502, 5, 1.2065, 0.0923, 0, "Newton", "02050",
    29145020601, 5, 1.3355, 0.1021, 0, "Newton", "02060"
  )
  
  # Format FIPS codes correctly for joining with census data
  joplin_data <- joplin_data %>%
    mutate(GEOID = as.character(FIPS))
  
  # Get census tract geometries for Jasper and Newton counties, MO
  # Note: Using get_acs as a simple way to get geometries, but we'll only use the geometries
  mo_tracts <- get_acs(
    geography = "tract",
    variables = "B01001_001", # Total population (we just need this to get geometries)
    state = "MO",
    county = c("Jasper", "Newton"),
    year = 2021,  # Using most recent year available
    geometry = TRUE
  )
  
  # Merge the allocation data with the census tract geometries
  joplin_map_data <- mo_tracts %>%
    left_join(joplin_data, by = "GEOID")
  
  # Replace NA values with 0 for tracts not in our dataset
  joplin_map_data <- joplin_map_data %>%
    mutate(AllocationPercentage = replace_na(AllocationPercentage, 0))
  
  # Create the map
  joplin_map <- ggplot(joplin_map_data) +
    geom_sf(aes(fill = AllocationPercentage)) +
    scale_fill_viridis_c(
      name = "Allocation %",
      option = "plasma",
      labels = scales::percent_format(accuracy = 0.1),
      na.value = "grey90"
    ) +
    theme_minimal() +
    labs(
      title = "Joplin, MO: Disaster Aid Allocation Percentages by Census Tract",
      subtitle = "Based on SVI and Damage Assessment",
      caption = "Data source: Custom allocation model"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      axis.text = element_text(size = 8)
    )
  
  # Print the map
  print(joplin_map)
  
  # Save the map
  ggsave("joplin_allocation_heatmap.png", joplin_map, width = 10, height = 8, dpi = 300)
  
  # Optional: Add city boundaries for context
  # If you want to add city boundaries, you can use tigris
  library(tigris)
  
  # Get Joplin city boundaries
  joplin_city <- places(state = "MO", year = 2021) %>%
    filter(NAME == "Joplin")
  
  # Add city boundary to map
  joplin_map_with_city <- joplin_map +
    geom_sf(data = joplin_city, fill = NA, color = "black", linetype = "dashed", size = 1) +
    labs(subtitle = "Based on SVI and Damage Assessment with City Boundary")
  
  # Print the map with city boundary
  print(joplin_map_with_city)
  
  
  
  