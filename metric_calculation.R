# Author: Catherine Macfarlane 
# Date: 2025-03-11

# Loading the required packages 

library("readxl")
library("janitor")
library("tidyverse")


# Loading in the data -----------------------------------------------------
# Loading in the raw data at the moment but that will be changed to use cleaned data 

risk_data = read.csv("data/Risks.csv")
mitigation_data = read.csv("data/Mitigations.csv")
source("clean_data.R")

# Calculating the required metrics -----------------------------------------------------
# Starting with the calculations recommended in the challenge 

# Loading in the data -----------------------------------------------------
#1. Risk Velocity: Measuring the speed at which risks escalate 

# Speed of the potential change for each risk between H1, H2 and H3 
# Determine how quickly each risk escalates by analysing the time between critical changes.

data_cleaned = data_cleaned %>% 
    mutate(risk_duration = as.numeric(difftime(relief, start, units = "days")))

velocity = data_cleaned %>% 
    select(risk_unique_id, criticality, start, relief) %>% 
    distinct()




#2. Resolutions Rate: Evaluating the success rate of mitigating risks over time 

# Calculation: Dividing the number of closed risks by the total number of risks.
# This doesn't add to 1 as some risks have more than one mitigation 


resolution_rate <- data_cleaned %>%
    # Group by risk_unique_id to summarize for each distinct risk
    group_by(risk_unique_id) %>%
    summarise(
        is_closed = any(status == "Closed" & !is.na(unique_mitigation_id)),
        is_open_with_mit = any(status == "Open" & !is.na(unique_mitigation_id)),
        is_open_no_mit = any(status == "Open" & is.na(unique_mitigation_id))
    ) %>%
    summarise(
        closed_mit_risks = sum(is_closed),
        open_mit_risks = sum(is_open_with_mit),
        open_no_mit_risks = sum(is_open_no_mit),
        total_risks = n_distinct(risk_unique_id)
    ) %>%
    mutate(
        closed_mit_risks_rate = closed_mit_risks / total_risks,
        open_mit_risks_rate = open_mit_risks / total_risks,
        open_no_mit_risks_rate = open_no_mit_risks / total_risks
    )


#3. Emergence Rate: Identifying periods of triggers associated with new risk idenitifcation 

# Calculation: Analysing the frequency of new risks over time.
# Useful plot looking at the trend over various months 

emergence_rate <- data_cleaned %>%
    group_by(start) %>%
    summarise(
        new_risks = n_distinct(risk_unique_id)
    ) %>% 
    filter(!start =="#VALUE!")

total_risks <- sum(emergence_rate$new_risks)
total_risks

# Calculate emergence rate
emergence_rate <- emergence_rate %>%
    mutate(
        emergence_rate = new_risks / total_risks
    )

#4. Likelihood of Risk and Impact Drift: Tracking shifts in project risk exposure




# Calculation: Comparing changes in PreMit_Probability and PreMit_Cost over time.

#5. Risk Clustering: Highlight interdependent risks prone to cascading failures 

# Calculation: This requires analysing relationships between risks
#. Amalia doing 

#6. Additional Metrics:








