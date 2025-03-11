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
# Determine how quickly each risk escalates by analysing the time between critical changes, looking at start date

#Looking at risk criticality over time
velocity = data_cleaned %>%
    select(report_date, risk_unique_id, criticality) %>%
    distinct()

# Assign numerical values to criticality levels
data_cleaned <- data_cleaned %>%
    mutate(
        criticality_level = case_when(
            criticality == "H1" ~ 1,
            criticality == "H2" ~ 2,
            criticality == "H3" ~ 3
        )
    )

criticality_range <- data_cleaned %>%
    arrange(risk_unique_id, report_date) %>%
    select(risk_unique_id, report_date, criticality_level) %>%
    distinct() %>% 
    group_by(risk_unique_id) %>%
    mutate(
        prev_criticality = lag(criticality_level),
        prev_date = min(report_date),
        change = prev_criticality- criticality_level ,
        time_diff = as.numeric(difftime(report_date, prev_date, units = "days")),
        rate_of_change = change/time_diff
    ) %>% 
    filter(change != "0")
    


    

# Print results
print(criticality_range)


#2. Resolutions Rate: Evaluating the success rate of mitigating risks over time 
# Removing acceptance

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
    ) 

total_risks <- sum(emergence_rate$new_risks)
total_risks

# Calculate emergence rate
emergence_rate <- emergence_rate %>%
    mutate(
        emergence_rate = new_risks / total_risks
    )

#4. Likelihood of Risk and Impact Drift: Tracking shifts in project risk exposure

likelihood <- data_cleaned %>%
    group_by(risk_unique_id) %>% 
    select(report_date, risk_unique_id, pre_factored_cost, post_factored_cost)



# Calculation: Comparing changes in PreMit_Probability and PreMit_Cost over time.


# Migation to reduce cost and how good to reuce probablityt 










