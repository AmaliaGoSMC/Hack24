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

# Calculating the required metrics -----------------------------------------------------
# Starting with the calculations recommended in the challenge 

# Loading in the data -----------------------------------------------------
#1. Risk Velocity: Measuring the speed at which risks escalate 

# Speed of the potenntial change
# Determining difference between start and end date 

# In the first format 
#  To provide a time-base, the data generates a risk update on a monthly basis for 30 months.

risk_data <- risk_data %>% 
    mutate(Start = as.Date(Start, format="%Y-%m"))




#2. Resolutions Rate: Evaluating the success rate of mitigating risks over time 

# Calculation: Dividing the number of closed risks by the total number of risks.
# Need to add whether risk tried to be mitigated

resolution_rate = risk_data %>% 
    summarise(
        closed_risks = sum(Status == "Closed"),
        total_risks = n()
    ) %>% 
    mutate(
        resolution_rate = closed_risks / total_risks
    )


#3. Emergence Rate: Identifying periods of triggers associated with new risk idenitifcation 

# Calculation: Analysing the frequency of new risks over time.
# Useful plot looking at the trend over various months 

emergence_rate <- risk_data %>%
    group_by(Start) %>%
    summarise(
        new_risks = n()
    )

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

#6. Additional Metrics:








