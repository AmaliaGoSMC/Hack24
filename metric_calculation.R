# Author: Catherine Macfarlane 
# Date: 2025-03-11
# Description: This script calculates the metrics required for the challenge.

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

# Pre mititgation 
#1 Looking at risk criticality over time
velocity = data_cleaned %>%
    select(report_date, risk_unique_id, postmit_criticality) %>%
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
    select(organisation, business_area, portfolio_id, risk_unique_id, report_date, criticality_level, trend, strategy) %>%
    arrange(risk_unique_id, report_date) %>%
    distinct() %>% 
    group_by(risk_unique_id) %>%
    mutate(
        prev_criticality = lag(criticality_level),
        prev_date = min(report_date),
        change = prev_criticality-criticality_level ,
        time_diff = as.numeric(difftime(report_date, prev_date, units = "days")),
        rate_of_change = ifelse(!is.na(change), change / time_diff, NA)
    ) %>% 
    filter(!is.na(change), change>0)

# Post mititgation 
data_cleaned <- data_cleaned %>%
    mutate(
        postmit_criticality_level = case_when(
            postmit_criticality == "H1" ~ 1,
            postmit_criticality == "H2" ~ 2,
            postmit_criticality == "H3" ~ 3
        )
    )


postmit_criticality_range <- data_cleaned %>%
    select(organisation, business_area, portfolio_id, risk_unique_id, report_date, criticality_level, postmit_criticality_level, trend, strategy, unique_mitigation_id) %>%
    arrange(risk_unique_id) %>%
    distinct() %>%
    group_by(risk_unique_id) %>%
    mutate(
        postmit_change = postmit_criticality_level - criticality_level,
        postmit_improvement = ifelse(postmit_change > 0, "Improved", ifelse(postmit_change > 0, "Worsened", "No Change"))
    ) %>%
    filter(!is.na(postmit_change)) 



#2. Evaluating the success rate of mitigating risks over time 


evaluating_success = data_cleaned %>%
    select(organisation, business_area, portfolio_id, risk_unique_id, report_date, pre_prob,post_prob, pre_cost,post_cost, unique_mitigation_id) %>%
    mutate(
        post_prob = as.numeric(post_prob),
        pre_prob = as.numeric(pre_prob)
    ) %>%
    group_by(risk_unique_id) %>%
    mutate(
        prob_change = case_when(!is.na(unique_mitigation_id) ~ post_prob - pre_prob # Case when mitigation 
        )) %>% 
    mutate(
        cost_change = case_when(!is.na(unique_mitigation_id) ~ post_cost - pre_cost # Case when mitigation 
        )  )

avg_changes <- evaluating_success %>%
    filter(!is.na(prob_change), !is.na(cost_change)) %>%
    group_by(risk_unique_id) %>%
    summarise(
        avg_prob_change = mean(prob_change),
        avg_cost_change = mean(cost_change)
    ) %>%
    ungroup()


#3. Emergence Rate: Identifying periods of triggers associated with new risk idenitifcation 
# Calculation: Analysing the frequency of new risks over time.
# Useful plot looking at the trend over various months 

emergence_rate <- data_cleaned %>%
    group_by(start) %>%
    summarise(
        new_risks = n_distinct(risk_unique_id)
    )

emergence_rate$month_name <- month(emergence_rate$start, label = TRUE, abbr = FALSE)


total_risks <- sum(emergence_rate$new_risks)

emergence_rate <- emergence_rate %>%
    mutate(
        emergence_rate = new_risks / total_risks
    )

emergence_rate <- emergence_rate %>%
    group_by(month_name) %>%
    summarise(
        new_risks = sum(new_risks),
        emergence_rate = sum(new_risks) / total_risks
    ) %>%
    ungroup()



#4. Likelihood of Risk and Impact Drift: Tracking shifts in project risk exposure

#4.1 Looking at change in probability over time (likelihood)

probability_change <- data_cleaned %>%
    select(risk_unique_id, report_date, pre_prob) %>%
    distinct() %>% 
    mutate(
        pre_prob = as.numeric(pre_prob) # Convert to numeric
    ) %>%
    group_by(risk_unique_id) %>%
    mutate(
        prev_prob = lag(pre_prob),
        prev_date = min(report_date),
        change = pre_prob - prev_prob,
        time_diff = as.numeric(difftime(report_date, prev_date, units = "days")),
        rate_of_change = change/time_diff
    ) %>% 
    filter(change > 0)


#4.2 Looking at change in cost over time (impact)

cost_change <- data_cleaned %>%
    select(risk_unique_id, report_date, pre_cost) %>%
    distinct() %>% 
    group_by(risk_unique_id) %>%
    mutate(
        prev_prob = lag(pre_cost),
        prev_date = min(report_date),
        change = pre_cost -prev_prob,
        time_diff = as.numeric(difftime(report_date, prev_date, units = "days")),
        rate_of_change = change/time_diff
    ) %>% 
    filter(change > 0)

#5. Resolution rates
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