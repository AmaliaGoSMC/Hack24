
# Load the necessary packages
library(tidyverse)


# Load the data
risk <- read_csv("data/Risks.csv", show_col_types = F) %>%
    # Remove rows with END in Risk ID and #VALUE! in Start
    filter(`Risk ID` != "END",
           `Start` != "#VALUE!") %>%
    distinct() %>%
    # Clean column names: convert to lowercase and replace spaces with underscores
    rename_with(~ str_replace_all(tolower(.), " ", "_")) %>%
    # Clean column names: remove dots
    rename_with(~ str_remove_all(.x, "\\.")) %>%
    mutate(
        # Parse dates using consistent format
        report_date = parse_date_time(report_date, "%m-%y"),
        start = parse_date_time(start, "%m-%y"),
        relief = parse_date_time(relief, "%m-%y"),
        
        # Clean and convert cost columns
        across(c(pre_cost, post_cost, pre_factored_cost, post_factored_cost),
               ~ str_replace_all(.x, "\\£", "") %>%
                   str_trim() %>%
                   str_replace_all("k$", "*1000") %>%
                   parse_number()
        )
    )

mitigation <- read_csv("data/Mitigations.csv", show_col_types = F) %>%
    # get rid of end row
    filter(!`Risk ID`== "END",
           !`Report Date` == "#VALUE!") %>%
    distinct() %>%
    rename_with(~ str_replace_all(tolower(.), " ", "_")) %>%
    rename_with(~ str_remove_all(tolower(.), "[ .]"))%>% # Removes spaces and dots instead of replacing
    mutate(report_date = parse_date_time(report_date, "%b-%y"))

# cols in common in both datasets
common_cols <- intersect(names(risk), names(mitigation))

# Join the data and further clean
data_cleaned <- left_join(risk, mitigation, by = common_cols, relationship = "many-to-many") %>%
    mutate(risk_unique_id = tolower(.$risk_unique_id),
           risk_unique_id = str_replace_all(risk_unique_id, "--r", ""), # Remove "--r"
           risk_unique_id = str_replace_all(risk_unique_id, "--", ""), # Remove "--"
           risk_unique_id = str_trim(risk_unique_id), # Remove leading/trailing spaces
           unique_mitigation_id = tolower(.$unique_mitigation_id),
    ) %>%
    select(-c(`risk_id`, ))

# cols in common in both datasets
common_cols <- intersect(names(risk), names(mitigation))

# Join the data and further clean
data_cleaned <- left_join(risk, mitigation, by = common_cols, relationship = "many-to-many") %>%
    mutate(risk_unique_id = tolower(.$risk_unique_id),
           risk_unique_id = str_replace_all(risk_unique_id, "--r", ""), # Remove "--r"
           risk_unique_id = str_replace_all(risk_unique_id, "--", ""), # Remove "--"
           risk_unique_id = str_trim(risk_unique_id), # Remove leading/trailing spaces
           unique_mitigation_id = tolower(.$unique_mitigation_id),
           
    ) %>%
    select(-c(`risk_id`, )) %>% 
    # make unique main_risk_cat variable
    mutate(
        main_risk_cat = tolower(main_risk_cat),  # Convert to lowercase
        main_risk_cat = case_when(
            str_detect(main_risk_cat, "\\b1\\b") ~ "1. Business Strategy and Objectives",
            str_detect(main_risk_cat, "\\b2\\b") ~ "2. Bid Management",
            str_detect(main_risk_cat, "\\b3\\b") ~ "3. Competitors",
            str_detect(main_risk_cat, "\\b4\\b") ~ "4. Customers",
            str_detect(main_risk_cat, "\\b5\\b") ~ "5. Project Management",
            str_detect(main_risk_cat, "\\b6\\b") ~ "6. Contractual/Legal",
            str_detect(main_risk_cat, "\\b7\\b") ~ "7. Finance",
            str_detect(main_risk_cat, "\\b8\\b") ~ "8. System Engineering",
            str_detect(main_risk_cat, "\\b9\\b") ~ "9. Hardware/Software Development",
            str_detect(main_risk_cat, "\\b10\\b") ~ "10. System Integration, Verification & Validation",
            str_detect(main_risk_cat, "\\b11\\b") ~ "11. Supply Chain",
            str_detect(main_risk_cat, "\\b12\\b") ~ "12. Acquisition and Offsets",
            str_detect(main_risk_cat, "\\b13\\b") ~ "13. Logistic Support and Services",
            str_detect(main_risk_cat, "\\b14\\b") ~ "14. Other Risks and Contingencies",
            str_detect(main_risk_cat, "\\b15\\b") ~ "5. Project Management",
            TRUE ~ main_risk_cat  # Retain original value if no match
        )
    ) %>%
    mutate(
        action_description = str_remove(action_description, '^\\d+\\.\\s*'), # Remove leading numbers + dot + space
        action_description = str_remove(action_description, '^\\d+\\,\\s*'), # Remove leading numbers + dot + space
        action_description = str_remove(action_description, '^\\d+\\)\\s*'), # Remove leading numbers + ) + space
        action_description = str_remove(action_description, '\\.\\s*'), # Remove dot + space
        action_description = str_remove(action_description, '^"'), # Remove leading double quotes if present
        action_description = str_remove(action_description, '^“'), # Remove leading double quotes if present
        action_description = str_trim(action_description) # Trim any remaining spaces
    )
    

main_risk_category_amount <- data_cleaned %>%
    mutate(project_id = as.factor(project_id)) %>%
    select(project_id, strategy, main_risk_cat) %>%
    filter(main_risk_cat != "END") %>%
    group_by(project_id, main_risk_cat) %>%
    summarise(risk_category_amount = n())

data_cleaned = data_cleaned %>% 
    mutate(
        quality_accept_with_actions = case_when(
            strategy == "Accept" & !is.na(action_description) ~ "1",
            TRUE ~ "0"
        )
    ) %>% 
    mutate(quality_no_action = case_when(
        strategy == "Accept" & is.na(action_description) ~ "1",
        TRUE ~ "0"
    ))

data_cleaned = data_cleaned %>% 
    mutate(pre_prob = as.numeric(pre_prob)) %>% 
    mutate(post_prob = as.numeric(post_prob))
