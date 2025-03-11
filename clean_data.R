
# Load the necessary packages
library(tidyverse)

# Load the data
risk <- read_csv("/Users/amalia.gomoiu1/Desktop/Hack24/data/Risks.csv", show_col_types = F) %>%
    # get rid of end row
    filter(!`Risk ID`== "END") %>%
    distinct() %>%
    rename_with(~ str_replace_all(tolower(.), " ", "_")) %>%
    rename_with(~ str_remove_all(tolower(.), "[ .]")) # Removes spaces and dots instead of replacing




mitigation <- read_csv("/Users/amalia.gomoiu1/Desktop/Hack24/data/Mitigations.csv", show_col_types = F) %>%
    # get rid of end row
    filter(!`Risk ID`== "END") %>%
    distinct() %>%
    rename_with(~ str_replace_all(tolower(.), " ", "_")) %>%
    rename_with(~ str_remove_all(tolower(.), "[ .]")) # Removes spaces and dots instead of replacing


# cols in common
common_cols <- intersect(names(risk), names(mitigation))

# Join the data and further clean
data_cleaned <- left_join(risk, mitigation, by = common_cols) %>%
    mutate(risk_unique_id = tolower(.$risk_unique_id),
           risk_unique_id = str_replace_all(risk_unique_id, "--r", ""), # Remove "--r"
           risk_unique_id = str_replace_all(risk_unique_id, "--", ""), # Remove "--"
           risk_unique_id = str_trim(risk_unique_id) # Remove leading/trailing spaces
    ) %>%
    select(-c(`risk_id`, ))
