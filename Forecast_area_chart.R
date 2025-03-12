#data manupulation----------------
totals_by_cost_type <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost) %>%
    pivot_longer(
        cols = c(pre_factored_cost, post_factored_cost),
        names_to = "cost_type",
        values_to = "cost"
    ) %>%
    group_by(filter_year = format(relief, "%Y"), cost_type) %>%
    summarise(cost = sum(cost)) %>%
    filter(filter_year %in% c(2016:2018))

# Create final dataset with totals
final_dataset <- bind_rows(
    totals_by_cost_type,
    totals_by_cost_type %>%
        group_by(cost_type) %>%
        summarise(
            filter_year = "Total",
            cost = sum(cost)
        )
)

# Calculate total values for labeling
facet_totals <- final_dataset %>%
    group_by(cost_type) %>%
    summarise(total = sum(cost))

# Enhanced area chart with facet labels

facet_totals <- final_dataset %>%
    filter(filter_year == "Total") %>%
    select(-filter_year)%>%
    mutate(cost_type = case_match(
        cost_type,
        "pre_factored_cost" ~ "Pre factored cost",
        "post_factored_cost" ~ "Post factored cost"))

#area chart without forecast-----------
enhanced_area_plot <- final_dataset %>%
    mutate(cost_type = case_match(
        cost_type,
        "pre_factored_cost" ~ "Pre factored cost",
        "post_factored_cost" ~ "Post factored cost")) %>%
    filter(filter_year != "Total")%>%
    ggplot(aes(x = filter_year, y = cost, 
               fill = cost_type, group = cost_type)
               ) +
    geom_area(position = "stack", alpha = 0.7) +
    geom_line(aes(color = cost_type), size = 1) +
    facet_wrap(~cost_type, scales = "free_y") +
    theme_minimal() +
    theme(
        legend.position = c(0.85, 0.95),
        legend.justification = c(1, 1),
        panel.grid.minor = element_blank(),
    ) +
    xlab(element_blank()) + ylab (element_blank()) +
    geom_text(
        data = facet_totals,
        aes(label = sprintf("Total: £%.0fK", cost/1000)),
        x = Inf,
        y = Inf,
        vjust = 1,
        hjust = 1,
        check_overlap = TRUE
    )+
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        limits = c(0,25000),
        minor_breaks = NULL
    ) + 
    theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    # Add facet strip text formatting
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))
enhanced_area_plot

####forecast_area

pre_post_cost <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost) %>%
    pivot_longer(cols = c(pre_factored_cost, post_factored_cost),
                 names_to = "cost_type",
                 values_to = "cost") %>%
    group_by(relief,cost_type) %>%
    summarise(cost = sum(cost))%>%
    arrange(relief)%>%
    mutate(
        year = format(relief, "%Y-%m"),
        filter_year = format(relief, "%Y")
    ) %>%
    ungroup() %>%
    select(-relief)%>%
    filter(filter_year %in% c(2016:2018))%>%
    group_by(filter_year,cost_type) %>%
    summarise(cost = sum(cost))%>%ungroup()%>%
    tibble()

# Filter data
pre_fact_cost <-  pre_post_cost %>%
    filter(cost_type =="pre_factored_cost",
           filter_year %in% c(2016:2018))

post_fact_cost <- pre_post_cost %>%
    filter(cost_type =="post_factored_cost",
           filter_year %in% c(2016:2018))

# Create time series objects separately for each cost type
ts_pre <- ts(
    filter(pre_fact_cost, cost_type == "pre_factored_cost")$cost,
    start = c(2016, 1),
    frequency = 1
)

ts_post <- ts(
    filter(post_fact_cost, cost_type == "post_factored_cost")$cost,
    start = c(2016, 1),
    frequency = 1
)

# Fit ARIMA models
fit_pre <- auto.arima(ts_pre)
fit_post <- auto.arima(ts_post)

# Generate forecasts for 2019
forecast_pre <- forecast(fit_pre, h = 1)
forecast_post <- forecast(fit_post, h = 1)

# Plot original series
autoplot(ts_pre) +
    labs(title = "Monthly Pre-Factored Cost Time Series",
         subtitle = "2016-2018",
         x = "Date",
         y = "Cost")

autoplot(ts_post) +
    labs(title = "Monthly Post-Factored Cost Time Series",
         subtitle = "2016-2018",
         x = "Date",
         y = "Cost")
# Add forecast data to your dataframe
forecast_data <- bind_rows(
    data.frame(
        filter_year = 2019,
        cost_type = "pre_factored_cost",
        cost = forecast_pre$mean[1]
    ),
    data.frame(
        filter_year = 2019,
        cost_type = "post_factored_cost",
        cost = forecast_post$mean[1]
    )
)%>%
    mutate(filter_year = as.character(filter_year))

# Combine actual and forecast data
combined_data <- bind_rows(
    pre_post_cost,
    forecast_data
)%>%rbind(.,combined_data %>%
              group_by(cost_type) %>%
              summarise(cost = sum(cost))%>%
              mutate(filter_year = "Total"))


# Enhanced area chart with facet labels

facet_totals <- combined_data %>%
    filter(filter_year == "Total") %>%
    select(-filter_year)%>%
    mutate(cost_type = case_match(
        cost_type,
        "pre_factored_cost" ~ "Pre factored cost",
        "post_factored_cost" ~ "Post factored cost"))

# Create area chart----------
forecast_area_plot <- combined_data %>%
    mutate(cost_type = case_match(
        cost_type,
        "pre_factored_cost" ~ "Pre factored cost",
        "post_factored_cost" ~ "Post factored cost")) %>%
    filter(filter_year != "Total") %>%
    ggplot(aes(x = filter_year, y = cost, group = cost_type)) +
    geom_area(aes(fill = interaction(cost_type, factor(filter_year == 2019))),
              alpha = 0.7,
              position = "stack") +
    geom_vline(xintercept = "2018", linetype = "dashed", color="red") +  # Added dashed vertical line
    scale_fill_manual(values = c(
        "Pre factored cost.FALSE" = "lightblue",
        "Post factored cost.FALSE" = "lightgreen",
        "Pre factored cost.TRUE" = "red",
        "Post factored cost.TRUE" = "darkred"
    ),
    labels = c("Pre factored cost", "Post factored cost"))+
    annotate("text", x = min(combined_data$filter_year), y = 22500, 
             label = "Forecast", hjust = -9, vjust = -5, size=5) +  # Added forecast label
    theme(legend.title = element_blank())+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    
    theme(legend.position = "none") +
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        minor_breaks = NULL
    )
forecast_area_plot

ggsave(paste("/Users/cilemgul.bozdemir/dev/Hack/images/pre_post_cost_forecast.png", sep=""),
       plot = last_plot(), width = 960, height = 640, dpi = 72, units = "px")
ggsave(paste("/Users/cilemgul.bozdemir/dev/Hack/images/pre_post_cost_forecast.png", sep=""),
       plot = last_plot(), width = 10, height = 6.6, dpi = 300, units = "in")



####monthly forecast all
monthly_pre_post_cost <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost) %>%
    pivot_longer(cols = c(pre_factored_cost, post_factored_cost),
                 names_to = "cost_type",
                 values_to = "cost") %>%
    group_by(relief,cost_type) %>%
    summarise(cost = sum(cost))%>%
    arrange(relief)%>%
    mutate(
        year = format(relief, "%Y-%m")
        # ,
        # filter_year = format(relief, "%Y")
    ) %>%
    ungroup() %>%
    select(-relief)%>%
    # filter(filter_year %in% c(2016:2018))%>%
    # group_by(filter_year,cost_type) %>%
    group_by(year,cost_type) %>%
    summarise(cost = sum(cost))%>%ungroup()%>%
    tibble()%>%
    mutate(year = as.Date(paste0(year, "-01")))


data_cleaned_pre_post_cost_basic <- monthly_pre_post_cost %>%
    mutate(filter_year = as_numeric(year)) %>%
    ggplot(aes(x = year, y = cost, fill = year)) +
    geom_bar(stat = "identity") +
    facet_wrap(~cost_type) +
    labs(title = "Yearly cost of relief",
         x = "Year",
         y = "Cost") +
    theme_minimal() +
    theme(legend.position = "none")
data_cleaned_pre_post_cost_basic

monthly_sequence <- seq(
    from = as.Date("2016-01-01"),
    to = as.Date("2018-12-01"),
    by = "month"
)
formatted_months <- format(monthly_sequence, "%Y-%m-%d")

# Filter data
pre_fact_cost <- monthly_pre_post_cost %>%
    filter(
        cost_type == "pre_factored_cost",
        year %in% formatted_months
    )

post_fact_cost <- monthly_pre_post_cost %>%
    filter(cost_type =="post_factored_cost",
           year %in% formatted_months)

# Create time series objects separately for each cost type
ts_pre <- ts(
    filter(pre_fact_cost, cost_type == "pre_factored_cost")$cost,
    start = c(2016, 1),
    frequency = 12
)

ts_post <- ts(
    filter(post_fact_cost, cost_type == "post_factored_cost")$cost,
    start = c(2016, 1),
    frequency = 12
)

# Fit ARIMA models
fit_pre <- auto.arima(ts_pre)
fit_post <- auto.arima(ts_post)

# Generate forecasts for 2019
forecast_pre <- forecast(fit_pre, h = 12)
forecast_post <- forecast(fit_post, h = 12)

forecast_pre$residuals
forecast_post$residuals

# Plot original series
autoplot(ts_pre) +
    labs(title = "Monthly Pre-Factored Cost Time Series",
         subtitle = "2016-2018",
         x = "Date",
         y = "Cost")

autoplot(ts_post) +
    labs(title = "Monthly Post-Factored Cost Time Series",
         subtitle = "2016-2018",
         x = "Date",
         y = "Cost")

ggplot() +
    geom_line(aes(x = time(ts_pre), y = ts_pre, color = "Pre-Factored Cost")) +
    geom_line(aes(x = time(ts_post), y = ts_post, color = "Post-Factored Cost")) +
    geom_line(aes(x = time(forecast_pre$mean), y = forecast_pre$mean, color = "Pre-Factored Cost Forecast")) +
    geom_line(aes(x = time(forecast_post$mean), y = forecast_post$mean, color = "Post-Factored Cost Forecast")) +
    labs(title = "Monthly Pre- and Post-Factored Cost Time Series",
         subtitle = "2016-2018",
         x = "Date",
         y = "Cost") +
    scale_color_manual(values = c("Pre-Factored Cost" = "green",
                                  "Post-Factored Cost" = "red",
                                  "Pre-Factored Cost Forecast" = "lightblue",
                                  "Post-Factored Cost Forecast" = "darkblue")) +
    theme_minimal()

###separate monthly forecast (main_risk_cat)

monthly_pre_post_cost <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost,main_risk_cat) %>%
    pivot_longer(cols = c(pre_factored_cost, post_factored_cost),
                 names_to = "cost_type",
                 values_to = "cost") %>%
    group_by(relief,cost_type,main_risk_cat) %>%
    summarise(cost = sum(cost))%>%
    arrange(relief)%>%
    mutate(
        year = format(relief, "%Y-%m")
        # ,
        # filter_year = format(relief, "%Y")
    ) %>%
    ungroup() %>%
    select(-relief)%>%
    # filter(filter_year %in% c(2016:2018))%>%
    # group_by(filter_year,cost_type) %>%
    group_by(year,cost_type,main_risk_cat) %>%
    summarise(cost = sum(cost))%>%ungroup()%>%
    tibble()%>%
    mutate(year = as.Date(paste0(year, "-01")))