#Total Pre & Post Factored Cost by Relief Day
data_cleaned_pre_post_cost_all <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost) %>% #,risk_type,main_risk_cat
    pivot_longer(cols = c(pre_factored_cost, post_factored_cost),
                 names_to = "cost_type",
                 values_to = "cost") %>%
    group_by(relief,cost_type) %>%#,risk_type,main_risk_cat
    summarise(cost = sum(cost))%>%
    arrange(relief)%>%
    mutate(
        year = format(relief, "%Y-%m"),
        filter_year = format(relief, "%Y")
    ) %>%
    ungroup() %>%
    select(-relief)%>%
    filter(filter_year %in% c(2016:2018))%>%
    group_by(filter_year,cost_type) %>% #,risk_type,main_risk_cat
    summarise(cost = sum(cost))%>%ungroup()%>%
    tibble()

#area chart
data_cleaned_pre_post_cost_yearly_chart <- data_cleaned_pre_post_cost_all %>%
    mutate(filter_year = as_numeric(filter_year),
           cost_type = case_match(
               cost_type,
               "pre_factored_cost" ~ "Pre factored cost",
               "post_factored_cost" ~ "Post factored cost")) %>%
    ggplot(aes(x = filter_year, y = cost, fill = cost_type)) +
    geom_area(stat = "identity") +
    facet_wrap(~ cost_type,labeller = labeller(cost_type = label_wrap_gen(width = 15))) +
    xlab(element_blank()) + ylab (element_blank()) +
    theme_minimal() +
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        limits = c(0,25000),
        minor_breaks = NULL
    ) +
    scale_x_continuous(breaks = c(2016:2018)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    # Add facet strip text formatting
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))

data_cleaned_pre_post_cost_yearly_chart

ggsave(paste("/Users/cilemgul.bozdemir/dev/Hack/pre_post_cost_yearly_area_chart_di.png", sep=""),
       plot = last_plot(), width = 960, height = 640, dpi = 72, units = "px")
ggsave(paste("/Users/cilemgul.bozdemir/dev/Hack/pre_post_cost_yearly_chart_di.png", sep=""),
       plot = last_plot(), width = 10, height = 6.6, dpi = 300, units = "in")


#bar chart
data_cleaned_pre_post_cost_yearly_bar_chart <- data_cleaned_pre_post_cost %>%
    mutate(filter_year = as_numeric(filter_year),
           cost_type = case_match(
               cost_type,
               "pre_factored_cost" ~ "Pre factored cost",
               "post_factored_cost" ~ "Post factored cost")) %>%
    ggplot(aes(x = filter_year, y = cost, fill = cost_type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ cost_type,labeller = labeller(cost_type = label_wrap_gen(width = 15))) +
    xlab(element_blank()) + ylab (element_blank()) +
    theme_minimal() +
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        limits = c(0,25000),
        minor_breaks = NULL
    ) +
    scale_x_continuous(breaks = c(2016:2018)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    # Add facet strip text formatting
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))

data_cleaned_pre_post_cost_yearly_bar_chart


#Total Pre&Post Factored Cost by Relif date by Main Risk Category
data_cleaned_pre_post_cost_main_cat <- data_cleaned %>%
    select(relief, pre_factored_cost, post_factored_cost,risk_type,main_risk_cat) %>% #
    pivot_longer(cols = c(pre_factored_cost, post_factored_cost),
                 names_to = "cost_type",
                 values_to = "cost") %>%
    group_by(relief,cost_type,risk_type,main_risk_cat) %>%
    summarise(cost = sum(cost))%>%
    arrange(relief)%>%
    mutate(
        year = format(relief, "%Y-%m"),
        filter_year = format(relief, "%Y")
    ) %>%
    ungroup() %>%
    select(-relief)%>%
    filter(filter_year %in% c(2016:2018))%>%
    group_by(filter_year,cost_type,risk_type,main_risk_cat) %>% #
    summarise(cost = sum(cost))%>%ungroup()%>%
    tibble()

#faced wrap main risk category column charts
data_cleaned_pre_post_cost_yearly_chart <- data_cleaned_pre_post_cost_main_cat %>%
    mutate(filter_year = as_numeric(filter_year)) %>%
    ggplot(aes(x = filter_year, y = cost, fill = main_risk_cat)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ main_risk_cat,labeller = labeller(main_risk_cat = label_wrap_gen(width = 15))) +
    xlab(element_blank()) + ylab (element_blank()) +
    theme_minimal() +
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        limits = c(0,5000),
        minor_breaks = NULL
    ) +
    scale_x_continuous(breaks = c(2016:2018)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    # Add facet strip text formatting
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))
data_cleaned_pre_post_cost_yearly_chart

#Forecating -------
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

data_cleaned_pre_post_cost_basic <- pre_post_cost %>%
    mutate(filter_year = as_numeric(filter_year)) %>%
    ggplot(aes(x = filter_year, y = cost, fill = filter_year)) +
    geom_bar(stat = "identity") +
    facet_wrap(~cost_type) +
    labs(title = "Yearly cost of relief",
         x = "Year",
         y = "Cost") +
    theme_minimal() +
    theme(legend.position = "none")
data_cleaned_pre_post_cost_basic

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
)

# Create the enhanced visualization
forecast_bar_plot <- combined_data %>%
    mutate(filter_year = as_numeric(filter_year),
           cost_type = case_match(
               cost_type,
               "pre_factored_cost" ~ "Pre factored cost",
               "post_factored_cost" ~ "Post factored cost")) %>%
    ggplot(aes(x = factor(filter_year), y = cost, 
               alpha = if_else(filter_year == 2019, 0.3, 1),
               fill = if_else(filter_year == 2019, "Forecast", "Actual"))) +
    geom_bar(stat = "identity") +
    facet_wrap(~cost_type) +
    theme_minimal() +
    theme(
        legend.position = c(0.85, 0.95),
        legend.justification = c(1, 1),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "plain")
    ) +
    scale_x_discrete(labels = c("2016", "2017", "2018", "2019")) +
    scale_alpha_identity() +
    scale_fill_manual(
        values = c("lightblue", "#999999"),
        guide = guide_legend(title = "Type")
    ) +
    theme(legend.background = element_blank())+
    scale_y_continuous(
        labels = dollar_format(suffix="", prefix="£"),
        limits = c(0,25000),
        minor_breaks = NULL
    ) +    theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    # Add facet strip text formatting
    theme(strip.text.x = element_text(
        size = 12,
        margin = margin(5, 0, 5, 0, "pt"),
    ))
forecast_bar_plot


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