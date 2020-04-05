library(tidyverse)
library(ggcorrplot)

base_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(base_dir)

data_dir <- file.path(base_dir, 'data')

calendar_fname <- file.path(data_dir, 'calendar.csv')
sales_validation_fname <- file.path(data_dir, 'sales_train_validation.csv')
prices_fname <- file.path(data_dir, 'sell_prices.csv')

days_of_week <- c(
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
)

###############################################################################
# read data while processing to limit memory use 

data <- read_csv(sales_validation_fname) %>% 
    pivot_longer(
        cols = -contains('id'), 
        names_to = 'day', 
        values_to = 'sales'
    ) %>% 
    left_join(
        read_csv(calendar_fname), 
        by = c('day' = 'd')
    ) %>% 
    filter(
        date > as.Date("2015-06-01")
    ) %>% 
    left_join(
        read_csv(prices_fname), 
        by = c(
            'wm_yr_wk' = 'wm_yr_wk', 
            'item_id' = 'item_id', 
            'store_id' = 'store_id'
        )
    ) %>% 
    filter(
        !is.na(sell_price)
    ) %>% 
    mutate(
        weekday = factor(
            weekday, ordered = TRUE, levels = days_of_week
        ), 
        event_type_1 = replace_na(event_type_1, "None"), 
        month = as_factor(month)
    )

###############################################################################
# graphs

#------------------------------------------------------------------------------
# correlation by weekday 

weekday_data <- data %>% 
    group_by(wm_yr_wk, weekday) %>% 
    summarise(
        sales = mean(sales)
    ) %>% 
    pivot_wider(
        id_cols = wm_yr_wk, 
        names_from = weekday, 
        values_from = sales
    ) %>% 
    ungroup() %>% 
    select(- wm_yr_wk) 

weekday_corr_mat <- weekday_data %>% 
    as.data.frame() %>% 
    cor(use = "pairwise.complete.obs")

weekday_corr_mat %>% 
    ggcorrplot(
        legend.title = "", 
        outline.color = "white"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        title = "Correlation by Weekday"
    )

#------------------------------------------------------------------------------
# correlation by dept

dept_data <- data %>% 
    group_by(date, dept_id) %>% 
    summarise(
        sales = mean(sales)
    ) %>% 
    pivot_wider(
        id_cols = date, 
        names_from = dept_id, 
        values_from = sales
    ) %>% 
    ungroup() %>% 
    select(- wm_yr_wk) 

weekday_corr_mat <- weekday_data %>% 
    as.data.frame() %>% 
    cor(use = "pairwise.complete.obs")

weekday_corr_mat %>% 
    ggcorrplot(
        legend.title = "", 
        outline.color = "white"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        title = "Correlation by Weekday"
    )
