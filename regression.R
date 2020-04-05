library(tidyverse)
library(tidymodels)

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

#------------------------------------------------------------------------------
# notes about variables 

# > unique(data$event_type_1)
# [1] NA "Sporting" "Cultural" "National" "Religious"
#
# > unique(data$event_type_2)
# [1] NA "Cultural" "Religious"
#
# > unique(data$dept_id)
# [1] "HOBBIES_1" "HOBBIES_2" "HOUSEHOLD_1" "HOUSEHOLD_2" "FOODS_1" "FOODS_2"    
# [7] "FOODS_3"
#
# > unique(data$store_id)
# [1] "CA_1" "CA_2" "CA_3" "CA_4" "TX_1" "TX_2" "TX_3" "WI_1" "WI_2" "WI_3"

###############################################################################
# make baseline prediction by department

reg_data <- data %>% filter(dept_id == "HOUSEHOLD_1")

# sales = price + weekday + dept + event + item 
res <- lm(reg_data$sales ~ reg_data$sell_price + 
        reg_data$weekday + reg_data$event_type_1 + 
        reg_data$store_id + reg_data$month + 
        reg_data$item_id
    )

summary(res)



