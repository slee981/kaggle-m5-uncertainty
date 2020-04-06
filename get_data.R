get_data <- function() {
    
    base_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
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
        mutate(
            weekday = factor(
                weekday, ordered = TRUE, levels = days_of_week
            ), 
            event_type_1 = replace_na(event_type_1, "None"), 
            month = as_factor(month)
        )
    
}