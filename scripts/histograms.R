library(tidyverse)

base_dir <- getwd()
source('scripts/get_data.R')

###############################################################################
# data 

data <- get_data() %>% 
    filter(
        !is.na(sell_price),         # get rid of rows that weren't for sale yet
        grepl("*validation*", id)   # only keep validation data
    )

# clean up
vars <- ls()
rm(list = vars[vars != "data"])

###############################################################################
# plot histograms 

#------------------------------------------------------------------------------
# histogram by weekday 

# aggregate to sales by weekday and plot histograms 
weekday_histogram <- data %>% 
    group_by(
        wm_yr_wk, weekday
    ) %>% 
    summarise(
        sales = mean(sales), 
        sd_sales = sd(sales)
    ) %>% 
    ungroup() %>% 
    ggplot() + 
    geom_density(aes(sales, ..scaled..)) + 
    facet_wrap(~ weekday, dir = 'v') + 
    theme_minimal()

#------------------------------------------------------------------------------
# histogram by dept

# aggregate to sales by weekday and plot histograms 
dept_histogram <- data %>% 
    group_by(
        date, dept_id
    ) %>% 
    summarise(
        sales = mean(sales), 
        sd_sales = sd(sales)
    ) %>% 
    ungroup() %>% 
    ggplot() + 
    geom_density(aes(x = sales, y = ..scaled..)) + 
    facet_wrap(~ dept_id, dir = 'v') + 
    theme_minimal()


