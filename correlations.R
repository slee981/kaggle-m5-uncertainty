library(tidyverse)
library(ggcorrplot)

base_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(base_dir)

source('get_data.R')

###############################################################################
# data 

data <- get_data() %>% 
    filter(
        !is.na(sell_price),         # get rid of rows that weren't for sale yet
        grepl("*validation*", id)   # only keep validation data
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
    select(- date) 

dept_corr_mat <- dept_data %>% 
    as.data.frame() %>% 
    cor(use = "pairwise.complete.obs")

dept_corr_mat %>% 
    ggcorrplot(
        legend.title = "", 
        outline.color = "white"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        title = "Correlation by Department"
    )

#------------------------------------------------------------------------------
# correlation by weekday-dept

weekday_dept_data <- data %>% 
    group_by(wm_yr_wk, weekday, dept_id) %>% 
    summarise(
        sales = mean(sales)
    ) %>% 
    unite(
        "weekday_dept", weekday:dept_id
    ) %>%
    pivot_wider(
        id_cols = wm_yr_wk, 
        names_from = weekday_dept, 
        values_from = sales
    ) %>% 
    ungroup() %>% 
    select(- wm_yr_wk) 

weekday_dept_corr_mat <- weekday_dept_data %>% 
    as.data.frame() %>% 
    cor(use = "pairwise.complete.obs")

weekday_dept_corr_mat %>% 
    ggcorrplot(
        legend.title = "", 
        outline.color = "white"
    ) +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        title = "Correlation by weekday-department"
    )
