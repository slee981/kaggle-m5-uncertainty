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

depts <- unique(data$dept_id)

reg_data <- data %>% filter(dept_id == "HOUSEHOLD_1")

# sales = price + weekday + dept + event + item 
res <- lm(sales ~ sell_price + 
        weekday + event_type_1 + 
        store_id + month + 
        item_id, 
        data = reg_data
    )

summary(res)

# make point estimates
eval_data <- reg_data[341:348, ]
eval_id <- eval_data$id
eval_y <- eval_data$sales
eval_x <- eval_data %>% 
    select(
        sales, sell_price, weekday, event_type_1, store_id, month, item_id
    ) %>% 
    as.data.frame()

pred_y <- predict(res, eval_x)


