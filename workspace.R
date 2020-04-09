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
# create emperical cdf

calc_cdf <- function(item_sales, graph = FALSE) {
    sales <- sort(item_sales)
    cdf   <- 1:length(sales) / length(sales)
    
    data <- tibble(cdf = cdf, sales = sales)
    return(data)
}

calc_predictions <- function(ecdf_data, nsteps = 201) {
    
    # normalize sales to 0 / 1 make logit prediction
    max_sales <- max(ecdf_data$sales)
    
    cdf <- ecdf_data$cdf
    sales <- ecdf_data$sales / max_sales
    res <- glm(sales ~ cdf, family = binomial("logit"))
    
    # make zero one predictions 
    x    <- seq(0, 1, length = nsteps)
    pred <- predict(res, list(cdf = x), type = "response")
    
    # rescale so that the 99th percentile is the max sales
    # i hate myself for doing this, but manually override the 
    # 95th+ percentile observations to rescale the tails to max
    max_pred               <- max(pred) # this is less than one, which we don't want
    idx                    <- which(x > 0.98)[1]
    rescaled_pred_95th     <- pred[0: idx] * max_sales 
    rescaled_pred_99th     <- tail(pred, -idx) * max_sales / max_pred
    rescaled_pred          <- c(rescaled_pred_95th, rescaled_pred_99th)
    return(
        tibble(
            quantile = x, 
            sales_prediction = rescaled_pred
        )
    )
}

plot_predictions <- function(observed_data, prediction_data) {
    cdf     <- observed_data$cdf 
    sales   <- observed_data$sales
    cdf_fit <- prediction_data$quantile 
    x       <- prediction_data$sales_prediction
    plot(cdf, sales)
    lines(cdf_fit, x, col = "steelblue", lty = 2)
}

filter_item_sales <- function(data, item_id, dow, m) {
    
    # storage 
    weekend <- c("Sunday", "Saturday")       # weekday and weekends? 
    q1      <- 1:3                           # quarterly? i.e 1:3, 4:6, 7:9, 10:12
    q2      <- 4:6
    q3      <- 7:9
    q4      <- 10:12
    
    cat("Filtering sales data for item ", item_id, "on day", dow, "for month", m ,"\n")
    return(data %>%
        filter(
            id == !!item_id 
        ) %>% 
        filter(
            if (any(!!dow %in% weekend)) {
                weekday %in% weekend
            } else {
                !weekday %in% weekend
            }
        ) %>% 
        filter(
            if (any(m %in% q1)) {
                month %in% q1
            } else if (any(m %in% q2)) {
                month %in% q2
            } else if (any(m %in% q3)) {
                month %in% q3
            } else {
                month %in% q4
            }
        ) %>% 
        select(
            sales
        )
    )
}

make_sales_prediction <- function(data, item_id, dow, m, graph = FALSE) {
    item_sales <- data %>% filter_item_sales(item_id, dow, m)
    ecdf_data <- item_sales$sales %>% calc_cdf()
    sales_predictions <- calc_predictions(ecdf_data)
    if (graph) {
        plot_predictions(ecdf_data, sales_predictions)
    }
    return (sales_predictions)
}


###############################################################################
# create emperical cdf

set.seed(981)
items   <- sample(data$id, 7) 
day     <- "Sunday"
month   <- 12

prediction_data <- data %>% make_sales_prediction(items[1], day, month, graph = TRUE)
prediction_data <- data %>% make_sales_prediction(items[2], day, month, graph = TRUE)
prediction_data <- data %>% make_sales_prediction(items[3], day, month, graph = TRUE)
prediction_data <- data %>% make_sales_prediction(items[4], day, month, graph = TRUE)
prediction_data <- data %>% make_sales_prediction(items[5], day, month, graph = TRUE)


