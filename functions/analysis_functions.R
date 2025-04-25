## Cleaning and Plotting Functions

# Generating Utility levels for transforming into factor 
gen_util_levels <- function(n_of_x = 6L, keep_x0 = TRUE) {
  n_of_x <- as.integer(n_of_x)
  stopifnot(is.integer(n_of_x))
  if (keep_x0) {
    c(paste0("x", n_of_x:1, "neg"),
      "x0",
      paste0("x", 1:n_of_x, "pos"))
  } else {
    c(paste0("x", n_of_x:1, "neg"),
      paste0("x", 1:n_of_x, "pos"))
    
  }
}


# pivot longer and change `utility` to factor type
utility_longer <- function(df, n_of_x = 6L){
  stopifnot(is.integer(n_of_x))
  util.levels <- gen_util_levels(n_of_x = n_of_x)
  # pivot_longer to x and utility levels
  new_df <- df %>% 
    pivot_longer(cols = starts_with("x"), 
                 names_to = "utility", 
                 values_to = "x_value") %>% 
    mutate(utility =
             factor(utility,
                    labels = -n_of_x:n_of_x,
                    levels = util.levels
             )
    )
  new_df
}


# saving pics
save_fig <- function(pname, p_path = "./figs",
                     width = 13, height = 8) {
  
  if (!file.exists(file.path(p_path, pname))) {
    ggsave(filename = pname,
           width = width,
           height = height,
           units = "in",
           device = "pdf",
           path = p_path)
  } else {
    cat("File already exists. Not saving:", file.path(p_path, pname), "\n")
  }
}


# fig labels in math expression style
latex_labels <- c(
  expression(x[6]^"-"), expression(x[5]^"-"), expression(x[4]^"-"),
  expression(x[3]^"-"), expression(x[2]^"-"), expression(x[1]^"-"),
  expression(x[1]^"+"), expression(x[2]^"+"), expression(x[3]^"+"),
  expression(x[4]^"+"), expression(x[5]^"+"), expression(x[6]^"+")
)


## Loss Averion Functions

get_minusx_utility <- function(x, data, method = c("interpolation", "power_func"), 
                               alpha = NULL, beta = NULL){
  method <- match.arg(method)
  
  minusx <- - x
  if (method == "interpolation") {
    interpolate_point <- approx(x = sort(c(data$loss, 0, data$gain)), y = -6:6, xout = minusx)
    minusx_utility <- interpolate_point$y
  } else { # method == "power_func"
    minusx_utility <- NA
  }
  
  return(minusx_utility)
}


KT2 <- function(data){
  gain_max <- max(data$gain)
  loss_min <- min(data$loss)
  
  lambda <- data %>% 
    pivot_longer(cols = c(gain, loss), names_to = "domain", values_to = 'x') %>% 
    mutate(utility = (-1)^(domain == "loss") * trial,
           minusx_utility = map_dbl(x, ~ get_minusx_utility(., data)),
           utility_ratio = pmap_dbl(list(domain, utility, minusx_utility), 
                                    ~ switch(..1, 
                                             gain = abs(..3 / ..2),
                                             loss = abs(..2 / ..3)))) %>% 
    summarise(mean = mean(utility_ratio, na.rm = TRUE),
              median = median(utility_ratio, na.rm = TRUE),
              n_reasonable_points = sum(!is.na(utility_ratio)),
              criterion = ceiling(n_reasonable_points / 2),
              n_ratio_greater_1 = sum(utility_ratio > 1, na.rm = TRUE),
              n_ratio_less_1 = sum(utility_ratio < 1, na.rm = TRUE)) %>% 
    mutate(classification = case_when(n_ratio_greater_1 > criterion  ~ "averse",
                                      n_ratio_less_1 > criterion ~ "seeking",
                                      TRUE ~ "neutral"))
  lambda_list <- lambda %>% 
    select(mean, median, classification) %>% 
    as.list()
  # return
  lambda_list
}
