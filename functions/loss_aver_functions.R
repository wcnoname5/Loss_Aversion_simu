library(tidyverse)
library(tidymodels)

# data format: data.frame with 3 columns
# trial: 1, ..., 8
# gain: x1+, ..., x8+
# loss: x1-, ..., x8-

est_alpha_beta <- function(data_matrix, scale_by =8){
  u = c(1,2,3,4,5,6,7,8,-1,-2,-3,-4,-5,-6,-7,-8)/scale_by # set U(x8+)=1 and U(x8-)=1
  
  ## double scaling wrt pos and neg
  
  data_matrix[,2:9] = data_matrix[,2:9]/data_matrix[,scale_by+1]
  data_matrix[,10:17] = data_matrix[,10:17]/-data_matrix[,scale_by+9]
  
  # data_matrix[,2:9] = data_matrix[,2:9]/apply(data_matrix[,2:9], 1, FUN = max) #column
  # data_matrix[,10:17] = data_matrix[,10:17]/-apply(data_matrix[,10:17], 1, FUN = min)
  
  alpha <- data.frame(subject = data_matrix[,1],
                      pos = 1:nrow(data_matrix),
                      neg = 1:nrow(data_matrix),
                      RMSE = 1:nrow(data_matrix))
  # sum of squared error
  upos = function(a.pos){sum((ypos - xpos^a.pos)^2)} 
  uneg = function(a.neg){sum((-yneg - (-xneg)^a.neg)^2)} 
  for (i in 1:nrow(data_matrix)){
    ypos = u[1:8]
    xpos = data_matrix[i,2:9]
    yneg = u[9:16]
    xneg = data_matrix[i,10:17]
    
    alpha[i,2] <- optimize(upos,interval = c(-40,40))$minimum
    a.pos <- optimize(upos,interval = c(-40,40))$minimum 
    alpha[i,3] <- optimize(uneg,interval = c(-40,40))$minimum
    a.neg <- optimize(uneg,interval = c(-40,40))$minimum 
    alpha[i,4] <- sqrt((sum((ypos - xpos^a.pos)^2) + sum((-yneg - (-xneg)^a.neg)^2)/16))
    
    # uplot1(i)
  }
  return(alpha)
}

get_minusx_utility <- function(x, data, method = c("interpolation", "power_func"), 
                               alpha = NULL, beta = NULL){
  method <- match.arg(method)
  
  minusx <- - x
  if (method == "interpolation") {
    interpolate_point <- approx(x = sort(c(data$loss, 0, data$gain)), y = -8:8, xout = minusx)
    minusx_utility <- interpolate_point$y
  } else { # method == "power_func"
    minusx_utility <- NA
  }
  
  return(minusx_utility)
}

# Kahneman & Tversky (1979)
KT <- function(data, output = c("coefficient", "classification", "all"),
               type = c("median", "mean")){
  output <- match.arg(output)
  type <- match.arg(type)
  
  gain_max <- max(data$gain)
  loss_min <- min(data$loss)
  
  lambda <- data %>% 
    pivot_longer(cols = c(gain, loss), names_to = "domain", values_to = 'x') %>% 
    mutate(utility = (-1)^(domain == "loss") * trial,
           minusx_utility = map_dbl(x, \(x) get_minusx_utility(x, data)),
           utility_ratio = pmap_dbl(list(domain, utility, minusx_utility), 
                                    ~ switch(..1, 
                                             gain = abs(..3 / ..2),
                                             loss = abs(..2 / ..3)))) %>% 
    summarise(estimate = switch(type, 
                                median = median(utility_ratio, na.rm = TRUE),
                                mean = mean(utility_ratio, na.rm = TRUE)),
              n_reasonable_points = sum(!is.na(utility_ratio)),
              criterion = ceiling(n_reasonable_points / 2),
              n_ratio_greater_1 = sum(utility_ratio > 1, na.rm = TRUE),
              n_ratio_less_1 = sum(utility_ratio < 1, na.rm = TRUE)) %>% 
    mutate(classification = case_when(n_ratio_greater_1 > criterion  ~ "averse",
                                      n_ratio_less_1 > criterion ~ "seeking",
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

## Regress gain(x+) on loss(x-)
KT2 <- function(data, output = c("coefficient", "classification", "all")){
  output <- match.arg(output)
  
  lambda <- data %>% 
    lm(gain ~ 0 + abs(loss), data = .) %>% 
    tidy(conf.int = TRUE, conf.level = 0.95) %>% 
    mutate(classification = case_when(conf.low > 1 ~ "averse",
                                      conf.high < 1 ~ "seeking",
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

# Neilson (2002)
Neilson <- function(data, output = c("coefficient", "classification", "all"), 
                    error = FALSE, epsilon = 0){
  output <- match.arg(output)
  n_max_index <- n_min_index <- ifelse(error, 2, 1)
  
  lambda <- data %>% 
    mutate(gain_slope = trial/gain, 
           loss_slope = trial/abs(loss)) %>% 
    summarise(estimate = sort(loss_slope)[n_min_index] / # nth smallest loss slop
                sort(gain_slope, decreasing = TRUE)[n_max_index]) %>% # nth largest gain slop
    mutate(classification = case_when(estimate > 1 + epsilon ~ "averse",
                                      estimate < 1 - epsilon ~ "seeking",
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

# Function: calculate slope  ----------------------------------------------------

calculate_slope <- function(data){
  data %>% 
    mutate(trial_pre_diff = trial - lag(trial, n = 1, default = 0),
           trial_post_diff = lead(trial, n = 1, default = n() + 1) - trial,
           gain_pre_diff = gain - lag(gain, n = 1, default = 0),
           gain_post_diff = lead(gain, n = 1) - gain,
           loss_pre_diff = loss - lead(loss, n = 1),
           loss_post_diff = lag(loss, n = 1, default = 0) - loss) %>% 
    mutate(gain_pre_slope = trial_pre_diff / gain_pre_diff,
           gain_post_slope = trial_post_diff / gain_post_diff,
           loss_pre_slope = trial_pre_diff / loss_pre_diff,
           loss_post_slope = trial_post_diff / loss_post_diff,
           gain_mean_slope = 0.5 * (gain_pre_slope + gain_post_slope),
           loss_mean_slope = 0.5 * (loss_pre_slope + loss_post_slope),
           gain_delta_slope = gain_post_slope - gain_pre_slope, 
           loss_delta_slope = loss_post_slope - loss_pre_slope)
}

delta_slope <- function(data, domain = c("gain", "loss"), criteria = NULL){
  domain <- match.arg(domain)
  
  slope_result <- calculate_slope(data)
  .delta_slope <- slope_result[[paste0(domain, "_delta_slope")]]
  delta_slope <- .delta_slope[!is.na(.delta_slope)]
  
  criteria <- ifelse(is.null(criteria), 
                     ceiling(length(delta_slope) / 2), criteria)
  n_greater_0 <- sum(delta_slope > 0)
  n_less_0 <- sum(delta_slope < 0)
  
  if (n_greater_0 >= criteria) {
    curvature <- "convex"
  } else if (n_less_0 >= criteria) {
    curvature <- "concave"
  } else {
    curvature <- "linear/mixed"
  }
  
  return(curvature)
}

minusgain_in_which_loss_range <- function(gain_point, loss_points){
  x <- -gain_point
  if (x %in% loss_points) {
    index <- which(x == loss_points)
  } else {
    index <- 0.5 + sum(loss_points > x)
  }
  
  return(index)
}

map_minusgain_slope <- function(index){
  which_slope <- ifelse(index %% 1 == 0, "loss_mean_slope", "loss_post_slope")
  
  return(which_slope)
}

# Wakker & Tversky (1993) -------------------------------------------------
WT <- function(data, output = c("coefficient", "classification", "all"),
               type = c("median", "mean")){
  output <- match.arg(output)
  type <- match.arg(type)


  data2 <- calculate_slope(data)
  
  lambda <- data2 %>% 
    mutate(.index = map_dbl(gain, ~ minusgain_in_which_loss_range(., data2$loss)),
           .slope = map_chr(.index, ~ map_minusgain_slope(.))) %>% 
    filter(.index <= n()) %>% # remove .index == 8.5 (out of the range of loss domain)
    mutate(minusgain_slope = map2_dbl(.index, .slope, 
                                      ~ data2 %>% filter(trial == ceiling(.x)) %>% pull(.y)),
           slope_ratio = minusgain_slope / gain_mean_slope) %>% 
    summarise(estimate = switch(type,
                                "median" = median(slope_ratio, na.rm = TRUE),
                                "mean" = {mean(slope_ratio, na.rm = TRUE) %>% 
                                    ifelse(is.nan(.), NA, .)}),
              n_reasonable_points = sum(!is.na(slope_ratio)), 
              criterion = ceiling(n_reasonable_points / 2),
              n_ratio_greater_1 = sum(slope_ratio > 1, na.rm = TRUE),
              n_ratio_less_1 = sum(slope_ratio < 1, na.rm = TRUE)) %>% 
    mutate(classification = case_when(n_ratio_greater_1 > criterion  ~ "averse",
                                      n_ratio_less_1 > criterion ~ "seeking",
                                      (n_ratio_greater_1 == 0) & (n_ratio_less_1 == 0) ~ NA_character_,
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

# Bowman et al. (1999)
Bowman <- function(data, output = c("coefficient", "classification", "all"), 
                   error = FALSE, epsilon = 0){
  output <- match.arg(output)
  n_max_index <- n_min_index <- ifelse(error, 2, 1)
  
  data2 <- calculate_slope(data)
  
  lambda <- data2 %>%  
    summarise(estimate = sort(loss_mean_slope)[n_min_index] / # nth smallest loss slop
                sort(gain_mean_slope, decreasing = TRUE)[n_max_index]) %>% # nth largest gain slop
    mutate(classification = case_when(estimate > 1 + epsilon ~ "averse",
                                      estimate < 1 - epsilon ~ "seeking",
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

# Kobberling & Wakker (2005)
KW <- function(data, output = c("coefficient", "classification", "all"), 
               epsilon = 0){
  output <- match.arg(output)
  
  lambda <- data %>% 
    summarise(estimate = min(gain) / abs(max(loss))) %>% 
    mutate(classification = case_when(estimate > 1 + epsilon ~ "averse",
                                      estimate < 1 - epsilon ~ "seeking",
                                      TRUE ~ "neutral"))
  
  if (output == "coefficient") {
    lambda$estimate
  } else if (output == "classification") {
    lambda$classification
  } else {
    lambda
  }
}

# prelec <- function(p, gamma){
#   return(p^(gamma)/(p^gamma+(1-p)^gamma)^(1/gamma))
# }
