source(here::here("functions/player_and_lotteries.R"))
source(here::here("functions/game_and_exp.R"))
library(tidyverse)
# Function: Experiment ----------------------------------------------------
## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。

experiment = function(params,
                      #alpha, beta, lambda, wp, wn,
                      exp_params,
                      phi,
                      u_func = c("CRRA", "CARA"),
                      est_type =
                        c("Bisection", "Bisection-Slider", "MOBS", "PEST", "ASA"),
                      ShowTaskLog = FALSE,
                      ...) {
  # Given player's attribute
  u_func <- match.arg(u_func)
  est_type <- match.arg(est_type)
  n_est <- ifelse(!is.null(exp_params$n_est), exp_params$n_est, 3L)
  stop_crit <- exp_params$stop_crit
  # Initialization
  player <- Player$new(params, phi, u_func)
  extra.arg <- list(...)
  ShowBound <- ifelse(is.null(extra.arg$ShowBound),
                      FALSE,
                      extra.arg$ShowBound)
  is.mixture <- !is.null(extra.arg$mix_param)
  if (is.mixture) {
    if (est_type %in% c("Bisection", "Bisection-Slider", "MOBS")){
      stop("Mixture Method not supported for Bisection/Slider")
    }
    UD_delta <- extra.arg$mix_param$UD_delta
    stop_rev_times <- extra.arg$mix_param$stop_rev_times
    if (is.null(UD_delta) || is.null(stop_rev_times)) {
      stop("Error: 'UD_delta' or 'stop_rev_times' cannot be NULL.")
    }
    game <- Game$new(exp_params, est_type, n_est = n_est,
                     mix_param = extra.arg$mix_param)
  }else {
    game <- Game$new(exp_params, est_type, n_est = n_est)
  }
  # Start the experiment
  for (cur_task_idx in 1:game$show_setting()[["N_est"]]) {
    # no. of estimated quantities
    trial <-  0L
    game$reset_step()
    # cat(paste("cur_task_idx:", cur_task_idx,
    #  "step:", game$show_step(est_type, cur_task_idx = cur_task_idx),"\n\n"))
    while (game$show_step(est_type, cur_task_idx = cur_task_idx) >= stop_crit) {
      # TODO: reversal criterion for MOBS
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(cur_task_idx, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = FALSE,
                          est_quant = cur_task_idx)
      game$update_task_log(
        choice = cur_lotteries$result,
        cur_task_idx = cur_task_idx,
        cur_trial = trial,
        random_init = exp_params$random_init,
        phi = phi
      )
    }
    # If slider, there's a final step
    if (est_type == "Bisection-Slider") {
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(cur_task_idx, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = TRUE,
                          est_quant = cur_task_idx)
      game$update_task_log(
        cur_lotteries$result,
        cur_task_idx,
        trial,
        phi = phi
      )
    }
  }
  # Finish the experiment
  if (ShowTaskLog) {
    exp_result <- list("estimates" = game$output_exp_result())
    exp_result$log <- game$show_task_log()
    exp_result$choice_history <-
      game$show_choice_history()
    if (ShowBound){
      exp_result$bound_history <-
        game$show_bound()
    }
  } else {
    exp_result <- game$output_exp_result()
  }
  return(exp_result)
}

# Generating-Preprocessing Simulation Data ----------------------------------------------

make_log <- function(rep, params, exp_params, phi,
                     est_type =
                       c("Bisection", "Bisection-Slider", "PEST", "ASA", "MOBS"),
                     ...){
  extra.arg <- list(...)
  est_type <- match.arg(est_type)
  is.mixture <- !is.null(extra.arg$mix_param)
  if (is.mixture){
    mix_param <- extra.arg$mix_param 
  }
  for (i in 1:rep){
    if (is.mixture){
      result <- experiment(
        params=params, exp_params = exp_params,
        phi = phi, u_func = "CRRA", est_type = est_type,
        ShowTaskLog = TRUE,
        mix_param = mix_param
      )
    }else{
      result <- experiment(
        params = params, exp_params = exp_params,
        phi = phi, u_func = "CRRA", est_type = est_type,
        ShowTaskLog =TRUE
      )
    }
    est.df <- result$estimates %>%
      as_tibble_row() %>%
      rename_with(~paste(.x, "est", sep="_"))
    
    # each entry is a vector
    df.tmp <- map(result$log, list) %>% 
      as.tibble() %>% 
      mutate(Nsim = i, .before =1)
      add_column(est.df)
    
    if(i==1) {df <- df.tmp}
    else {df <- rbind(df, df.tmp)}
  }
  # df <- df %>% 
  #   mutate(lambda = -x1pos/x1neg)
  df
}

cleaning <- function(df) {
  get_step <- function(vec){
    vec <- diff(vec)
    len <- length(vec)
    new_vec <- c(vec[-len], NA, NA)
    new_vec
  }
  max_num <- str_extract(names(a), "x[1-9](pos|neg)") %>% 
    str_extract("[1-9]") %>% 
    as.numeric() %>% 
    max(na.rm = T)
  if (max_num > 2){
    last_x_name <- paste0("x",max_num,"neg")
  } else{
    .name <- names(df)[length(names(df))]
    last_x_name <- str_remove(.name, "_est")
  }
  df %>%
    mutate(
      across(L:!!sym(last_x_name),
             \(x) map(x, get_step), # diff 
             .names = "{.col}_step"
      ),
      across(L:!!sym(last_x_name),
             \(x) map_dbl(x, \(x) x[1]),
             .names = "{.col}_start"),
      across(L:!!sym(last_x_name),
             \(x) map_dbl(x, length),
             .names = "{.col}_len"),
      lambda_KW = -x1pos_est / x1neg_est
    )
}

# Summary Table -----------------------------------------------------------

summary_stats2 <- function(df) {
  df %>%
    select(Nsim, ends_with("est"), ends_with("len")) %>%
    filter(x1neg_est != 0) %>% 
    mutate(lambda_est = -x1pos_est / x1neg_est) %>%
    pivot_longer(
      cols = -Nsim,
      names_to = c("variable", "type"),
      names_pattern = "^(.*)_(.*)$",
      values_to = "values"
    ) %>%
    group_by(type, variable) %>% 
    summarise(across(
      values,
      list(
        mean = \(x) mean(x) |> round(2),
        median = \(x) median(x, na.rm = TRUE) |> round(2),
        sd = \(x) sd(x, na.rm = TRUE) |> round(2),
        lower95 = \(x) quantile(x, .025) |> round(2),
        Q1 = \(x) quantile(x, .25) |> round(2),
        Q3 = \(x) quantile(x, .75) |> round(2),
        upper95 = \(x) quantile(x, .975) |> round(2),
        min = \(x) min(x),
        max = \(x) max(x)
      ),
      .names = "{.fn}"
    ),
    .groups = "drop")
}

make_summ_table <- function(df_list){
  n_methods <- length(df_list)
  map(df_list, summary_stats2) %>%
    bind_rows(.id = "Est_method") %>%
    mutate(
      variable = factor(variable,
                        levels = c("L", "x1pos", "x1neg", "lambda")
      )
    ) %>%
    arrange(type, variable) %>% 
    add_column(
      true_value= c(rep(opt, rep(n_methods, 4)) , rep(NA, 3*n_methods)) ,
      .after = "sd"
    ) %>%
    mutate(med_bias = round(median - true_value,2),
           mean_bias = round(mean - true_value,2),
           .after = sd)
}