source(here::here("functions/player_and_lotteries.R"))
source(here::here("functions/game_and_exp.R"))
library(tidyverse)
# Function: Experiment ----------------------------------------------------
## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。
# exp_params = list(
#   init_values = c(
#     "G" = 2000L,
#     "g" = 300L,
#     "l" = -300L,
#     "x1+" = 1000L
#   ),
#   random_init = FALSE,
#   stop_crit = 5L,
#   step_size = 320L
# )

experiment = function(params,
                      #alpha, beta, lambda, wp, wn,
                      exp_params,
                      phi,
                      u_func = c("CRRA", "CARA"),
                      est_type =
                        c("Bisection", "Bisection-Slider", "MOBS", "PEST", "ASA"),
                      task_log = FALSE,
                      ...) {
  # Given player's attribute
  u_func <- match.arg(u_func)
  est_type <- match.arg(est_type)
  # step_size <- exp_params$step_size
  stop_crit <- exp_params$stop_crit
  # random_init <- exp_params$random_init
  # Initialization
  player <- Player$new(params, phi, u_func)
  extra.arg <- list(...)
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
    game <- Game$new(exp_params, est_type,
                     mix_param = extra.arg$mix_param)
  }else {
    game <- Game$new(exp_params, est_type)
  }
  # Start the experiment
  for (est_quant in 1:game$show_setting()[1]) {
    # no. of estimated quantities
    trial <-  0L
    game$reset_step()
    while (game$show_step(est_type, cur_est = est_quant) >= stop_crit) {
      # TODO: reversal criterion for MOBS
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(est_quant, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = FALSE,
                          est_quant = est_quant)
      game$update_task_log(
        cur_lotteries$result,
        cur_est = est_quant,
        cur_trial = trial,
        phi = phi,
        random_init = exp_params$random_init
      )
    }
    # If slider, there's a final step
    if (est_type == "Bisection-Slider") {
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(est_quant, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = T,
                          est_quant = est_quant)
      game$update_task_log(
        cur_lotteries$result,
        est_quant,
        trial,
        phi = phi,
        random_init = random_init
      )
    }
  }
  # Finish the experiment
  exp_result <- game$output_exp_result()
  if (task_log) {
    exp_result <- list("estimates" = exp_result)
    exp_result$log <- game$show_task_log()
    exp_result$choice_history <-
      game$show_choice_history()
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
        phi=phi, u_func = "CRRA", est_type = est_type,
        task_log = TRUE,
        mix_param = mix_param
      )
    }else{
      result <- experiment(
        params=params, exp_params = exp_params,
        phi=phi, u_func = "CRRA", est_type = est_type,
        task_log =TRUE
      )
    }
    
    # each entry is a vector
    df.tmp <- tibble(
      Nsim = i,
      L = result$log["L"], 
      x1pos = result$log["x1pos"],
      x1neg = result$log["x1neg"])
    
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
  get_last_element <- function(vec) {
    map_dbl(vec, \(x) x[length(x)])
  }
  df %>%
    mutate(
      across(L:x1neg,
             \(x) map(x, get_step), # diff 
             .names = "{.col}_step"
      ),
      across(L:x1neg,
             \(x) map_dbl(x, \(x) x[1]),
             .names = "{.col}_start"),
      across(L:x1neg,
             get_last_element,
             .names = "{.col}_est"),
      across(L:x1neg,
             \(x) map_dbl(x, length),
             .names = "{.col}_len"),
      lambda = -x1pos_est / x1neg_est
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

# Plotting Functions ------------------------------------------------------

##> find_lims (for plotting)
find_xlims <- function(df_list, lambda=FALSE){
  # Function
  SelectEst <- function(df){
    df %>% 
      select(ends_with("est"),"lambda")
  }
  # find Max/Min to define xlims
  if (!lambda){
    .df <- map(df_list, SelectEst) %>% 
      bind_rows(.id = "method") %>% 
      filter(x1neg_est != 0) %>% 
      summarise(
        across(-1,
               # list(lower95 = ~ quantile(.x, .025),
               #      upper95 = ~ quantile(.x, .975)),
               list(max = max,
                    min = min),
               .names = "{.col}_{.fn}")
      ) %>% 
      pivot_longer(
        cols = everything(),
        names_to = c("method", ".value"),
        names_pattern = "(.*).*_(min|max)"
      )
    # Change to list
    lim_list <- .df  %>% 
      mutate(
        method = str_remove(method, '_est'),
        min = round_to_5(min, ifelse(method=="lambda", 1L, 50L)),
        max = round_to_5(max, ifelse(method=="lambda", 1L, 50L))
      ) %>%
      split(.$method) %>%
      map(~ c(.x$min, .x$max))
    
    
    } else {
    .df <- map(df_list, ~ select(.x, "lambda")) %>% 
      bind_rows(.id = "method") %>% 
      summarise(
        across(-1,
               list(lower95 = ~ quantile(.x, .05),
                    upper95 = ~ quantile(.x, .95)),
               .names = "{.col}_{.fn}")
        ) %>% 
      pivot_longer(
          cols = everything(),
          names_to = c("method", ".value"),
          names_pattern = "(.*)_(.*95)"
        )
    # Change to list
    lim_list <- .df  %>% 
      mutate(
        upper95 = round_to_5(upper95, 1L),
        lower95 = round_to_5(lower95, 1L)
      ) %>%
      split(.$method) %>%
      map(~ c(.x$lower95, .x$upper95))
    lim_list <- lim_list[[1]]
    names(lim_list) <- c(NULL, NULL)
  }
  return(lim_list)
}


##> Plot Violin
simu_violin <- function(df_list, ...){
  extra_args <- list(...)
  if (is.null(extra_args$params)){
    opt <- find_optimal_params()
  } else{
    opt <-
      find_optimal_params(params=extra_args$params)
  }
  total_df <- df_list %>% 
    map(~select(.x, ends_with("est"), "lambda")) %>% 
    bind_rows(.id = "method") %>% 
    rename_with(~ str_remove(.x, "_est"),
                ends_with("est"))
  est_names <- c("L", "x1pos", "x1neg", "lambda")
  ylims <- list(c(-3250L, +750L),
                c(-250L, 2750L),
                c(-2250L, 750L),
                c(0, 12.5)) 
  names(ylims) <- est_names
  plist <- list()
  for (est in est_names){
    fig_title <- glue("{est} Estimates")
    if (est == "lambda") {
      # Rule out extreme values
      plt_df <- total_df %>% 
        filter(lambda<12 & lambda>0)
    } else {
      plt_df <- total_df
    }
    
    plt <- plt_df %>% 
      ggplot(aes(x=method)) +
      geom_jitter(aes(y=!!sym(est)),
                  alpha=.2,
                  width = 0.25,
                  color= "grey50")+
      geom_violin(aes(y=!!sym(est), fill = method),
                  alpha = .9)+
      geom_hline(yintercept = opt[est], 
                 color = "red3",
                 lwd=1.1, linetype = 2)+
      geom_hline(yintercept = 0L,
                 color = "grey10",
                 linetype = "dotdash")+
      theme(axis.text.x =
              element_text(angle = 45, hjust=1))+
      labs(title = fig_title)
      # scale_y_continuous(limits= ylims[[est]],
      #                    breaks = seq(ylims[[est]][1],
      #                                 ylims[[est]][2],
      #                                 ifelse(est == "lambda",
      #                                        2L,
      #                                        250L)
      #                                 )
      #                    )
    # if (est == "lambda"){
    #   plt <- plt +
    #     scale_y_continuous(limits= c(0,12.5), breaks = seq(0,12,2))
    # }
    
    plist[[length(plist) + 1]] <- plt
  }

  combined_plot <- wrap_plots(plist, ncol = 2) +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  return(combined_plot)
}

simu_boxplot <- function(df_list, ...){
  extra_args <- list(...)
  if (is.null(extra_args$params)){
    opt <- find_optimal_params()
  } else{
    opt <-
      find_optimal_params(params=extra_args$params)
  }
  total_df <- df_list %>% 
    map(~select(.x, ends_with("est"), "lambda")) %>% 
    bind_rows(.id = "method") %>% 
    rename_with(~ str_remove(.x, "_est"),
                ends_with("est"))
  est_names <- c("L", "x1pos", "x1neg", "lambda")
  plist <- list()
  for (est in est_names){
    fig_title <- glue("{est} Estimates")
    # Rule out extreme values
    if (est == "lambda"){
      tmp_df <- total_df |>
        filter(lambda<12 & lambda>0)
    } else{
      tmp_df <- total_df
    }
    # Calculate 95% confidence interval (IQR-based)
    summary_stats <- total_df %>%
      group_by(method) %>%
      summarize(
        lower95 = quantile(!!sym(est), 0.025),
        upper95 = quantile(!!sym(est), 0.975)
      ) %>% 
      ungroup()
    
    plt <- tmp_df %>% 
      ggplot(aes(method))+
      # Add the 95% range as error bars
      geom_errorbar(
        data = summary_stats,
        aes(x = method, ymin = lower95, ymax = upper95),
        width = 0.7, color = "blue1"
      ) +
      geom_boxplot(aes(y= !!sym(est), fill = method),
                   outlier.alpha = .5) +
      geom_hline(yintercept = opt[est], 
                 color = "red3",
                 lwd=1.1, linetype = 2)+
      geom_hline(yintercept = 0L,
                 color = "grey10",
                 linetype = "dotdash")+
      theme(axis.text.x =
              element_text(angle = 45, hjust=1))+
      labs(title = fig_title)
    # if (est == "lambda"){
    #   plt <- plt +
    #     scale_y_continuous(limits= c(0,12.5), breaks = seq(0,12,2))
    # }
    
    plist[[length(plist) + 1]] <- plt
  }
  combined_plot <- wrap_plots(plist, ncol = 2) +
    plot_layout(guides = "collect") + 
    theme(legend.position = "right")
  return(combined_plot)
}

##> Plot Length distributions
iter_plot <- function(df_list, exclude_methods=c()){
  plist <- list()
  est_names <- c("L", "x1pos", "x1neg")
  total_df <- sublist %>% 
    map(~select(.x, ends_with("len"))) %>% 
    bind_rows(.id = "method") %>% 
    rename_with(~ str_remove(.x, "_len")) %>% 
    pivot_longer(cols = -1,
                 names_to = "estimates",
                 values_to = "iteration_num") %>% 
    mutate(estimates = factor(estimates,
                              levels = est_names))

  medians <- total_df %>% 
    filter(!method %in% exclude_methods) %>%
    group_by(estimates, method) %>%
    summarize(median_iteration = median(iteration_num),
              Q3_iteration = quantile(iteration_num, .75))

  plt <- total_df %>%
    filter(!method %in% exclude_methods) %>%
    ggplot(aes(x = iteration_num)) +
    geom_bar(aes(y = after_stat(count) / sum(after_stat(count))),
             fill = "grey80", color = "black") +
    # Add median lines
    geom_vline(data = medians, aes(xintercept = median_iteration),
               linetype = "dashed", color = "red") +
    # Add Q3 lines
    geom_vline(data = medians, aes(xintercept = Q3_iteration),
               linetype = "3313", color = "blue4") +
    ylab("Proportion") +
    scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10)) +
    # Use the reordered 'method' for facet_grid
    facet_grid(cols = vars(estimates),
               rows = vars(method),
               scales = "free_y")
  plt
}

##> Plot distributions
simu_plot2 <- function(df, fix_xlim=T, ...) {
  extra_args <- list(...)
  if (is.null(extra_args$params)){
    opt <- find_optimal_params()
  } else{
    opt <- find_optimal_params(params=extra_args$params)
  }
  show_zero <- ifelse(is.null(extra_args$show_zero),
                      TRUE,
                      extra_args$show_zero)
  
  est_names <- c("L", "x1pos", "x1neg")
  suffix <- c("est", "len")
  if (is.null(extra_args$xlims)){
    xlims <- list("L" = c(-1000, -500),
                  "x1pos" = c(650, 1200),
                  "x1neg" = c(-600,-150))
  } else if (!is.list(extra_args$xlims)){
    stop("Invalid xlims input")
  } else {
    xlims <- extra_args$xlims
  }

  # Create an empty list to store plots
  plot_list <- list()
  # Generate plots and store them in the list
  for (suf in suffix) {
    for (point in est_names) {
      .colname <- paste(point, suf, sep = "_")
      fig_suf  <- switch (suf,
                          "est" = "Estimates",
                          "len" = "Iteration Times")
      fig.title <- glue(" {fig_suf} of {point}")
      
      # Times of PEST
      if (suf == "len") { 
        plt <- df %>%
          # ggplot(aes_string(x = .colname)) +
          ggplot(aes(x = !!sym(.colname))) +
          geom_bar(aes(y = after_stat(count) / sum(after_stat(count))),
                   fill = "grey80", color = "black") +
          ylab("Propotion") +
          scale_x_continuous(breaks =
                               seq(min(df[[.colname]]),
                                   max(df[[.colname]]),
                                   by = 5))+
          theme(axis.text.x = element_text(angle = 45, hjust=1))
      } else { # Estimation
        plt <- df %>%
          # ggplot(aes_string(x = .colname)) +
          ggplot(aes(x = !!sym(.colname))) +
          geom_histogram(
            aes(y = after_stat(density)),
            bins = 20,
            fill = "grey80",
            color = "black"
          )
        # Fix x_limit == TRUE
        curr_lim <- xlims[[point]]
        x_step <- diff(curr_lim)/15
        x_step <- round_to_5(x_step, 50L)
        if (fix_xlim){
          plt <- plt +
            scale_x_continuous(
              limits = round_to_5(curr_lim, 100L),
              breaks = seq(curr_lim[1],
                           curr_lim[2],
                           by = x_step
              )
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust=1))
        }else{
          plt <- plt +
            scale_x_continuous(breaks=scales::breaks_pretty(n=7))
        }
      }
      # 
      plt <- plt +
        xlab(point) +
        ggtitle(fig.title)
      # Add line True Value & 0 value
      if (suf == "est") {
        plt <- plt +
          geom_vline(xintercept = opt[point],
                     color = "red",
                     lwd=1.1, linetype = 2)
        if (show_zero){
          plt <- plt +
            geom_vline(xintercept = 0L,
                     color = "grey10",
                     linetype = "dotdash")
        }
      }
      # Add the plot to the list
      plot_list[[length(plot_list) + 1]] <- plt
    }
  }
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plot_list, ncol = 3)
  return(combined_plot)
}

##> Plot lambda distributions
lambda_plot <- function(df_list, ncol, nrow, ...){
  # assign extra arguments variables 
  extra_args <- list(...)
  .byrow <- ifelse(is.null(extra_args$byrow),
                  TRUE,
                  extra_args$byrow)
  fix_xlim <- ifelse(is.null(extra_args$fix_xlim),
                     TRUE,
                     extra_args$fix_xlim)
  if (is.null(extra_args$xlims)){
    xlims <- c(1.4, 4)
    xlim_step <- 0.2
  } else{
    xlims <- extra_args$xlims
    xlims[1] <- 0
    xlim_step <- diff(xlims) / 16
    xlim_step <- round_to_5(xlim_step, .2)
  }
  if (is.null(extra_args$params)){
    opt <- find_optimal_params()  
  } else{
    opt <- find_optimal_params(params)
  }
  
  # start plotting 
  plist <- list()
  n_methods <- length(df_list)
  for (i in 1:n_methods) {
    plt <- df_list[[i]] %>%
      ggplot(aes(x = lambda)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30 ,
                     fill = "grey80",
                     color = "black") +
      geom_vline(
        xintercept = -opt[2] / opt[3],
        color = "red",
        lwd = 1.1,
        lty = 2
      ) +
      geom_vline(
        xintercept = 1L,
        color = "blue3",
        linetype = "dotdash"
      ) 
    if (fix_xlim){
    plt <- plt +
      scale_x_continuous(limits = xlims,
                         breaks = seq(xlims[1],
                                      xlims[2],
                                      xlim_step))
    }
    plt <- plt +
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      ggtitle( {names(df_list)[i]} )
    
    plist[[length((plist)) + 1]] <- plt
  }
  patchwork::wrap_plots(plist, ncol = ncol, nrow = nrow, byrow = .byrow) +
    plot_annotation(title = "Lambda estimates")
}

##> plot last `n` trials trace
FindLastLimit <- function(lastnum = 7, result_list=NULL,
                          combined_list = NULL,...){
  if (is.null(result_list) & is.null(combined_list)){
    stop("No Input")
  }
  lastnum <- lastnum + 1
  if (is.null(combined_list)){
    SelectLast <- function(df) {
    df %>%
      mutate(across(L:x1neg,
                    \(x) map(x, \(x) tail(x, lastnum)[-lastnum]), # stary last
                    .names = "{.col}_last")) %>%
      select(1, ends_with("last")) %>%
      unnest(cols = ends_with("last")) %>%
      mutate(last_trial =
               rep(c(-(lastnum-1):-1), {{Nsim}})
      )
  }
    combined_list <- map(result_list, SelectLast)
  }
  # To find limit
  limit.df <- combined_list %>%
    bind_rows(.id = "method") %>% 
    summarise(
      across(ends_with("last"),
             list(max = max,
                  min = min),
             .names = "{str_remove(.col, '_last')}_{.fn}"))
  limit.list <- limit.df %>% 
    pivot_longer(
      cols = everything(),
      names_to = c("method", ".value"),
      names_pattern = "(.*)_(min|max)"
      ) %>% 
    mutate(
      max = round_to_5(max, 50L),
      min = round_to_5(min, 50L)
      ) %>%
    split(.$method) %>%
    map(~ c(.x$min, .x$max))
  return(limit.list)
}

converg.plot <- function(lastnum = 7, result_list, ...){
  extra_args <- list(...)
  if (!is.null(extra_args$limit.list) &
      is.list(extra_args$limit.list)){
    limit.list <- extra_args$limit.list
  } else{
    limit.list <- NULL
  }
  lastnum <- lastnum + 1
  plot_list <- list()
  SelectLast <- function(df) {
    df %>%
      mutate(across(L:x1neg,
                    \(x) map(x, \(x) tail(x, lastnum)[-lastnum]), # stary last
                    .names = "{.col}_last")) %>%
      select(1, ends_with("last")) %>%
      unnest(cols = ends_with("last")) %>%
      mutate(last_trial =
               rep(c(-(lastnum-1):-1), {{Nsim}})
      )
  }
  combined_list <- map(result_list, SelectLast)
  # To find limit
  if (is.null(limit.list)){
    limit.list <- FindLastLimit((lastnum-1),
                                combined_list = combined_list)
  }
  for (i in 1:length(combined_list)) {
    df <- combined_list[[i]]
    df.name <- names(combined_list)[i]
    # CI dataframe
    CI.summ <- df %>%
      group_by(last_trial) %>%
      summarise(across(
        ends_with("last"),
        list(
          upper95 = ~ quantile(.x, .975),
          lower95 = ~ quantile(.x, .025)
        ),
        .names = "{str_remove(.col, '_last')}_{.fn}"
      ))
    
    for (pts in c("L", "x1pos", "x1neg")) {
      .y <-  glue(pts, "_last")
      .y_CI <- paste(pts, c("lower95", "upper95"),sep="_")
      ylim <- limit.list[[pts]]
      ylim <- round_to_5(ylim, 100L)
      y_axis_step <- diff(ylim) %/% 15
      y_axis_step <- round_to_5(y_axis_step, 50L)
      
      plt <- df %>%
        ggplot() +
        geom_line(
          aes(x = last_trial, y = !!sym(.y),
              group = Nsim),
          color = "grey50",
          linewidth = 1,
          alpha = .15
        ) +
        # CI area
        geom_ribbon(data = CI.summ,
                    aes(x = last_trial, ymin = !!sym(.y_CI[1]), ymax = !!sym(.y_CI[2])),
                    fill = "cornflowerblue",
                    alpha = .65
        ) +
        theme(legend.position = "none") +
        geom_point(
          aes(x = last_trial, y = !!sym(.y)),
          color = "grey30",
          alpha = .2
        ) +
        geom_hline(
          yintercept = opt[pts],
          color = "red", lty = 2, lwd = 1
        ) +
        # scale_x_continuous(breaks = scales::breaks_pretty()) +
        xlim(-(lastnum-1), -1) +
        scale_y_continuous(limits = ylim ,
                           breaks =
                             seq(ylim[1],
                                 ylim[2],
                                 y_axis_step)
        ) +
        # scale_colour_gradientn(colours = hcl.colors(10, alpha = .4))+
        labs(title = pts,
             subtitle = df.name,
             y = pts)
      
      plot_list[[length(plot_list) + 1]] <- plt
    }
  }
  combined_plot <- wrap_plots(plot_list, byrow = FALSE, ncol = length(result_list)) +
    plot_annotation(title = glue("Last {lastnum-1} Choice Trace"))
  combined_plot
}

##> plot all trials trace
converg.plot.bisec <- function(result_list){
  plot_list <- list()
  # Unnest along a column
  unnest_col <- function(df, column) {
    df %>%
      select(1, {{column}}) %>% 
      unnest_longer(col = {{column}}, indices_to = "Trial")
  }
  # Some useful objects
  columns_to_unnest <- c("L", "x1pos","x1neg")
  # Each Method
  for (i in 1:length(result_list)) {
    df <- result_list[[i]]
    df.name <- names(result_list)[i]
    unnested_dfs <- map(columns_to_unnest, ~ unnest_col(df, .x))
    names(unnested_dfs) <- columns_to_unnest
    # Each Estimate 
    for (j in 1:length(unnested_dfs)){
      unnest_df <- unnested_dfs[[j]]
      pts <- columns_to_unnest[j] #names
      # limit.df <- df %>%
      #   summarise(
      #   across(ends_with("last"),
      #          list(max = max,
      #               min = min),
      #          .names = "{str_remove(.col, '_last')}_{.fn}"))
      CI.summ <- unnest_df %>%
        group_by(Trial) %>% 
        summarise(
          upper95 = quantile( !!sym(pts), .975),
          lower95 = quantile( !!sym(pts), .025)
        )
      
      .y <- pts
      .y_CI <-  c("lower95", "upper95")
      # ylim <- c(limit.df[[glue("{pts}_min")]],
      #           limit.df[[glue("{pts}_max")]])
      # ylim <- ylim %/% 50 *50
      
      plt <- unnest_df %>%
        ggplot() +
        geom_line(
          aes(x = Trial, y = !!sym(.y), # symbol
              # color = Nsim,
              group = Nsim),
          color = "grey50",
          linewidth = 1,
          alpha = .15
        ) +
        geom_ribbon(data = CI.summ,
                    aes(x = Trial, ymin = !!sym(.y_CI[1]), ymax = !!sym(.y_CI[2])),
                    fill = "cornflowerblue",
                    alpha = .65
        ) +
        theme(legend.position = "none") +
        geom_point(
          aes(x = Trial, y = !!sym(.y)),
          color = "grey30",
          alpha = .2
        ) +
        geom_hline(
          yintercept = opt[pts],
          color = "red", lty = 2, lwd = 1
        ) +
        scale_x_continuous(breaks = scales::breaks_pretty()) +
        # scale_y_continuous(limits = ylim ,
        #                    breaks =
        #                      seq(ylim[1],
        #                          ylim[2],
        #                          100)
        #                    ) +
        # scale_colour_gradientn(colours = hcl.colors(10, alpha = .4))+
        labs(title = pts,
             subtitle = df.name,
             y = pts)
      plot_list[[length(plot_list) + 1]] <- plt
    }
  }
  combined_plot <- wrap_plots(plot_list, byrow = FALSE, ncol = length(result_list)) +
    plot_annotation(title = glue("Choice Trace by All Trials"))
  combined_plot
}

