source(here::here("functions", "player_and_lotteries.R"))
source(here::here("functions", "new_game.R"))
library(dplyr)
library(purrr)
library(furrr)
library(stringr)
# Function: Experiment ----------------------------------------------------
experiment <- function(params,
                       exp_params,
                       phi,
                       u_func = c("CRRA", "CARA"),
                       elicit_method = c("Bisection",
                                    "Bisection-Slider",
                                    "SimpBisection",
                                    "MOBS",
                                    "PEST",
                                    "ASA"),
                       ShowTaskLog = FALSE,
                       ...) {
  # Given player's attribute
  n_est <- ifelse(!is.null(exp_params$n_est), exp_params$n_est, 3L)
  u_func <- match.arg(u_func)
  elicit_method <- match.arg(elicit_method)
  min_step <- exp_params$min_step
  # Early stop criterion
  if (is.null(exp_params$early_stop)) {
    early_stop <- 1000L
  } else {
    early_stop <- exp_params$early_stop
  }
  # Initialization
  player <- Player$new(params, phi, u_func)
  extra.arg <- list(...)
  ShowBound <- extra.arg$ShowBound %||% FALSE
  is.mixture <- !is.null(extra.arg$mix_param)
  if (is.mixture) {
    if (elicit_method %in% c("Bisection-Slider", "MOBS")) {
      stop("Mixture Method not supported for Bisection-Slider/MOBS")
    }
    game <- Game$new(exp_params,
                     elicit_method,
                     n_est = n_est,
                     late_phase_param = extra.arg$mix_param)
  } else {
    game <- Game$new(exp_params, elicit_method, n_est = n_est)
  }
  # Start the experiment
  for (cur_task_idx in 1:n_est) {
    trial <-  0L
    game$reset_step()
    while (game$get_current_step(cur_task_idx, trial) >= min_step) {
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(cur_task_idx, trial) # (-> Lottery class object)
      # Update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = FALSE,
                          est_quant = cur_task_idx)
      game$update_task_log(
        choice = cur_lotteries$result,
        cur_task_idx = cur_task_idx,
        cur_trial = trial,
        random_init = exp_params$random_init,
        phi = phi,
        fix_bnd_width = exp_params$fix_bnd_width
      )
      # early stop: if current iteration >= early stop
      if (trial >= early_stop) {
        break
      }
      start_late_phase <-
        ifelse(is.null(game$get_late_phase_status()),
               FALSE,
               game$get_late_phase_status())
      cur_strategy <- ifelse(start_late_phase,
                             extra.arg$mix_param$est_method,
                             elicit_method)
      if (extra.arg$show_each_step %||% FALSE) {
        cat(cur_task_idx, "-", trial, "Method: ", cur_strategy, "\n")
      }
    }
    # If slider, there's a final step
    if (elicit_method == "Bisection-Slider") {
      cur_lotteries <-
        game$generate_lotteries(cur_task_idx, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = TRUE,
                          est_quant = cur_task_idx)
      game$update_task_log(cur_lotteries$result,
                           cur_task_idx,
                           trial,
                           phi = phi)
    }
  }
  # Finish the experiment
  if (ShowTaskLog) {
    exp_result <- list("estimates" = game$get_estimates())
    exp_result$log <- game$get_task_log()
    exp_result$choice_history <-
      game$get_choice_history()
    if (ShowBound) {
      exp_result$bound_history <-
        game$get_bound_history()
    }
  } else {
    exp_result <- game$get_estimates()
  }
  return(exp_result)
}

# Generating-Preprocessing Simulation Data ----------------------------------------------
make_log <- function(n.rep, params, exp_params, phi,
                     elicit_method = c("Bisection", "Bisection-Slider", "SimpBisection", "MOBS", "PEST", "ASA"),
                     ...) {
  # Match the argument for estimation type once
  elicit_method <- match.arg(elicit_method)
  extra.arg <- list(...)
  is.mixture <- !is.null(extra.arg$mix_param)
  mix_param <- if (is.mixture) extra.arg$mix_param else NULL

  # Create experiment configuration once
  exp_config <- list(
    params = params,
    exp_params = exp_params,
    phi = phi,
    u_func = "CRRA",
    est_type = elicit_method,
    ShowTaskLog = TRUE,
    mix_param = mix_param
  )

  # Optimize the replication processor
  process_replication <- function(i) {
    source(here::here("functions", "player_and_lotteries.R"))
    source(here::here("functions", "new_game.R"))

    # Experiment result
    result <- do.call(experiment, exp_config)

    # Extract estimates more efficiently
    est.df <- result$estimates %>%
      tibble::as_tibble_row() %>%
      rename_with(~ paste(.x, "est", sep = "_"))

    # More efficient data frame creation
    bind_cols(
      tibble(Nsim = i),
      map(result$log, \(x) list(x)) |> as_tibble(), # to tibble
      est.df
    )
  }

  # Set up parallel processing with appropriate chunk size
  n_cores <- parallel::detectCores() - 1
  chunk_size <- max(1, floor(n.rep / (3 * n_cores)))  # Dynamic chunk size

  # Configure parallel processing
  plan(multisession, workers = n_cores)
  # plan(sequential)
  # Process replications in parallel with progress tracking
  message(sprintf("Starting %d replications using %d cores", n.rep, n_cores))
  start_time <- Sys.time()

  df <- future_map_dfr(
    1:n.rep,
    process_replication,
    .options = furrr_options(
      seed = TRUE,
      chunk_size = chunk_size,
      scheduling = 2  # Dynamic scheduling
    ),
    .progress = TRUE
  )

  end_time <- Sys.time()
  message(sprintf("Completed in %.2f minutes", 
                  as.numeric(difftime(end_time, start_time, units = "mins"))))

  df
}


cleaning2 <- function(df) {
  max_num <- str_extract(names(df), "x[1-9](pos|neg)") %>%
    str_extract("[1-9]") %>%
    as.numeric() %>%
    max(na.rm = T)
  # find last name
  if (max_num > 2){
    last_x_name <- paste0("x",max_num,"neg")
  } else{
    .name <- names(df)[length(names(df))]
    last_x_name <- str_remove(.name, "_est")
  }
  df %>%
    mutate(
      across(L:!!sym(last_x_name),
             \(x) map_dbl(x, length),
             .names = "{.col}_len"),
      lambda_KW = - (x1pos_est / x1neg_est)
    )
}