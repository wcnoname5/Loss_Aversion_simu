source(here::here("functions/player_and_lotteries.R"))
source(here::here("functions/game_and_exp.R"))
# Function: Experiment ----------------------------------------------------
## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。

mix_param = list(UD_delta = 20L, # step size change to UD method
                 stop_rev_times = 5L # time for reversal
                 )
                 

experiment = function(params,
                      #alpha, beta, lambda, wp, wn,
                      exp_params = list(
                        init_values = c(
                          "G" = 2000L,
                          "g" = 300L,
                          "l" = -300L,
                          "x1+" = 1000L
                        ),
                        random_init = FALSE,
                        stop_crit = 5L,
                        step_size = 320L
                      ),
                      phi,
                      u_func = c("CRRA", "CARA"),
                      est_type =
                        c("Bisection", "Bisection-Slider", "MOBS", "PEST", "ASA"),
                      task_log = FALSE,
                      ...) {
  # Given player's attribute
  u_func <- match.arg(u_func)
  est_type <- match.arg(est_type)
  step_size <- exp_params$step_size
  stop_crit <- exp_params$stop_crit
  random_init <- exp_params$random_init
  # Initialization
  player <- Player$new(params, phi, u_func)
  # TODO: mixture adaptive and UD method
  extra.arg <- list(...)
  # print(paste0("TO: ", extra.arg))
  # print(is.null(extra.arg$mix_param))
  is.mixture <- !is.null(extra.arg$mix_param)
  if (is.mixture) {
    # print("Mixure")
    if (est_type %in% c("Bisection", "Bisection-Slider")){
      stop("Mixture Method not supported for Bisection/Slider")
    }
    UD_delta <- extra.arg$mix_param$UD_delta
    stop_rev_times <- extra.arg$mix_param$stop_rev_times
    if (is.null(UD_delta) || is.null(stop_rev_times)) {
      stop("Error: 'UD_delta' or 'stop_rev_times' cannot be NULL.")
    }
    game <- Game$new(exp_params, est_type,
                     random_init = random_init,
                     step_size = step_size,
                     mix_param = mix_param)
  }else {
    game <- Game$new(exp_params, est_type,
                     random_init = random_init,
                     step_size = step_size)
  }
  # Start the experiment
  for (est_quant in 1:game$show_setting()[1]) {
    # no. of estimated quantities
    trial <-  0L
    game$reset_step()
    while (game$show_step(est_type, cur_est = est_quant) >= stop_crit) {
      # TODO: reversal criterion for MOBS
      trial <- trial + 1L
      # if (trial%%5 == 1){
      # print(game$show_step(est_type, cur_est = est_quant))
      # }
      cur_lotteries <-
        game$generate_lotteries(est_quant, trial) # (->Lottery class object)
      # update lottery by player's choice
      # print(paste0("Est: ", est_quant,"; ", "trial: ", trial))
      # print(paste0("A: ", cur_lotteries$A))
      # print(paste0("B: ", cur_lotteries$B))
      player$input_choice(cur_lotteries,
                          slider = FALSE,
                          est_quant = est_quant)
      game$update_task_log(
        cur_lotteries$result,
        est_quant,
        trial,
        phi = phi,
        random_init = random_init
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