# packages
library(R6)
source(here::here("functions/player_and_lotteries.R"))

# methods
est_mehods <- c("Bisection", "Bisection-Slider", "MOBS", "PEST", "ASA")
# Game -------------------------------------------------------------------
Game <- R6Class(
  "Game",
  # private
  private = list(
    # Attributes
    init_values = # G, L_init, g, l, x1pos
      c("G"= 2000L, "L"=-2000L, "g"=300L, "l"=-300L, "x1+"=1000L),  
    n_est = 3L,  #no. of estimation. 19L = 5+ 7 + 7
    task_log = NA,  # list of vectors, should be initialized
    slider = FALSE, # using slider after `n_trial` bisections
    bound_hist =
      lapply(1:3, \(x) list(up = c(0L), low = c(0L))), # use to perform bisection
    est_type = "", 
    # Attributes of MOBS
    consis_check = FALSE,
    # Attributes for PEST 
    last_choice = c(NA, NA, NA, NA), #last element is nearest choice
    step = 320L,
    max_step = 1280L,
    extra.step = FALSE,
    # Attribute for ASA
    m_shift = 0L, 
    ASA_step = 320L,
    choice_history = list(),
    converge = FALSE,
    
    # Methods
    ## Initialization:
    init_log = function(est_type, G, random_init, ...){
        # kwargs <- list(...) 
        if (est_type %in% c("Bisection", "Bisection-Slider")){
          random_init <- FALSE
          names(private$bound_hist) <- c("L", "x1pos", "x1neg")
          #> Set bisection boundaries 
          #> (x1- lower bound set in `update_task_log`)
          private$bound_hist$L[["up"]] <-  0L
          private$bound_hist$L[["low"]] <-  -2L*private$init_values[["G"]]
          private$bound_hist$x1pos[["up"]] <-  private$init_values[["G"]]
          private$bound_hist$x1pos[["low"]] <-  0L
          private$bound_hist$x1neg[["up"]] <-  0L
        }else if (est_type == "PEST"){
          private$extra.step <- FALSE
          private$last_choice <-  c(NA, NA, NA, NA)
        } else if (est_type == "ASA"){
          private$last_choice <- NA
        }else if (est_type == "MOBS"){
          random_init <- FALSE
          names(private$bound_hist) <- c("L", "x1pos", "x1neg")
          #> Set bisection boundaries 
          #> (x1- lower bound set in `update_task_log`)
          private$last_choice <- NA #c(NA, NA)
          #> High and Low stack
          private$bound_hist$L[["up"]] <- rep(0L,3)
          private$bound_hist$L[["low"]] <-  
            rep(-2L*private$init_values[["G"]],3)
          private$bound_hist$x1pos[["up"]] <- 
            rep(private$init_values[["G"]],3)
          private$bound_hist$x1pos[["low"]] <- rep(0L,3)
          private$bound_hist$x1neg[["up"]] <- rep(0L,3)
        }
      private$task_log <- list(
        "L" = ifelse(!random_init,
                     -G,
                     runif(1, -2 * G, 0) |> {\(x) x %/% 5 * 5}()),
        "x1pos" = ifelse(!random_init,
                         (0 + (G)) / 2,
                         runif(1, 0, G) |> {\(x) x %/% 5 * 5}()),
        "x1neg" = c()
      )
      private$choice_history <- list("L" = c(),
                                     "x1pos" = c(),
                                     "x1neg" = c())
    },

    bisection_update = function(choice, #"A" or "B"/ numeric if slider used
                                cur_est, cur_trial,
                                phi) {
      upp_history <- private$bound_hist[[cur_est]][["up"]]
      low_history <- private$bound_hist[[cur_est]][["low"]]
      boundary <- c(low_history[length(low_history)],
                    upp_history[length(upp_history)]
      )
      ## Case I : Slider
      if (is.numeric((choice))){
        bounnd_range <- abs(boundary[2]-boundary[1])
        # expand interval by 3 times wider
        new_bound <- boundary + c(-bounnd_range, bounnd_range)
        # Probabilistic choice
        if (is.na(phi)){
          new_stim <- choice |> as.integer()
        }else{
          new_stim <- rnorm(1, mean = choice, sd = (1/phi)*5)|>
            as.integer()
          # boundary conditions
          if ((new_stim >= new_bound[1]) && (new_stim <= new_bound[2])){
            new_stim <- new_stim
          }else if (new_stim < new_bound[1]){
            new_stim <- new_bound[1]
          }else if (new_stim > new_bound[2]){
            new_stim <- new_bound[2]
          }
        }
      }else{
      ## Case II: Bisection
        change_option <- ifelse(cur_est == 1,
                                  "A",
                                  "B")
        upper <- ifelse(choice == change_option,
                        sum(boundary) %/% 2,
                        boundary[2])
        lower <- ifelse(choice == change_option,
                        boundary[1],
                        sum(boundary) %/% 2)
        new_stim <- (upper+lower) %/% 2
        # record 
        private$bound_hist[[cur_est]][["up"]] <- 
          c(upp_history, upper)
        private$bound_hist[[cur_est]][["low"]] <- 
          c(low_history, lower)
        }
        return(new_stim) # return bisection iteration value
      },
    # TODO: MOBS doubtly, need more checks
    MOBS_update = function(choice, #"A" or "B"/ numeric if slider used
                           cur_est, cur_trial) {
      
      low_stack <- private$bound_hist[[cur_est]][["low"]]
      high_stack <- private$bound_hist[[cur_est]][["up"]]
      boundary <- c(low_stack[1], high_stack[1])
      change_option <- ifelse(cur_est == 1,
                                "A",
                                "B")
      upper <- ifelse(choice == change_option,
                      sum(boundary) %/% 2,
                      boundary[2])
      lower <- ifelse(choice == change_option,
                      boundary[1],
                      sum(boundary) %/% 2)
      # cat(low_stack); cat(";")
      # cat(high_stack); cat("\n ")
      # print(private$consis_check)
      if (private$consis_check) {
        ## TODO: bug in first stimuli in which()
        print("Consistency Check")
        tasklog <- private$task_log[[cur_est]]
        last_stim <- tasklog[cur_trial]
        idx <- which(tasklog == last_stim)
        # print(tasklog)
        # print(private$choice_history[[cur_est]])
        notConsis <-
          ifelse(
            length(idx)==1 , # not chosen before
            FALSE,
            (private$choice_history[[cur_est]][idx[1]] != choice)
          )
        # Regression
        ## Update Stacks 
        if (notConsis && (choice == change_option)){
          private$bound_hist[[cur_est]][["low"]] <- 
            c(low_stack[-1], 0L)
        } else if (notConsis && (choice != change_option)){
          private$bound_hist[[cur_est]][["up"]] <- 
            c(high_stack[-1], 0L)
        }
        private$consis_check <- FALSE
        new_stim <- (upper + lower) %/% 2
      } else {
        # Update Stacks 
        private$bound_hist[[cur_est]][["up"]] <- 
          c(upper, high_stack)[-4]
        private$bound_hist[[cur_est]][["low"]] <- 
          c(lower, low_stack)[-4]
        private$consis_check <-
          ifelse(is.na(private$last_choice),
                 FALSE,
                 choice == private$last_choice
                 )
        new_stim <- ifelse(!private$consis_check,
                           (upper + lower) %/% 2,
                           ifelse(choice == change_option,
                                  low_stack[1],
                                  high_stack[1])
                           )
      }
      # update choice
      # print(c(choice, private$last_choice))
      private$last_choice <- choice
      return(new_stim) # return bisection iteration value
    },
    ## PEST
    PEST_update = function(choice,
                           last_point, cur_trial, cur_est){
      if (cur_trial == 1) {
        private$last_choice <- c(NA, NA, NA, NA)
        private$extra.step <- FALSE
        private$step <- 320L #5 - 1280
        private$last_choice <- c(private$last_choice, choice)[-1]
      }
      else {
        private$last_choice <- c(private$last_choice, choice)[-1]
        # trial == 2 or 3
        if (cur_trial < 4) {
          if (private$last_choice[3] != private$last_choice[4]) {
            private$step <- private$step / 2
          } else if ((cur_trial == 3) &&
                     (private$last_choice[2] == private$last_choice[3])&&
                     (private$last_choice[3] == private$last_choice[4])) {
            private$step <- private$step * 2
          }
        }else {
          # Reversal: half the step size
          if (private$last_choice[3] != private$last_choice[4]) {
            private$step <- private$step / 2
            ## Reversal after doubled: Extra step before doubled
            if (all(private$last_choice[1:3] == "A") | all(private$last_choice[1:3] == "B")) {
              private$extra.step <- TRUE
            }
          }
          # Same direction for 3 streak: Double
          else if (all(private$last_choice[2:4] == "A") | all(private$last_choice[2:4] == "B")) {
            if (private$extra.step) {
              private$extra.step <- FALSE
            } else {
              private$step <- private$step * 2
            }
          } # else{}
        }
        private$step <- ifelse(private$step > private$max_step,
                               private$max_step,
                               private$step)
      }
      
      # Update new point
      direction <- ifelse(private$last_choice[4] == "A", 1, -1)
      if (cur_est == 1) {  # L
        direction <- -direction
      }
      new_point <- last_point + direction * round(private$step/5)*5
      
      return(new_point)
      
    },
    ## ASA
    ASA_update = function(choice,
                          last_point, cur_trial, cur_est
    ){
      # TODO: save as integer
      ## Current response, see Treutwein, (1995)
      Zn <- ifelse(cur_est == 1, # L
                   as.numeric(choice == "A"),
                   as.numeric(choice == "B"))
      # print(private$m_shift)
      if (cur_trial == 1) {
        private$step <- 320L #c
        private$m_shift <- 0L # times of reversal
        private$last_choice <- c(choice)
      }else{
        if (choice != private$last_choice){
          private$m_shift <- private$m_shift + 1
        }
        private$last_choice <- c(choice)
      }
      step <- private$step %/% (2+private$m_shift) * (Zn - .5) 
      step <- round(step) # not force multiples of 5
      private$ASA_step <- abs(step) # step size of ASA
      new_point <- last_point - step
      return(new_point)
    }
  ),
  ## Public
  public = list(
    initialize =
      function(exp_params =
                 list(init_values =
                        c("G"=2000L, "L"= -2000L,
                          "g"=300L, "l"=-300L, "x1+"=1000L)
                 ),
               est_type = est_mehods,
               step_size = 320L,
               random_init = FALSE 
      ){
        est_type <- match.arg(est_type)
        private$est_type <- est_type
        private$init_values <- exp_params$init_values
        private$slider <- (est_type == "Bisection-Slider")
        private$step <- step_size
        # Initialize task_log
        G <- exp_params$init_values[["G"]]
        private$init_log(est_type, G, random_init)
      },
    show_setting = function() {
      # Output how many trials and tasks there are in this game
      setting <- c(
        "N_est" = private$n_est,
        "Init" = private$init_values,
        "est_Type" = private$est_type
      )
      return(setting)
    },
    
    # Generate a new pair of lotteries.
    generate_lotteries = function(cur_est, cur_trial) {  
      G <- private$init_values["G"]
      if (cur_est == 1) {  # L
        .A <- c(G, self$show_task_log()[["L"]][cur_trial])
        .B <- rep(0L,2)
      } else if (cur_est == 2) {  # x1pos
        .A <- c(G, 0)
        .B <- rep(self$show_task_log()[["x1pos"]][cur_trial], 2)
      } else if (cur_est == 3) {  # x1neg
        L.vec <- self$show_task_log()[["L"]]
        L <- L.vec[length(L.vec)]
        .A <- c(0, L)
        .B <- rep(self$show_task_log()[["x1neg"]][cur_trial], 2)
      }
      lottery_values <- list(".A" = .A, ".B" = .B)
      new_lotteries <- Lotteries$new(lottery_values)
      return(new_lotteries)
    },
    
    update_task_log = function(choice, cur_est, cur_trial,
                               phi, random_init = FALSE, ...) {
      #> The function takes the choice of players, compute next step,
      #> and update the task log.
      last <- private$task_log[[cur_est]][cur_trial]
      if (private$est_type == "PEST") {
        value <- private$PEST_update(choice, last_point = last,
                                   cur_est = cur_est, cur_trial = cur_trial)
      }else if (private$est_type == "ASA") {
        value <- private$ASA_update(choice, last_point = last,
                                     cur_est = cur_est, cur_trial = cur_trial)
      }else if (private$est_type == "MOBS") {
        value <- private$MOBS_update(choice,
                                    cur_est = cur_est, cur_trial = cur_trial)
      }else if (private$est_type %in%  c("Bisection", "Bisection-Slider")){
        value <- private$bisection_update(choice,
                                     cur_est = cur_est, cur_trial = cur_trial,
                                     phi = phi)
      }
      private$task_log[[cur_est]] <-
       c(private$task_log[[cur_est]], value)
      private$choice_history[[cur_est]] <-
       c(private$choice_history[[cur_est]], choice)
      # print(c(ur_est, cur_trial))
      #> end of x1-, initialize x1neg[1]
      if ((cur_trial == 1) && (cur_est == 2)) { 
        L.vec <- self$show_task_log()[["L"]]
        L <- L.vec[length(L.vec)]
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[["x1neg"]][["low"]] <- L
        }else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[["x1neg"]][["low"]] <- rep(L, 3)
        }
        private$task_log[["x1neg"]][1] <-
          ifelse(random_init,
                 runif(1, L, 0) |> {\(x) x %/% 5*5}(),
                 (L+0) %/% 2)
      }
    },
    output_exp_result = function() {
      result <- rep(0L, 2)
      log <- private$task_log
      # print(log)
      result[1] <- log[["x1pos"]][ length(log[["x1pos"]]) ]
      result[2] <- log[["x1neg"]][ length(log[["x1neg"]]) ]
      names(result) <- paste0("x1", c("+","-"))
      return(result)
    },
    reset_step = function(stepsize=320L) {
      private$step <- stepsize
      private$ASA_step <- stepsize
      private$m_shift <- 0L
      private$consis_check == FALSE
    } ,
    show_step = function(est_type,
                         ...){
      extra_arg <- list(...)
      if (est_type =="PEST") {private$step}
      else if (est_type =="ASA") {
        return(private$ASA_step)
        
      }else if (est_type %in% c("Bisection", "Bisection-Slider")){
        cur_est <- extra_arg$cur_est 
        lower <- private$bound_hist[[cur_est]][["low"]]
        upper <- private$bound_hist[[cur_est]][["up"]]
        leng <- length(lower)
        step <- (upper[leng] - lower[leng]) %/% 2
        return(step)
      }else if (est_type == "MOBS"){
        cur_est <- extra_arg$cur_est 
        lower <- private$bound_hist[[cur_est]][["low"]]
        upper <- private$bound_hist[[cur_est]][["up"]]
        step <- (upper[1] - lower[1]) %/% 2
        return(step)
      }
    },
    show_bound = function() private$bound_hist,
    show_task_log = function() private$task_log,
    show_choice_history = function() private$choice_history
  ),
  # active = list(
  #   # show_bound = function(value){
  #   #   if( missing(value)){
  #   #     private$bound_hist
  #   #   }
  #   # },
  #   show_task_log = function(value){
  #     if( missing(value)){
  #       private$task_log
  #     }
  #   },
  #   show_choice_history = function(value){
  #     if (missing(value)){
  #       private$choice_history
  #     }
  #   }
  # )
)


# `Game` 有 5 個 private attributes：
# + `exp_params`: A list, with `n_trial`, `init_value` two vectors
# + `init_value`：$G$、$L$、$g$、$l$
#   + `n_est`
# + `n_trial`
# + `task_log`: $19 \times 6$ 的 list of vectors，紀錄實驗中的所有點
# + `slider` (logical): using slider to choose indifferent point after `n_trial` bisection  
# 
# 有 1 個 private method：`bisection_update()`，根據 player 的選擇來更新下一張 lotteries 所需要的數值。  
# 有 7 個 public method：
# 
# + `show_setting()`：輸出 `n_est`、`n_trial` 和 `init_value`
# + `generate_lotteries()`：根據目前的 est_quant 和 task 產生相對應的 lottery
# + `update_task_log()`：player 選擇之後，計算下一張 lottery 會用到的值，並且紀錄下來
# + `output_exp_result()`：實驗結束後，把 $x^+_1$ 到 $x^+_8$、$x^-_1$ 到 $x^-_8$ 儲存起來，並以 1d vector 的形式輸出
# + `show_task_log()`：輸出 `task_log`，輸出格式是 list of vector



# Function: Experiment ----------------------------------------------------

## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。

experiment = function(params, #alpha, beta, lambda, wp, wn,
                      exp_params=list(
                        init_values =c(
                          "G"= 2000L,
                          "g"=300L, "l"=-300L, "x1+"=1000L),
                        random_init = FALSE,
                        converg_crit = 5L
                                      ),
                      phi, u_func = c("CRRA", "CARA"),
                      est_type = 
                        c("Bisection", "Bisection-Slider", "MOBS", "PEST", "ASA"),
                      task_log = FALSE) {  # Given player's attribute
  u_func <- match.arg(u_func)
  est_type <- match.arg(est_type)
  random_init <-  exp_params$random_init
  converg_crit <- exp_params$converg_crit
  # Initialization
  player <- Player$new(params, phi, u_func)
  game <- Game$new(exp_params, est_type,
                   random_init = random_init)
  # Start the experiment
  for (est_quant in 1:game$show_setting()[1]) { # no. of estimated quantities
    trial <-  0L
    game$reset_step()
    while (game$show_step(est_type, cur_est = est_quant) >= converg_crit) {
      # TODO: reversal critirion for MOBS
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(est_quant, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = FALSE,
                          est_quant = est_quant) 
      game$update_task_log(cur_lotteries$result, est_quant, trial, phi = phi,
                           random_init = random_init)
    }
    # If slider, there's a final step
    if (est_type == "Bisection-Slider"){
      trial <- trial + 1L
      cur_lotteries <-
        game$generate_lotteries(est_quant, trial) # (->Lottery class object)
      # update lottery by player's choice
      player$input_choice(cur_lotteries,
                          slider = T,
                          est_quant = est_quant) 
      game$update_task_log(cur_lotteries$result,
                           est_quant, trial, phi = phi,
                           random_init = random_init)
    }
  }
  # Finish the experiment
  exp_result <- game$output_exp_result()
  if (task_log) {
    exp_result <- list("estimates" = exp_result)
    exp_result$log <- game$show_task_log()
    exp_result$choice_history <- game$show_choice_history()
  }
  return(exp_result)
}