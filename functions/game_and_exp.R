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
      list("G"=2000L, "L"=-2000L, "g"=300L, "l"=-300L, "x1+"=1000L),  
    n_est = 3L,  # no. of estimation. 19L = 5+ 7 + 7
    estimated_pts = 
      c("L", "x1pos", "x1neg", "L2", "G2"), # names of est quant
    task_log = NA,  # list of vectors, should be initialized
    slider = FALSE, # using slider after `n_trial` bisections
    bound_hist = list(), # For bisection
    est_type = "", 
    # Attributes of MOBS
    consis_check = FALSE,
    # Attributes for PEST
    last_choice = c(NA, NA, NA, NA), #last element is nearest choice
    PEST_step = 320L,
    max_step = 1280L,
    extra.step = FALSE,
    # Attribute for ASA
    m_shift = 0L,
    ASA_c = 4L*320L,
    ASA_step = 320L,
    # Attribute for UD (up-down)
    is.mix = FALSE,
    UD_param = list(),
    step_size = 320L,
    choice_history = list(),
    
    # Methods
    bisection_update = function(choice, #"A" or "B"/ numeric if slider used
                                cur_task_idx, cur_trial,
                                phi) {
      upp_history <- private$bound_hist[[cur_task_idx]][["up"]]
      low_history <- private$bound_hist[[cur_task_idx]][["low"]]
      boundary <- c(
        low_history[length(low_history)],
        upp_history[length(upp_history)]
      )
      if (is.numeric((choice))) {
        ## Case I : Slider
        bounnd_range <- abs(boundary[2] - boundary[1])
        # expand interval by 3 times wider
        new_bound <- boundary + c(-bounnd_range, bounnd_range)
        # Probabilistic choice
        if (is.na(phi)) {
          new_stim <- choice |> as.integer()
        } else {
          new_stim <- rnorm(1, mean = choice, sd = 20L)|>
            as.integer()
          # boundary conditions
          if ((new_stim >= new_bound[1]) && (new_stim <= new_bound[2])){
            new_stim <- new_stim
          }else if (new_stim < new_bound[1]) {
            new_stim <- new_bound[1]
          }else if (new_stim > new_bound[2]) {
            new_stim <- new_bound[2]
          }
        }
      } else {
        ## Case II: Bisection
        # change option: option that contains value one aims to elicit
        # if change option chosen, lower the upper bound (by midpoint)
        change_option <- ifelse(cur_task_idx == 1,
                                  "A",
                                  "B")
        next_upper <- ifelse(choice == change_option,
                        sum(boundary) %/% 2,
                        boundary[2])
        next_lower <- ifelse(choice == change_option,
                        boundary[1],
                        sum(boundary) %/% 2)
        new_stim <- (next_upper + next_lower) %/% 2
        # record 
        private$bound_hist[[cur_task_idx]][["up"]] <- 
          c(upp_history, next_upper)
        private$bound_hist[[cur_task_idx]][["low"]] <- 
          c(low_history, next_lower)
        }
        return(new_stim) # return bisection iteration value
      },
    # MOBS 
    MOBS_update = function(choice, cur_task_idx, cur_trial) {
      low_stack <- private$bound_hist[[cur_task_idx]][["low"]]
      high_stack <- private$bound_hist[[cur_task_idx]][["up"]]
      boundary <- c(low_stack[1], high_stack[1])
      change_option <- ifelse(cur_task_idx == 1,
                                "A",
                                "B")
      next_upper <- ifelse(choice == change_option,
                      sum(boundary) %/% 2,
                      boundary[2])
      next_lower <- ifelse(choice == change_option,
                      boundary[1],
                      sum(boundary) %/% 2)
      if (private$consis_check) {
        # Find choice history
        tasklog <- private$task_log[[cur_task_idx]]
        last_stim <- tasklog[cur_trial]
        idx <- which(tasklog == last_stim)
        # Check consistency with choice history
        notConsis <-
          ifelse(
            length(idx)==1 , # not chosen before
            FALSE,
            (private$choice_history[[cur_task_idx]][idx[1]] != choice)
          )
        # If Not Consistent: regression
        ## Update Stacks
        if (notConsis && (choice == change_option)){
          private$bound_hist[[cur_task_idx]][["low"]] <- 
            c(low_stack[-1], 0L)
        } else if (notConsis && (choice != change_option)){
          private$bound_hist[[cur_task_idx]][["up"]] <- 
            c(high_stack[-1], 0L)
        }
        # else: not update stacks
        private$consis_check <- FALSE
        
        # Prepare next stimulus (use updated stack)
        low_stack <- private$bound_hist[[cur_task_idx]][["low"]]
        high_stack <- private$bound_hist[[cur_task_idx]][["up"]]

        new_stim <- (low_stack[1] + high_stack[1]) %/% 2
      } else {
        # Update Stacks
        if (choice == change_option){
          private$bound_hist[[cur_task_idx]][["up"]] <-
            c(next_upper, high_stack)[-4]
        } else{
          private$bound_hist[[cur_task_idx]][["low"]] <-
            c(next_lower, low_stack)[-4]
        }
        
        # Check Should next trial check consistency?
        #> check if same choices
        private$consis_check <-
          ifelse(is.na(private$last_choice),
                 FALSE,
                 choice == private$last_choice
                 )
        # Prepare next stimulus
        new_stim <- ifelse(!private$consis_check,
                           (next_upper + next_lower) %/% 2,
                           ifelse(choice == change_option,
                                  next_lower,
                                  next_upper)
                           )
      }
      # update choice history
      private$last_choice <- choice
      return(new_stim) # return bisection iteration value
    },
    ## PEST
    PEST_update = function(choice, last_stim,
                           cur_task_idx, cur_trial) {
      if (cur_trial == 1) {
        private$last_choice <- c(NA, NA, NA, NA)
        private$extra.step <- FALSE
        private$last_choice <- c(private$last_choice, choice)[-1]
      } else {
        private$last_choice <- c(private$last_choice, choice)[-1]
        # trial == 2 or 3
        if (cur_trial < 4) {
          if (private$last_choice[3] != private$last_choice[4]) {
            private$PEST_step <- private$PEST_step / 2
          } else if ((cur_trial == 3) &&
                       (private$last_choice[2] == private$last_choice[3]) &&
                       (private$last_choice[3] == private$last_choice[4])) {
            private$PEST_step <- private$PEST_step * 2
          }
        } else {
          # Reversal: half the step size
          if (private$last_choice[3] != private$last_choice[4]) {
            private$PEST_step <- private$PEST_step / 2
            ## Reversal after doubled: Extra step before doubled
            if (all(private$last_choice[1:3] == "A") |
                  all(private$last_choice[1:3] == "B")) {
              private$extra.step <- TRUE
            }
          } else if (all(private$last_choice[2:4] == "A") |
                       all(private$last_choice[2:4] == "B")) {
            # Same direction for 3 streak: Double
            if (private$extra.step) {
              private$extra.step <- FALSE
            } else {
              private$PEST_step <- private$PEST_step * 2
            }
          }
        }
        private$PEST_step <- ifelse(
          private$PEST_step > private$max_step,
          private$max_step,
          private$PEST_step)
      }
      # Update new point
      direction <- ifelse(private$last_choice[4] == "A", 1, -1)
      if (cur_task_idx == 1) {  # L
        direction <- -direction
      }
      new_stim <- last_stim + direction * round_to_5(private$PEST_step)
      return(new_stim)
    },

    ## ASA
    ASA_update = function(choice,
                          last_stim, cur_task_idx, cur_trial){
      ## Current response, see Treutwein, (1995)
      Zn <- ifelse(cur_task_idx == 1, # L
                   as.numeric(choice == "A"),
                   as.numeric(choice == "B"))
      if (cur_trial == 1) {
        private$m_shift <- 0L # times of reversal
      } else{
        # trials > 1
        if (choice != private$last_choice){
            private$m_shift <- private$m_shift + 1
        }
      }
      private$last_choice <- c(choice)
      
      if (cur_trial %in% 1:2){
        step <- (private$ASA_c / cur_trial) * (Zn - .5) 
      } else {
        step <- private$ASA_c / (2 + private$m_shift) * (Zn - .5) 
      }
      step <- round(step) # not force multiples of 5
      private$ASA_step <- abs(step)
      new_stim <- last_stim - step
      return(new_stim)
    },

    # Up-down method
    UD_update = function(choice, last_stim,
                         cur_task_idx, cur_trial){
      Zn <- ifelse(cur_task_idx == 1, # L
                   as.integer(choice == "A"),
                   as.integer(choice == "B"))
      last_choice <- private$last_choice
      lc_len <- length(last_choice)
      log <- private$task_log[[cur_task_idx]]
      delta <-  private$UD_param$delta
      step_size <- diff(
        log[c(length(log), (length(log) - 1))]
        ) |>
        abs()
      shift_count <- private$UD_param$UD_shift
      stop_criterion <- private$UD_param$stop_rev_times
      # Reversal: update reversal times and reversal hist
      if (choice != last_choice[lc_len]) {
        private$UD_param$UD_shift <-
          private$UD_param$UD_shift + 1L
        private$UD_param$shift_hist <-
          c(private$UD_param$shift_hist, last_stim)
      }
      #Update new stimuli
      if (shift_count >= stop_criterion &&
            (step_size == delta)) {
        # Final estimate: midrun/last stimuli
        if (private$UD_param$UseMidrunEst) {
          new_stim <- mean(private$UD_param$shift_hist) |>
            round()
        } else {
          new_stim <- last_stim
        }
      } else {
        # If last step > delta, use UD_update not midrun estimate
        new_stim <-
          last_stim - delta * (2L * Zn - 1)
      }
      # Update Last Choice:
      private$last_choice <- c(choice)
      return(new_stim)
    },
    
    ## Initialization:
    init_log = function(est_type, init_values, random_init,
                        step_size, ...) {
      if (!is.list(init_values)) {
        stop("Input Error:'init_values' is not a list.")
      }
      private$step_size <- step_size
      if (est_type %in% c("Bisection", "Bisection-Slider")) {
        random_init <- FALSE
        names(private$bound_hist) <- private$estimated_pts
        bnd <- list("up" = 0L, "low" = 0L)
        bnd_list <- rep(list(bnd),
                        length(private$estimated_pts))
        names(bnd_list) <- private$estimated_pts
        #> Set bisection boundaries
        private$bound_hist$L[["up"]] <-  0L
        private$bound_hist$L[["low"]] <-  -2L * init_values[["G"]]
        private$bound_hist$x1pos[["up"]] <-  init_values[["G"]]
        private$bound_hist$x1pos[["low"]] <-  0L
        private$bound_hist$x1neg[["up"]] <-  0L
      } else if (est_type == "PEST") {
        private$extra.step <- FALSE
        private$last_choice <-  c(NA, NA, NA, NA)
      } else if (est_type == "ASA") {
        private$last_choice <- NA
      }else if (est_type == "MOBS") {
        random_init <- FALSE
        names(private$bound_hist) <- private$estimated_pts
        #> Set bisection boundaries 
        private$last_choice <- NA 
        #> High and Low stack
        private$bound_hist$L[["up"]] <- rep(0L,3)
        private$bound_hist$L[["low"]] <-  
          rep(-2L*init_values[["G"]],3)
        private$bound_hist$x1pos[["up"]] <- 
          rep(init_values[["G"]],3)
        private$bound_hist$x1pos[["low"]] <- rep(0L,3)
        private$bound_hist$x1neg[["up"]] <- rep(0L,3)
      }
      G <- init_values[["G"]]
      private$task_log <-
        rep(list(c()), length(private$estimated_pts))
      names(private$task_log) <- private$estimated_pts
      private$task_log[["L"]] <- 
        ifelse(!random_init,
               -G,
               runif(1, -2 * G, 0) |> round_to_5())
      private$task_log[["x1pos"]] <- 
        ifelse(!random_init,
               (0 + (G)) / 2,
               runif(1, 0, G) |> round_to_5())
      
      private$choice_history <- 
        rep(list(c()), length(private$estimated_pts))
      names(private$choice_history) <- private$estimated_pts
    }
  ),
  ## Public
  public = list(
    initialize =
      function(exp_params,
               est_type = est_mehods,
               n_est = 3L,
               ...) {
        extra.arg <- list(...)
        if (!is.null(extra.arg$mix_param)){
           private$is.mix <- TRUE
           private$UD_param <- list(
             UD_start = FALSE,
             UD_shift = 0L, # shift count
             shift_hist = c(), # shifted-stimuli
             delta = extra.arg$mix_param$UD_delta,
             stop_rev_times = extra.arg$mix_param$stop_rev_times,
             UseMidrunEst = ifelse(is.logical(extra.arg$mix_param$UseMidrunEst),
                                   extra.arg$mix_param$UseMidrunEst,
                                   TRUE)
           )
        }
        
        # check valid num. of estimates
        if ((!is.numeric(n_est)) || (((n_est - 3L) %% 2) != 0)) {
          stop("Invalid 'n_est' value.")
        }
        private$n_est <- n_est
        estimated_pts <- c("L", "x1pos", "x1neg", "L2", "G2")
        .len <- length(private$estimated_pts)
        if (n_est <= 5) {
          private$estimated_pts <- estimated_pts[1:n_est]
        } else {
          n_est <- (n_est - 5L) %/% 2L
          new_names <- paste0("x",
                              rep(1:n_est, each = 2) + 1L,
                              c("pos", "neg"))
          private$estimated_pts <- c(private$estimated_pts, new_names)
        }
        est_type <- match.arg(est_type)
        private$slider <- (est_type == "Bisection-Slider")
        private$est_type <- est_type
        private$init_values <- exp_params$init_values
        private$bound_hist <- 
          lapply(1:private$n_est, \(x) list(up = c(0L), low = c(0L)))
        # Initialize task_log
        random_init <- ifelse(is.logical(exp_params$random_init),
                              exp_params$random_init,
                              FALSE)
        step_size <- ifelse(is.numeric(exp_params$step_size),
                            exp_params$step_size,
                            320L)
        private$init_log(est_type, exp_params$init_values, random_init,
                         step_size)
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
    generate_lotteries = function(cur_task_idx, cur_trial) {  
      G <- private$init_values[["G"]]
      loss_1 <- private$init_values[["l"]]
      gain_1 <- private$init_values[["g"]]
      cur_est_pt <- private$estimated_pts[cur_task_idx]
      # the index(i, i>1) of x_i+, x_i-
      if (private$n_est <= 5) {
        x_pos <- 0L
        x_neg <- 0L
      } else {
        x_pos <- seq(6L, private$n_est, 2L)
        x_neg <- seq(7L, private$n_est, 2L)
      }

      if (cur_est_pt == "L") {  # L
        .A <- c(G, self$show_task_log()[["L"]][cur_trial])
        .B <- rep(0L, 2)
      } else if (cur_est_pt == "x1pos") {  # x1pos
        .A <- c(G, 0)
        .B <- rep(self$show_task_log()[["x1pos"]][cur_trial], 2)
      } else if (cur_est_pt == "x1neg") {  # x1neg
        cur_vec <- self$show_task_log()[["L"]]
        L <- cur_vec[length(cur_vec)]
        .A <- c(0, L)
        .B <- rep(self$show_task_log()[["x1neg"]][cur_trial], 2)
      } else if (cur_est_pt == "L2") {  # L2: (0,l_1) vs (x1+, "L2")
        cur_vec <- self$show_task_log()[["x1pos"]]
        x1pos <- cur_vec[length(cur_vec)]
        .A <- c(0L, loss_1)
        .B <- c(x1pos, self$show_task_log()[["L2"]][cur_trial])
      } else if (cur_est_pt == "G2") {  # G2: (g_1,0) vs ("G2", x1-)
        cur_vec <- self$show_task_log()[["x1neg"]]
        x1neg <- cur_vec[length(cur_vec)]
        .A <- c(gain_1, 0L)
        .B <- c(self$show_task_log()[["G2"]][cur_trial], x1neg)
      } else if (cur_task_idx %in% x_pos) {
        # current x_i: x_i^{pos}:
        x_idx <- which(x_pos == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "pos")
        last_xi_name <- paste0("x", (x_idx - 1), "pos")
        # last x_i :x_{i-1}^{pos}:
        cur_vec <- self$show_task_log()[[last_xi_name]]
        last_xipos <- cur_vec[length(cur_vec)]
        # L_2
        L2_vec <- self$show_task_log()[["L2"]]
        L2 <- L2_vec[length(L2_vec)]
        # x_i^+: (x_{i-1}^{pos}, l_1) vs ("x_i^{pos}", L_2)
        .A <- c(last_xipos, loss_1)
        .B <- c(self$show_task_log()[[cur_xi_name]][cur_trial], L2)
      } else if (cur_task_idx %in% x_neg) {
        # current x_i: x_i^{neg}:
        x_idx <- which(x_neg == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "neg")
        last_xi_name <- paste0("x", (x_idx - 1), "neg")
        # last x_i :x_{i-1}^{neg}:
        cur_vec <- self$show_task_log()[[last_xi_name]]
        last_xineg <- cur_vec[length(cur_vec)]
        # L_2
        G2_vec <- self$show_task_log()[["G2"]]
        G2 <- G2_vec[length(G2_vec)]
        # x_i^-: (g_1, x_{i-1}^{neg}) vs (G_2, "x_i^{neg}")
        .A <- c(gain_1, last_xineg)
        .B <- c(G2, self$show_task_log()[[cur_xi_name]][cur_trial])
      }
      lottery_values <- list(".A" = .A, ".B" = .B)
      if (is.na(.A) || is.na(.B)){
        print(c(cur_task_idx, cur_trial))
        print(lottery_values)
        stop("NA Lotteries Created in `generate_lotteries()`")
      } else if ((diff(.A) > 0) || (diff(.B) > 0)) {
        print(c(diff(.A), (diff(.B))))
        print(c(cur_task_idx, cur_trial))
        print(lottery_values)
        stop("Lotteries order reversed in `generate_lotteries()`")
      }
      new_lotteries <- Lotteries$new(lottery_values)
      return(new_lotteries)
    },

    # Update Initial stim in chained task
    prepare_chained_stim = function(cur_task_idx, random_init){
      # Convert estimated point to next task idx
      EstToNextIdx <- \(x) (which(private$estimated_pts == x) + 1)
      if (private$n_est <= 5) {
        x_pos <- 0L
        x_neg <- 0L
      } else {
        # end of L2, G2 also need to update
        x_pos <- c(4, seq(6L, private$n_est, 2L)) + 1L
        x_neg <- c(5, seq(7L, private$n_est, 2L)) + 1L
      }

      if (cur_task_idx == EstToNextIdx("L")) { 
        # end of L, initialize x1neg[1]
        cur_vec <- self$show_task_log()[["L"]]
        L <- cur_vec[length(cur_vec)]
        #> Update for Bisection
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[["x1neg"]][["low"]] <- L
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[["x1neg"]][["low"]] <- rep(L, 3)
        }
        # To avoid extreme cases that L>0
        private$task_log[["x1neg"]][1] <-
          ifelse(((random_init) && (L<0)),
                 runif(1, L, 0) |> round_to_5(),
                 (L + 0) %/% 2)
      } else if (cur_task_idx == EstToNextIdx("x1pos")) {
        # end of x1pos, initialize L2[1]
        cur_vec <- self$show_task_log()[["x1pos"]]
        x1pos <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        loss_1 <- private$init_values[["l"]]
        mid_point <- loss_1 - x1pos
        lower_bnd <- loss_1 + (2 * (mid_point - loss_1))
        upper_bnd <- loss_1
        #> Update for Bisection
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[["L2"]][["low"]] <- lower_bnd
          private$bound_hist[["L2"]][["up"]] <- upper_bnd
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[["L2"]][["low"]] <- rep(lower_bnd, 3)
          private$bound_hist[["L2"]][["up"]] <- rep(upper_bnd, 3)
        }
        # To avoid extreme cases that x1pos < 0
        private$task_log[["L2"]][1] <-
          ifelse(((random_init) && (lower_bnd < upper_bnd)),
                 runif(1, lower_bnd, upper_bnd) |> round_to_5(),
                 mid_point)
      } else if (cur_task_idx == EstToNextIdx("x1neg")) {
        # end of x1-, initialize G2[1]
        cur_vec <- self$show_task_log()[["x1neg"]]
        x1neg <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        gain_1 <- private$init_values[["g"]]
        mid_point <- gain_1 - x1neg
        lower_bnd <- gain_1
        upper_bnd <- gain_1 + (2 * (mid_point - gain_1))
        #> Update for Bisection
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[["G2"]][["low"]] <- lower_bnd
          private$bound_hist[["G2"]][["up"]] <- upper_bnd
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[["G2"]][["low"]] <- rep(lower_bnd, 3)
          private$bound_hist[["G2"]][["up"]] <- rep(upper_bnd, 3)
        }
        # To avoid extreme cases:
        private$task_log[["G2"]][1] <-
          ifelse(((random_init) && (lower_bnd < upper_bnd)),
                 runif(1, lower_bnd, upper_bnd) |> round_to_5(),
                 mid_point)
      } else if (cur_task_idx %in% x_pos) {
        # end of x_{i}^{pos}, initialize x_{i+1}^{pos}[1]
        ## current (i) of x_i^{pos}:
        x_idx <- which(x_pos == cur_task_idx)
        cur_xi_name <- paste0("x", x_idx, "pos")
        next_xi_name <- paste0("x", (x_idx + 1), "pos")
        # current xi value
        cur_vec <- self$show_task_log()[[cur_xi_name]]
        cur_xi <- cur_vec[length(cur_vec)]
        loss_1 <- private$init_values[["l"]]
        # L2
        L2_vec <- self$show_task_log()[["L2"]]
        L2 <- L2_vec[length(L2_vec)]
        # starting point s.t. expectation b/t are the same
        # x_i^+: (x_{i-1}^{pos}, loss_1) vs ("x_i^{pos}", L2)
        mid_point <- cur_xi + loss_1 - L2
        lower_bnd <- cur_xi
        upper_bnd <- lower_bnd + (2 * (mid_point - lower_bnd))
        #> Update for Bisection
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[[next_xi_name]][["low"]] <- lower_bnd
          private$bound_hist[[next_xi_name]][["up"]] <- upper_bnd
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[[next_xi_name]][["low"]] <- rep(lower_bnd, 3)
          private$bound_hist[[next_xi_name]][["up"]] <- rep(upper_bnd, 3)
        }
        # To avoid extreme cases:
        private$task_log[[next_xi_name]][1] <-
          ifelse(((random_init) && (lower_bnd < upper_bnd)),
                 runif(1, lower_bnd, upper_bnd) |> round_to_5(),
                 mid_point)
      } else if (cur_task_idx %in% x_neg) {
        # end of x_{i}^{neg}, initialize x_{i+1}^{neg}[1]
        ## current (i) of x_i^{neg}:
        x_idx <- which(x_neg == cur_task_idx)
        cur_xi_name <- paste0("x", x_idx, "neg")
        next_xi_name <- paste0("x", (x_idx + 1), "neg")
        # current xi value
        cur_vec <- self$show_task_log()[[cur_xi_name]]
        cur_xi <- cur_vec[length(cur_vec)]
        gain_1 <- private$init_values[["g"]]
        # G2
        G2_vec <- self$show_task_log()[["G2"]]
        G2 <- G2_vec[length(G2_vec)]
        # starting point s.t. expectation b/t are the same
        # x_{i+1}^-: (gain_1, x_i^{neg}) vs (G2, "x_{i+1}^{neg}")
        mid_point <- gain_1 + cur_xi - G2
        upper_bnd <- cur_xi 
        lower_bnd <- upper_bnd + (2 * (mid_point - upper_bnd))
        #> Update for Bisection
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          private$bound_hist[[next_xi_name]][["low"]] <- lower_bnd
          private$bound_hist[[next_xi_name]][["up"]] <- upper_bnd
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[[next_xi_name]][["low"]] <- rep(lower_bnd, 3)
          private$bound_hist[[next_xi_name]][["up"]] <- rep(upper_bnd, 3)
        }
        # To avoid extreme cases:
        private$task_log[[next_xi_name]][1] <-
          ifelse(((random_init) && (lower_bnd < upper_bnd)),
                 runif(1, lower_bnd, upper_bnd) |> round_to_5(),
                 mid_point)
      }
    },

    update_task_log = function(choice, cur_task_idx, cur_trial,
                               random_init = FALSE, ...) {
      #> The function takes the choice of players, compute next step,
      #> and update the task log.
      extra.arg <- list(...)
      if (!is.na(extra.arg$phi)) {
        phi <- extra.arg$phi
      }
      last <- private$task_log[[cur_task_idx]][cur_trial]
      UD_start <- private$UD_param$UD_start
      if (!is.null(UD_start) && UD_start) {
        value <- private$UD_update(choice, last_stim = last,
                                   cur_task_idx, cur_trial)
      } else if (private$est_type == "PEST") {
        value <- private$PEST_update(choice, last_stim = last,
                                   cur_task_idx, cur_trial)
      } else if (private$est_type == "ASA") {
        value <- private$ASA_update(choice, last_stim = last,
                                     cur_task_idx, cur_trial)

      } else if (private$est_type == "MOBS") {
        value <-
          private$MOBS_update(choice, 
                              cur_task_idx = cur_task_idx,
                              cur_trial = cur_trial)
      } else if (private$est_type %in% c("Bisection", "Bisection-Slider")) {
        value <-
          private$bisection_update(choice,
                                   cur_task_idx = cur_task_idx,
                                   cur_trial = cur_trial,
                                   phi = phi)
      }
      cur_est_pt <- private$estimated_pts[cur_task_idx]

      # Wtite task Log
      private$task_log[[cur_est_pt]] <-
        c(private$task_log[[cur_est_pt]], value)
      private$choice_history[[cur_task_idx]] <-
        c(private$choice_history[[cur_task_idx]], choice)
      # In start of a new task, update initial stim in next chained task
      if ((cur_task_idx <= private$n_est - 1) & (cur_trial == 1)) {
        # -1 because no need update next one in final
        self$prepare_chained_stim(cur_task_idx = cur_task_idx,
                                  random_init = random_init)
      }
      # For mixture, initialize the UD
      if (private$is.mix) {
        .step <- self$show_step(
          private$est_type,
          cur_task_idx)
        # Condition of Starting UD
        private$UD_param$UD_start <-
          .step <= private$UD_param$delta*4 # <=80L
      }
    },

    reset_step = function() {
      private$PEST_step <- private$step_size
      private$ASA_c <- private$step_size *2L
      private$ASA_step <- private$step_size
      private$m_shift <- 0L
      private$consis_check == FALSE
      if (private$is.mix){
        private$UD_param$UD_start  <- FALSE
        private$UD_param$UD_shift  <- 0L
        private$UD_param$shift_hist  <- c()
      }
    },

    show_step = function(est_type,
                         ...){
      extra_arg <- list(...)
      if (private$is.mix &&
          private$UD_param$UD_shift >= private$UD_param$stop_rev_times
          ){
        return(0L) # stop
      }
      if (est_type == "PEST") {
        private$PEST_step
      }else if (est_type == "ASA") {
        return(private$ASA_step)
      }else if (est_type %in% c("Bisection", "Bisection-Slider")) {
        if (is.na(extra_arg$cur_task_idx)) {
          stop("w/o specifying `cur_task_idx` in Bisection/Slider!")
        }
        cur_task_idx <- extra_arg$cur_task_idx
        lower <- private$bound_hist[[cur_task_idx]]$low
        upper <- private$bound_hist[[cur_task_idx]]$up
        leng <- length(lower)
        step <- (upper[leng] - lower[leng]) %/% 2
        return(step)
      }else if (est_type == "MOBS") {
        cur_task_idx <- extra_arg$cur_task_idx
        lower <- private$bound_hist[[cur_task_idx]][["low"]]
        upper <- private$bound_hist[[cur_task_idx]][["up"]]
        step <- (upper[1] - lower[1]) %/% 2
        return(step)
      }
    },

    output_exp_result = function() {
      total_leng <- private$n_est
      x_leng <- ifelse(private$n_est<=5,
                       2L,
                       private$n_est - 3L)
      result <- rep(0L, total_leng)
      log <- private$task_log
      x_name <- paste0("x", ((1:x_leng) + 1L) %/% 2, c("pos", "neg"))
      all_pts_name <-  c("L", x_name[1:2], "L2", "G2", x_name[3:x_leng])
      all_pts_name <- all_pts_name[1:total_leng]
      names(result) <- all_pts_name
      for (idx in 1:total_leng){
        .name <- all_pts_name[idx]
        result[idx] <- log[[.name]][length(log[[.name]])]
      }
      return(result)
    },
    show_bound = function() private$bound_hist,
    show_midrun_stim = function() private$UD_param$shift_hist,
    show_task_log = function() private$task_log,
    show_choice_history = function() private$choice_history
  )
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
