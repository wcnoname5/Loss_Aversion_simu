# source(here::here("functions/player_and_lotteries.R"))
source("E:/Proj/Loss_Aversion_simu/functions/player_and_lotteries.R")
# packages
library(R6)

# Game -------------------------------------------------------------------
Game <- R6Class(
  "Game",
  # private
  private = list(
    # Attributes
    init_values = # G, L_init, g, l, x1pos
      list("G"=2000L, "g"=300L, "l"=-300L),  
    n_est = 3L,  # no. of estimation. 19L = 5+ 7 + 7
    estimated_pts = 
      c("L", "x1pos", "x1neg", "L2", "G2"), # names of est quant
    task_log = NA,  # list of vectors, should be initialized
    slider = FALSE, # using slider after `n_trial` bisections
    bound_hist = list(), # For bisection
    est_type = "",
    # Attributes of SimpBisection
    first_step = 1000L,
    # Attributes of MOBS
    consis_check = FALSE,
    regression_fill = list(up = 0L, low = 0L),
    # Attributes of PEST
    last_choice = c(NA, NA, NA, NA), # last element is nearest choice
    PEST_step = 320L,
    max_step = 1280L,
    min_step = 5L,
    extra.step = FALSE,
    # Attribute of ASA
    m_shift = 0L,
    ASA_c = 2 * 320L,
    ASA_step = 320L,
    # Attribute of UD (up-down)
    is.mix = FALSE,
    late_phase_param = list(),
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
      
    # Simplified Bisection (that used in EDP)
    SimpBisection_update = function(
      choice, last_stim, cur_task_idx, cur_trial
    ) {
      # TODO: determine first_step
      if (cur_trial == 1) {
        private$first_step <- abs(last_stim)
      }
      # Choose target, Zn = +1 (positive step), else -1 (negative step)
      Zn <- ifelse(
        cur_task_idx == 1, # L
        ifelse(choice == "A", 1L, -1L),
        ifelse(choice == "B", 1L, -1L)
      )
      step <- (((1 / 2) ^ cur_trial) * private$first_step) * Zn
      step <- round_to_5(step) # forced to multiples of 5
      new_stim <- last_stim - step # align to ASA
      return(new_stim) # return bisection iteration value
    },

    # MOBS
    MOBS_update = function(choice, cur_task_idx, cur_trial) {
      low_stack <- private$bound_hist[[cur_task_idx]][["low"]]
      high_stack <- private$bound_hist[[cur_task_idx]][["up"]]
      if (cur_trial == 1) {
        # the bottom element when regression happens
        private$regression_fill$low <- low_stack[1]
        private$regression_fill$up <- high_stack[1]
      }
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
            length(idx) == 1 , # not chosen before
            FALSE,
            (private$choice_history[[cur_task_idx]][idx[1]] != choice)
          )
        # If Not Consistent: regression
        ## Update Stacks: remove top element, fill bottom element
        if (notConsis && (choice == change_option)){
          private$bound_hist[[cur_task_idx]][["low"]] <- 
            c(low_stack[-1], private$regression_fill$low)
        } else if (notConsis && (choice != change_option)){
          private$bound_hist[[cur_task_idx]][["up"]] <- 
            c(high_stack[-1], private$regression_fill$up)
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
        new_stim <- ifelse(
          !private$consis_check,
          (next_upper + next_lower) %/% 2, # Midpoint
          ifelse(
            choice == change_option,
            next_lower,
            next_upper
          )
        )
      }
      # update choice history
      private$last_choice <- choice
      return(new_stim) # return bisection iteration value
    },
    # PEST
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

    # ASA
    ASA_update = function(
      choice, last_stim, cur_task_idx, cur_trial
    ) {
      ## Current response, see Treutwein, (1995)
      Zn <- ifelse(cur_task_idx == 1, # L
                   as.numeric(choice == "A"),
                   as.numeric(choice == "B"))
      late_phase_trial <- ifelse(
        is.null(private$late_phase_param$late_phase_trial),
        1L,
        private$late_phase_param$late_phase_trial
      )
      if (cur_trial %in% c(1, late_phase_trial)) {
        private$m_shift <- 0L # times of reversal
      } else if (choice != private$last_choice) {
        private$m_shift <- private$m_shift + 1
      }
      private$last_choice <- c(choice)
      # Update next stimulus
      if (cur_trial %in% c(1, late_phase_trial)) {
        step <- (private$ASA_c / 1) * (Zn - .5)
      } else if (cur_trial %in% c(2, late_phase_trial + 1)) {
        step <- (private$ASA_c / 2) * (Zn - .5)
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
      last_choice_idx <- length(last_choice)
      # step size
      log <- private$task_log[[cur_task_idx]]
      delta <- private$late_phase_param$start_step
      latest_two_stimuli <- log[c(length(log), (length(log) - 1))]
      step_size <- diff(latest_two_stimuli) |> abs()
      # reversal count
      rev_count <- private$late_phase_param$UD_reversal
      stop_criterion <- private$late_phase_param$stop_rev_times
      # Reversal: update reversal times and reversal history
      if (choice != last_choice[last_choice_idx]) {
        private$late_phase_param$UD_reversal <-
          private$late_phase_param$UD_reversal + 1L
        private$late_phase_param$reversal_history <-
          c(private$late_phase_param$reversal_history, last_stim)
      }
      # Update new stimuli
      if (rev_count >= stop_criterion &&
            (step_size == delta)) {
        # Final estimate: midrun/last stimuli
        if (private$late_phase_param$UseMidrunEst) {
          new_stim <- mean(private$late_phase_param$reversal_history) |>
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
    
    # Initialization:
    init_log = function(est_type, init_values, random_init,
                        step_size, fixed_bnd_width = FALSE) {
      if (!is.list(init_values)) {
        stop("Input Error:'init_values' is not a list.")
      }
      private$step_size <- step_size
      if (est_type %in% c("Bisection", "Bisection-Slider")) {
        random_init <- FALSE
        names(private$bound_hist) <- private$estimated_pts
        #> Set bisection boundaries
        private$bound_hist$L[["up"]] <- 0L
        private$bound_hist$L[["low"]] <- ifelse(
          fixed_bnd_width,
          -5000,
          -2L * init_values[["G"]]
        )
        private$bound_hist$x1pos[["up"]] <- init_values[["G"]]
        private$bound_hist$x1pos[["low"]] <-  0L
        private$bound_hist$x1neg[["up"]] <-  0L
      } else if (est_type == "SimpBisection") {
        fixed_bnd_width <- FALSE
        private$first_step <- abs(init_values[["G"]])
      } else if (est_type == "PEST") {
        private$extra.step <- FALSE
        private$last_choice <-  c(NA, NA, NA, NA)
      } else if (est_type == "ASA") {
        private$last_choice <- NA
      } else if (est_type == "MOBS") {
        random_init <- FALSE
        names(private$bound_hist) <- private$estimated_pts
        #> Set bisection boundaries
        private$last_choice <- NA
        #> High and Low stack
        private$bound_hist$L[["up"]] <- rep(0L, 3)
        if (fixed_bnd_width) {
          private$bound_hist$L[["low"]] <- rep(-5000L, 3)
        } else {
          private$bound_hist$L[["low"]] <-
            rep(-2L * init_values[["G"]], 3)
        }
        private$bound_hist$x1pos[["up"]] <-
          rep(init_values[["G"]],3)
        private$bound_hist$x1pos[["low"]] <- rep(0L, 3)
        private$bound_hist$x1neg[["up"]] <- rep(0L, 3)
      }
      # Initialize Logs
      G <- init_values[["G"]]
      private$task_log <-
        rep(list(c(Inf)), length(private$estimated_pts))
      names(private$task_log) <- private$estimated_pts
      private$task_log[["L"]] <- 
        if (fixed_bnd_width) {
          -as.integer(5000 / 2)
        } else if (random_init) {
          -G + round_to_5(rnorm(1, sd = 100))
        } else {
          -G
        }
      private$task_log[["x1pos"]] <- 
        if (random_init) {
          as.integer((0 + (G)) / 2) + round_to_5(rnorm(1, sd = 100))
        } else {
          as.integer((0 + (G)) / 2)
        }
      
      private$choice_history <- 
        rep(list(c()), length(private$estimated_pts))
      names(private$choice_history) <- private$estimated_pts
    }
  ),
  # Public
  public = list(
    initialize =
      function(exp_params,
               est_type =
               c("Bisection", "Bisection-Slider", "SimpBisection", "MOBS", "PEST", "ASA"),
               n_est = 3L,
               ...) {
        extra.arg <- list(...)
        # Mixture Setting
        if (!is.null(extra.arg$late_phase_param)) {
          private$is.mix <- TRUE
          private$late_phase_param <- extra.arg$late_phase_param
          .names <- names(private$late_phase_param)
          # Check mix_param elements
          stopifnot(c("est_method", "start_crit", "start_step") %in% .names)
          if (private$late_phase_param$est_method == "UD") {
            # UD Setup: Check mix_param elements
            stopifnot(
              "Error: missing elements in mix_param (UD)" =
                c("stop_rev_times", "UseMidrunEst") %in% .names
            )
            private$late_phase_param["UD_reversal"] <- 0L # reversal count
            private$late_phase_param["reversal_history"] <- c()
          } else if (private$late_phase_param$est_method == "ASA") {
            # ASA setup
            private$ASA_c <- private$late_phase_param$start_step * 2L
            private$ASA_step <- private$late_phase_param$start_step
            private$last_choice <- NA
            # Trial number when late phase start
            private$late_phase_param["late_phase_trial"] <- -10L
          } else {
            stop("Error: Invalid 'est_method' value.")
          }
          private$late_phase_param["isLatePhase"] <- FALSE
        }
        
        # Check valid num. of estimates
        if ((!is.numeric(n_est)) | (((n_est - 3L) %% 2) != 0)) {
          stop("Invalid 'n_est' value.")
        }
        # Naming estimated points
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
        private$min_step <- exp_params$min_step # minimum_step
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
                         step_size, exp_params$fix_bnd_width)
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
      if (anyNA(.A) | anyNA(.B)) {
        print(c(cur_task_idx, cur_trial))
        print(lottery_values)
        print(self$show_task_log()[[cur_task_idx]])
        stop("NA Lotteries Created in `generate_lotteries()`")
      }
      new_lotteries <- Lotteries$new(lottery_values)
      return(new_lotteries)
    },

    # Update Initial stim in chained task
    prepare_chained_stim = function(cur_task_idx, random_init,
                                    fix_bnd_width = FALSE) {
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

      initialize_bounds <- function(est_type, lower_bnd, upper_bnd) {
        if (private$est_type %in%  c("Bisection", "Bisection-Slider")) {
          list(low = lower_bnd, up = upper_bnd)
        } else if (est_type == "MOBS") {
          list(low = rep(lower_bnd, 3), up = rep(upper_bnd, 3))
        }
      }

      update_next_task <-
        function(chained_val_name, left_pos, left_neg,
                 right_stim, IsNextPos = FALSE,
                 random_init = FALSE, fix_bnd_width = FALSE) {
          # Boundary Setting Rule for bisection-based methods
          if (fix_bnd_width) {
            if (IsNextPos) {
              # if next update estimate positive, lower bound is left_pos
              lower_bnd <- left_pos
              upper_bnd <- lower_bnd + 5000L
            } else {
              upper_bnd <- left_neg
              lower_bnd <- upper_bnd - 5000L
            }
            mid_point <- (lower_bnd + upper_bnd) %/% 2
          } else {
            # starting point s.t. expectation b/t are the same
            mid_point <- left_pos + left_neg - right_stim
            if (IsNextPos) {
              # if next update estimate positive, lower bound is left_pos
              lower_bnd <- left_pos
              upper_bnd <- lower_bnd + (2 * (mid_point - lower_bnd))
            } else {
              upper_bnd <- left_neg
              lower_bnd <- upper_bnd + (2 * (mid_point - upper_bnd))
            }
          }
          if (private$est_type %in% c("Bisection", "Bisection-Slider", "MOBS")) {
            bounds <- initialize_bounds(private$est_type, lower_bnd, upper_bnd)
            private$bound_hist[[chained_val_name]][["low"]] <- bounds$low
            private$bound_hist[[chained_val_name]][["up"]] <- bounds$up
          } else {
            invisible()
          }
          # Initialize starting point for non-bisection methods
          private$task_log[[chained_val_name]][1] <-
            mid_point + 
            ifelse(
              (random_init && (lower_bnd < upper_bnd)),
              rnorm(1, sd = 100) |> round_to_5(),
              0
            )
        }

      if (cur_task_idx == EstToNextIdx("L")) {
        # end of L, initialize x1neg[1]
        cur_vec <- self$show_task_log()[["L"]]
        L <- cur_vec[length(cur_vec)]
        #> Update for Bisection
        if (private$est_type %in% c("Bisection", "Bisection-Slider")) {
          private$bound_hist[["x1neg"]][["low"]] <- L
        } else if (private$est_type == "MOBS") {
          # low stack
          private$bound_hist[["x1neg"]][["low"]] <- rep(L, 3)
        }
        # To avoid extreme cases that L>0
        private$task_log[["x1neg"]][1] <-
          ((L + 0) %/% 2)
        # ifelse(
        #   ((random_init) && (L < 0)),
        #   rnorm(1, sd = 100) |> round_to_5(),
        #   0
        #   ) # not sure what this ifelse is working for

      } else if (cur_task_idx == EstToNextIdx("x1pos")) {
        # end of x1pos, initialize L2[1]
        cur_vec <- self$show_task_log()[["x1pos"]]
        x1pos <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        loss_1 <- private$init_values[["l"]]
        update_next_task("L2", 0, loss_1, x1pos, FALSE,
                         random_init, fix_bnd_width)

      } else if (cur_task_idx == EstToNextIdx("x1neg")) {
        # end of x1-, initialize G2[1]
        cur_vec <- self$show_task_log()[["x1neg"]]
        x1neg <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        gain_1 <- private$init_values[["g"]]
        update_next_task("G2", gain_1, 0, x1neg, TRUE,
                         random_init, fix_bnd_width)

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
        update_next_task(next_xi_name, cur_xi, loss_1, L2, TRUE,
                         random_init, fix_bnd_width)

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
        update_next_task(next_xi_name, gain_1, cur_xi, G2, FALSE,
                         random_init, fix_bnd_width)

      }
    },
    
    # Update next stim to task log from player's choice
    update_task_log = function(choice, cur_task_idx, cur_trial,
                               random_init = FALSE, ...) {
      extra.arg <- list(...)
      fix_bnd_width <- ifelse(is.null(extra.arg$fix_bnd_width),
                              FALSE,
                              extra.arg$fix_bnd_width)
      if (!is.na(extra.arg$phi)) {
        phi <- extra.arg$phi
      }
      cur_stim <- private$task_log[[cur_task_idx]][cur_trial]
      isLatePhase <- private$late_phase_param$isLatePhase
      isLatePhase <- ifelse(
        is.null(isLatePhase),
        FALSE,
        isLatePhase
      )
      est_type <- ifelse(
        isLatePhase,
        private$late_phase_param$est_method,
        private$est_type
      )
      if (est_type == "PEST") {
        value <- private$PEST_update(
          choice,
          last_stim = cur_stim,
          cur_task_idx,
          cur_trial
        )
      } else if (est_type == "ASA") {
        value <- private$ASA_update(
          choice,
          last_stim = cur_stim,
          cur_task_idx,
          cur_trial
        )
      } else if (est_type == "MOBS") {
        value <- private$MOBS_update(
          choice,
          cur_task_idx = cur_task_idx,
          cur_trial = cur_trial
        )
      } else if (est_type %in% c("Bisection", "Bisection-Slider")) {
        value <- private$bisection_update(
          choice,
          cur_task_idx = cur_task_idx,
          cur_trial = cur_trial,
          phi = phi
        )
      } else if (est_type == "UD") {
        value <- private$UD_update(
          choice,
          last_stim = cur_stim,
          cur_task_idx,
          cur_trial
        )
      } else if (est_type == "SimpBisection") {
        value <- private$SimpBisection_update(
          choice,
          last_stim = cur_stim,
          cur_task_idx = cur_task_idx,
          cur_trial = cur_trial
        )
      }
      cur_est_pt <- private$estimated_pts[cur_task_idx]

      # Write task Log
      step <- self$show_step(
        est_type,
        cur_task_idx = cur_task_idx,
        cur_trial = cur_trial
      )

      if (step >= private$min_step) {
        # Writing task_log
        private$task_log[[cur_est_pt]] <-
          c(private$task_log[[cur_est_pt]], value)
        # Writing choice_history
        private$choice_history[[cur_task_idx]] <-
          c(private$choice_history[[cur_task_idx]], choice)
      } else {
        # Final estimate is the last tested stimulus
        # Not writing to task_log in this case
        invisible()
      }

      # Beginning of a new task, update initial stim in next chained task
      if ((cur_task_idx <= private$n_est - 1) & (cur_trial == 1)) {
        # -1 because no need update next one in final
        self$prepare_chained_stim(cur_task_idx = cur_task_idx,
                                  random_init = random_init,
                                  fix_bnd_width = fix_bnd_width)
      }

      # Update late phase status:
      # if step <= start criterion, start late phase in next trial
      if (private$is.mix) {
        late_phase_flag <- step <= private$late_phase_param$start_crit
        if (isLatePhase != late_phase_flag) {
          # isLatePhase from FALSE to TRUE
          private$late_phase_param$late_phase_trial <- (cur_trial + 1)
        }
        private$late_phase_param$isLatePhase <- late_phase_flag
      }
    },

    reset_step = function() {
      private$PEST_step <- private$step_size
      private$ASA_c <- private$step_size * 2L
      private$ASA_step <- private$step_size
      private$m_shift <- 0L
      private$consis_check == FALSE

      # Reset Mixture States
      late_method <- private$late_phase_param$est_method
      if (private$is.mix) {
        private$late_phase_param$isLatePhase <- FALSE
        if (late_method == "UD") {
          private$late_phase_param$UD_reversal  <- 0L
          private$late_phase_param$reversal_history  <- c()
        } else if (late_method == "ASA") {
          private$ASA_c <- private$late_phase_param$start_step * 2L
          private$ASA_step <- private$late_phase_param$start_step
          private$last_choice <- NA
          private$late_phase_param$late_phase_trial <- -10L
        }
      }
    },

    show_step = function(est_type,
                         ...) {
      extra_arg <- list(...)

      is_mixture <- private$is.mix
      if (est_type == "PEST") {
        private$PEST_step
      } else if (est_type == "ASA") {
        return(private$ASA_step)
      } else if (est_type %in% c("Bisection", "Bisection-Slider")) {
        if (is.null(extra_arg$cur_task_idx)) {
          stop("w/o specifying `cur_task_idx` in Bisection/Slider!")
        }
        cur_task_idx <- extra_arg$cur_task_idx
        lower <- private$bound_hist[[cur_task_idx]]$low
        upper <- private$bound_hist[[cur_task_idx]]$up
        leng <- length(lower)
        step <- (upper[leng] - lower[leng]) %/% 2
        return(step)
      } else if (est_type == "MOBS") {
        cur_task_idx <- extra_arg$cur_task_idx
        lower <- private$bound_hist[[cur_task_idx]][["low"]]
        upper <- private$bound_hist[[cur_task_idx]][["up"]]
        step <- (upper[1] - lower[1]) %/% 2
        return(step)
      } else if (est_type == "UD") {
        UD_reversal <- private$late_phase_param$UD_reversal
        stop_rev_times <- private$late_phase_param$stop_rev_times
        # step == 0 if reversal times til limits
        reversal_limit_reached <- UD_reversal >= stop_rev_times
        step <- ifelse(
          reversal_limit_reached,
          0L,
          private$late_phase_param$start_step
        )
        return(step)
      } else if (est_type == "SimpBisection") {
        # TODO: finish computation of step calculation
        cur_trial <- extra_arg$cur_trial
        step <- ((1 / 2) ^ cur_trial) * abs(private$first_step)
        return(step)
      }
    },

    get_late_phase_status = function() {
      if (is.null(private$late_phase_param)) {
        return(FALSE)
      } else {
        return(private$late_phase_param$isLatePhase)
      }
    },

    output_exp_result = function() {
      total_leng <- private$n_est
      # num of xipos and xineg
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
        if (length(log[[.name]]) > 0) {
          result[idx] <- log[[.name]][length(log[[.name]])]
        } else {
          print(log)
          print(private$choice_history)
          print(private$bound_hist)
          stop("`output_exp_result()`: NA/NULL in experiment log")
        }
      }
      return(result)
    },
    show_bound = function() private$bound_hist,
    show_midrun_stim = function() private$late_phase_param$reversal_history,
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
