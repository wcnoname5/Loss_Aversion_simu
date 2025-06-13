# Refactored Game Architecture ----------------------------------------------
source(here::here("functions", "player_and_lotteries.R"))
library(R6)

# Base Strategy Class for Estimation Methods --------------------------------
elicit_methods <- R6Class(
  "ElicitMethods",
  public = list(
    name = NULL,
    initialize = function(name) {
      # initialize the strategy with a name
      self$name <- name
    },
    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      # context is a list containing bounds_manager and other parameters
      # should return the new stimulus value
      stop("update method must be implemented by subclass")
    },
    get_step = function(cur_task_idx, cur_trial, context) {
      # context is a list containing bounds_manager and other parameters
      stop("get_step method must be implemented by subclass")
    },
    reset = function() {
      # Default implementation - can be overridden
    },
    # For bisection-based strategies
    init_bounds = function(bounds_manager, init_values, estimated_pts, fix_bnd_width) {
      # Default implementation - can be overridden
    }
  )
)

# Bisection Strategy ---------------------------------------------------------
bisection_strategy <- R6Class(
  "BisectionStrategy",
  inherit = elicit_methods,
  private = list(
  ),
  public = list(
    initialize = function() {
      super$initialize("Bisection")
    },

    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      bounds <- context$bounds_manager$get_bounds(cur_task_idx)
      # current interval
      cur_boundaries <- c(bounds$low[length(bounds$low)], bounds$up[length(bounds$up)])

      if (is.numeric(choice)) {
        # Slider case
        bound_range <- abs(cur_boundaries[2] - cur_boundaries[1])
        new_bound <- cur_boundaries + c(-bound_range, bound_range)

        if (is.na(context$phi)) {
          new_stim <- choice |> as.integer()
        } else {
          new_stim <- rnorm(1, mean = choice, sd = 20L) |> as.integer()
          new_stim <- pmax(pmin(new_stim, new_bound[2]), new_bound[1])
        }
      } else {
        # Binary choice case
        change_option <- ifelse(cur_task_idx == 1, "A", "B")
        next_upper <- ifelse(choice == change_option, sum(cur_boundaries) %/% 2, cur_boundaries[2])
        next_lower <- ifelse(choice == change_option, cur_boundaries[1], sum(cur_boundaries) %/% 2)
        new_stim <- (next_upper + next_lower) %/% 2

        # Update bounds
        context$bounds_manager$update_bounds(cur_task_idx, next_lower, next_upper)
      }
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      bounds <- context$bounds_manager$get_bounds(cur_task_idx)
      lower <- bounds$low
      upper <- bounds$up
      step <- (upper[length(upper)] - lower[length(lower)]) %/% 2
      step
    },

    init_bounds = function(bounds_manager, init_values, estimated_pts, fix_bnd_width) {
      bounds_manager$init_bounds(init_values, estimated_pts, fix_bnd_width)
    }
  )
)
# MOBS Strategy -------------------------------------------------------------
MOBS_strategy <- R6Class(
  "MOBSStrategy",
  inherit = elicit_methods,
  private = list(
    # Only store current tops and their history
    low_stack_top = NA,
    high_stack_top = NA,
    low_stack_history = c(),
    high_stack_history = c(),
    # Track initial boundaries for regression
    initial_low_bound = NA,
    initial_high_bound = NA,
    # Choice tracking for consistency checking - stimulus -> choice mapping
    stimulus_choice_map = list(),
    last_choice = NA,
    consis_check = FALSE,
    # Store previous top values for regression
    regression_low_top = NA,
    regression_high_top = NA
  ),
  public = list(
    initialize = function() {
      super$initialize("MOBS")
    },

    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      change_option <- ifelse(cur_task_idx == 1, "A", "B")

      # Initialize on first trial
      if (cur_trial == 1) {
        bounds <- context$bounds_manager$get_bounds(cur_task_idx)
        private$initial_low_bound <- bounds$low[1]
        private$initial_high_bound <- bounds$up[1]
        private$low_stack_top <- private$initial_low_bound
        private$high_stack_top <- private$initial_high_bound
        private$regression_low_top <- private$initial_low_bound
        private$regression_high_top <- private$initial_high_bound
        private$stimulus_choice_map <- list()
      }

      # Calculate midpoint for current stimulus
      current_midpoint <- (private$low_stack_top + private$high_stack_top) %/% 2
      if (private$consis_check) {
        # Handle consistency check phase
        # Check if current choice is inconsistent with previous choice for same stimulus
        stim_key <- as.character(last_stim)
        previous_choice_for_stim <- private$stimulus_choice_map[[stim_key]]
        inconsistent <- !is.null(previous_choice_for_stim) && (previous_choice_for_stim != choice)

        if (inconsistent) {
          # Regression: restore previous top values
          if (choice == change_option) {
            private$low_stack_top <- private$regression_low_top
          } else {
            private$high_stack_top <- private$regression_high_top
          }
        }
        # Reset consistency check
        private$consis_check <- FALSE
        # Calculate next stimulus after potential regression
        new_stim <- (private$low_stack_top + private$high_stack_top) %/% 2
      } else {
        # Normal update phase
        # Store current tops for potential regression
        private$regression_low_top <- private$low_stack_top
        private$regression_high_top <- private$high_stack_top

        # Update stack tops based on choice
        if (choice == change_option) {
          # Target option chosen - update high stack top
          private$high_stack_top <- current_midpoint
          private$high_stack_history <- c(private$high_stack_history, current_midpoint)
        } else {
          # Non-target option chosen - update low stack top
          private$low_stack_top <- current_midpoint
          private$low_stack_history <- c(private$low_stack_history, current_midpoint)
        }

        # Check if next trial should check consistency
        # This happens when we have the same consecutive choices
        private$consis_check <- !is.na(private$last_choice) && (choice == private$last_choice)

        # Calculate next stimulus
        if (!private$consis_check) {
          # Normal case
          new_stim <- (private$low_stack_top + private$high_stack_top) %/% 2
        } else {
          # Consistency check - use stack top directly
          if (choice == change_option) {
            new_stim <- private$low_stack_top
          } else {
            new_stim <- private$high_stack_top
          }
        }
      }

      # Record choice for this stimulus (for future inconsistency checks)
      stim_key <- as.character(last_stim)
      private$stimulus_choice_map[[stim_key]] <- choice

      # Update choice history
      private$last_choice <- choice
      # Update bounds manager with current stack tops
      context$bounds_manager$update_bounds(cur_task_idx, private$low_stack_top, private$high_stack_top)
      # Return new stimulus
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      step <- abs(private$high_stack_top - private$low_stack_top) %/% 2
      step
    },

    # Get current stack tops
    get_current_bounds = function() {
      list(low = private$low_stack_top, high = private$high_stack_top)
    },

    # Get stack histories
    get_stack_histories = function() {
      list(
        low_history = private$low_stack_history,
        high_history = private$high_stack_history
      )
    },

    reset = function() {
      private$low_stack_top <- NA
      private$high_stack_top <- NA
      private$low_stack_history <- c()
      private$high_stack_history <- c()
      private$initial_low_bound <- NA
      private$initial_high_bound <- NA
      private$last_choice <- NA
      private$consis_check <- FALSE
      private$regression_low_top <- NA
      private$regression_high_top <- NA
    },

    init_bounds = function(bounds_manager, init_values, estimated_pts, fix_bnd_width) {
      # Initialize bounds in the bounds manager (for compatibility)
      bounds_manager$init_mobs_bounds(init_values, estimated_pts, fix_bnd_width)
    }
  )
)
# SimpBisection Strategy ---------------------------------------------------
SimpBisection_strategy <- R6Class(
  "SimpBisectionStrategy",
  inherit = elicit_methods,
  private = list(
    last_choice = NA,
    m_shift = 0L,
    ASA_c = 640L,
    cur_step = 320L
  ),
  public = list(
    initialize = function(step_size = 320L) {
      super$initialize("SimpBisection")
      private$ASA_c <- step_size * 2L
      private$cur_step <- step_size
    },

    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      Zn <- ifelse(cur_task_idx == 1, as.numeric(choice == "A"), as.numeric(choice == "B"))

      late_phase_trial <- context$late_phase_param$late_phase_trial %||% 1L

      if (cur_trial %in% c(1, late_phase_trial)) {
        private$m_shift <- 0L
      } else if (choice != private$last_choice) {
        private$m_shift <- private$m_shift + 1
      }
      private$last_choice <- choice

      if (cur_trial %in% c(1, late_phase_trial)) {
        step <- (private$ASA_c / 1) * (Zn - 0.5)
      } else if (cur_trial %in% c(2, late_phase_trial + 1)) {
        step <- (private$ASA_c / 2) * (Zn - 0.5)
      } else {
        step <- private$ASA_c / (2 + private$m_shift) * (Zn - 0.5)
      }

      step <- round(step)
      private$cur_step <- abs(step)
      new_stim <- last_stim - step
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      private$cur_step
    },

    reset = function() {
      private$cur_step <- 320L
      private$ASA_c <- 640L
      private$m_shift <- 0L
      private$last_choice <- NA
    }
  )
)
# PEST Strategy --------------------------------------------------------------
PEST_strategy <- R6Class( # nolint: object_name_linter.
  "PESTStrategy",
  inherit = elicit_methods,
  private = list(
    last_choice = c(NA, NA, NA, NA),
    cur_step = 320L,
    max_step = 1280L,
    extra_step = FALSE
  ),
  public = list(
    initialize = function(step_size = 320L) {
      super$initialize("PEST")
      private$cur_step <- step_size
    },

    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      if (cur_trial == 1) {
        private$last_choice <- c(NA, NA, NA, NA)
        private$extra_step <- FALSE
        private$last_choice <- c(private$last_choice, choice)[-1]
      } else {
        private$last_choice <- c(private$last_choice, choice)[-1]

        if (cur_trial < 4) {
          if (private$last_choice[3] != private$last_choice[4]) {
            private$cur_step <- private$cur_step %/% 2
          } else if ((cur_trial == 3) &&
                       all(private$last_choice[2:4] == private$last_choice[4])) {
            private$cur_step <- private$cur_step * 2
          }
        } else {
          if (private$last_choice[3] != private$last_choice[4]) {
            private$cur_step <- private$cur_step %/% 2
            if (all(private$last_choice[1:3] == "A") | all(private$last_choice[1:3] == "B")) {
              private$extra_step <- TRUE
            }
          } else if (all(private$last_choice[2:4] == "A") | all(private$last_choice[2:4] == "B")) {
            if (private$extra_step) {
              private$extra_step <- FALSE
            } else {
              private$cur_step <- private$cur_step * 2
            }
          }
        }
        private$cur_step <- pmin(private$cur_step, private$max_step)
      }

      direction <- ifelse(private$last_choice[4] == "A", 1, -1)
      if (cur_task_idx == 1) direction <- -direction

      new_stim <- last_stim + direction * round_to_5(private$cur_step)
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      private$cur_step
    },

    reset = function() {
      private$cur_step <- 320L
      private$last_choice <- c(NA, NA, NA, NA)
      private$extra_step <- FALSE
    }
  )
)
# ASA Strategy ---------------------------------------------------------------
ASA_strategy <- R6Class(
  "ASAStrategy",
  inherit = elicit_methods,
  private = list(
    last_choice = NA,
    m_shift = 0L,
    ASA_c = 640L,
    cur_step = 320L
  ),
  public = list(
    initialize = function(step_size = 320L) {
      super$initialize("ASA")
      private$ASA_c <- step_size * 2L
      private$cur_step <- step_size
    },

    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      Zn <- ifelse(cur_task_idx == 1, as.numeric(choice == "A"), as.numeric(choice == "B"))

      late_phase_trial <- context$late_phase_param$late_phase_trial %||% 1L

      if (cur_trial %in% c(1, late_phase_trial)) {
        private$m_shift <- 0L
      } else if (choice != private$last_choice) {
        private$m_shift <- private$m_shift + 1
      }
      private$last_choice <- choice

      if (cur_trial %in% c(1, late_phase_trial)) {
        step <- (private$ASA_c / 1) * (Zn - 0.5)
      } else if (cur_trial %in% c(2, late_phase_trial + 1)) {
        step <- (private$ASA_c / 2) * (Zn - 0.5)
      } else {
        step <- private$ASA_c / (2 + private$m_shift) * (Zn - 0.5)
      }

      step <- round(step)
      private$cur_step <- abs(step)
      new_stim <- last_stim - step
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      private$cur_step
    },

    reset = function() {
      private$cur_step <- 320L
      private$ASA_c <- 640L
      private$m_shift <- 0L
      private$last_choice <- NA
    }
  )
)
# Bounds Manager -------------------------------------------------------------
bounds_manager <- R6Class(
  "BoundsManager",
  private = list(
    bound_hist = list(),
    n_est = 0L
  ),
  public = list(
    initialize = function(n_est) {
      private$n_est <- n_est
      private$bound_hist <- lapply(seq_len(n_est), function(x) list(up = c(0L), low = c(0L)))
    },

    init_bounds = function(init_values, estimated_pts, fix_bnd_width = FALSE) {
      names(private$bound_hist) <- estimated_pts

      private$bound_hist$L[["up"]] <- 0L
      if (fix_bnd_width) {
        private$bound_hist$L[["low"]] <- -5000L
      } else {
        private$bound_hist$L[["low"]] <- -2L * init_values[["G"]]
      }
      private$bound_hist$x1pos[["up"]] <- init_values[["G"]]
      private$bound_hist$x1pos[["low"]] <- 0L
      private$bound_hist$x1neg[["up"]] <- 0L
    },

    # Get the bound history for a specific task index
    get_bounds = function(task_idx) {
      private$bound_hist[[task_idx]]
    },

    update_bounds = function(task_idx, lower = NULL, upper = NULL) {
      if (!is.null(lower)) {
        private$bound_hist[[task_idx]][["low"]] <- c(private$bound_hist[[task_idx]][["low"]], lower)
      }
      if (!is.null(upper)) {
        private$bound_hist[[task_idx]][["up"]] <- c(private$bound_hist[[task_idx]][["up"]], upper)
      }
    },

    show_bounds = function() {
      private$bound_hist
    }
  )
)

# Task Logger ----------------------------------------------------------------
# Log task values and choices
task_logger <- R6Class(
  "TaskLogger",
  private = list(
    task_log = list(),
    choice_history = list(),
    estimated_pts = character()
  ),
  public = list(
    initialize = function(estimated_pts, init_values, random_init = FALSE, fix_bnd_width = FALSE) {
      # Handling initialization schemes
      private$estimated_pts <- estimated_pts
      private$task_log <- rep(list(c(Inf)), length(estimated_pts))
      names(private$task_log) <- estimated_pts
      private$choice_history <- rep(list(c()), length(estimated_pts))
      names(private$choice_history) <- estimated_pts

      G <- init_values[["G"]]
      private$task_log[["L"]] <- if (fix_bnd_width) {
        -as.integer(5000 / 2)
      } else if (random_init) {
        -G + round_to_5(rnorm(1, sd = 100))
      } else {
        -G
      }

      private$task_log[["x1pos"]] <- if (random_init) {
        as.integer((0 + G) / 2) + round_to_5(rnorm(1, sd = 100))
      } else {
        as.integer((0 + G) / 2)
      }
    },

    # Prepare chained task initial stimulus
    prepare_chained_stim = function(cur_task_idx, random_init, fix_bnd_width = FALSE) {
      # Convert estimated point to next task idx
      EstToNextIdx <- function(pt) which(private$estimated_pts == pt) + 1
      if (private$n_est <= 5) {
        # For n_est <= 5, x_pos, x_neg are integer but not vector
        x_pos <- integer(0)
        x_neg <- integer(0)
      } else {
        # end of L2, G2 also need to update
        x_pos <- c(4, seq(6L, private$n_est, 2L)) + 1L
        x_neg <- c(5, seq(7L, private$n_est, 2L)) + 1L
      }

      # Create correct bound types based on estimation type
      create_bounds <- function(est_type, lower_bnd, upper_bnd) {
        list(low = lower_bnd, up = upper_bnd)
      }

      init_next_task <- function(
          chained_val_name, left_pos, left_neg, right_stim,
          IsNextPos = FALSE, random_init = FALSE, fix_bnd_width = FALSE) {
        # Boundary Setting Rule for bisection-based methods
        if (fix_bnd_width) {
          if (IsNextPos) {
            lower_bnd <- left_pos
            upper_bnd <- lower_bnd + 5000L
          } else {
            upper_bnd <- left_neg
            lower_bnd <- upper_bnd - 5000L
          }
          mid_point <- (lower_bnd + upper_bnd) %/% 2
        } else {
          mid_point <- left_pos + left_neg - right_stim
          if (IsNextPos) {
            lower_bnd <- left_pos
            upper_bnd <- lower_bnd + (2 * (mid_point - lower_bnd))
          } else {
            upper_bnd <- left_neg
            lower_bnd <- upper_bnd + (2 * (mid_point - upper_bnd))
          }
        }
        if (private$est_type %in% c("Bisection", "Bisection-Slider", "MOBS")) {
          bounds <- create_bounds(private$est_type, lower_bnd, upper_bnd)
          private$bounds_manager$update_bounds(
            which(private$estimated_pts == chained_val_name),
            bounds$low,
            bounds$up
          )
        }
        # Initialize starting point for non-bisection methods
        self$update_task_value(
          chained_val_name, index = 1,
          mid_point +
            ifelse(
              (random_init && (lower_bnd < upper_bnd)),
              round_to_5(rnorm(1, sd = 100)),
              0L
            )
        )
      }

      if (cur_task_idx == EstToNextIdx("L")) {
        # end of L, initialize x1neg[1]
        cur_vec <- self$show_task_log()[["L"]]
        L <- cur_vec[length(cur_vec)]
        # Prepare bounds for x1neg
        if (private$est_type %in% c("Bisection", "Bisection-Slider", "MOBS")) {
          bounds <- create_bounds(private$est_type, L, 0L)
          private$bounds_manager$update_bounds(
            EstToNextIdx("x1neg"),
            bounds$low,
            bounds$up
          )
        }

      } else if (cur_task_idx == EstToNextIdx("x1pos")) {
        # end of x1pos, initialize L2[1]
        cur_vec <- self$show_task_log()[["x1pos"]]
        x1pos <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        loss_1 <- private$init_values[["l"]]
        init_next_task("L2", 0, loss_1, x1pos, FALSE, random_init, fix_bnd_width)

      } else if (cur_task_idx == EstToNextIdx("x1neg")) {
        # end of x1-, initialize G2[1]
        cur_vec <- self$show_task_log()[["x1neg"]]
        x1neg <- cur_vec[length(cur_vec)]
        # starting point s.t. expectation b/t are the same
        gain_1 <- private$init_values[["g"]]
        init_next_task("G2", gain_1, 0, x1neg, TRUE, random_init, fix_bnd_width)

      } else if (cur_task_idx %in% x_pos) {
        # end of x_{i}^+, initialize x_{i+1}^+[1]
        ## current (i) of x_i^+:
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
        init_next_task(next_xi_name, cur_xi, loss_1, L2, TRUE,
                       random_init, fix_bnd_width)

      } else if (cur_task_idx %in% x_neg) {
        # end of x_{i}^-, initialize x_{i+1}^-[1]
        ## current (i) of x_i^-:
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
        init_next_task(next_xi_name, gain_1, cur_xi, G2, FALSE,
                       random_init, fix_bnd_width)
      }
    },

    get_log = function(est_point = NULL) {
      # Return the entire log or a specific point, e.g., L, x1pos
      if (is.null(est_point)) {
        return(private$task_log)
      } else {
        return(private$task_log[[est_point]])
      }
    },

    # Add a new value to the log for a specific estimated point
    add_entry = function(est_point, value) {
      private$task_log[[est_point]] <- c(private$task_log[[est_point]], value)
    },

    add_choice = function(task_idx, choice) {
      private$choice_history[[task_idx]] <- c(private$choice_history[[task_idx]], choice)
    },

    get_choice_history = function() {
      private$choice_history
    },

    # Update a specific task value at a given index
    # Useful for preparing chained task initial stimulus
    update_task_value = function(est_point, index, value) {
      private$task_log[[est_point]][index] <- value
    }
  )
)

# Main Game Class ----------------------------------------------
Game <- R6Class(
  "Game",
  private = list(
    # Core components
    strategy = NULL,
    bounds_manager = NULL,
    task_logger = NULL,

    # Configuration
    init_values = list("G" = 2000L, "g" = 300L, "l" = -300L),
    n_est = 3L,
    estimated_pts = c("L", "x1pos", "x1neg", "L2", "G2"),
    min_step = 5L,
    est_type = "",

    # Late phase / mixture settings
    is_mix = FALSE,
    late_phase_param = list(),

    # Helper method to create strategy
    create_strategy = function(est_type, step_size) {
      switch(est_type,
        "Bisection" = BisectionStrategy$new(),
        "Bisection-Slider" = BisectionStrategy$new(),
        "PEST" = PEST_strategy$new(step_size),
        "ASA" = ASA_strategy$new(step_size),
        # TODO: Add other strategies
        "MOBS" = MOBS_strategy$new(), # Example for MOBS
        # "SimpBisection" = SimpBisection_strategy$new(), # Example for SimpBisection
        stop("Unknown estimation type: ", est_type)
      )
    }
  ),

  public = list(
    initialize = function(
        exp_params,
        est_type = c("Bisection", "PEST", "ASA", "Bisection-Slider", "MOBS", "SimpBisection"),
        n_est = 3L, ...) {

      extra_arg <- list(...)
      # Setup mixture parameters if provided
      if (!is.null(extra_arg$late_phase_param)) {
        private$is_mix <- TRUE
        private$late_phase_param <- extra_arg$late_phase_param
      }

      # Validate and set `n_est` value
      if ((!is.numeric(n_est)) | (((n_est - 3L) %% 2) != 0)) {
        stop("Invalid 'n_est' value.")
      }
      private$n_est <- n_est

      # Set estimated points names
      estimated_pts <- c("L", "x1pos", "x1neg", "L2", "G2")
      if (n_est <= 5) {
        private$estimated_pts <- estimated_pts[1:n_est]
      } else {
        n_add <- (n_est - 5L) %/% 2L
        new_names <- paste0("x", rep(1:n_add, each = 2) + 1L, c("pos", "neg"))
        private$estimated_pts <- c(private$estimated_pts, new_names)
      }

      # Set configuration
      est_type <- match.arg(est_type)
      BisectionBased <- est_type %in% c("Bisection", "Bisection-Slider", "MOBS")
      private$est_type <- est_type
      private$init_values <- exp_params$init_values
      private$min_step <- exp_params$min_step

      # Initialize components
      step_size <- exp_params$step_size %||% 320L
      private$strategy <- private$create_strategy(est_type, step_size)
      private$bounds_manager <- if (BisectionBased) {
        bounds_manager$new(private$n_est)
      } else {
        NULL
      }

      # Initialize bounds if needed
      if (BisectionBased) {
        private$strategy$init_bounds(
          private$bounds_manager,
          private$init_values,
          private$estimated_pts,
          exp_params$fix_bnd_width
        )
      }

      # Initialize task logger
      random_init <- exp_params$random_init %||% FALSE
      fix_bnd_width <- exp_params$fix_bnd_width %||% FALSE
      private$task_logger <- task_logger$new(
        private$estimated_pts,
        private$init_values,
        random_init,
        fix_bnd_width
      )
    },

    # Method: Generate lotteries
    generate_lotteries = function(cur_task_idx, cur_trial) {
      G <- private$init_values[["G"]]
      loss_1 <- private$init_values[["l"]]
      gain_1 <- private$init_values[["g"]]
      cur_est_pt <- private$estimated_pts[cur_task_idx]

      # Set the indices(i>1) of x_i+, x_i-
      if (private$n_est <= 5) {
        x_pos_set <- 0L
        x_neg_set <- 0L
      } else {
        x_pos_set <- seq(6L, private$n_est, 2L)
        x_neg_set <- seq(7L, private$n_est, 2L)
      }

      if (cur_est_pt == "L") {
        # (G, .5; L) vs 0
        .A <- c(G, private$task_logger$get_log("L")[cur_trial])
        .B <- rep(0L, 2)
      } else if (cur_est_pt == "x1pos") {
        # (G, .5; 0) vs x1+
        .A <- c(G, 0)
        .B <- rep(private$task_logger$get_log("x1pos")[cur_trial], 2)
      } else if (cur_est_pt == "x1neg") {
        # (0, .5; L) vs x1-
        cur_vec <- private$task_logger$get_log("L")
        L <- cur_vec[length(cur_vec)]
        .A <- c(0, L)
        .B <- rep(private$task_logger$get_log("x1neg")[cur_trial], 2)
      } else if (cur_est_pt == "L2") { 
        # (0, .5; l_1) vs (x1+, .5; "L2")
        cur_vec <- self$show_task_log()[["x1pos"]]
        x1pos <- cur_vec[length(cur_vec)]
        .A <- c(0L, loss_1)
        .B <- c(x1pos, self$show_task_log()[["L2"]][cur_trial])
      } else if (cur_est_pt == "G2") {
        # (g_1, .5; 0) vs ("G2", .5;  x1-)
        cur_vec <- self$show_task_log()[["x1neg"]]
        x1neg <- cur_vec[length(cur_vec)]
        .A <- c(gain_1, 0L)
        .B <- c(self$show_task_log()[["G2"]][cur_trial], x1neg)
      } else if (cur_task_idx %in% x_pos_set) {
        # current x_i+:
        # get the index of x_i
        x_idx <- which(x_pos_set == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "pos")
        last_xi_name <- paste0("x", (x_idx - 1), "pos")
        # last x_i :x_{i-1}+:
        cur_vec <- self$show_task_log()[[last_xi_name]]
        last_xipos <- cur_vec[length(cur_vec)]
        # L_2
        L2_vec <- self$show_task_log()[["L2"]]
        L2 <- L2_vec[length(L2_vec)]
        # x_i^+: (x_{i-1}+, .5; l_1) vs ("x_i+", .5; L_2)
        .A <- c(last_xipos, loss_1)
        .B <- c(self$show_task_log()[[cur_xi_name]][cur_trial], L2)
      } else if (cur_task_idx %in% x_neg_set) {
        # current x_i-:
        x_idx <- which(x_neg_set == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "neg")
        last_xi_name <- paste0("x", (x_idx - 1), "neg")
        # last x_i :x_{i-1}-:
        cur_vec <- self$show_task_log()[[last_xi_name]]
        last_xineg <- cur_vec[length(cur_vec)]
        # L_2
        G2_vec <- self$show_task_log()[["G2"]]
        G2 <- G2_vec[length(G2_vec)]
        # x_i^-: (g_1, .5; x_{i-1}-) vs (G_2, .5; "x_i-")
        .A <- c(gain_1, last_xineg)
        .B <- c(G2, self$show_task_log()[[cur_xi_name]][cur_trial])
      }

      lottery_values <- list(".A" = .A, ".B" = .B)
      if (anyNA(.A) | anyNA(.B) || !is.numeric(.A) || !is.numeric(.B)) {
        print(c(cur_task_idx, cur_trial))
        print(lottery_values)
        print(self$show_task_log()[[cur_task_idx]])
        stop("NA or non-numeric Lotteries Created in `generate_lotteries()`")
      }
      new_lotteries <- Lotteries$new(lottery_values)
      new_lotteries
    },

    # Method: Main update
    update_task_log = function(choice, cur_task_idx, cur_trial, ...) {
      extra_arg <- list(...)

      # Get current stimulus
      cur_est_pt <- private$estimated_pts[cur_task_idx]
      cur_stim <- private$task_logger$get_log(cur_est_pt)[cur_trial]

      # Create context for strategy
      context <- list(
        bounds_manager = private$bounds_manager,
        late_phase_param = private$late_phase_param,
        phi = extra_arg$phi
      )

      # Get appropriate strategy (handle mixture)
      current_strategy <- if (private$is_mix && private$late_phase_param$isLatePhase) {
        private$create_strategy(private$late_phase_param$est_method,
                                private$late_phase_param$start_step)
      } else {
        private$strategy
      }

      # Update using strategy
      new_value <- current_strategy$update(choice, cur_stim, cur_task_idx, cur_trial, context)

      # Get step size to determine if we should continue
      step <- current_strategy$get_step(cur_task_idx, cur_trial, context)

      # Add to log if step is large enough
      if (step >= private$min_step) {
        private$task_logger$add_entry(cur_est_pt, new_value)
        private$task_logger$add_choice(cur_task_idx, choice)
      }

      # Handle chained task initialization and late phase updates
      if ((cur_task_idx <= private$n_est - 1) & (cur_trial == 1)) {
        self$prepare_chained_stim(cur_task_idx, extra_arg$random_init %||% FALSE,
                                  extra_arg$fix_bnd_width %||% FALSE)
      }

      # Update late phase status if mixture
      if (private$is_mix) {
        late_phase_flag <- step <= private$late_phase_param$start_crit
        if (!private$late_phase_param$isLatePhase && late_phase_flag) {
          # Update trial idx which starts late phase
          private$late_phase_param$late_phase_trial <- (cur_trial + 1)
        }
        private$late_phase_param$isLatePhase <- late_phase_flag
      }
    },

    # Other public methods
    show_task_log = function() private$task_logger$get_log(),
    show_choice_history = function() private$task_logger$get_choice_history(),
    show_bound = function() {
      if (private$est_type %in% c("Bisection", "Bisection-Slider", "MOBS")) {
        private$bounds_manager$show_bounds()
      } else {
        list() # For other strategies, return empty
      }
    },

    reset_step = function() private$strategy$reset(),
    # Prepare chained task initial stimulus
    prepare_chained_stim = function(cur_task_idx, random_init, fix_bnd_width = FALSE) {
      # Delegate to task_logger, passing all needed context
      private$task_logger$prepare_chained_stim(
        cur_task_idx = cur_task_idx,
        random_init = random_init,
        fix_bnd_width = fix_bnd_width
      )
    },

    output_exp_result = function() {
      # (Keep original logic but use task_logger methods)
      total_leng <- private$n_est
      result <- rep(0L, total_leng)
      log <- private$task_logger$get_log()

      # Build names
      x_leng <- ifelse(private$n_est <= 5, 2L, private$n_est - 3L)
      x_name <- paste0("x", ((1:x_leng) + 1L) %/% 2, c("pos", "neg"))
      all_pts_name <- c("L", x_name[1:2], "L2", "G2", x_name[3:x_leng])
      all_pts_name <- all_pts_name[1:total_leng]
      names(result) <- all_pts_name

      for (idx in 1:total_leng) {
        name <- all_pts_name[idx]
        if (length(log[[name]]) > 0) {
          result[idx] <- log[[name]][length(log[[name]])]
        } else {
          # Error handling
          print("Experiment log:")
          print(log)
          print("Choice history:")
          print(private$task_logger$get_choice_history())
          stop("`output_exp_result()`: NA/NULL in experiment log")
        }
      }
      result
    }
  )
)


# Null-coalescing operator helper
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
