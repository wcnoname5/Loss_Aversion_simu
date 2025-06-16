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
    },    # For bisection-based strategies
    init_bounds = function(
      bounds_manager,
      init_values,
      targets,
      bound_scheme = NULL, ...
    ) {
      # Default implementation - can be overridden
    }
  )
)

# Bisection Strategy ---------------------------------------------------------
Bisection_strategy <- R6Class(
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
        target_option <- ifelse(cur_task_idx == 1, "A", "B")
        next_upper <- ifelse(choice == target_option, sum(cur_boundaries) %/% 2, cur_boundaries[2])
        next_lower <- ifelse(choice == target_option, cur_boundaries[1], sum(cur_boundaries) %/% 2)
        new_stim <- (next_upper + next_lower) %/% 2

        # Update bounds
        context$bounds_manager$update_bounds(cur_task_idx, next_lower, next_upper)
      }
      new_stim
    },
    get_step = function(cur_task_idx, cur_trial, context) {
      bounds <- context$bounds_manager$get_bounds(cur_task_idx)
      lower <- tail(bounds$low, 1)
      upper <- tail(bounds$up, 1)
      step <- abs(upper - lower) %/% 2  # Use abs() to ensure non-negative step
      # cat("Task:", cur_task_idx, " Trial:", cur_trial, step, "\n")
      step
    },

    init_bounds = function(
      bounds_manager,
      init_values,
      targets,
      bound_scheme = NULL, ...
    ) {
      extra.arg <- list(...)
      bounds_manager$init_bounds(init_values, targets, bound_scheme)
    }
  )
)
# Adaptive Strategy to find boundary---------------------------------------------------------
# It just the Up-down method to find the boundary
Adaptive_bound_strategy <- R6Class(
  "AdaptiveStrategy",
  inherit = elicit_methods,
  private = list(
    step = 500L,
    EndSearching = FALSE
  ),
  public = list(
    initialize = function(step = 500L) {
      super$initialize("AdaptiveBounds")
      private$step <- step
    },
    update = function(choice, last_stim, cur_task_idx, cur_trial, context) {
      step <- private$step
      CheckGainBoundary <- context$CheckGainBoundary # logical
      bounds <- context$bounds_manager$get_bounds(cur_task_idx)      # cat("DEBUG Adaptive_bound_strategy update:\n")
      # cat("  choice:", choice, "last_stim:", last_stim, "\n")
      # cat("  cur_task_idx:", cur_task_idx, "cur_trial:", cur_trial, "\n")
      # cat("  CheckGainBoundary:", CheckGainBoundary, "\n")
      # cat("  bounds: low =", tail(bounds$low, 1), "up =", tail(bounds$up, 1), "\n")# Binary choice case
      target_option <- ifelse(cur_task_idx == 1, "A", "B")
      if (CheckGainBoundary) {
        # If we are checking upper-bound of a gain
        next_lower <- tail(bounds$low, 1)
        if (choice == target_option) {
          # If the choice is the target_option, we found a gain upper-bound  (i.e., last_stim)
          next_upper <- last_stim
          private$EndSearching <- TRUE
          # next_stim is the midpoint of next_upper and next_lower
          new_stim <- (next_upper + next_lower) %/% 2
          # Update bounds only when boundary is found
          context$bounds_manager$update_bounds(cur_task_idx, next_lower, next_upper)
        } else {
          # Keep searching - don't update bounds
          new_stim <- last_stim + step
        }
      } else {
        next_upper <- tail(bounds$up, 1)
        if (choice != target_option) {
          # If the choice isn't the target_option, we found a loss lower-bound (i.e., last_stim)
          next_lower <- last_stim
          private$EndSearching <- TRUE
          # next_stim is the midpoint of next_upper and next_lower
          new_stim <- (next_upper + next_lower) %/% 2
          # Update bounds only when boundary is found
          context$bounds_manager$update_bounds(cur_task_idx, next_lower, next_upper)
        } else {
          # Keep searching - don't update bounds
          new_stim <- last_stim - step
        }
      }

      # cat("  new_stim:", new_stim, "EndSearching:", private$EndSearching, "\n")

      # Remove the unconditional bounds update that was causing the problem
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      Inf
    },
    is_searching_complete = function() {
      private$EndSearching
    },
    reset = function() {
      private$EndSearching <- FALSE
    },
    init_bounds = function(
      bounds_manager,
      init_values,
      targets,
      bound_scheme = NULL, ...
    ) {
      extra.arg <- list(...)
      bounds_manager$init_bounds(init_values, targets, bound_scheme)
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
      target_option <- ifelse(cur_task_idx == 1, "A", "B")
      # cat("DEBUG MOBS_strategy update:\n")
      # cat("  choice:", choice, "last_stim:", last_stim, "\n")
      # cat("  cur_task_idx:", cur_task_idx, "cur_trial:", cur_trial, "\n")
      # Initialize on first trial
      if (cur_trial == 1) {
        bounds <- context$bounds_manager$get_bounds(cur_task_idx)
        cat("  MOBS first trial bounds: low =", bounds$low, "up =", bounds$up, "\n")
        private$low_stack_top <- bounds$low[1]
        private$high_stack_top <- bounds$up[1]
        private$regression_low_top <- bounds$low[1]
        private$regression_high_top <- bounds$up[1]
        private$stimulus_choice_map <- list()
      } else {
        # Not first trial for MOBS, but might be first time MOBS is called
        if (is.na(private$low_stack_top) || is.na(private$high_stack_top)) {
          # MOBS is taking over from another strategy, need to initialize from current bounds
          bounds <- context$bounds_manager$get_bounds(cur_task_idx)
          # cat("  MOBS taking over, bounds: low =", bounds$low, "up =", bounds$up, "\n")
          private$low_stack_top <- tail(bounds$low, 1)  # Use latest bounds
          private$high_stack_top <- tail(bounds$up, 1)  # Use latest bounds
          private$regression_low_top <- tail(bounds$low, 1)
          private$regression_high_top <- tail(bounds$up, 1)
          private$stimulus_choice_map <- list()
        }
      }
      cat("  MOBS stack tops: low =", private$low_stack_top, "high =", private$high_stack_top, "\n")

      # Calculate midpoint for current stimulus
      current_midpoint <- (private$low_stack_top + private$high_stack_top) %/% 2
      # cat("  current_midpoint:", current_midpoint, "\n")
      if (private$consis_check) {
        # Handle consistency check phase
        # Check if current choice is inconsistent with previous choice for same stimulus
        stim_key <- as.character(last_stim)
        previous_choice_for_stim <- private$stimulus_choice_map[[stim_key]]
        inconsistent <- !is.null(previous_choice_for_stim) && (previous_choice_for_stim != choice)

        if (inconsistent) {
          # Regression: restore previous top values
          if (choice == target_option) {
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
        if (choice == target_option) {
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
          if (choice == target_option) {
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
      context$bounds_manager$update_bounds(
        cur_task_idx,
        private$low_stack_top, private$high_stack_top
      )
      # Return new stimulus
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      if (any(is.na(private$low_stack_top), is.na(private$high_stack_top))) {
        # If stack tops are not initialized, get bounds from bounds manager
        bounds <- context$bounds_manager$get_bounds(cur_task_idx)
        step <- abs(bounds$up[1] - bounds$low[1]) %/% 2
      } else {
        step <- abs(private$high_stack_top - private$low_stack_top) %/% 2
      }
      cat("  MOBS_strategy get_step() =", step, "\n")
      if (is.na(step) || step <= 0) {
        # If step is NA or negative, return Inf to avoid errors
        if (any(is.na(private$low_stack_top), is.na(private$high_stack_top))) {
          cat("Bounds, up:", bounds$up[1], "low:", bounds$low[1], "\n")
        } else {
          cat("Bounds, up:", private$high_stack_top, "low:", private$low_stack_top, "\n")
        }
      }
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
      private$last_choice <- NA
      private$consis_check <- FALSE
      private$regression_low_top <- NA
      private$regression_high_top <- NA
      private$stimulus_choice_map <- list()
    },
    init_bounds = function(
      bounds_manager,
      init_values,
      targets,
      bound_scheme = NULL, ...
    ) {
      # Initialize bounds in the bounds manager (for compatibility)
      bounds_manager$init_bounds(init_values, targets, bound_scheme)
    }
  )
)
# SimpBisection Strategy ---------------------------------------------------
# TODO: Implement SimpBisection strategy
SimpBisection_strategy <- R6Class(
  "SimpBisectionStrategy",
  inherit = elicit_methods,
  private = list(first_step = NA),
  public = list(
    initialize = function() {
      super$initialize("SimpBisection")
    },

    update = function(choice,
                      last_stim,
                      cur_task_idx,
                      cur_trial,
                      context) {
      # Determine first_step on first trial
      if (cur_trial == 1) {
        private$first_step <- context$task_logger$get_log(cur_task_idx)[1] |>
          abs()
      }

      # Choose target, Zn = +1 (positive step), else -1 (negative step)
      Zn <- ifelse(cur_task_idx == 1,
                   # L
                   ifelse(choice == "A", 1L, -1L),
                   ifelse(choice == "B", 1L, -1L))

      step <- (((1 / 2) ^ cur_trial) * private$first_step) * Zn
      step <- round(step) # round to nearest integer
      new_stim <- last_stim - step # align to ASA
      new_stim # return bisection iteration value
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      if (cur_trial == 0) {
        # Return Inf to avoid error in early stop cases
        Inf
      } else if (is.na(private$first_step)) {
        stop("First step is not initialized. Call update with choice first.")
      } else {
        abs(((1 / 2) ^ cur_trial) * private$first_step) |>
          round()
      }
    },

    reset = function() {
      private$first_step <- NA
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

      new_stim <- last_stim + direction * round(private$cur_step)
      new_stim
    },

    get_step = function(cur_task_idx, cur_trial, context) {
      round(private$cur_step)
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
      } else if (!is.na(private$last_choice) && choice != private$last_choice) {
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
      private$bound_hist <-
        lapply(seq_len(n_est),
          function(x) list(up = c(numeric()), low = c(numeric()))
        )
    },    init_bounds = function(
      init_values,
      targets,
      bound_scheme = NULL
    ) {
      names(private$bound_hist) <- targets

      private$bound_hist$L[["up"]] <- 0L
      if (bound_scheme == "fixed_bnd") {
        private$bound_hist$L[["low"]] <- -5000L
      } else if (bound_scheme == "equal_expectation") {
        private$bound_hist$L[["low"]] <- -2L * init_values[["G"]]
      } else if (bound_scheme == "adaptive") {
        # TODO: implement adaptive bounds
        private$bound_hist$L[["low"]] <- -Inf
      } else {
        stop("Unknown 'bound_scheme': ", bound_scheme)
      }
      private$bound_hist$x1pos[["up"]] <- init_values[["G"]]
      private$bound_hist$x1pos[["low"]] <- 0L
      private$bound_hist$x1neg[["up"]] <- 0L
    },    # Get the bound history for a specific task index
    get_bounds = function(task_idx) {
      # cat("DEBUG BoundsManager get_bounds for task", task_idx, ":\n")
      # cat("  low:", private$bound_hist[[task_idx]][["low"]], "\n")
      # cat("  up:", private$bound_hist[[task_idx]][["up"]], "\n")
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
    init_values = list("G" = integer(), "g" = integer(), "l" = integer()),
    task_log = list(),
    choice_history = list(),
    targets = character()
  ),
  public = list(
    initialize = function(
      targets,
      init_values,
      random_init = FALSE,
      bound_scheme = NULL
    ) {
      # Handling initialization schemes
      private$targets <- targets
      private$task_log <- rep(list(c(Inf)), length(targets))
      names(private$task_log) <- targets
      private$choice_history <- rep(list(c()), length(targets))
      names(private$choice_history) <- targets

      # Validate initialization values
      stopifnot(
        "Initialization values must include 'G', 'g', and 'l'." =
          all(c("G", "g", "l") %in% names(init_values))
      )
      private$init_values <- init_values
      G <- init_values[["G"]]

      # Initialize the task log for "L" based on the bound scheme
      if (is.null(bound_scheme)) {
        # For non-bisection methods, set L to -G
        private$task_log[["L"]] <- -G
      } else {
        private$task_log[["L"]] <- switch(bound_scheme,
          "fixed_bnd" = -as.integer(5000 / 2),
          "equal_expectation" = -G,
          "adaptive" = -G, # Start with equal_expectation value
          stop("Unknown 'bound_scheme': ", bound_scheme)
        )
      }

      # Apply random initialization if enabled
      # x1pos need not to consider bound scheme since its boundaries are well-defined
      if (random_init) {
        private$task_log[["L"]] <- private$task_log[["L"]] + round(rnorm(1, sd = 100))
        private$task_log[["x1pos"]] <- (0 + G) %/% 2 + round(rnorm(1, sd = 100))
      } else {
        private$task_log[["x1pos"]] <- (0 + G) %/% 2
      }
    },

    # Prepare chained task initial stimulus
    prepare_chained_stim = function(
      elicit_method,
      cur_task_idx,
      bounds_manager,
      random_init = FALSE,
      bound_scheme = NULL
    ) {
      # Helper function convert estimated point to next task idx
      GetTargetIndex <- function(target_name) which(private$targets == target_name) + 1

      # Helper function to update bounds using bounds manager
      SetTaskBounds <- function(target_name, lower_bnd, upper_bnd) {
        if (target_name %in% private$targets) {
          task_index <- which(private$targets == target_name)
          bounds_manager$update_bounds(task_index, lower_bnd, upper_bnd)
        } else {
          stop(paste("Target name not found in private$targets:", target_name))
        }
      }
      bound_scheme <- bound_scheme %||% "equal_expectation"

      # Helper function initializes the next chained target value in the task log.
      # Also sets the bounds for bisection-based methods.
      # outcomes is a list with left_pos, left_neg, right_stim
      InitializeNextTask <- function(elicit_method,
        chained_target,
        outcomes,
        ChainedTargetGain = FALSE,
        random_init = FALSE,
        bound_scheme = NULL
      ) {
        # Check if chained_target is valid
        stopifnot(
          "Chained target must be one of the estimated points." =
            chained_target %in% private$targets,
          "Outcomes must be a list with left_pos, left_neg, right_stim." =
            all(c("left_pos", "left_neg", "right_stim") %in% names(outcomes))
        )
        # Check if outcomes are provided and be numeric
        left_pos <- outcomes$left_pos
        left_neg <- outcomes$left_neg
        right_stim <- outcomes$right_stim # fixed right outcome
        # cat("DEBUG InitializeNextTask:\n")
        # cat("  chained_target:", chained_target, "\n")
        # cat("  left_pos:", left_pos, "left_neg:", left_neg, "right_stim:", right_stim, "\n")
        # cat("  bound_scheme:", bound_scheme, "ChainedTargetGain:", ChainedTargetGain, "\n")
        if (!is.numeric(left_pos) || !is.numeric(left_neg) || !is.numeric(right_stim)) {
          stop("All outcomes must be numeric.\n Outcomes: ",
               paste(names(outcomes), outcomes, collapse = ", "))
        }

        # Set boundaries for bisection-based methods
        if (bound_scheme == "fixed_bnd") {
          # Case: fixed boundary width
          if (ChainedTargetGain) {
            lower_bnd <- left_pos
            upper_bnd <- lower_bnd + 5000L
          } else {
            upper_bnd <- left_neg
            lower_bnd <- upper_bnd - 5000L
          }
          start_point <- (lower_bnd + upper_bnd) %/% 2
        } else if (bound_scheme == "equal_expectation") {
          # Case: equal_expectation
          start_point <- left_pos + left_neg - right_stim
          if (ChainedTargetGain) {
            lower_bnd <- left_pos
            upper_bnd <- lower_bnd + (2 * (start_point - lower_bnd))
          } else {
            upper_bnd <- left_neg
            lower_bnd <- upper_bnd + (2 * (start_point - upper_bnd))
          }
        } else if (bound_scheme == "adaptive") {
          # Case: adaptive bounds: first set boundaries to Inf, -Inf
          start_point <- left_pos + left_neg - right_stim
          if (ChainedTargetGain) {
            lower_bnd <- left_pos
            upper_bnd <- Inf
          } else {
            upper_bnd <- left_neg
            lower_bnd <- -Inf
          }
        } else {
          stop("Unknown 'bound_scheme': ", bound_scheme)
        }

        # cat("  start_point:", start_point, "lower_bnd:", lower_bnd, "upper_bnd:", upper_bnd, "\n")

        # Initialize bounds using bounds manager
        if (elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")) {
          SetTaskBounds(chained_target, lower_bnd, upper_bnd)
        }
        # Random offset for random initial scheme
        random_offset <- if (random_init && (lower_bnd < upper_bnd)) {
          round(rnorm(1, sd = 100))
        } else {
          0L
        }
        # Set starting point into task log
        initial_value <- start_point + random_offset
        # cat("  Setting", chained_target, "[1] =", initial_value, "\n")
        self$update_task_value(
          target = chained_target,
          index = 1,
          value = initial_value
        )
      }

      # Get task index that needed to be initialized next
      if (length(private$targets) <= 5) {
        # For n_est <= 5, x_pos_idx, x_neg_idx are empty
        x_pos_idx <- integer(0)
        x_neg_idx <- integer(0)
      } else {
        # end of L2, G2 also need to update
        x_pos_idx <-
          which(grepl("x[2-9]pos|L2", private$targets)) + 1L
        x_neg_idx <-
          which(grepl("x[2-9]neg|G2", private$targets)) + 1L
      }
      # Main execution: control flow for initializing next tasks
      if (cur_task_idx == GetTargetIndex("L")) {
        # end of L, initialize x1neg[1]
        cur_vec <- self$get_log()[["L"]]
        L <- tail(cur_vec, 1) # last value
        # cat("DEBUG prepare_chained_stim: Initializing x1neg after L task\n")
        # cat("  L value:", L, "bound_scheme:", bound_scheme, "\n")

        # Prepare bounds for x1neg, which is perfectly bounded by L and 0
        if (elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")) {
          SetTaskBounds("x1neg", L, 0)
        }
        # Define start_point for x1neg initialization
        start_point <- (L + 0) %/% 2
        # Random offset for random initial scheme
        random_offset <- ifelse(random_init,
                                round_to_5(rnorm(1, sd = 100)),
                                0L)
        # Set starting point into task log
        initial_value <- start_point + random_offset
        # cat("  Setting x1neg[1] =", initial_value, "\n")
        self$update_task_value(target = "x1neg",
                               index = 1,
                               value = initial_value)
      } else if (cur_task_idx == GetTargetIndex("x1pos")) {
        # end of x1pos, initialize L2[1]
        cur_vec <- self$get_log()[["x1pos"]]
        x1pos <- tail(cur_vec, 1)
        loss_1 <- private$init_values[["l"]]

        # cat("DEBUG prepare_chained_stim: Initializing L2 after x1pos task\n")
        # cat("  x1pos value:",
        #     x1pos,
        #     "loss_1:",
        #     loss_1,
        #     "bound_scheme:",
        #     bound_scheme,
        #     "\n")

        outcomes <- list(left_pos = 0L,
                         left_neg = loss_1,
                         right_stim = x1pos)
        InitializeNextTask(elicit_method,
                           "L2",
                           outcomes,
                           ChainedTargetGain = FALSE,
                           random_init,
                           bound_scheme)

      } else if (cur_task_idx == GetTargetIndex("x1neg")) {
        # end of x1-, initialize G2[1]
        cur_vec <- self$get_log()[["x1neg"]]
        x1neg <- tail(cur_vec, 1)
        # starting point s.t. expectation b/t are the same
        gain_1 <- private$init_values[["g"]]
        outcomes <- list(
          left_pos = gain_1,
          left_neg = 0L,
          right_stim = x1neg
        )
        InitializeNextTask(elicit_method,
          "G2",
          outcomes,
          ChainedTargetGain = TRUE,
          random_init,
          bound_scheme
        )

      } else if (cur_task_idx %in% x_pos_idx) {
        # end of x_{i}^+, initialize x_{i+1}^+[1]
        ## current (i) of x_i^+:
        x_idx <- which(x_pos_idx == cur_task_idx)
        cur_xi_name <- paste0("x", x_idx, "pos")
        next_xi_name <- paste0("x", (x_idx + 1), "pos")

        # Get current xi value and required parameters
        cur_xi <- tail(self$get_log()[[cur_xi_name]], 1)
        loss_1 <- private$init_values[["l"]]
        L2 <- tail(self$get_log()[["L2"]], 1)

        outcomes <- list(
          left_pos = cur_xi,
          left_neg = loss_1,
          right_stim = L2
        )
        InitializeNextTask(elicit_method,
          next_xi_name,
          outcomes,
          ChainedTargetGain = TRUE,
          random_init,
          bound_scheme
        )

      } else if (cur_task_idx %in% x_neg_idx) {
        # end of x_{i}^-, initialize x_{i+1}^-[1]
        ## current (i) of x_i^-:
        x_idx <- which(x_neg_idx == cur_task_idx)
        cur_xi_name <- paste0("x", x_idx, "neg")
        next_xi_name <- paste0("x", (x_idx + 1), "neg")

        # Get current xi value and required parameters
        cur_xi <- tail(self$get_log()[[cur_xi_name]], 1)
        gain_1 <- private$init_values[["g"]]
        G2 <- tail(self$get_log()[["G2"]], 1)

        outcomes <- list(
          left_pos = gain_1,
          left_neg = cur_xi,
          right_stim = G2
        )
        InitializeNextTask(elicit_method,
          next_xi_name,
          outcomes,
          ChainedTargetGain = FALSE,
          random_init,
          bound_scheme
        )
      }
    },

    get_log = function(target = NULL) {
      # Return the entire log or a specific point, e.g., L, x1pos
      if (is.null(target)) {
        return(private$task_log)
      } else {
        return(private$task_log[[target]])
      }
    },

    # Add a new value to the log for a specific estimated point
    add_entry = function(target, value) {
      private$task_log[[target]] <- c(private$task_log[[target]], value)
    },

    add_choice = function(task_idx, choice) {
      private$choice_history[[task_idx]] <- c(private$choice_history[[task_idx]], choice)
    },

    get_choice_history = function() {
      private$choice_history
    },

    # Update a specific task value at a given index
    # Useful for preparing chained task initial stimulus
    update_task_value = function(target, index, value) {
      stopifnot(
        "Target must be one of the estimated points." =
          target %in% private$targets,
        "Index must be a positive integer." = is.numeric(index) && index > 0,
        "Value must be numeric." = is.numeric(value)
      )
      private$task_log[[target]][index] <- value
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
    targets = c("L", "x1pos", "x1neg", "L2", "G2"),
    min_step = 5L,
    elicit_method = "",
    # Late phase / mixture settings
    is_mix = FALSE,
    late_phase_param = list(),
    # Helper method to create strategy
    create_strategy = function(elicit_method, step_size) {
      switch(elicit_method,
        "Bisection" = Bisection_strategy$new(),
        "Bisection-Slider" = Bisection_strategy$new(),
        "PEST" = PEST_strategy$new(step_size),
        "ASA" = ASA_strategy$new(step_size),
        "MOBS" = MOBS_strategy$new(),
        "SimpBisection" = SimpBisection_strategy$new(),
        "AdaptiveBounds" = Adaptive_bound_strategy$new(), # Placeholder for adaptive strategy
        stop("Unknown elicit method: ", elicit_method)
      )
    }
  ),

  public = list(
    initialize = function(
        exp_params,
        elicit_method = c("Bisection", "PEST", "ASA", "Bisection-Slider", "MOBS", "SimpBisection"),
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
      targets <- c("L", "x1pos", "x1neg", "L2", "G2")
      if (n_est <= 5) {
        private$targets <- targets[1:n_est]
      } else {
        n_add <- (n_est - 5L) %/% 2L
        new_names <- paste0("x", rep(1:n_add, each = 2) + 1L, c("pos", "neg"))
        private$targets <- c(private$targets, new_names)
      }

      # Set configuration
      elicit_method <- match.arg(elicit_method)
      BisectionBased <- elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")
      step_size <- exp_params$step_size %||% 320L
      private$elicit_method <- elicit_method
      private$init_values <- exp_params$init_values
      private$min_step <- exp_params$min_step      # stop criterion
      private$strategy <- private$create_strategy(elicit_method, step_size)

      # Handle bound_scheme parameter
      if (!is.null(exp_params$bound_scheme)) {
        bound_scheme <- exp_params$bound_scheme
      } else if (!is.null(exp_params$fix_bnd_width)) {
        warning(
          "The 'fix_bnd_width' parameter is deprecated. Use 'bound_scheme' instead."
        )
        bound_scheme <- if (isTRUE(exp_params$fix_bnd_width)) "fixed_bnd" else "equal_expectation"
      } else {
        # Default to equal_expectation if neither parameter is provided
        bound_scheme <- "equal_expectation"
      }
      stopifnot(bound_scheme %in% c("fixed_bnd", "equal_expectation", "adaptive"))

      # Initialize task logger
      random_init <- exp_params$random_init %||% FALSE
      if (BisectionBased) {
        # Initialize bounds manager for bisection-based methods
        private$bounds_manager <- bounds_manager$new(private$n_est)
        private$strategy$init_bounds(
          private$bounds_manager,
          private$init_values,
          private$targets,
          bound_scheme
        )
        random_init <- FALSE # Bisection-based methods do not use random initialization
        if (bound_scheme == "adaptive") {
          # Implement adaptive bounds
          private$is_mix <- TRUE
          adaptive_step_size <- exp_params$adaptive_step_size %||% 500L
          if (is.null(private$late_phase_param$est_method)) {
            # If late phase method is not set, use the current elicit method
            elicit_method <- private$elicit_method
          } else {
            # Use the provided late phase estimation method
            elicit_method <- private$late_phase_param$est_method
          }
          private$late_phase_param <- c(
            private$late_phase_param,
            list(
              isLatePhase = FALSE,
              late_phase_trial = integer(),
              # New parameters for adaptive phase control
              isAdaptivePhase = TRUE,
              adaptive_step_size = adaptive_step_size
            )
          )

        }
      } else {
        # Non-bisection strategies do not require bounds manager
        private$bounds_manager <- NULL
        bound_scheme <- NULL
      }
      # Initialize task logger with targets and initial values
      private$task_logger <- task_logger$new(
        private$targets,
        private$init_values,
        random_init,
        bound_scheme
      )
    },

    # Method: Generate lotteries
    generate_lotteries = function(cur_task_idx, cur_trial) {
      G <- private$init_values[["G"]]
      loss_1 <- private$init_values[["l"]]
      gain_1 <- private$init_values[["g"]]
      cur_est_pt <- private$targets[cur_task_idx]

      # DEBUG: Print current state
      # cat("DEBUG generate_lotteries:\n")
      # cat("  cur_task_idx:", cur_task_idx, "cur_trial:", cur_trial, "\n")
      # cat("  cur_est_pt:", cur_est_pt, "\n")
      # cat("  G:", G, "loss_1:", loss_1, "gain_1:", gain_1, "\n")
      # Print current task log for debugging
      current_log <- private$task_logger$get_log(cur_est_pt)
      # cat("  current_log for", cur_est_pt, ":", paste(current_log, collapse=", "), "\n")
      if (cur_trial <= length(current_log)) {
        # cat("  current_log[cur_trial]:", current_log[cur_trial], "\n")
      } else {
        cat("  ERROR: cur_trial", cur_trial, "exceeds log length", length(current_log), "\n")
        stop("Cannot generate lotteries for trial ", cur_trial, 
             " because task log only has ", length(current_log), " entries. ",
             "This suggests the experiment is calling generate_lotteries before update_task_log.")
      }

      # Set the indices(i>1) of x_i+, x_i-
      if (private$n_est <= 5) {
        x_pos_seq <- 0L
        x_neg_seq <- 0L
      } else {
        x_pos_seq <- seq(6L, private$n_est, 2L)
        x_neg_seq <- seq(7L, private$n_est, 2L)
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
        L <- tail(cur_vec, 1)
        # cat("  L from previous task:", L, "\n")
        .A <- c(0, L)
        .B <- rep(private$task_logger$get_log("x1neg")[cur_trial], 2)
      } else if (cur_est_pt == "L2") {
        # (0, .5; l_1) vs (x1+, .5; "L2")
        cur_vec <- self$get_task_log()[["x1pos"]]
        x1pos <- tail(cur_vec, 1)
        # cat("  x1pos from previous task:", x1pos, "\n")
        .A <- c(0L, loss_1)
        .B <- c(x1pos, self$get_task_log()[["L2"]][cur_trial])
      } else if (cur_est_pt == "G2") {
        # (g_1, .5; 0) vs ("G2", .5;  x1-)
        cur_vec <- self$get_task_log()[["x1neg"]]
        x1neg <- tail(cur_vec, 1)
        # cat("  x1neg from previous task:", x1neg, "\n")
        .A <- c(gain_1, 0L)
        .B <- c(self$get_task_log()[["G2"]][cur_trial], x1neg)
      } else if (cur_task_idx %in% x_pos_seq) {
        # current x_i+:
        # get the index of x_i
        x_idx <- which(x_pos_seq == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "pos")
        last_xi_name <- paste0("x", (x_idx - 1), "pos")
        # last x_i :x_{i-1}+:
        cur_vec <- self$get_task_log()[[last_xi_name]]
        last_xipos <- tail(cur_vec, 1)
        # L_2
        L2_vec <- self$get_task_log()[["L2"]]
        L2 <- tail(L2_vec, 1)
        # cat("  last_xipos:", last_xipos, "L2:", L2, "\n")
        # x_i^+: (x_{i-1}+, .5; l_1) vs ("x_i+", .5; L_2)
        .A <- c(last_xipos, loss_1)
        .B <- c(self$get_task_log()[[cur_xi_name]][cur_trial], L2)
      } else if (cur_task_idx %in% x_neg_seq) {
        # current x_i-:
        x_idx <- which(x_neg_seq == cur_task_idx) + 1
        cur_xi_name <- paste0("x", x_idx, "neg")
        last_xi_name <- paste0("x", (x_idx - 1), "neg")
        # last x_i :x_{i-1}-:
        cur_vec <- self$get_task_log()[[last_xi_name]]
        last_xineg <- tail(cur_vec, 1)
        # L_2
        G2_vec <- self$get_task_log()[["G2"]]
        G2 <- tail(G2_vec, 1)
        # cat("  last_xineg:", last_xineg, "G2:", G2, "\n")
        # x_i^-: (g_1, .5; x_{i-1}-) vs (G_2, .5; "x_i-")
        .A <- c(gain_1, last_xineg)
        .B <- c(G2, self$get_task_log()[[cur_xi_name]][cur_trial])
      }

      # DEBUG: Print lottery values before validation
      # cat("  .A:", paste(.A, collapse = ", "), "\n")
      # cat("  .B:", paste(.B, collapse = ", "), "\n")

      lottery_values <- list(".A" = .A, ".B" = .B)
      if (anyNA(.A) | anyNA(.B) || !is.numeric(.A) || !is.numeric(.B)) {
        cat("ERROR: NA or non-numeric values detected!\n")
        sprintf("Currrent Task: %d; Currrent Trial: %d", cur_task_idx, cur_trial)
        print(lottery_values)
        print(self$get_task_log()[[cur_task_idx]])
        stop("NA or non-numeric Lotteries Created in `generate_lotteries()`")
      }
      new_lotteries <- Lotteries$new(lottery_values)
      new_lotteries
    },

    # Method: Main update
    update_task_log = function(choice, cur_task_idx, cur_trial, ...) {
      extra_arg <- list(...)
      use_adaptive_bound <- if (
        !private$elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")
      ) {
        FALSE
      } else if (!is.null(extra_arg$bound_scheme)) {
        extra_arg$bound_scheme == "adaptive"
      } else {
        FALSE
      }
      # Create context for strategy
      context <- list(
        bounds_manager = private$bounds_manager,
        task_logger = private$task_logger,
        late_phase_param = private$late_phase_param,
        phi = extra_arg$phi,
        CheckGainBoundary = cur_task_idx %in% which(grepl("x[2-9]pos|G2", private$targets))
      )

      # Get appropriate strategy (handle mixture)
      if (private$is_mix && cur_trial == 1) {
        # For the first trial, reset phase status
        private$late_phase_param$isLatePhase <- FALSE
        if (!is.null(private$late_phase_param$isAdaptivePhase)) {
          private$late_phase_param$isAdaptivePhase <- TRUE
        }
      }
      IsLatePhase <- private$late_phase_param$isLatePhase %||% FALSE
      IsAdaptivePhase <- private$late_phase_param$isAdaptivePhase %||% FALSE

      current_strategy <-
        if (use_adaptive_bound && IsAdaptivePhase) {
          # Adaptive bounds strategy (first phase)
          private$create_strategy("AdaptiveBounds",
            private$late_phase_param$adaptive_step_size
          )
        } else if (private$is_mix && IsLatePhase) {
          # Use late phase strategy if in late phase (final phase)
          private$create_strategy(private$late_phase_param$est_method,
            private$late_phase_param$start_step
          )
        } else {
          # Use the strategy created at initialization (middle phase or non-mixture)
          private$strategy
        }
      # Get current stimulus
      cur_est_pt <- private$targets[cur_task_idx]
      cur_stim <- private$task_logger$get_log(cur_est_pt)[cur_trial]      # Update using strategy
      new_value <-
        current_strategy$update(choice, cur_stim, cur_task_idx, cur_trial, context)
      cat("DEBUG update_task_log: cur_task_idx =", cur_task_idx, "cur_trial =", cur_trial, "choice", choice, "from", class(current_strategy)[1],"\n")
      cat("  cur_Stim =", cur_stim, "new_Stim =", new_value ,  "\n")      # Get step size to determine if we should continue
      step <- current_strategy$get_step(cur_task_idx, cur_trial, context)

      cat("   step =", step, "diff=", abs(new_value - cur_stim), "min_step =", private$min_step, "\n")

      if (!is.numeric(step) || is.na(step)) {
        print(cur_task_idx)
        print(cur_trial)
        print(context$bounds_manager$get_bounds(cur_task_idx))
        print(current_strategy$name)
        stop(paste("Step must be a positive integer. Current step =", step))
      }

      # Add to log if step is not below minimum
      if (step >= private$min_step) {
        private$task_logger$add_entry(cur_est_pt, new_value)
        private$task_logger$add_choice(cur_task_idx, choice)
      }

      # Handle chained task initialization
      if ((cur_task_idx <= private$n_est - 1) && (cur_trial == 1)) {
        # if not the last task, prepare chained task initial stimulus
        self$prepare_chained_stim(
          cur_task_idx,
          extra_arg$random_init %||% FALSE,
          extra_arg$bound_scheme %||% NULL
        )
      }

      # Handle late phase status for mixture strategies
      if (private$is_mix) {
        current_is_late_phase <- private$late_phase_param$isLatePhase
        current_is_adaptive_phase <- private$late_phase_param$isAdaptivePhase %||% FALSE

        # Determine phase transitions
        if (use_adaptive_bound && current_is_adaptive_phase) {
          # Check if should exit adaptive phase (adaptive -> bisection)
          should_exit_adaptive_phase <- current_strategy$is_searching_complete()
          if (should_exit_adaptive_phase) {
            private$late_phase_param$isAdaptivePhase <- FALSE
            current_strategy$reset()  # Reset adaptive strategy when finishing adaptive phase
          }
        }

        # Check if should enter late phase (bisection -> final strategy like ASA)
        should_enter_late_phase <-
          if (!current_is_adaptive_phase && !current_is_late_phase) {
            # Only check for late phase if not in adaptive phase nor already in late phase
            # Also check if late_phase_param includes start_crit (needed for transition)
            if (!is.null(private$late_phase_param$start_crit)) {
              # transition to late phase if current step exceed start_crit
              step <= private$late_phase_param$start_crit
            } else {
              FALSE  # Don't transition if no criteria specified
            }
          } else {
            current_is_late_phase  # Keep current late phase status
          }
        # Only record transition trial when actually transitioning
        if (!current_is_late_phase && should_enter_late_phase) {
          private$late_phase_param$late_phase_trial <- cur_trial + 1
        }
        private$late_phase_param$isLatePhase <- should_enter_late_phase
      }
    },


    # Method: Prepare chained task initial stimulus (handle initialization schemes)
    prepare_chained_stim = function(
      cur_task_idx,
      random_init = FALSE,
      bound_scheme = NULL
    ) {
      # Delegate to task_logger, passing all needed context
      private$task_logger$prepare_chained_stim(
        elicit_method = private$elicit_method,
        cur_task_idx = cur_task_idx,
        bounds_manager = private$bounds_manager,
        random_init = random_init,
        bound_scheme = bound_scheme
      )
    },

    # Method: print expeiment output
    # format: a vector with names of estimated points
    get_estimates = function() {
      n_est <- private$n_est
      result <- rep(0L, n_est) # Initialize result vector
      log <- private$task_logger$get_log()

      # Build names
      x_leng <- ifelse(private$n_est <= 5, 2L, private$n_est - 3L)
      x_name <- paste0("x", ((1:x_leng) + 1L) %/% 2, c("pos", "neg"))
      all_pts_name <- c("L", x_name[1:2], "L2", "G2", x_name[3:x_leng])
      all_pts_name <- all_pts_name[1:n_est]
      names(result) <- all_pts_name

      for (idx in 1:n_est) {
        name <- all_pts_name[idx]
        if (length(log[[name]]) > 0) {
          result[idx] <- log[[name]][length(log[[name]])]
        } else {
          # Error handling
          print("Experiment log:")
          print(log)
          print("Choice history:")
          print(private$task_logger$get_choice_history())
          stop("`get_estimates()`: NA/NULL in experiment log")
        }
      }
      result
    },

    # Other public methods
    reset_step = function() private$strategy$reset(),
    get_current_step = function(cur_task_idx, cur_trial) {
      # Use the same strategy selection logic as update_task_log
      IsLatePhase <- private$late_phase_param$isLatePhase %||% FALSE
      IsAdaptivePhase <- private$late_phase_param$isAdaptivePhase %||% FALSE

      # Check if we should use adaptive bounds
      AdaptiveBound <- if (!private$elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")) {
        FALSE
      } else if (!is.null(private$late_phase_param$adaptive_step_size)) {
        # If adaptive_step_size exists, we're using adaptive bounds
        TRUE
      } else {
        FALSE
      }

      current_strategy <-
        if (AdaptiveBound && IsAdaptivePhase) {
          # Adaptive bounds strategy (first phase)
          private$create_strategy("AdaptiveBounds",
            private$late_phase_param$adaptive_step_size
          )
        } else if (private$is_mix && IsLatePhase) {
          # Use late phase strategy if in late phase (final phase)
          private$create_strategy(private$late_phase_param$est_method,
            private$late_phase_param$start_step
          )
        } else {
          # Use the strategy created at initialization (middle phase or non-mixture)
          private$strategy
        }

      context <- list(
        bounds_manager = private$bounds_manager,
        late_phase_param = private$late_phase_param
      )
      step <- current_strategy$get_step(cur_task_idx, cur_trial, context)
      if (!is.numeric(step) || is.na(step)) {
        print("Step is not numeric or is NA. Printing stack trace:")
        print(sys.calls())
        stop("Step must be numeric and not NA.")
      } else if (step < 0L) {
        stop(paste("Step must be a non-negative integer. Current step =", step))
      } else {
        step
      }
    },
    get_late_phase_status = function() {
      if (private$is_mix) {
        private$late_phase_param$isLatePhase
      } else {
        FALSE # Not applicable for non-mixture strategies
      }
    },
    get_task_log = function() private$task_logger$get_log(),
    get_choice_history = function() private$task_logger$get_choice_history(),
    get_bound_history = function() {
      if (private$elicit_method %in% c("Bisection", "Bisection-Slider", "MOBS")) {
        private$bounds_manager$show_bounds()
      } else {
        list() # For other strategies, return empty
      }
    }

  )
)


# Null-coalescing operator helper
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}