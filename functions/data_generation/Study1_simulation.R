library(glue)
library(tidyr)
library(purrr)
library(stringr)
library(dplyr)

# Define file_name before using it
file_name <- paste0("Study1_simulation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")

# Source the necessary functions
source(here::here("functions", "player_and_lotteries.R"))
source(here::here("functions", "new_game.R"))
source(here::here("functions", "TOexperiment.R"))

# Paths -------------------------------------------------------------------
data_dir <- here::here("simulated_data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}
fname <- file.path(data_dir, file_name)

# Create a global log file
log_dir <- file.path(data_dir, "logs")
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}
log_file <- file.path(log_dir,
  paste0("Study1-simulation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# Function to write logs to a file and optionally to the console
write_process_log <- function(msg, show_console = TRUE) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0(timestamp, " - ", msg, "\n")
    cat(message, file = log_file, append = TRUE)
    if (show_console) {
        cat(message)
    }
}


# Simulation Parameters ---------------------------------------------------
.seed <- 898 # random seed

# Prospect Theory parameters
alpha_levels <- c(.88)  # Risk sensitivity for gains
beta_levels <- c(.88)   # Risk sensitivity for losses
lambda_levels <- c(0.5, 1.0, 2.25) # Loss aversion parameter
wp <- 0.5  # Probability weighting for gains
wn <- 0.5  # Probability weighting for losses

# Create parameter combinations
comb_mtx <-
    expand.grid(alpha = alpha_levels, beta = beta_levels, lambda = lambda_levels)

Nsim <- 1000L  # Number of simulations per condition

# Noise parameters (phi controls choice randomness)
.phi <- 0.0367 # Medium noise level (err = 1e-8)
phi_vec <-  c(.phi * 10, .phi, .phi * 0.1)  # Large, medium, small noise
phi_names <- c("large", "medium", "small")

# Create parameter list for each condition
param_list <- lapply(seq_len(nrow(comb_mtx)), function(i) {
    list(
        alpha = comb_mtx$alpha[i],
        beta = comb_mtx$beta[i],
        lambda = comb_mtx$lambda[i],
        wp = wp,
        wn = wn
    )
})

exp_param <- list(
  init_values =
    list("G" = 2000L,
         "g" = 300L, "l" = -300L),
  random_init = FALSE,
  bound_scheme = "fixed_bnd", # "equal_expectation","adaptive"
  n_est = 3L + (6*2L),
  min_step = 5L,
  step_size = 320L,
  early_stop = 500L
)

# Mixture method parameters
mix_param <- list(
    est_method = "ASA", # changed method type: ASA, UD
    start_crit = 500L, # Bisection 3 Times
    start_step = 100L, # ASA_c = 2*100
    stop_rev_times = 3L,
    UseMidrunEst = TRUE
)

# Generate all method names with early stopping variations
method_names <-
    c(
        outer(c("Bisection", "Bisection-Slider", "MOBS"),
                    c("", "_fixbnd"),
                    function(x, y) paste0(x, y)),
        outer(c("SimpBisection", "PEST", "ASA"),
                    c("", "_randInit"),
                    function(x, y) paste0(x, y))
    ) |>
    as.vector()


# Functions ---------------------------------------------------------------
# Generate data given method
process_method <- function(method_name, param, exp_param, mix_param, phi) {
    write_process_log(glue("STARTING: Method {method_name} with phi = {phi}"))

    # Extract base method name
    method <- str_split_1(method_name, "_")[1]
    
    # Extract base method name without early stopping suffix for pattern matching
    base_method_name <- sub("_early\\d+$", "", method_name)

    # Set bound scheme for Bisection-Based Methods
    # Check the base method name (without _early suffix) for adaptiveBnd
    exp_param$bound_scheme <-
    if (!grepl("^(Bisection)|(Bisection-Slider)|(MOBS)|.*mix", base_method_name)) {
      NULL
    } else if (grepl("adaptiveBnd$", base_method_name)) {
        "adaptive"
    } else if (grepl("fixbnd", base_method_name)) {
        "fixed_bnd"
    } else {
        'equal_expectation'
    }
    
    # Debug output for bound scheme setting
    write_process_log(glue("DEBUG: Method {method_name} -> bound_scheme: {exp_param$bound_scheme %||% 'NULL'}"))
    if (grepl("adaptiveBnd", base_method_name)) {
        write_process_log(glue("DEBUG: Confirmed adaptiveBnd pattern matched for method {method_name} (base: {base_method_name})"))
    }
    
    exp_param$random_init <- grepl("randInit", method_name)

    # Check if using mixture method
    use_mixture <- grepl("mix", method_name)
    
    # Set estimation method for mixture
    tmp_method <- sub(".*(ASA).*", "\\1", method_name)
    if (tmp_method %in% c("ASA")) {
        mix_param$est_method <- tmp_method
    }
    # Run simulation with error handling
    result <- tryCatch({
        res <- if (use_mixture) {
            make_log(
                n.rep = Nsim,
                params = param,
                exp_params = exp_param,
                phi = phi,
                elicit_method = method,
                mix_param = mix_param
            )
        } else {
            make_log(
                n.rep = Nsim,
                params = param,
                exp_params = exp_param,
                phi = phi,
                elicit_method = method
            )
        }
        write_process_log(glue("COMPLETED: Method {method_name} with phi = {phi}"))
        res
    }, error = function(e) {
        write_process_log(glue("ERROR in method {method_name} with phi = {phi}: {conditionMessage(e)}"))
        
        # Return an empty tibble with appropriate structure instead of NULL
        # Generate expected target structure based on n_est
        n_est <- exp_param$n_est %||% 3L
        base_targets <- c("L", "x1pos", "x1neg", "L2", "G2")
        if (n_est <= 5) {
            expected_targets <- base_targets[1:n_est]
        } else {
            n_add <- (n_est - 5L) %/% 2L
            new_names <- paste0("x", rep(1:n_add, each = 2) + 1L, c("pos", "neg"))
            expected_targets <- c(base_targets, new_names)
        }
          # Create empty tibble with proper structure
        na_estimates <- setNames(rep(NA_real_, length(expected_targets)), paste0(expected_targets, "_est"))
        na_log <- setNames(rep(list(list(NA_real_)), length(expected_targets)), expected_targets)
        
        tibble::tibble(
            Nsim = integer(0),
            !!!na_log,
            !!!na_estimates
        )
    })

    result
}


# Generate data given hyperparameter of the agent
process_phi <- function(phi, param) {
    write_process_log(glue("\n=== STARTING phi = {phi} ==="))
    write_process_log(
        glue("\n=== with alpha = {param$alpha}, beta = {param$beta}, lambda = {param$lambda} ===")
    )
    # Process methods sequentially
    tmp_list <- map(
        method_names,
        ~process_method(.x, param, exp_param, mix_param, phi)
    )
    names(tmp_list) <- method_names
    
    # Filter out any NULL results before combining
    tmp_list <- tmp_list[!sapply(tmp_list, is.null)]
    
    # Combine results and clean the data
    if (length(tmp_list) > 0) {
        tmp.df <- tmp_list %>%
            bind_rows(.id = "method_type") %>% 
            cleaning2()  # Make sure this function is defined somewhere
    } else {
        # If all methods failed, return an empty tibble
        write_process_log(glue("WARNING: All methods failed for phi = {phi}"))
        tmp.df <- tibble::tibble()
    }

    write_process_log(glue("=== COMPLETED phi = {phi} ==="))
    tmp.df
}


# Main Execution ----------------------------------------------------------
if (file.exists(fname)) {
    # Load existing data if file exists
    target_list <- readRDS(fname)
    cat(glue("{fname} Exists and Loaded\n"))
} else {
    # Run new simulation
    set.seed(.seed)
    cat("File name:", file_name, "\n", file = log_file, append = TRUE)
    write_process_log(glue("=== Starting Simulation at {Sys.time()} ==="))    # Process everything sequentially except for the internal Nsim parallelization
    target_list <- list()
    for (p in param_list) {
        param_name <- glue("alpha{p$alpha}_beta{p$beta}_lambda{p$lambda}")

        # Process each phi and collect results with error handling
        tmp_list <- lapply(seq_along(phi_vec), function(i) {
            phi <- phi_vec[i]
            tryCatch({
                process_phi(phi, param = p)
            }, error = function(e) {
                write_process_log(glue("ERROR in processing phi = {phi} for {param_name}: {conditionMessage(e)}"))
                # Return empty tibble on failure
                tibble::tibble()
            })
        })
        
        # Combine results into a data frame
        names(tmp_list) <- phi_names
        tmp_df <- tmp_list %>%
            bind_rows(.id = "phi")

        target_list[[param_name]] <- tmp_df
    }
    
    # Combine all parameter results into a single data frame
    target_df <- target_list %>%
        bind_rows(.id = "param") %>% 
        separate_wider_regex(
            param,
            patterns =
                c("alpha", alpha = "\\d+\\.\\d+","_beta",
                    beta = "\\d+\\.\\d+", "_lambda",
                    lambda = "\\d+\\.?\\d*")
        ) %>%
        mutate(
            across(c(alpha, beta, lambda), as.numeric),
        )
        
    # Save results
    saveRDS(target_df, file = fname)
    write_process_log(glue("=== Simulation Completed at {Sys.time()} ==="))
}