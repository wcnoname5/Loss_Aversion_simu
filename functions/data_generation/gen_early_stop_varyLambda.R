library(glue)
library(purrr)
library(stringr)

source("../player_and_lotteries.R")
source("../game_and_exp.R")
source("../TO_exp.R")

# Paths -------------------------------------------------------------------
dir_name <- "./simulation_Rmds/ASA_simulation_RDS/"
file_names <- "early_stop_varyLambda_df.RDS"
fname <- paste0(dir_name, file_names)
# Create a global log file
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
log_file <- file.path(script_dir, "logs",
                      paste0("Fixed_Boundary_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create("logs", showWarnings = FALSE)
write_process_log <- function(msg, show_console = TRUE) {
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0(timestamp, " - ", msg, "\n")
  cat(message, file = log_file, append = TRUE)
  if (show_console) {
    cat(message)
  }
}


# Simulation Parameters ---------------------------------------------------
alpha_levels <- c(.88)
beta_levels <- c(.88)
lambda_levels <- c(0.5, 1.0, 2.25)
wp <- 0.5
wn <- 0.5
comb_mtx <- 
  expand.grid(alpha = alpha_levels, beta = beta_levels, lambda = lambda_levels)

Nsim <-  1000L
.phi <-  0.0367 # err = 1e-8, 0.027513126 is err = 1e-6
param_list <- lapply(1:nrow(comb_mtx), function(i) {
  list(
    alpha = comb_mtx$alpha[i],
    beta = comb_mtx$beta[i],
    lambda = comb_mtx$lambda[i],
    wp = wp,
    wn = wn
  )
})

Nsim <-  1000L
.phi <-  0.0367 # err = 1e-8, 0.027513126 is err = 1e-6
exp_param <- list(
  init_values =
    list("G"= 2000L,
         "g"=300L, "l"=-300L, "x1+"=1000L),
  random_init = FALSE,
  fix_bnd_width = FALSE,
  n_est = 3L + (6*2L),
  min_step = 5L,
  step_size = 320L,
  early_stop = 500L
)
mix_param <- list(
  est_method = "ASA", # changed method type: ASA, UD
  start_crit = 500L, # Bisection 3 Times
  start_step = 100L, # ASA_c = 2*100
  stop_rev_times = 3L,
  UseMidrunEst = TRUE 
) 

phi_vec <-  c(.phi * 10, .phi, .phi * 0.1)
phi_names <- c("large", "medium", "small")
method_names <-
  c(
    "Bisection", "Bisection-Slider",
    "MOBS",
    "PEST", "ASA",
    paste0(c("ASA", "PEST"), "_randInit"),
    "Bisection_mixASA"
  )|>
  outer(c(5, 10), paste, sep="_early")|>
  as.vector()


# Functions ---------------------------------------------------------------
# Generate data given method
process_method <- function(method_name, param, exp_param, mix_param, phi) {
  write_process_log(glue("STARTING: Method {method_name} with phi = {phi}"))
  
  # Modify parameters based on method_name
  exp_param$early_stop <- sub(".*_early", "", method_name) |> as.numeric()
  if (grepl("Slider", method_name)) {
    exp_param$early_stop <- exp_param$early_stop - 1
  }
  early_stop_trial <- sub(".*_early", "", method_name) |> as.numeric()
  if (is.na(early_stop_trial)) stop("Invalid: Early Stop is NA")
  method <- str_split_1(method_name, "_")[1]
  exp_param$early_stop <- early_stop_trial
  # Fixed_bound_width for Bisection-Based Methods 
  exp_param$fix_bnd_width <- grepl("Bisection|MOBS", method_name)
  exp_param$random_init <- grepl("randInit", method_name)
  use_mixture <- grepl("mix", method_name)
  tmp_method <- sub(".*(ASA|UD).*", "\\1", method_name)
  if (tmp_method %in% c("ASA", "UD")) {
    mix_param$est_method <- tmp_method
  } 
  result <- tryCatch({
    res <- if (use_mixture){
      make_log(
        n.rep = Nsim,
        params = param,
        exp_params = exp_param,
        phi = phi,
        est_type = method,
        mix_param = mix_param
      )
    }else {
      make_log(
        n.rep = Nsim,
        params = param,
        exp_params = exp_param,
        phi = phi,
        est_type = method
      )
    }
    write_process_log(glue("COMPLETED: Method {method_name} with phi = {phi}"))
    res
  }, error = function(e) {
    write_process_log(glue("ERROR in method {method_name} with phi = {phi}: {conditionMessage(e)}"))
    NULL
  })
  
  result
}

# Generate data given hyperparemeter of the agent
process_phi <- function(phi, param) {
  write_process_log(glue("\n=== STARTING phi = {phi} ==="))
  write_process_log(
    glue("\n=== with alpha = {param$alpha}, beta = {param$beta}, lambda = {param$lambda}} ===")
  )
  # Process methods sequentially
  tmp_list <- map(
    method_names,
    ~process_method(.x, param, exp_param, mix_param, phi)
  )
  names(tmp_list) <- gsub("Bisection-Slider", "Bisection_Slider", method_names)
  
  tmp.df <- tmp_list %>%
    bind_rows(.id = "method_type") %>% 
    cleaning2()
  
  write_process_log(glue("=== COMPLETED phi = {phi} ==="))
  tmp.df
}

# Main Execution ----------------------------------------------------------
if (file.exists(fname)) {
  target_list <- readRDS(fname)
  cat(glue("{fname} Exists and Readed"))
} else {
  set.seed(898)
  cat("File name:", file_names, "\n", file = log_file, append = TRUE)
  write_process_log(glue("=== Starting Simulation at {Sys.time()} ==="))
  
  # Process everything sequentially except for the internal Nsim parallelization
  target_list <- list()
  for (p in param_list) {
    param_name <- glue("alpha{p$alpha}_beta{p$beta}_lambda{p$lambda}")
    
    # Process each phi and collect results
    tmp_list <- lapply(seq_along(phi_vec), function(i) {
      phi <- phi_vec[i]
      process_phi(phi, param = p)
    })
    # Combine results into a data frame
    names(tmp_list) <- phi_names
    tmp_df <- tmp_list %>%
      bind_rows(.id = "phi")
    
    target_list[[param_name]] <- tmp_df
  }
  rm(tmp_df)
  # Combine all parameter results into a single data frame
  target_df <- target_list %>%
    bind_rows(.id = "param") %>% 
    separate_wider_regex(
      param,
      patterns =
        c("alpha", alpha = "\\d+\\.\\d+","_beta",
          beta = "\\d+\\.\\d+", "_lambda",
          lambda= "\\d+\\.?\\d*")
    ) %>% 
    mutate(
      across(c(alpha, beta, lambda), as.numeric),
    )
  # Save results
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  saveRDS(target_df, file = fname)
  
  write_process_log(glue("=== Simulation Completed at {Sys.time()} ==="))
}

