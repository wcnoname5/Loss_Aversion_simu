library(glue)
library(purrr)
library(stringr)

source("../player_and_lotteries.R")
source("../game_and_exp.R")
source("../TO_exp.R")

# Simulation Parameters ---------------------------------------------------

Nsim <-  1000L
.phi <-  0.0367 # err = 1e-8, 0.027513126 is err = 1e-6
param <- list("alpha"=.88, "beta"=.88, "lambda"=2.25, "wp"=.5, "wn"=.5)
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

phi_vec <-  c(.phi*10, .phi)
phi_names <- c("large", "medium")

# File Parameters --------
dir_name <- "./simulation_RDS"
file_names <- "boundary_parallel.RDS"
fname <- file.path(dir_name, file_names)
pic_path <- "./figs/"

method_names <-
  c(
    paste(c(
      "PEST",
      "ASA",
      "ASA_randInit",
      "PEST_randInit"
    ), "fixbnd", sep="_"),
    "Bisection", "Bisection-Slider", "MOBS",
    paste(c("Bisection", "Bisection-Slider", "MOBS"), "fixbnd", sep="_"),
    "PEST", "ASA",
    "ASA_randInit", "PEST_randInit"
  )

# Create a global log file
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
if (!dir.exists(dir_name)) {
  # create RDS directory
  dir.create(script_dir, dir_name, showWarnings = FALSE)
}
log_file <- file.path(script_dir, "logs",
                      paste0("Early_Stop_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create("logs", showWarnings = FALSE)

write_process_log <- function(msg, show_console = TRUE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0(timestamp, " - ", msg, "\n")
  cat(message, file = log_file, append = TRUE)
  if (show_console) {
    cat(message)
  }
}

process_method <- function(method_name, exp_param, mix_param, phi) {
  write_process_log(glue("STARTING: Method {method_name} with phi = {phi}"))
  
  # Modify parameters based on method_name
  exp_param$early_stop <- sub(".*_early", "", method_name) |> as.numeric()
  if (grepl("Slider", method_name)) {
    exp_param$early_stop <- exp_param$early_stop - 1
  }
  exp_param$fix_bnd_width <- grepl("fixbnd", method_name)
  exp_param$random_init <- grepl("randInit", method_name)
  method <- str_split_1(method_name, "_")[1]
  
  result <- tryCatch({
    res <- make_log(
      n.rep = Nsim,
      params = param,
      exp_params = exp_param,
      phi = phi,
      est_type = method
    )
    write_process_log(glue("COMPLETED: Method {method_name} with phi = {phi}"))
    res
  }, error = function(e) {
    write_process_log(glue("ERROR in method {method_name} with phi = {phi}: {conditionMessage(e)}"))
    NULL
  })
  
  result
}

process_phi <- function(phi, phi_name) {
  write_process_log(glue("\n=== STARTING phi = {phi} ==="))
  
  # Process methods sequentially
  tmp_list <- map(
    method_names,
    ~process_method(.x, exp_param, mix_param, phi)
  )
  names(tmp_list) <- gsub("Bisection-Slider", "Bisection_Slider", method_names)
  
  tmp.df <- tmp_list %>%
    bind_rows(.id = "method_type") %>% 
    cleaning2()
  
  write_process_log(glue("=== COMPLETED phi = {phi} ==="))
  tmp.df
}

# Main execution
if (file.exists(fname)) {
  target_list <- readRDS(fname)
  cat(glue("{fname} Exists and Readed"))
} else {
  set.seed(898)
  write_process_log(glue("=== Starting Simulation at {Sys.time()} ==="))
  
  # Process everything sequentially except for the internal Nsim parallelization
  target_list <- list()
  for (i in seq_along(phi_vec)) {
    phi <- phi_vec[i]
    phi_name <- phi_names[i]
    target_list[[phi_name]] <- process_phi(phi, phi_name)
  }
  
  # Save results
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  saveRDS(target_list, file = fname)
  
  write_process_log(glue("=== Simulation Completed at {Sys.time()} ==="))
}


