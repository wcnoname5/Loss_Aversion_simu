source(here::here("functions", "player_and_lotteries.R"))

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

# Prob of choosing the option having greater value
softmax <- function(dU, phi) {
  ifelse(dU >= 0, 
         1 / (1 + exp(-dU * phi)), 
         1 / (1 + exp(dU * phi)))
}

solve_phi <- Vectorize(function(dU, target = 0.1) {
  # Define the equation to solve
  equation <- function(phi) {
    softmax(dU, phi) - (1 - target)
  }
  # Solve for phi using `uniroot`
  result <- uniroot(equation, lower = -0, upper = 5)
  # Return the root
  result$root
}, vectorize.args = "dU")
wn <- param$wn
wp <- param$wp

# delta U in first choice of L in bisection (equal expectation)
dU_1st <- (utility(2000, params = param ,"CRRA") * wp) + 
  (utility(-2000, params = param ,"CRRA")* wn) - 0

# delta U in first choice of x1+ in bisection (equal expectation)
dU_2nd <- (utility(2000, params = param ,"CRRA") * wp) + 0 -  
  (utility(1000, params = param ,"CRRA")* wp) 

rm(wn, wp)

err <- c(0.2, 10 ^ (-1:-15))

mtx <- matrix(0, nrow = 2, ncol = length(err),
              dimnames = list(c("L", "x1pos"), err))
for (i in 1:length(err)){
  mtx[, i] <-  solve_phi(c(dU_1st, dU_2nd), err[i])
}
cat("column names stand for error rate, row names stand for conditions. \n")
print(round(mtx,7))
