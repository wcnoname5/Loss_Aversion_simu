# packages
library(R6)

# Player ------------------------------------------------------------------

# `Player` 有 4 個 private attributes：`alpha`、`beta` 和 `lambda`、`phi`。
# 有 2 個 private methods：
#
# + `choose()`：用`compute_prospect_value()` 算一對彩券分別的 CPT value，再依softmax做選擇
# + 若`use_slider`，則回傳理論值。
# + `compute_prospect_value()`：根據效用函數算出該彩券的 CPT value
#
# 當 A 和 B 的 prospects 相同時，player 會有 50% 選 A、50% 選 B。
# 有 1 個 public method：`input_choice()`（把 player 的決策寫在 lotteries 上）。

Player <- R6Class(
  "Player",
  private = list(
    # Attributes
    ## list about utility function parameters
    params = list(),
    ## choice consistency
    phi = NA,
    utility_function = NA,
    # Method
    choose = function(lotteries, utility_function,
                      use_slider =  FALSE, cur_task_idx = NULL, ...) {
      #> choose one of the lotteries depending on the players utility function
      #> Compute net utilities of the 2 lotteries

      # Validate input
      if (is.null(lotteries$A) || is.null(lotteries$B)) {
        stop("Lotteries A and B must not be NULL")
      }

      if (!use_slider) {
        # NOT using slider, return chosen option CPT value
        prospect.A <-
          private$compute_prospect_value(lotteries$A, utility_function)
        prospect.B <-
          private$compute_prospect_value(lotteries$B, utility_function)

        if (is.na(prospect.A)) {
          print(lotteries$A)
          stop("prospect.A error!")
        } else if (is.na(prospect.B)) {
          print(lotteries$B)
          stop("prospect.B error!")
        }

        prospect_dif <- prospect.B - prospect.A

        probChooseA <-
          1 / (1 + exp(private$phi * prospect_dif))
        chooseA <-
          ifelse(is.na(private$phi),
                 prospect_dif < 0,
                 rbinom(1, 1, probChooseA)) #dif ==0, choose A
        if (chooseA) {
          return("A")
        } else {
          return("B")
        }

      } else {
        # if using slider, return indifferent point
        lotteries$find_optimal(
          params = private$params,
          task_idx = cur_task_idx
        ) #trial
      }
    },
    compute_prospect_value = function(lottery, type = c("CRRA", "CARA")) {
      type <- match.arg(type)
      # Compute mixed prospects so that the player can evaluate the lottery
      # lottery <-  c(gain, loss) ,which is lotteries$A or lotteries$B
      if (any(is.na(lottery))) {
        stop(glue("Error: lottery contains NA values. Lottery: {lottery}"))
      } else if (any(is.null(lottery))) {
        stop(glue("Error: lottery contains Null values. Lottery: {lottery}"))
      }
      tryCatch(
        {
          if (lottery[1] == lottery[2]) {
            invisible()
          }
        },
        error = function(e) {
          cat("Error caught: ", e$message, "\n")
          cat("DEBUG: lottery[1] =", lottery[1], "lottery =", lottery, "\n")
          stop(e) # Re-throw the error after logging
        }
      )



      if (lottery[1] == lottery[2]) {
        # 100%
        prospect_value <- utility(lottery[1],
                                  private$params, type = type)
      } else {
        # 50%
        gain_prospect <- utility(lottery[1],
                                 private$params, type = type)
        loss_prospect <- utility(lottery[2],
                                 private$params, type = type)
        lott_sign <- sign(lottery)
        wp_50 <- private$params[["wp"]]
        wn_50 <- private$params[["wn"]]
        if (lott_sign[1] >= 0 && lott_sign[2] >= 0) {
          # All gain
          prospect_value <-
            (wp_50 * gain_prospect) +
            ((1 - wp_50) * loss_prospect)
        }else if (lott_sign[1] <= 0 && lott_sign[2] <= 0) {
          # All loss
          prospect_value <-
            ((1 - wn_50) * gain_prospect) +
            (wn_50 * loss_prospect)
        }else if (lott_sign[1] >= 0 && lott_sign[2] <= 0) {
          # Mixed
          prospect_value <-
            (wp_50 * gain_prospect) +
            (wn_50 * loss_prospect)
        }else { # loss, gain (very strange be still occur)
          # Mixed
          prospect_value <-
            (wn_50 * gain_prospect) +
            (wp_50 * loss_prospect)
        }
      }

      return(prospect_value)
    }
  ),
  public = list(
    initialize = function(params,
                          #include (alpha, beta, lambda, wp, wn),
                          phi,
                          utility_function = c("CRRA", "CARA")) {
      private$params <- params
      private$phi <- phi
      private$utility_function <-
        match.arg(utility_function)
    },
    input_choice = function(lotteries, use_slider = FALSE, cur_task_idx = NULL, ...) {
      # Send player's choice to the game
      choice <-
        private$choose(
          lotteries,
          private$utility_function,
          use_slider,
          cur_task_idx = cur_task_idx
        )
      lotteries$update_result(choice)
    }
  )
)

# Lotteries ---------------------------------------------------------------

# `Lotteries` 有 3 個 private attributes：`A`（彩券 A）、`B`（彩券 B）和 `result`（紀錄 player 的選擇）。
# 同時也用了 active field 來簡化對 A、B 彩券的呼叫。
# 有 1 個 private method：`new_lotteries()`，會根據目前的 cur_task_idx (current task index)
#   產生相對應的兩張 lotteries。
# 該 method 在創建物件時被呼叫。
# 有 1 個 public method：`update_result()`，讓 player 把答案寫在 lotteries 上。


Lotteries <- R6Class(
  "Lotteries",
  private = list(
    # Attributes
    .A = rep(0L, 2), # Lottery 1
    .B = rep(0L, 2), # Lottery 2
    .result = "",
    # Methods
    new_lotteries = function(lottery_values) {
      # Set values of private$.A and private$.B
      private$.A <- lottery_values$.A
      private$.B <- lottery_values$.B
    }
  ),
  active = list(
    A = function(value) {
      if (missing(value)) {
        private$.A
      }
    },
    B = function(value) {
      if (missing(value)) {
        private$.B
      }
    },
    result = function(value) {
      if (missing(value)) {
        private$.result
      }
    }
  ),
  public = list(
    initialize = function(lottery_values) {
      private$new_lotteries(lottery_values)
    },
    update_result = function(choice) {
      private$.result <- choice
    },
    find_optimal = function(params, task_idx) {
      # Given player's parameters and trial index, find the true indifference point
      # first calculate the utility of the target outcome
      # then invert it to get the target dollar value

      # Validate input parameters
      required_params <- c("wp", "wn", "lambda", "alpha", "beta")
      missing_params <- setdiff(required_params, names(params))
      if (length(missing_params) > 0) {
        stop("Missing required parameters: ", paste(missing_params, collapse = ", "))
      }

      wp_50 <- params$wp
      wn_50 <- params$wn
      lambda <- params$lambda
      .alpha <- params$alpha
      .beta <- params$beta

      if (task_idx == 1) {
        result <- -(utility(self$A[1], params, "CRRA") * (wp_50 / wn_50))
        result <- inv_utility(result, params)
      } else if (task_idx %in% c(2, 3)) {
        result <- wp_50 * utility(self$A[1], params, "CRRA") +
                  wn_50 * utility(self$A[2], params, "CRRA")
        result <- inv_utility(result, params)
      } else {
        # task 4, 5, 6, 7, ...
        # Utility of lottery A
        UA <- (wp_50 * utility(self$A[1], params, "CRRA")) +
              (wn_50 * utility(self$A[2], params, "CRRA"))
        if (task_idx == 4 || ((task_idx > 5) && (task_idx %% 2 == 1))) {
          # find loss indifference point
          # 4: L2; 7, 9...: x2_neg, x3_neg
          U_diff <- UA - (wp_50 * utility(self$B[1], params, "CRRA"))
          prob_weight <- wn_50
        } else {
          # find gain indifference point
          U_diff <- UA - (wn_50 * utility(self$B[2], params, "CRRA"))
          prob_weight <- wp_50
        }
        result <- inv_utility(U_diff / prob_weight, params)
      }

      round(result)
    }
  )
)


# Utility Functions -------------------------------------------------------

## CRRA (power function, e.g., TK 1992)
CRRA <- function(x, alpha, beta = NULL, lambda = 2.25) {
  if (is.null(beta)) beta <-  alpha
  utility <- ifelse(x >= 0, x^alpha, -lambda * (abs(x)^beta))
  return(utility)
}

## CARA (exponential, e.g., KW 2005)
CARA <- function(x, mu, nu, lambda = 2.25) {
  utility <- ifelse(x >= 0, (1 - exp(-mu * x)) / mu, -lambda * (exp(-nu * x) - 1) / nu)
  return(utility)
}

utility <- function(x, params, type = c("CRRA", "CARA")) { #params be a list
  type <-  match.arg(type)
  lambda <-  params$lambda
  if (type == "CRRA") {
    alpha <- params$alpha
    beta <- params$beta
  }else {
    mu <- params$mu
    nu <- params$nu
  }
  ss <-  switch(type,
                "CRRA" = CRRA(x, alpha = alpha,
                              beta = beta, lambda = lambda),
                "CARA" = CARA(x, mu = mu,
                              nu = nu, lambda = lambda))
  return(ss)
}

# Inverse Utility (CRRA only)
inv_utility <- function(
  u,
  params = list(alpha = 0.88, beta = 0.88, lambda = 2.25, wp = 0.5, wn = 0.5)
) {
  alpha <- params$alpha
  beta <- params$beta
  lambda <- params$lambda
  if (u >= 0) {
    res <- u^(1 / alpha)
  } else {
    res <- -((-u) / lambda)^(1 / beta)
  }
  round(res)
}

# other functions  --------------------------------------------------------

round_to_5 <- function(x, multiples = 5L) {
  if (!is.numeric(x)) {
    stop("Input is not numeric")
  }
  ifelse(x >= 0,
    ceiling(x / multiples) * multiples,
    (x %/% multiples) * multiples
  )
}

find_optimal_params <- function(
  params = list("alpha" = .88, "beta" = .88, "lambda" = 2.25, "wp" = .5, "wn" = .5),
  exp_params_init = list("G" = 2000L, "l" = -300L, "g" = 300L),
  x_num = 3
) {
  G <- exp_params_init$G
  loss_1 <- exp_params_init$l
  gain_1 <- exp_params_init$g

  # L
  lott <- Lotteries$new(list(".A" = c(G, 0), ".B" = c(0, 0)))
  L <- lott$find_optimal(params = params, 1)
  # x1pos
  lott <- Lotteries$new(list(".A" = c(G, 0), ".B" = c(0, 0)))
  x1pos <- lott$find_optimal(params = params, 2)
  # x1neg
  lott <- Lotteries$new(list(".A" = c(0, L), ".B" = c(0, 0)))
  x1neg <- lott$find_optimal(params = params, 3)
  lambda_KW <- -x1pos / x1neg
  if (x_num == 3) {
    opt <- c(L, x1pos, x1neg, lambda_KW)
    names(opt) <- c("L", "x1pos", "x1neg", "lambda_KW")
  } else if ((x_num %in% c(4, 5)) || ((x_num %% 2) == 1)) {
    # L_2
    lott <- Lotteries$new(list(".A" = c(0, loss_1), ".B" = c(x1pos, 0)))
    L_2 <- lott$find_optimal(params = params, 4)
    # G2
    lott <- Lotteries$new(list(".A" = c(gain_1, 0), ".B" = c(0, x1neg)))
    G_2 <- lott$find_optimal(params = params, 5)
    # x2pos
    opt <- c(L, x1pos, x1neg, L_2, G_2)
    names(opt) <- c("L", "x1pos", "x1neg", "L_2", "G_2")
    # number of x_is
    max_i_value <-  (x_num - 3L) %/% 2
    if (max_i_value >= 2) {
      for (idx in 2:max_i_value){
        last_xi_name <- paste0("x", idx - 1, c("pos", "neg"))
        xi_name <- paste0("x", idx, c("pos", "neg"))
        last_xi_value <-  opt[last_xi_name]
        # find x_i_pos and x_i_neg true value
        lott <- Lotteries$new(list(".A" = c(last_xi_value[1], loss_1), ".B" = c(0, L_2)))
        curr_xi_pos <- lott$find_optimal(params = params, ((2 * idx) - 1) + 5)
        lott <- Lotteries$new(list(".A" = c(gain_1, last_xi_value[2]), ".B" = c(G_2, 0)))
        curr_xi_neg <- lott$find_optimal(params = params, ((2 * idx) + 5))
        opt <- c(opt, curr_xi_pos, curr_xi_neg)
        names(opt)[c((length(opt) - 1), length(opt))] <- xi_name
      }
    }
    opt <- opt[1: x_num]
    opt <- c(opt, "lambda_KW" = lambda_KW)
  } else {
    stop("Invalid 'x_num' value")
  }
  return(opt)
}