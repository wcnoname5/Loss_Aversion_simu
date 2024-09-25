# packages
library(R6)

# Player ------------------------------------------------------------------

# `Player` 有 4 個 private attributes：`alpha`、`beta` 和 `lambda`、`phi`。
# 有 2 個 private methods：
#
# + `choose()`：用`compute_prospects()` 算一對彩券分別的 prospects，再依softmax做選擇
# + 若`slider`，則回傳理論值。
# + `compute_prospects()`：根據效用函數算出該彩券的 prospect
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
                      slider =  FALSE, ...) {
      #> choose one of the lotteries depending on the players utility function
      #> Compute net utilities of the 2 lotteries
      if (!slider) {
        #NOT using slider, return choosen option
        prospect.A <-
          private$compute_prospects(lotteries$A, utility_function)
        prospect.B <-
          private$compute_prospects(lotteries$B, utility_function)
        if (is.na(prospect.A)) {
          print(lotteries$A)
          stop("prospect.A error!")
        } else if (is.na(prospect.B)) {
          print(lotteries$B)
          stop("prospect.B error!")
        }
        ## TODO other than difference?
        prospect_dif <- prospect.B - prospect.A
        
        cho_A_prob <-
          1 / (1 + exp(private$phi * prospect_dif))
        choose_A <-
          ifelse(is.na(private$phi),
                 prospect_dif < 0,
                 rbinom(1, 1, cho_A_prob)) #dif ==0, choose A
        if (choose_A) {
          return("A")
        } else{
          return("B")
        }
        
      } else{
        # if using slider, return indifferent point
        est_quant <- list(...)$est_quant
        return(lotteries$find_optimal(params = private$params,
                                      est_quant = est_quant)) #trial
      }
    },
    compute_prospects = function(lottery, type = c("CRRA", "CARA")) {
      type <- match.arg(type)
      # Compute mixed prospects so that the player can evaluate the lottery
      # lottery <-  c(gain, loss) ,which is lotteries$A or lotteries$B
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
        if (lott_sign[1]>=0 && lott_sign[2]>=0){
          # All gain
          prospect_value <-
            (wp_50 * gain_prospect) +
            ((1-wp_50) * loss_prospect)
        }else if (lott_sign[1]<=0 && lott_sign[2]<=0){
          # All loss
          prospect_value <-
            ((1-wn_50) * gain_prospect) +
            (wn_50 * loss_prospect)
        }else if (lott_sign[1]>=0 && lott_sign[2]<=0){
          # Mixed
          prospect_value <-
            (wp_50 * gain_prospect) +
            (wn_50 * loss_prospect)
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
    input_choice = function(lotteries, slider = FALSE, ...) {
      # Send player's choice to the game
      est_quant <- list(...)$est_quant
      choice <-
        private$choose(lotteries, private$utility_function, slider, est_quant=est_quant)
      lotteries$update_result(choice)
    }
  )
)

# Lotteries ---------------------------------------------------------------

# `Lotteries` 有 3 個 private attributes：`A`（彩券 A）、`B`（彩券 B）和 `result`（紀錄 player 的選擇）。
# 同時也用了 active field 來簡化對 A、B 彩券的呼叫。  
# 有 1 個 private method：`new_lotteries()`，會根據目前的 est_quant
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
      if(missing(value)) {
        return(private$.A)
      }
    },
    B = function(value) {
      if(missing(value)) {
        return(private$.B)
      }
    },
    result = function(value) {
      if(missing(value)) {
        return(private$.result)
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
    find_optimal = function(params, est_quant){
      wp_50 <- params$wp
      wn_50 <- params$wn
      lambda <- params$lambda
      .alpha <- params$alpha
      .beta <- params$beta
      if (est_quant==1){
        # result <- -((utility(self$A[1],
        #                      params, "CRRA")/lambda)^(1/.beta)
        result <- -(utility(self$A[1], params, "CRRA")*
                     (wp_50/wn_50))
        # print(result)
        result <- inv_utility(result, params)
      } else if(est_quant %in% c(2,3)){
        result <- wp_50 * utility(self$A[1], params, "CRRA") +
          wn_50 * utility(self$A[2], params, "CRRA")
        # if (result>=0) {
        #   result <- result^(1/.alpha)
        # }else {
        #   result <- -((1/lambda)*(-result))^(1/.alpha)
        # }
        result <- inv_utility(result)
      }
      return(round(result))
    }
  )
)


# Utility Functions -------------------------------------------------------

## CRRA (power function, e.g., TK 1992)
CRRA <- function(x, alpha, beta = NULL, lambda=2.25){
  if (is.null(beta)) beta <-  alpha
  utility <- ifelse(x>=0, x^alpha, -lambda*(abs(x)^beta))
  return(utility)
}

## CARA (exponential, e.g., KW 2005)
CARA <- function(x, mu, nu, lambda=2.25){
  utility <- ifelse(x>=0, (1-exp(-mu*x))/mu, -lambda*(exp(-nu*x)-1)/nu)
  return(utility)
}

utility <- function(x, params, type = c("CRRA", "CARA")){ #params be a list
  type <-  match.arg(type)
  lambda <-  params$lambda
  if (type =="CRRA"){
    alpha <- params$alpha
    beta <- params$beta
  }else{
    mu <- params$mu
    nu <- params$nu
  }
  ss <-  switch(type,
                "CRRA" = CRRA(x, alpha = alpha,
                              beta = beta, lambda= lambda),
                "CARA" = CARA(x, mu = mu,
                              nu = nu, lambda= lambda))
  return(ss)
}

# Inverse Utility (CRRA only)
inv_utility <- function(u, params = list("alpha"=.88, "beta"=.88, "lambda"=2.25, "wp"=.5, "wn"=.5)){
  alpha <- params$alpha
  beta <- params$beta
  lambda <- params$lambda
  if (u >= 0){
    res <- u^(1/alpha)
  } else {
    res <- -((-u)/lambda)^(1/beta)
  }
  return(round(res))
}

find_optimal_params <- function(
    params = list("alpha"=.88, "beta"=.88, "lambda"=2.25, "wp"=.5, "wn"=.5),
    exp_param = list("G"=2000L, "L_lc"= -300L, "G_lc" = 300L),
    elicit_x2 = FALSE
){
  wp <- params$wp
  wn <- params$wn
  G <- exp_param$G
  l <- exp_param$L_lc
  g <- exp_param$G_lc
  
  # L
  lott <- Lotteries$new(list(".A" = c(G,0), ".B"= c(0,0)))
  L <- lott$find_optimal(params = params, 1) 
  # x1pos
  lott <- Lotteries$new(list(".A" = c(G,0), ".B"= c(0,0)))
  x1pos <- lott$find_optimal(params = params, 2) 
  # x1neg
  lott <- Lotteries$new(list(".A" = c(0, L), ".B"= c(0,0)))
  x1neg <- lott$find_optimal(params = params, 3) 
  lambda_KW <- -x1pos/x1neg
  if (!elicit_x2){
    opt <- c(L, x1pos, x1neg, lambda_KW)
    names(opt) <- c("L", "x1pos", "x1neg", "lambda")
  } else {
    # L_hw
    L_hw <-( utility(l, params, type = "CRRA") - 
      ((wp/wn) * utility(x1pos, params, type = "CRRA"))) |>
      inv_utility(params = params)
    # x2pos
    x2pos <-
      (utility(x1pos, params, type = "CRRA") +
      ((wn/wp) * utility(l, params, type = "CRRA")) - 
      ((wn/wp) * utility(L_hw, params, type = "CRRA"))) |>
      inv_utility(params = params)
    G_hw <- (utility(g, params, type = "CRRA") - 
      ((wn/wp) * utility(x1neg, params, type = "CRRA"))) |>
      inv_utility(params = params) #L_hw
    x2neg <-
      (utility(x1neg, params, type = "CRRA")+
      ((wp/wn) * utility(g, params, type = "CRRA")) - 
      ((wp/wn) * utility(G_hw, params, type = "CRRA"))) |>
      inv_utility(params = params)
    opt <- c(L, x1pos, x1neg,
             L_hw, x2pos, G_hw, x2neg)
    names(opt) <- c("L", "x1pos", "x1neg",
                    "L_hw", "x2pos", "G_hw", "x2neg")
  } 
  return(opt)
}

opt <- find_optimal_params()
