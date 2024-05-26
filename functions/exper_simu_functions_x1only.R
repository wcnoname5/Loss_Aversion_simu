# packages
library(R6)
library(tidyverse)
# 1. Defining classes (Player, lottery, games) -------------------------------

### 1.1 Player

Player <- R6Class("Player",
  private = list(
    # Attributes
    alpha = NA, # power in gain domain
    beta = NA, # power in loss domain
    lambda = NA, # loss aversion in KT, 1992
    phi = NA, # choice consistency
    utility_function = NA,
    wp = .5,
    wn = .5,
    # Method
    choose = function(lotteries, utility_function) {
      # Choose one of the lotteries depending on the player's utility function
      # Compute net utilities of the 2 lotteries
      prospect.A <- private$compute_prospects(lotteries$A, utility_function)
      prospect.B <- private$compute_prospects(lotteries$B, utility_function)
      if (is.na(prospect.A)) {
        print(lotteries$A)
        stop("prospect.A error!")
      } else if (is.na(prospect.B)) {
        print(lotteries$B)
        stop("prospect.B error!")
      }
      prospect_dif <- prospect.B-prospect.A
      # Determinstic:
      # if (is.null(phi)) {
      #   cA = ifelse(prospect_dif<0, TRUE,
      #               ifelse(prospect_dif==0,rbinom(1,1,.5),FALSE))}
      # Softmax:
      cho_A_prob <- 1/( 1+exp(private$phi * prospect_dif) ) 
      choose_A <- ifelse(is.na(private$phi), prospect_dif<0,
                   rbinom(1,1, cho_A_prob)) #dif ==0, choose A
      if (choose_A){
        return("A")
      }else{
        return("B")
      }
    },
    compute_prospects = function(lottery, type=c("CRRA","CARA")) {
      type <- match.arg(type)
      # Compute mixed prospects so that the player can evaluate the lottery
      # lottery <-  c(gain, loss) ,which is lotteries$A or lotteries$B
      if (lottery[1] == lottery[2]) {  # 100%
          mixed_prospect <- utility(lottery[1],
                                    private$alpha,
                                    private$beta,
                                    private$lambda, type = type)
       } else {  # 50% vs. 50%
        gain_prospect <- utility(lottery[1],
                                 private$alpha,
                                 private$beta,
                                 private$lambda, type = type)
        loss_prospect <- utility(lottery[2],
                                 private$alpha,
                                 private$beta,
                                 private$lambda, type = type)
        ## TODO: w(.5) = .5 quite doubt
        mixed_prospect <- 
          (private$wp*gain_prospect) + 
          (private$wn*loss_prospect)  # assume no weighting function
      }
      return(mixed_prospect)
    }
  ),
  public = list(
    initialize = function(params,
                          # alpha, beta, lambda, wp, wn,
                          phi,
                          utility_function = c("CRRA","CARA")) {
      private$alpha <- params["alpha"] 
      private$beta <- params["beta"]  
      private$lambda <- params["lambda"] 
      private$wp <- params["wp"]
      private$wn <- params["wn"]
      private$phi <- phi 
      private$utility_function <-  match.arg(utility_function)
    },
    input_choice = function(lotteries) {
      # Send player's choice to the game
      choice <- private$choose(lotteries, private$utility_function)
      lotteries$update_result(choice)
    }
  )
)

# `Player` 有 3 個 private attributes：`alpha`、`beta` 和 `lambda`、`phi`。  
# 有 2 個 private methods：
# 
# + `choose()`：用`compute_prospects()` 算一對彩券分別的 prospects，再依softmax做選擇
# + `compute_prospects()`：根據效用函數算出該彩券的 prospect
# 
# 當 A 和 B 的 prospects 相同時，player 會有 50% 選 A、50% 選 B。  
# 有 1 個 public method：`input_choice()`（把 player 的決策寫在 lotteries 上）。  


### 1.2 Lotteries
Lotteries <- R6Class("Lotteries",
    private = list(
     # Attributes
     .A = rep(0L, 2), # Lottery 1
     .B = rep(0L, 2), # Lottery 2
     .result = "",
     # Methods
     new_lotteries = function(game, trial, task, given_values) {
       # Set values of private$.A and private$.B
       # given_values <- c(x(i-1)pos, xipos_(j-1), L2, G2)
       
       # Get initial values from the game setting
       G <- game$show_setting()[3]
       L <- game$show_setting()[4]
       g <- game$show_setting()[5]
       l <- game$show_setting()[6]
       
       # Set lotteries' value
       if (trial == 1) {  # L
         private$.A <- c(G, given_values[2])
       } else if (trial == 2) {  # x1pos
         private$.A[1] <- G
         private$.B[1:2] <- rep(given_values[2])
       } else if (trial == 3) {  # x1neg
         private$.A[2] <- L
         private$.B <- rep(given_values[2], 2)
       }
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
     initialize = function(game, trial, task, given_values) {
       # given_values <- c(x(i-1)pos, xipos_(j-1), L2, G2)
       private$new_lotteries(game, trial, task, given_values)
     },
     update_result = function(choice) {
       private$.result <- choice
     }
   )
)

# `Lotteries` 有 3 個 private attributes：`A`（彩券 A）、`B`（彩券 B）和 `result`（紀錄 player 的選擇）。
# 同時也用了 active field 來簡化對 A、B 彩券的呼叫。  
# 有 1 個 private method：`new_lotteries()`，會根據目前的 trial 和 task 來產生相對應的兩張 lotteries。
# 該 method 在創建物件時被呼叫。  
# 有 1 個 public method：`update_result()`，讓 player 把答案寫在 lotteries 上。  


### 1.3 Game


# TODO: change n_trials to 3 ----------------------------------------------

Game <- R6Class("Game",
    private = list(
      # Attributes
      # TODO variable init value and sequence
      init_values = c("G"= 2000L, "L"=-2000L, "g"=300L, "l"=-300L, "x1+"=1000L),  # G, L, g, l, x1pos
      n_trial = 3L, #19L, # = 5+ 7 + 7
      n_task = 5L, # times of bisection
      task_log = NA,  # list of vectors, should be initialized
      lotteries_box = NA,  # 2D list, should be initialized
      # Methods
      ## bisection
      bisection_update = function(choice, value_list, cur_task, cur_trial) {
        # choice <- "A" or "B"
        # value_list: e.g., x1pos_list
        # length(value_list) <- 6
        target <- value_list[cur_task] 
        step <- abs(value_list[1])
        change <- ((1 / 2) ^ cur_task) * step
        
        if (cur_trial == 1) {  # L
          change <- -change
        }
        
        if (choice == "A") {
          target <- target + change
        } else {
          target <- target - change
        }
        
        target <- as.integer(round(target / 5) * 5)  # Make target divisible by 5
        return(target) # return bisection iteration value
      }
    ),
    public = list(
      initialize = function() {
        n_trial <- private$n_trial
        n_task <- private$n_task
        # Initialize task_log
        private$task_log <- rep(list(rep(NA, n_task + 1L)), n_trial)
        private$task_log[[1]][1] <- private$init_values[2] # -2000
        private$task_log[[2]][1] <- private$init_values[5] # 1000
        
        # Initialize lotteries_box
        private$lotteries_box <- as.list(rep(NA, n_trial * n_task))
        dim(private$lotteries_box) <- c(n_trial, n_task)
      },
      show_setting = function() {
        # Output how many trials and tasks there are in this game
        setting <- c(
          "N_est" = private$n_trial,
          "N_bisect" = private$n_task,
          "Init" = private$init_values
        )
        return(setting)
      },
      
      # Generate a new pair of lotteries.
      generate_lotteries = function(cur_trial, cur_task) {  
        needed_values <- rep(NA, 2)
        # Find needed_values[2] depending on current task
        needed_values[2] <- private$task_log[[cur_trial]][cur_task]
        
        # Produce a new pair of lotteries
        new_lotteries <- Lotteries$new(self, cur_trial, cur_task, needed_values)
        return(new_lotteries)
      },
      
      update_lotteries_box = function(lotteries, cur_trial, cur_task) {
        private$lotteries_box[[cur_trial, cur_task]] <- lotteries
      },
      
      update_task_log = function(choice, cur_trial, cur_task) {
        # The function takes the choice of players, does the computation, and update the task log.
        # Update the current value
        target_list <- private$task_log[[cur_trial]]
        value <- private$bisection_update(choice, target_list, cur_task, cur_trial) 
        private$task_log[[cur_trial]][cur_task + 1] <- value
        
        # Update next trial's initial value
        if (cur_task == 5) {  # Updating values after the final task of this trial
          g <- private$init_values[3]
          l <- private$init_values[4]
          
          if (cur_trial == 2) {  # x1pos, initialize x1neg[1]
            L <- private$task_log[[1]][6]
            private$task_log[[3]][1] <- as.integer(round(floor(L * .5) / 5) * 5)  # x1neg[1]
          } else if (cur_trial == 3) {  # x1neg, initialize L2 & G2
            x1pos <- private$task_log[[2]][6]
            x1neg <- private$task_log[[3]][6]
          }
        }
      },
      output_exp_result = function() {
        # result <- rep(NA, 16)
        result <- rep(NA, 2)
        result[1] <- private$task_log[[2]][6]
        result[2] <- private$task_log[[3]][6]
        names(result) <- paste0("x1", c("+","-"))
        return(result)
      },
      show_task_log = function() private$task_log,
      show_lotteries_box = function() private$lotteries_box
  )
)


# `Game` 有 5 個 private attributes：
# 
# + `init_value`：$G$、$L$、$g$、$l$
#   + `n_trial`
# + `n_task`
# + `task_log`：$19 \times 6$ 的 list of vectors，紀錄實驗中的所有點
# + `lotteries_box`：紀錄實驗中的所有 lotteries
# 
# 有 1 個 private method：`bisection_update()`，根據 player 的選擇來更新下一張 lotteries 所需要的數值。  
# 有 7 個 public method：
# 
# + `show_setting()`：輸出 `n_trial`、`n_task` 和 `init_value`
# + `generate_lotteries()`：根據目前的 trial 和 task 產生相對應的 lottery
# + `update_lotteries_box()`：把 lotteries 和 player 的選擇，都一起儲存下來
# + `update_task_log()`：player 選擇之後，計算下一張 lottery 會用到的值，並且紀錄下來
# + `output_exp_result()`：實驗結束後，把 $x^+_1$ 到 $x^+_8$、$x^-_1$ 到 $x^-_8$ 儲存起來，並以 1d vector 的形式輸出
# + `show_task_log()`：輸出 `task_log`，輸出格式是 list of vector
# + `show_lotteries_box()`：輸出 `lotteries_box`，輸出格式是 `dim = 2` 的 list

# 2. Define `experiment()` ------------------------------------------------


## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。

experiment = function(params, #alpha, beta, lambda, wp, wn,
                      phi, u_func = c("CRRA", "CARA"),
                      task_log=FALSE,
                      lotteries_box=FALSE) {  # Given player's attribute
  u_func <- match.arg(u_func)
  # Initialization
  player <- Player$new(params, phi, u_func)
  game <- Game$new()
  
  # Start the experiment
  for (trial in 1:game$show_setting()[1]) {
    for (task in 1:game$show_setting()[2]) {
      cur_lotteries <- game$generate_lotteries(trial, task)
      player$input_choice(cur_lotteries)
      game$update_lotteries_box(cur_lotteries, trial, task)
      game$update_task_log(cur_lotteries$result, trial, task)
    }
  }
  
  # Finish the experiment
  exp_result <- game$output_exp_result()
  if (task_log | lotteries_box) {
    exp_result <- list(exp_result)
    if (task_log) {
      exp_result <- append(exp_result, list(game$show_task_log()))
    }
    if (lotteries_box) {
      exp_result <- append(exp_result, list(game$show_lotteries_box()))
    }
  }
  return(exp_result)
}

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

utility <- function(x, a, b, lambda, type = c("CRRA", "CARA")){
  type <-  match.arg(type)
  ss <-  switch(type,
              "CRRA" = CRRA(x, alpha = a, beta = b, lambda= lambda),
              "CARA" = CARA(x, mu = a, nu = b, lambda= lambda))
  return(ss)
}

# Playground --------------------------------------------------------------

param = c("alpha"=.88, "beta"=.88, "lambda"=2.25, "wp"=.5, "wn"=.5)
# experiment(param, phi=NA, u_func = "CRRA", T, T)
# experiment(.88, .88, 2.25, phi = NA, u_func = "CRRA", F, F)
# 
# curve(utility(x, .88, .88, 2.25, "CRRA"), from =-5, to = 5, 
#       lwd= 2, col = "lightblue")
# curve(utility(x, .1, .1, 2.25, "CARA"), from =-5, to = 5 ,lwd= 2,lty=2, col = "pink", add = T)
# 
# points(0,0, pch=19, col = "red")
# 
