# packages
library(R6)
library(tidyverse)
# 1. Defining classes (Player, lottery, games) -------------------------------

### 1.1 Player ----

Player <- R6Class("Player",
  private = list(
    # Attributes
    params = list(), # list about utility functions
    phi = NA, # choice consistency
    utility_function = NA,
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
      ## TODO other than difference?
      prospect_dif <- prospect.B-prospect.A
      
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
                                    private$params, type = type)
       } else {  # 50% 
        gain_prospect <- utility(lottery[1],
                                 private$params, type = type)
        loss_prospect <- utility(lottery[2],
                                 private$params, type = type)
        ## TODO: w(.5) = .5 quite doubt
        mixed_prospect <- 
          (private$params[["wp"]] * gain_prospect) + 
          (private$params[["wn"]] * loss_prospect)  # assume no weighting function
      }
      return(mixed_prospect)
    }
  ),
  public = list(
    initialize = function(params, #include (alpha, beta, lambda, wp, wn),
                          phi,
                          utility_function = c("CRRA","CARA")) {
      private$params <- params
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


### 1.2 Lotteries ----
Lotteries <- R6Class("Lotteries",
    private = list(
     # Attributes
     .A = rep(0L, 2), # Lottery 1
     .B = rep(0L, 2), # Lottery 2
     .result = "",
     # Methods
     new_lotteries = function(game, lottery_values) {
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
     initialize = function(game, lottery_values) {
       # given_values <- c(x(i-1)pos, xipos_(j-1), L2, G2)
       private$new_lotteries(game, lottery_values)
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



###1.3 Game----
Game <- R6Class(
  "Game",
  # private
  private = list(
    # Attributes
    init_values = # G, L_init, g, l, x1pos
      c("G"= 2000L, "L"=-2000L, "g"=300L, "l"=-300L, "x1+"=1000L),  
    n_trial = 3L,  #no. of estimation. 19L = 5+ 7 + 7
    n_task = 5L, # times of bisection
    task_log = NA,  # list of vectors, should be initialized
    lotteries_box = NA,  # 2D list, should be initialized
    scroll = FALSE, # using scroll bar after `n_trial` bisections
    # Methods
    ## bisection
    bisection_update = function(choice, value_list, cur_task, cur_trial,
                                scroll=FALSE) {
      # choice <- "A" or "B"
      # value_list: e.g., x1pos_list
      # TODO: modify scroll bar
      last_est <- value_list[cur_task] #x_{i-1}
      if  (scroll & cur_task == private$n_task){
        interv <-  c(last_est, 0)# interval
        traget <- runif(1, interv[0], interv[1]) # a probability generator
      }else{
        # TODO: modify biesction s.t. starting value affect less
        step <- abs(value_list[1]) # x_0 
        # upbd <- 2000 # upper bound of step
        # step <- max(abs(value_list[1]), upbd) 
        change <- ((1 / 2) ^ cur_task) * step
        
        if (cur_trial == 1) {  # estimate L
          change <- -change
        }
        
        if (choice == "A") {
          target <- last_est + change
        } else {
          target <- last_est - change
        }
        
        target <- as.integer(round(target / 5) * 5)  # Make target divisible by 5
      }
        return(target) # return bisection iteration value
    }
    ## PEST
  ),
  ## Public
  public = list(
    initialize =
      function(exp_params =
                 list(n_task=5L,
                      init_values =
                        c("G"= 2000L, "L"=-2000L,
                          "g"=300L, "l"=-300L, "x1+"=1000L)
                 ),
               scroll = FALSE) {
        n_trial <- private$n_trial
        n_task <- exp_params$n_task
        private$n_task <- n_task
        private$init_values <- exp_params$init_values
        private$scroll <- scroll
        
        # Initialize task_log
        private$task_log <- matrix(NA, ncol = n_trial, nrow = n_task + 1L,
                                   dimnames = list(c(), c("L", "x1pos","x1neg")))
        private$task_log[1, 1] <- private$init_values["L"] # -2000
        private$task_log[1, 2] <- private$init_values["x1+"] # 1000
        
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
      G <- private$init_values["G"]
      if (cur_trial == 1) {  # L
        .A <- c(G, self$show_task_log()[cur_task, "L"])
        .B <- rep(0L,2)
      } else if (cur_trial == 2) {  # x1pos
        .A <- c(G, 0)
        .B <- rep(self$show_task_log()[cur_task, "x1pos"], 2)
      } else if (cur_trial == 3) {  # x1neg
        L <- self$show_task_log()[private$n_task, "L"]
        .A <- c(0, L)
        .B <- rep(self$show_task_log()[cur_task, "x1neg"], 2)
      }
      
      lottery_values <- list(".A" = .A, ".B" = .B)
      new_lotteries <- Lotteries$new(self, lottery_values)
      return(new_lotteries)
    },
    
    update_lotteries_box = function(lotteries, cur_trial, cur_task) {
      private$lotteries_box[[cur_trial, cur_task]] <- lotteries
    },
    
    update_task_log = function(choice, cur_trial, cur_task) {
      # The function takes the choice of players, does the computation, and update the task log.
      # Update the current value
      target_list <- private$task_log[, cur_trial]
      value <- private$bisection_update(choice, target_list, cur_task, cur_trial, scroll=FALSE) 
      private$task_log[cur_task + 1, cur_trial] <- value
      
      # Update next trial's initial value
      if (cur_task == private$n_task) {  # Updating values after the final task of this trial
        # g <- private$init_values["g"]
        # l <- private$init_values["l"]

        if (cur_trial == 2) {  #end of x1+, initialize x1neg[1]
          L <- private$task_log[(private$n_task+1), "L"]
          private$task_log[1, "x1neg"] <-
            as.integer(round(floor(L * .5) / 5) * 5)  
        } else if (cur_trial == 3) {  # x1neg, initialize L2 & G2
          # x1pos <- private$task_log[(private$n_task+1), "x1pos"]
          # x1neg <- private$task_log[(private$n_task+1), "x1neg"]
        }
      }
    },
    output_exp_result = function() {
      len <- length(private$task_log[,1])
      result <- rep(NA, 2)
      result[1] <- private$task_log[(private$n_task+1), "x1pos"] #x1+
      result[2] <- private$task_log[(private$n_task+1), "x1neg"] #x1-
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
                      exp_params=list(n_task = 5L,
                                      init_values =
                                        c("G"= 2000L, "L"=-2000L,"g"=300L, "l"=-300L, "x1+"=1000L)
                                      ),
                      phi, u_func = c("CRRA", "CARA"),
                      task_log=FALSE,
                      lotteries_box=FALSE) {  # Given player's attribute
  u_func <- match.arg(u_func)
  # Initialization
  player <- Player$new(params, phi, u_func)
  game <- Game$new(exp_params=exp_params)
  
  # Start the experiment
  for (trial in 1:game$show_setting()[1]) { # no. of estimated quantities
    for (task in 1:game$show_setting()[2]) { # no. of bisection
      cur_lotteries <- game$generate_lotteries(trial, task) # (->Lottery class object)
      player$input_choice(cur_lotteries) # update lottery by player's choice
      game$update_lotteries_box(cur_lotteries, trial, task)
      game$update_task_log(cur_lotteries$result, trial, task) # bisection
    }
  }
  
  # Finish the experiment
  exp_result <- game$output_exp_result()
  if (task_log | lotteries_box) {
    exp_result <- list("estimates" = exp_result)
    if (task_log) {
      exp_result$log <- game$show_task_log()
    }
    if (lotteries_box) {
      exp_result$lotteries <- game$show_lotteries_box()
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
              "CRRA" = CRRA(x, alpha = alpha, beta = beta, lambda= lambda),
              "CARA" = CARA(x, mu = mu, nu = nu, lambda= lambda))
  return(ss)
}

# Playground --------------------------------------------------------------

# param = c("alpha"=.88, "beta"=.88, "lambda"=2.25, "wp"=.5, "wn"=.5)
# experiment(param, phi=NA, u_func = "CRRA", T, T)
# experiment(.88, .88, 2.25, phi = NA, u_func = "CRRA", F, F)
# 
# curve(utility(x, .88, .88, 2.25, "CRRA"), from =-5, to = 5, 
#       lwd= 2, col = "lightblue")
# curve(utility(x, .1, .1, 2.25, "CARA"), from =-5, to = 5 ,lwd= 2,lty=2, col = "pink", add = T)
# 
# points(0,0, pch=19, col = "red")
# 

  