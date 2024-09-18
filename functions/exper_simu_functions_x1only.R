# packages
library(R6)
# library(tidyverse)
# 1. Defining classes (Player, lottery, games) -------------------------------

### 1.1 Player ----

Player <- R6Class("Player",
  private = list(
    # Attributes
    params = list(), # list about utility function parameters
    phi = NA, # choice consistency
    utility_function = NA,
    # Method
    choose = function(lotteries, utility_function, slider=FALSE, ...) {
      # Choose one of the lotteries depending on the player's utility function
      # Compute net utilities of the 2 lotteries
      if (!slider){ #NOT using slider, return choosen option
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
        
      }else{ # if using slider, return indifferent point 
        return(lotteries$find_optimal(params = private$params, ...)) #trial
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
    input_choice = function(lotteries, slider=FALSE,...) {
      # Send player's choice to the game
      choice <- private$choose(lotteries, private$utility_function, slider,...)
      lotteries$update_result(choice)
    }
  )
)

# `Player` 有 3 個 private attributes：`alpha`、`beta` 和 `lambda`、`phi`。  
# 有 2 個 private methods：
# 
# + `choose()`：用`compute_prospects()` 算一對彩券分別的 prospects，再依softmax做選擇
# + 若`slider`，則回傳理論值。
# + `compute_prospects()`：根據效用函數算出該彩券的 prospect
# 
# 當 A 和 B 的 prospects 相同時，player 會有 50% 選 A、50% 選 B。  
# 有 1 個 public method：`input_choice()`（把 player 的決策寫在 lotteries 上）。  


### 1.2 Lotteries ----

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
     find_optimal = function(params, trial){
       if (trial==1){
         result <- -((utility(self$A[1], params, "CRRA")*
                        (params$wp/params$wn))/params$lambda)^(1/params$beta)
         
       } else if(trial %in% c(2,3)){
         result <- params$wp * utility(self$A[1], params, "CRRA")+
           params$wn*utility(self$A[2], params, "CRRA")
         if (result>=0) result <- result^(1/params$alpha)
         else result <- -((1/params$lambda)*(-result))^(1/params$alpha)
         
       }
       return(round(result))
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
    slider = FALSE, # using slider after `n_trial` bisections
    bound = list(), # use to perform bisection
    est_type = "", 
    # Attributes only for PEST 
    last.choose = c(NA, NA, NA, NA), #last element is nearest choice
    step = 320L,
    max_step = 1280L,
    extra.step = FALSE,
    # Attribute for ASA
    m_shift = 0L, 
    choice_histroy = list(),
    converge = FALSE,
    
    # Methods
    ## bisection: suppose x in [x0, x0+delta]
    ## iterate next point by delta/(2^(j+1)) each time
    bisection_update = function(choice, #"A" or "B"/ numeric if slider used
                                bisect_log, cur_task, cur_trial,
                                phi) {
      last_est <- bisect_log[cur_task] #x_{i-1}
      # Slider
      if  (is.numeric(choice)){
        interv <- private$bound[[cur_trial]][private$n_task, ]
        int.range <- abs(interv[2]-interv[1])
        # expand interval by 3 times wider
        interv <- interv + c(-int.range, int.range)
        ## Probability generator
        if (is.na(phi)){
          target <- choice |> as.integer()
        }else{
          target <- rnorm(1, mean = choice, sd = (1/phi)*5)|>
            as.integer()
          ## boundary conditions
          if ((target >= interv[1]) & (target <= interv[2])){
            target <- target
          }else if (target < interv[1]){
            target <- interv[1]
          }else if (target > interv[2]){
            target <- interv[2]
          }
        }
      }
      # Bisection
      else{ 
        delta <- abs(private$bound[[cur_trial]][1,2] -
                       private$bound[[cur_trial]][1,1])  #compute delta
        step <- (((1 / 2) ^ cur_task) * delta) |>
          round() # update step
        
        if (cur_trial == 1) {  # estimate L
          change <- list("A" = c(0, -step), "B" = c(step, 0))
        }else{
          change <- list("A" = c(step, 0), "B" = c(0, -step))
        } 
        # update
        private$bound[[cur_trial]][cur_task+1,] <-
          private$bound[[cur_trial]][cur_task,] + change[[choice]]
        
        target <- private$bound[[cur_trial]][cur_task+1,] |> 
          mean()  
        target <- as.integer(round(target / 5) * 5)  # Make target divisible by 5
        # target <- as.integer(round(target))  # Make target integer
        # cat(c(cur_trial,cur_task), "choice:",choice, "; update:", target,"\n")
      }
        return(target) # return bisection iteration value
    },
    ## PEST
    PEST_update = function(choice,
                           last_point, cur_task, cur_trial){
      if (cur_task == 1) {
        private$last.choose <- c(NA, NA, NA, NA)
        private$extra.step <- FALSE
        private$step <- 320L #5 - 1280
        private$last.choose <- c(private$last.choose, choice)[-1]
      }
      else {
        private$last.choose <- c(private$last.choose, choice)[-1]
        # task == 2 or 3
        if (cur_task < 4) {
          if (private$last.choose[3] != private$last.choose[4]) {
            private$step <- private$step / 2
          } else if ((cur_task == 3) &&
                     (private$last.choose[2] == private$last.choose[3])&&
                     (private$last.choose[3] == private$last.choose[4])) { #Bug 
            private$step <- private$step * 2
          }
        }
        else {
          # Reversal: half the step size
          if (private$last.choose[3] != private$last.choose[4]) {
            private$step <- private$step / 2
            ## Reversal after doubled: Extra step before doubled
            if (all(private$last.choose[1:3] == "A") | all(private$last.choose[1:3] == "B")) {
              private$extra.step <- TRUE
            }
          }
          # Same direction for 3 streak: Double
          else if (all(private$last.choose[2:4] == "A") | all(private$last.choose[2:4] == "B")) {
            if (private$extra.step) {
              private$extra.step <- FALSE
            } else {
              private$step <- private$step * 2
            }
          } # else{}
        }
        private$step <- ifelse(private$step > private$max_step,
                               private$max_step,
                               private$step)
      }
      
      # Update new point
      direction <- ifelse(private$last.choose[4] == "A", 1, -1)
      if (cur_trial == 1) {  # L
        direction <- -direction
      }
      new_point <- last_point + direction * round(private$step/5)*5
      
      return(new_point)

    },
    ## ASA
    ASA_update = function(choice,
                          last_point, cur_task, cur_trial
                          ){
      ## Current response, see Treutwein, (1995)
      Zn <- ifelse(cur_trial == 1, # L
                   as.numeric(choice == "A"),
                   as.numeric(choice == "B"))
      # print(private$m_shift)
      if (cur_task == 1) {
        private$step <- 320L #c
        private$m_shift <- 0L # times of reversal
        private$last.choose <- c(choice)
      }else{
        if (choice != private$last.choose){
          private$m_shift <- private$m_shift + 1
        }
        private$last.choose <- c(choice)
      }
      new_point <- last_point - (private$step/(2+private$m_shift) *(Zn - .5))
      return(new_point)
    }
  ),
  ## Public
  public = list(
    initialize =
      function(exp_params =
                 list(n_task=5L,
                      init_values =
                        c("G"=2000L, "L"= -2000L,
                          "g"=300L, "l"=-300L, "x1+"=1000L)
                 ),
               # est_type = c("Bisection", "Bisection-Slider", "PEST"),
               est_type = c("Bisection", "Bisection-Slider", "PEST", "ASA"),
               random_init = FALSE){
        est_type <- match.arg(est_type)
        n_task <- exp_params$n_task
        n_trial <- private$n_trial
        private$est_type <- est_type
        private$n_task <- n_task
        private$init_values <- exp_params$init_values
        private$slider <- (est_type == "Bisection-Slider")
        
        # Initialize task_log
        if (est_type == "PEST") {
          G <- exp_params$init_values[["G"]]
          private$extra.step <- FALSE
          private$last.choose <-  c(NA, NA, NA, NA)
          private$step <- 320
          private$task_log <- list(
            "L" = ifelse(!random_init,-G,
                         runif(1, -2 * G, 0) |> {\(x) round(x/5)*5}()),
            "x1pos" = ifelse(!random_init,
                             (0 + (G)) / 2,
                             runif(1, 0, G) |> {\(x) round(x/5)*5}()),
            "x1neg" = c()
          )
          private$choice_histroy <- list("L" = c(),
                                         "x1pos" = c(),
                                         "x1neg" = c())
        }
        else if (est_type == "ASA"){
          G <- exp_params$init_values[["G"]]
          private$last.choose <- NA
          private$step <- 320L
          private$task_log <- list(
            "L" = ifelse(!random_init,
                         -G,
                         runif(1, -2 * G, 0) |> {\(x) round(x/5)*5}()),
            "x1pos" = ifelse(!random_init,
                             (0 + (G)) / 2,
                             runif(1, 0, G) |> {\(x) round(x/5)*5}()),
            "x1neg" = c()
          )
          private$choice_histroy <- list("L" = c(),
                                         "x1pos" = c(),
                                         "x1neg" = c())
        }
        else{ #Bisection Method
          private$task_log <-
            matrix(NA,
                   ncol = n_trial,
                   nrow = n_task + 1L,
                   dimnames = list(c(), c("L", "x1pos", "x1neg")))
          ## The bisection method: suppose x in [x0, x0+delta]
          ## delta for L is 2*G
          private$bound <- lapply(1:3, \(x)(matrix(nrow = (n_task+1),
                                                   ncol = 2)))
          names(private$bound) <- c("L", "x1pos", "x1neg")
          private$bound[["L"]][1,] <-  c(-2*private$init_values[["G"]], 0)
          private$bound[["x1pos"]][1,] <-  c(0, private$init_values[["G"]])
          #choose midpoint as start point
          private$task_log[1, 1] <- private$bound[["L"]][1,] |> mean() # -2000
          private$task_log[1, 2] <- private$bound[["x1pos"]][1,] |> mean() # 1000
        }
        
      },
    
    show_setting = function() {
      # Output how many trials and tasks there are in this game
      setting <- c(
        "N_est" = private$n_trial,
        "N_bisect" = private$n_task,
        "Init" = private$init_values,
        "est_Type" = private$est_type
      )
      return(setting)
    },
    
    # Generate a new pair of lotteries.
    generate_lotteries = function(cur_trial, cur_task) {  
      G <- private$init_values["G"]
      if (private$est_type == "PEST" | private$est_type == "ASA"){
        if (cur_trial == 1) {  # L
          .A <- c(G, self$show_task_log()[["L"]][cur_task])
          .B <- rep(0L,2)
        } else if (cur_trial == 2) {  # x1pos
          .A <- c(G, 0)
          .B <- rep(self$show_task_log()[["x1pos"]][cur_task], 2)
        } else if (cur_trial == 3) {  # x1neg
          L.vec <- self$show_task_log()[["L"]]
          L <- L.vec[length(L.vec)]
          .A <- c(0, L)
          .B <- rep(self$show_task_log()[["x1neg"]][cur_task], 2)
        }
      }
      else{
        if (cur_trial == 1) {  # L
          .A <- c(G, self$show_task_log()[cur_task, "L"])
          .B <- rep(0L,2)
        } else if (cur_trial == 2) {  # x1pos
          .A <- c(G, 0)
          .B <- rep(self$show_task_log()[cur_task, "x1pos"], 2)
        } else if (cur_trial == 3) {  # x1neg
          L <- self$show_task_log()[private$n_task+1, "L"]
          .A <- c(0, L)
          .B <- rep(self$show_task_log()[cur_task, "x1neg"], 2)
        }
      }
      
      lottery_values <- list(".A" = .A, ".B" = .B)
      new_lotteries <- Lotteries$new(lottery_values)
      return(new_lotteries)
    },
    
    update_task_log = function(choice, cur_trial, cur_task,
                               phi, random_init=FALSE) {
      # The function takes the choice of players, does the computation, and update the task log.
      if (private$est_type == "PEST" | private$est_type == "ASA"){
        last <- private$task_log[[cur_trial]][cur_task]
        if (private$est_type == "PEST"){
          value <- private$PEST_update(choice, last_point = last,
                                     cur_trial = cur_trial, cur_task = cur_task)
        }else{
          value <- private$ASA_update(choice, last_point = last,
                                       cur_trial = cur_trial, cur_task = cur_task)
        }
        private$task_log[[cur_trial]][cur_task + 1] <- value
        private$choice_histroy[[cur_trial]][cur_task] <- choice
        # print(c(cur_trial, cur_task))
        if ( (cur_task == 1) & (cur_trial == 2)){ # end of x1-, initialize x1neg[1]
          L.vec <- self$show_task_log()[["L"]]
          L <- L.vec[length(L.vec)]
          private$task_log[["x1neg"]][1] <- 
            ifelse(random_init,
                   runif(1, L, 0) |> {\(x) round(x/5)*5}(),
                   round((L+0)/2))
        }
      }else{
        # Update the current value
        bisec_pts <- private$task_log[, cur_trial]
        value <- private$bisection_update(choice, bisec_pts, cur_task, cur_trial,
                                          phi=phi) 
        private$task_log[cur_task + 1, cur_trial] <- value
        
        # Update next trial's initial value
        if (cur_task == private$n_task) {  # Updating values after the final task of this trial
          # g <- private$init_values["g"]
          # l <- private$init_values["l"]
  
          if (cur_trial == 2) {  #end of x1+, initialize x1neg[1]
            L <- private$task_log[(private$n_task+1), "L"]
            private$bound[["x1neg"]][1, ] <- c(L, 0)
            private$task_log[1, "x1neg"] <-
              as.integer(round(floor(L * .5) / 5) * 5)  
          } else if (cur_trial == 3) {  # x1neg, initialize L2 & G2
            # x1pos <- private$task_log[(private$n_task+1), "x1pos"]
            # x1neg <- private$task_log[(private$n_task+1), "x1neg"]
          }
        }
      }
    },
    output_exp_result = function() {
      result <- rep(0L, 2)
      log <- private$task_log
      if (private$est_type == "PEST" | private$est_type == "ASA"){
        # print(log)
        result[1] <- log[["x1pos"]][ length(log[["x1pos"]]) ]
        result[2] <- log[["x1neg"]][ length(log[["x1neg"]]) ]
      }
      else{
        result[1] <- log[(private$n_task+1), "x1pos"] #x1+
        result[2] <- log[(private$n_task+1), "x1neg"] #x1-
      }
      names(result) <- paste0("x1", c("+","-"))
      return(result)
    },
    reset_step = function() {
      private$step <- 320L
      private$m_shift <- 0
    } ,
    show_step = function() private$step,
    show_ASA_step = function() private$step/(2*(2+private$m_shift)),
    show_bound = function() private$bound,
    show_task_log = function() private$task_log
  )
)


# `Game` 有 5 個 private attributes：
# + `exp_params`: A list, with `n_task`, `init_value` two vectors
# + `init_value`：$G$、$L$、$g$、$l$
#   + `n_trial`
# + `n_task`
# + `task_log`: $19 \times 6$ 的 list of vectors，紀錄實驗中的所有點
# + `slider` (logical): using slider to choose indifferent point after `n_task` bisection  
# 
# 有 1 個 private method：`bisection_update()`，根據 player 的選擇來更新下一張 lotteries 所需要的數值。  
# 有 7 個 public method：
# 
# + `show_setting()`：輸出 `n_trial`、`n_task` 和 `init_value`
# + `generate_lotteries()`：根據目前的 trial 和 task 產生相對應的 lottery
# + `update_task_log()`：player 選擇之後，計算下一張 lottery 會用到的值，並且紀錄下來
# + `output_exp_result()`：實驗結束後，把 $x^+_1$ 到 $x^+_8$、$x^-_1$ 到 $x^-_8$ 儲存起來，並以 1d vector 的形式輸出
# + `show_task_log()`：輸出 `task_log`，輸出格式是 list of vector


# 2. Define `experiment()` ------------------------------------------------


## 根據 3 個 class 之間的互動，implement sequence diagram。
## 可以在 `experiment()` 分別設定要不要輸出 `task_log` 和 `lotteries_box`。

experiment = function(params, #alpha, beta, lambda, wp, wn,
                      exp_params=list(n_task = 5L,
                                      init_values =
                                        c("G"= 2000L, "L"=-2000L,"g"=300L, "l"=-300L, "x1+"=1000L)
                                      ),
                      phi, u_func = c("CRRA", "CARA"),
                      est_type = c("Bisection", "Bisection-Slider", "PEST", "ASA"),
                      task_log=FALSE, random_init = FALSE,
                      converg_crit = 5L) {  # Given player's attribute
  u_func <- match.arg(u_func)
  est_type <- match.arg(est_type)

  slider <- (est_type == "Bisection-Slider")
  # Initialization
  player <- Player$new(params, phi, u_func)
  game <- Game$new(exp_params, est_type,
                   random_init = random_init)
  # Start the experiment
  for (trial in 1:game$show_setting()[1]) { # no. of estimated quantities
    if (est_type == "PEST"){
      task <-  0L
      game$reset_step()
      while ( game$show_step() >= converg_crit) {
        task <- task + 1L
        cur_lotteries <-
          game$generate_lotteries(trial, task) # (->Lottery class object)
        # update lottery by player's choice
        player$input_choice(cur_lotteries,
                            slider = F,
                            trial = trial) 
        game$update_task_log(cur_lotteries$result, trial, task, phi = phi,
                             random_init = random_init)
      }
    }else if (est_type == "ASA"){
      # TODO
      task <-  0L
      game$reset_step()
      while ( game$show_ASA_step() >= converg_crit) {
        task <- task + 1L
        cur_lotteries <-
          game$generate_lotteries(trial, task) # (->Lottery class object)
        # update lottery by player's choice
        player$input_choice(cur_lotteries,
                            slider = F,
                            trial = trial) 
        game$update_task_log(cur_lotteries$result, trial, task, phi = phi,
                             random_init = random_init)
        }
    }else{
      for (task in 1:game$show_setting()[2]) { # no. of bisection
        cur_lotteries <- game$generate_lotteries(trial, task) # (->Lottery class object)
        # update lottery by player's choice
        player$input_choice(cur_lotteries,
                            slider =
                              (slider & task==exp_params$n_task),
                            trial = trial) 
        game$update_task_log(cur_lotteries$result, trial, task, phi = phi) # bisection
        ## debug
        # if (task == game$show_setting()[2]){
        #   print(game$show_bound()[[trial]])
        # }
      }
    }
  }
  
  # Finish the experiment
  exp_result <- game$output_exp_result()
  if (task_log) {
    exp_result <- list("estimates" = exp_result)
    exp_result$log <- game$show_task_log()
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

  