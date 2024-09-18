
source(here::here("functions/game_and_exp.R"))
##
# Note: lambda = KT1979_risk
param_abd2016 <- list(
  "alpha"=.87, "beta"=.93, "lambda"=2.21, "wp"=.5, "wn"=.5
)
# find_optimal_params(elicit_x2 = TRUE)
abd_x2 <- find_optimal_params(param_abd2016, elicit_x2 = TRUE)
# abd_x2 <- find_optimal_params(elicit_x2 = TRUE)

# statrting point make the exp b/t gamble are the same
x2pos <- abd_x2["x1pos"]
x2pos_start <- abd_x2["x1pos"]+ (-300) - abd_x2["L_hw"]
# x2pos_range <- c, (x2pos_start-abd_x2["x1pos"])*2+abd_x2["x1pos"])
# x2pos_3rd_width <- (x2pos_start-abd_x2["x1pos"]*2)/4
## 3rd step approx 250
x2pos_3rdStep <- abs(x2pos_start-x2pos)*2/8
# x2neg
x2neg <- abd_x2["x1neg"]
x2neg_start <- abd_x2["x1neg"]+ (300) - abd_x2["G_hw"]
x2neg_3rdStep <- abs(x2neg_start-x2neg)*2/8

softmax_choice <- function(trueV, delta,  phi,
                    params =list("alpha"=.88, "beta"=.88,
                                 "lambda"=2.25, "wp"=.5, "wn"=.5)){
  dt <- max(abs(utility(trueV+delta, params, type = "CRRA") -
              utility(trueV, params, type = "CRRA")),
            abs(utility(trueV, params, type = "CRRA") -
              utility(trueV-delta, params, type = "CRRA")))
  # note: delta>0
  if (trueV >=0){
    1 / (1+exp(- phi*dt* params$wp))
  } else {
    1 / (1+exp(- phi*dt* params$wn))
  }
}

subject_fun <- function(phi){
  mn <-
    (softmax_choice(x2pos, x2pos_3rdStep, phi)+
       softmax_choice(x2neg, x2neg_3rdStep, phi))/2
  # mn <- softmax_choice(x2pos, x2pos_3rdStep, phi)
  abs(mn - .636)
  # abs(softmax_choice(x2neg, x2neg_3rdStep, phi) - .636)
}
optimize(subject_fun, c(1e-5,1), maximum = FALSE)
# nlm(subject_fun, p = runif(1, .5,1))
# phi be 0.043 for x2pos
# phi be 0.006 for x2pos+x2neg

