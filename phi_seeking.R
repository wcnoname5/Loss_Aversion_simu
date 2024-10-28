source(here::here("functions/player_and_lotteries.R"))
##
# Note: lambda from KT1979 definition
param_abd2016 <- list(
  "alpha"=.87, "beta"=.93, "lambda"=2.21, "wp"=.5, "wn"=.5
)
param_TK1992 <- list("alpha"=.88, "beta"=.88,
                "lambda"=2.25, "wp"=.5, "wn"=.5)


# find_optimal_params(elicit_x2 = TRUE)
abd_x2 <- find_optimal_params(param_abd2016, elicit_x2 = TRUE)

# staring point make the Expectation b/t gamble are the same
x2pos <- abd_x2["x1pos"]
x2pos_start <- abd_x2["x1pos"]+ (-300) - abd_x2["L_hw"]
x2pos_3rdStep <- abs(x2pos_start-x2pos)*2/8
# x2neg
x2neg <- abd_x2["x1neg"]
x2neg_start <- abd_x2["x1neg"]+ (300) - abd_x2["G_hw"]
x2neg_3rdStep <- abs(x2neg_start-x2neg)*2/8

softmax_choice <- function(trueV, delta,  phi,
                    params){
  dt <- 0.5 * max(abs(utility(trueV+delta, params, type = "CRRA") -
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


seek_phi <- function(params, 
                     type = c("pos", "mean", "neg")){
    type <- match.arg(type)
    
    equation <- function(phi){
      if (type == "pos"){
        mn <- softmax_choice(x2pos, x2pos_3rdStep, phi, params)
      } else if (type == "neg"){
        mn <- softmax_choice(x2neg, x2neg_3rdStep, phi, params) #
      } else {
        mn <- (softmax_choice(x2pos, x2pos_3rdStep, phi, params)+
           softmax_choice(x2neg, x2neg_3rdStep, phi, params))/2
      }
      (mn - .636)
    }
    
    result <- uniroot(equation, lower = -1, upper=1)
    
    result$root
}

seek_phi(params = param_abd2016, "mean")
seek_phi(params = param_abd2016, "neg")
seek_phi(params = param_abd2016, "pos")


# Summary -----------------------------------------------------------------

#> [U(A)-U(B)] (wrong)
# phi be 0.047 for x2pos / abd2016
# phi be 0.0046 for x2pos+x2neg / abd2016
# phi be 0.043 for x2pos / TK1992
# phi be 0.0059 for x2pos+x2neg / TK1992

#> 1/2 *[U(A)-U(B)]
# phi be 0.0945 for x2pos / abd2016
# phi be 0.0117 for x2pos+x2neg / abd2016
# phi be 0.0874 for x2pos / TK1992
# phi be 0.0118 for x2pos+x2neg / TK1992

# neg only works for TK1992
