#!/usr/bin/env Rscript
# Re-run archived algorithms, adding observation only. Run from BIsection.
suppressPackageStartupMessages({library(R6); library(dplyr); library(purrr); library(stringr); library(ggplot2)})
args <- commandArgs(trailingOnly=TRUE)
N <- if (length(args)) as.integer(args[1]) else 1000L
out <- if (length(args)>1) args[2] else "analysis"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
# Archived algorithms = functions/ at commit 6ac7f3c (pre strategy-refactor API), read straight from git.
archive <- "6ac7f3c:functions"
# Remove only nonportable source() directives. Algorithm definitions are intact.
load_archive <- function(file) {
  lines <- system2("git", c("show", file), stdout=TRUE)
  lines <- lines[!grepl("^\\s*source\\(", lines)]
  eval(parse(text=lines), envir=.GlobalEnv)
}
load_archive(file.path(archive,"player_and_lotteries.R"))
load_archive(file.path(archive,"game_and_exp.R"))
load_archive(file.path(archive,"TO_exp.R"))
capture <- new.env(parent=emptyenv())
Player$set("public", "input_choice", function(lotteries, slider=FALSE, ...) {
  est_quant <- list(...)$est_quant
  if (!slider) {
    a <- private$compute_prospects(lotteries$A, private$utility_function)
    b <- private$compute_prospects(lotteries$B, private$utility_function)
    capture$trial[est_quant] <- capture$trial[est_quant]+1L
    capture$rows[[length(capture$rows)+1L]] <- c(
      task=est_quant, trial=capture$trial[est_quant], delta=a-b,
      p_higher=plogis(private$phi*abs(a-b)))
  }
  choice <- private$choose(lotteries, private$utility_function, slider, est_quant=est_quant)
  lotteries$update_result(choice)
}, overwrite=TRUE)
params <- list(alpha=.88,beta=.88,lambda=2.25,wp=.5,wn=.5)
base_exp <- list(init_values=list(G=2000L,g=300L,l=-300L,`x1+`=1000L),
  random_init=FALSE,fix_bnd_width=FALSE,n_est=15L,min_step=1L,step_size=320L,early_stop=10L)
methods <- c(BiM="Bisection",`BiM-S`="Bisection-Slider",HaB="SimpBisection",MOBS="MOBS",ASA="ASA",PEST="PEST")
grid <- expand.grid(method=names(methods),phi=c(.367,.0367,.00367),stringsAsFactors=FALSE)
run_condition <- function(j) {
  method <- grid$method[j]; phi <- grid$phi[j]
  seed <- 20261026L+j
  set.seed(seed)
  ep <- base_exp
  ep$random_init <- method %in% c("HaB","ASA","PEST")
  ep$fix_bnd_width <- method %in% c("BiM","BiM-S","MOBS")
  reps <- vector("list",N)
  for (r in seq_len(N)) {
    capture$rows <- list(); capture$trial <- integer(15L)
    invisible(experiment(params,ep,phi,est_type=unname(methods[method])))
    rows <- as.data.frame(do.call(rbind,capture$rows))
    # Standard sequence comprises tasks 2,3,6:15, excluding L,L2,G2.
    rows <- rows[rows$task %in% c(2,3,6:15),]
    reps[[r]] <- rows %>% group_by(trial) %>% summarise(
      mean_p=mean(p_higher), n_points=n(),.groups="drop") %>% mutate(replication=r)
  }
  cat(sprintf("Completed %s phi=%g N=%d seed=%d\n",method,phi,N,seed))
  bind_rows(reps) %>% mutate(method=method,phi=phi,seed=seed)
}
# Independent conditions have explicit seeds, so fork order cannot alter output.
# Note: windows does not support mc.cores > 1
n_cores <- ifelse(.Platform$OS.type == "windows", 1L, 3L)
results <- parallel::mclapply(seq_len(nrow(grid)),run_condition,mc.cores=n_cores)
if (any(vapply(results,inherits,logical(1),"try-error"))) stop("A simulation condition failed")
replications <- bind_rows(results)
summary <- replications %>% group_by(method,phi,trial) %>% summarise(
  mean_probability=mean(mean_p), monte_carlo_se=sd(mean_p)/sqrt(n()),
  n_replications=n(), n_point_trials=sum(n_points),
  min_points_per_replication=min(n_points),max_points_per_replication=max(n_points),.groups="drop")
stopifnot(all(is.finite(summary$mean_probability)), all(summary$mean_probability>=.5),all(summary$mean_probability<=1))
write.csv(summary,file.path(out,"phi-trajectories.csv"),row.names=FALSE)
saveRDS(replications,file.path(out,"phi-replications.rds"))
gap <- abs(.5*2000^.88-.5*2.25*2000^.88)
cal <- data.frame(phi=c(.367,.0367,.00367),absolute_CPT_gap=gap,
  error_probability=plogis(-c(.367,.0367,.00367)*gap))
write.csv(cal,file.path(out,"first-trial-calibration.csv"),row.names=FALSE)
summary$method <- factor(summary$method,levels=names(methods))
summary$sensitivity <- factor(summary$phi,levels=c(.367,.0367,.00367),
 labels=c("High: phi = 0.367","Moderate: phi = 0.0367","Low: phi = 0.00367"))
p <- ggplot(summary,aes(trial,mean_probability,color=sensitivity,linetype=sensitivity))+
  geom_hline(yintercept=.5,linetype="dotted",color="grey45")+
  geom_line(linewidth=.65)+geom_point(size=1.2)+facet_wrap(~method,ncol=3)+
  scale_color_manual(values=c("#0072B2","#D55E00","#009E73"))+
  scale_x_continuous(breaks=c(1,3,5,7,10),limits=c(1,10))+
  scale_y_continuous(limits=c(.48,1.01),breaks=c(.5,.6,.7,.8,.9,1))+
  labs(x="Binary-choice trial within each elicitation",y="Mean probability of choosing the higher-CPT option",color=NULL,linetype=NULL)+
  theme_bw(base_size=10)+theme(legend.position="bottom",legend.text=element_text(size=8),panel.grid.minor=element_blank())
ggsave(file.path(out,"phi-trajectories.pdf"),p,width=7.1,height=4.7)
ggsave(file.path(out,"phi-trajectories.png"),p,width=7.1,height=4.7,dpi=180)
writeLines(capture.output(sessionInfo()),file.path(out,"R-session-info.txt"))
cat("Endpoints:\n"); print(as.data.frame(summary[summary$trial %in% c(1,10),c("method","phi","trial","mean_probability","n_replications","n_point_trials")]),row.names=FALSE)
