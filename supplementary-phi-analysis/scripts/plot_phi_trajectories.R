#!/usr/bin/env Rscript
# Presentation-only update. Never changes simulation settings, seeds, or data.
suppressPackageStartupMessages(library(ggplot2))
out <- "analysis/focused-revision"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
d <- read.csv("analysis/phi-trajectories.csv", check.names=FALSE)
keys <- c("BiM", "BiM-S", "MOBS", "ASA", "PEST", "HaB")
labels <- c("BiM_fixbnd", "BiM-S_fixbnd", "MOBS_fixbnd", "ASA_randInit", "PEST_randInit", "HaB_randInit")
stopifnot(setequal(unique(d$method), keys), nrow(d)==180L)
d$method_label <- factor(d$method, levels=keys, labels=labels)
d$sensitivity <- factor(d$phi, levels=c(.367,.0367,.00367),
  labels=c("Large: phi = 0.367", "Medium: phi = 0.0367", "Small: phi = 0.00367"))
p <- ggplot(d,aes(trial,mean_probability,color=sensitivity,linetype=sensitivity))+
  geom_hline(yintercept=.5,linetype="dotted",color="grey45")+
  geom_line(linewidth=.65)+geom_point(size=1.2)+facet_wrap(~method_label,ncol=3)+
  scale_color_manual(values=c("#0072B2","#D55E00","#009E73"))+
  scale_x_continuous(breaks=c(1,3,5,7,10),limits=c(1,10))+
  scale_y_continuous(limits=c(.48,1.01),breaks=c(.5,.6,.7,.8,.9,1))+
  labs(x="Binary-choice trial within each elicitation",
       y="Mean probability of choosing the higher-CPT option",color=NULL,linetype=NULL)+
  theme_bw(base_size=10)+theme(legend.position="bottom",
      legend.text=element_text(size=8),panel.grid.minor=element_blank())
ggsave(file.path(out,"phi-trajectories.pdf"),p,width=7.1,height=4.7)
ggsave(file.path(out,"phi-trajectories.png"),p,width=7.1,height=4.7,dpi=180)
write.csv(data.frame(method=keys,study2_label=labels),file.path(out,"method-labels.csv"),row.names=FALSE)
cat("Replotted unchanged data with Study 2 labels and panel order.\n")
