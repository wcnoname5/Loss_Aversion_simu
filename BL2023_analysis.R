library(readxl)
library(tidyverse)
# dir_name <- "./ASA_simulation_RDS"
pic_path <- "./simulation_Rmds/figs"
BL2023_raw <- read_excel(
  "E:/Proj/Loss_Aversion_simu/BL2023_data/Large_Stakes_Experiment_Data.xlsx", 
  sheet = "Risk"
  )

# check violations of Stochastic Dominance
BL2023 <- BL2023_raw %>%
  rowwise() %>%
  mutate(violation = is.unsorted(c(`X5-`, `X4-`, `X3-`,`X2-`,`X1-`, `0`,
                                   `X1+`, `X2+`, `X3+`,`X4+`,`X5+`))) %>% 
  ungroup() %>% 
  filter(!violation)

cat("Original observaions:", nrow(BL2023_raw),
    "; Obs exclude Violations of Stochastic Dominance:", nrow(BL2023), "\n")

trimmed_obs <- BL2023 %>% 
  mutate(x3p_diff = `X3+` - `X3+repeat`) %>%
  filter(
    x3p_diff > quantile(x3p_diff, 0.025),  # Keep rows above the 5th percentile
    x3p_diff < quantile(x3p_diff, 0.975)   # Keep rows below the 95th percentile
  )

var_diff <- trimmed_obs
  pull(x3p_diff) %>% 
  var()

# Error variance estimate = 1/2 difference variance estimate 
var_e <- var_diff/2
sd_e <- sqrt(var_e)
cat("SD estimate:", sd_e, "\n")
# Moment Estimate of b param. in Laplace Distribution
b <-  (var_e/2) |> sqrt()

dlaplace <- function(x, mu=0, b=1) {
  (1/(2*b)) * exp(-abs(x - mu)/b)
}

BL2023 %>%
  mutate(x3p_diff = `X3+` - `X3+repeat`) %>%
  ggplot(aes(x = x3p_diff)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  # normal curve
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = sd_e),
                color = "cyan4", linetype = "twodash",
                alpha = 0.7,
                linewidth = 1) +
  ggtitle("BL2023, Risk Data (Large stakes)") +
  theme_bw() +
  labs(x = "Difference", y = "Density")
ggsave(filename = "Appendix_BL2023difference.pdf",
       path = pic_path,
       width = 6,
       height = 5,
       units = "in",
       device = "pdf")
