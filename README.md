# R Simulation for Loss Aversion

**This is the `Archive` branch. The codes and the generated synthetic data correspond to those in the manuscript.**

This repository contains simulation codes and results for comparing different elicitation methods for estimating indifference points and loss aversion.

We follow the [Abdellaoui et al. (2016)](https://doi.org/10.1007/s11166-016-9234-y) paper, which proposed a non-parametric method to estimate loss aversion and indifference points as the benchmark to compare different choice-based elicitation methods. The methods we focus on are the *Bisection* method (used in the original paper) and other psychophysical methods, such as *ASA* and *PEST*.

## Structure

The repository is organized as follows:

```
Loss_Aversion_simu/
├── README.md              # This file, provides an overview of the project
├── data/                  # This folder stores our unpublished experiment data (EDP_stage1_rawdata.csv)
├── functions/             # Contains all R scripts for simulation and analysis
│   ├── data_generation/   # Scripts for generating synthetic data across two studies 
│   │   ├── Study1_simulation.R 
│   │   └── Study2_simulation.R 
│   ├── analysis_functions.R # Handy functions for simulation data analysis and plotting
│   ├── game_and_exp.R       # A `Game` object with various elicitation strategy objects
│   ├── player_and_lotteries.R # Functions to create `Player` and `Lottery` objects
│   └── TO_expe.R              # Functions to generate TO (tradeoff) experiment simulation data
├── simulation_Rmds/         # Contains several `Rmd` and `R` scripts for data analysis
├── phi_calculator.R       # A script to calculate the phi value given first-trial error rate
├── Empirical_Study.Rmd    # This Rmd file analyzes our experiment data
├── AppendixA-BL2023_analysis.R # This file re-analyzes the Bleichrodt & L’Haridon (2023) data 
```

## Steps for Running the Simulation

1. Run the `Study1_simulation.R` and `Study2_simulation.R` scripts in the `functions/data_generation/` directory to generate synthetic data for two studies. Generated data will be stored in the `simulated_data/` folder in the root directory.

2. In `simulation_Rmds/`, `Study1_latex.Rmd` and `Study2_latex.Rmd` are provided to analyze the data generated from the two studies. These scripts will produce figures (`.pdf` files) in the `Result_figs/` folder in the root directory.

## Empirical and Re-analysis Scripts

`Empirical_Study.Rmd` and `AppendixA-BL2023_analysis.R` provide analysis based on our unpublished data (`EDP_stage1_rawdata.csv` in the folder `data/`) and the data from [Bleichrodt & L’Haridon (2023)](https://doi.org/10.1017/jdm.2023.2).

- `Empirical_Study.Rmd` analyzes the `EDP_stage1_rawdata.csv` and provides classifications of loss attitude and violations in monotonicity.

- `AppendixA-BL2023_analysis.R` re-analyzes Bleichrodt & L’Haridon (2023) raw data (see [OSF](https://osf.io/4y38q/) provided by the authors) for both violations in monotonicity and the estimation of variability in the slider task.
