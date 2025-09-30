# R simulation For Loss aversion

This repository contains simulation codes and results for comparing different elicitation methods for estimating indifference points and loss aversion. For a detailed description, please refer to [my thesis dissertation](https://doi.org/10.6342/NTU202501601).

We follow the [Abdellaoui et al. (2016)](https://doi.org/10.1007/s11166-016-9234-y) paper, which proposed a non-parametric method to estimate loss aversion and indifference points as the benchmark to compare different choice-based elicitation methods. The methods we focus on are the *Bisection* method (which is used in the original paper), and some other psychophysical methods, including *ASA*, *PEST* and *MOBS*.

## Structure

The repository is organized as follows:

```
Loss_Aversion_simu/
├── README.md              # This file, provides an overview of the project
├── functions/             # Contains all R scripts for simulation and analysis
│   ├── data_generation/   # Scripts for generating synthetic data across two studies 
│   │   ├── Study1_simulation.R 
│   │   └── Study2_simulation.R 
│   ├── analysis_functions.R # Handy functions for simulation data analysis and plotting.
│   ├── new_game.R         # A `Game` object with various elicitation strategy objects
│   ├── player_and_lotteries.R # Functions to create `Player` and `Lottery` objects
│   └── TOexperiment.R     # Functions to generate TO experiment simulation data
├── Analysis_Rmds/         # Contains several `Rmd` and `R` scripts for data analysis
├── phi_calculator.R       # A script to calculate the phi value given error rate
```

## Steps for Running the Simulation

1.  Run the `Study1_simulation.R` and `Study2_simulation.R` scripts in the `functions/data_generation/` directory to generate synthetic data for two studies. Generated data will be stored in the `simulated_data/` folder in the root directory.

2.  In `Analysis_Rmds/`, `Study1_latex.Rmd` and `Study2_latex.Rmd` are provided to analyze the data generated from the two studies. These scripts will produce figures (`.pdf` files) in the `Result_figs/` folder in the root directory.

3.  `Analysis_Rmds/` also contains `Empirical_Study.Rmd` and `AppendixA-BL2023_analysis.R` which provide additional analysis for empirical study data. These scripts will also produce figures (`.pdf` files) in the `Result_figs/` folder in the root directory.

## Notes:

For the current version, the `functions/` codes are greatly modified (in order to include adaptive schemes for the bisection-based methods) and the results may not match the original paper. For the original code and synthetic data reproduction, please refer to the `Archive` branch of this repository.
