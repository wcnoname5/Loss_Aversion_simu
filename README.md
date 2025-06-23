# R simulation For Loss aversion

This is the simulation codes and results for loss aversion measurement.

## Directory Structure

```
Loss_Aversion_simu/
├── README.md              # This file, provides an overview of the project
├── functions/             # Contains all R scripts for simulation and analysis
│   ├── data_genetation/   # Scripts for generating synthetic data across two studies 
│   │   ├── Study1_simulation.R 
│   │   └── Study2_simulation.R 
│   ├── analysis_fucntions.R # Handy functions for simulation data analysis and plotting.
│   ├── new_game.R         # A `Game` object with various elicitation strategy objects
│   └── player_and_lotteries.R # Functions to create `Player` and `Lottery` objects
│   └── TOexperiment.R     # Functions to generate TO experiment simulation data
├── Analysis_Rmds/         # Contains several `Rmd` and `R` scripts for data analysis
├── phi_calculator.R     # A script to calculate the phi value given error rate
```

## Steps for Running the Simulation

1.  Run the `Study1_simulation.R` and `Study2_simulation.R` scripts in the `functions/data_genetation/` directory to generate synthetic data for two studies. Generated data would store in `simulated_data/` folder in root directory.

2.  In `Analysis_Rmds/`, `Study1_latex.Rmd` and `Study2_latex.Rmd` are provided to analyze the data generated from the two studies. These scripts will produce `.pdf` formatted figures in `Result_figs/`folder in root directory.

3.  `Analysis_Rmds/` also contains `Empirical_Study.Rmd` and `AppendixA-BL2023_analysis.R` which provide additional analysis for empirical study data. These scripts would alse produce `.pdf` formatted figures in `Result_figs/`folder in root directory.


## Notes:

For current version, the `functions/` are greatly modified and the results may not match the original paper. For the original code (and data), please refer to another branch of this repository.