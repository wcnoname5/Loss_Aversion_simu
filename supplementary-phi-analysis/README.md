# Reproduce the choice-probability figure

The simulation loads the archived algorithms (`player_and_lotteries.R`, `game_and_exp.R`, `TO_exp.R`) directly from this repo's git history at commit `6ac7f3c` (`git show 6ac7f3c:functions/...`), so `git` must be on PATH and the archived `TO_exp.R` needs the `furrr` package.

From `supplementary-phi-analysis/`, run:
``` bash
    Rscript scripts/phi_trajectories.R 1000 analysis
    Rscript scripts/plot_phi_trajectories.R
```

The first command produces the numerical summaries and a plot under `analysis/`.

The second command produces the figure present in the manuscript. It only changes the panel order and labels, does not change simulation order or seeds.

## Output files from `phi_trajectories.R`

- `phi-trajectories.csv`: 180 method × phi × trial summaries, with explicit denominators and Monte Carlo standard errors.
- `phi-replications.rds`: replication-level means and available-point counts, with seed and condition identifiers.
- `first-trial-calibration.csv`: exact model arithmetic for (2000, 0.5; −2000) versus zero.
- `phi-trajectories.pdf` / `.png`: figure preview (unordered).
- `phi-run.log`, `instrumentation-verification.txt`, `R-session-info.txt`: execution evidence.