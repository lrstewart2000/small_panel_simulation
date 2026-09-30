# Evaluating Modern Policy Evaluation Methods with Realistic Data

Replication code for [**"Small Samples and Short Panels: Evaluating Modern Policy Evaluation Methods with Realistic Data"**](https://arxiv.org/abs/2608.01537) by Stewart, Hettinger, Lee, and Mitra (2026).
The paper evaluates the performance of synthetic difference-in-differences and augmented synthetic controls in realistic, small-sample settings, by employing a calibrated simulation strategy that allows the injection of a known treatment effect into existing data in a setting of interest. This repository contains our simulation code, including that used to create the tables in our paper.

## Repository structure

```
.
├── small_panel_simulation.Rproj      # Open this to set the project root
├── README.md
├── data/
│   └── raw/
│       └── README.md         # Instructions for downloading the raw data
├── scripts/
│   ├── 01_calibrate_sim.R
│   ├── 02_run_aggregate_sim.R
│   ├── 03_summarize_sim.R
│   ├── 04a_table_results.R
│   ├── 04b_table_results_part_pool.R
│   ├── ar_helper_functions.R
│   ├── sim_helper_functions.R
│   └── parsed_fwf_spec.R
└── out/                      # Created by the scripts; not tracked by git
    ├── tables/
    └── results/
```

## Requirements

### Software

- R version 4.4.1 was used. Earlier versions of 4.x should also be suitable.

### R packages

Install the CRAN packages with:

```r
install.packages(c(
  "here", "dplyr", "tidyr", "readr", "tibble", "stringr", "purrr",
  "pracma", "sandwich", "mvtnorm", "kableExtra", "synthdid", "MASS"
))
```

`augsynth` (and, depending on the version you use, `synthdid`) is not on CRAN and is installed from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ebenmichael/augsynth")
# remotes::install_github("synth-inference/synthdid")  # if not installing from CRAN
```

Full list of packages used: `here`, `dplyr`, `tidyr`, `readr`, `synthdid`, `tibble`, `MASS`, `pracma`, `parallel`, `sandwich`, `mvtnorm`, `augsynth`, `stringr`, `purrr`, `kableExtra`.

## Data

The raw YRBSS data are too large to include in this repository, but they are publicly available. See [`data/raw/README.md`](data/raw/README.md) for detailed instructions. CPS data is included in the `synthdid` package.

## Running the code

1. Clone this repository and open `small_panel_simulation.Rproj` in RStudio. (If you are not using RStudio, run all scripts from the repository root.)
2. Install the required packages (see above).
3. Download the raw data into `data/raw/` (see above).
4. Run the scripts in numerical order:

```bash
Rscript scripts/01_calibrate_sim.R
Rscript scripts/02_run_aggregate_sim.R
Rscript scripts/03_summarize_sim.R
Rscript scripts/04a_table_results.R
Rscript scripts/04b_table_results_part_pool.R
```

Output directories (e.g., `out/tables/`) are created automatically by the scripts if they do not already exist.

### Script descriptions

| Script | Description | Depends on |
|--------|-------------|------------|
| `01_calibrate_sim.R` | Estimates simulation parameters from YRBSS and CPS data and saves them for use in `02_run_aggregate_sim.R`. | `ar_helper_functions.R`, `parsed_fwf_spec.R` |
| `02_run_aggregate_sim.R` | Executes the main simulation loop using the parameters estimated in `01_calibrate_sim.R`. | `ar_helper_functions.R`, `sim_helper_functions.R` |
| `03_summarize_sim.R` | Computes summary statistics from individual simulation runs, organized by specification and parameters. | Output of `02_run_aggregate_sim.R` |
| `04a_table_results.R` | Constructs LaTeX-formatted tables of simulation results for the main text. | Output of `03_summarize_sim.R` |
| `04b_table_results_part_pool.R` | Constructs LaTeX-formatted tables of simulation results for the appendix. | Output of `03_summarize_sim.R` |

### Helper files

These are sourced by the numbered scripts and are not run directly.

| File | Description |
|------|-------------|
| `ar_helper_functions.R` | Functions for estimating autoregressive parameters from panel data. |
| `sim_helper_functions.R` | Functions for simulating data and computing treatment effect estimates. |
| `parsed_fwf_spec.R` | Fixed-width file specification for use with `readr::read_fwf()`. |

## Outputs

| Output | Produced by | Location | Corresponds to |
|--------|-------------|----------|----------------|
| Calibrated parameters | `01_calibrate_sim.R` | `data/temp/calibrated_params_cps.rds` | |
| | | `data/temp/calibrated_params_soda.rds` | |
| | | `data/temp/outcome_sds.rds` | |
| Simulation results | `02_run_aggregate_sim.R` | `out/results/sim_results.rds` | |
| Summary statistics | `03_summarize_sim.R` | `out/results/summarized_results.rds` | |
| Main-text tables | `04a_table_results.R` | `out/tables/main_[table_name].tex` | Tables 1-3 in the paper |
| Appendix tables | `04b_table_results_part_pool.R` | `out/tables/part_pool_[table_name].tex` | Tables B1-B3 in the appendix |

The outputted `.tex` files were combined, and additional LaTeX formatting was added to finalize the tables used in the paper.

## Computing environment and run time

Main simulations were run on a Linux computing cluster with 50 cores, with a run time of approximately 5 days. This includes more specifications than were included in the paper, but even just the results in the paper are very computationally intensive. `02_run_aggregate_sim.R` uses the `parallel` package for parallelization. Seeds are set within the scripts here. 
