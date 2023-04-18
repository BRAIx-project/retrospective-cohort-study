## README.md


### Folder structure

```
root
|- README.md: Provides some basic information about the workflow of the simulation. (The file you are reading at the moment.)
|- renv.lock: The environment file capturing all the R packages needed for the analysis.
|- makefile: Specifies the building procedure and the dependencies.
|- setup.R: Preprocess and tidy the data for the simulation.
|- load.R: Load all the utility functions for the simulation.
|- helpers/: The folder storing the utility functions for the simulation.
|- scripts/: The folder storing the main functions for the simulation.
|- output/
 |- log/: The folder storing the output.

```


### Workflow

0. Set up the environment
    - Install R from https://cran.r-project.org/.
    - Launch R and install the `renv` package: `install.packages("renv")`.
    - Run `renv::restore()` to reproduce the environment. 

1. Preprocess and tidy the data for the simulation.

    Run `setup.R` with `TEST = TRUE` and again with `TEST = FALSE` (by changing the flag at the beginning of the file).
    These will generate `model_pred`, `accession_df` and `reader_perf` (also the `dev` version) in `output/log`.
    ```{r}
    renv::run("setup.R")
    ```

2. Compute the human reader performance from the development set

    Run `compute_reader_performance.R`. These will generate `reader_weighted_mean`, `reader_perf_by_position` in `output/log`.
    ```{r}
    renv::run("scripts/compute_reader_performance.R")
    ```

3. Find the threshold of the model that matches the human reader performance

    Run `find_threshold_replacement.R` to get `replacement_threshold.RDS`, and run `find_threshold_bandpass.R` to get `bandpass_threshold.RDS`.
    ```{r}
    renv::run("scripts/find_threshold_replacement.R")
    renv::run("scripts/find_threshold_bandpass.R")
    ```

4. Conduct simulation

    Run `run_simulation.R` to get `scenario_simulation.RDS` and `scenario_simulation_multiple.RDS`.
    ```{r}
    renv::run("scripts/run_simulation.R")
    ```
