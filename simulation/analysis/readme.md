## README.md


### Folder structure

```
root
|- README.md: Provides some basic information about the workflow of the simulation. (The file you are reading at the moment.)
|- renv.lock: The environment file capturing all the R packages needed for the analysis.
|- main.R: The main file for the simulation.
|- helpers.R: The file storing the utility functions for the simulation.
|- data/: The folder storing the data.
|- output/: The folder storing all the output.
```


### Workflow

0. Set up the environment
    - Install R from https://cran.r-project.org/.
    - Launch R and install the `renv` package: `install.packages("renv")`.
    - Run `renv::restore()` to install all the packages needed for the analysis (as specified in `renv.lock`).


1. The main file for the simulation is `main.R`. 
    - Run the R file line by line to get the simulation results. The output are R objects, plots and generated LaTeX code. Most intermediate results are saved in the `output/` folder, and they can be loaded and examined using the `readRDS` function. The plots are for checking the simulation results, the production version as seen in the manuscript is generated using Python.

    - The example data are provided in `data/` folder (in .CSV format) and in the `output/` folder (in .RDS format). The entire example data are randomly generated and are for pipeline testing purposes only. Note that ROC curves may look rough because of the small sample size.

    - The "setup" section does not apply to the example data, because it has been processed and no further processing is needed. The "prospective" section does not apply to the example data, because the data are not prospective.

    - Note that it may take up to a day to run the simulation (on a 6-core machine with 40GB memory).
