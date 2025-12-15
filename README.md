[![DOI](https://zenodo.org/badge/956470504.svg)](https://doi.org/10.5281/zenodo.17940878)

This repository contains the code to fit a p-curve to the results of Clarke et al. (2025)

Here is the link to the Google doc that we'll use for keeping track of the project organization: https://docs.google.com/document/d/1clkeVn67dqRXtTqbZvx74JBFI9EQF0QWn0f4JlxoaLc/edit?usp=sharing

Note: The **p-value selection strategies** are: 
1: smallest, 2: smallest significant, 3: first significant

While building the simulations (before knowing the actual target p-curve by Clarke et al.), we used three existing empirical p-curves as targets:

1. "Sotola": 
    - Sotola, L. (2023). How Can I Study from Below, that which Is Above?: Comparing Replicability Estimated by Z-Curve to Real Large-Scale Replication Attempts. Meta-Psychology, 7. https://doi.org/10.15626/MP.2022.3299
    - k=155 significant p-values
2. "Wetzels":
    - Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., & Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. https://doi.org/10.1177/1745691611406923
    - k=593 significant p-values
3. "Simonsohn":
    - Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014). P-curve: A key to the file-drawer. Journal of Experimental Psychology: General, 143, 534–547, Figure 3B. http://www.p-curve.com/Supplement/full_pdt.xlsx
    - k = 22 significant p-values

These have different shapes and numbers of p-values, which is useful for testing the fitting procedure.


## Installing the package

```r
install.packages("devtools") # if not already installed
devtools::install_github("astefan1/pcurve_fitting")
```

## Running the simulations

All simulation code is in the subfolder `simulations` (which is not copied when the `fitPCurve` package is installed).
Working directory should be the top directory.

Each simulation gets a name, e.g. `"sim_realistic"`. The simulations of the multDV condition create a subfolder `/simulationssim-results/sim_realistic` that itself contains a hash tree. The reason is that thousands of intermediate files are created, and the computer gets unresponsive when thousands of files are stored in one folder. Each simulation condition gets a unique hash value that is used to build a tree with two hierarchical levels. This way only a few files are in each subfolder.
The simulation function checks whether a condition already has been simulated and is stored on the drive; in this case it skips this condition. Hence, if you want to recompute a simulation, you need to delete the respective subfolder `/simulationssim-results/sim_realistic` (or whatever it is called).
For a completely fresh run of the simulations, delete everything in the `sim-results` folder.

Once the computation has finished, the results are stored in the `sim_realistic.csv` file in the `/simulations/sim-results` folder and the subfolder can be deleted.

## Start the Shiny App

The simulation results (which are already included in the repository) can be explored with a Shiny App. To start the app, run:

```r
library(fitPCurve)
source("simulations/ShinyApp.R", echo=TRUE)
```
