This repository contains the code to fit a p-curve to the results of Clarke et al. (2025)

Here is the link to the Google doc that we'll use for keeping track of the project organization: https://docs.google.com/document/d/1clkeVn67dqRXtTqbZvx74JBFI9EQF0QWn0f4JlxoaLc/edit?usp=sharing

Note: The **p-value selection strategies** are: 
1: smallest, 2: smallest significant, 3: first significant

## Running the simulations

All simulation code is in the subfolder `simulations` (which is not copied when the `fitPCurve` package is installed).
Working directory should be the top directory.

Each simulation gets a name, e.g. `"sim_realistic"`. The simulations of the multDV condition create a subfolder `sim_realistic` that contains a hash tree. The reason is that thousands of intermediate files are created, and the computer gets unresponsive when thousands of files are stored in one folder. Each simulation condition gets a unique hash value that is used to build a tree with two hierarchical levels. This way only a few files are in each subfolder.
The simulation function checks whether a condition already has been simulated and is stored on the drive; in this case it skips this condition. Hence, if you want to recompute a simulation, you need to delete the respective subfolder `sim_realistic` (or whatever it is called).

Once the computation has finished, the results are stored in the `sim_realistic.csv` file in the `/simulations` folder and the subfolder can be deleted.

## Start the Shiny App

```r
source("simulations/ShinyApp.R", echo=TRUE)
```