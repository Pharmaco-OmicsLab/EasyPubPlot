[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![MIT license](https://img.shields.io/badge/License-MIT-984ea3.svg)](https://mit-license.org/)
[![min R](https://img.shields.io/badge/R%3E%3D-4.2.0-386cb0.svg)](https://cran.r-project.org/)
[![Journal of Proteome Research](https://img.shields.io/badge/Journal_of_Proteome_Research-10.1101/2024.11.26.625339-ef3b2c.svg)](https://pubs.acs.org/doi/10.1021/acs.jproteome.4c01068)

# `EasyPubPlot` - Easy and Publishable Ploting
An interactive, customizable, and coding-free Shiny App to easily create publishable plots for scientific papers. `EasyPubPlot` is freely available at https://pharmaco-omicslab.shinyapps.io/EasyPubPlot.

<p align="center">
  <img src="https://github.com/Pharmaco-OmicsLab/EasyPubPlot/blob/a0e562c811f639e9d732cab4096ae601818c327b/docs/Example_plots/Embed_Graphical_Abstract.png?raw=true" width="730"/>
</p>

## App Interface

![Graphical abstract Figure](https://github.com/Pharmaco-OmicsLab/EasyPubPlotdev/blob/344cd5cb996330f4139ff6c2c8221bed3da7d433/docs/README_Figures/main_UI.png?raw=true)

## Example

![Graphical abstract Figure](https://github.com/Pharmaco-OmicsLab/EasyPubPlotdev/blob/b0c65ee487d7fb007aa77c8a0746ca841506658f/docs/README_Figures/BoxPlot_screenshot.png?raw=true)

## Install and run locally

`EasyPubPlot` requires [![min R](https://img.shields.io/badge/R%3E%3D-4.2.0-386cb0.svg)](https://cran.r-project.org/)

Step 1: Check and install the necessary packages from `CRAN` to run `EasyPubPlot`.

```r
for (pkg in c("remotes", "shiny", "shinyjs", "shinyWidgets", "colourpicker", "bslib", "shinytoastr", "dplyr", "magrittr", "ggplot2", "tibble", "tidyr", "ggthemes", "BiocManager", "circlize", "ggiraph")) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
```

Step 2: Check and install the necessary packages from `BiocManager` to run `EasyPubPlot`.

```r
for (pkg in c("EnhancedVolcano", "ComplexHeatmap")) {
  if (!require(pkg, character.only = TRUE)) {
    BiocManager::install(pkg, dependencies = TRUE)
  }
}    
```

Step 3: Install `EasyPubPlot`.

```r
remotes::install_github("Pharmaco-OmicsLab/EasyPubPlot")
```

Step 4: Run `EasyPubPlot` in your computer.

Run this code
```r
library(EasyPubPlot)  
runEasyPubPlot()     
```

Or run this code to use interactive plot mode (support volcano and bubble plots)
```r
library(EasyPubPlot)  
runInteractiveEasyPubPlot()     
```

## Citation

Tien NTN, Thu NQ, Kim DH, Park S#, Long NP#. **EasyPubPlot: A Shiny Web Application for Rapid Omics Data Exploration and Visualization**. J. Proteome Res. 2025. DOI: [10.1021/acs.jproteome.4c01068](https://doi.org/10.1021/acs.jproteome.4c01068).

## Contributors

- Primary developer and GitHub maintainer: Nguyen Tran Nam Tien (current)
- Advisor and PI:  Seongoh Park, Ph.D. and Nguyen Phuoc Long, M.D., Ph.D. 

## License

This repository is licensed under the [MIT License](LICENSE).

