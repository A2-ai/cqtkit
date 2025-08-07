# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**cqtkit** is an R package for C-QT (concentration-QT interval) analyses in clinical pharmacology. It provides functions for ECG data preprocessing, exploratory data analysis, statistical modeling, goodness-of-fit evaluation, and exposure-response predictions to streamline C-QT studies from data exploration through model validation.

## Development Commands

### Testing
```bash
# Run all tests
Rscript -e "devtools::test()"

# Run a specific test file
Rscript -e "testthat::test_file('tests/testthat/test-compute_deltas.R')"
```

### Building and Checking
```bash
# Check package
Rscript -e "devtools::check()"

# Build package
Rscript -e "devtools::build()"

# Install package locally
Rscript -e "devtools::install()"

# Load package for development
Rscript -e "devtools::load_all()"
```

### Documentation
```bash
# Generate documentation
Rscript -e "devtools::document()"
```

## Code Architecture

### Core Module Structure

The package is organized into logical modules in the `R/` directory:

- **`preprocess.R`**: Data preprocessing functions for computing baseline-corrected ECG parameters (QTcB, QTcF, deltaQTcF, deltaQTcB, deltaHR)
- **`eda.R`**: Exploratory data analysis functions for C-QT analyses including drug effect plots, QTc correction comparison, hysteresis detection, and linearity assessment
- **`fit.R`**: Model fitting functions, particularly the prespecified linear mixed-effects model from the scientific white paper
- **`compute.R`**: Core computation functions for statistics, parameters, and data transformations 
- **`gof.R`**: Goodness-of-fit evaluation functions including residual plots, VPC plots, and concordance plots
- **`predict.R`**: Exposure-response prediction functions for computing QTc predictions at different concentrations
- **`tabulate.R`**: Table generation functions using the `gt` package for formatted output
- **`style.R`**: Plot styling and theming functions for consistent visualization
- **`plot-helpers.R`**: Helper functions for plot generation and customization
- **`data.R`**: Data documentation and example datasets
- **`helper.R`**: General utility functions
- **`utils-pipe.R`**: Pipe operator utilities

### Key Dependencies

- **Core R packages**: `dplyr`, `ggplot2`, `gt`, `nlme`, `MASS`
- **Statistical modeling**: `nlme` for mixed-effects models, `contrast` for contrasts
- **Visualization**: `ggplot2`, `ggpubr`, `scales` for plotting
- **Data manipulation**: `dplyr`, `purrr`, `tibble`, `forcats`
- **Tables**: `gt` for formatted table output

### Testing Framework

Uses `testthat` framework with tests organized in `tests/testthat/`. Test files follow the naming convention `test-[function_name].R` and cover:

- Data preprocessing computations
- Model fitting and parameter estimation
- Statistical calculations and transformations
- Plot generation functions

### Data Requirements

Functions expect datasets with standard C-QT column names:
- ECG parameters: `QT`, `QTBL`, `RR`, `RRBL`, `HR`, `HRBL`
- Study design: `ID`, `TRTG`, `DOSE`, `DOSEF`, `NTLD`, `TAFD`, `CONC`
- Computed: `QTcB`, `QTcF`, `deltaQTcF`, `deltaQTcB`, `deltaHR`