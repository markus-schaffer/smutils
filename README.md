<!-- badges: start -->
[![R-CMD-check](https://github.com/markus-schaffer/smutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markus-schaffer/smutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# smutils

A collection of utility functions for smart meter data processing, interpolation, and splitting total energy use.

## Installation

You can install the package from source:

```r
# Install remotes if needed
install.packages("remotes")

# Install from GitHub
remotes::install_github("markus-schaffer/smutils")
```

## Overview

This package provides functions for:
- **Time Series Interpolation**: Interpolate values
- **Data Imputation**: Impute missing values using moving averages with scaling
- **Meter Reading Processing**: Handle cumulative meter resets
- **String Matching**: Efficient C++ implementations for string completeness checks and value matching


## Main Functions

### Time Series and Interpolation

- `interpolate_linear()` - Interpolate values in time series with extrapolation
- `impute_ma()` - Impute using moving average, scaled to cumulative totals
- `impute_linear()` - Impute with linear interpolation and extrapolation

### Data Processing

- `make_cumulative()` - Convert demand values to cumulative format
- `scale_accum()` - Scale imputed values to match cumulative totals
- `reset_meter()` - Correct cumulative meter values for resets
- `reset_meter_binary()` - Check if meter has been reset
- `spms()` - Apply SPMS to demand data

### C++ Optimised Functions

- `check_completeness_list()` - Check string list completeness against reference
- `get_match_values()` - Get matching values from character vectors
- `get_match_values_list()` - Get matching values from a list of character vectors
- `rle2()` - Run-length encoding with length per element
- `fast_round_time()` - Fast time rounding with precision control
- `ma()` - Moving average imputation

## Usage Examples

```r
library(smutils)

# Check whether a meter vector contains any resets
meter_readings <- c(100, 200, 300, 50, 150)
reset_meter_binary(meter_readings)  # TRUE

# Impute missing demand values using moving average
demand <- c(10, NA, NA, 15, 20)
cumulative <- c(10, 25, 40, 55, 75)
imputed <- impute_ma(demand, cumulative)

# Interpolate missing values with extrapolation
x <- c(1, 2, NA, NA, 5, 6)
impute_linear(x)
```
## License

GPL (>= 3)
