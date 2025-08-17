Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
================
Steven P. Sanderson II, MPH - Date:
17 August, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 149,527
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2025-08-15 21:58:40, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.735401^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 149527        |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |    108515 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    108515 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    108515 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       61 |          0 |
| country       |     12757 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-08-15 | 2023-08-17 | 1727 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1130220.0 | 1505958.05 | 355 | 14701 | 305638 | 2366117 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11247.9 | 21571.32 | 1 | 280 | 3064 | 12131 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-08-15 21:58:40 | 2023-08-17 16:23:50 | 92759 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 20S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -147.53  -36.13  -11.24   26.71  817.50 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.931e+02  6.376e+01
    ## date                                                1.174e-02  3.376e-03
    ## lag(value, 1)                                       1.050e-01  2.385e-02
    ## lag(value, 7)                                       9.311e-02  2.471e-02
    ## lag(value, 14)                                      8.326e-02  2.475e-02
    ## lag(value, 21)                                      6.580e-02  2.482e-02
    ## lag(value, 28)                                      6.815e-02  2.471e-02
    ## lag(value, 35)                                      6.473e-02  2.478e-02
    ## lag(value, 42)                                      5.930e-02  2.487e-02
    ## lag(value, 49)                                      6.467e-02  2.476e-02
    ## month(date, label = TRUE).L                        -9.355e+00  5.111e+00
    ## month(date, label = TRUE).Q                         2.608e+00  5.003e+00
    ## month(date, label = TRUE).C                        -1.415e+01  5.107e+00
    ## month(date, label = TRUE)^4                        -6.623e+00  5.078e+00
    ## month(date, label = TRUE)^5                        -1.009e+01  5.058e+00
    ## month(date, label = TRUE)^6                        -3.810e+00  5.133e+00
    ## month(date, label = TRUE)^7                        -8.181e+00  5.015e+00
    ## month(date, label = TRUE)^8                        -4.235e+00  5.004e+00
    ## month(date, label = TRUE)^9                         5.663e+00  5.001e+00
    ## month(date, label = TRUE)^10                        4.509e+00  4.964e+00
    ## month(date, label = TRUE)^11                       -2.109e+00  4.854e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.162e+01  2.283e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.858e+00  2.396e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -3.028 0.002498 ** 
    ## date                                                 3.478 0.000519 ***
    ## lag(value, 1)                                        4.403 1.13e-05 ***
    ## lag(value, 7)                                        3.768 0.000170 ***
    ## lag(value, 14)                                       3.363 0.000787 ***
    ## lag(value, 21)                                       2.651 0.008094 ** 
    ## lag(value, 28)                                       2.758 0.005874 ** 
    ## lag(value, 35)                                       2.612 0.009072 ** 
    ## lag(value, 42)                                       2.384 0.017238 *  
    ## lag(value, 49)                                       2.612 0.009079 ** 
    ## month(date, label = TRUE).L                         -1.830 0.067362 .  
    ## month(date, label = TRUE).Q                          0.521 0.602263    
    ## month(date, label = TRUE).C                         -2.771 0.005647 ** 
    ## month(date, label = TRUE)^4                         -1.304 0.192398    
    ## month(date, label = TRUE)^5                         -1.994 0.046316 *  
    ## month(date, label = TRUE)^6                         -0.742 0.457997    
    ## month(date, label = TRUE)^7                         -1.631 0.103031    
    ## month(date, label = TRUE)^8                         -0.846 0.397469    
    ## month(date, label = TRUE)^9                          1.132 0.257679    
    ## month(date, label = TRUE)^10                         0.908 0.363787    
    ## month(date, label = TRUE)^11                        -0.435 0.663921    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.090 3.99e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.279 0.001062 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.8 on 1655 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2349, Adjusted R-squared:  0.2247 
    ## F-statistic:  23.1 on 22 and 1655 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

## NNS Forecasting

This is something I have been wanting to try for a while. The `NNS`
package is a great package for forecasting time series data.

[NNS GitHub](https://github.com/OVVO-Financial/NNS)

``` r
library(NNS)

data_list <- base_data |>
    select(package, value) |>
    group_split(package)

data_list |>
    imap(
        \(x, idx) {
            obj <- x
            x <- obj |> pull(value) |> tail(7*52)
            train_set_size <- length(x) - 56
            pkg <- obj |> pluck(1) |> unique()
#            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            seas <- t(
                sapply(
                    1:25, 
                    function(i) c(
                        i,
                        sqrt(
                            mean((
                                NNS.ARMA(x, 
                                         h = 28, 
                                         training.set = train_set_size, 
                                         method = "lin", 
                                         seasonal.factor = i, 
                                         plot=FALSE
                                         ) - tail(x, 28)) ^ 2)))
                    )
                )
            colnames(seas) <- c("Period", "RMSE")
            sf <- seas[which.min(seas[, 2]), 1]
            
            cat(paste0("Package: ", pkg, "\n"))
            NNS.ARMA.optim(
                variable = x,
                h = 28,
                training.set = train_set_size,
                #seasonal.factor = seq(12, 60, 7),
                seasonal.factor = sf,
                pred.int = 0.95,
                plot = TRUE
            )
            title(
                sub = paste0("\n",
                             "Package: ", pkg, " - NNS Optimization")
            )
        }
    )
```

    ## Package: healthyR
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 23.364506253612"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 23.364506253612"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 18.0634252460048"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 18.0634252460048"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.4272754331149"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.4272754331149"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 52.5978585765936"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 52.5978585765936"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.83684831585605"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.83684831585605"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 8.46976270133438"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 8.46976270133438"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 9.09821316752863"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 9.09821316752863"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.96181107556686"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.96181107556686"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.99619191953341"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.99619191953341"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.2316818150404"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 23 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.2316818150404"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.2092590608986"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 23 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.2092590608986"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 23 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 11.1802824988019"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 23 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 11.1802824988019"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 10.6435317753903"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 10.6435317753903"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.4054691553164"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.4054691553164"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.35960068472584"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.35960068472584"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 27.0589636588123"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 27.0589636588123"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.74601770000736"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.74601770000736"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.09675220518514"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.09675220518514"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.69640920094529"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.69640920094529"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.58427204412413"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.58427204412413"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.19324357782316"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.19324357782316"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 22.1608083730719"
    ## [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 22.1608083730719"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.0449408986718"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.0449408986718"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.13804784974537"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.13804784974537"

![](man/figures/README-nns_forecasting-8.png)<!-- -->

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL
    ## 
    ## [[8]]
    ## NULL

## Pre-Processing

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,719 × 2]> <tibble [28 × 2]> <split [1691|28]>
    ## 2 healthyR      <tibble [1,711 × 2]> <tibble [28 × 2]> <split [1683|28]>
    ## 3 healthyR.ts   <tibble [1,656 × 2]> <tibble [28 × 2]> <split [1628|28]>
    ## 4 healthyverse  <tibble [1,627 × 2]> <tibble [28 × 2]> <split [1599|28]>
    ## 5 healthyR.ai   <tibble [1,452 × 2]> <tibble [28 × 2]> <split [1424|28]>
    ## 6 TidyDensity   <tibble [1,303 × 2]> <tibble [28 × 2]> <split [1275|28]>
    ## 7 tidyAML       <tibble [911 × 2]>   <tibble [28 × 2]> <split [883|28]> 
    ## 8 RandomWalker  <tibble [333 × 2]>   <tibble [28 × 2]> <split [305|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.8581902 | 175.19202 | 0.9587821 | 173.82328 | 1.0222551 | 0.0043844 |
| healthyR.data | 2 | LM | Test | 0.9191485 | 205.44807 | 1.0268855 | 166.61880 | 1.0478243 | 0.1023859 |
| healthyR.data | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.data | 4 | NNAR | Test | 0.7279048 | 104.16847 | 0.8132254 | 187.44976 | 0.9322930 | 0.0417877 |
| healthyR | 1 | ARIMA | Test | 0.6269112 | 132.97062 | 1.0501525 | 172.60530 | 0.7604293 | 0.0025486 |
| healthyR | 2 | LM | Test | 0.5909846 | 110.67690 | 0.9899710 | 173.89895 | 0.7244624 | 0.0368116 |
| healthyR | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR | 4 | NNAR | Test | 0.5853567 | 105.66986 | 0.9805436 | 164.22187 | 0.7253907 | 0.0157543 |
| healthyR.ts | 1 | ARIMA | Test | 0.7298746 | 85.38622 | 0.9383829 | 137.83779 | 0.8578540 | 0.1469709 |
| healthyR.ts | 2 | LM | Test | 0.7115941 | 94.35316 | 0.9148802 | 116.35343 | 0.8417581 | 0.1999509 |
| healthyR.ts | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8369669 | 100.18986 | 1.0760691 | 186.52194 | 0.9651037 | 0.0846589 |
| healthyverse | 1 | ARIMA | Test | 0.7698957 | 297.01364 | 1.2163435 | 123.09754 | 0.9007167 | 0.0001280 |
| healthyverse | 2 | LM | Test | 0.7813985 | 305.90390 | 1.2345165 | 121.06251 | 0.9204640 | 0.1909049 |
| healthyverse | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyverse | 4 | NNAR | Test | 0.6970016 | 200.52538 | 1.1011794 | 132.72129 | 0.8178367 | 0.0323701 |
| healthyR.ai | 1 | ARIMA | Test | 0.8371307 | 137.19913 | 1.0512377 | 163.61450 | 0.9856141 | 0.1899907 |
| healthyR.ai | 2 | LM | Test | 0.8337634 | 129.29475 | 1.0470092 | 170.19154 | 0.9823167 | 0.1406464 |
| healthyR.ai | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ai | 4 | NNAR | Test | 0.8272318 | 132.29468 | 1.0388070 | 165.61037 | 0.9818256 | 0.0507504 |
| TidyDensity | 1 | ARIMA | Test | 0.8853190 | 65.69631 | 1.0016707 | 94.78570 | 1.0884059 | 0.0054661 |
| TidyDensity | 2 | LM | Test | 0.7749182 | 60.79404 | 0.8767606 | 75.38025 | 1.0047440 | 0.0121122 |
| TidyDensity | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| TidyDensity | 4 | NNAR | Test | 1.0254486 | 77.37701 | 1.1602165 | 128.17989 | 1.2042349 | 0.0006216 |
| tidyAML | 1 | ARIMA | Test | 0.7097639 | 339.47995 | 1.3326862 | 137.52117 | 0.8144261 | 0.0003381 |
| tidyAML | 2 | LM | Test | 0.7419336 | 380.30834 | 1.3930895 | 136.10573 | 0.8458533 | 0.2208375 |
| tidyAML | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| tidyAML | 4 | NNAR | Test | 0.7196267 | 363.43418 | 1.3512051 | 135.75280 | 0.8203410 | 0.1529985 |
| RandomWalker | 1 | ARIMA | Test | 1.1384311 | 105.76664 | 0.6348776 | 135.12707 | 1.5372913 | 0.0279998 |
| RandomWalker | 2 | LM | Test | 1.0804006 | 110.18731 | 0.6025153 | 178.51202 | 1.3227147 | 0.0098576 |
| RandomWalker | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| RandomWalker | 4 | NNAR | Test | 1.1556195 | 151.83139 | 0.6444632 | 153.12854 | 1.3617672 | 0.0004379 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         4 NNAR        Test  0.728 104.  0.813 187.  0.932 4.18e-2
    ## 2 healthyR             2 LM          Test  0.591 111.  0.990 174.  0.724 3.68e-2
    ## 3 healthyR.ts          2 LM          Test  0.712  94.4 0.915 116.  0.842 2.00e-1
    ## 4 healthyverse         4 NNAR        Test  0.697 201.  1.10  133.  0.818 3.24e-2
    ## 5 healthyR.ai          4 NNAR        Test  0.827 132.  1.04  166.  0.982 5.08e-2
    ## 6 TidyDensity          2 LM          Test  0.775  60.8 0.877  75.4 1.00  1.21e-2
    ## 7 tidyAML              1 ARIMA       Test  0.710 339.  1.33  138.  0.814 3.38e-4
    ## 8 RandomWalker         2 LM          Test  1.08  110.  0.603 179.  1.32  9.86e-3

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1691|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1683|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1628|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1599|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1424|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1275|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [883|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [305|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
