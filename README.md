# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-06-08

# Introduction

This analysis follows a *Nested Modeltime Workflow* from **`modeltime`**
along with using the **`NNS`** package. I use this to monitor the
downloads of all of my packages:

- [`healthyR`](https://www.spsanderson.com/healthyR/)
- [`healthyR.data`](https://www.spsanderson.com/healthyR.data/)
- [`healthyR.ts`](https://www.spsanderson.com/healthyR.ts/)
- [`healthyR.ai`](https://www.spsanderson.com/healthyR.ai/)
- [`healthyverse`](https://www.spsanderson.com/healthyverse/)
- [`TidyDensity`](https://www.spsanderson.com/TidyDensity/)
- [`tidyAML`](https://www.spsanderson.com/tidyAML/)
- [`RandomWalker`](https://www.spsanderson.com/RandomWalker/)

## Get Data

``` r
glimpse(downloads_tbl)
```

    Rows: 180,358
    Columns: 11
    $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2026-06-06 23:24:35, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
5240.61 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 180358        |
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
| r_version     |    134674 |          0.25 |   5 |   7 |     0 |       51 |          0 |
| r_arch        |    134674 |          0.25 |   1 |   7 |     0 |        6 |          0 |
| r_os          |    134674 |          0.25 |   7 |  19 |     0 |       30 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     16986 |          0.91 |   2 |   2 |     0 |      169 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-06-06 | 2024-01-25 | 2015 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1131085.53 | 1476004.37 | 355 | 43637 | 325468.5 | 2334475 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11687.22 | 23662.48 | 1 | 174 | 2705.0 | 11844 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-06-06 23:24:35 | 2024-01-25 11:54:26 | 115117 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      9 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

    [[1]]

![](man/figures/README-initial_ts_plot-3.png)


    [[2]]

![](man/figures/README-initial_ts_plot-4.png)


    [[3]]

![](man/figures/README-initial_ts_plot-5.png)


    [[4]]

![](man/figures/README-initial_ts_plot-6.png)


    [[5]]

![](man/figures/README-initial_ts_plot-7.png)


    [[6]]

![](man/figures/README-initial_ts_plot-8.png)


    [[7]]

![](man/figures/README-initial_ts_plot-9.png)


    [[8]]

![](man/figures/README-initial_ts_plot-10.png)

Now lets take a look at some time series decomposition graphs.

    [[1]]

![](man/figures/README-ts_decomp_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-8.png)

    [[1]]

![](man/figures/README-ts_decomp_plt-9.png)


    [[2]]

![](man/figures/README-ts_decomp_plt-10.png)


    [[3]]

![](man/figures/README-ts_decomp_plt-11.png)


    [[4]]

![](man/figures/README-ts_decomp_plt-12.png)


    [[5]]

![](man/figures/README-ts_decomp_plt-13.png)


    [[6]]

![](man/figures/README-ts_decomp_plt-14.png)


    [[7]]

![](man/figures/README-ts_decomp_plt-15.png)


    [[8]]

![](man/figures/README-ts_decomp_plt-16.png)

Seasonal Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_seasonal_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_seasonal_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_seasonal_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_seasonal_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_seasonal_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_seasonal_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_seasonal_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_seasonal_plt-8.png)

ACF and PACF Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_acf_plt-1.png)


    [[2]]

![](man/figures/README-ts_decomp_acf_plt-2.png)


    [[3]]

![](man/figures/README-ts_decomp_acf_plt-3.png)


    [[4]]

![](man/figures/README-ts_decomp_acf_plt-4.png)


    [[5]]

![](man/figures/README-ts_decomp_acf_plt-5.png)


    [[6]]

![](man/figures/README-ts_decomp_acf_plt-6.png)


    [[7]]

![](man/figures/README-ts_decomp_acf_plt-7.png)


    [[8]]

![](man/figures/README-ts_decomp_acf_plt-8.png)

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.


    Call:
    stats::lm(formula = .formula, data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -150.96  -37.99  -11.71   28.52  828.47 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.572e+02  5.144e+01
    date                                                1.002e-02  2.719e-03
    lag(value, 1)                                       8.622e-02  2.230e-02
    lag(value, 7)                                       7.223e-02  2.290e-02
    lag(value, 14)                                      6.825e-02  2.277e-02
    lag(value, 21)                                      8.923e-02  2.284e-02
    lag(value, 28)                                      7.714e-02  2.275e-02
    lag(value, 35)                                      4.023e-02  2.278e-02
    lag(value, 42)                                      6.257e-02  2.288e-02
    lag(value, 49)                                      7.570e-02  2.285e-02
    month(date, label = TRUE).L                        -8.338e+00  4.738e+00
    month(date, label = TRUE).Q                         7.771e-02  4.722e+00
    month(date, label = TRUE).C                        -1.606e+01  4.722e+00
    month(date, label = TRUE)^4                        -8.404e+00  4.776e+00
    month(date, label = TRUE)^5                        -3.869e+00  4.730e+00
    month(date, label = TRUE)^6                        -1.415e+00  4.776e+00
    month(date, label = TRUE)^7                        -4.319e+00  4.708e+00
    month(date, label = TRUE)^8                        -3.450e+00  4.696e+00
    month(date, label = TRUE)^9                         2.608e+00  4.723e+00
    month(date, label = TRUE)^10                        1.249e-01  4.715e+00
    month(date, label = TRUE)^11                       -3.108e+00  4.782e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.083e+01  2.119e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  7.696e+00  2.179e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -3.057 0.002267 ** 
    date                                                 3.685 0.000235 ***
    lag(value, 1)                                        3.866 0.000114 ***
    lag(value, 7)                                        3.154 0.001637 ** 
    lag(value, 14)                                       2.997 0.002759 ** 
    lag(value, 21)                                       3.907 9.67e-05 ***
    lag(value, 28)                                       3.391 0.000711 ***
    lag(value, 35)                                       1.766 0.077513 .  
    lag(value, 42)                                       2.735 0.006294 ** 
    lag(value, 49)                                       3.313 0.000941 ***
    month(date, label = TRUE).L                         -1.760 0.078601 .  
    month(date, label = TRUE).Q                          0.016 0.986871    
    month(date, label = TRUE).C                         -3.402 0.000683 ***
    month(date, label = TRUE)^4                         -1.760 0.078611 .  
    month(date, label = TRUE)^5                         -0.818 0.413561    
    month(date, label = TRUE)^6                         -0.296 0.767096    
    month(date, label = TRUE)^7                         -0.917 0.359068    
    month(date, label = TRUE)^8                         -0.735 0.462709    
    month(date, label = TRUE)^9                          0.552 0.580818    
    month(date, label = TRUE)^10                         0.026 0.978862    
    month(date, label = TRUE)^11                        -0.650 0.515832    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.113 3.48e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.532 0.000422 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.08 on 1943 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.204, Adjusted R-squared:  0.195 
    F-statistic: 22.63 on 22 and 1943 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)

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

    Package: healthyR
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.90745982775317"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.90745982775317"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.1684323601097"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 8.1684323601097"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.78270307192378"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.78270307192378"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 4.95254607348669"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 4.95254607348669"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.3851427810383"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.3851427810383"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.8138669475875"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.8138669475875"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 18.4122316987127"
    [1] "BEST method = 'lin' PATH MEMBER = c( 10 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 18.4122316987127"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.83962398627949"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 10 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.83962398627949"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.4271632935017"
    [1] "BEST method = 'both' PATH MEMBER = c( 10 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.4271632935017"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 11.7438468091559"
    [1] "BEST method = 'lin' PATH MEMBER = c( 10 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 11.7438468091559"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.97259398233287"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 10 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.97259398233287"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 10 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.1425276664031"
    [1] "BEST method = 'both' PATH MEMBER = c( 10 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.1425276664031"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 25.7950718639939"
    [1] "BEST method = 'lin' PATH MEMBER = c( 3 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 25.7950718639939"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.44416305872466"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 3 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.44416305872466"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 3 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 8.13171325765033"
    [1] "BEST method = 'both' PATH MEMBER = c( 3 )"
    [1] "BEST both OBJECTIVE FUNCTION = 8.13171325765033"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.78334439170363"
    [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.78334439170363"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.36437149882731"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.36437149882731"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.79364504296781"
    [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.79364504296781"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 217.019666237063"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 217.019666237063"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 60.9592390402201"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 60.9592390402201"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 58.1518586604007"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 58.1518586604007"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 33.67091497442"
    [1] "BEST method = 'lin' PATH MEMBER = c( 15 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 33.67091497442"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.621410006435"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 15 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.621410006435"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.816292877293"
    [1] "BEST method = 'both' PATH MEMBER = c( 15 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.816292877293"

![](man/figures/README-nns_forecasting-8.png)

    [[1]]
    NULL

    [[2]]
    NULL

    [[3]]
    NULL

    [[4]]
    NULL

    [[5]]
    NULL

    [[6]]
    NULL

    [[7]]
    NULL

    [[8]]
    NULL

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
  group_by(package) %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  tk_augment_fourier(
    .date_var = date,
    .periods  = c(7, 14, 30, 90, 180),
    .K        = 2
  ) %>%
  tk_augment_timeseries_signature(
    .date_var = date
  ) %>%
  ungroup() %>%
  select(-c(value, -year.iso))
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

    # 0. Filter out column where package is NA
    filter(!is.na(package)) %>%
    
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

    # A tibble: 8 × 4
      package       .actual_data          .future_data       .splits          
      <fct>         <list>                <list>             <list>           
    1 healthyR.data <tibble [2,004 × 50]> <tibble [28 × 50]> <split [1976|28]>
    2 healthyR      <tibble [1,997 × 50]> <tibble [28 × 50]> <split [1969|28]>
    3 healthyR.ts   <tibble [1,933 × 50]> <tibble [28 × 50]> <split [1905|28]>
    4 healthyverse  <tibble [1,855 × 50]> <tibble [28 × 50]> <split [1827|28]>
    5 healthyR.ai   <tibble [1,739 × 50]> <tibble [28 × 50]> <split [1711|28]>
    6 TidyDensity   <tibble [1,591 × 50]> <tibble [28 × 50]> <split [1563|28]>
    7 tidyAML       <tibble [1,196 × 50]> <tibble [28 × 50]> <split [1168|28]>
    8 RandomWalker  <tibble [619 × 50]>   <tibble [28 × 50]> <split [591|28]> 

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
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
  add_recipe(recipe = recipe_date) %>%
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
| healthyR.data | 1 | ARIMA | Test | 0.6764389 | 147.79687 | 0.8009642 | 170.37485 | 0.7936494 | 0.0086718 |
| healthyR.data | 2 | LM | Test | 0.7438229 | 320.14870 | 0.8807529 | 145.98855 | 0.8762229 | 0.0275493 |
| healthyR.data | 3 | EARTH | Test | 3.5839884 | 1880.28919 | 4.2437630 | 161.34414 | 3.9952129 | 0.0890724 |
| healthyR.data | 4 | NNAR | Test | 0.7508628 | 315.17476 | 0.8890887 | 142.39328 | 0.8710216 | 0.0529344 |
| healthyR | 1 | ARIMA | Test | 0.7595969 | 103.37714 | 0.7044665 | 126.43050 | 0.9473772 | 0.0087351 |
| healthyR | 2 | LM | Test | 0.7658737 | 163.92052 | 0.7102877 | 115.52338 | 0.8943794 | 0.1213003 |
| healthyR | 3 | EARTH | Test | 0.7874757 | 93.07862 | 0.7303218 | 149.81204 | 0.9976287 | 0.0175278 |
| healthyR | 4 | NNAR | Test | 0.7498291 | 149.58517 | 0.6954076 | 121.76648 | 0.8902560 | 0.1051266 |
| healthyR.ts | 1 | ARIMA | Test | 0.7254406 | 5327.47418 | 0.7560107 | 181.24665 | 0.9755184 | 0.0545159 |
| healthyR.ts | 2 | LM | Test | 0.6839537 | 28559.43776 | 0.7127755 | 127.52456 | 0.8714182 | 0.0147288 |
| healthyR.ts | 3 | EARTH | Test | 0.6954175 | 11511.40246 | 0.7247224 | 146.24133 | 0.8469707 | 0.0004646 |
| healthyR.ts | 4 | NNAR | Test | 0.7246544 | 23736.87578 | 0.7551914 | 130.21391 | 0.8947169 | 0.0083425 |
| healthyverse | 1 | ARIMA | Test | 0.5601789 | 44.00505 | 0.8377859 | 39.71029 | 0.7037626 | 0.0647749 |
| healthyverse | 2 | LM | Test | 0.8027636 | 51.49140 | 1.2005880 | 66.85198 | 0.9568800 | 0.0247263 |
| healthyverse | 3 | EARTH | Test | 0.5246755 | 57.77308 | 0.7846882 | 33.72452 | 0.8015070 | 0.0424375 |
| healthyverse | 4 | NNAR | Test | 0.8249469 | 50.68791 | 1.2337646 | 66.44521 | 1.0031147 | 0.0002612 |
| healthyR.ai | 1 | ARIMA | Test | 0.7116391 | 97.89554 | 0.7993983 | 130.09348 | 0.8836693 | 0.0007595 |
| healthyR.ai | 2 | LM | Test | 0.6312663 | 139.28349 | 0.7091139 | 92.69268 | 0.8097772 | 0.0816917 |
| healthyR.ai | 3 | EARTH | Test | 0.8543285 | 102.91993 | 0.9596841 | 178.40615 | 1.0128528 | 0.0199367 |
| healthyR.ai | 4 | NNAR | Test | 0.5908902 | 129.92286 | 0.6637586 | 91.68455 | 0.7698612 | 0.1482381 |
| TidyDensity | 1 | ARIMA | Test | 0.9535473 | 204.56960 | 0.6480622 | 155.65245 | 1.1255805 | 0.0459492 |
| TidyDensity | 2 | LM | Test | 0.9942471 | 201.25980 | 0.6757231 | 148.80628 | 1.1642515 | 0.0097836 |
| TidyDensity | 3 | EARTH | Test | 1.0156060 | 230.10078 | 0.6902393 | 150.42983 | 1.1512405 | 0.0095626 |
| TidyDensity | 4 | NNAR | Test | 1.0080625 | 173.07647 | 0.6851125 | 150.15359 | 1.2449024 | 0.0075319 |
| tidyAML | 1 | ARIMA | Test | 0.6276080 | 120.60398 | 0.7501839 | 140.94953 | 0.7461348 | 0.0037902 |
| tidyAML | 2 | LM | Test | 0.7181980 | 142.09441 | 0.8584668 | 138.33665 | 0.8433201 | 0.0028725 |
| tidyAML | 3 | EARTH | Test | 0.7099862 | 156.66226 | 0.8486511 | 162.48615 | 0.8361755 | 0.0004124 |
| tidyAML | 4 | NNAR | Test | 0.8932117 | 337.19040 | 1.0676618 | 150.79392 | 1.0134567 | 0.0125481 |
| RandomWalker | 1 | ARIMA | Test | 0.8944643 | 99.79503 | 0.5999677 | 161.32861 | 1.0184778 | 0.2096574 |
| RandomWalker | 2 | LM | Test | 1.0154690 | 106.31147 | 0.6811323 | 174.51459 | 1.1483161 | 0.0127671 |
| RandomWalker | 3 | EARTH | Test | 0.9940811 | 105.65668 | 0.6667862 | 182.17514 | 1.1104719 | 0.0023668 |
| RandomWalker | 4 | NNAR | Test | 1.2007988 | 147.95257 | 0.8054435 | 180.00105 | 1.3396718 | 0.2720340 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  facet_wrap(~ package, nrow = 3) +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 10
      package     .model_id .model_desc .type   mae   mape  mase smape  rmse     rsq
      <fct>           <int> <chr>       <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.d…         1 ARIMA       Test  0.676 1.48e2 0.801 170.  0.794 8.67e-3
    2 healthyR            4 NNAR        Test  0.750 1.50e2 0.695 122.  0.890 1.05e-1
    3 healthyR.ts         3 EARTH       Test  0.695 1.15e4 0.725 146.  0.847 4.65e-4
    4 healthyver…         1 ARIMA       Test  0.560 4.40e1 0.838  39.7 0.704 6.48e-2
    5 healthyR.ai         4 NNAR        Test  0.591 1.30e2 0.664  91.7 0.770 1.48e-1
    6 TidyDensity         1 ARIMA       Test  0.954 2.05e2 0.648 156.  1.13  4.59e-2
    7 tidyAML             1 ARIMA       Test  0.628 1.21e2 0.750 141.  0.746 3.79e-3
    8 RandomWalk…         1 ARIMA       Test  0.894 9.98e1 0.600 161.  1.02  2.10e-1

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 5
      package       .actual_data .future_data .splits           .modeltime_tables 
      <fct>         <list>       <list>       <list>            <list>            
    1 healthyR.data <tibble>     <tibble>     <split [1976|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [1969|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1905|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1827|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1711|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1563|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1168|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [591|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(package) %>%
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
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)
