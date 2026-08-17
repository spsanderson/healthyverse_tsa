# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-08-17

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

    Rows: 185,812
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

The last day in the data set is 2026-08-12 23:49:01, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
6849.02 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 185812        |
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
| r_version     |    139443 |          0.25 |   5 |  17 |     0 |       54 |          0 |
| r_arch        |    139443 |          0.25 |   1 |   7 |     0 |        7 |          0 |
| r_os          |    139443 |          0.25 |   7 |  33 |     0 |       38 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     18175 |          0.90 |   2 |   2 |     0 |      172 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-08-12 | 2024-02-21 | 2082 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136703.60 | 1473582.76 | 355 | 43661 | 325826.5 | 2348410.25 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 12320.66 | 25963.98 | 1 | 161 | 2732.0 | 12087.25 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-08-12 23:49:01 | 2024-02-21 23:01:06 | 119328 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   44.5 |       60 |

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
    -153.10  -38.53  -12.17   28.29  826.00 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.149e+02  4.863e+01
    date                                                7.738e-03  2.566e-03
    lag(value, 1)                                       9.981e-02  2.185e-02
    lag(value, 7)                                       7.124e-02  2.247e-02
    lag(value, 14)                                      7.620e-02  2.233e-02
    lag(value, 21)                                      8.855e-02  2.240e-02
    lag(value, 28)                                      7.992e-02  2.232e-02
    lag(value, 35)                                      3.627e-02  2.234e-02
    lag(value, 42)                                      6.361e-02  2.248e-02
    lag(value, 49)                                      7.723e-02  2.244e-02
    month(date, label = TRUE).L                        -8.797e+00  4.747e+00
    month(date, label = TRUE).Q                         2.190e+00  4.642e+00
    month(date, label = TRUE).C                        -1.466e+01  4.709e+00
    month(date, label = TRUE)^4                        -9.731e+00  4.724e+00
    month(date, label = TRUE)^5                        -5.617e+00  4.702e+00
    month(date, label = TRUE)^6                        -2.363e-02  4.722e+00
    month(date, label = TRUE)^7                        -2.081e+00  4.659e+00
    month(date, label = TRUE)^8                        -4.009e+00  4.636e+00
    month(date, label = TRUE)^9                         2.124e-01  4.648e+00
    month(date, label = TRUE)^10                        9.692e-02  4.637e+00
    month(date, label = TRUE)^11                       -4.002e-01  4.528e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.035e+01  2.079e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  6.976e+00  2.139e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -2.364 0.018187 *  
    date                                                 3.016 0.002594 ** 
    lag(value, 1)                                        4.567 5.24e-06 ***
    lag(value, 7)                                        3.171 0.001544 ** 
    lag(value, 14)                                       3.413 0.000656 ***
    lag(value, 21)                                       3.954 7.96e-05 ***
    lag(value, 28)                                       3.581 0.000350 ***
    lag(value, 35)                                       1.623 0.104704    
    lag(value, 42)                                       2.829 0.004716 ** 
    lag(value, 49)                                       3.442 0.000588 ***
    month(date, label = TRUE).L                         -1.853 0.063977 .  
    month(date, label = TRUE).Q                          0.472 0.637152    
    month(date, label = TRUE).C                         -3.114 0.001875 ** 
    month(date, label = TRUE)^4                         -2.060 0.039547 *  
    month(date, label = TRUE)^5                         -1.194 0.232455    
    month(date, label = TRUE)^6                         -0.005 0.996007    
    month(date, label = TRUE)^7                         -0.447 0.655249    
    month(date, label = TRUE)^8                         -0.865 0.387221    
    month(date, label = TRUE)^9                          0.046 0.963549    
    month(date, label = TRUE)^10                         0.021 0.983325    
    month(date, label = TRUE)^11                        -0.088 0.929591    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -4.979 6.95e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.261 0.001131 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.25 on 2010 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.1967,    Adjusted R-squared:  0.1879 
    F-statistic: 22.37 on 22 and 2010 DF,  p-value: < 2.2e-16

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
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 19.0621460895815"
    [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 19.0621460895815"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.4865057427848"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.4865057427848"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.4568573760027"
    [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.4568573760027"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 36.3273561635628"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 36.3273561635628"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 60.8256805364734"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 60.8256805364734"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 45.6182373514284"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 45.6182373514284"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 17.7394621787216"
    [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 17.7394621787216"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.92982635981493"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.92982635981493"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.69496094368595"
    [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.69496094368595"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 17.212597109438"
    [1] "BEST method = 'lin' PATH MEMBER = c( 8 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 17.212597109438"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.343811841752"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 8 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 11.343811841752"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.4075418817245"
    [1] "BEST method = 'both' PATH MEMBER = c( 8 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.4075418817245"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 21.3148077891198"
    [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 21.3148077891198"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.0627821679027"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 15.0627821679027"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 17.8865788840414"
    [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    [1] "BEST both OBJECTIVE FUNCTION = 17.8865788840414"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 6271.92009128511"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 6271.92009128511"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 178.595317124805"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 178.595317124805"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 479.342235794988"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 479.342235794988"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 8.739321131053"
    [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 8.739321131053"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.53942595975739"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.53942595975739"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 3.818542525217"
    [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    [1] "BEST both OBJECTIVE FUNCTION = 3.818542525217"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 86.8752052089132"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 86.8752052089132"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 41.8200769994928"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 41.8200769994928"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 55.4418318561376"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 55.4418318561376"

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
    1 healthyR.data <tibble [2,070 × 50]> <tibble [28 × 50]> <split [2042|28]>
    2 healthyR      <tibble [2,064 × 50]> <tibble [28 × 50]> <split [2036|28]>
    3 healthyR.ts   <tibble [2,000 × 50]> <tibble [28 × 50]> <split [1972|28]>
    4 healthyverse  <tibble [1,890 × 50]> <tibble [28 × 50]> <split [1862|28]>
    5 healthyR.ai   <tibble [1,805 × 50]> <tibble [28 × 50]> <split [1777|28]>
    6 TidyDensity   <tibble [1,658 × 50]> <tibble [28 × 50]> <split [1630|28]>
    7 tidyAML       <tibble [1,261 × 50]> <tibble [28 × 50]> <split [1233|28]>
    8 RandomWalker  <tibble [686 × 50]>   <tibble [28 × 50]> <split [658|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 1.2475480 | 130.99341 | 1.0692570 | 167.13919 | 1.3757800 | 0.0018677 |
| healthyR.data | 2 | LM | Test | 1.1636056 | 107.51190 | 0.9973111 | 144.84886 | 1.3080493 | 0.0115668 |
| healthyR.data | 3 | EARTH | Test | 1.2776767 | 116.22315 | 1.0950798 | 175.96116 | 1.4117645 | 0.0303842 |
| healthyR.data | 4 | NNAR | Test | 1.2072181 | 118.41367 | 1.0346907 | 158.00377 | 1.3690485 | 0.0223194 |
| healthyR | 1 | ARIMA | Test | 1.0487998 | 109.45348 | 0.8629369 | 130.39493 | 1.2040963 | 0.0088567 |
| healthyR | 2 | LM | Test | 1.2418580 | 122.09360 | 1.0217823 | 156.61282 | 1.4049996 | 0.0277288 |
| healthyR | 3 | EARTH | Test | 3.0241629 | 622.82104 | 2.4882364 | 117.46593 | 3.4980145 | 0.0007302 |
| healthyR | 4 | NNAR | Test | 1.2489626 | 114.41543 | 1.0276279 | 169.69526 | 1.4014975 | 0.0448677 |
| healthyR.ts | 1 | ARIMA | Test | 0.9411904 | 129.10598 | 1.1881710 | 159.68979 | 1.1393432 | 0.0334913 |
| healthyR.ts | 2 | LM | Test | 1.1340451 | 349.61168 | 1.4316334 | 169.74851 | 1.2877443 | 0.0142765 |
| healthyR.ts | 3 | EARTH | Test | 0.8414453 | 264.35203 | 1.0622516 | 115.25801 | 1.0078498 | 0.0250098 |
| healthyR.ts | 4 | NNAR | Test | 1.1728843 | 357.08807 | 1.4806645 | 171.32165 | 1.3403424 | 0.0015224 |
| healthyverse | 1 | ARIMA | Test | 0.6411360 | 59.29195 | 1.3422993 | 47.09701 | 0.7150279 | 0.0005666 |
| healthyverse | 2 | LM | Test | 0.9709916 | 96.84077 | 2.0328938 | 85.44969 | 1.1001985 | 0.0151731 |
| healthyverse | 3 | EARTH | Test | 2.3262519 | 173.91767 | 4.8703028 | 192.53598 | 2.4439489 | 0.0443497 |
| healthyverse | 4 | NNAR | Test | 1.1828033 | 100.14400 | 2.4763484 | 114.60798 | 1.2901008 | 0.0012831 |
| healthyR.ai | 1 | ARIMA | Test | 0.8295018 | 120.06811 | 1.0459239 | 131.52289 | 0.9959383 | 0.0095065 |
| healthyR.ai | 2 | LM | Test | 0.9480808 | 184.24490 | 1.1954408 | 140.30425 | 1.0645674 | 0.0000532 |
| healthyR.ai | 3 | EARTH | Test | 0.8430790 | 114.15456 | 1.0630435 | 133.59591 | 1.0078022 | 0.0079355 |
| healthyR.ai | 4 | NNAR | Test | 1.1099905 | 183.80042 | 1.3995938 | 171.71866 | 1.2934770 | 0.0543153 |
| TidyDensity | 1 | ARIMA | Test | 0.9926706 | 112.75891 | 1.2255919 | 175.88314 | 1.1415656 | 0.0015641 |
| TidyDensity | 2 | LM | Test | 1.3004697 | 193.79339 | 1.6056133 | 181.64498 | 1.4782811 | 0.0004868 |
| TidyDensity | 3 | EARTH | Test | 0.8191880 | 277.02305 | 1.0114031 | 80.89388 | 1.1258959 | 0.0456100 |
| TidyDensity | 4 | NNAR | Test | 1.2539695 | 152.93388 | 1.5482022 | 159.70487 | 1.4749300 | 0.0008574 |
| tidyAML | 1 | ARIMA | Test | 0.7245984 | 108.69815 | 0.6718092 | 167.59262 | 0.8772451 | 0.0365705 |
| tidyAML | 2 | LM | Test | 0.7986040 | 321.65393 | 0.7404232 | 145.68967 | 0.9641929 | 0.0026836 |
| tidyAML | 3 | EARTH | Test | 0.7473965 | 135.11916 | 0.6929464 | 183.83442 | 0.8974639 | 0.0003023 |
| tidyAML | 4 | NNAR | Test | 0.8206068 | 289.80233 | 0.7608231 | 166.99322 | 0.9637589 | 0.0006421 |
| RandomWalker | 1 | ARIMA | Test | 0.8056065 | 128.12722 | 0.9282885 | 141.26491 | 0.9054138 | 0.0044891 |
| RandomWalker | 2 | LM | Test | 1.0340149 | 117.55403 | 1.1914802 | 175.84330 | 1.2035435 | 0.0028558 |
| RandomWalker | 3 | EARTH | Test | 0.6468538 | 199.37461 | 0.7453601 | 81.72407 | 0.8397919 | 0.0109621 |
| RandomWalker | 4 | NNAR | Test | 1.1229348 | 141.35264 | 1.2939413 | 164.19317 | 1.2957129 | 0.0429708 |

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
      package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
      <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.da…         2 LM          Test  1.16  108.  0.997 145.  1.31  1.16e-2
    2 healthyR             1 ARIMA       Test  1.05  109.  0.863 130.  1.20  8.86e-3
    3 healthyR.ts          3 EARTH       Test  0.841 264.  1.06  115.  1.01  2.50e-2
    4 healthyverse         1 ARIMA       Test  0.641  59.3 1.34   47.1 0.715 5.67e-4
    5 healthyR.ai          1 ARIMA       Test  0.830 120.  1.05  132.  0.996 9.51e-3
    6 TidyDensity          3 EARTH       Test  0.819 277.  1.01   80.9 1.13  4.56e-2
    7 tidyAML              1 ARIMA       Test  0.725 109.  0.672 168.  0.877 3.66e-2
    8 RandomWalker         3 EARTH       Test  0.647 199.  0.745  81.7 0.840 1.10e-2

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
    1 healthyR.data <tibble>     <tibble>     <split [2042|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2036|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1972|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1862|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1777|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1630|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1233|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [658|28]>  <mdl_tm_t [1 × 5]>

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
