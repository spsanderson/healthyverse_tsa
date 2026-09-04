# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-09-04

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

    Rows: 187,725
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

The last day in the data set is 2026-09-02 23:49:26, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
7353.02 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 187725        |
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
| r_version     |    141017 |          0.25 |   5 |  17 |     0 |       54 |          0 |
| r_arch        |    141017 |          0.25 |   1 |   7 |     0 |        7 |          0 |
| r_os          |    141017 |          0.25 |   7 |  33 |     0 |       38 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     18362 |          0.90 |   2 |   2 |     0 |      172 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-09-02 | 2024-03-01 | 2103 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1139803.70 | 1472299.93 | 355 | 46549 | 329166 | 2353487 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 12519.74 | 26381.33 | 1 | 162 | 2741 | 12270 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-09-02 23:49:26 | 2024-03-01 14:56:14 | 121100 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 15M 9S |       60 |

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
    -152.07  -38.70  -11.75   28.34  826.41 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.195e+02  4.745e+01
    date                                                7.998e-03  2.499e-03
    lag(value, 1)                                       9.828e-02  2.174e-02
    lag(value, 7)                                       7.077e-02  2.234e-02
    lag(value, 14)                                      7.703e-02  2.218e-02
    lag(value, 21)                                      8.775e-02  2.225e-02
    lag(value, 28)                                      8.186e-02  2.219e-02
    lag(value, 35)                                      3.585e-02  2.223e-02
    lag(value, 42)                                      6.130e-02  2.232e-02
    lag(value, 49)                                      7.409e-02  2.227e-02
    month(date, label = TRUE).L                        -8.646e+00  4.730e+00
    month(date, label = TRUE).Q                         2.126e+00  4.618e+00
    month(date, label = TRUE).C                        -1.476e+01  4.684e+00
    month(date, label = TRUE)^4                        -9.781e+00  4.713e+00
    month(date, label = TRUE)^5                        -5.561e+00  4.663e+00
    month(date, label = TRUE)^6                         1.710e-01  4.706e+00
    month(date, label = TRUE)^7                        -2.125e+00  4.624e+00
    month(date, label = TRUE)^8                        -4.111e+00  4.603e+00
    month(date, label = TRUE)^9                         2.298e-02  4.625e+00
    month(date, label = TRUE)^10                       -1.203e-02  4.559e+00
    month(date, label = TRUE)^11                       -3.839e-01  4.480e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.039e+01  2.058e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  7.110e+00  2.118e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -2.518 0.011878 *  
    date                                                 3.201 0.001393 ** 
    lag(value, 1)                                        4.520 6.54e-06 ***
    lag(value, 7)                                        3.168 0.001560 ** 
    lag(value, 14)                                       3.473 0.000526 ***
    lag(value, 21)                                       3.944 8.28e-05 ***
    lag(value, 28)                                       3.690 0.000230 ***
    lag(value, 35)                                       1.613 0.106885    
    lag(value, 42)                                       2.747 0.006066 ** 
    lag(value, 49)                                       3.327 0.000892 ***
    month(date, label = TRUE).L                         -1.828 0.067718 .  
    month(date, label = TRUE).Q                          0.460 0.645305    
    month(date, label = TRUE).C                         -3.152 0.001644 ** 
    month(date, label = TRUE)^4                         -2.075 0.038069 *  
    month(date, label = TRUE)^5                         -1.192 0.233209    
    month(date, label = TRUE)^6                          0.036 0.971022    
    month(date, label = TRUE)^7                         -0.460 0.645920    
    month(date, label = TRUE)^8                         -0.893 0.371862    
    month(date, label = TRUE)^9                          0.005 0.996036    
    month(date, label = TRUE)^10                        -0.003 0.997895    
    month(date, label = TRUE)^11                        -0.086 0.931724    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.046 4.90e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.357 0.000803 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.14 on 2031 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.1952,    Adjusted R-squared:  0.1865 
    F-statistic: 22.39 on 22 and 2031 DF,  p-value: < 2.2e-16

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
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 7.40034368051499"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 7.40034368051499"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 12.3032789629617"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 12.3032789629617"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 9.83801555832493"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 9.83801555832493"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 7.30502443316129"
    [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 7.30502443316129"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.4960639391382"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 11.4960639391382"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 9.87740414601748"
    [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    [1] "BEST both OBJECTIVE FUNCTION = 9.87740414601748"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 13.1149974194161"
    [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 13.1149974194161"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.9626734930599"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.9626734930599"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.6340786267887"
    [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.6340786267887"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 4.21607474427729"
    [1] "BEST method = 'lin' PATH MEMBER = c( 14 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 4.21607474427729"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.17239972642686"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 8.17239972642686"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.33081335098511"
    [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.33081335098511"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 13.2828230806306"
    [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 13.2828230806306"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.47182779653506"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.47182779653506"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.56280050364373"
    [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.56280050364373"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 2852.85126060735"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 2852.85126060735"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 53.332218302048"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 53.332218302048"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 125.457577789155"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 125.457577789155"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 8.34678394198462"
    [1] "BEST method = 'lin' PATH MEMBER = c( 22 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 8.34678394198462"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.101084070555"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 22 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.101084070555"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 22 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.16596192691766"
    [1] "BEST method = 'both' PATH MEMBER = c( 22 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.16596192691766"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.05761890765119"
    [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.05761890765119"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.1635164464915"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.1635164464915"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 4.42973117107477"
    [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    [1] "BEST both OBJECTIVE FUNCTION = 4.42973117107477"

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
    1 healthyR.data <tibble [2,091 × 50]> <tibble [28 × 50]> <split [2063|28]>
    2 healthyR      <tibble [2,085 × 50]> <tibble [28 × 50]> <split [2057|28]>
    3 healthyR.ts   <tibble [2,021 × 50]> <tibble [28 × 50]> <split [1993|28]>
    4 healthyverse  <tibble [1,901 × 50]> <tibble [28 × 50]> <split [1873|28]>
    5 healthyR.ai   <tibble [1,826 × 50]> <tibble [28 × 50]> <split [1798|28]>
    6 TidyDensity   <tibble [1,679 × 50]> <tibble [28 × 50]> <split [1651|28]>
    7 tidyAML       <tibble [1,282 × 50]> <tibble [28 × 50]> <split [1254|28]>
    8 RandomWalker  <tibble [706 × 50]>   <tibble [28 × 50]> <split [678|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 0.9472332 | 650.70475 | 0.7150394 | 114.02803 | 1.2126982 | 0.0299258 |
| healthyR.data | 2 | LM | Test | 0.8975402 | 554.79536 | 0.6775276 | 127.12341 | 1.1451449 | 0.0018779 |
| healthyR.data | 3 | EARTH | Test | 2.3307883 | 2507.56373 | 1.7594458 | 133.86034 | 2.6245785 | 0.0064303 |
| healthyR.data | 4 | NNAR | Test | 0.8781185 | 565.07871 | 0.6628667 | 125.24932 | 1.1159613 | 0.0300751 |
| healthyR | 1 | ARIMA | Test | 0.7157275 | 275.95276 | 0.6915785 | 117.12457 | 0.8892630 | 0.0397126 |
| healthyR | 2 | LM | Test | 0.7548899 | 243.95437 | 0.7294195 | 125.08418 | 0.9835815 | 0.0000755 |
| healthyR | 3 | EARTH | Test | 1.9038195 | 1392.16696 | 1.8395837 | 133.50801 | 2.1315352 | 0.0611022 |
| healthyR | 4 | NNAR | Test | 0.6856910 | 145.93915 | 0.6625555 | 129.05222 | 0.9196714 | 0.0192564 |
| healthyR.ts | 1 | ARIMA | Test | 0.6217551 | 475.58831 | 0.6936846 | 116.85805 | 0.7844280 | 0.0588297 |
| healthyR.ts | 2 | LM | Test | 0.7062205 | 462.58862 | 0.7879215 | 172.43298 | 0.8518597 | 0.0061547 |
| healthyR.ts | 3 | EARTH | Test | 0.7709714 | 678.48872 | 0.8601634 | 122.57547 | 0.9251031 | 0.0521985 |
| healthyR.ts | 4 | NNAR | Test | 0.6268848 | 241.85230 | 0.6994078 | 160.70228 | 0.8027713 | 0.0260555 |
| healthyverse | 1 | ARIMA | Test | 0.4163765 | 56.92084 | 0.8293629 | 30.40768 | 0.4873999 | 0.0109569 |
| healthyverse | 2 | LM | Test | 0.8253289 | 95.43262 | 1.6439380 | 74.47565 | 0.9592074 | 0.0033460 |
| healthyverse | 3 | EARTH | Test | 0.3905476 | 61.50322 | 0.7779153 | 28.34866 | 0.4630734 | 0.0069388 |
| healthyverse | 4 | NNAR | Test | 1.0098992 | 84.01675 | 2.0115759 | 99.68465 | 1.1211961 | 0.0036483 |
| healthyR.ai | 1 | ARIMA | Test | 0.8892639 | 166.80442 | 1.0391073 | 138.70966 | 1.0393641 | 0.2612336 |
| healthyR.ai | 2 | LM | Test | 0.9458971 | 163.21690 | 1.1052833 | 153.96185 | 1.0805653 | 0.0182220 |
| healthyR.ai | 3 | EARTH | Test | 1.6939776 | 389.96209 | 1.9794175 | 141.86818 | 1.9612350 | 0.2379208 |
| healthyR.ai | 4 | NNAR | Test | 0.9260953 | 168.09401 | 1.0821449 | 152.73519 | 1.0679374 | 0.0000070 |
| TidyDensity | 1 | ARIMA | Test | 0.9128076 | 209.79950 | 0.8604351 | 131.09093 | 1.1142145 | 0.1799499 |
| TidyDensity | 2 | LM | Test | 0.8153827 | 151.64563 | 0.7685999 | 151.50131 | 0.9303623 | 0.0202111 |
| TidyDensity | 3 | EARTH | Test | 2.0563785 | 550.45399 | 1.9383934 | 149.25656 | 2.2921492 | 0.0720139 |
| TidyDensity | 4 | NNAR | Test | 0.8079948 | 152.57305 | 0.7616360 | 156.43974 | 0.9068954 | 0.0147979 |
| tidyAML | 1 | ARIMA | Test | 0.9476315 | 156.41259 | 1.1682035 | 169.02693 | 1.1294280 | 0.1675692 |
| tidyAML | 2 | LM | Test | 1.0135208 | 191.37611 | 1.2494293 | 164.75606 | 1.1881313 | 0.0375460 |
| tidyAML | 3 | EARTH | Test | 0.8907107 | 91.88401 | 1.0980336 | 174.41349 | 1.0671884 | 0.2517910 |
| tidyAML | 4 | NNAR | Test | 0.9536134 | 148.46110 | 1.1755778 | 169.88144 | 1.1248895 | 0.0181122 |
| RandomWalker | 1 | ARIMA | Test | 0.8088110 | 261.65475 | 0.8501192 | 122.12844 | 0.9505473 | 0.0084982 |
| RandomWalker | 2 | LM | Test | 0.6633518 | 106.06526 | 0.6972309 | 158.93288 | 0.7968009 | 0.0015149 |
| RandomWalker | 3 | EARTH | Test | 1.2362083 | 426.79747 | 1.2993448 | 128.28033 | 1.4514051 | 0.0843063 |
| RandomWalker | 4 | NNAR | Test | 0.7446074 | 108.02187 | 0.7826366 | 155.79040 | 0.9260425 | 0.1184855 |

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
    1 healthyR.da…         4 NNAR        Test  0.878 565.  0.663 125.  1.12  0.0301 
    2 healthyR             1 ARIMA       Test  0.716 276.  0.692 117.  0.889 0.0397 
    3 healthyR.ts          1 ARIMA       Test  0.622 476.  0.694 117.  0.784 0.0588 
    4 healthyverse         3 EARTH       Test  0.391  61.5 0.778  28.3 0.463 0.00694
    5 healthyR.ai          1 ARIMA       Test  0.889 167.  1.04  139.  1.04  0.261  
    6 TidyDensity          4 NNAR        Test  0.808 153.  0.762 156.  0.907 0.0148 
    7 tidyAML              3 EARTH       Test  0.891  91.9 1.10  174.  1.07  0.252  
    8 RandomWalker         2 LM          Test  0.663 106.  0.697 159.  0.797 0.00151

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
    1 healthyR.data <tibble>     <tibble>     <split [2063|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2057|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1993|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1873|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1798|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1651|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1254|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [678|28]>  <mdl_tm_t [1 × 5]>

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
