# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-09-15

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

    Rows: 188,925
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

The last day in the data set is 2026-09-13 23:06:55, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
7616.32 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 188925        |
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
| r_version     |    142089 |          0.25 |   5 |  17 |     0 |       54 |          0 |
| r_arch        |    142089 |          0.25 |   1 |   7 |     0 |        7 |          0 |
| r_os          |    142089 |          0.25 |   6 |  33 |     0 |       40 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     18415 |          0.90 |   2 |   2 |     0 |      172 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-09-13 | 2024-03-08 | 2114 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1143462.96 | 1472618.86 | 355 | 46575 | 331022 | 2356912 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 12596.28 | 26458.34 | 1 | 166 | 2742 | 12327 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-09-13 23:06:55 | 2024-03-08 20:21:16 | 122240 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 15M 40S |       60 |

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
    -151.44  -38.67  -11.83   28.21  826.54 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.233e+02  4.688e+01
    date                                                8.195e-03  2.467e-03
    lag(value, 1)                                       9.851e-02  2.168e-02
    lag(value, 7)                                       7.188e-02  2.229e-02
    lag(value, 14)                                      7.674e-02  2.213e-02
    lag(value, 21)                                      8.863e-02  2.218e-02
    lag(value, 28)                                      8.304e-02  2.213e-02
    lag(value, 35)                                      3.471e-02  2.214e-02
    lag(value, 42)                                      6.087e-02  2.224e-02
    lag(value, 49)                                      7.222e-02  2.221e-02
    month(date, label = TRUE).L                        -8.461e+00  4.718e+00
    month(date, label = TRUE).Q                         2.019e+00  4.613e+00
    month(date, label = TRUE).C                        -1.501e+01  4.665e+00
    month(date, label = TRUE)^4                        -9.862e+00  4.705e+00
    month(date, label = TRUE)^5                        -5.383e+00  4.653e+00
    month(date, label = TRUE)^6                         5.188e-01  4.676e+00
    month(date, label = TRUE)^7                        -2.032e+00  4.618e+00
    month(date, label = TRUE)^8                        -4.304e+00  4.590e+00
    month(date, label = TRUE)^9                        -3.762e-01  4.579e+00
    month(date, label = TRUE)^10                       -3.686e-01  4.523e+00
    month(date, label = TRUE)^11                       -5.128e-01  4.471e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.039e+01  2.050e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  6.910e+00  2.107e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -2.629 0.008626 ** 
    date                                                 3.321 0.000911 ***
    lag(value, 1)                                        4.545 5.83e-06 ***
    lag(value, 7)                                        3.225 0.001282 ** 
    lag(value, 14)                                       3.467 0.000536 ***
    lag(value, 21)                                       3.997 6.65e-05 ***
    lag(value, 28)                                       3.752 0.000180 ***
    lag(value, 35)                                       1.568 0.117137    
    lag(value, 42)                                       2.737 0.006255 ** 
    lag(value, 49)                                       3.252 0.001165 ** 
    month(date, label = TRUE).L                         -1.793 0.073043 .  
    month(date, label = TRUE).Q                          0.438 0.661623    
    month(date, label = TRUE).C                         -3.218 0.001309 ** 
    month(date, label = TRUE)^4                         -2.096 0.036194 *  
    month(date, label = TRUE)^5                         -1.157 0.247370    
    month(date, label = TRUE)^6                          0.111 0.911675    
    month(date, label = TRUE)^7                         -0.440 0.660035    
    month(date, label = TRUE)^8                         -0.938 0.348533    
    month(date, label = TRUE)^9                         -0.082 0.934529    
    month(date, label = TRUE)^10                        -0.082 0.935051    
    month(date, label = TRUE)^11                        -0.115 0.908690    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.069 4.35e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.280 0.001056 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.1 on 2042 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.1946,    Adjusted R-squared:  0.186 
    F-statistic: 22.43 on 22 and 2042 DF,  p-value: < 2.2e-16

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
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 20 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 4.94887329357572"
    [1] "BEST method = 'lin' PATH MEMBER = c( 20 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 4.94887329357572"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 20 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.24499467051641"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 20 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 8.24499467051641"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 20 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.68746111187506"
    [1] "BEST method = 'both' PATH MEMBER = c( 20 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.68746111187506"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 8.49890795479518"
    [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 8.49890795479518"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.8318074439005"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.8318074439005"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.88057852082554"
    [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.88057852082554"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 10.8916689670977"
    [1] "BEST method = 'lin' PATH MEMBER = c( 19 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 10.8916689670977"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.4907840056176"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 19 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.4907840056176"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 19 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 9.45727111152325"
    [1] "BEST method = 'both' PATH MEMBER = c( 19 )"
    [1] "BEST both OBJECTIVE FUNCTION = 9.45727111152325"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 4.31365309741877"
    [1] "BEST method = 'lin' PATH MEMBER = c( 14 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 4.31365309741877"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.54827591104096"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 14 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 4.54827591104096"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 3.82334798713711"
    [1] "BEST method = 'both' PATH MEMBER = c( 14 )"
    [1] "BEST both OBJECTIVE FUNCTION = 3.82334798713711"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 23 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 3.97617027519524"
    [1] "BEST method = 'lin' PATH MEMBER = c( 23 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 3.97617027519524"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 23 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.2624691408661"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 23 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 15.2624691408661"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 23 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.84404518927717"
    [1] "BEST method = 'both' PATH MEMBER = c( 23 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.84404518927717"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 9.02725118049705"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 9.02725118049705"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.894525920809"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 13.894525920809"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.55861603665354"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.55861603665354"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 40.9843580998748"
    [1] "BEST method = 'lin' PATH MEMBER = c( 4 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 40.9843580998748"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.7740959292622"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 4 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.7740959292622"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 4 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 8.03674675709572"
    [1] "BEST method = 'both' PATH MEMBER = c( 4 )"
    [1] "BEST both OBJECTIVE FUNCTION = 8.03674675709572"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 136.563435999033"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 136.563435999033"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 106.214499258916"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 106.214499258916"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 86.7576264757681"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 86.7576264757681"

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
    1 healthyR.data <tibble [2,102 × 50]> <tibble [28 × 50]> <split [2074|28]>
    2 healthyR      <tibble [2,096 × 50]> <tibble [28 × 50]> <split [2068|28]>
    3 healthyR.ts   <tibble [2,032 × 50]> <tibble [28 × 50]> <split [2004|28]>
    4 healthyverse  <tibble [1,909 × 50]> <tibble [28 × 50]> <split [1881|28]>
    5 healthyR.ai   <tibble [1,837 × 50]> <tibble [28 × 50]> <split [1809|28]>
    6 TidyDensity   <tibble [1,690 × 50]> <tibble [28 × 50]> <split [1662|28]>
    7 tidyAML       <tibble [1,293 × 50]> <tibble [28 × 50]> <split [1265|28]>
    8 RandomWalker  <tibble [717 × 50]>   <tibble [28 × 50]> <split [689|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 0.7937053 | 339.4235 | 0.6924430 | 116.97425 | 1.0498319 | 0.0004874 |
| healthyR.data | 2 | LM | Test | 0.7925748 | 368.7517 | 0.6914567 | 129.66154 | 1.0359269 | 0.0024303 |
| healthyR.data | 3 | EARTH | Test | 3.1262047 | 1877.9506 | 2.7273582 | 149.94285 | 3.5024076 | 0.0565909 |
| healthyR.data | 4 | NNAR | Test | 0.7499529 | 303.1814 | 0.6542727 | 124.47347 | 0.9843006 | 0.0472141 |
| healthyR | 1 | ARIMA | Test | 0.8160139 | 348.4432 | 0.7701317 | 135.46151 | 1.0047858 | 0.0364240 |
| healthyR | 2 | LM | Test | 0.8757219 | 305.2530 | 0.8264825 | 147.74919 | 1.0795340 | 0.0411325 |
| healthyR | 3 | EARTH | Test | 1.4895597 | 1050.4018 | 1.4058059 | 138.84449 | 1.7174014 | 0.0602724 |
| healthyR | 4 | NNAR | Test | 0.7754687 | 207.6136 | 0.7318662 | 146.06459 | 0.9567735 | 0.0042930 |
| healthyR.ts | 1 | ARIMA | Test | 0.6014732 | 215.2225 | 0.8234056 | 113.36155 | 0.8107512 | 0.1064051 |
| healthyR.ts | 2 | LM | Test | 0.6211552 | 238.3478 | 0.8503499 | 148.74923 | 0.7503547 | 0.0003679 |
| healthyR.ts | 3 | EARTH | Test | 0.7898566 | 339.0671 | 1.0812989 | 124.64586 | 0.9702645 | 0.0239741 |
| healthyR.ts | 4 | NNAR | Test | 0.5572963 | 155.5145 | 0.7629282 | 145.27182 | 0.7121056 | 0.0000751 |
| healthyverse | 1 | ARIMA | Test | 0.5163002 | 262.9236 | 0.8085397 | 45.90365 | 0.6437384 | 0.0004674 |
| healthyverse | 2 | LM | Test | 0.8486822 | 217.2782 | 1.3290584 | 86.17994 | 0.9899439 | 0.0019802 |
| healthyverse | 3 | EARTH | Test | 0.5244493 | 296.1225 | 0.8213014 | 45.03139 | 0.6881126 | 0.0761033 |
| healthyverse | 4 | NNAR | Test | 1.0730260 | 190.5534 | 1.6803868 | 124.75955 | 1.2034550 | 0.0029077 |
| healthyR.ai | 1 | ARIMA | Test | 0.9694833 | 209.6829 | 1.1691374 | 162.45081 | 1.1240853 | 0.0003423 |
| healthyR.ai | 2 | LM | Test | 0.9385613 | 214.3140 | 1.1318473 | 165.07722 | 1.1038862 | 0.0537758 |
| healthyR.ai | 3 | EARTH | Test | 1.4746557 | 396.7168 | 1.7783442 | 165.82034 | 1.6277959 | 0.0132690 |
| healthyR.ai | 4 | NNAR | Test | 0.7778171 | 203.9479 | 0.9379997 | 148.21629 | 0.9341237 | 0.0002912 |
| TidyDensity | 1 | ARIMA | Test | 0.7996306 | 222.2614 | 0.7846324 | 143.26237 | 0.9712721 | 0.0363025 |
| TidyDensity | 2 | LM | Test | 0.8353911 | 266.2314 | 0.8197221 | 145.43422 | 0.9586022 | 0.0041026 |
| TidyDensity | 3 | EARTH | Test | 1.9524540 | 1097.2373 | 1.9158329 | 148.26291 | 2.2264689 | 0.0000445 |
| TidyDensity | 4 | NNAR | Test | 0.8163970 | 408.8386 | 0.8010843 | 129.89909 | 1.0204974 | 0.0020267 |
| tidyAML | 1 | ARIMA | Test | 1.0308305 | 102.2263 | 1.2397088 | 192.36235 | 1.1403204 | 0.0031530 |
| tidyAML | 2 | LM | Test | 1.0520333 | 101.3032 | 1.2652079 | 158.78014 | 1.2342108 | 0.0166388 |
| tidyAML | 3 | EARTH | Test | 1.0203706 | 100.6482 | 1.2271294 | 196.92915 | 1.1357668 | 0.0011939 |
| tidyAML | 4 | NNAR | Test | 1.0673606 | 110.6602 | 1.2836411 | 156.59387 | 1.2378320 | 0.0270568 |
| RandomWalker | 1 | ARIMA | Test | 0.7002169 | 153.0552 | 0.7454421 | 131.69589 | 0.8444666 | 0.0177297 |
| RandomWalker | 2 | LM | Test | 0.5639427 | 111.9331 | 0.6003663 | 148.54450 | 0.6538168 | 0.1955310 |
| RandomWalker | 3 | EARTH | Test | 2.9599039 | 880.7642 | 3.1510764 | 166.05006 | 3.2281139 | 0.0881354 |
| RandomWalker | 4 | NNAR | Test | 0.5361455 | 130.5868 | 0.5707737 | 131.92048 | 0.6325525 | 0.2860845 |

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
    1 healthyR.da…         4 NNAR        Test  0.750  303. 0.654 124.  0.984 4.72e-2
    2 healthyR             4 NNAR        Test  0.775  208. 0.732 146.  0.957 4.29e-3
    3 healthyR.ts          4 NNAR        Test  0.557  156. 0.763 145.  0.712 7.51e-5
    4 healthyverse         1 ARIMA       Test  0.516  263. 0.809  45.9 0.644 4.67e-4
    5 healthyR.ai          4 NNAR        Test  0.778  204. 0.938 148.  0.934 2.91e-4
    6 TidyDensity          2 LM          Test  0.835  266. 0.820 145.  0.959 4.10e-3
    7 tidyAML              3 EARTH       Test  1.02   101. 1.23  197.  1.14  1.19e-3
    8 RandomWalker         4 NNAR        Test  0.536  131. 0.571 132.  0.633 2.86e-1

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
    1 healthyR.data <tibble>     <tibble>     <split [2074|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2068|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [2004|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1881|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1809|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1662|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1265|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [689|28]>  <mdl_tm_t [1 × 5]>

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
