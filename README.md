# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-08-12

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

    Rows: 185,649
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

The last day in the data set is 2026-08-10 22:51:28, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
6800.06 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 185649        |
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
| r_version     |    139324 |          0.25 |   5 |  17 |     0 |       54 |          0 |
| r_arch        |    139324 |          0.25 |   1 |   7 |     0 |        7 |          0 |
| r_os          |    139324 |          0.25 |   7 |  33 |     0 |       35 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     18164 |          0.90 |   2 |   2 |     0 |      172 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-08-10 | 2024-02-21 | 2080 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1136290.83 | 1473684.65 | 355 | 43661 | 325815 | 2348407 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 12306.93 | 25943.55 | 1 | 160 | 2732 | 12070 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-08-10 22:51:28 | 2024-02-21 22:24:13 | 119172 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 13M 37S |       60 |

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
    -152.95  -38.57  -12.15   28.29  826.05 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.160e+02  4.880e+01
    date                                                7.798e-03  2.576e-03
    lag(value, 1)                                       1.000e-01  2.187e-02
    lag(value, 7)                                       7.099e-02  2.249e-02
    lag(value, 14)                                      7.601e-02  2.235e-02
    lag(value, 21)                                      8.854e-02  2.241e-02
    lag(value, 28)                                      7.981e-02  2.234e-02
    lag(value, 35)                                      3.619e-02  2.236e-02
    lag(value, 42)                                      6.329e-02  2.251e-02
    lag(value, 49)                                      7.715e-02  2.246e-02
    month(date, label = TRUE).L                        -8.767e+00  4.750e+00
    month(date, label = TRUE).Q                         2.151e+00  4.646e+00
    month(date, label = TRUE).C                        -1.469e+01  4.712e+00
    month(date, label = TRUE)^4                        -9.720e+00  4.726e+00
    month(date, label = TRUE)^5                        -5.563e+00  4.708e+00
    month(date, label = TRUE)^6                        -1.178e-02  4.724e+00
    month(date, label = TRUE)^7                        -2.135e+00  4.664e+00
    month(date, label = TRUE)^8                        -4.055e+00  4.641e+00
    month(date, label = TRUE)^9                         2.311e-01  4.650e+00
    month(date, label = TRUE)^10                        1.762e-01  4.647e+00
    month(date, label = TRUE)^11                       -3.376e-01  4.535e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.040e+01  2.084e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  6.979e+00  2.143e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -2.378 0.017520 *  
    date                                                 3.027 0.002497 ** 
    lag(value, 1)                                        4.574 5.08e-06 ***
    lag(value, 7)                                        3.156 0.001622 ** 
    lag(value, 14)                                       3.401 0.000684 ***
    lag(value, 21)                                       3.951 8.05e-05 ***
    lag(value, 28)                                       3.573 0.000361 ***
    lag(value, 35)                                       1.618 0.105737    
    lag(value, 42)                                       2.812 0.004978 ** 
    lag(value, 49)                                       3.435 0.000604 ***
    month(date, label = TRUE).L                         -1.846 0.065049 .  
    month(date, label = TRUE).Q                          0.463 0.643514    
    month(date, label = TRUE).C                         -3.118 0.001847 ** 
    month(date, label = TRUE)^4                         -2.057 0.039842 *  
    month(date, label = TRUE)^5                         -1.182 0.237471    
    month(date, label = TRUE)^6                         -0.002 0.998010    
    month(date, label = TRUE)^7                         -0.458 0.647236    
    month(date, label = TRUE)^8                         -0.874 0.382325    
    month(date, label = TRUE)^9                          0.050 0.960369    
    month(date, label = TRUE)^10                         0.038 0.969750    
    month(date, label = TRUE)^11                        -0.074 0.940663    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -4.988 6.64e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.257 0.001143 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 60.27 on 2008 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.1967,    Adjusted R-squared:  0.1879 
    F-statistic: 22.35 on 22 and 2008 DF,  p-value: < 2.2e-16

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
    [1] "CURRENT lin OBJECTIVE FUNCTION = 10.5443297780974"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 10.5443297780974"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.18965228273696"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 6.18965228273696"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 5.57689904041901"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 5.57689904041901"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 36.2323781542498"
    [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 36.2323781542498"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.95810767075938"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 9.95810767075938"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 8.37432022377156"
    [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    [1] "BEST both OBJECTIVE FUNCTION = 8.37432022377156"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 13.087173471286"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 13.087173471286"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 15.7549376601962"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 15.7549376601962"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 16.8143176555052"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 16.8143176555052"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 19.2025643442146"
    [1] "BEST method = 'lin' PATH MEMBER = c( 8 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 19.2025643442146"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 39.1934091542366"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 8 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 39.1934091542366"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 8 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 24.9207984395001"
    [1] "BEST method = 'both' PATH MEMBER = c( 8 )"
    [1] "BEST both OBJECTIVE FUNCTION = 24.9207984395001"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 21.0280316985106"
    [1] "BEST method = 'lin' PATH MEMBER = c( 5 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 21.0280316985106"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 20.7236604666416"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 5 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 20.7236604666416"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 5 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 15.2393906239156"
    [1] "BEST method = 'both' PATH MEMBER = c( 5 )"
    [1] "BEST both OBJECTIVE FUNCTION = 15.2393906239156"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 10333.9112101152"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 10333.9112101152"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 71.3151579029109"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 71.3151579029109"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 97.6733731018554"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 97.6733731018554"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 16.4406572241384"
    [1] "BEST method = 'lin' PATH MEMBER = c( 17 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 16.4406572241384"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.4379118950651"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 17 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.4379118950651"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 17 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.5122958681497"
    [1] "BEST method = 'both' PATH MEMBER = c( 17 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.5122958681497"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 100.854470214937"
    [1] "BEST method = 'lin' PATH MEMBER = c( 1 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 100.854470214937"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 28.2847421881589"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 1 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 28.2847421881589"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 1 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 43.3781829859391"
    [1] "BEST method = 'both' PATH MEMBER = c( 1 )"
    [1] "BEST both OBJECTIVE FUNCTION = 43.3781829859391"

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
    1 healthyR.data <tibble [2,068 × 50]> <tibble [28 × 50]> <split [2040|28]>
    2 healthyR      <tibble [2,062 × 50]> <tibble [28 × 50]> <split [2034|28]>
    3 healthyR.ts   <tibble [1,998 × 50]> <tibble [28 × 50]> <split [1970|28]>
    4 healthyverse  <tibble [1,889 × 50]> <tibble [28 × 50]> <split [1861|28]>
    5 healthyR.ai   <tibble [1,803 × 50]> <tibble [28 × 50]> <split [1775|28]>
    6 TidyDensity   <tibble [1,656 × 50]> <tibble [28 × 50]> <split [1628|28]>
    7 tidyAML       <tibble [1,259 × 50]> <tibble [28 × 50]> <split [1231|28]>
    8 RandomWalker  <tibble [684 × 50]>   <tibble [28 × 50]> <split [656|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 1.3115196 | 97.81517 | 1.1937254 | 180.72635 | 1.4283259 | 0.0209525 |
| healthyR.data | 2 | LM | Test | 1.2101787 | 118.47939 | 1.1014864 | 150.92212 | 1.3317336 | 0.0041248 |
| healthyR.data | 3 | EARTH | Test | 1.2921713 | 113.38771 | 1.1761149 | 175.00643 | 1.4203542 | 0.0097621 |
| healthyR.data | 4 | NNAR | Test | 1.3573996 | 115.88937 | 1.2354847 | 178.56335 | 1.5038655 | 0.0074601 |
| healthyR | 1 | ARIMA | Test | 1.0710171 | 105.00627 | 0.8756501 | 136.06150 | 1.2147969 | 0.0073446 |
| healthyR | 2 | LM | Test | 1.2740601 | 129.26372 | 1.0416555 | 163.92914 | 1.4258871 | 0.0261281 |
| healthyR | 3 | EARTH | Test | 3.9010963 | 735.35004 | 3.1894870 | 128.63332 | 4.3864297 | 0.0023128 |
| healthyR | 4 | NNAR | Test | 1.3542464 | 139.96269 | 1.1072147 | 170.47036 | 1.5142355 | 0.1445703 |
| healthyR.ts | 1 | ARIMA | Test | 0.9813935 | 105.23121 | 1.1962166 | 167.07198 | 1.1590053 | 0.0879346 |
| healthyR.ts | 2 | LM | Test | 1.1633926 | 146.99101 | 1.4180546 | 172.53083 | 1.3334896 | 0.0000003 |
| healthyR.ts | 3 | EARTH | Test | 0.9489926 | 132.04352 | 1.1567233 | 144.14212 | 1.1180976 | 0.0100200 |
| healthyR.ts | 4 | NNAR | Test | 1.1475855 | 199.88256 | 1.3987875 | 162.64731 | 1.3480712 | 0.0099564 |
| healthyverse | 1 | ARIMA | Test | 0.6314465 | 67.99225 | 1.2280762 | 47.33967 | 0.6937174 | 0.0135108 |
| healthyverse | 2 | LM | Test | 0.9543454 | 97.51079 | 1.8560701 | 84.64839 | 1.0925331 | 0.0079923 |
| healthyverse | 3 | EARTH | Test | 0.4701572 | 68.54135 | 0.9143910 | 34.68507 | 0.5354903 | 0.0000107 |
| healthyverse | 4 | NNAR | Test | 1.1906267 | 97.07500 | 2.3156046 | 115.75219 | 1.2800360 | 0.0013663 |
| healthyR.ai | 1 | ARIMA | Test | 0.7876610 | 115.34575 | 1.0599638 | 125.16418 | 0.9803029 | 0.0116408 |
| healthyR.ai | 2 | LM | Test | 0.9213294 | 187.62915 | 1.2398428 | 141.68216 | 1.0470849 | 0.0039953 |
| healthyR.ai | 3 | EARTH | Test | 0.8036989 | 110.45324 | 1.0815461 | 127.94007 | 0.9923989 | 0.0020660 |
| healthyR.ai | 4 | NNAR | Test | 1.0587673 | 164.86415 | 1.4247944 | 173.53415 | 1.2626258 | 0.1033299 |
| TidyDensity | 1 | ARIMA | Test | 1.0052156 | 105.11862 | 1.3679357 | 177.85574 | 1.1670579 | 0.0205121 |
| TidyDensity | 2 | LM | Test | 1.3702912 | 207.94335 | 1.8647445 | 188.24957 | 1.5280143 | 0.0165886 |
| TidyDensity | 3 | EARTH | Test | 2.9168915 | 815.54566 | 3.9694171 | 120.78363 | 3.3563004 | 0.0002239 |
| TidyDensity | 4 | NNAR | Test | 1.3798250 | 175.04536 | 1.8777185 | 183.17445 | 1.5641853 | 0.0726991 |
| tidyAML | 1 | ARIMA | Test | 0.7208438 | 119.85011 | 0.7060977 | 170.29023 | 0.8743105 | 0.0386901 |
| tidyAML | 2 | LM | Test | 0.7826731 | 316.08525 | 0.7666621 | 145.89498 | 0.9533722 | 0.0013360 |
| tidyAML | 3 | EARTH | Test | 0.7546669 | 145.07979 | 0.7392289 | 183.03395 | 0.9020768 | 0.0004661 |
| tidyAML | 4 | NNAR | Test | 0.8053653 | 257.96624 | 0.7888901 | 151.77645 | 0.9679740 | 0.0002462 |
| RandomWalker | 1 | ARIMA | Test | 0.9151267 | 101.09485 | 1.0638730 | 156.85161 | 1.0540182 | 0.0105246 |
| RandomWalker | 2 | LM | Test | 1.1230306 | 125.73588 | 1.3055699 | 181.29805 | 1.2666944 | 0.0073254 |
| RandomWalker | 3 | EARTH | Test | 0.9374722 | 99.93864 | 1.0898506 | 188.66647 | 1.0683018 | 0.0019928 |
| RandomWalker | 4 | NNAR | Test | 1.1954708 | 156.38006 | 1.3897847 | 171.22613 | 1.3837063 | 0.0471839 |

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
    1 healthyR.da…         2 LM          Test  1.21  118.  1.10  151.  1.33  4.12e-3
    2 healthyR             1 ARIMA       Test  1.07  105.  0.876 136.  1.21  7.34e-3
    3 healthyR.ts          3 EARTH       Test  0.949 132.  1.16  144.  1.12  1.00e-2
    4 healthyverse         3 EARTH       Test  0.470  68.5 0.914  34.7 0.535 1.07e-5
    5 healthyR.ai          1 ARIMA       Test  0.788 115.  1.06  125.  0.980 1.16e-2
    6 TidyDensity          1 ARIMA       Test  1.01  105.  1.37  178.  1.17  2.05e-2
    7 tidyAML              1 ARIMA       Test  0.721 120.  0.706 170.  0.874 3.87e-2
    8 RandomWalker         1 ARIMA       Test  0.915 101.  1.06  157.  1.05  1.05e-2

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
    1 healthyR.data <tibble>     <tibble>     <split [2040|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2034|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1970|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1861|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1775|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1628|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1231|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [656|28]>  <mdl_tm_t [1 × 5]>

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
