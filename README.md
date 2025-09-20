# Time Series Analysis, Modeling and Forecasting of the Healthyverse
Packages
Steven P. Sanderson II, MPH - Date:
2025-09-20

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

    Rows: 152,836
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

The last day in the data set is 2025-09-18 23:58:46, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
2.817201^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 152836        |
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
| r_version     |    111272 |          0.27 |   5 |   5 |     0 |       48 |          0 |
| r_arch        |    111272 |          0.27 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    111272 |          0.27 |   7 |  15 |     0 |       23 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       62 |          0 |
| country       |     13628 |          0.91 |   2 |   2 |     0 |      165 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-09-18 | 2023-09-06 | 1754 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1128331.10 | 1500799.08 | 355 | 16111.75 | 306995 | 2365155 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 11250.22 | 21630.75 | 1 | 244.00 | 3013 | 12070 | 299146 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-09-18 23:58:46 | 2023-09-06 07:42:10 | 95325 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     34 |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

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
    -146.77  -36.13  -10.71   26.79  815.92 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.942e+02  6.281e+01
    date                                                1.173e-02  3.327e-03
    lag(value, 1)                                       1.144e-01  2.365e-02
    lag(value, 7)                                       9.178e-02  2.459e-02
    lag(value, 14)                                      8.209e-02  2.463e-02
    lag(value, 21)                                      6.201e-02  2.465e-02
    lag(value, 28)                                      7.668e-02  2.456e-02
    lag(value, 35)                                      7.227e-02  2.466e-02
    lag(value, 42)                                      5.649e-02  2.482e-02
    lag(value, 49)                                      6.380e-02  2.472e-02
    month(date, label = TRUE).L                        -8.852e+00  5.118e+00
    month(date, label = TRUE).Q                         2.517e+00  5.007e+00
    month(date, label = TRUE).C                        -1.505e+01  5.074e+00
    month(date, label = TRUE)^4                        -7.430e+00  5.087e+00
    month(date, label = TRUE)^5                        -9.932e+00  5.039e+00
    month(date, label = TRUE)^6                        -2.450e+00  5.101e+00
    month(date, label = TRUE)^7                        -6.955e+00  5.009e+00
    month(date, label = TRUE)^8                        -4.522e+00  4.990e+00
    month(date, label = TRUE)^9                         3.478e+00  4.939e+00
    month(date, label = TRUE)^10                        1.872e+00  4.885e+00
    month(date, label = TRUE)^11                       -3.766e+00  4.827e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.272e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  6.878e+00  2.376e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -3.091 0.002027 ** 
    date                                                 3.527 0.000431 ***
    lag(value, 1)                                        4.837 1.44e-06 ***
    lag(value, 7)                                        3.732 0.000196 ***
    lag(value, 14)                                       3.333 0.000877 ***
    lag(value, 21)                                       2.516 0.011952 *  
    lag(value, 28)                                       3.123 0.001823 ** 
    lag(value, 35)                                       2.931 0.003427 ** 
    lag(value, 42)                                       2.276 0.022972 *  
    lag(value, 49)                                       2.581 0.009936 ** 
    month(date, label = TRUE).L                         -1.730 0.083891 .  
    month(date, label = TRUE).Q                          0.503 0.615246    
    month(date, label = TRUE).C                         -2.965 0.003066 ** 
    month(date, label = TRUE)^4                         -1.461 0.144310    
    month(date, label = TRUE)^5                         -1.971 0.048912 *  
    month(date, label = TRUE)^6                         -0.480 0.631077    
    month(date, label = TRUE)^7                         -1.389 0.165119    
    month(date, label = TRUE)^8                         -0.906 0.364948    
    month(date, label = TRUE)^9                          0.704 0.481349    
    month(date, label = TRUE)^10                         0.383 0.701612    
    month(date, label = TRUE)^11                        -0.780 0.435334    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.077 4.26e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   2.895 0.003846 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 59.04 on 1682 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.2344,    Adjusted R-squared:  0.2244 
    F-statistic:  23.4 on 22 and 1682 DF,  p-value: < 2.2e-16

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
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 11.6194147132022"
    [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 11.6194147132022"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 14.8913766203788"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 14.8913766203788"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 13.5260709162044"
    [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    [1] "BEST both OBJECTIVE FUNCTION = 13.5260709162044"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 57.9091044136175"
    [1] "BEST method = 'lin' PATH MEMBER = c( 18 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 57.9091044136175"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 73.5930513368044"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 18 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 73.5930513368044"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 18 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 69.639602931472"
    [1] "BEST method = 'both' PATH MEMBER = c( 18 )"
    [1] "BEST both OBJECTIVE FUNCTION = 69.639602931472"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 7.98910238271507"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 7.98910238271507"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.15668744559106"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.15668744559106"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 8.43332318882575"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 8.43332318882575"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 12.2339779442534"
    [1] "BEST method = 'lin' PATH MEMBER = c( 11 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 12.2339779442534"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.15405887979871"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 11 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.15405887979871"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 11 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.2801748584435"
    [1] "BEST method = 'both' PATH MEMBER = c( 11 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.2801748584435"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 12.1272336950012"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 12.1272336950012"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.00224077718545"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.00224077718545"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 7.45563157547075"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 7.45563157547075"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 6.34471179695871"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 6.34471179695871"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.25094689371275"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 9.25094689371275"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 9.57656477836938"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 9.57656477836938"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.85479860038351"
    [1] "BEST method = 'lin' PATH MEMBER = c( 15 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.85479860038351"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.76214320018243"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 15 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.76214320018243"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 15 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 6.9675274069398"
    [1] "BEST method = 'both' PATH MEMBER = c( 15 )"
    [1] "BEST both OBJECTIVE FUNCTION = 6.9675274069398"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 33.9062876292941"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 33.9062876292941"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.42134811090085"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 8.42134811090085"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 11.8952008659933"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 11.8952008659933"

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
      package       .actual_data         .future_data      .splits          
      <fct>         <list>               <list>            <list>           
    1 healthyR.data <tibble [1,746 × 2]> <tibble [28 × 2]> <split [1718|28]>
    2 healthyR      <tibble [1,737 × 2]> <tibble [28 × 2]> <split [1709|28]>
    3 healthyR.ts   <tibble [1,683 × 2]> <tibble [28 × 2]> <split [1655|28]>
    4 healthyverse  <tibble [1,654 × 2]> <tibble [28 × 2]> <split [1626|28]>
    5 healthyR.ai   <tibble [1,479 × 2]> <tibble [28 × 2]> <split [1451|28]>
    6 TidyDensity   <tibble [1,330 × 2]> <tibble [28 × 2]> <split [1302|28]>
    7 tidyAML       <tibble [937 × 2]>   <tibble [28 × 2]> <split [909|28]> 
    8 RandomWalker  <tibble [360 × 2]>   <tibble [28 × 2]> <split [332|28]> 

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
| healthyR.data | 1 | ARIMA | Test | 0.6651500 | 119.37629 | 0.9339750 | 164.17911 | 0.8565008 | 0.0090729 |
| healthyR.data | 2 | LM | Test | 0.7290682 | 202.73194 | 1.0237262 | 143.84765 | 0.8462367 | 0.1567406 |
| healthyR.data | 3 | EARTH | Test | 1.7052139 | 676.22497 | 2.3943879 | 160.61394 | 1.8752327 | 0.1567406 |
| healthyR.data | 4 | NNAR | Test | 0.6601106 | 96.44941 | 0.9268989 | 171.78519 | 0.8780870 | 0.0000450 |
| healthyR | 1 | ARIMA | Test | 0.6513550 | 105.85383 | 0.8255975 | 191.78848 | 0.8399649 | 0.1686648 |
| healthyR | 2 | LM | Test | 0.6168333 | 129.70780 | 0.7818410 | 176.48608 | 0.7738905 | 0.3472526 |
| healthyR | 3 | EARTH | Test | 0.8994570 | 457.10762 | 1.1400687 | 142.35938 | 1.1132289 | 0.3472526 |
| healthyR | 4 | NNAR | Test | 0.6318506 | 141.65728 | 0.8008756 | 171.07691 | 0.7872598 | 0.0008226 |
| healthyR.ts | 1 | ARIMA | Test | 0.7730158 | 170.03181 | 0.8851846 | 129.61683 | 0.9851954 | 0.0259325 |
| healthyR.ts | 2 | LM | Test | 0.7503509 | 153.23254 | 0.8592309 | 130.26255 | 0.9804526 | 0.0032577 |
| healthyR.ts | 3 | EARTH | Test | 1.0593708 | 379.44091 | 1.2130912 | 127.65354 | 1.2853109 | 0.0032577 |
| healthyR.ts | 4 | NNAR | Test | 0.7940534 | 116.05046 | 0.9092749 | 170.40376 | 1.0254580 | 0.0123897 |
| healthyverse | 1 | ARIMA | Test | 0.6961666 | 113.70908 | 0.9869882 | 95.96285 | 0.8698926 | 0.0351670 |
| healthyverse | 2 | LM | Test | 0.6411469 | 140.40791 | 0.9089842 | 82.61709 | 0.7906490 | 0.1586174 |
| healthyverse | 3 | EARTH | Test | 0.9864836 | 117.70236 | 1.3985843 | 172.51924 | 1.2052487 | 0.1586174 |
| healthyverse | 4 | NNAR | Test | 0.7523234 | 109.27501 | 1.0666044 | 109.12548 | 0.9325204 | 0.0605134 |
| healthyR.ai | 1 | ARIMA | Test | 0.9203673 | 97.74969 | 1.0375336 | 152.97872 | 1.4390714 | 0.0484407 |
| healthyR.ai | 2 | LM | Test | 0.8919514 | 108.80826 | 1.0055003 | 137.68213 | 1.4335664 | 0.2529396 |
| healthyR.ai | 3 | EARTH | Test | 2.0568104 | 949.59163 | 2.3186503 | 163.95059 | 2.2222648 | 0.2529396 |
| healthyR.ai | 4 | NNAR | Test | 0.9446470 | 109.95457 | 1.0649042 | 143.31809 | 1.4732924 | 0.0354469 |
| TidyDensity | 1 | ARIMA | Test | 0.9187180 | 254.45386 | 0.9520800 | 111.70242 | 1.2572642 | 0.0145002 |
| TidyDensity | 2 | LM | Test | 0.8993756 | 256.44973 | 0.9320353 | 107.94648 | 1.2246038 | 0.3964973 |
| TidyDensity | 3 | EARTH | Test | 1.0373944 | 478.31094 | 1.0750661 | 103.00135 | 1.1830397 | 0.3964973 |
| TidyDensity | 4 | NNAR | Test | 1.0157798 | 178.84136 | 1.0526666 | 141.87032 | 1.4361223 | 0.1006192 |
| tidyAML | 1 | ARIMA | Test | 0.7907077 | 107.19492 | 0.9620560 | 133.28414 | 0.9276386 | 0.0598504 |
| tidyAML | 2 | LM | Test | 0.6068050 | 125.12174 | 0.7383011 | 84.31398 | 0.7536642 | 0.1448777 |
| tidyAML | 3 | EARTH | Test | 2.5960351 | 594.73538 | 3.1586022 | 191.28187 | 2.7244342 | 0.1448777 |
| tidyAML | 4 | NNAR | Test | 0.7597421 | 114.48558 | 0.9243800 | 121.78237 | 0.8949390 | 0.0902503 |
| RandomWalker | 1 | ARIMA | Test | 1.0424098 | 101.41239 | 0.6314563 | 182.58596 | 1.3031826 | 0.0884823 |
| RandomWalker | 2 | LM | Test | 1.0829595 | 120.90369 | 0.6560199 | 164.53391 | 1.2976104 | 0.0009189 |
| RandomWalker | 3 | EARTH | Test | 1.0826705 | 100.27244 | 0.6558448 | 188.28185 | 1.3462748 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2959780 | 270.35195 | 0.7850592 | 156.12804 | 1.4602932 | 0.0157105 |

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
    1 healthyR.da…         2 LM          Test  0.729  203. 1.02  144.  0.846 1.57e-1
    2 healthyR             2 LM          Test  0.617  130. 0.782 176.  0.774 3.47e-1
    3 healthyR.ts          2 LM          Test  0.750  153. 0.859 130.  0.980 3.26e-3
    4 healthyverse         2 LM          Test  0.641  140. 0.909  82.6 0.791 1.59e-1
    5 healthyR.ai          2 LM          Test  0.892  109. 1.01  138.  1.43  2.53e-1
    6 TidyDensity          3 EARTH       Test  1.04   478. 1.08  103.  1.18  3.96e-1
    7 tidyAML              2 LM          Test  0.607  125. 0.738  84.3 0.754 1.45e-1
    8 RandomWalker         2 LM          Test  1.08   121. 0.656 165.  1.30  9.19e-4

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
    1 healthyR.data <tibble>     <tibble>     <split [1718|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [1709|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [1655|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1626|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1451|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1302|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [909|28]>  <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [332|28]>  <mdl_tm_t [1 × 5]>

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

![](man/figures/README-refit-1.png)
